#lang racket

(provide lir-convert)

(require (only-in "utils.rkt" symbol-append))

; Input Language (output of `ssa-closure-convert`):

; p ::= ((proc (x x x) e) ...)
; e ::= r
;     | (let ([x l]) e)
;     | (if x e e)
;     | (cond-bind x x e e e)
; l ::= r
;     | (make-closure x x ...)
;     | (env-ref x nat)
;     | (quote dat)
;     | (prim op x ...)
;     | (apply-prim op x)
; r ::= x
;     | (call/cc x)
;     | (clo-app x x)

; Output language (lir-conv):

; p ::= ((proc (x nat x x) e) ...)
; e ::= (i ... (return r))
; i ::= (assign x l)
;     | (if x (label x) (label x))
;     | (jump (label x))
;     | (label x)
; l ::= (make-closure x x ...)
;     | (env-ref x nat)
;     | (quote dat)
;     | (prim op x ...)
;     | (apply-prim op x)
;     | (phi (x x) (x x))
;     | r
; r ::= x
;     | (call/cc x)
;     | (clo-app x x)

; low-level IR is a list-based, imperative style IR
; designed to be easily converted into an imperative asm language
; such as LLVM IR or ASM directly.
; the `nat` in the proc is for the amount of stack slots needed.
; This is the 2 arguments + the amount of locals.
(define (lir-convert procs)
  (define (conv-l l)
    (match l
      [(or `(make-closure ,_ . ,_) `(env-ref ,_ ,_) `(quote ,_)
           `(prim ,_ . ,_) `(apply-prim ,_ ,_)) l]
      [r (conv-r r)]))
  (define (conv-r r)
    (match r
      [`(call/cc ,x) `(call/cc ,x)]
      [`(clo-app ,xf ,xx) `(clo-app ,xf ,xx)]
      [(? symbol? x) x]))
  (define (conv-e e)
    (match e
      [`(let ([,x ,l]) ,e)
       (list `(assign x ,(conv-l l))
             (conv-e e))]
      [`(if ,x ,et ,ef)
       (define label-base (gensym 'branch))
       (define t-label (symbol-append label-base '-true))
       (define f-label (symbol-append label-base '-false))
       `((if ,x (label ,t-label) (label ,f-label))
         (label ,t-label)
         ,@(conv-e et)
         (label ,f-label)
         ,@(conv-e ef))]
      [`(cond-bind ,x ,xc ,et ,ef ,ejoin)
       (define phi-base (symbol-append (gensym 'branch-tmp) '- x))
       (define phi-t (symbol-append phi-base '-true))
       (define phi-f (symbol-append phi-base '-false))
       (define branch-label-base (symbol-append (gensym 'branch) '- x '-))
       (define branch-label-true (symbol-append branch-label-base '-true))
       (define branch-label-false (symbol-append branch-label-base '-false))
       (define branch-label-join (symbol-append branch-label-base '-join))
       ; this is an ugly hack, we replace the output return with a jump to the join.
       ; TODO: is there a better way?
       (define t-conv
         (match-let ([`(,is ... (return ,tret)) (conv-e et)])
           `((label ,branch-label-true)
             ,@is
             (assign ,phi-t ,tret)
             (jump ,branch-label-join))))
       (define f-conv
         (match-let ([`(,is ... (return ,fret)) (conv-e ef)])
           `((label ,branch-label-false)
             ,@is
             (assign ,phi-f ,fret)
             (jump ,branch-label-join))))
       (define join-conv
         `((label ,branch-label-join)
           (assign ,x (phi (,phi-t ,branch-label-true) (,phi-f ,branch-label-false)))
           ,@(conv-e ejoin)))
       `((if ,xc (label ,branch-label-true) (label ,branch-label-false))
         ,@t-conv
         ,@f-conv
         ,@join-conv)]
      ; else we are in tail position
      [r `(return ,(conv-r r))]))
  (define (num-slots body)
    (define (num-in-i i)
      (match i
        [`(assign . ,_) 1]
        [_ 0]))
    ; always at least 4, closures take 2 arguments (the closure and the arg-list)
    ; and there is 1 more slot for the return-address, and 1 for call-frame size.
    ; so num-local-vars + 4 = number of slots needed on stack for function call.
    (foldl + 4 (map num-in-i body)))
  (define (conv-proc proc)
    (match-define `(proc (,xname ,xenv ,xargs) ,ebody) proc)
    (define converted-body (conv-e ebody))
    (define nslots (num-slots converted-body))
    `(proc (,xname ,nslots ,xenv ,xargs) ,converted-body))
  (map conv-proc procs))

; it would be nice to make labels something like `(label x e)`
; so we can properly indent the final LLVM IR.
; i think it would require the conv-* functions to take a `final` argument
; to determine what to do in the arguments tail position. We usually
; want to just `(return ,final)` but sometimes (like in a phi-join)
; we want other things. and for branches, we need to do both, not just the last,
; which is arbitrarily the false branch.
