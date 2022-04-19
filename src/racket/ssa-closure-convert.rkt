#lang racket

(provide ssa-closure-convert)

(require (only-in "utils.rkt" symbol-append))

; Input Grammar (after unifying lambdas `unify-lambdas`):

; e ::= r
;     | (let ([x l]) e)
;     | (if x e e)
;     | (cond-bind x x e e e)
; l ::= r
;     | (lambda (x) e)
;     | (quote dat)
;     | (prim op x ...)
;     | (apply-prim op x)
; r ::= x
;     | (call/cc x)
;     | (x x)


; Output language (`ssa-closure-convert`):

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


; in closure conversion, (f x) -> (clo-app f x) which can be turned into a
; (C-style-app f[0] f x) very easily.
; we utilize 'bottom-up' conversion to make flat-closures.
; this is more efficient time-wise, as there are less memory look-ups.
(define (ssa-closure-convert ssa)
  (define (layout-clo-env todo cur env-name lambody-conv)
    (match todo
      ['() lambody-conv]
      [`(,hd . ,tl) `(let ([,hd (env-ref ,env-name ,cur)]) ,(layout-clo-env tl (add1 cur)))]))
  ; The `conv-*` Functions take a * and returns a list of length 3:
  ; 1. The converted *
  ; 2. The set of free vars in the converted expression.
  ; 3. the list of procedures generated during the conversion.
  ; where * = l/r/e
  (define (conv-l l)
    (match l
      [`(lambda (,x) ,elambody)
       (match-define `(,lambody-conv ,lambody-frees ,lambody-procs) (conv-e elambody))
       (define clo-env-vars (set->list (set-remove lambody-frees x)))
       (define clo-name (gensym 'clo))
       (define env-name (symbol-append clo-name '-env))
       (define new-proc `(proc (,clo-name ,env-name ,x)
                               ,(layout-clo-env clo-env-vars 0 env-name lambody-conv)))
       (list `(make-closure ,clo-name . ,clo-env-vars)
             (list->set clo-env-vars)
             (cons new-proc lambody-procs))]
      [`(quote ,_)
       (list l (set) '())]
      [`(prim ,op . ,xs)
       (list l (list->set xs) '())]
      [`(apply-prim ,op ,x)
       (list l (set x) '())]
      [r (conv-r r)]))
  (define (conv-r r)
    (match r
      [(? symbol? x) (list x (set x) '())]
      [`(call/cc ,x) (list `(call/cc ,x) (set x) '())]
      [`(,xf ,xx) (list `(clo-app ,xf ,xx) (set xf xx) '())]))
  (define (conv-e e)
    (match e
      [`(let ([,x ,l]) ,ebody)
       (match-define `(,bodyconv ,bodyfrees ,bodyprocs) (conv-e ebody))
       (match-define `(,lconv ,lfrees ,lprocs) (conv-l l))
       (list `(let ([,x ,lconv]) ,bodyconv)
             ; this is tricky!
             ; The free variables in this expression are:
             ; the frees of ,ebody  minus ,x plus ,lfrees
             (set-union (set-remove bodyfrees x) lfrees)
             (append lprocs bodyprocs))]
      [`(if ,xc ,et ,ef)
       (match-define (list t-conv t-free t-procs) (conv-e et))
       (match-define (list f-conv f-free f-procs) (conv-e ef))
       (list `(if ,xc ,t-conv ,f-conv)
             (set-union t-free f-free)
             (append t-procs f-procs))]
      [`(cond-bind ,x ,xc ,et ,ef ,ejoin)
       (match-define (list t-conv t-free t-procs) (conv-e et))
       (match-define (list f-conv f-free f-procs) (conv-e ef))
       (match-define (list join-conv join-free join-procs) (conv-e ejoin))
       (list `(cond-bind ,x ,xc ,t-conv ,f-conv ,join-conv)
             (set-union t-free f-free join-free)
             (append t-procs f-procs join-procs))]
      [r (conv-r r)]))
  (match-define `(,main-body ,free ,procs) (conv-e ssa))
  (when (not (set-empty? free)) (displayln `(TOPLEVEL-FREE-VARS: ,free)))
  `((proc (main ,(gensym 'mainenv) ,(gensym 'mainargs)) ,main-body) ,@procs))


