#lang racket

(provide closure-convert)

(require (only-in "utils.rkt" symbol-append))

; Input grammar:
; anf-convert =>

; e ::= (let ([x e]) e)
;     | (apply x x)
;     | (prim op x ...)
;     | (apply-prim op x)
;     | (if x e e)
;     | (call/cc x)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (quote dat)
;     | x
;     | (x x ...)

; unify-lambdas =>

; e ::= (let ([x e]) e)
;     | (apply x x)
;     | (prim op x ...)
;     | (apply-prim op x)
;     | (if x e e)
;     | (call/cc x)
;     | (lambda (x) e)
;     | (quote dat)
;     | x
;     | (x x)

; partition-grammar =>

; e ::= (let ([x l]) e)
;     | (if x e e)
;     | (%%cond-bind x x e e e)
;     | r
; l ::= r
;     | (lambda (x) e)
;     | (quote dat)
;     | (prim op x ...)
;     | (apply-prim op x)
; r ::= x
;     | (apply x x)
;     | (call/cc x)
;     | (x x)

; Output language (`closure-convert`):

; p ::= ((proc (x x x) e) ...)
; e ::= r
;     | (let ([x l]) e)
;     | (if x e e)
;     | (%%cond-bind x x e e e)
; l ::= r
;     | (make-closure x x ...)
;     | (env-ref x nat)
;     | (quote dat)
;     | (prim op x ...)
;     | (apply-prim op x)
; r ::= x
;     | (call/cc x)
;     | (clo-app x x)

; turn (λ (xs ...) e) -> (λ xs e) -> (λ (xs) e)
; turn (λ xs e) -> (λ (xs) e)
; AKA turn every λ into a 1-arg lambda, that takes an explicit list.
; turn (x x ...) -> (x x)
; by explicitly cons'ing together a list and giving it to the function.
; by doing this, we also remove the `apply` form (by making everything an apply!).
(define (unify-lambdas anf)
  (define (layout-fixed-arg xs body)
    (define (fixed-arg-helper body todo base cur)
      (define next (add1 cur))
      (define cur-arg (symbol-append base cur))
      (define next-arg (symbol-append base next))
      (match todo
        ['() (unify-lambdas body)]
        [`(,h) `(let ([,h (prim car ,cur-arg)]) ,(unify-lambdas body))]
        [`(,hd . ,tl)
         `(let ([,hd (prim car ,cur-arg)])
            (let ([,next-arg (prim cdr ,cur-arg)])
              ,(fixed-arg-helper body tl base next)))]))
    (define base (gensym 'arglist))
    (define arg0 (symbol-append base 0))
    `(lambda (,arg0) ,(fixed-arg-helper body xs base 0)))
  (define (layout-callsite xf xs)
    ; Callsites now only take a single explicit arg list.
    ; So we convert all `xs` into an explicit cons-list.
    (define (callsite-helper xf todo argsname cur)
      (match todo
        ['() `(,xf ,(symbol-append argsname cur))]
        [`(,hd . ,tl)
         (define next (add1 cur))
         (define cur-name (symbol-append argsname cur))
         (define next-name (symbol-append argsname next))
         `(let ([,next-name (prim cons ,hd ,cur-name)])
            ,(callsite-helper xf tl argsname next))]))
    (define argsname (symbol-append (gensym 'args) '$ xf '$))
    `(let ([,(symbol-append argsname 0) '()])
       ,(callsite-helper xf (reverse xs) argsname 0)))
  (match anf
    [`(let ([,x ,ebnd]) ,ebody)
     `(let ([,x ,(unify-lambdas ebnd)]) ,(unify-lambdas ebody))]
    [`(apply ,xf ,xx)
     `(,xf ,xx)]
    [`(prim ,op ,xs ...)
     `(prim ,op ,@xs)]
    [`(apply-prim ,op ,x)
     `(apply-prim ,op ,x)]
    [`(if ,xc ,et ,ef)
     `(if ,xc ,(unify-lambdas et) ,(unify-lambdas ef))]
    [`(call/cc ,xf)
     `(call/cc ,xf)]
    [`(lambda (,xs ...) ,ebody)
     (layout-fixed-arg xs ebody)]
    [`(lambda ,x ,ebody)
     `(lambda (,x) ,(unify-lambdas ebody))]
    [`',_ anf]
    [(? symbol? x) x]
    [`(,xf ,xs ...)
     (layout-callsite xf xs)]))


; This function takes us one step closer to 'real-SSA' by partitioning the grammar.
; Now, we have 3 main productions:
; e production:
;    The top-level expression, only can be returning, binding, or branching.
; l production:
;    Things that can be bound by a let.
; r production:
;    Things that can be returned.
(define (partition-grammar e)
  (define (process-single e)
    (match e
      [`(lambda ,xs ,elambody) `(lambda ,xs ,(partition-grammar elambody))]
      [`(if ,xc ,et ,ef) `(if ,xc ,(partition-grammar et) ,(partition-grammar ef))]
      [`(let ([,x ,ebnd]) ,ebody)
       `(let ([,x ,(partition-grammar ebnd)]) ,(partition-grammar ebody))]
      [(or (? symbol?) `(quote ,_) `(call/cc ,_) `(apply ,_ ,_)
           `(prim ,_ ,_ ...) `(apply-prim ,_ ,_) `(,_ ,_ ...)) e]))
  (match e
    ; remove `if` and `let` from let binding-expressions.
    [`(let ([,x (let ([,y ,e0]) ,e1)]) ,e2)
     (partition-grammar `(let ([,y ,e0]) (let ([,x ,e1]) ,e2)))]
    [`(let ([,x (if ,xc ,et ,ef)]) ,ebody)
     ; Use %% becuase this isnt a scheme special form, so
     ; we dont want to tread over a programmers
     ; function with the same name.
     `(%%cond-bind ,x ,xc ,(partition-grammar et)
                   ,(partition-grammar ef) ,(partition-grammar ebody))]
    [`(let ([,x ,ebnd]) ,ebody)
     `(let ([,x ,(process-single ebnd)]) ,(partition-grammar ebody))]
    [(or `(lambda ,_ ,_) `(quote ,_) `(prim ,_ ,_ ...) `(apply-prim ,_ ,_))
     ; these need to be put into a let binding! and then returned!
     (define admin-bnd (gensym 'admin-toplevel-bnd))
     `(let ([,admin-bnd ,(process-single e)])
        ,admin-bnd)]
    [(or (? symbol?) `(if ,_ ,_ ,_) `(apply ,_ ,_) `(call/cc ,_) `(,_ ,_ ...))
     ; return points!
     (process-single e)]))


; in closure conversion, (f x) -> (clo-app f x) which can be turned into a
; a c-style application: `(f->fn_part)(f x)` very easily.
; we utilize 'bottom-up' conversion to make flat-closures.
; this is more efficient time-complexity, as there are less memory look-ups.
(define (ssa-closure-convert ssa)
  (define (layout-clo-env todo cur env-name lambody-conv)
    (match todo
      ['() lambody-conv]
      [`(,hd . ,tl) `(let ([,hd (env-ref ,env-name ,cur)])
                       ,(layout-clo-env tl (add1 cur) env-name lambody-conv))]))
  (define (conv-lambda x body)
    (match-define `(,body-conv ,body-frees ,body-procs) (conv-e body))
    (define clo-env-vars (set->list (set-remove body-frees x)))
    (define clo-name (gensym 'clo))
    (define cloarg-name (symbol-append clo-name '_cloarg))
    (define new-proc `(proc (,clo-name ,cloarg-name ,x)
                            ,(layout-clo-env clo-env-vars 0 cloarg-name body-conv)))
    (list `(make-closure ,clo-name ,@clo-env-vars)
          (list->set clo-env-vars)
          (cons new-proc body-procs)))
  ; The `conv-*` Functions take a l/r/e and returns a list of length 3:
  ; 1. The converted l/r/e
  ; 2. The set of free vars in the converted expression.
  ; 3. the list of procedures generated during the conversion.
  (define (conv-l l)
    ;(pretty-display `(conv-l ,l))
    (match l
      [`(lambda (,x) ,elambody)
       (conv-lambda x elambody)]
      [`(quote ,_)
       (list l (set) '())]
      [`(prim ,_ . ,xs)
       (list l (list->set xs) '())]
      [`(apply-prim ,_ ,x)
       (list l (set x) '())]
      [r (conv-r r)]))
  (define (conv-r r)
    ;(pretty-display `(conv-r ,r))
    (match r
      [(? symbol? x) (list x (set x) '())]
      [`(call/cc ,x) (list `(call/cc ,x) (set x) '())]
      [`(,xf ,xx) (list `(clo-app ,xf ,xx) (set xf xx) '())]))
  (define (conv-e e)
    ;(pretty-display `(conv-e ,e))
    (match e
      [`(let ([,x ,l]) ,ebody)
       (match-define `(,bodyconv ,bodyfrees ,bodyprocs) (conv-e ebody))
       (match-define `(,lconv ,lfrees ,lprocs) (conv-l l))
       (list
        `(let ([,x ,lconv]) ,bodyconv)
        (set-union (set-remove bodyfrees x) lfrees)
        (append lprocs bodyprocs))]
      [`(if ,xc ,et ,ef)
       (match-define (list t-conv t-free t-procs) (conv-e et))
       (match-define (list f-conv f-free f-procs) (conv-e ef))
       (list `(if ,xc ,t-conv ,f-conv)
             (set-add (set-union t-free f-free) xc)
             (append t-procs f-procs))]
      [`(%%cond-bind ,x ,xc ,et ,ef ,ejoin)
       (match-define (list t-conv t-frees t-procs) (conv-e et))
       (match-define (list f-conv f-frees f-procs) (conv-e ef))
       (match-define (list join-conv join-frees join-procs) (conv-e ejoin))
       (list `(%%cond-bind ,x ,xc ,t-conv ,f-conv ,join-conv)
             (set-add (set-union t-frees f-frees (set-remove join-frees x)) xc)
             (append t-procs f-procs join-procs))]
      [r (conv-r r)]))
  (match-define `(,main-body ,free ,procs) (conv-e ssa))
  (when (not (set-empty? free)) (displayln `(TOPLEVEL-FREE-VARS: ,free)))
  `((proc (__main ,(gensym 'mainenv) ,(gensym 'mainargs)) ,main-body) ,@procs))

(define (closure-convert anf) (ssa-closure-convert (partition-grammar (unify-lambdas anf))))
