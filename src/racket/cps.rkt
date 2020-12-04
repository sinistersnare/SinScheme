#lang racket

(provide cps-convert)

(require "utils.rkt")

; ANF Grammar
; e ::= (let ([x e]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (prim op ae ...)
;     | (apply-prim op ae)
;     | (if ae e e)
;     | (call/cc ae)
;     | ae
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)

; cps-convert => 

; e ::= (let ([x (apply-prim op ae)]) e)
;     | (let ([x (prim op ae ...)]) e)
;     | (let ([x (lambda x e)]) e)
;     | (let ([x (lambda (x ...) e)]) e)
;     | (let ([x (quote dat)]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (if ae e e)
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)

; CPS conversion turns a direct-style program into a continuation-passing style program.
; This ensures that 100% of functions are tail-recursive, so practically removes the stack.


(define (cps-convert anf-code)
  ; TODO docs for this func.
  ; convert an ANF-AE to CPS
  ; an atomic expression doesnt have a continuation-expr,
  ; because they are trivially evaluated.
  (define (conv-atomic exp)
    (match exp
      ; symbol and quoted datums are already atomic expressions.
      [(? symbol? x) exp]
      [`(quote ,(? datum? d)) exp]
      ; Lambdas simply add an arg for a continuation, and use that in future compilation
      [`(lambda (,xs ...) ,e0)
       (let ([k (gensym 'k)]) `(lambda (,k ,@xs) ,(conv-complex e0 k)))]
      ; for the vararg lambda, we need to rewrite it to capture the k explictly.
      [`(lambda ,restvar ,e0)
       (define k (gensym 'k))
       (define k-and-args (gensym 'k-and-args))
       `(lambda ,k-and-args
          (let ([,k (prim car ,k-and-args)])
            (let ([,restvar (prim cdr ,k-and-args)])
              ,(conv-complex e0 k))))]
      [else (raise `('bad-atomic-syntax ,exp))]))
  ; TODO docs for this function, describing what cae is for.
  ; convert an ANF-CE to CPS
  (define (conv-complex exp cae)
    (match exp
      [(? symbol? x) `(,cae ,x ,x)]
      [`(quote ,(? datum? d)) `(,cae ,exp ,exp)]
      [`(let ([,bind (apply-prim ,op ,argslistae)]) ,body)
       `(let ([,bind (apply-prim ,op ,(conv-atomic argslistae))]) ,(conv-complex body cae))]
      [`(let ([,bind (prim ,op ,aes ...)]) ,body)
       `(let ([,bind (prim ,op ,@(map conv-atomic aes))]) ,(conv-complex body cae))]
      [`(let ([,bind (lambda ,args ,lambdabody)]) ,body)
       `(let ([,bind ,(conv-atomic `(lambda ,args ,lambdabody))]) ,(conv-complex body cae))]
      [`(let ([,bind (quote ,(? datum? dat))]) ,body)
       `(let ([,bind (quote ,dat)]) ,(conv-complex body cae))]
      ; evaluate the bindval, then use it in a continuation immediately.
      [`(let ([,bind ,bindval]) ,body)
       (let ([unusedk (gensym 'unusedk)])
         (conv-complex bindval `(lambda (,unusedk ,bind) ,(conv-complex body cae))))]
      ; forward any lambda to conv-atomic
      [`(lambda . ,rest)
       `(,cae (lambda (_un _used) (_un _used _used)) ,(conv-atomic exp))]
      ; we need to add the continuation to the args list
      ; and that must be done thru a prim in a let-binding now.
      [`(apply ,funcae ,argslistae)
       (let ([applyarg (gensym 'applyarg)])
         `(let ([,applyarg (prim cons ,cae ,(conv-atomic argslistae))])
            (apply ,(conv-atomic funcae) ,applyarg)))]
      ; evaluate the prim with an intermediate let-binding, to conform to the new grammar.
      ; the continuation is not used.. TODO WHY???? Because prims dont take a continuation I think...
      ; but what does that mean... why not? I mean, I know they dont... caues theyre defined by the
      ; runtime... but........... what? why?
      [`(prim ,(? symbol? op) ,rest ...)
       (define ret (gensym 'prim-ret))
       (conv-complex `(let ([,ret (prim ,op ,@rest)]) ,ret) cae)]
      ; see above for how apply and how prim work. This meshes the two.
      [`(apply-prim ,(? symbol? op) ,argslist)
       (define ret (gensym 'aprim-ret))
       (conv-complex `(let ([,ret (apply-prim ,op ,argslist)]) ,ret) cae)]
      [`(if ,aecond ,ethen ,eelse)
       `(if ,(conv-atomic aecond) ,(conv-complex ethen cae) ,(conv-complex eelse cae))]
      ; convert call/cc to finally use the continuation!! WOOHOO SEE YOU LATER call/cc *glasses-emoji*
      [`(call/cc ,ae) `(,(conv-atomic ae) ,cae ,cae)]
      ; add the current continuation to the function call
      [`(,aef ,aes ...) `(,(conv-atomic aef) ,cae ,@(map conv-atomic aes))]
      [raise `(bad-complex-syntax ,exp)]))
  (conv-complex anf-code '(lambda (k x) (let ([_1 (prim halt x)]) (k x)))))

