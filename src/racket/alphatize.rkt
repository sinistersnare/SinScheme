#lang racket

(provide alphatize)

(require (only-in "utils.rkt" prim? datum?))

; alpatize does not change the grammar, the input and output are both:
; e ::= (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (apply e e)
;     | (e e ...)
;     | (prim op e ...)
;     | (apply-prim op e)
;     | (if e e e)
;     | (call/cc e)
;     | x
;     | (quote dat)

(define (alphatize e)
  (define (rename env e)
    (match e
      [(? symbol? x) (hash-ref env x)]
      [`(quote ,(? datum? dat)) `(quote ,dat)]
      [`(let ,bindings ,body)
       (define new-bindings (map (lambda (bnd) `(,(gensym (car bnd)) ,(rename env (cadr bnd))))
                                 bindings))
       (define new-env (foldl (lambda (x new-x env) (hash-set env (car x) (car new-x)))
                              env bindings new-bindings))
       `(let ,new-bindings ,(rename new-env body))]
      [`(lambda (,xs ...) ,e0)
       (define new-xs (map gensym xs))
       (define new-env (foldl (lambda (x new-x env) (hash-set env x new-x)) env xs new-xs))
       `(lambda ,new-xs ,(rename new-env e0))]
      [`(lambda ,bindlist ,body)
       (define new-bindlist (gensym bindlist))
       (define new-env (hash-set env bindlist new-bindlist))
       `(lambda ,new-bindlist ,(rename new-env body))]
      [`(apply ,func ,args) `(apply ,(rename env func) ,(rename env args))]
      [`(prim ,(? prim? op) ,args ...) `(prim ,op ,@(map (lambda (a) (rename env a)) args))]
      [`(apply-prim ,(? prim? op) ,argslist) `(apply-prim ,op ,(rename env argslist))]
      [`(if ,if ,then ,else) `(if ,(rename env if) ,(rename env then) ,(rename env else))]
      [`(call/cc ,e) `(call/cc ,(rename env e))]
      [`(,func ,args  ...) (map (lambda (x) (rename env x)) (cons func args))]
      [else (raise `('bad-syntax ,e))]))
  (rename (hash) e))