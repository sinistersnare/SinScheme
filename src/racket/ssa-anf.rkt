#lang racket

(provide anf-convert)

(require (only-in "utils.rkt" datum? test-anf-convert))

; sym-anf-convert =>

; e ::= (let ([x e]) e)
;     | (apply x x)
;     | (x x ...)
;     | (prim op x ...)
;     | (apply-prim op x)
;     | (if x e e)
;     | (call/cc x)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (quote dat)
;     | x


; This is similar to the original ANF-conversion, but the `ae` production
; is ONLY symbols now, not also quotes and lambdas. This makes
; future conversions even simpler!
(define (anf-convert e)
  ; CPS is used to sort of guarantee that we have a symbol in the correct position,
  ; the norm-sym function guarantees its a symbol and then lets you do
  ; what you want with that. To guarantee its a symbol, it may have to do
  ; some work 'outside' of the current expression, so we need to wrap the
  ; input expression into an 'administrative binding', and then inject
  ; the transformed input in the middle of that. CPS makes that easy!
  (define (norm-sym e k)
    ; ensure that the given expression is a symbol,
    ; if its not, then convert it further and bind the converted
    ; bits to an administrative binding, and continue.
    (norm e (λ (anf) (if (symbol? anf)
                         (k anf)
                         (let ([ax (gensym 'anf-bind)])
                           `(let ([,ax ,anf]) ,(k ax)))))))
  (define (norm-syms es k)
    ; normalize many symbols at once, binding them in left-to-right order
    ; this is useful for when we need to process many es at once
    ; like in a function application.
    (match es
      ['() (k '())]
      [`(,hd . ,tl) (norm-sym hd (λ (sym) (norm-syms tl (λ (syms) (k `(,sym . ,syms))))))]))
  (define (norm-let bnds body)
    ; we can now safely nest `let` bindings because we are alphatized, so
    ; we don't need to worry about shadowing in the environment.
    (match bnds
      ['() (anf-convert body)]
      [`((,bnd ,val) . ,tl)
       `(let ([,bnd ,(anf-convert val)])
          ,(norm-let tl body))]))
  (define (norm e k)
    (match e
      [(? symbol? x) (k x)]
      [`(let ,bindings ,body) (k (norm-let bindings body))]
      [`(apply ,ef ,eargs) (norm-syms `(,ef ,eargs) (λ (syms) (k `(apply . ,syms))))]
      [`(prim ,op . ,es) (norm-syms es (λ (syms) (k `(prim ,op . ,syms))))]
      [`(apply-prim ,op ,eargs) (norm-sym eargs (λ (sym) (k `(apply-prim ,op ,sym))))]
      [`(call/cc ,e) (norm-sym e (lambda (sym) `(call/cc ,sym)))]
      [`(quote ,(? datum?)) (let ([ax (gensym 'anf-datum)]) `(let ([,ax ,e]) ,(k ax)))]
      [`(lambda ,bnds ,eb) (k `(lambda ,bnds ,(anf-convert eb)))]
      [`(if ,ec ,et ,ef)
       (norm-sym ec (λ (sym) (k `(if ,sym ,(anf-convert et) ,(anf-convert ef)))))]
      [`(,_ . ,_) (norm-syms e k)]
      [_ (raise `(bad-syntax ,e))]))
  (norm e identity))

