#lang racket

(provide anf-convert)

(require "utils.rkt")

; ANF conversion happens after Alphatization, and proceeds CPS Conversion.

; This gives an explicit evaluation order to each expression.

(define (atomic? e)
  (match e
    [(? symbol?) #t]
    [`(quote ,(? datum?)) #t]
    [`(lambda ,binds ,e) #t]
    [else #f]))

; ANF conversion adapted from Flanagan et al.

; In CPS, you dont return you simply call the continuation instead
; thats what we do in all these inner functions, because CPS made it a bit easier to implement!
(define (anf-convert e)
  ; Takes in any expression and convert into atomic-expression (ae) form.
  ; if the expression is already an ae, then no work needs to be done
  ; The continuation is used because this function is written in CPS where
  ; future work is passed explicitly through the k variable.
  ; If the expression is a complex-expression, its value is 'administratively' bound
  ; to give the wrapping expression (the expression that contains the expression we are converting)
  ; an explicit evaluation order.
  ; the variable `k` is future work to be done. It can be thought of as the 'wrapping' expression
  ; that was just described.
  (define (normalize-ae e k)
    (normalize e (lambda (anf) (if (atomic? anf)
                                   (k anf)
                                   (let ([ax (gensym 'anf-bind)]) `(let ([,ax ,anf]) ,(k ax)))))))
  ; A helper to convert many expressions into atomic-expressions at once.
  ; the continuation's argument is the list of expressions in the given order
  ; with each unchanged if they were already in ae-form, or the binding of the expression
  ; if it needed to be converted (if it was complex).
  (define (normalize-aes es k)
    (match es
      ['() (k '())]
      [`(,first ,rest ...)
       (normalize-ae first (lambda (ae) (normalize-aes rest (lambda (aes) (k `(,ae ,@aes))))))]))
  (define (normalize-let bindings body)
    (match bindings
      ['() (anf-convert body)]
      [`((,binding ,val) ,rest ...) `(let ([,binding ,(anf-convert val)])
                                       ,(normalize-let rest body))]))
  ; TODO: DESCRIBE WHY WE USE CPS TO DEFINE THIS FUNCTION!
  ; because it leads to smaller code? (yes) Because it makes intuitive sense? (not really!)
  ; how to read it:
  ; talk about how the continuation is like, here is what we do after processing.
  ; and how when we call a func like normalize-ae, the k arg is like
  ; 'what to output after processing the e into an ae, where the lambdas arg is the processed ae'.
  ; and that we call k in that lambda because we need to 'continue' computing
  ; after returning the finished syntax.
  ; why CPS:
  ; And how its easier to use CPS instead of a more 'direct' method, because when we
  ; wrap the complex exp in a let binding, we need to also call back into the original code,
  ; which would be really akward in direct-style code. So by using continuations
  ; we can pass the administrative binding back into the code that knows what to do with it
  (define (normalize e k)
    (match e
      [(? symbol? x) (k x)]
      [`(let ,bindings ,body) (k (normalize-let bindings body))]
      [`(apply ,func ,argslist) (normalize-aes `(,func ,argslist) (lambda (aes) (k `(apply ,@aes))))]
      [`(prim ,op ,es ...) (normalize-aes es (lambda (aes) (k `(prim ,op ,@aes))))]
      [`(apply-prim ,op ,argslist) (normalize-ae argslist (lambda (ae) (k `(apply-prim ,op ,ae))))]
      [`(call/cc ,e) (normalize-ae e (lambda (ae) (k `(call/cc ,ae))))]
      [`(quote ,(? datum?)) (k e)]
      [`(lambda ,binds ,e0) (k `(lambda ,binds ,(anf-convert e0)))]
      [`(if ,e0 ,e1 ,e2)
       (normalize-ae e0 (lambda (ae) (k `(if ,ae ,(anf-convert e1) ,(anf-convert e2)))))]
      ; no fancy editing of the aes need to be done after processing, so just pass k along.
      [`(,es ...) (normalize-aes es k)]
      [else (raise `(bad-syntax ,e))]))
  (normalize e (lambda (x) x)))

; for ez testing...
(define anf anf-convert)

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

; anf-convert =>

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