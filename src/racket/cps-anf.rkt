#lang racket

(provide anf-convert)

(require "utils.rkt")

; ANF conversion happens after Alphatization, and proceeds CPS Conversion.
; Originally created in:
; 'The Essence of Compiling With Continuations' by Flanagan et al.


; This gives an explicit evaluation order to each expression.
; it also segregates the grammar into complex and atomic expressions
; which is useful for understanding program flow.

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

(define (atomic? e)
  (match e
    [(? symbol?) #t]
    [`(quote ,(? datum?)) #t]
    [`(lambda ,binds ,e) #t]
    [else #f]))

; we defined these inner functions using continuation passing style, instead of direct style.
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
  ; CPS is used here because we need to process code that kind of works 'outwards'.
  ; We are processing an inner node, and need to hoist it out of the current code,
  ; what it outputs is unknown, but once its hoisted, we call back to the continuation
  ; so we can fill in the inner bits.
  ; The continuation's arg is the ae that was processed that we can slip in to our
  ; new inner bit...
  ; like, when we process ((let ([f (lambda ...)]) f) 5), we need to use a continuation
  ; because the let is hoisted out of the function-position to output
  ; `(let ([f (lambda ...)]) (f 5))`. The continuation arg (`ae`) would be `f` in this case.
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
