#lang racket

; assignment-convert converts set! (assignment) into boxed references on the heap.
; this makes SSA form a lot easier to achieve.

(provide assignment-convert)

(require (only-in "utils.rkt" prim? datum?))

; 1) Compute a list of all names that are mutable
; 2) While traversing, if a binding is made that is in the list of mutable names
;    check to see if that exact binding is mutable
;    (it may just be another variable with the same name this is mutable)
; 3) If its mutable, box that one.
; 4) This way, we dont need to go down the rabbit hole for _every_ variable,
;    just the ones that may be mutable.
(define (assignment-convert e)
  (define maybe-muts (generate-maybe-muts e))
  (define (box-muts vars) (map (lambda (mutvar) `[,mutvar (prim make-vector '1 ,mutvar)])
                               (set->list vars)))
  (define (boxify e muts)
    (match e
      [`(let ,bindings ,body)
       (define new-muts (list->set
                         (filter (lambda (bnd) (check-mut maybe-muts bnd body))
                                 (map car bindings))))
       (define new-immuts (set-subtract (list->set (map car bindings)) new-muts))
       (define body-muts (set-subtract (set-union muts new-muts) new-immuts))
       `(let ,(map (lambda (bnd) `(,(car bnd)
                                   ; dont use new-muts in bindings, just old-muts.
                                   ,(if (set-member? new-muts (car bnd))
                                        `(prim make-vector '1 ,(boxify (cadr bnd) muts))
                                        (boxify (cadr bnd) muts))))
                   bindings)
          ,(boxify body body-muts))]
      [`(lambda (,bindings ...) ,body)
       (define new-muts (list->set (filter (lambda (bnd) (check-mut maybe-muts bnd body)) bindings)))
       (define new-immuts (set-subtract (list->set bindings) new-muts))
       (define body-muts (set-subtract (set-union muts new-muts) new-immuts))
       (if (set-empty? new-muts)
           `(lambda ,bindings ,(boxify body body-muts))
           `(lambda ,bindings (let ,(box-muts new-muts) ,(boxify body body-muts))))]
      [`(lambda ,bindlist ,body)
       (if (check-mut maybe-muts bindlist body)
           `(lambda ,bindlist (let ,(box-muts `(,bindlist)) ,(boxify body (set-add muts bindlist))))
           `(lambda ,bindlist ,(boxify body (set-remove muts bindlist))))]
      [`(apply ,func ,args) `(apply ,(boxify func muts) ,(boxify args muts))]
      [`(prim ,(? prim? op) ,args ...) `(prim ,op ,@(map (lambda (arg) (boxify arg muts)) args))]
      [`(apply-prim ,(? prim? op) ,argslist) `(apply-prim ,op ,(boxify argslist muts))]
      [`(if ,if ,then ,else) `(if ,(boxify if muts) ,(boxify then muts) ,(boxify else muts))]
      [`(call/cc ,e) `(call/cc ,(boxify e muts))]
      [`(quote ,(? datum? dat)) `(quote ,dat)]
      [`(set! ,x ,e0)
       `(prim vector-set! ,x '0 ,(boxify e0 muts))]
      [(? symbol? x)
       (if (set-member? muts x)
           `(prim vector-ref ,x '0)
           x)]
      [`(,func ,args  ...) (map (lambda (x) (boxify x muts)) (cons func args))]
      [else (raise `('bad-syntax ,e))]))
  (boxify e (set)))

; If you encounter a variable that is a member of the set this returns,
; it is not guaranteed to be mutated by a call to set!
; BUT if a variable is NOT on this list, then it will NEVER be mutated through set!
; kind of like a bloom-filter!
; i.e. (let ([x '5]) (let ([x 'hi]) (set! x '0)))
; Only the inner x is mutable. So you cant guarantee that all variables named x are mutated.
(define (generate-maybe-muts e)
  ; local-rename for ease of use
  (define gen generate-maybe-muts)
  (match e
    [(? symbol?) (set)]
    [`(quote ,_) (set)]
    [`(let (,bindings ...) ,body)
     (apply set-union (cons (gen body)
                            (map (lambda (binding) (gen (cadr binding))) bindings)))]
    [`(lambda ,bindings ,body) (gen body)]
    [`(apply ,func ,args) (set-union (gen func) (gen args))]
    [`(prim ,(? prim? op) ,args ...) (apply set-union
                                            (cons (set) (map (lambda (arg) (gen arg)) args)))]
    [`(apply-prim ,(? prim? op) ,argslist) (gen argslist)]
    [`(if ,if ,then ,else) (set-union (gen if) (gen then) (gen else))]
    [`(set! ,mutvar ,body) (set-union (set mutvar) (gen body))]
    [`(call/cc ,e) (gen e)]
    [`(,func ,args  ...)
     (apply set-union (cons (gen func) (map (lambda (arg) (gen arg)) args)))]
    ; this includes invalid syntax... but we are assuming there is no invalid syntax ;)
    [else (raise `('bad-syntax ,e))]))

; returns #t if the given variable is found to be mutated through set! in the .
; this may be an expensive call, depending on how large `e` is.
(define (check-mut maybe-muts var e)
  (define (check var e)
    (match e
      [(? symbol?) #f]
      [`(quote ,_) #f]
      [`(let (,bindings ...) ,body)
       (or (ormap (lambda (bnd) (if (eq? (car bnd) var) #f (check var (cadr bnd)))) bindings)
           (check var body))]
      [`(lambda (,bindings ...) ,body) (if (member var bindings) #f (check var body))]
      [`(lambda ,bindlist ,body) (if (eq? var bindlist) #f (check var body))]
      [`(apply ,func ,args) (or (check var func) (check var args))]
      [`(prim ,(? prim? op) ,args ...) (ormap (lambda (arg) (check var arg)) args)]
      [`(apply-prim ,(? prim? op) ,argslist) (check var argslist)]
      [`(if ,if ,then ,else) (or (check var if) (check var then) (check var else))]
      [`(set! ,mutvar ,body) (if (eq? mutvar var) #t (check var body))]
      [`(call/cc ,e) (check var e)]
      [`(,func ,args  ...) (ormap (lambda (e) (check var e)) (cons func args))]
      [else (raise `('bad-syntax ,e))]))
  ; fast path if the variable is guaranteed never mutable!
  (if (set-member? maybe-muts var)
      (check var e)
      #f))

; The output of assignment 2:
; e ::= (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (apply e e)
;     | (e e ...)
;     | (prim op e ...)
;     | (apply-prim op e)
;     | (if e e e)
;     | (set! x e)
;     | (call/cc e)
;     | x
;     | (quote dat)

; assignment-convert =>

;;; set! is removed and replaced with vector-set!
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