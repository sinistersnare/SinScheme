#lang racket

(provide assignment-convert
         alphatize
         anf-convert
         cps-convert)

; Written by Davis Silverman

(require "utils.rkt")


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

; box-mutable-variables => 

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

; alpha-rename both takes and produces this language as well, then

; ANF-convert =>

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

; CPS convert => 

; e ::= (let ([x (apply-prim op ae)]) e)
;     | (let ([x (prim op ae ...)]) e)
;     | (let ([x (lambda x e)]) e)
;     | (let ([x (lambda (x ...) e)]) e)
;     | (let ([x (quote dat)]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (if ae e e)
;     | ae
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)


(define (mutated-variables e)
  (match e
         [`(let ([,xs ,es] ...) ,e0)
          (set-union (mutated-variables e0)
                     (foldl set-union (set) (map mutated-variables es)))]
         [`(lambda (,xs ...) ,e0)
          (mutated-variables e0)]
         [`(lambda ,x ,e0)
          (mutated-variables e0)]
         [`(prim ,op ,es ...)
          (foldl set-union (set) (map mutated-variables es))]
         [`(apply-prim ,op ,e0)
          (mutated-variables e0)]
         [`(if ,e0 ,e1 ,e2)
          (set-union (mutated-variables e0)
                     (mutated-variables e1)
                     (mutated-variables e2))]
         [`(set! ,x ,e0)
          (set-add (mutated-variables e0) x)]
         [`(call/cc ,e0)
          (mutated-variables e0)]
         [`(apply ,e0 ,e1)
          (set-union (mutated-variables e0)
                     (mutated-variables e1))]
         [(? symbol? x)
          (set)]
         [`',dat
          (set)]
         [`(,es ...)
          (foldl set-union (set) (map mutated-variables es))]))


(define (assignment-convert e) 
  (define alpha-e (alphatize e))
  (define vars (set->list (mutated-variables alpha-e))) 
  (define (boxify e) 
    (match e
           [`(let ([,xs ,es] ...) ,e0)
            `(let ,(map (lambda (x e)
                          (if (memv x vars)
                              `(,x (prim make-vector '1 ,(boxify e)))
                              `(,x ,(boxify e))))
                         xs
                         es)
               ,(boxify e0))]
           [`(lambda (,xs ...) ,e0)
            (define boxes (filter (lambda (x) (memv x vars)) xs))
            `(lambda ,xs
               ,(if (null? boxes)
                    (boxify e0)
                    `(let ,(map (lambda (x) `(,x (prim make-vector '1 ,x))) boxes)
                       ,(boxify e0))))]
           [`(lambda ,x ,e0)
            `(lambda ,x
               ,(if (memv x vars)
                    `(let ([,x (prim make-vector '1 ,x)])
                       ,(boxify e0))
                    (boxify e0)))]
           [`(prim ,op ,es ...)
            `(prim ,op ,@(map boxify es))]
           [`(apply-prim ,op ,e0)
            `(apply-prim ,op ,(boxify e0))]
           [`(if ,e0 ,e1 ,e2)
            `(if ,(boxify e0)
                 ,(boxify e1)
                 ,(boxify e2))]
           [`(set! ,x ,e0)
            `(prim vector-set! ,x '0 ,(boxify e0))]
           [`(call/cc ,e0)
            `(call/cc ,(boxify e0))]
           [`(apply ,e0 ,e1)
            `(apply ,(boxify e0)
                 ,(boxify e1))]
           [(? symbol? x)
            (if (set-member? vars x)
                `(prim vector-ref ,x '0)
                x)]
           [`',dat
            `',dat]
           [`(,es ...)
            (map boxify es)]))
  (boxify alpha-e))


(define (alphatize e)
  (displayln e)
  (displayln "ENDED")
  (define (rename x)
    (define xs (symbol->string x))
    (define lst (string->list xs))
    (define rest (member #\$ lst))
    (define alpha "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
    (define alphanum (string-append "0123456789" alpha))
    (define (random-char pool) (list-ref (string->list pool) (random (string-length pool))))
    (if rest
        (if (null? rest) 
            (rename 'unnamed)
            (rename (string->symbol (list->string (cdr rest)))))
        (string->symbol (string-append (list->string (cons (random-char alpha)
                                                           (map (lambda (n) (random-char alphanum))
                                                                '(0 1))))
                                       "$" xs))))
  (define ((alpha-rename env) e)
    (match e
           [`(let ([,xs ,es] ...) ,e0)
            (define xs+ (map rename xs))
            (define env+ (foldl (lambda (x x+ env) (hash-set env x x+)) env xs xs+))
            `(let ,(map list xs+ (map (alpha-rename env) es))
              ,((alpha-rename env+) e0))]
           [`(lambda (,xs ...) ,e0)
            (define xs+ (map rename xs))
            (define env+ (foldl (lambda (x x+ env) (hash-set env x x+)) env xs xs+))
            `(lambda ,xs+ ,((alpha-rename env+) e0))]
           [`(lambda ,x ,e0)
            (define x+ (rename x))
            (define env+ (hash-set env x x+))
            `(lambda ,x+ ,((alpha-rename env+) e0))]
           [`(prim ,op ,es ...)
            `(prim ,op ,@(map (alpha-rename env) es))]
           [`(apply-prim ,op ,e0)
            `(apply-prim ,op ,((alpha-rename env) e0))]
           [`(if ,e0 ,e1 ,e2)
            `(if ,((alpha-rename env) e0)
                 ,((alpha-rename env) e1)
                 ,((alpha-rename env) e2))]
           [`(call/cc ,e0)
            `(call/cc ,((alpha-rename env) e0))]
           [`(set! ,x ,e0)
            `(set! ,(hash-ref env x (lambda () (display `('SET!e ,e env! ,env)) 'BLOOPBAPSDFASDF2)) ,((alpha-rename env) e0))]
           [`(apply ,e0 ,e1)
            `(apply ,((alpha-rename env) e0)
                    ,((alpha-rename env) e1))]
           [(? symbol? x)
            (hash-ref env x (lambda () (display `('FUCKFUCK ,e lol)) 'BLOOPBAPSDFASDF2))]
           [`',dat
            `',dat]
           [`(,es ...)
            (map (alpha-rename env) es)]))
  ((alpha-rename (hash)) e))


; Converts to ANF; adapted from Flanagan et al.
(define (anf-convert e)
  (propagate-xy (normalize e (lambda (x) x))))



(define (normalize e k)
  (define (normalize-ae e k)
    (normalize e (lambda (anf)
                   (match anf
                          [`(lambda ,xs ,e0)
                           (k `(lambda ,xs ,e0))]
                          [`',dat (k `',dat)]
                          [(? symbol? x) (k x)]
                          [else
                           (let ([x (gensym 'a)])
                             `(let ([,x ,anf]) ,(k x)))]))))
  (define (normalize-aes es k)
    (if (null? es)
        (k '())
        (normalize-ae (car es)
                      (lambda (x) (normalize-aes (cdr es)
                                                 (lambda (xs) (k `(,x . ,xs))))))))
  (match e
         [`',dat (k `',dat)]
         [(? symbol? x) (k x)]
         [`(lambda ,xs ,e0)
          (k `(lambda ,xs ,(anf-convert e0)))]
         [`(let () ,e0)
          (normalize e0 k)]
         [`(let ([,x ,rhs] . ,rest) ,e0)
          (k `(let ([,x ,(anf-convert rhs)])
                ,(anf-convert
                  `(let ,rest ,e0))))]
         [`(if ,ec ,et ,ef)
          (normalize-ae ec
                          (lambda (xc)
                            (k `(if ,xc
                                    ,(anf-convert et)
                                    ,(anf-convert ef)))))]
         [`(prim ,op ,es ...)
          (normalize-aes es
                           (lambda (xs)
                             (k `(prim ,op . ,xs))))]
         [`(apply-prim ,op ,e0)
          (normalize-ae e0
                          (lambda (x)
                            (k `(apply-prim ,op ,x))))]
         [`(call/cc ,e0)
          (normalize-ae e0
                          (lambda (x)
                            (k `(call/cc ,x))))]
         [`(apply ,es ...)
          (normalize-aes es
                           (lambda (xs)
                             (k `(apply . ,xs))))]
         [`(,es ...)
          (normalize-aes es k)]))


(define (propagate-xy e)
  (define ((prop env) e)
    (match e
           [`',dat `',dat]
           [(? symbol? x)
            (hash-ref env x (lambda () x))]
           [`(let ([,x ,(? symbol? y)]) ,e0)
            ((prop (hash-set env x (hash-ref env y (lambda () y)))) e0)]
           [`(let ([,x ,e0]) ,e1)
            `(let ([,x ,((prop env) e0)]) ,((prop env) e1))]
           [`(lambda ,xs ,e0)
            `(lambda ,xs ,((prop env) e0))]
           [`(if ,es ...)
            `(if . ,(map (prop env) es))]
           [`(prim ,op ,es ...)
            `(prim ,op . ,(map (prop env) es))]
           [`(apply-prim ,op ,e0)
            `(apply-prim ,op ,((prop env) e0))]
           [`(call/cc ,e0)
            `(call/cc ,((prop env) e0))]
           [`(apply ,es ...)
            `(apply . ,(map (prop env) es))]
           [`(,es ...)
            (map (prop env) es)]))
  ((prop (hash)) e))



(define (cps-convert e)
  (define (T-ae ae)
    (match ae
           [`(lambda (,xs ...) ,e0)
            (define cx (gensym 'cont))
            `(lambda (,cx ,@xs)
               ,(T e0 cx))]
           [`(lambda ,x ,e0)
            (define cx (gensym 'cont))
            (define x+ (gensym x))
            `(lambda ,x+
               (let ([,cx (prim car ,x+)])
                 (let ([,x (prim cdr ,x+)]) 
                   ,(T e0 cx))))]
           [(? symbol? x)
            x]
           [`',dat
            `',dat]))
  (define (T e cae)
    (match e
           ; return (call continuation)
           [(? symbol? x)
            `(,cae '() ,x)]
           [`',dat
            `(,cae '() ',dat)]
           [`(lambda . ,rest)
            `(,cae '() ,(T-ae e))]
           ; prim ops
           [`(prim ,op ,aes ...)
            (define retx (gensym 'retprim))
            (T `(let ([,retx (prim ,op ,@aes)]) ,retx) cae)]
           [`(apply-prim ,op ,ae)
            (define retx (gensym 'retprim))
            (T `(let ([,retx (apply-prim ,op ,ae)]) ,retx) cae)]
           [`(let ([,x (apply-prim ,op ,ae)]) ,e0)
            `(let ([,x (apply-prim ,op ,(T-ae ae))])
               ,(T e0 cae))]
           [`(let ([,x (prim ,op ,aes ...)]) ,e0)
            `(let ([,x (prim ,op ,@(map T-ae aes))])
               ,(T e0 cae))]
           [`(let ([,x (lambda ,xs ,elam)]) ,e0)
            `(let ([,x ,(T-ae `(lambda ,xs ,elam))])
               ,(T e0 cae))]
           [`(let ([,x ',dat]) ,e0)
            `(let ([,x ',dat])
               ,(T e0 cae))]
           ; let -> continuation
           [`(let ([,x ,rhs]) ,e0)
            (define _x (gensym '_))
            (T rhs `(lambda (,_x ,x)
                      ,(T e0 cae)))]
           ; walk if, desugar call/cc, apply function 
           [`(if ,ae ,e0 ,e1)
            `(if ,(T-ae ae) ,(T e0 cae) ,(T e1 cae))]
           [`(call/cc ,ae)
            `(,(T-ae ae) ,cae ,cae)]
           [`(apply ,ae0 ,ae1)
            (define xlst (gensym 'cps-lst))
            `(let ([,xlst (prim cons ,cae ,(T-ae ae1))])
               (apply ,(T-ae ae0) ,xlst))]
           [`(,fae ,args ...)
            `(,(T-ae fae) ,cae ,@(map T-ae args))]))
  (T e '(lambda (_0 x) (let ([_1 (prim halt x)]) (_1 _1)))))


