#lang racket

(provide top-level)
(require "utils.rkt")

(define (top-level e)
  (define (T-qq qq [depth 0])
      (match qq
        [(list 'unquote exp)
         (if (= depth 0)
             (T exp)
             (list 'list ''unquote (T-qq exp (sub1 depth))))]
        [`(quasiquote ,qq1)
         `(list 'quasiquote ,(T-qq qq1 (add1 depth)))]
        [`(,qq1 . ,qrest)
         `(cons ,(T-qq qq1 depth) ,(T-qq qrest depth))]
        [else `(quote ,qq)]))
  (define (T-pattern-guard k pat)
    (match pat ; generate a boolean expression for "does k match pat"
      [``,qq-pat
       (T-pattern-guard k (T-qq qq-pat))]
      [`(cons ,p0 ,p1)
       (define k0 (gensym 'pk))
       (define k1 (gensym 'pk))
       `(and (cons? ,k)
             (let ([,k0 (car ,k)]
                   [,k1 (cdr ,k)])
               (and ,(T-pattern-guard k0 p0)
                    ,(T-pattern-guard k1 p1))))]
      [`(? ,pred ,p0)
       `(and (,(T pred) ,k) ,(T-pattern-guard k p0))]
      [(? symbol? x)
       ''#t]
      [`(quote ,(? datum? d))
       `(equal? ',d ,k)]
      [(? datum? d)
       `(equal? ',d ,k)]))
  (define (T-pattern-bindings k pat thunk)
    (match pat ; generate bindings for k, which matches pat, around (thunk)
      [``,qq-pat
       (T-pattern-bindings k (T-qq qq-pat) thunk)]
      [`(cons ,p0 ,p1)
       (define k0 (gensym 'pk))
       (define k1 (gensym 'pk))
       `(let ([,k0 (car ,k)]
              [,k1 (cdr ,k)])
          ,(T-pattern-bindings k0 p0 (lambda () (T-pattern-bindings k1 p1 thunk))))]
      [`(? ,pred ,p0)
       (T-pattern-bindings k p0 thunk)]
      [(? symbol? x)
       `(let ([,x ,k])
          ,(thunk))]
      [`(quote ,(? datum? d))
       (thunk)]
      [(? datum? d)
       (thunk)]))
  (define (extract-defs beg)
    (define not-define?
      (match-lambda [`(define . _) #f] [else #t]))
    (match beg
      [`(begin (define ,(? symbol? x) ,body) ,es ...)
       (match-define (cons defs e+) (extract-defs `(begin . ,es)))
       (cons (cons `[,x ,(T body)] defs) e+)]
      [`(begin
          (define (,(? symbol? f) ,(? symbol? xs) ... ,(list def-xs def-es) ...) ,bodies ...)
          ,es ...)
       (match-define (cons defs e+) (extract-defs `(begin . ,es)))
       (cons (cons `[,f ,(T `(lambda (,@xs ,@(map list def-xs def-es)) . ,bodies))] defs) e+)]
      [`(begin
          (define (,(? symbol? f) ,(? symbol? xs) ... . ,(? symbol? args)) ,bodies ...)
          ,es ...)
       (match-define (cons defs e+) (extract-defs `(begin . ,es)))
       (cons (cons `[,f ,(T `(lambda (,@xs . ,args) . ,bodies))] defs) e+)]
      [`(begin (begin ,e0s ...) ,e1s ...)
       (extract-defs `(begin ,@e0s ,@e1s))]
      [`(begin)
       (cons '() '(begin))]
      [`(begin ,(? not-define? nde) ,es ...)
       (let ([nde+ (T nde)])
         (if (null? es)
             (cons '() nde+)
             (match-let ([(cons defs e+) (extract-defs `(begin . ,es))])
               (cons (cons `(,(gensym 'tmp) ,nde+) defs) e+))))]))
  (define (T-lambda-with-default-params xs def-xs def-es body+)
    (define args (gensym 'args))
    (define bindings
      (let lp ([dxs def-xs] [des+ (map T def-es)]
               [args args] [bindings '()])
        (if (null? dxs)
            bindings
            (let* ([pair (gensym 'pair)]
                   [new-args (gensym 'args)]
                   [new-bindings (list
                                  `(,pair (if (null? ,args)
                                              (cons ,(car des+) '())
                                              ,args))
                                  `(,(car dxs) (car ,pair))
                                  `(,new-args (cdr ,pair)))])
              (lp (cdr dxs) (cdr des+) new-args (append bindings new-bindings))))))
    (if (null? def-xs)
        `(lambda ,xs ,body+)
        `(lambda (,@xs . ,args) (let* ,bindings ,body+))))
  (define (T e)
    (match e
      [`(begin ,es ...)
       (match-define (cons defs e+) (extract-defs e))
       `(letrec* ,defs ,e+)]
      [`(let ,loop ([,xs ,rhss] ...) ,es ...)
       `(let ,loop ,(map list xs (map T rhss)) ,(T `(begin . ,es)))]
      [`(cond [,guards ,ess ...] ...)
       `(cond ,@(map (lambda (guard es) (if (null? es) `(,(T guard)) `[,(T guard) ,(T `(begin . ,es))])) guards ess))]
      [`(case ,key [,guards ,ess ...] ...)
       `(case ,(T key) ,@(map (lambda (guard es) `[,guard ,(T `(begin . ,es))]) guards ess))]
      [`(guard (,x [,guards ,ess ...] ...) ,bodies ...)
       `(guard (,x ,@(map (lambda (guard es) (if (null? es) `(,(T guard)) `(,(T guard) ,(T `(begin . ,es))))) guards ess))
               ,(T `(begin . ,bodies)))]
      [`(lambda ,param ,es ...)
       (match param
              [(? symbol? x)
               `(lambda ,x ,(T `(begin . ,es)))]
              [`(,(? symbol? xs) ... . ,(? symbol? args))
               `(lambda (,@xs . ,args) ,(T `(begin . ,es)))]
              [`(,(? symbol? xs) ... ,(list def-xs def-es) ...)
               (T-lambda-with-default-params xs def-xs def-es (T `(begin . ,es)))]
              [else (error "unexpected arg list")])]
      [`(match ,(? symbol? k))
       `(raise '"Match failed")]
      [`(match ,(? symbol? k) [,pat ,es ...] . ,rest)
       `(if ,(T (T-pattern-guard k pat))
            ,(T (T-pattern-bindings k pat (lambda () `(begin . ,es))))
            ,(T `(match ,k . ,rest)))]
      [`(match ,key . ,rest)
       (define k (gensym 'match-key))
       `(let ([,k ,(T key)]) ,(T `(match ,k . ,rest)))]
                                        ; Handle various forms with implicit begins at once
      [`(,(and tag (or 'letrec 'letrec* 'let 'let* 'when 'unless)) ,stuff ,es ...)
       `(,tag ,(T stuff) ,(T `(begin . ,es)))]
      [``,quasi (T-qq quasi)]
      [`',d `',d]
      [(? symbol? x) x]
      [`(,es ...) (map T es)]
      [(? datum? d) `',d]))
  (T e))
