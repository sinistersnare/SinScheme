#lang racket

; Originally Written by Thomas Gilray and Javran Cheng
; heavily modified since then, thanks Tom & Javran!

(provide prim? reserved? prims-list symbol-append c-name prim-name applyprim-name
         c-hex-encode encoded-str-length datum? read-begin
         test-top-level test-desugar test-alphatize test-anf-convert
         test-closure-convert eval-top-level eval-scheme eval-ir eval-proc
         scheme-exp? ir-exp? alphatized-exp? anf-exp? proc-exp? lir-exp?)

(require (only-in racket/format ~a))

; many of these prims are expanded by hand in the `simplify-ir`
; pass in `desugar.rkt`. The rest are implemented in runtime.cpp
(define prims-list
  '(= > < <= >= + - * / promise?
      cons? null? cons car cdr list first second third fourth fifth
      length list-tail drop take member memv map append foldl foldr
      vector? vector make-vector vector-ref vector-set! vector-length
      set set->list list->set set-add set-union set-count set-first
      set-rest set-remove hash hash-ref hash-set hash-count hash-keys
      hash-has-key? hash? list? void? procedure? number? integer? boolean?
      error void print println display write exit halt eq? eqv? equal? not))

(define (prim? op) (member op prims-list))

(define (symbol-append . ss)
  (match ss
    ['() '||]
    [`(,one ,rest ...) (string->symbol (string-append (~a one) (~a (apply symbol-append rest))))]))

(define (c-name s)
  (define ok-set
    (list->set (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789$")))
  (foldr string-append
         ""
         (map (lambda (c)
                (if (set-member? ok-set c)
                    (string c)
                    (string-append "_" (number->string (char->integer c)))))
              (string->list (symbol->string s)))))

(define (prim-name op)
  (string-append "prim_" (c-name op)))
(define (applyprim-name op)
  (string-append "applyprim_" (c-name op)))

(define (c-hex-encode s-unknown)
  ; Hex encodes a string and adds a null terminator.
  ; Used for encoding global strings and symbols.
  (define s (if (string? s-unknown) s-unknown (symbol->string s-unknown)))
  (string-append (string-join (map (lambda (c)
                                     (format "\\~a"
                                             (~r (char->integer c)
                                                 #:base '(up 16) #:min-width 2 #:pad-string "0")))
                                   (string->list s)) "") "\\00"))

; because a `c-encode`d string is hex-encoded,
; we can just split on `\\` and get the length of that list.
(define (encoded-str-length s) (length (string-split s "\\")))

(define reserved-list '(letrec letrec* let let* if and or set! quote begin
                         cond case when unless delay force dynamic-wind
                         raise guard call/cc prim apply-prim))

(define (reserved? id) (if (member id reserved-list) #t #f))

(define (datum? d)
  (match d
    [`#(,(? datum?) ...) #t]
    [`(,(? datum?) ...) #t]
    [(? string?) #t]
    [(? integer?) #t]
    [(? symbol?) #t]
    [(? boolean?) #t]
    [else (pretty-print `(bad-datum ,d)) #f]))


(define (read-all [port (current-input-port)])
  (define e (read port))
  (if (eof-object? e)
      '()
      `(,e . ,(read-all port))))

(define (read-begin [port (current-input-port)])
  `(begin . ,(read-all port)))


(define (scheme-exp? e [env (set)])
  (define (var? x) (symbol? x))
  (define ((rec/with env) e)
    (scheme-exp? e env))
  (define (no-duplicates? lst)
    (= (set-count (list->set lst)) (length lst)))
  (define (ext env lst)
    (set-union env (list->set lst)))
  (define (improper-args? args)
    (if (var? args)
        #t
        (and (cons? args)
             (var? (car args))
             (improper-args? (cdr args)))))
  (define (cond-clause? cls)
    (match cls
      [`(,(? (rec/with env))) #t]
      [`(,(? (rec/with env)) ,(? (rec/with env))) #t]
      [else #f]))
  (define (case-clause? cls)
    (match cls
      [`((,(? datum?) ...) ,(? (rec/with env))) #t]
      [else #f]))
  (match e
    [`#(,(? datum?) ...) #t]
    [`(letrec* ([,(? var? xs) ,es] ...) ,e0)
     (and (no-duplicates? xs)
          (andmap (rec/with (ext env xs))
                  (cons e0 es)))]
    [`(letrec ([,(? var? xs) ,es] ...) ,e0)
     (and (no-duplicates? xs)
          (andmap (rec/with (ext env xs))
                  (cons e0 es)))]
    [`(let* () ,e0)
     ((rec/with env) e0)]
    [`(let* ([,x ,e0]) ,e1)
     ((rec/with env) `(let ([,x ,e0]) ,e1))]
    [`(let* ([,x ,e0] . ,rest) ,e1)
     ((rec/with env) `(let ([,x ,e0]) (let* ,rest ,e1)))]
    [`(let ([,(? symbol? xs) ,(? (rec/with env) es)] ...) ,e0)
     (and (no-duplicates? xs)
          ((rec/with (ext env xs)) e0))]
    [`(let ,(? var? lp)  ([,xs ,es] ...) ,e0)
     ((rec/with env) `(letrec ([,lp (lambda ,xs ,e0)]) (,lp . ,es)))]
    [`(lambda (,(? var? xs) ...) ,e0)
     (and (no-duplicates? xs)
          ((rec/with (ext env xs)) e0))]
    [`(lambda ,(? var? x) ,e0)
     ((rec/with (ext env (list x))) e0)]
    [`(lambda ,(? improper-args? args) ,e0)
     (and (no-duplicates? (flatten args))
          ((rec/with (ext env (flatten args))) e0))]
    [`(delay ,(? (rec/with env))) #t]
    [`(force ,(? (rec/with env))) #t]
    [`(guard (,(? symbol? x) ,clauses ...) ,(? (rec/with env)))
     (scheme-exp? `(cond . ,clauses) (ext env (list x)))]
    [`(raise ,(? (rec/with env))) #t]
    [`(dynamic-wind ,(? (rec/with env))
                    ,(? (rec/with env))
                    ,(? (rec/with env))) #t]
    [`(cond ,(? cond-clause?) ...) #t]
    [`(cond ,(? cond-clause?) ... (else ,(? (rec/with env)))) #t]
    [`(case ,(? (rec/with env)) ,(? case-clause?) ...) #t]
    [`(case ,(? (rec/with env)) ,(? case-clause?) ...
        (else ,(? (rec/with env)))) #t]
    [`(and ,(? (rec/with env)) ...) #t]
    [`(or ,(? (rec/with env)) ...) #t]
    [`(when ,(? (rec/with env)) ,(? (rec/with env))) #t]
    [`(unless ,(? (rec/with env)) ,(? (rec/with env))) #t]
    [`(if ,(? (rec/with env)) ,(? (rec/with env)) ,(? (rec/with env))) #t]
    [`(set! ,(? symbol?) ,(? (rec/with env))) #t]
    [`(begin ,(? (rec/with env)) ,(? (rec/with env)) ...) #t]
    [`(call/cc ,(? (rec/with env))) #t]
    [`(let/cc ,(? symbol? x) ,eb) ((rec/with (ext env (list x))) eb)]
    [(? var? x) (if (set-member? env x) #t (prim? x))]
    [`(quote ,(? datum?)) #t]
    [`(,(? prim?) ,(? (rec/with env)) ...) #t]
    [`(apply ,(? (rec/with env)) ,(? (rec/with env))) #t]
    [`(,(? (rec/with env)) ,(? (rec/with env)) ...) #t]
    [else (pretty-print `(bad-scheme ,e ,env)) #f]))

(define (eval-top-level e)
  (racket-compile-eval e))

(define (test-top-level top-level top-level-e)
  (define val1 (eval-top-level top-level-e))
  (define scm-e (top-level top-level-e))
  (define val2 (eval-scheme scm-e))
  (if (equal? val1 val2)
      #t
      (begin
        (display (format (string-append "Test-top-level: two different values"
                                        " (~a and ~a) before and after top-level processing\n")
                         val1 val2))
        #f)))

(define (eval-scheme e)
  (if (scheme-exp? e)
      (racket-compile-eval e)
      (error 'malformed-scheme)))

(define (test-desugar desugar scheme-prog)
  (define val1 (eval-scheme scheme-prog))
  (define ir-e (desugar scheme-prog))
  (define val2 (eval-ir ir-e))
  (if (equal? val1 val2)
      #t
      (begin
        (displayln (format (string-append "Test-desugar: two different values"
                                          " (~a and ~a) before and after desugaring")
                           val1 val2))
        #f)))

; tests if this expression conforms to
; what the desugar pass should output
; TODO improve this so it says what var isnt in scope?
(define (ir-exp? e [env (set)])
  (define (var? x) (symbol? x))
  (define ((rec/with env) e)
    (ir-exp? e env))
  (define (no-duplicates? lst)
    (= (set-count (list->set lst)) (length lst)))
  (define (ext env lst)
    (set-union env (list->set lst)))
  (match e
    [`(let ([,(? var? xs) ,(? (rec/with env) es)] ...) ,e0)
     (and (no-duplicates? xs)
          ((rec/with (ext env xs)) e0))]
    [`(lambda (,(? var? xs) ...) ,e0)
     (and (no-duplicates? xs)
          ((rec/with (ext env xs)) e0))]
    [`(lambda ,(? var? x) ,e0)
     ((rec/with (ext env (list x))) e0)]
    [`(apply ,(? (rec/with env)) ,(? (rec/with env))) #t]
    [`(if ,(? (rec/with env)) ,(? (rec/with env)) ,(? (rec/with env))) #t]
    [`(cond-bind ,xbnd ,(? (rec/with env)) ,(? (rec/with env)) ,(? (rec/with env)) ,ejoin)
     ((rec/with (ext env (list xbnd))) ejoin)]
    [`(set! ,(? symbol?) ,(? (rec/with env))) #t]
    [`(call/cc ,(? (rec/with env))) #t]
    [(? var? x) (if (set-member? env x) #t #f)]
    [`(quote ,(? datum?)) #t]
    [`(prim ,op ,(? (rec/with env)) ...)
     (if (prim? op) #t (begin (displayln `(op ,op doesnt-exist)) #f))]
    [`(apply-prim ,op ,(? (rec/with env)))
     (if (prim? op) #t (begin (displayln `(op ,op doesnt-exist)) #f))]
    [`(,(? (rec/with env)) ,(? (rec/with env)) ...) #t]
    [else (pretty-print `(bad-ir ,e ,env)) #f]))



(define (eval-ir e)
  (if (ir-exp? e)
      (racket-compile-eval e)
      (error 'malformed-ir)))


(define (test-alphatize box rename prog)
  (define val (eval-ir prog))
  (define boxed-e (box prog))
  (define alphatized-e (rename boxed-e))
  (define correct (alphatized-exp? alphatized-e))
  (if correct
      (if (equal? val (eval-ir alphatized-e))
          #t
          (begin
            (display (format (string-append "Test-alphatized: two different values (~a and ~a)"
                                            " before and after boxing and alphatizing.\n")
                             val (eval-ir alphatized-e)))
            #f))
      (begin
        (display "Output not in alphatized language.")
        #f)))


(define (alphatized-exp? e)
  (define seen (set))
  (define (not-seen-var? x)
    (define valid (and (var? x)  (not (set-member? seen x))))
    (set! seen (set-add seen x))
    valid)
  (define (var? x) (symbol? x))
  (define (no-duplicates? lst)
    (= (set-count (list->set lst)) (length lst)))
  (define (ext env lst)
    (set-union env (list->set lst)))
  (define (alpha? e)
    (match e
      [`(let ([,(? not-seen-var? xs) ,(? alpha? es)] ...) ,e0)
       (and (no-duplicates? xs)
            (alpha? e0))]
      [`(lambda (,(? not-seen-var? xs) ...) ,e0)
       (and (no-duplicates? xs)
            (alpha? e0))]
      [`(lambda ,(? not-seen-var? x) ,e0)
       (alpha? e0)]
      [`(apply ,(? alpha?) ,(? alpha?)) #t]
      [`(if ,(? alpha?) ,(? alpha?) ,(? alpha?)) #t]
      [`(call/cc ,(? alpha?)) #t]
      [(? var? x) #t]
      [`(quote ,(? datum?)) #t]
      [`(prim ,(? prim?) ,(? alpha?) ...) #t]
      [`(apply-prim ,(? prim?) ,(? alpha?)) #t]
      [`(,(? (and/c (not/c (lambda (x)
                             (member x '(let lambda apply if call/cc quote prim quote-prim))))
                    alpha?))
         ,(? alpha?) ...) #t]
      [_ (pretty-print `(bad-alphatized ,e)) #f]))
  (and (ir-exp? e) (alpha? e)))



(define (test-anf-convert anf-convert prog)
  (define val (eval-ir prog))
  (define anf-e (anf-convert prog))
  (define grammar? (anf-exp? anf-e))
  (if grammar?
      (let* ([anf-val (eval-ir anf-e)]
             [success (equal? val anf-val)])
        (unless success
          (displayln (format (string-append "Test-anf-convert: Two different values."
                                            " (~a and ~a) before and after anf conversion.")
                             val anf-val)))
        success)
      (displayln "Output from ANF conversion does not fit ANF grammar.")))

(define (anf? e)
  (match e
    [`(let ([,(? symbol?) ,(? anf?)]) ,(? anf?)) #t]
    [`(apply ,(? symbol?) ,(? symbol?)) #t]
    [`(prim ,(? prim?) . ,(? symbol?)) #t]
    [`(apply-prim ,(? prim?) ,(? symbol?)) #t]
    [`(if ,(? symbol?) ,(? anf?) ,(? anf?)) #t]
    [`(call/cc ,(? symbol?)) #t]
    [`(lambda (,(? symbol?) ...) ,(? anf?)) #t]
    [`(lambda ,(? symbol?) ,(? anf?)) #t]
    [`(quote ,_) #t]
    [`(,(? symbol?) ...) #t]
    [(? symbol?) #t]
    [_ #f]))
(define anf-exp? anf?)


(define (eval-proc proc)
  (if (proc-exp? proc)
      (racket-proc-eval proc)
      (error 'malformed-proc-ir)))


(define (proc-exp? ps)
  (define (proc? p)
    (match p
      [`(proc (,(? symbol?) ,(? symbol?) ,(? symbol?)) ,(? e?)) #t]
      [_ (pretty-display `(bad-proc ,p)) #f]))
  (define (e? e)
    (match e
      [`(let ([,(? symbol?) ,(? l?)]) ,(? e?)) #t]
      [`(if ,(? symbol?) ,(? e?) ,(? e?)) #t]
      [`(%%cond-bind ,(? symbol?) ,(? symbol?) ,(? e?) ,(? e?) ,(? e?)) #t]
      [(? r?) #t]
      [_ (pretty-display `(bad-e ,e)) #f]))
  (define (l? l)
    (match l
      [`(make-closure ,(? symbol?) ,(? symbol?) ...) #t]
      [`(env-ref ,(? symbol?) ,(? nonnegative-integer?)) #t]
      [`(quote ,(? datum?)) #t]
      [`(prim ,(? prim?) ,(? symbol?) ...) #t]
      [`(apply-prim ,(? prim?) ,(? symbol?)) #t]
      [(? r?) #t]
      [_ (pretty-display `(bad-l ,l)) #f]))
  (define (r? r)
    (match r
      [(? symbol?) #t]
      [`(call/cc ,(? symbol?)) #t]
      [`(clo-app ,(? symbol?) ,(? symbol?)) #t]
      [_ (pretty-display `(bad-r ,r)) #f]))
  (match ps
    [`(,(? proc?) ...) #t]
    [_ (pretty-display `(bad-procs ,ps)) #f]))

(define (test-closure-convert closure-convert prog)
  (define val (eval-ir prog))
  (define proc (closure-convert prog))
  (define grammar? (proc-exp? proc))
  (if grammar?
      (let* ([proc-val (eval-proc proc)]
             [success (equal? val proc-val)])
        (unless success
          (displayln (format (string-append "Test-closure-convert: Two different values. "
                                            "(~a and ~a) before and after closure conversion")
                             val proc-val)))
        success)
      (displayln "Output from closure conversion does not fit the proc-exp? grammar.")))

(define (lir-exp? ps)
  (define (p? p)
    (match p
      [`(proc (,(? symbol?) ,(? nonnegative-integer?)
                            ,(? symbol?) ,(? symbol?))
              ,(? e?)) #t]
      [_ (pretty-display `(bad-proc ,p)) #f]))
  (define (e? e)
    (match e
      [`(,(? i?) ... (return ,(? r?))) #t]
      [_ (pretty-display `(bad-e ,e)) #f]))
  (define (i? i)
    (match i
      [`(bind ,(? symbol?) ,(? l?)) #t]
      [`(if ,(? symbol?) (label ,(? symbol?)) (label ,(? symbol?))) #t]
      [`(jump (label ,(? symbol?))) #t]
      [`(label ,(? symbol?)) #t]
      [`(return ,(? r?)) #t]
      [_ (pretty-display `(bad-i ,i)) #f]))
  (define (l? l)
    (match l
      [`(make-closure ,(? symbol?) ,(? symbol?) ...) #t]
      [`(env-ref ,(? symbol?) ,(? nonnegative-integer?)) #t]
      [`(quote ,(? datum?)) #t]
      [`(prim ,(? prim?) ,(? symbol?) ...) #t]
      [`(apply-prim ,(? prim?) ,(? symbol?)) #t]
      [`(phi (,(? symbol?) (label ,(? symbol?))) (,(? symbol?) (label ,(? symbol?)))) #t]
      [(? r?) #t]
      [_ (pretty-display `(bad-l ,l)) #f]))
  (define (r? r)
    (match r
      [(? symbol?) #t]
      [`(call/cc ,(? symbol?)) #t]
      [`(clo-app ,(? symbol?) ,(? symbol?)) #t]
      [_ (pretty-display `(bad-r ,r)) #f]))
  (match ps
    [`(,(? p?) ...) #t]
    [_ (pretty-display `(bad-procs ,ps)) #f]))


(define (racket-compile-eval e)
  (define (rewrite-match e)
    ; Hacky way to get match to raise a srfi/34 exception when it fails
    (match e [`(match ,e0 ,clauses ... (,pat0 ,es ...))
              #:when (not (equal? pat0 'else))
              (rewrite-match `(match ,e0 ,@clauses (,pat0 ,@es) (else (raise "no match"))))]
      [`(,e0 . ,es) (cons (rewrite-match e0) (rewrite-match es))]
      [_ e]))
  (with-handlers ([exn:fail? (lambda (x) (pretty-print "Evaluation failed:")
                               (pretty-display x)
                               (pretty-print e)
                               (error 'eval-fail))])
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'rnrs)
      (namespace-require 'racket)
      (namespace-require 'srfi/34)
      (eval (compile
             `(call/cc (lambda (exit+)
                         (define halt exit)
                         (define (prim op . args) (apply op args))
                         (define (apply-prim op args) (apply op args))
                         (define (drop xs n)
                           (pretty-display `(calling-drop ,args)) (drop xs n))
                         ,(rewrite-match e))))))))


(define (racket-proc-eval procs)
  (with-handlers ([exn:fail? (lambda (x) (begin (pretty-display "Evaluation failed:")
                                                (pretty-display x)
                                                (pretty-display procs)
                                                (error 'eval-fail)))])
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'rnrs)
      (namespace-require 'racket)
      (namespace-require 'srfi/34)
      (eval (compile
             `(begin
                (call/cc (lambda (exit+)
                           (define-syntax-rule (%%cond-bind xb xc et ef ej)
                             (let ([xb (if xc et ef)]) ej))
                           (define (halt x) (exit+ x))
                           (define datum (lambda (d) d))
                           (define (prim op . args) (apply op args))
                           (define apply-prim apply)
                           (struct closure (lam env) #:transparent)
                           (define (make-closure lam . frees)
                             (closure lam (list->vector frees)))
                           (define (env-ref clo n)
                             (match-define (closure _ env) clo)
                             (vector-ref env n))
                           (define (clo-app f x)
                             (match-define (closure lam _) f)
                             (lam f x))
                           (define (call/cc clo)
                             (call-with-current-continuation
                              (lambda (k) (clo-app clo
                                                   ; gotta do weird list/car stuff
                                                   ; cause explicit arg-lists.
                                                   (list (closure (λ (f x) (k (car x)))
                                                                  (set)))))))
                           ,@(map (match-lambda [`(proc (,xname ,xclo ,xs) ,bdy)
                                                 `(define (,xname ,xclo ,xs) ,bdy)])
                                  procs)
                           (__main '() '())))))))))
