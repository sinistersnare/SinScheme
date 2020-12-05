#lang racket

; Written by Thomas Gilray and Javran Cheng

(provide prim? reserved? prims->list
         c-name
         prim-name
         prim-applyname
         datum?
         read-begin
         test-top-level
         test-desugar
         eval-top-level
         eval-scheme
         eval-ir
         eval-proc
         eval-llvm
         simplify-ir
         scheme-exp?
         ir-exp?
         alphatized-exp?
         test-alphatize
         anf-exp?
         test-anf-convert
         cps-exp?
         test-cps-convert
         proc-exp?
         test-closure-convert
         test-proc->llvm)

(define project-path (current-directory))
(define libgc-path
  (path->string
   (build-path project-path "lib" "local" "lib" "libgc.a")))
(define gc-include-path
  (path->string
   (build-path project-path "lib" "local" "include")))

(define clang++-path
  (let ([clang++-path-submit-server "/opt/llvm-3.9.0/bin/clang++"])
    (if (file-exists? clang++-path-submit-server)
        clang++-path-submit-server
        "clang++")))

; TODO: add boolean? prim, make sure runtime has support for it.
(define prims-list
  '(= > < <= >= + - * / promise?
      cons? null? cons car cdr list first second third fourth fifth
      length list-tail drop take member memv map append foldl foldr
      vector? vector make-vector vector-ref vector-set! vector-length
      set set->list list->set set-add set-union set-count set-first
      set-rest set-remove hash hash-ref hash-set hash-count hash-keys
      hash-has-key? hash? list? void? procedure? number? integer?
      error void print println display write exit halt eq? eqv? equal? not))

(define ok-set (list->set (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789$")))

(define (c-name s)
  (foldr string-append
         ""
         (map (lambda (c)
                (if (set-member? ok-set c)
                    (string c)
                    (string-append "_" (number->string (char->integer c)))))
              (string->list (symbol->string s)))))
(define (prim-name op)
  (string-append "prim_" (c-name op)))
(define (prim-applyname op)
  (string-append "applyprim_" (c-name op)))
(define (prims->list) prims-list)
(define (prim? op)
  (if (member op prims-list)
      #t
      #f))

(define reserved-list '(letrec letrec* let let* if and or set! quote begin
                               cond case when unless delay force dynamic-wind
                               raise guard call/cc prim apply-prim))

(define (reserved? id) (if (member id reserved-list) #t #f))

(define (datum? d)
  (match d
    [`#(,(? datum?) ...) #t]
    [`(,(? datum?) ...) #t]
    [(cons datum? datum?) #t]
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
        (display (format "Test-top-level: two different values (~a and ~a) before and after top-level processing\n"
                         val1 val2))
        #f)))

(define (eval-scheme e)
  (if (scheme-exp? e)
      (racket-compile-eval e)
      (error 'malformed-scheme)))

(define (test-desugar desugar scheme-prog)
  (define val1 (eval-scheme scheme-prog))
  (define ir-e (simplify-ir (desugar scheme-prog)))
  (define val2 (eval-ir ir-e))
  (if (equal? val1 val2)
      #t
      (begin
        (displayln (format "Test-desugar: two different values (~a and ~a) before and after desugaring"
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
    [`(set! ,(? symbol?) ,(? (rec/with env))) #t]
    [`(call/cc ,(? (rec/with env))) #t]
    [(? var? x) (if (set-member? env x) #t #f)]
    [`(quote ,(? datum?)) #t]
    [`(prim ,(? prim?) ,(? (rec/with env)) ...) #t]
    [`(apply-prim ,(? prim?) ,(? (rec/with env))) #t]
    [`(,(? (rec/with env)) ,(? (rec/with env)) ...) #t]
    [else (pretty-print `(bad-ir ,e ,env)) #f]))


(define (simplify-ir ir-e)
  (define (T e)
    (match e
      [`(let ([,xs ,es] ...) ,e0)
       `(let ,(map list xs (map T es)) ,(T e0))]
      [`(lambda ,xs ,e0)
       `(lambda ,xs ,(T e0))]
      [`(apply ,e0 ,e1)
       `(apply ,(T e0) ,(T e1))]
      [`(if ,e0 ,e1 ,e2)
       `(if ,(T e0) ,(T e1) ,(T e2))]
      [`(set! ,x ,e0)
       `(set! ,x ,(T e0))]
      [`(call/cc ,e0)
       `(call/cc ,(T e0))]
      [(? symbol? x)
       x]
      [`(quote ,(? symbol? x))
       `(quote ,x)]
      [`(quote #(,ds ...))
       (T `(prim vector ,@(map (lambda (d) `',d) ds)))]
      [`(quote ,(? list? lst))
       (T `(prim list ,@(map (lambda (d) `',d) lst)))]
      [`(quote ,other)
       `(quote ,other)]

      [`(prim +)
       ''0]
      [`(prim + ,e0)
       (T e0)]
      [`(prim + ,e0 ,e1 ,e2 ,es ...)
       (T `(prim + ,e0 (prim + ,e1 ,e2 ,@es)))]
      [`(prim *)
       ''1]
      [`(prim * ,e0)
       (T e0)]
      [`(prim * ,e0 ,e1 ,e2 ,es ...)
       (T `(prim * ,e0 (prim * ,e1 ,e2 ,@es)))]
      [`(prim - ,e0 ,es ...)
       #:when (> (length es) 1)
       (T `(prim - (prim - ,e0 ,@(drop-right es 1)) ,(last es)))]
      [`(prim / ,e0 ,e1)
       `(prim / ,(T e0) ,(T e1))]

      ; Remove list, vector->apply vector, map foldl, foldr, drop, memv, >, >=, ...
      [`(prim list ,es ...) ; optimize
       `((lambda lst lst) ,@(map T es))]
      [`(apply-prim list ,e0)
       (T e0)]
      [`(prim vector ,es ...)
       (T `((lambda el (apply-prim vector el)) ,@es))]
      [`(prim foldl ,e0 ,e1 ,e2)
       `(%foldl1 ,@(map T (list e0 e1 e2)))]
      [`(prim foldr ,e0 ,e1 ,e2)
       `(%foldr1 ,@(map T (list e0 e1 e2)))]
      [`(prim map ,e0 ,e1)
       `(%map1 ,@(map T (list e0 e1)))]

      [`(prim ,op ,es ...)
       #:when (member op '(drop memv / > >= list? drop-right length append last
                                map foldl foldr first second third fourth))
       `(,(string->symbol (string-append "%" (symbol->string op))) ,@(map T es))]
      [`(apply-prim ,op ,e0)
       #:when (member op '(drop memv / > >= list? drop-right length append last
                                map foldl foldr first second third fourth))
       `(apply ,(string->symbol (string-append "%" (symbol->string op))) ,(T e0))]

      [`(prim ,op ,es ...)
       `(prim ,op ,@(map T es))]
      [`(apply-prim ,op ,e0)
       `(apply-prim ,op ,(T e0))]
      [`(,ef ,es ...)
       `(,(T ef) ,@(map T es))]))
  `(let ([Ycmb
          ((lambda (yu) (yu yu))
           (lambda (y) (lambda (f) (f (lambda args (apply ((y y) f) args))))))])
     (let ([%foldr1
            (Ycmb
             (lambda (%foldr1)
               (lambda (f acc lst)
                 (if (prim null? lst)
                     acc
                     (f (prim car lst)
                        (%foldr1 f acc (prim cdr lst)))))))]
           [%map1 (Ycmb (lambda (%map) (lambda (f lst) (if (prim null? lst)
                                                           '()
                                                           (prim cons (f (prim car lst))
                                                                 (%map f (prim cdr lst)))))))]
           [%take
            (Ycmb (lambda (%take) (lambda (lst n) (if (prim = n '0) '() (if (prim null? lst) '()
                                                                            (prim cons (prim car lst) (%take (prim cdr lst) (prim - n '1))))))))]
           [%length (Ycmb (lambda (%length) (lambda (lst) (if (prim null? lst) '0 (prim + '1 (%length (prim cdr lst)))))))]
           [%foldl1
            (Ycmb
             (lambda (%foldl1)
               (lambda (f acc lst)
                 (if (prim null? lst)
                     acc
                     (%foldl1 f (f (prim car lst) acc) (prim cdr lst))))))])
       (let ([%last (lambda (lst) (%foldl1 (lambda (x y) x) '() lst))]
             [%drop-right (lambda (lst n) (%take lst (prim - (%length lst) n)))]
             [%foldr
              (Ycmb
               (lambda (%foldr)
                 (lambda args
                   (let ([f (prim car args)]
                         [acc (prim car (prim cdr args))]
                         [lsts (prim cdr (prim cdr args))])
                     (if (%foldr1 (lambda (lst b) (if b b (prim null? lst))) '#f lsts)
                         acc
                         (let ([lsts+ (%map1 (lambda (x) (prim cdr x)) lsts)]
                               [vs (%map1 (lambda (x) (prim car x)) lsts)])
                           (apply f (%foldr1 (lambda (a b) (prim cons a b))
                                             (prim cons
                                                   (apply %foldr (prim cons f (prim cons acc lsts+)))
                                                   '())
                                             vs))))))))])
         (let ([%map1
                (lambda (f lst)
                  (%foldr1 (lambda (v r) (prim cons (f v) r)) '() lst))]
               [%map
                (lambda args
                  (let ([f (prim car args)]
                        [lsts (prim cdr args)])
                    (apply %foldr (prim cons
                                        (lambda fargs
                                          (prim cons
                                                (apply f (%drop-right fargs '1))
                                                (%last fargs)))
                                        (prim cons '() lsts)))))])
           (let ([%foldl
                  (Ycmb
                   (lambda (%foldl)
                     (lambda args
                       (let ([f (prim car args)]
                             [acc (prim car (prim cdr args))]
                             [lsts (prim cdr (prim cdr args))])
                         (if (%foldr1 (lambda (lst b) (if b b (prim null? lst))) '#f lsts)
                             acc
                             (let ([lsts+ (%map1 (lambda (x) (prim cdr x)) lsts)]
                                   [vs (%map1 (lambda (x) (prim car x)) lsts)])
                               (let ([acc+ (apply f (%foldr (lambda (a b) (prim cons a b)) (prim cons acc '()) vs))])
                                 (apply %foldl (prim cons f (prim cons acc+ lsts+))))))))))]
                 [%>
                  (lambda (a b) ; we'll assume comparitors are binary, but we could tweak this here
                    (prim not (prim <= a b)))]
                 [%>=
                  (lambda (a b)
                    (prim not (prim < a b)))]
                 [%append (let ([%append '()])
                            (let ([_0 (set! %append (lambda (ls0 ls1)
                                                      (if (prim null? ls0)
                                                          ls1
                                                          (prim cons (prim car ls0) (%append (prim cdr ls0) ls1)))))])
                              %append))]
                 [%list?
                  (lambda (a)
                    (let ([cc (call/cc (lambda (k) k))])
                      (if (prim null? a)
                          '#t
                          (if (prim cons? a)
                              (let ([b (prim cdr a)])
                                (let ([_0 (set! a (prim cdr a))])
                                  (cc cc)))
                              '#f))))]
                 [%drop
                  (lambda (lst n)
                    (let ([cc (call/cc (lambda (u) (u u)))])
                      (if (prim = '0 n)
                          lst
                          (let ([_0 (set! lst (prim cdr lst))]
                                [_1 (set! n (prim - n '1))])
                            (cc cc)))))]
                 [%memv
                  (lambda (v lst)
                    (let ([cc (call/cc (lambda (u) (u u)))])
                      (if (prim null? lst)
                          '#f
                          (if (prim eqv? (prim car lst) v)
                              lst
                              (let ([_0 (set! lst (prim cdr lst))])
                                (cc cc))))))]
                 [%/ (lambda args (if (prim null? args) '1
                                      (if (prim null? (prim cdr args))
                                          (prim car args)
                                          (%foldl1 (lambda (n v) (prim / v n)) (prim car args) (prim cdr args)))))]
                 [%first (lambda (x) (prim car x))]
                 [%second (lambda (x) (prim car (prim cdr x)))]
                 [%third (lambda (x) (prim car (prim cdr (prim cdr x))))]
                 [%fourth (lambda (x) (prim car (prim cdr (prim cdr (prim cdr x)))))])
             ,(T ir-e)))))))


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
            (if correct
                (display (format "Test-alphatized: two different values (~a and ~a) before and after boxing and alphatizing.\n"
                                 val (eval-ir alphatized-e)))
                (display "Output from boxing and alphatizing does not fit the output grammar.\n"))
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
      [`(,(? (and/c (not/c (lambda (x) (member x '(let lambda apply if call/cc quote prim quote-prim))))
                    alpha?))
         ,(? alpha?) ...) #t]
      [else (pretty-print `(bad-alphatized ,e)) #f]))
  (and (ir-exp? e) (alpha? e)))



(define (test-anf-convert anf-convert prog)
  (define val (eval-ir prog))
  (define anf-e (anf-convert prog))
  (define correct (anf-exp? anf-e))
  (if (and correct (equal? val (eval-ir anf-e)))
      #t
      (begin
        (if correct
            (display (format "Test-anf-convert: two different values (~a and ~a) before and after anf conversion.\n"
                             val (eval-ir anf-e)))
            (display "Output from anf conversion does not fit ANF grammar.\n"))
        #f)))


(define (anf-exp? e)
  (define (a-exp? e)
    (match e
      [`(lambda ,xs ,(? c-exp? e0)) #t]
      [`',dat #t]
      [(? symbol? x) #t]
      [else #f]))
  (define (c-exp? e)
    (match e
      [`(let ([,(? symbol? x) ,(? c-exp? rhs)]) ,(? c-exp? e0)) #t]
      [`(if ,(? a-exp? ae) ,(? c-exp? e0) ,(? c-exp? e1)) #t]
      [`(prim ,op ,(? a-exp? aes) ...) #t]
      [`(apply-prim ,op ,(? a-exp? ae)) #t]
      [`(call/cc ,(? a-exp? ae)) #t]
      [`(apply ,(? a-exp? aes) ,(? a-exp? aes)) #t]
      [`(,(? (and/c (not/c reserved?) a-exp?) aef) ,(? a-exp? aes) ...) #t]
      [(? a-exp? e) #t]
      [else (pretty-print `(bad-anf ,e)) #f]))
  (and (ir-exp? e) (c-exp? e)))



(define (test-cps-convert cps-convert prog)
  (define val (eval-ir prog))
  (define cps-e (cps-convert prog))
  (define correct (cps-exp? cps-e))
  (if (and correct (equal? val (eval-ir cps-e)))
      #t
      (begin
        (if correct
            (display (format "Test-cps-convert: two different values (~a and ~a) before and after cps conversion.\n"
                             val (eval-ir cps-e)))
            (display "Output from cps conversion does not fit the CPS grammar.\n"))
        #f)))



(define (cps-exp? e)
  (define (a-exp? e)
    (match e
      [`(lambda ,xs ,(? c-exp? e0)) #t]
      [`',dat #t]
      [(? symbol? x) #t]
      [else (pretty-print `(bad-cps-ae ,e)) #f]))
  (define (c-exp? e)
    (match e
      [`(let ([,(? symbol? x) (prim ,op ,(? a-exp? aes) ...)]) ,(? c-exp? e0)) #t]
      [`(let ([,(? symbol? x) (apply-prim ,op ,(? a-exp? ae))]) ,(? c-exp? e0)) #t]
      [`(let ([,(? symbol? x) (lambda ,xs ,(? c-exp? lam-e))]) ,(? c-exp? e0)) #t]
      [`(let ([,(? symbol? x) ',dat]) ,(? c-exp? e0)) #t]
      [`(if ,(? a-exp? ae) ,(? c-exp? e0) ,(? c-exp? e1)) #t]
      [`(apply ,(? a-exp? aes) ,(? a-exp? aes)) #t]
      [`(,(? (and/c (not/c reserved?) a-exp?) aef) ,(? a-exp? aes) ...) #t]
      [else (pretty-print `(bad-cps-e ,e)) #f]))
  (and (anf-exp? e) (c-exp? e)))



(define (eval-proc proc)
  (if (proc-exp? proc)
      (racket-proc-eval proc)
      (error 'malformed-proc-ir)))



(define (proc-exp? e)
  (define (c-exp? e)
    (match e
      [`(let ([,(? symbol? x) (make-closure ,(? symbol? xs) ...)]) ,(? c-exp? e0)) #t]
      [`(let ([,(? symbol? x) (env-ref ,(? symbol?) ,(? integer?))]) ,(? c-exp? e0)) #t]
      [`(let ([,(? symbol? x) (prim ,op ,(? symbol? xs) ...)]) ,(? c-exp? e0)) #t]
      [`(let ([,(? symbol? x) (apply-prim ,op ,(? symbol? y))]) ,(? c-exp? e0)) #t]
      [`(let ([,(? symbol? x) ',dat]) ,(? c-exp? e0)) #t]
      [`(if ,(? symbol? x) ,(? c-exp? e0) ,(? c-exp? e1)) #t]
      [`(clo-app ,(? symbol? f) ,(? symbol? xs) ...) #t]
      [else (pretty-print `(bad-proc-e ,e)) #f]))
  (match e
    [`((proc (,(? symbol? xs) ...) ,(? c-exp? e)) ...) #t]
    [else (pretty-print `(bad-proc ,e)) #f]))

(define recent-header #f)
(define (eval-llvm llvm-str)
  ; freshly compile the header / runtime library if not already
  (when (not recent-header)
    (set! recent-header #t)
    ;(system (string-append clang++-path " header.cpp " " -I " gc-include-path " -S -emit-llvm -o header.ll"))
    (system (string-append clang++-path " src/cpp/header.cpp " " -S -emit-llvm -o build/header.ll")))
  (define header-str (read-string 299999 (open-input-file "build/header.ll" #:mode 'text)))
  (define llvm (string-append header-str "\n\n;;;;;;\n\n" llvm-str))
  (display llvm (open-output-file "combined.ll" #:exists 'replace))
  ;(system (string-append clang++-path " combined.ll " libgc-path " -I " gc-include-path " -lpthread -o bin"))
  (system (string-append clang++-path " combined.ll " " -o bin"))
  (match-define `(,out-port ,in-port ,id ,err-port ,callback) (process "./bin"))
  (define starttime (current-milliseconds))
  (let loop ()
    ;(sleep 1)
    (define time (current-milliseconds))
    (define status (callback 'status))
    (if (> time (+ starttime 12000))
        (begin (pretty-print '(eval-llvm "binary execution timed out")) (void))
        (if (eq? status 'done-ok)
            ; use a read to turn the printed value back into a racket value
            (let ([v (eval (read out-port) (make-base-namespace))])
              (callback 'kill)
              v)
            (if (eq? status 'done-error)
                (begin (pretty-print '(eval-llvm "bad status code")) (void))
                (loop))))))


(define (test-closure-convert closure-convert prog)
  (define val (eval-ir prog))
  (define proc (closure-convert prog))
  (define correct (proc-exp? proc))
  (if (and correct (equal? val (eval-proc proc)))
      #t
      (begin
        (if correct
            (display (format "Test-closure-convert: two different values (~a and ~a) before and after closure conversion.\n"
                             val (eval-proc proc)))
            (display "Output from closure conversion does not fit the proc-exp? grammar.\n"))
        #f)))




(define (test-proc->llvm proc->llvm prog)
  (define val (eval-proc prog))
  (define llvm (proc->llvm prog))
  (define val0 (eval-llvm llvm))
  (if (equal? val val0)
      #t
      (begin
        (display (format "Test-proc->llvm: two different values (~a and ~a) before and after closure conversion.\n"
                         val val0))
        #f)))




(define (racket-compile-eval e)
  (define (rewrite-match e)
    ; Hacky way to get match to raise a srfi/34 exception when it fails
    (match e [`(match ,e0 ,clauses ... (,pat0 ,es ...))
              #:when (not (equal? pat0 'else))
              (rewrite-match `(match ,e0 ,@clauses (,pat0 ,@es) (else (raise "no match"))))]
           [`(,e0 . ,es) (cons (rewrite-match e0) (rewrite-match es))]
           [else e]))
  (with-handlers ([exn:fail? (lambda (x) (pretty-print "Evaluation failed:") (pretty-print x) (pretty-print e) (error 'eval-fail))])
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'rnrs)
      (namespace-require 'racket)
      (namespace-require 'srfi/34)
      (eval (compile
             `(call/cc (lambda (exit+)
                         (define (halt x) (exit+ x))
                         (define (prim op . args) (apply op args))
                         (define (apply-prim op args) (apply op args))
                         ,(rewrite-match e))))))))



(define (racket-proc-eval procs)
  (with-handlers ([exn:fail? (lambda (x) (begin (pretty-print "Evaluation failed:") (pretty-print x) (pretty-print procs) (error 'eval-fail)))])
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'rnrs)
      (namespace-require 'racket)
      (namespace-require 'srfi/34)
      (eval (compile
             `(begin
                (call/cc (lambda (exit+)
                           (define (halt x) (exit+ x))
                           (define datum (lambda (d) d))
                           (define (prim op . args) (apply op args))
                           (define (apply-prim op args) (apply op args))
                           (define (procedure? p) (and (vector? p)
                                                       (equal? '%clo (vector-ref p (- (vector-length p) 1)))))
                           (define (make-closure . args) (list->vector (append args '(%clo))))
                           (define env-ref vector-ref)
                           (define (clo-app f . vs) (apply (vector-ref f 0) (cons f vs)))
                           ,@(map (match-lambda [`(proc (,xs ...) ,bdy) `(define ,xs ,bdy)]) procs)
                           (main)))))))))
