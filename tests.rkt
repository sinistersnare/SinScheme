#lang racket

;; Testing apparatus modified from assignments
;; Its pretty bad, i hacked it to work for this... so yeah

(require (only-in "src/racket/utils.rkt"
                  test-desugar ; desugar testing
                  test-alphatize ; alphatize testing
                  test-anf-convert ; anf testing
                  test-cps-convert ; cps testing
                  test-closure-convert ; closure-convert testing
                  eval-proc ; llvm testing
                  read-begin eval-top-level))

(require (only-in "src/racket/desugar.rkt" desugar))

(require (only-in "src/racket/alphatize.rkt" alphatize))
(require (only-in "src/racket/assignment-convert.rkt" assignment-convert))

(require (only-in "src/racket/anf.rkt" anf-convert))
(require (only-in "src/racket/cps.rkt" cps-convert))
(require (only-in "src/racket/closure-convert.rkt" closure-convert))
(require (only-in "src/racket/llvm-convert.rkt" llvm-convert))

(require (only-in "compiler.rkt" gen-build-file scm->exe llvm->exe))

; compiles and interprets the code and calls the callback
; giving it the compiled and interpreted results as strings (in that order).
(define (compile-and-interpret code-str fin)
  ; get value when we interpret the input
  (define interpreted-input (read-begin (open-input-string code-str)))
  (define interpreted-eval (eval-top-level interpreted-input))
  (define interpreted-value (~a interpreted-eval))
  ; get value when we compile and execute the input
  (define compiler-input (open-input-string code-str))
  (define generated-name (gensym 'generated))
  (define header-name (gen-build-file generated-name ".ll"))
  (define exe-name (gen-build-file generated-name ".x"))
  (scm->exe (open-input-string code-str) exe-name)
  (define compiled-value
    (string-normalize-spaces (with-output-to-string (λ () (system (format "./~a" exe-name))))))
  ; call the callback with the differente values
  (fin compiled-value interpreted-value))

(define (test-llvm-convert llvm-convert prog)
  (define interpreted-val (~a (eval-proc prog)))
  (define llvm (llvm-convert prog))
  (define exe-name (gen-build-file (gensym 'testllvm) ".x"))
  (llvm->exe llvm exe-name)
  ; if this function gets any more complicated we should refactor and share code with the
  ; compile-and-run function.
  (define compiled-val
    (string-normalize-spaces (with-output-to-string (λ () (system (format "./~a" exe-name))))))
  (define success (equal? interpreted-val compiled-val))
  (unless success (displayln (format "llvm:'~a'\ninterpreted:'~a'\n" compiled-val interpreted-val)))
  success)

; Single test creation.

(define (new-failing-test test-file-path)
  (lambda ()
    (define didnt-fail-tag (gensym 'did-not-fail))
    (define file-contents (file->string test-file-path))
    (define guarded-code `(guard (_ [',didnt-fail-tag #t] [else #f])
                                 ,file-contents (raise ',didnt-fail-tag)))
    (define guarded-string (~s guarded-code))
    (compile-and-interpret
     guarded-string
     (λ (c i)
       (define success (and (equal? "#t" c) (equal? "#t" i)))
       (unless success (displayln (format "llvm:~a\ntop-level:~a" c i)))
       success))))

(define (new-passing-test test-file-path)
  (lambda ()
    (compile-and-interpret
     (file->string test-file-path)
     (λ (c i)
       ; convert the interpreted output to a string to compare.
       (define success (equal? c i))
       (unless success (displayln (format "llvm:~a\ntop-level:~a" c i)))
       success))))

; TODO: move testing functions from utils.rkt to here?
; Or maybe move the testing functionality into the tests/ folder, and this just be the API?
(define (new-pass-test test-file-path f)
  (lambda ()
    (define test-contents (read (open-input-string (file->string test-file-path))))
    (with-handlers ([exn:fail? (λ (ex) (displayln ex) #f)])
      (f test-contents))))

(define (new-desugar-test p) (new-pass-test p (λ (c) (test-desugar desugar c))))
(define (new-alphatize-test p)
  (new-pass-test p (λ (c) (test-alphatize assignment-convert alphatize c))))
(define (new-anf-test p) (new-pass-test p (λ (c) (test-anf-convert anf-convert c))))
(define (new-cps-test p) (new-pass-test p (λ (c) (test-cps-convert cps-convert c))))
(define (new-clo-test p) (new-pass-test p (λ (c) (test-closure-convert closure-convert c))))
(define (new-llvm-test p) (new-pass-test p (λ (c) (test-llvm-convert llvm-convert c))))

; test suite creation

(define (get-tests-at testsloc filetype)
  (map (λ (p) (string->path (string-append testsloc (path->string p))))
       (filter (λ (p) (equal? (last (string-split (path->string p) ".")) filetype))
               (directory-list testsloc))))

; makes a suite of tests that can each be executed.
; prepends each test name with the prefix.
(define (make-suite prefix make-test test-list)
  ; takes a path like /a/b/c/d.scm and returns 'd'
  (define (extract-filename p)
    (last (string-split (string-join (drop-right (string-split (path->string p) ".") 1) ".") "/")))
  (map (λ (p) (list (string-append prefix (extract-filename p)) (make-test p))) test-list))

(define passing-tests (make-suite "passing-" new-passing-test
                                  (get-tests-at "tests/passing/" "sinscm")))
(define failing-tests (make-suite "failing-" new-failing-test
                                  (get-tests-at "tests/failing/" "sinscm")))
(define desugar-tests (make-suite "desugar-" new-desugar-test
                                  (get-tests-at "tests/passes/desugar/" "scm")))
(define alphatize-tests (make-suite "alpha-" new-alphatize-test
                                    (get-tests-at "tests/passes/alphatize/" "ir")))
(define anf-tests (make-suite "anf-" new-anf-test
                              (get-tests-at "tests/passes/anf/" "alpha")))
(define cps-tests (make-suite "cps-" new-cps-test
                              (get-tests-at "tests/passes/cps/" "anf")))
(define clo-tests (make-suite "clo-" new-clo-test
                              (get-tests-at "tests/passes/clo/" "cps")))
(define llvm-tests (make-suite "llvm-" new-llvm-test
                               (get-tests-at "tests/passes/llvm/" "proc")))

(define (run-single testcase)
  (match-define (list test-name exec) testcase)
  (define exec-result
    (with-handlers ([exn:fail? (lambda (ex) (displayln ex) #f)]) (exec)))
  (define passed (eq? exec-result #t))
  (displayln (format "Test '~a' ~a." test-name
                     (if passed "passed" "failed")))
  passed)

(define (run-suite suite)
  (define failed-tests
    (foldl (λ (testcase fails)
             (if (run-single testcase) fails (cons (car testcase) fails)))
           '() suite))
  (displayln (format "Score on tests: ~a/~a"
                     (- (length suite) (length failed-tests))
                     (length suite)))
  (unless (empty? failed-tests)
    (displayln (format "Failed: [~a]" (string-join failed-tests ", ")))))

(define all-tests (append passing-tests failing-tests
                          desugar-tests alphatize-tests
                          anf-tests cps-tests
                          clo-tests llvm-tests))
(define (run-test/internal is-repl . args)
  ;; Run all tests, a specific test or suite, or print the available tests
  (match args
    [(list "all") (run-suite all-tests)]
    [(list "passing") (run-suite passing-tests)]
    [(list "failing") (run-suite failing-tests)]
    [(list "desugar") (run-suite desugar-tests)]
    [(list "alpha") (run-suite alphatize-tests)]
    [(list "anf") (run-suite anf-tests)]
    [(list "cps") (run-suite cps-tests)]
    [(list "clo") (run-suite clo-tests)]
    [(list "llvm") (run-suite llvm-tests)]
    [(list test-name)
     #:when (assoc test-name all-tests)
     (define passed (run-single (assoc test-name all-tests)))
     (unless is-repl
       (exit (if passed 0 1)))]
    [(list unknown)
     (displayln (string-append "unknown test name: '" unknown "'"))]
    ['()
     (displayln "Available tests: ")
     (displayln (string-join (map car all-tests) ", "))]))

(define run-test
  (curry run-test/internal #t))

(define r run-test) ; for easier repl usage

(apply run-test/internal
       (cons #f (vector->list (current-command-line-arguments))))
