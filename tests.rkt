#lang racket

;; Testing apparatus modified from assignments
;; Its pretty bad, i hacked it to work for this... so yeah

(require (only-in "src/racket/utils.rkt"
                  test-desugar test-alphatize
                  test-anf-convert test-cps-convert
                  test-closure-convert test-llvm-convert
                  read-begin eval-top-level))

(require (only-in "src/racket/desugar.rkt" desugar))

(require (only-in "src/racket/alphatize.rkt" alphatize))
(require (only-in "src/racket/assignment-convert.rkt" assignment-convert))

(require (only-in "src/racket/anf.rkt" anf-convert))
(require (only-in "src/racket/cps.rkt" cps-convert))
(require (only-in "src/racket/closure-convert.rkt" closure-convert))
(require (only-in "src/racket/llvm-convert.rkt" llvm-convert))

(require (only-in "compiler.rkt"
                  gen-header-name
                  libgc-include-dir libgc-obj-path
                  clang++-path compiler-flags scm->exe))

(define (new-failing-test test-file-path)
  (lambda ()
    (define didnt-fail-tag (gensym 'did-not-fail))
    (define file-contents (file->string test-file-path))
    (define guarded-code `(guard (_ [',didnt-fail-tag #t] [else #f])
                                 ,file-contents (raise ',didnt-fail-tag)))
    (define guarded-string (symbol->string guarded-code))
    (define top-level-part-port (open-input-string guarded-string))
    (define top-level-part-scm (read-begin top-level-part-port))
    (define top-level-part-value (eval-top-level top-level-part-scm))

    (define llvm-part-port (open-input-string (symbol->string guarded-string)))
    (define gen-ll-name (gen-header-name))
    (define gen-exe-name (string-append "build/" (symbol->string (gensym 'genexe)) ".exe"))
    (scm->exe llvm-part-port gen-exe-name clang++-path compiler-flags
              libgc-obj-path libgc-include-dir gen-ll-name)
    (define llvm-part-value
      (read (open-input-string
             (with-output-to-string (lambda () (system (format "./~a" gen-exe-name)))))))
    ; check that top-level and compiled codes return the same.
    (and top-level-part-value llvm-part-value)))

(define (new-passing-test test-file-path)
  (lambda ()
    (define file-contents (file->string test-file-path))

    (define top-level-part-input (open-input-string file-contents))
    (define top-level-part-scm (read-begin top-level-part-input))
    (define top-level-part-value (eval-top-level top-level-part-scm))

    ; This part is very brittle but it works
    ; Convert the process output to a racket value
    ; if that value is a (quote anything) racket will `read` it as (quote (quote anything))
    ; So we need to guard against that (the (list 'quote anything) match to remove the quote).
    ; Then we turn it back into a string, because the passing-div0 test
    ; wasnt working even though they printed to look the same
    ; so I figure make it a string, then compare! Hope this holds up...
    (define llvm-part-input (open-input-string file-contents))
    (define gen-ll-name (gen-header-name))
    (define gen-exe-name (string-append "build/" (symbol->string (gensym 'genexe)) ".exe"))
    (scm->exe llvm-part-input gen-exe-name clang++-path compiler-flags
              libgc-obj-path libgc-include-dir gen-ll-name)
    (define llvm-part-string
      (string-normalize-spaces
       (with-output-to-string (lambda () (system (format "./~a" gen-exe-name))))))
    (define llvm-part-output (read (open-input-string llvm-part-string)))
    (define llvm-val
      (match llvm-part-output
        [(list 'quote val) val]
        [else llvm-part-output]))
    (when (not (equal? (~a llvm-val) (~a top-level-part-value)))
      (displayln (format "llvm:~a\ntop-level:~a" (~a llvm-val) (~a top-level-part-value))))
    (equal? (~a llvm-val) (~a top-level-part-value))))

; TODO: move test-desugar from utils to here?
(define (new-desugar-test test-file-path)
  (lambda ()
    (define test-contents (read (open-input-string (file->string test-file-path))))
    (with-handlers ([exn:fail? #f])
      (test-desugar desugar test-contents))))

(define (new-alphatize-test test-file-path)
  (lambda ()
    (define test-contents (read (open-input-string (file->string test-file-path))))
    (with-handlers ([exn:fail? #f])
      (test-alphatize assignment-convert alphatize test-contents))))

(define (new-anf-test test-file-path)
  (lambda ()
    (define test-contents (read (open-input-string (file->string test-file-path))))
    (with-handlers ([exn:fail? #f])
      (test-anf-convert anf-convert test-contents))))

(define (new-cps-test test-file-path)
  (lambda ()
    (define test-contents (read (open-input-string (file->string test-file-path))))
    (with-handlers ([exn:fail? #f])
      (test-cps-convert cps-convert test-contents))))

(define (new-clo-test test-file-path)
  (lambda ()
    (define test-contents (read (open-input-string (file->string test-file-path))))
    (with-handlers ([exn:fail? #f])
      (test-closure-convert closure-convert test-contents))))

(define (new-llvm-test test-file-path)
  (lambda ()
    (define test-contents (read (open-input-string (file->string test-file-path))))
    (with-handlers ([exn:fail? #f])
      (test-llvm-convert llvm-convert test-contents))))

(define (get-tests-at testsloc filetype)
  (map (λ (p) (string->path (string-append testsloc (path->string p))))
       (filter
        (λ (p) (equal? (last (string-split (path->string p) "."))
                       filetype))
        (directory-list testsloc))))

; takes a path like /a/b/c/d.boop and returns 'd'
(define (extract-filename p)
  (last (string-split
         (string-join (drop-right (string-split (path->string p) ".") 1) ".")
         "/")))

(define (make-suite prefix make-test test-list)
  (map (λ (p) (list (string-append prefix (extract-filename p))
                    (make-test p)))
       test-list))

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
(define llvm-tests (make-suite "llvm-" new-clo-test
                               (get-tests-at "tests/passes/clo/" "proc")))

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
