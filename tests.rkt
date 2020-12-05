#lang racket

;; Testing apparatus modified from assignments
;; Its pretty bad, i hacked it to work for this... so yeah

(require (only-in "src/racket/utils.rkt"
                  test-desugar read-begin eval-top-level))

(require (only-in "src/racket/desugar.rkt" desugar))

(require (only-in "compiler.rkt"
                  gen-header-name
                  libgc-include-dir libgc-obj-path
                  clang++-path compiler-flags scm->exe))

(define (new-failing-test test-file-path)
  (lambda ()
    (define exception-name (gensym 'ex))
    (define didnt-fail-tag (gensym 'did-not-fail))
    (define file-contents (file->string test-file-path))
    (define guarded-code (format "(guard (~a ['~a #t] [else #f]) ~a (raise '~a))"
                                 exception-name didnt-fail-tag file-contents didnt-fail-tag))
    (define top-level-part-port (open-input-string guarded-code))
    (define top-level-part-scm (read-begin top-level-part-port))
    (define top-level-part-value (eval-top-level top-level-part-scm))

    (define llvm-part-port (open-input-string guarded-code))
    (define gen-ll-name (gen-header-name))
    (define gen-exe-name (string-append "build/" (symbol->string (gensym 'genexe)) ".exe"))
    (scm->exe llvm-part-port gen-exe-name clang++-path compiler-flags
              libgc-obj-path libgc-include-dir gen-ll-name)
    (define llvm-part-value
      (read (open-input-string
             (with-output-to-string (lambda () (system (format "./~a" gen-exe-name)))))))
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
    (with-handlers ([exn:fail? (begin #f)])
      (test-desugar desugar test-contents))))

(define (get-tests-at testsloc)
  (map (λ (p) (string->path (string-append testsloc (path->string p))))
       (filter
        (λ (p) (member (last (string-split (path->string p) "."))
                       '("scm" "sinscm")))
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
                                  (get-tests-at "tests/passing/")))
(define failing-tests (make-suite "failing-" new-failing-test
                                  (get-tests-at "tests/failing/")))
(define desugar-tests (make-suite "desugar-" new-desugar-test
                                  (get-tests-at "tests/passes/desugar/")))

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

(define all-tests (append passing-tests failing-tests desugar-tests))
(define (run-test/internal is-repl . args)
  ;; Run all tests, a specific test or suite, or print the available tests
  (match args
    [(list "all") (run-suite all-tests)]
    [(list "passing") (run-suite passing-tests)]
    [(list "failing") (run-suite failing-tests)]
    [(list "desugar") (run-suite desugar-tests)]
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
