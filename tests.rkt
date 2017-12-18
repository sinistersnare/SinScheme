#lang racket

;; Testing apparatus modified from assignments
;; Its pretty bad, i hacked it to work for this... so yeah

(require "src/racket/utils.rkt")

; libgc-include-dir
; libgc-obj-path
; clang++-path
; compiler-flags
; scheme->exe
(require "compiler.rkt")



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
    (when (not (directory-exists? "build")) (make-directory "build"))
    (define gen-ll-name (string-append "build/" (symbol->string (gensym 'genheader)) ".ll"))
    (define gen-exe-name (string-append "build/" (symbol->string (gensym 'genexe)) ".exe"))
    (scm->exe llvm-part-port gen-exe-name clang++-path compiler-flags
              libgc-obj-path libgc-include-dir
              gen-ll-name)
    (define llvm-part-value (read (open-input-string (with-output-to-string (lambda () (system (format "./~a" gen-exe-name)))))))
    (and top-level-part-value llvm-part-value)))

(define (new-test test-file-path)
  (lambda ()
    (define file-contents (file->string test-file-path))

    (define top-level-part-input (open-input-string file-contents))
    (define top-level-part-scm (read-begin top-level-part-input))
    (define top-level-part-value (eval-top-level top-level-part-scm))

    (define llvm-part-input (open-input-string file-contents))
    (when (not (directory-exists? "build")) (make-directory "build"))
    (define gen-ll-name (string-append "build/" (symbol->string (gensym 'genheader)) ".ll"))
    (define gen-exe-name (string-append "build/" (symbol->string (gensym 'genexe)) ".exe"))
    (scm->exe llvm-part-input gen-exe-name clang++-path compiler-flags
              libgc-obj-path libgc-include-dir
              gen-ll-name)
    ; This part is very brittle but it works
    ; Convert the process output to a racket value
    ; if that value is a (quote anything) racket will `read` it as (quote (quote anything))
    ; So we need to guard against that (the (list 'quote anything) match to remove the quote).
    ; Then we turn it back into a string, because the passing-div0 test wasnt working even though they printed to look the same
    ; so I figure make it a string, then compare! Hope this holds up...
    (define llvm-part-string (string-normalize-spaces (with-output-to-string (lambda () (system (format "./~a" gen-exe-name))))))
    (define llvm-part-output (read (open-input-string llvm-part-string)))
    (define llvm-val
      (match llvm-part-output
        [(list 'quote val) val]
        [else llvm-part-output]))
    ; (when (not (equal? llvm-val top-level-part-value)) (displayln (format "llvm:~a\ntop-level:~a" llvm-val top-level-part-value)))
    (equal? (~a llvm-val) (~a top-level-part-value))))

(define passing-tests-list
  (map
   (lambda (path)
     (string->path
      (string-append "tests/passing/"
                     (path->string path))))
   (filter (lambda (path)
             (define p (path->string path))
             (member (last (string-split p ".")) '("scm" "sinscm")))
           (directory-list "tests/passing"))))

(define failing-tests-list
  (map
   (lambda (path)
     (string->path
      (string-append "tests/failing/" (path->string path))))
   (filter (lambda (path)
             (define p (path->string path))
             (member (last (string-split p ".")) '("scm" "sinscm")))
           (directory-list "tests/failing"))))


(define (path->passing-test p)
  (define filename (last (string-split (path->string p) "/")))
  `(,(string-append "passing-" (last (string-split (string-join (drop-right (string-split (path->string p) ".") 1) ".") "/")))
    ,(new-test p)))

(define (path->failing-test p)
  (define filename (last (string-split (path->string p) "/")))
  `(,(string-append "failing-" (last (string-split (string-join (drop-right (string-split (path->string p) ".") 1) ".") "/")))
    ,(new-failing-test p)))

(define passing-tests (map path->passing-test passing-tests-list))
(define failing-tests (map path->failing-test failing-tests-list))
(define all-tests (append passing-tests failing-tests))
(define (run-test/internal is-repl . args)
  ;; Run all tests, a specific test, or print the available tests
  (match args
    [(list "all")
     (define correct-count
       (foldl (lambda (testcase count)
                (match testcase
                  [(list test-name exec)
                   (define exec-result
                     (with-handlers ([exn:fail? identity])
                       (exec)))
                   (if (eq? exec-result #t)
                       (begin
                         (display "Test ")
                         (display test-name)
                         (display " passed.")
                         (newline)
                         (+ count 1))
                       (begin
                         (display "Test ")
                         (display test-name)
                         (display " failed!")
                         (newline)
                         count))]))
              0
              all-tests))
     (display "Score on tests: ")
     (display (/ (round (/ (* 10000 correct-count) (length all-tests))) 100.0))
     (display "%")
     (newline)]
    [(list test-name)
     #:when (assoc test-name passing-tests)
     (match (assoc test-name passing-tests)
       [(list name exec)
        (define exec-result
          (with-handlers ([exn:fail? identity])
            (exec)))
        (define passed (eq? exec-result #t))
        (displayln (if passed (format "Test \"~a\" passed!" name) "Test failed!"))
        (unless is-repl
          (exit (if (eq? exec-result #t) 0 1)))])]
    [(list test-name)
     #:when (assoc test-name failing-tests)
     (match (assoc test-name failing-tests)
       [(list name exec)
        (define exec-result
          (with-handlers ([exn:fail? identity])
            (exec)))
        (define passed (eq? exec-result #t))
        (displayln (if passed (format "Test \"~a\" passed!" name) "Test failed!"))
        (unless is-repl
          (exit (if (eq? exec-result #t) 0 1)))])]
    [else
     (display "Available tests: ")
     (newline)
     (display
      (string-join
       (map car all-tests)
       ", "))
     (newline)]))

(define run-test
  (curry run-test/internal #t))

(define r run-test)

(apply
 run-test/internal
 (cons
  #f
  (vector->list (current-command-line-arguments))))



