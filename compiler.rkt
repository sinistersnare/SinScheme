#lang racket


(provide
 libgc-include-dir
 libgc-obj-path
 clang++-path
 compiler-flags
 scm->exe
 llvmir->exe
 scm->llvmir
 gen-header-name)

(require (only-in "src/racket/top-level.rkt" top-level))
(require (only-in "src/racket/desugar.rkt" desugar))
(require (only-in "src/racket/cps.rkt" cps-convert))
(require (only-in "src/racket/anf.rkt" anf-convert))
(require (only-in "src/racket/assignment-convert.rkt" assignment-convert))
(require (only-in "src/racket/alphatize.rkt" alphatize))
(require (only-in "src/racket/closure-convert.rkt" closure-convert))
(require (only-in "src/racket/llvm-convert.rkt" llvm-convert))
(require (only-in "src/racket/utils.rkt" read-begin simplify-ir))

(require threading)

; this file provides the common API for tests.rkt and sinscm.rkt
; It should not be used directly unless you are writing another front-end for sinscm.
; I tried putting this in src/racket
; (and changing the requires and provides in tests.rkt sinscm.rkt and this file)
; but when I try to run those programs, they hang for some reason.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SET THESE TO YOUR LIBGC LOCATIONS PLZ
; windows not supported! It can be, I just dont know where the paths would be.

(define macos-base (build-path "/" "usr" "local" "Cellar" "bdw-gc" "8.0.4"))
(define unix-base (build-path "/" "usr" "local"))

(define libgc-include-dir
  (match (system-type 'os)
    ['macosx (path->string (build-path macos-base "8.0.4" "include"))]
    ['unix (path->string (build-path unix-base "include"))]
    ['windows (raise 'windows-not-supported)]
    [else (raise 'unknown-os-type)]))

(define libgc-obj-path
  (match (system-type 'os)
    ['macosx (path->string (build-path macos-base "8.0.4" "lib" "libgc.a"))]
    ['unix (path->string (build-path unix-base "lib" "libgc.a"))]
    ['windows (raise 'windows-not-supported)]
    [else (raise `('unsupported-os-type ,else))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; fail if we cant find libgc stuff
(unless (directory-exists? libgc-include-dir) (raise `('cant-find-include ,libgc-include-dir)))
(unless (file-exists? libgc-obj-path) (raise `('cant-find-libgc-obj ,libgc-obj-path)))

;; Actual LLVM Emitter code
(define (compile-program inport [outport (current-output-port)])
  (define llvm-code (compile-code (read-begin inport)))
  (displayln llvm-code outport))

; compile-code SinScheme -> String
; Takes a valid SinScheme program (a symbol, or an S-Expression) and compiles it to LLVM IR code.
(define (compile-code scm)
  (llvm-convert (closure-convert
                 (cps-convert (anf-convert (alphatize (assignment-convert
                                                       (simplify-ir (desugar (top-level scm))))))))))

;; End actual LLVM emitter code.

(define clang++-path "clang++")

(define (gen-header-name)
  (when (not (directory-exists? "build")) (make-directory "build"))
  (string-append "build/" (symbol->string (gensym 'generated_header)) ".ll"))

(define compiler-flags
  (string-join '("-Wno-unused-command-line-argument"
                 "-std=c++11"
                 ; "-lpthread" ; why am i compiling threads?
                 ; "-O2" ;;; TODO figure out why optimization fails
                 "-Wall"
                 "-Weverything"
                 "-Wno-c++98-compat" ; so i can use nullptr unmolested.
                 "-Wno-extra-semi-stmt"
                 ; "-g"
                 ; "-DGC_DEBUG"
                 )))

; should probably parameterize this too.
(define header-location "./src/cpp/header.cpp")

(define (scm->llvmir in-port out-port clang-path all-compiler-flags
                     libgc-include-dir-path compiled-prelude-name)
  ; Compile the header file into llvm-ir
  (system (format "~a ~a ~a -I~s -S -emit-llvm -o ~a"
                  clang-path all-compiler-flags header-location
                  libgc-include-dir-path compiled-prelude-name))
  ; Compile Scheme code to llvm-ir
  (define compiled-code
    (let ([compiled-str (open-output-string)])
      (compile-program in-port compiled-str)
      (get-output-string compiled-str)))
  ; create a complete llvm program with prelude.
  (define complete-program (string-append (file->string compiled-prelude-name)
                                          "\n\n;;;;;;;End Prelude;;;;;;;\n\n" compiled-code))
  (display complete-program out-port))

(define (llvmir->exe combined-ir-filepath clang-path all-compiler-flags libgc-lib-path outfilename)
  (system (format "~a ~a ~a ~a -o ~a" clang-path all-compiler-flags
                  combined-ir-filepath libgc-lib-path outfilename)))

(define (scm->exe inputport outfilename clang-path all-compiler-flags
                  libgc-object-file-path libgc-include-dir-path header-llvm-built-name)
  (define combined-file-location (string-append "build/"
                                                (symbol->string (gensym 'generated_combined)) ".ll"))
  (define combined-output-file (open-output-file combined-file-location #:exists 'replace))
  (scm->llvmir inputport combined-output-file clang-path all-compiler-flags
               libgc-include-dir-path header-llvm-built-name)

  (close-output-port combined-output-file) ; dont use combined-output-file anymore!
  (llvmir->exe combined-file-location clang-path
               all-compiler-flags libgc-object-file-path outfilename)
  (void))

; if all params are null then we are in library mode,
; and we provide scm->llvm scm->exe and llvm->exe in library mode.

