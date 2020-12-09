#lang racket


(provide
 scm->exe llvm->exe scm->llvm
 gen-build-file)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SET THESE TO YOUR LIBGC LOCATIONS PLZ

; Windows not 'not' supported! It can be, I just dont know where the paths would be.
; LibGC works on windows alledgedly, so if you get this working, lemme know!

(define macos-base (build-path "/" "usr" "local" "Cellar" "bdw-gc" "8.0.4"))
(define unix-base (build-path "/" "usr" "local"))

(define libgc-include-dir
  (match (system-type 'os)
    ['macosx (path->string (build-path macos-base "8.0.4" "include"))]
    ['unix (path->string (build-path unix-base "include"))]
    ['windows (raise 'windows-not-supported-make-a-PR!)]
    [else (raise 'unknown-os-type)]))

(define libgc-obj-path
  (match (system-type 'os)
    ['macosx (path->string (build-path macos-base "8.0.4" "lib" "libgc.a"))]
    ['unix (path->string (build-path unix-base "lib" "libgc.a"))]
    ['windows (raise 'windows-not-supported-make-a-PR!)]
    [else (raise `('unsupported-os-type ,else))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DONE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define clang++-path "clang++") ; let's hope its in the PATH lol

(define (gen-build-file name extension)
  (unless (directory-exists? "build") (make-directory "build"))
  (string-append "build/" (symbol->string name) extension))

(define compiler-flags
  (string-join '("-std=c++11"
                 ; "-lpthread" ; why am i compiling threads?
                 ; "-O2" ;;; TODO figure out why optimization fails
                 "-g" ;;; TODO figure out why debug fails
                 "-DGC_DEBUG"
                 "-Wall"
                 "-Weverything"
                 "-Wno-unused-command-line-argument"
                 "-Wno-c++98-compat" ; so i can use nullptr unmolested.
                 "-Wno-c++98-compat-pedantic" ; so i can use variadic macros for errors.
                 "-Wno-extra-semi-stmt")))

; should probably parameterize this too.
(define header-location "./src/cpp/header.cpp")

; separates the runtime header and the user code
(define ir-separator "\n\n;;;;;;;End Runtime Header;;;;;;;\n\n")

; gets the runtime-half of the LLVM IR as a string.
(define (get-runtime-header)
  (define runtime-header-name (gen-build-file (gensym 'compiled_runtime) ".ll"))
  (system (format "~a ~a ~a -I~s -S -emit-llvm -o ~a"
                  clang++-path compiler-flags header-location
                  libgc-include-dir runtime-header-name))
  (file->string runtime-header-name))

; compile the given scheme to a full exe named `exe-name`.
; Takes an input-port that will be `read-begin`d into a scheme symbol,
; and a string, the name of the resulting executable.
; Returns void.
; The input port is NOT closed by this, close it yourself, chump!
(define (scm->exe scm-port exe-name)
  (define user-llvm (scm->llvm scm-port))
  (llvm->exe user-llvm exe-name))

; compiles the given scheme to the user-half of the LLVM output.
; Takes an input-port that will be `read-begin`d into a scheme symbol,
; returns a string, the user-half of the LLVM IR.
; The input port is NOT closed by this, close it yourself, chump!
(define (scm->llvm scm-port)
  (define scm (read-begin scm-port))
  (~> scm top-level desugar simplify-ir assignment-convert
      alphatize anf-convert cps-convert closure-convert llvm-convert))

; compile the given user-half of LLVM to a full exe named `exe-name`.
; Takes 2 strings:, the user-half LLVM, and the name of the resulting executable.
; Returns void.
(define (llvm->exe user-llvm exe-name)
  (define full-ir (string-append (get-runtime-header) ir-separator user-llvm))
  (define combined-ir-path (gen-build-file (gensym 'generated_combined) ".ll"))
  (define out-combined-file (open-output-file combined-ir-path #:mode 'text #:exists 'replace))
  (display full-ir out-combined-file)
  (system (format "~a ~a ~a ~a ~a ~a"
                  clang++-path compiler-flags combined-ir-path libgc-obj-path "-o" exe-name))
  (close-output-port out-combined-file))

(define code '(guard (x [else "GOOD"]) (/ 1 2 3 0)))
(define proc (~> code top-level desugar assignment-convert
                 alphatize anf-convert cps-convert closure-convert))
(define f (open-output-file "/home/sinistersnare/code/SinScheme/tests/passes/llvm/div-passes-disp.proc"
                            #:mode 'text #:exists 'replace))
(pretty-display proc f)