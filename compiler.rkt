#lang racket

(provide
 scm->exe llvm->exe scm->llvm
 gen-build-file)

(require (only-in "src/racket/top-level.rkt" top-level))
(require (only-in "src/racket/desugar.rkt" desugar))
(require (only-in "src/racket/assignment-convert.rkt" assignment-convert))
(require (only-in "src/racket/alphatize.rkt" alphatize))
; (require (only-in "src/racket/cps-anf.rkt" anf-convert))
; (require (only-in "src/racket/cps.rkt" cps-convert))
; (require (only-in "src/racket/cps-closure-convert.rkt" closure-convert))
; (require (only-in "src/racket/cps-llvm-convert.rkt" llvm-convert))
(require (only-in "src/racket/ssa-anf.rkt" anf-convert))
(require (only-in "src/racket/ssa-closure-convert.rkt" closure-convert))
(require (only-in "src/racket/ssa-llvm-segmented.rkt" llvm-convert))

(require (only-in "src/racket/utils.rkt" read-begin))

(require threading)

; this file provides the common API for tests.rkt and sinscm.rkt
; It should not be used directly unless you are writing another front-end for sinscm.
; I tried putting this in src/racket
; (and changing the requires and provides in tests.rkt sinscm.rkt and this file)
; but when I try to run those programs, they hang for some reason.

;; Actual LLVM Emitter code
(define (compile-program inport [outport (current-output-port)])
  (define llvm-code (compile-code (read-begin inport)))
  (displayln llvm-code outport))

; compile-code SinScheme -> String
; Takes a valid SinScheme program (a symbol, or an S-Expression) and compiles it to LLVM IR code.
(define (compile-code scm)
  (~> scm top-level desugar assignment-convert alphatize anf-convert
      closure-convert llvm-convert))

;; End actual LLVM emitter code.

;(define clang++-path (path->string (find-executable-path "clang++"))) ; let's hope its in the PATH lol
;;; TODO: THIS SHOULD BE SET BY AN ENV-VAR??? OR SOMETHING???? THIS MUST BE THE SINSCHEME LLVM!
(define llvm-bin-path (build-path "/" "usr" "lib" "llvm-14" "bin"))
#;(define llvm-bin-path (build-path (find-system-path 'home-dir) "code" "llvm" "build" "bin"))
(define clang++-path (path->string (build-path llvm-bin-path "clang++")))
(define opt-path (path->string (build-path llvm-bin-path "opt")))


(define (gen-build-file name extension)
  (unless (directory-exists? "build") (make-directory "build"))
  (string-append "build/" (symbol->string name) extension))

(define compiler-flags
  (string-join '("-std=c++11"
                 ; "-O2" ; TODO:  use llvm's `opt` instead of clang optimizations.
                 "-DGC_DEBUG"
                 "-Wall"
                 ;"-Xclang" "-no-opaque-pointers"
                 "-Weverything"
                 "-Wno-unused-command-line-argument"
                 "-Wno-c++98-compat" ; so i can use nullptr unmolested.
                 "-Wno-c++98-compat-pedantic" ; so i can use variadic macros for errors.
                 "-Wno-extra-semi-stmt")))

(define runtime-location "./src/cpp/runtime.cpp")

; separates the runtime and the user code
(define ir-separator "\n\n;;;;;;;End Runtime Code;;;;;;;\n\n")

; generates a runtime.ll file, and returns the file name/location of it.
(define (get-runtime-file)
  (define runtime-ll-name (gen-build-file (gensym 'compiled_runtime) ".ll"))
  (system (format "~a ~a ~a -S -emit-llvm -o ~a"
                  clang++-path compiler-flags runtime-location runtime-ll-name))
  runtime-ll-name)

; gets the runtime-half of the LLVM IR as a string.
(define (get-runtime-code)
  (file->string (get-runtime-file)))

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
  (compile-code scm))

; compile the given user-half of LLVM to a full exe named `exe-name`.
; Takes 2 strings:, the user-half LLVM, and the name of the resulting executable.
; Returns void.
(define (llvm->exe user-llvm exe-name)
  (define combined-ir-path (gen-build-file (gensym 'generated_combined) ".ll"))
  (define out-combined-file (open-output-file combined-ir-path #:mode 'text #:exists 'replace))
  (display (get-runtime-code) out-combined-file)
  (display ir-separator out-combined-file)
  (display user-llvm out-combined-file)
  (flush-output out-combined-file)
  (system (string-join (list clang++-path compiler-flags combined-ir-path "-o" exe-name) " "))
  (close-output-port out-combined-file))
