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

(require "src/racket/top-level.rkt")       ; top-level
(require "src/racket/desugar.rkt")         ; desugar
(require "src/racket/cps.rkt")             ; assignment-convert, alphatize, anf-convert, cps-convert
(require "src/racket/closure-convert.rkt") ; closure-convert, proc->llvm
(require "src/racket/utils.rkt")           ; read-begin, simplify-ir


; this file provides the common API for tests.rkt and sinscm.rkt
; It should not be used directly unless you are writing another front-end for sinscm.
; I tried putting this in src/racket (and changing the requires and provides in tests.rkt sinscm.rkt and this file)
;   but when I try to run those programs, they hang for some reason.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET THESE TO YOUR LIBGC LOCATIONS PLZ


; Default fallback locations from utils.rkt
; (this is what you want to change probably)
(define project-path (current-directory))
(define libgc-path
  (path->string
   (build-path project-path "lib" "local" "lib" "libgc.a")))
(define gc-include-path
  (path->string
   (build-path project-path "lib" "local" "include")))

; locations where brew installs libgc (brew install libgc)
(define brew-include-dir "/usr/local/Cellar/bdw-gc/7.6.0/include/")
(define brew-libgc-path "/usr/local/Cellar/bdw-gc/7.6.0/lib/libgc.a")

(define libgc-include-dir
  (if (directory-exists? brew-include-dir)
      brew-include-dir
      gc-include-path))
(define libgc-obj-path
  (if (file-exists? brew-libgc-path)
      brew-libgc-path
      libgc-path))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Actual LLVM Emitter code
(define (compile-program inport [outport (current-output-port)])
  (define llvm-code (compile-code (read-begin inport)))
  (displayln llvm-code outport))

; compile-code SinScheme -> String
; Takes a valid SinScheme program (a symbol, or an S-Expression) and compiles it to LLVM IR code.
(define (compile-code scm)
  ; awh no threading macro in Racket stdlib :(
  #; (~> code top-level desugar simplify-ir assignment-convert alphatize anf-convert cps-convert closure-convert proc->llvm)
  (define ccd (closure-convert
               (cps-convert (anf-convert (alphatize (assignment-convert
                                                                     (simplify-ir (desugar (top-level scm)))))))))
  ; (displayln ccd)
  (proc->llvm ccd))

;; End actual LLVM emitter code.


(define clang++-path
  (let ([clang++-path-submit-server "/opt/llvm-3.9.0/bin/clang++"])
    (if (file-exists? clang++-path-submit-server)
        clang++-path-submit-server
        "clang++")))


(define (gen-header-name)
  (when (not (directory-exists? "build")) (make-directory "build"))
  (string-append "build/" (symbol->string (gensym 'generated_header)) ".ll"))

(define compiler-flags
  (string-join '("-Wno-unused-command-line-argument"
                 "-std=c++11"
                 "-lpthread"
                 ; "-O2" ;;; TODO figure out why optimization fails
                 "-Wall"
                 "-Weverything"
                 ;"-g"
                 ;"-DGC_DEBUG"
                 )))

; should probably parameterize this too.
(define header-location "./src/cpp/header.cpp")

(define (scm->llvmir in-port out-port clang-path all-compiler-flags
                     libgc-include-dir-path output-llvm-ir-header-name)
  ; Compile the header file into llvm-ir
  (system (format "~s ~a ~a -I~s -S -emit-llvm -o ~s"
                  clang-path all-compiler-flags header-location libgc-include-dir-path output-llvm-ir-header-name))
  ; Compile Scheme code to llvm-ir
  (define compiled-code
    (let ([compiled-str (open-output-string)])
      (compile-program in-port compiled-str)
      (get-output-string compiled-str)))
  ; create a complete llvm program with prelude.
  (define complete-program (string-append (file->string output-llvm-ir-header-name) "\n\n;;;;;;;End Prelude;;;;;;;\n\n" compiled-code))
  (display complete-program out-port))

(define (llvmir->exe combined-ir-filepath clang-path all-compiler-flags libgc-lib-path outfilename)
  (system (format "~a ~a ~a ~a -o ~a" clang-path all-compiler-flags libgc-lib-path combined-ir-filepath outfilename)))

(define (scm->exe inputport outfilename clang-path all-compiler-flags
                  libgc-object-file-path libgc-include-dir-path header-llvm-built-name)
  (define combined-file-location (string-append "build/" (symbol->string (gensym 'generated_combined)) ".ll"))
  (define combined-output-file (open-output-file combined-file-location #:exists 'replace))
  (scm->llvmir inputport combined-output-file clang-path all-compiler-flags libgc-include-dir-path header-llvm-built-name)

  (close-output-port combined-output-file) ; dont use combined-output-file anymore!
  (llvmir->exe combined-file-location clang-path all-compiler-flags libgc-object-file-path outfilename)
  ; remove this void for a status check to be printed out
  ; (it seems if system is the last call in a block it will print whether it worked or not?)
  (void))




; if all params are null then we are in library mode,
; and we provide scm->llvm scm->exe and llvm->exe in library mode.







