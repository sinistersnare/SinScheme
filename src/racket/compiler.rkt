#lang racket

; Written By Davis Silverman

(provide
 compile-program
 compile-code)

;;;;;;;;;;;;;;;;;;;             ; Provided Functions:
(require "top-level.rkt")       ; top-level
(require "desugar.rkt")         ; desugar
(require "cps.rkt")             ; assignment-convert, alphatize, anf-convert, cps-convert
(require "closure-convert.rkt") ; closure-convert, proc->llvm
(require "utils.rkt")           ; read-begin, simplify-ir

; compile-program: Port X Port -> void
; This function will read from the port a SinScheme program and output the emitted LLVM code to the outport.
; compile-program will NOT close either port, so you must close it after the function returns.
; The data in the port must be syntactically valid scheme code, or it will fail to read
; The returned string is LLVM IR code.
(define (compile-program inport [outport (current-output-port)])
  (define beginified-code (read-begin inport))
  (define llvm-code (compile-code beginified-code))
  (displayln llvm-code outport))

; compile-code SinScheme -> String
; Takes a valid SinScheme program (a symbol, or an S-Expression) and compiles it to LLVM IR code.
(define (compile-code scm)
  ; awh no threading macro in Racket stdlib :(
  #; (~> code top-level desugar simplify-ir assignment-convert alphatize anf-convert cps-convert closure-convert proc->llvm)
  (proc->llvm (closure-convert
               (cps-convert (anf-convert (alphatize (assignment-convert
                                                                     (simplify-ir (desugar (top-level scm))))))))))











