#lang racket


(require (only-in "compiler.rkt"
                  libgc-include-dir libgc-obj-path
                  clang++-path compiler-flags
                  scm->exe llvm->exe scm->llvm gen-header-name))

; TODO: delete all the generated headers at some point? Maybe after
; everything works and no error generated?


(define outfileparam (make-parameter null))
(define infileparam (make-parameter null))
(define sourcecodeparam (make-parameter null))
(define outputtypeparam (make-parameter null))
(define inputtypeparam (make-parameter null))

(command-line
 #:program "Sinscheme compiler"
 #:once-each
 [("-o" "--outfile") outfile
                     "LLVM output to specific file"
                     (outfileparam outfile)]
 [("-t" "--out-type") outputtype
                      ("What kind of thing to output. -t LLVM"
                       " for llvm ir text file, -t EXE for executable.")
                      (outputtypeparam outputtype)]
 [("-j" "--in-type") inputtype
                     "What kind of thing is input, LLVM or SCM (-j LLVM or -j SCM)"
                     (inputtypeparam inputtype)]
 #:once-any
 ["-e" source-code
       "Compile given code directly"
       (sourcecodeparam source-code)]
 [("-i" "--infile") infile
                    "Sin Scheme file that will be compiled to LLVM"
                    (infileparam infile)])

(define input-port
  (cond
    [(not (null? (infileparam))) (open-input-file (infileparam) #:mode 'text)]
    [(not (null? (sourcecodeparam))) (open-input-string (sourcecodeparam))]
    [else (error "Please provide an input file name (-i)")]))

(define output-port
  (if (null? (outfileparam))
      (error "Please provide an output file name (-o)")
      (open-output-file (outfileparam) #:mode 'text #:exists 'replace)))

(define input-type
  (if (null? (inputtypeparam))
      '()
      (case (inputtypeparam)
        [("LLVM" "llvm") 'llvm]
        [("SCM" "scm" "SCHEME" "scheme" "SINSCM" "sinscm") 'scm]
        [else (error "Please provide an input type (-j)")])))

(define output-type
  (if (null? (outputtypeparam))
      '()
      (case (outputtypeparam)
        [("LLVM" "llvm") 'llvm]
        [("EXE" "exe") 'exe]
        [else (error "Please provide an output type (-t)")])))

(match (cons input-type output-type)
  [(cons 'llvm 'exe)
   (llvm->exe)
   (define input-llvm-string (file->string (infileparam)))
   (llvm->exe input-llvm-string (outfileparam))]
  [(cons 'scm 'llvm)
   (define out-file (open-output-file (outfileparam)))
   (display (scm->llvm input-port) out-file)
   (close-output-port out-file)]
  [(cons 'scm 'exe)
   (scm->exe input-port (outfileparam))]
  [(cons i o) (error (format "Unsupported input/output combo I:'~a' and O:'~a'" i o))])

(if (null? input-port)
    (displayln "Why is input port null?")
    (close-input-port input-port))

(if (null? output-port)
    (displayln "Why is output port null?")
    (close-output-port output-port))
