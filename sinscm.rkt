#lang racket


(require (only-in "compiler.rkt"
                  libgc-include-dir libgc-obj-path
                  clang++-path compiler-flags
                  scm->exe llvmir->exe scm->llvmir gen-header-name))

; TODO: delete all the generated headers at some point? Maybe after
; everything works and no error generated?


(define outfileparam (make-parameter null))
(define infileparam (make-parameter null))
(define sourcecodeparam (make-parameter null))
(define outputtypeparam (make-parameter null))
(define inputtypeparam (make-parameter null))

(define file-to-compile
  (command-line
   #:program "compiler"
   #:once-each
   [("-o" "--outfile") outfile
                       "LLVM output to specific file"
                       (outfileparam outfile)]
   [("-t" "--out-type") outputtype
                        "What kind of thing to output. -t LLVM" " for llvm ir text file, -t EXE for executable."
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
                      (infileparam infile)]))

(define input-port
  (cond
    [(not (null? (infileparam))) (open-input-file (infileparam))]
    [(not (null? (sourcecodeparam))) (open-input-string (sourcecodeparam))]
    [else (error "Please provide an input file name (-i)")]))

(define output-port
  (if (null? (outfileparam))
      (error "Please provide an output file name (-o)")
      (open-output-file (outfileparam) #:exists 'replace)))

(define input-type
  (if (null? (inputtypeparam))
      '()
      (case (inputtypeparam)
        [("LLVM" "llvm") 'llvm]
        [("SCM" "scm" "SCHEME" "scheme" "SINSCM" "sinscm") 'scm]
        [else (error "Pleaes provide an input type (-j)")])))

(define output-type
  (if (null? (outputtypeparam))
      '()
      (case (outputtypeparam)
        [("LLVM" "llvm") 'llvm]
        [("EXE" "exe") 'exe]
        [else (error "Please provide an output type (-t)")])))





;; FIXME: if only one port is created (either input-port or output-port) then the one opened will not be closed
;; so instead of ormap? maybe we need to check and close if one is open.
(define params `(,output-port ,input-port ,output-type ,input-type))
(when (not (andmap null? params))
  (if (ormap null? params)
      ; If not all params are null, but at least 1 is..
      (error "Must provide all possible arguments! Please see --help")
      ; If all params are provided, then we are in CLI mode:
      (begin
        (case input-type
          [(llvm)
           (case output-type
             [(exe)
              (llvmir->exe (infileparam) clang++-path compiler-flags libgc-obj-path (outfileparam))]
             [(llvm)
              (error "Output type LLVM not supported for input type LLVM. You already have the file!")]
             [else (error "I figured this was unreachable??")])]
          [(scm)
           (case output-type
             [(llvm)
              (scm->llvmir input-port output-port clang++-path compiler-flags libgc-include-dir (gen-header-name))]
             [(exe)
              (scm->exe input-port (outfileparam) clang++-path compiler-flags libgc-obj-path libgc-include-dir (gen-header-name))]
             [else (error "I figured this was unreachable??")])])
        (close-input-port input-port)
        (close-output-port output-port))))


