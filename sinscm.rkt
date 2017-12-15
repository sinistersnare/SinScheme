#lang racket

(require "src/racket/compiler.rkt") ; compile-code, compile-program

(provide scm->llvmir
         scm->exe
         llvmir->exe
         libgc-include-dir
         libgc-obj-path
         clang++-path
         compiler-flags)

(require racket/cmdline)

; CLI ONLY
; wont work in REPL, sorry :(

; TODO: delete all the generated headers at some point? Maybe after everything works and no error generated?

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
                        "What kind of thing to output. -t LLVM for llvm ir text file, -t EXE for executable."
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


(define clang++-path
  (let ([clang++-path-submit-server "/opt/llvm-3.9.0/bin/clang++"])
    (if (file-exists? clang++-path-submit-server)
        clang++-path-submit-server
        "clang++")))

(define output-port
  (if (null? (outfileparam))
      '()
      (open-output-file (outfileparam) #:exists 'replace)))

(define input-port
  (cond
    [(not (null? (infileparam))) (open-input-file (infileparam))]
    [(not (null? (sourcecodeparam))) (open-input-string (sourcecodeparam))]
    [else '()]))


(define output-type
  (if (null? (outputtypeparam))
      (error "Need an output type (-t)! Check --help for details!")
      (case (outputtypeparam)
        [("LLVM" "llvm") 'llvm]
        [("EXE" "exe") 'exe]
        [else '()])))

(define input-type
  (if (null? (inputtypeparam))
      (error "Need an input type (-j)! Check --help for details!")
      (case (inputtypeparam)
        [("LLVM" "llvm") 'llvm]
        [("SCM" "scm" "SCHEME" "scheme" "SINSCM" "sinscm") 'scm]
        [else '()])))



(define (gen-header-name)
  (when (not (directory-exists? "build")) (make-directory "build"))
  (string-append "build/" (symbol->string (gensym 'generated_header)) ".ll"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET THESE TO YOUR GC LOCATIONS PLZ


; Default fallback locations from utils.rkt
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (scm->llvmir [in-port input-port] [out-port output-port] [clang-path clang++-path]
                     [all-compiler-flags compiler-flags] [libgc-include-dir-path libgc-include-dir]
                     [output-llvm-ir-header-name (gen-header-name)])
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

(define (llvmir->exe combined-ir-filepath [clang-path clang++-path]
                     [all-compiler-flags compiler-flags] [libgc-lib-path libgc-obj-path]
                     [outfilename (outfileparam)])
  (system (format "~a ~a ~a ~a -o ~a" clang-path all-compiler-flags libgc-lib-path combined-ir-filepath outfilename)))

(define (scm->exe inputport outfilename
                  [clang-path clang++-path] [all-compiler-flags compiler-flags]
                  [libgc-object-file-path libgc-obj-path] [libgc-include-dir-path libgc-include-dir]
                  [header-llvm-built-name (gen-header-name)])
  (define combined-file-location (string-append "build/" (symbol->string (gensym 'generated_combined)) ".ll"))
  (define combined-output-file (open-output-file combined-file-location #:exists 'replace))
  (scm->llvmir inputport combined-output-file clang-path all-compiler-flags libgc-include-dir-path header-llvm-built-name)

  (close-output-port combined-output-file) ; dont use combined-output-file anymore!
  (llvmir->exe combined-file-location clang-path all-compiler-flags libgc-object-file-path outfilename)
  ; remove this void for a status check to be printed out
  ; (it seems if system is the last call in a block it will print whether it worked or not?)
  (void))


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
               [(exe) (llvmir->exe (infileparam))]
               [(llvm) (error "Output type LLVM not supported for input type LLVM. You already have the file!")]
               [else (error "I figured this was unreachable??")])]
            [(scm)
             (case output-type
               [(llvm) (scm->llvmir input-port)]
               [(exe) (scm->exe input-port (outfileparam) clang++-path compiler-flags libgc-obj-path libgc-include-dir (gen-header-name))]
               [else (error "I figured this was unreachable??")])])
          (close-input-port input-port)
          (close-output-port output-port))))

; if all params are null then we are in library mode,
; and we provide scm->llvm scm->exe and llvm->exe in library mode.







