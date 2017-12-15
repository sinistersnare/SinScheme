#lang racket

(require "src/racket/compiler.rkt") ; compile-code, compile-program

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
      (error "Need Output File (-o)! Check --help for details")
      (open-output-file (outfileparam) #:exists 'replace)))

(match-define `(,compilation-method ,input-port)
  (cond
    [(not (null? (infileparam))) `(,compile-program ,(open-input-file (infileparam)))]
    [(not (null? (sourcecodeparam))) `(,compile-code ,(open-input-string (sourcecodeparam)))]
    [else (error "Need Input File or Source Code! Check --help for -i and -e")]))

(define output-type
  (if (null? (outputtypeparam))
      (error "Need an output type (-t)! Check --help for details!")
      (case (outputtypeparam)
        [("LLVM" "llvm") 'llvm]
        [("EXE" "exe") 'exe]
        [else (error (format "Output type ~s not supported!" (outputtypeparam)))])))

(define input-type
  (if (null? (inputtypeparam))
      (error "Need an input type (-j)! Check --help for details!")
      (case (inputtypeparam)
        [("LLVM" "llvm") 'llvm]
        [("SCM" "scm" "SCHEME" "scheme" "SINSCM" "sinscm") 'scm]
        [else (error (format "Input type ~s not supported!" (inputtypeparam)))])))

(when (not (directory-exists? "build")) (make-directory "build"))

(define gen-header-name (string-append "build/" (symbol->string (gensym 'generated_header)) ".ll"))

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
  (string-join '("-g"
                   "-std=c++11"
                   "-lpthread"
                   "-Wall"
                   "-Weverything"
                   "-DGC_DEBUG"
                   )))

(define (output-to-llvm [out-port output-port])
  (system (format "~s ~a ./src/cpp/header.cpp -I~s -S -emit-llvm -o ~s"
                  clang++-path compiler-flags libgc-include-dir gen-header-name))
  #;(system (format "~s -g -std=c++11 -Wall -Weverything ./src/cpp/header.cpp -I~s -DGC_DEBUG -S -emit-llvm -o ~s"
                  clang++-path libgc-include-dir gen-header-name))
  (define header-code (file->string gen-header-name))
  (define compiled-code
    (let ([compiled-str (open-output-string)])
      (compile-program input-port compiled-str)
      (get-output-string compiled-str)))
  (define complete-program (string-append header-code "\n\n;;;;;;;End Prelude;;;;;;;\n\n" compiled-code))
  (display complete-program out-port))

(define (output-to-exe)
  (define combined-file-location (string-append "build/" (symbol->string (gensym 'generated_combined)) ".ll"))
  (define combined-output-file (open-output-file combined-file-location #:exists 'replace))
  (output-to-llvm combined-output-file)
  (close-output-port combined-output-file) ; cant use combined-output-file anymore!
  (ir-to-exe combined-file-location)
  #;(system (format "~s ~a -o ~s ~s ~s"
                  clang++-path compiler-flags
                  (outfileparam) libgc-obj-path combined-file-location))
  ; remove this void for a status check to be printed out
  ; (it seems if system is the last call in a block it will print whether it worked or not?)
  (void))


(define (ir-to-exe combined-ir-filepath)
  (system (format "~s ~a -o ~s ~s ~s"
                  clang++-path  compiler-flags
                  (outfileparam) libgc-obj-path combined-ir-filepath)))

(case input-type
  [(llvm)
   (case output-type
     [(exe) (ir-to-exe (infileparam))]
     [(llvm) (error "Output type LLVM not supported for input type LLVM. You already have the file!")]
     [else (error "I figured this was unreachable??")])]
  [(scm)
   (case output-type
     [(llvm) (output-to-llvm)]
     [(exe) (output-to-exe)]
     [else (error "I figured this was unreachable??")])])



(close-input-port input-port)
(close-output-port output-port)



