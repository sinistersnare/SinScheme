#lang racket

(require "src/racket/compiler.rkt") ; compile-code, compile-program

(require racket/cmdline)

(define outfileparam (make-parameter null))
(define infileparam (make-parameter null))
(define sourcecodeparam (make-parameter null))

(define file-to-compile
  (command-line
   #:program "compiler"
   #:once-each
   [("-o" "--outfile") outfile
                       "LLVM output to specific file"
                       (outfileparam outfile)]
   #:once-any
   ["-e" source-code
         "Compile given code directly"
         (sourcecodeparam source-code)]
   [("-i" "--infile") infile
                      "Sin Scheme file that will be compiled to LLVM"
                      (infileparam infile)]))

(define output-port
  (if (null? (outfileparam))
      (error "Need Output File! Check --help for -o")
      (open-output-file (outfileparam) #:exists 'replace)))

(match-define `(,compilation-method ,input-port)
  (cond
    [(not (null? infileparam)) `(,compile-program ,(open-input-file (infileparam)))]
    [(not (null? sourcecodeparam)) `(,compile-code ,(open-input-string (sourcecodeparam)))]
    [else (error "Need Input File or Source Code! Check --help for -i and -e")]))

(define clang++-path
  (let ([clang++-path-submit-server "/opt/llvm-3.9.0/bin/clang++"])
    (if (file-exists? clang++-path-submit-server)
        clang++-path-submit-server
        "clang++")))
(define gen-header-name (string-append (symbol->string (gensym 'generated_header)) ".ll"))

(system (string-append clang++-path " ./src/cpp/header.cpp " " -S -emit-llvm -o " gen-header-name))

(define header-code (file->string gen-header-name))

(define compiled-code
  (let ([compiled-str (open-output-string)])
    (compile-program input-port compiled-str)
    (get-output-string compiled-str)))

(close-input-port input-port)

(define complete-program (string-append header-code "\n" compiled-code))

(display complete-program output-port)

(close-output-port output-port)





