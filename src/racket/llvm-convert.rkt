#lang racket

(require (only-in "utils.rkt"
                  c-name prim-name applyprim-name datum?))

(provide llvm-convert)

; Input Language
;
; p ::= ((proc (x x ...) e) ...)
; e ::= (let ([x (apply-prim op x)]) e)
;     | (let ([x (prim op x ...)]) e)
;     | (let ([x (make-closure x x ...)]) e)
;     | (let ([x (env-ref x nat)]) e)
;     | (let ([x (quote dat)]) e)
;     | (clo-app x x ...)
;     | (if x e e)
;
; Output Language is LLVM IR.

; This file does LLVM-IR emission.
; Turning our closure-converted language into LLVM IR.
; This involves a lot of string formatting. Beware.
;
; the body-convert function is the meat you are probably looking for.
;

(define (llvm-convert procs)
  (define globals (get-globals procs))
  (string-join
   (append
    (list "; begin string and symbol globals")
    (layout-globals globals)
    (list  "; end string and symbol globals\n")
    (layout-main)
    (list "; begin user program\n")
    (layout-procs procs globals)
    (list "; end user program\n"))
   "\n"))

(define (layout-main)
  (define main-env (gensym 'mainenv))
  (define main-args (gensym 'mainargs))
  (append
   (list "define i32 @main() {")
   (ilist
    1
    "call void @start_program()"
    (format "; main args are unused, but kept to preserve similarity to other procs.")
    (format "%~a = call %struct.SinObj* @const_init_null()" main-env)
    (format "%~a = call %struct.SinObj* @const_init_null()" main-args)
    ; This cant use musttail because the prototype of @main() doesnt match the procs.
    (format "call tailcc void @proc_main(%struct.SinObj* %~a, %struct.SinObj* %~a)"
            main-env main-args)
    "ret i32 0")
   (list "}")))

(define (layout-globals globals)
  (hash-map globals
            (λ (k v) (format "@~a = private unnamed_addr constant [~a x i8] c\"~a\", align 8"
                             v (enc-len k) k))))

(define (layout-procs procs globals)
  (define (proc-convert proc)
    (match-define `(proc (,fname ,env ,arglist) ,procbody) (normalize-names proc))
    (define header
      ; dso_local says that this function is defined locally... obviously though? idk
      ; all procs are tailcc, which is the calling conv we use in SinScm
      (format "define dso_local tailcc void @proc_~a(%struct.SinObj* %~a, %struct.SinObj* %~a) {"
              fname env arglist))
    (append
     (list header)
     (body-convert procbody globals 1)
     (list "}\n")))
  (foldr append '() (map proc-convert procs)))

;; FIXME: Bug when symbols have same contents as a string.
;;  Fix is to have separate global lists for each global type.

; consumes an expression and returns a string-list of lines that are the LLVM IR equivalent.
(define (body-convert e globals indent-level)
  ; convert the form (let ([x datum)] body)
  ; in the case that the datum is a string or symbol
  ; we refer it to the global table to get the data.
  (define (dat-convert x dat globals)
    (define (layout-global key type constant-stack-addr raw-global)
      (define encoded (c-encode key))
      (define global-name (hash-ref globals encoded))
      (list
       (format "%~a = alloca i8*, align 8" constant-stack-addr)
       ; store the address of the global into constant-stack-addr
       (format (string-append "store i8* getelementptr inbounds ([~a x i8], "
                              "[~a x i8]* @~a, i32 0, i32 0), i8** %~a, align 8")
               (enc-len encoded) (enc-len encoded) global-name constant-stack-addr)
       (format "%~a = load i8*, i8** %~a, align 8" raw-global constant-stack-addr)
       (format "%~a = call %struct.SinObj* @const_init_~a(i8* %~a)" x type raw-global)))
    (match dat
      [#t (list (format "%~a = call %struct.SinObj* @const_init_true()" x))]
      [#f (list (format "%~a = call %struct.SinObj* @const_init_false()" x))]
      ['() (list (format "%~a = call %struct.SinObj* @const_init_null()" x))]
      [(? integer? n) (list (format "%~a = call %struct.SinObj* @const_init_int(i64 ~a)" x n))]
      [(? string? str) (layout-global dat "string"
                                      (gensym 'constaddr) (gensym 'rawstr))]
      [(? symbol? sym) (layout-global (symbol->string dat) "symbol"
                                      (gensym 'constaddr) (gensym 'rawsym))]))
  (match e
    ; apply-prim binding.
    [`(let ([,x (apply-prim ,op ,arg)]) ,letbody)
     (define stackaddr (gensym 'applyprim_stackaddr))
     (append
      (ilist
       indent-level
       ; Allocate stack address so GC can find the value.
       (format "%~a = alloca %struct.SinObj*, align 8" stackaddr)
       ; Call prim, places into register
       (format "%~a = call %struct.SinObj* @~a(%struct.SinObj* %~a)" x (applyprim-name op) arg)
       ; Store register value into stack
       (format "store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8\n" x stackaddr))
      (body-convert letbody globals indent-level))]
    ; Prim binding
    ; TODO: old version has a special case for single-arg `-` prim... why? Investigate!
    ;       something about how simplify-ir messes it up? Just fix simplify-ir?
    [`(let ([,x (prim ,op ,args ...)]) ,letbody)
     (define stackaddr (gensym 'prim_stackaddr))
     (append
      (ilist
       indent-level
       ; Allocate stack address so GC can find the value.
       (format "%~a = alloca %struct.SinObj*, align 8" stackaddr)
       ; Call prim, places into register
       (format "%~a = call %struct.SinObj* @~a(~a)"
               x (prim-name op)
               (string-join (map (λ (a) (format "%struct.SinObj* %~a" a)) args) ", "))
       ; Store register value into stack
       (format "store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8\n" x stackaddr))
      (body-convert letbody globals indent-level))]
    ; Closure creation binding
    [`(let ([,x (make-closure ,fname ,freevars ...)]) ,letbody)
     (define stackaddr (gensym 'makeclosure_stackaddr))
     (define fptr->int (gensym 'fptrasint))
     (define free-placements
       (map (λ (i fv)
              (format
               "call void @closure_place_freevar(%struct.SinObj* %~a, %struct.SinObj* %~a, i64 ~a)"
               x fv i))
            (range (length freevars)) freevars))
     (append
      (ilist
       indent-level
       ; Allocate stack address so GC can find the value.
       (format "%~a = alloca %struct.SinObj*, align 8" stackaddr)
       ; Convert function pointer to integer for storage
       ; All procs must be 2-args returning void, to appease the musttail requirements
       ; that the prototypes must be the same.
       (format "%~a = ptrtoint void(%struct.SinObj*, %struct.SinObj*)* @proc_~a to i64"
               fptr->int fname)
       (format "%~a = call %struct.SinObj* @closure_alloc(i64 ~a, i64 %~a)"
               x (length freevars) fptr->int)
       (format "store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8" x stackaddr))
      ; place free variables
      (indent free-placements indent-level)
      ; keeping this here so we get a newline after the placing of free-vars.
      (list "")
      (body-convert letbody globals indent-level))]
    ; Env-Ref binding
    [`(let ([,x (env-ref ,envname ,n)]) ,letbody)
     (define stackaddr (gensym 'envref_stackaddr))
     (append
      (ilist
       indent-level
       ; Allocate stack address so GC can find the value.
       (format "%~a = alloca %struct.SinObj*, align 8" stackaddr)
       (format "%~a = call %struct.SinObj* @closure_env_get(%struct.SinObj* %~a, i64 ~a)" x envname n)
       (format "store %struct.SinObj* %~a, %struct.SinObj** %~a, align 8\n" x stackaddr))
      (body-convert letbody globals indent-level))]
    ; Datum binding
    [`(let ([,x ',dat]) ,letbody)
     (define stackaddr (gensym 'datum_stackaddr))
     (append
      (ilist indent-level (format "%~a = alloca %struct.SinObj*, align 8" stackaddr))
      (indent (dat-convert x dat globals) indent-level)
      (ilist indent-level
             (format "store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8\n"
                     x stackaddr))
      (body-convert letbody globals indent-level))]
    ; Closure Application
    [`(clo-app ,clo-obj ,args ...)
     (when (> (length args) 1) (error `(got-too-many-args-in-clo-app ,(length args))))
     (define clo-fn (gensym 'clofn))
     (ilist
      indent-level
      (format "%~a = call void ~a @closure_get_fn_part(%struct.SinObj* %~a)"
              clo-fn "(%struct.SinObj*, %struct.SinObj*)*" clo-obj)
      (format "musttail call tailcc void %~a(%struct.SinObj* %~a, ~a)"
              clo-fn clo-obj (string-join (map (λ (a) (format "%struct.SinObj* %~a" a)) args) ", "))
      "ret void")]
    ; If Expression
    [`(if ,xc ,et ,ef)
     (define truthy? (gensym 'conditional))
     (define comparator (gensym 'cmp))
     (define thenblock (gensym 'thenblock))
     (define elseblock (gensym 'elseblock))
     (append
      (ilist
       indent-level
       (format "%~a = call i64 @is_truthy_value(%struct.SinObj* %~a)" truthy? xc)
       (format "%~a = icmp eq i64 %~a, 1" comparator truthy?)
       (format "br i1 %~a, label %~a, label %~a" comparator thenblock elseblock)
       (format "~a:" thenblock))
      (body-convert et globals (add1 indent-level))
      (ilist indent-level (format "~a:" elseblock))
      (body-convert ef globals (add1 indent-level)))]))

(define INDENT-SIZE 2) ; set this to make indents larger
(define (indent lines level)
  (map (λ (l) (string-append (make-string (* level INDENT-SIZE) #\space) l)) lines))

; returns a hash map of mapping -> global value
; in the given list of procs.
(define (get-globals procs)
  ; retrieves symbols and strings from program
  (define (get-symbols proc)
    (define (get proc st)
      (match proc
        [`(let ([,x ',(? datum? dat)]) ,ebody)
         (match dat
           [#t (get ebody st)]
           [#f (get ebody st)]
           [(? integer?) (get ebody st)]
           ['() (get ebody st)]
           [(? symbol? sym) (get ebody (set-add st sym))]
           [(? string? str) (get ebody (set-add st str))])]
        [`(let ,bnd ,ebody) (get ebody st)]
        [`(proc (,fname ,args ...) ,ebody) (get ebody st)]
        [`(clo-app ,fname ,args ...) st]
        [`(if ,xcond ,et ,ef) (set-union (get et st) (get ef st))]))
    (get proc (set)))
  ; 1) get symbols/strings from program
  ; 2) add it to a hash with a generated key.
  (foldl (λ (g h) (hash-set h (c-encode g) (gensym (if (string? g) '.str.global '.sym.global))))
         (hash) (set->list (foldl set-union (set) (map get-symbols procs)))))

; rewrite symbols to conform to LLVM-IR allowed characters
(define (normalize-names proc)
  (define (unweirdify s) (string->symbol (c-name s)))
  (match proc
    [`(proc (,fname ,args ...) ,ebody)
     `(proc (,(unweirdify fname) ,@(map unweirdify args)) ,(normalize-names ebody))]
    [`(let ([,x (apply-prim ,op ,args)]) ,ebody)
     `(let ([,(unweirdify x) (apply-prim ,op ,(unweirdify args))])
        ,(normalize-names ebody))]
    [`(let ([,x (prim ,op ,args ...)]) ,ebody)
     `(let ([,(unweirdify x) (prim ,op ,@(map unweirdify args))])
        ,(normalize-names ebody))]
    [`(let ([,x (make-closure ,fname ,args ...)]) ,ebody)
     `(let ([,(unweirdify x) (make-closure ,(unweirdify fname) ,@(map unweirdify args))])
        ,(normalize-names ebody))]
    [`(let ([,x (env-ref ,env ,(? natural? nat))]) ,ebody)
     `(let ([,(unweirdify x) (env-ref ,(unweirdify env) ,nat)]) ,(normalize-names ebody))]
    [`(let ([,x ',dat]) ,ebody)
     `(let ([,(unweirdify x) ',dat]) ,(normalize-names ebody))]
    [`(clo-app ,fname ,args ...)
     `(clo-app ,(unweirdify fname) ,@(map unweirdify args))]
    [`(if ,xcond ,et ,ef)
     `(if ,(unweirdify xcond) ,(normalize-names et) ,(normalize-names ef))]))

; Hex encodes a string and adds a null terminator.
; Used for globals.
(define (c-encode s-unknown)
  (define s (if (string? s-unknown) s-unknown (symbol->string s-unknown)))
  (string-append (string-join (map (lambda (c)
                                     (format "\\~a"
                                             (~r (char->integer c)
                                                 #:base '(up 16) #:min-width 2 #:pad-string "0")))
                                   (string->list s)) "") "\\00"))

(define (enc-len s) (length (string-split s "\\")))


; a helper to indent a list of strings
(define (ilist indent-level . xs)
  (indent xs indent-level))

