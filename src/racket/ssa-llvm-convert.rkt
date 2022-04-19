#lang racket

(provide ssa-llvm-convert)

(require (only-in "utils.rkt" symbol-append c-hex-encode
                  encoded-str-length c-name prim-name applyprim-name))


; Input Language (output of `lir-convert`):

; p ::= ((proc (x x x) e) ...)
; e ::= (i ... (return r))
; i ::= (assign x l)
;     | (if x (label x) (label x))
;     | (jump (label x))
;     | (return r)
;     | (label x)
; l ::= (make-closure x x ...)
;     | (env-ref x nat)
;     | (quote dat)
;     | (prim op x ...)
;     | (apply-prim op x)
;     | (phi (x x) (x x))
;     | r
; r ::= x
;     | (call/cc x)
;     | (clo-app x x)

; Output language is LLVM IR! I will NOT write that grammar out!!

(define (ssa-llvm-convert lisp-style-lir)
  (define lir (normalize-names pretty-lir))
  (match-define (cons symbol-globals string-globals) (compute-globals lir))
  (string-join
   `("; Begin user globals"
     ,@(layout-globals symbol-globals string-globals)
     "; End user globals"
     ,@(layout-main)
     "; Begin user program"
     ,@(layout-procs lir symbol-globals string-globals)
     "; End user program")
   "\n"))

(define (normalize-names lir)
  ; converts all variable names from a 'lisp style' (i.e. kebab case with any symbol)
  ; into a c-style (only character class \w allowed basically)
  (define (normalize x) (string-symbol (c-name x)))
  (define (norm-proc proc)
    (match-define `(proc (,name ,env ,args) ,e))
    `(proc (,(normalize name) ,(normalize env) ,(normalize args)) ,(map norm-i e)))
  (define (norm-i i)
    (match i
      [`(assign ,x ,l)
       `(assign ,(normalize x) ,(norm-l l))]
      [`(if ,xc (label ,xt) (label ,xf))
       `(if ,(normalize xc) (label ,(normalize xt)) (label ,(normalize ,xf)))]
      [`(jump (label x)) `(jump (label ,(normalize x)))]
      [`(label ,x) `(label ,(normalize x))]
      [`(return ,r) `(return ,(norm-r r))]))
  (define (norm-l l)
    (match l
      [`(make-closure ,x . ,xs) `(make-closure . ,(map normalize (cons x xs)))]
      [`(env-ref ,x ,nat) `(env-ref ,(normalize x) ,nat)]
      [`(quote ,_) l]
      [`(prim ,op . ,xs) `(prim ,op . ,(map normalize xs))]
      [`(apply-prim ,op ,x) `(apply-prim ,op ,(normalize x))]
      [`(phi (,x0 ,lx0) (,x1 ,lx1)) `(phi (,(normalize x0) ,(normalize lx0))
                                          (,(normalize x1) ,(normalize lx1)))]
      [r (norm-r r)]))
  (define (norm-r r)
    (match r
      [`(call/cc ,x) `(call/cc ,(normalize x))]
      [`(clo-app ,xf ,xx) `(clo-app ,(normalize ,xf) ,(normalize xx))]
      [(? symbol? x) (normalize x)]))
  (map norm-proc lir))

; returns a hash with mapping ((cons type global-value-as-string) |-> global-name)
; in the given list of procs.
(define (compute-globals lir)
  ;; (... (assign x 'boop) ...)
  ;; =>
  ;; globals -> (hash (cons 'sym "boop") .sym.global54463)
  (define (compute-globals-i i acc)
    (match i
      [`(assign ,_ (quote ,(? string? str)))
       (define key (cons 'str str))
       (hash-set acc key (cons (gensym '.str.global)
                               (encoded-str-length (c-hex-encode str))))]
      [`(assign ,_ (quote ,(? symbol? sym)))
       ; symbol->string so we dont accidentally try to eq? uninterned symbols.
       (define key (cons 'sym (symbol->string sym)))
       (hash-set acc key (cons (gensym '.sym.global)
                               (encoded-str-length (c-hex-encode (symbol->string sym)))))]
      [_ acc]))
  (foldl (λ (e done) (foldl compute-globals-i done e))
         (hash) (map caddr lir)))

(define (layout-globals globals)
  ;; globals -> (hash (cons 'sym "boop") .sym.global54463)
  ;; =>
  ;; @sym.global54463 = private unnamed_addr constant [5 x i8] c"\62\6F\6F\70\00", align 8
  (define fmt-string "@~a = private unnamed_addr constant [~a x i8] c\"~a\", align 8")
  (match-define (cons global-syms global-strs)
    (foldl (λ (k acc)
             (match-define (cons syms strs) acc)
             (match-define (cons type global-value) k)
             (define global-key (hash-ref globals k))
             (define enc-value (c-hex-encode global-value))
             (define global (format fmt-string
                                    global-key (encoded-str-length enc-value) enc-value))
             (if (eq? type 'sym)
                 (cons (cons global syms) strs)
                 (cons syms (cons global strs))))
           '(() . ()) (hash-keys globals)))
  (string-join `("; Global Symbols" ,@global-syms "; Global Strings" ,@global-strs) "\n"))

(define (layout-main)
  (define main-env (gensym 'mainenv))
  (define main-args (gensym 'mainargs))
  `("define i32 @main() {"
    "  call void @start_program()"
    "  ; main args are unused but kept to keep same protoypes with other procs."
    ,(format "  %~a = call %struct.SinObj* @const_init_null()" main-env)
    ,(format "  %~a = call %struct.SinObj* @const_init_null()" main-args)
    ,(format "  call void @proc_main(%struct.SinObj* %~a, %struct.SinObj* %~a)" main-env main-args)
    "  ret i32 0"
    "}"))

(define (layout-procs lir globals)
  (define (datum-convert x dat globals)
    (define (conv-global type val)
      ; convert string and symbol globals into LLVM IR by correct GEP usage.
      (define key (cons type val))
      (match-define (cons global-name size) (hash-ref globals key))
      (define stackaddr (gensym (symbol-append 'const type 'sa)))
      (define raw-global (gensym (symbol-append 'raw type)))
      `(,(format "  %~a = alloca i8*, align 8" stackaddr)
        ,(format (string-append "store i8* getelementptr inbounds ([~a x i8], "
                                "[~a x i8]* @~a, i32 0, i32 0), i8** %~a, align 8")
                 size size global-name stackaddr)
        ,(format "  %~a load i8*, i8** %~a, align 8" raw-global stackaddr)
        ,(format "  %~a call %struct.SinObj* @const_init_~a(i8* %~a)" x type raw-global)))
    (match dat
      [#t `(,(format "  %~a = call %struct.SinObj* @const_init_true()" x))]
      [#f `(,(format "  %~a = call %struct.SinObj* @const_init_false()" x))]
      ['() `(,(format " %~a = call %struct.SinObj* @const_init_null()" x))]
      [(? integer? n) `(,(format "  %~a = call %struct.SinObj* @const_init_int(i64 ~a)" x n))]
      [(? string? str) (conv-global 'str str)]
      [(? symbol? sym) (conv-global 'sym sym)]))
  (define (layout-proc p)
    (match-define `(proc (,name ,env ,args) ,e) p)
    ; TODO: this isnt tailcc with segmented-stack stuff, gonna have to do that work ourselves!
    `(,(format "define dso_local tailcc %struct.SinObj* @proc_~a(%struct.SinObj* %~a, %struct.SinObj* %~a) {"
               name env args)
      ,@(map layout-i e)
      "}\n"))
  (define (layout-i i)
    (match i
      [`(label ,x) `(,(format "%~a:" x))]
      [`(if ,xc (label ,xt) (label ,xf))
       (define truthy? (gensym 'cond))
       (define cmp (gensym 'cmp))
       `(,(format "  %~a = call i64 @is_truthy_value(%struct.SinObj* %~a)" truthy? xc)
         ,(format "  %~a = icmp eq i64 %~a, 1" cmp truthy?)
         ,(format "  br i1 %~a, label %~a, label %~a" cmp xt xf))]
      [`(jump (label ,x)) `(,(format "  br label %~a" x))]
      ; cant do pretty stuff with `l` like before!
      [`(assign ,x (quote ,dat))
       (define stackaddr (gensym 'datumsa))
       `(,(format "  %~a = alloca %struct.SinObj*, align 8" stackaddr)
         ,@(datum-convert x dat globals)
         ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8\n" x stackaddr))]
      [`(assign ,x (make-closure ,xf . ,xs))
       (define stackaddr (gensym 'makeclosa))
       (define fptr->int (gensym 'fptrasint))
       (define free-placements
         (map (λ (fv i)
                (format "  call void @closure_place_freevar(%struct.SinObj* %~a, %struct.SinObj* ~a, i64 a)"
                        x fv i))
              xs (range (length xs))))
       `(; allocate stack addr
         ,(format "  %~a = alloca %struct.SinObj*, align 8" stackaddr)
         ; convert the function pointer to an integer for storage
         ,(format "  %~a = ptrtoint %struct.SinObj*(%struct.SinObj*, %struct.SinObj*)* @proc_~a to i64"
                  fptr->int fname)
         ; allocate the closure
         ,(format "  %~a = call %struct.SinObj* @closure_alloc(i64 ~a, i64 %~a)" x (length freevars) fptr->int)
         ; store the closure on the stack
         ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8\n" x stackaddr)
         ; place the free variables
         ,@free-placements)]
      [`(assign ,x (env-ref ,xenv ,n))
       (define stackaddr (gensym 'envrefsa))
       `(,(format "  %~a = alloca %struct.SinObj*, align 8" stackaddr)
         ,(format "  %~a = call %struct.SinObj* @closure_env_get(%struct.SinObj* %~a, i64 ~a)"
                  x xenv n)
         ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8\n" x stackaddr))]
      [`(assign ,x (prim op . ,xs))
       (define stackaddr (gensym 'primsa))
       `(; allocate stack address
         ,(format "  %~a = alloca %struct.SinObj*, align 8" stackaddr)
         ; call prim, place into register
         ,(format "  %~a = call %struct.Sinobj* @~a(~a)" x (prim-name op)
                  (string-join (map (λ (a) (format "%struct.SinObj* %~a" a)) xs) ", "))
         ; store register value onto stack
         (format "  store volatile %struct.SinObj* %~a, %struct.SinOBj** %~a, align 8\n" x stackaddr))]
      [`(assign ,x (apply-prim ,op ,xx))
       (define stackaddr (gensym 'applyprimsa))
       `(; Allocate stack address
         ,(format "  %~a = alloca %struct.SinObj*, align 8" stackaddr)
         ; Call apply-prim, place into register
         ,(format "  %~a = call %struct.SinObj* @~a(%struct.SinObj* %~a)" x (applyprim-name op) arg)
         ; Store register value onto stack
         ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8\n" x stackaddr))]
      [`(assign ,x (phi (,x0 ,lx0) (,x1 ,lx1)))
       `(,(format "  %~a = phi %struct.SinObj* [%~a, %~a] [%~a, %~a]" x x0 lx0 x1 lx1))]
      [`(assign ,x ,r)
       (layout-r r (λ (final-exp) `(,(format "  %~a = %a" x final-exp))))]
      ; cant do pretty stuff with `r` like before, need to do hardcore moving!
      [`(return ,r)
       ; TODO: implement tail-call-elimination!?!
       (define ret-var (gensym 'ret))
       (layout-r r (λ (final-exp)
                     `(,(format "  %~a = ~a" ret-var final-exp)
                       ,(format "  ret %struct.SinObj* %~a" ret-var))))]))
  (define (layout-r r k)
    ; k is a callback function, it returns a list of strings (instructions).
    (match r
      [(? symbol? x) (k x)]
      [`(clo-app ,xf ,xx)
       (define clo-fn (gensym 'clofn))
       `(,(format "  %~a = call void ~a @closure_get_fn_part(%struct.SinObj* %~a)"
                  clo-fn "(%struct.SinObj*, %struct.SinObj*)*" xf)
         ,@(k (format "  call void %~a(%struct.SinObj* %~a, %struct.SinObj* %~a)"
                      clo-fn xf xx)))]
      [`(call/cc ,xf) (raise 'unimplemented-call/cc!)]))
  (foldr append '() (map layout-proc lir)))
