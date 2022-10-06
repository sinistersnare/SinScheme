#lang racket

(provide llvm-convert)

(require (only-in "utils.rkt" c-name prim-name applyprim-name
                  encoded-str-length c-hex-encode symbol-append
                  datum? prim?))

; Input Language (output of `ssa-closure-convert`):

; p ::= ((proc (x x x) e) ...)
; e ::= r
;     | (let ([x l]) e)
;     | (if x e e)
;     | (%%cond-bind x x e e e)
; l ::= r
;     | (make-closure x x ...)
;     | (env-ref x n)
;     | (quote dat)
;     | (prim op x ...)
;     | (apply-prim op x)
; r ::= x
;     | (call/cc x)
;     | (clo-app x x)


; Normalization w/ De Brujin indexing =>

; p ::= ((proc (x n  1 2) e) ...)
; e ::= r
;     | (let ([n l]) e)
;     | (if n e e)
;     | (%%cond-bind n n e e e)
; l ::= r
;     | (make-closure x n ...)
;     | (env-ref n n)
;     | (quote dat)
;     | (prim op n ...)
;     | (apply-prim op n)
; r ::= n
;     | (call/cc n)
;     | (clo-app n n)



; lir-convert =>

; prog ::= (f ...)
; f ::= p | s
; p ::= (proc (x n x x) (i i ...))
; s ::= (sub x (x b x) (i i ...))
; i ::= (label x)
;     | (bind x l)
;     | (if x (label x) (label x))
;     | (jump (label x) r)
;     | (return r)
; l ::= r
;     | (make-closure x x ...)
;     | (env-ref x n)
;     | (quote dat)
;     | (prim op x ...)
;     | (apply-prim op x)
; r ::= x
;     | (call/cc x)
;     | (clo-app x x)

; Output language is LLVM IR! I will NOT write _that_ grammar out!
; See https://www.llvm.org/docs/LangRef.html

;; LIR (low-level IR) is a list-based, imperative style IR.
;; It closely models the LLVM output, but does not use actual LLVM constructs.
;; The `nat` in the procedure prototype is the number of stack slots
;; required to run the function.
;;
;; One interesting thing in the LIR is that there is 2 'function' types
;; procedures, and subroutines (names are not linked to their
;; historical meanings, i just needed 2 names for functiony-things)
;; Procedures are the 'actual' functions that extend the stack when called
;; Subroutines are parts of procedures that we use in lieu of 'returning'
;; When using naked functions, we cant return, so we just jump to the rest.
;; Kind of like CPS-style, but we are manually managing our stack,
;; So even though LLVM functions aren't returning (Because no C-stack),
;; We handle those logistics ourselves.
;;
;; The bool in the subroutine is to note if its a function return or a phi-like return.
(define (lir-convert cloconv-program)

  ; Takes something in the closure-converted 'proc' language
  ; and transforms it to use de-brujin indices.
  ; The `self` reference is always 0, and the args are always `1`.
  ; The index given to the variables _are_ the offset of @fpr.
  (define (brujin clo-conv-ir)
    (define (indexify p)
      (match-define `(proc (,xname ,xself ,xargs) ,e) p)
      (match-define (list e-finished _ nslots) (indexify-e e (hash xself 1 xargs 2) 3))
      `(proc (,xname ,(sub1 nslots) 1 2) ,e-finished))
    (define (indexify-e e indexes n)
      (match e
        [`(let ([,x ,l]) ,ebody)
         (match-define (list body-finished body-indexes body-n)
           (indexify-e ebody (hash-set indexes x n) (add1 n)))
         (list
          `(let ([,n ,(indexify-l l indexes)]) ,body-finished)
          body-indexes
          body-n)]
        [`(if ,xc ,et ,ef)
         (match-define (list t-finished t-indexes t-n) (indexify-e et indexes n))
         (match-define (list f-finished f-indexes f-n) (indexify-e ef t-indexes t-n))
         (list
          `(if ,(hash-ref indexes xc) ,t-finished ,f-finished)
          f-indexes
          f-n)]
        [`(%%cond-bind ,xb ,xc ,et ,ef ,ej)
         (match-define (list t-fin t-indexes t-n) (indexify-e et indexes n))
         (match-define (list f-fin f-indexes f-n) (indexify-e ef t-indexes t-n))
         (match-define (list j-fin j-indexes j-n) (indexify-e ej
                                                              (hash-set f-indexes xb f-n)
                                                              (add1 f-n)))
         (list
          `(%%cond-bind ,f-n ,(hash-ref indexes xc) ,t-fin ,f-fin ,j-fin)
          j-indexes
          j-n)]
        [r (list (indexify-r r indexes) indexes n)]))
    (define (indexify-l l indexes)
      (match l
        [`(make-closure ,xf ,xs ...) `(make-closure ,xf ,@(map (λ (x) (hash-ref indexes x)) xs))]
        [`(env-ref ,x ,n) `(env-ref ,(hash-ref indexes x) ,n)]
        [`(quote ,_) l]
        [`(prim ,op ,xs ...) `(prim ,op ,@(map (λ (x) (hash-ref indexes x)) xs))]
        [`(apply-prim ,op ,x) `(apply-prim ,op ,(hash-ref indexes x))]
        [r (indexify-r r indexes)]))
    (define (indexify-r r indexes)
      (match r
        [`(call/cc ,x) `(call/cc ,(hash-ref indexes x))]
        [`(clo-app ,xf ,xx) `(clo-app ,(hash-ref indexes xf) ,(hash-ref indexes xx))]
        [(? symbol? x) (hash-ref indexes x)]))
    (map indexify clo-conv-ir))

  (define (lir-conv e procname nslots ret-goto)
    ; ret-goto helps us support %%cond-bind, it allows us to override
    ; return points to jump points, so we can correct join the %%cond-bind.
    ; If there are no %%cond-binds, ret-goto will always be #f.
    (match e
      [`(let ([,x (clo-app ,xf ,xx)]) ,ebody)
       (define goto-sub (gensym 'gotosub))
       (match-define (cons body-sub body-other) (lir-conv ebody procname nslots ret-goto))
       (cons `((jump ,goto-sub (clo-app ,xf ,xx)))
             `((sub ,procname (,goto-sub ,nslots ,#t ,x) ,body-sub)
               ,@body-other))]
      [`(let ([,x (call/cc ,xf)]) ,ebody)
       (define goto-sub (gensym 'gotosub))
       (match-define (cons body-sub body-rest) (lir-conv ebody procname nslots ret-goto))
       (cons `((jump ,goto-sub (call/cc ,xf)))
             `((sub ,procname (,goto-sub ,nslots ,#t ,x) ,body-sub)
               ,@body-rest))]
      [`(let ([,x ,l]) ,ebody)
       (match-define (cons conv subs) (lir-conv ebody procname nslots ret-goto))
       (cons `((bind ,x ,l) ,@conv)
             subs)]
      [(? number? xret)
       (cons (if ret-goto `((jump ,ret-goto ,xret)) `((return ,xret)))
             '())]
      [`(clo-app ,xf ,xx)
       (cons (if ret-goto
                 `((jump ,ret-goto (clo-app ,xf ,xx)))
                 `((return (clo-app ,xf ,xx))))
             '())]
      [`(call/cc ,xf)
       (cons (if ret-goto
                 `((jump ,ret-goto (call/cc ,xf)))
                 `((return (call/cc ,xf))))
             '())]
      [`(if ,xc ,et ,ef)
       (define base (gensym 'if))
       (define t (symbol-append base '_t))
       (define f (symbol-append base '_f))
       (match-define (cons here-t there-t) (lir-conv et procname nslots ret-goto))
       (match-define (cons here-f there-f) (lir-conv ef procname nslots ret-goto))
       (cons `((if ,xc (label ,t) (label ,f))
               (label ,t)
               ,@here-t
               (label ,f)
               ,@here-f)
             (append there-t there-f))]
      [`(%%cond-bind ,xb ,xc ,et ,ef ,ej)
       (define base (gensym 'phi))
       (define t (symbol-append base '_t))
       (define f (symbol-append base '_f))
       (define j (symbol-append base '_join))
       (match-define (cons here-t there-t) (lir-conv et procname nslots j))
       (match-define (cons here-f there-f) (lir-conv ef procname nslots j))
       (match-define (cons body-sub-j body-other-j) (lir-conv ej procname nslots ret-goto))
       (cons
        ; XXX: Its weird that we _need_ a subroutine for this, but its because
        ;      we can leave the function at any time inside the t/f branches
        ;      so we cant return to the join point if that happens.
        `((if ,xc (label ,t) (label ,f))
          (label ,t) ,@here-t (label ,f) ,@here-f)
        `((sub ,procname (,j ,nslots ,#f ,xb) ,body-sub-j)
          ,@there-t
          ,@there-f
          ,@body-other-j))]))

  (foldl (λ (cloconv-proc all-procs)
           (match-define `(proc (,xprocname ,nslots ,xself ,xargs) ,e) cloconv-proc)
           (match-define (cons proc-body subs) (lir-conv e xprocname nslots #f))
           ;(pretty-display `(nslots: ,xprocname ,nslots))
           `((proc (,xprocname ,nslots ,xself ,xargs) ,proc-body) ,@subs ,@all-procs))
         '() (brujin cloconv-program)))

; TODO: better indentation in LLVM-IR
; it would be nice to make labels something like `(label x e)`
; so we can properly indent the final LLVM IR.

; I think we can get by with just a naked-marked c-calling-convention.
(define calling-convention "ccc")

; TODO: some way not to hardcode these? Better than a magic number I guess
(define record-next-field-pos 1)
(define record-base-field-pos 0)
(define (compute-globals lir)
  ;; returns a hash with mapping ((cons type global-value-as-string) |-> global-name)
  ;; in the given list of procs.
  ;; The end result is something like
  ;;
  ;; (... (bind x 'boop) ...)
  ;; =>
  ;; globals -> (hash (cons 'sym "boop") .sym.global54463)
  (define (compute-globals-i i acc)
    (match i
      [`(bind ,_ (quote ,(? string? str)))
       (define key (cons 'str str))
       (hash-set acc key (cons (gensym '.str.global)
                               (encoded-str-length (c-hex-encode str))))]
      [`(bind ,_ (quote ,(? symbol? sym)))
       ; symbol->string so we dont accidentally try to eq? uninterned symbols.
       (define key (cons 'sym (symbol->string sym)))
       (hash-set acc key (cons (gensym '.sym.global)
                               (encoded-str-length (c-hex-encode (symbol->string sym)))))]
      [_ acc]))
  (foldl (λ (e done) (foldl compute-globals-i done e))
         (hash) (map caddr lir)))

(define (layout-globals globals)
  ; XXX: we can take this list and do (sort xs string<?) to order it!
  (foldl (λ (k acc)
           (match-define (cons _ val) k)
           (match-define (cons name size) (hash-ref globals k))
           `(,(format "; ~a" val)
             ,(format "@~a = private unnamed_addr constant [~a x i8] c\"~a\", align 1"
                      name size (c-hex-encode val))
             . ,acc))
         '() (hash-keys globals)))

(define (llvmir-ir->llvm-ir-string ir)
  (define (conv-args args)
    ; converts an args-list for a function call.
    (string-join (map (match-lambda [`(,argty ,argval)
                                     (format "~a ~a"
                                             (conv-type argty)
                                             (conv-name argval))]) args) ", "))
  (define (conv-name atom)
    (match atom
      [`(% ,sym) (format "%~a" sym)]
      [`(@ ,sym) (format "@~a" sym)]
      [(? number? n) (format "~a" n)]
      [_ (pretty-display `(unknown-atom ,atom)) (raise 'unknown-atom)]))
  (define (conv-type typ)
    (match typ
      ['SinFunc "void ()*"]
      ['SinObj "%struct.SinObj"]
      ['SinRecord "%struct.SinRecord"]
      ['i1 "i1"]
      ['i8 "i8"]
      ['i32 "i32"]
      ['i64 "i64"]
      [`(arr ,size ,typ) (format "[~a x ~a]" size (conv-type typ))]
      [`(* ,inner-typ) (format "~a*" (conv-type inner-typ))]
      [`(** ,inner-typ) (format "~a**" (conv-type inner-typ))]
      [`(*** ,inner-typ) (format "~a***" (conv-type inner-typ))]
      [`(**** ,inner-typ) (format "~a****" (conv-type inner-typ))]
      [_ (pretty-display `(unknown-type: ,typ)) (raise 'unknown-type)]))

  #;(pretty-display `(converting ,ir))
  (match ir
    [`(define-proc ,xname ,nslots)
     (format "define ~a void @~a() naked noreturn prefix i64 ~a {" calling-convention xname nslots)]
    [`(define-sub ,procname ,subname) (format "define ~a void @~a() naked noreturn alwaysinline {"
                                              calling-convention subname)]
    ['END "}\n"]
    ['unreachable "  unreachable"]
    ['DEBUG (string-append "  call void @debug_output_registers(%struct.SinRecord** @srr, "
                           "%struct.SinObj*** @fpr, %struct.SinObj*** @spr, "
                           "%struct.SinObj** @retr)")]
    [`(label ,name) (format "~a:" name)]
    [`(comment ,str) (format "  ; ~a" str)]
    [`(,to = load ,ty0 ,ty1 ,from) (format "  ~a = load ~a, ~a ~a, align 8"
                                           (conv-name to) (conv-type ty0)
                                           (conv-type ty1) (conv-name from))]
    [`(,to = getelementptr inbounds ,tyidx ,tyinput ,val ,idxs ...)
     (format "  ~a = getelementptr inbounds ~a, ~a ~a, ~a"
             (conv-name to) (conv-type tyidx) (conv-type tyinput)
             (conv-name val)
             (string-join (map (match-lambda [`(,idxty ,idxval)
                                              (format "~a ~a"
                                                      (conv-type idxty)
                                                      (conv-name idxval))]) idxs) ", "))]
    [`(,to = getelementptr ,tyidx ,tyinput ,val ,idxs ...)
     (format "  ~a = getelementptr ~a, ~a ~a, ~a"
             (conv-name to) (conv-type tyidx) (conv-type tyinput)
             (conv-name val)
             (string-join (map (match-lambda [`(,idxty ,idxval)
                                              (format "~a ~a"
                                                      (conv-type idxty)
                                                      (conv-name idxval))]) idxs) ", "))]
    [`(,to = inttoptr ,fromty ,from ,toty)
     (format "  ~a = inttoptr ~a ~a to ~a"
             (conv-name to) (conv-type fromty) (conv-name from) (conv-type toty))]
    [`(,to = ptrtoint ,fromty ,from ,toty)
     (format "  ~a = ptrtoint ~a ~a to ~a"
             (conv-name to) (conv-type fromty) (conv-name from) (conv-type toty))]
    [`(,to = mul ,ty ,name0 ,name1)
     (format "  ~a = mul ~a ~a, ~a"
             (conv-name to) (conv-type ty) (conv-name name0) (conv-name name1))]
    [`(,to = sub nuw nsw ,ty ,l ,r)
     (format "  ~a = sub nuw nsw ~a ~a, ~a"
             (conv-name to) (conv-type ty) (conv-name l) (conv-name r))]
    [`(,to = lshr ,ty ,val ,amt)
     (format "  ~a = lshr ~a ~a, ~a"
             (conv-name to) (conv-type ty) (conv-name val) (conv-name amt))]
    [`(,to = phi ,ty (,tval (label ,tlabel)) (,fval (label ,flabel)))
     (format "  ~a = phi ~a [~a, ~a], [~a, ~a]"
             (conv-name to) (conv-type ty)
             (conv-name tval) (conv-name tlabel)
             (conv-name fval) (conv-name flabel))]
    [`(call void ,name ,args)
     (format "  call void ~a(~a)"
             (conv-name name) (conv-args args))]
    [`(,to = call ,ty ,fn-name ,args)
     (format "  ~a = call ~a ~a(~a)"
             (conv-name to) (conv-type ty) (conv-name fn-name)
             (conv-args args))]
    [`(store ,tyfrom (inttoptr [,ity ,ival]) ,tyto ,to)
     (format "  store ~a inttoptr (~a ~a to ~a), ~a ~a, align 8"
             (conv-type tyfrom) (conv-type ity) (conv-name ival)
             (conv-type tyfrom) (conv-type tyto) (conv-name to))]
    [`(store ,tyfrom (bitcast [,ity ,ival]) ,tyto ,to)
     (format "  store ~a bitcast (~a ~a to ~a), ~a ~a, align 8"
             (conv-type tyfrom) (conv-type ity) (conv-name ival)
             (conv-type tyfrom) (conv-type tyto) (conv-name to))]
    [`(store ,tyfrom ,from ,tyto ,to)
     (format "  store ~a ~a, ~a ~a, align 8"
             (conv-type tyfrom) (conv-name from) (conv-type tyto) (conv-name to))]
    [`(,to = bitcast ,fromty ,from ,toty)
     (format "  ~a = bitcast ~a ~a to ~a"
             (conv-name to) (conv-type fromty) (conv-name from) (conv-type toty))]
    [`(indirectbr ,ty ,to ,possibilities)
     (format "  indirectbr ~a ~a, [~a]"
             (conv-type ty) (conv-name to)
             (string-join (map (match-lambda [`(label ,l) (format "label ~a" (conv-name l))])
                               possibilities) ", "))]
    [`(,to = ,toty blockaddress ,fn ,block)
     (format "  ~a = bitcast i8* blockaddress(~a, ~a) to ~a"
             (conv-name to) (conv-name fn) (conv-name block) (conv-type toty))]
    [`(br (label ,to))
     (format "  br label ~a" (conv-name to))]
    [`(br ,ty ,cmp (label ,t) (label ,f))
     (format "  br ~a ~a, label ~a, label ~a"
             (conv-type ty) (conv-name cmp) (conv-name t) (conv-name f))]
    [_ (pretty-display `(NO-MATCH: ,ir)) (raise 'no-match)]))

(define (layout-program lir globals)
  (define size-i 1)
  (define (arg-i i) (+ 2 i))
  (define (local-i i) (+ 4 i))
  (define (layout-prologue fp nslots)
    `((comment "Begin function prologue.")
      ((% ,fp) = load (** SinObj) (*** SinObj) (@ fpr))
      ,@(map (λ (i) `((% ,i) = getelementptr inbounds (* SinObj) (** SinObj) (% ,fp) (i64 ,(+ 1 i))))
             (range 1 (add1 nslots)))
      (comment "End function prologue.")))
  (define (layout-proc proc)
    (match-define `(proc (,procname ,nslots 1 2) ,e) proc)
    (define fp (gensym 'prologue_fp))
    `((define-proc ,procname ,nslots)
      ,@(layout-prologue fp nslots)
      ,@(foldr (λ (i acc) (append (layout-i i procname) acc)) '() e)
      END))
  (define (layout-sub sub)
    (match-define `(sub ,procname (,subname ,nslots ,is-return ,retval) ,e) sub)
    (define base (gensym 'sub_prologue))
    (define ret (cons (symbol-append base '_retr_val) retval))

    (define (layout-epilogue)
      (define base (gensym 'epilogue))
      (define callee-fp (symbol-append base '_callee_fp))
      (define slots-loc (symbol-append base '_slots_loc))
      (define slots-ptr (symbol-append base '_slots_ptr))
      (define slots (symbol-append base '_slots))
      (define offset (symbol-append base '_offset))
      (define caller-fp (symbol-append base '_caler_fp))
      `((comment "Laying out epilogue from the callee.")
        ((% ,callee-fp) = load (** SinObj) (*** SinObj) (@ fpr))
        ((% ,slots-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                        (% ,callee-fp) (i64 ,size-i))
        ((% ,slots-ptr) = load (* SinObj) (** SinObj) (% ,slots-loc))
        ((% ,slots) = ptrtoint (* SinObj) (% ,slots-ptr) i64)
        ((% ,offset) = mul i64 (% ,slots) -1)
        ((% ,caller-fp) = getelementptr inbounds (* SinObj) (** SinObj)
                        (% ,callee-fp) (i64 (% ,offset)))
        (store (** SinObj) (% ,caller-fp) (*** SinObj) (@ fpr))
        (comment "End Epilogue.")))
    `((define-sub ,procname ,subname)
      (comment ,(format "Subroutine for ~a" procname))
      ,@(layout-prologue (gensym 'prologue_fp) nslots)
      ,@(if is-return (layout-epilogue) '()) ; if doing a phi, no stack changes.
      ((% ,(car ret)) = load (* SinObj) (** SinObj) (@ retr))
      (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret)))
      ,@(foldr (λ (i acc) (append (layout-i i procname) acc)) '() e)
      END))
  (define (layout-i i procname)
    (match i
      [`(label ,x) `((label ,x))]
      [`(if ,xc (label ,xt) (label ,xf))
       (define base (gensym 'if))
       (define reg (cons (symbol-append base '_reg) xc))
       (define istrue (symbol-append base '_istrue))
       `((comment "starting if")
         ((% ,(car reg)) = load (* SinObj) (** SinObj) (% ,(cdr reg)))
         ((% ,istrue) = call i1 (@ is_truthy_value) ([(* SinObj) (% ,(car reg))]))
         (br i1 (% ,istrue) (label (% ,xt)) (label (% ,xf))))]
      [`(bind ,x (make-closure ,xname ,xfrees ...))
       (define base (gensym 'makeclo))
       (define ret (cons (symbol-append base '_ret) x))
       (define num-frees (length xfrees))
       (define frees (map (λ (a i) (cons (symbol-append base '_arg_ i) a))
                          xfrees (range (length xfrees))))
       `((comment "Starting make-closure.")
         ,@(map (λ (a) `((% ,(car a)) = load (* SinObj) (* SinObj) (% ,(cdr a)))) frees)
         ((% ,(car ret)) = call (* SinObj) (@ closure_alloc)
                         ([i64 ,num-frees] [SinFunc (@ ,xname)]))
         ,@(map (λ (f i) `(call void (@ closure_place_freevar)
                                ([(* SinObj) (% ,(car ret))]
                                 [(* SinObj) (% ,(car f))] [i64 ,i])))
                frees (range num-frees))
         (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
      [`(bind ,x (env-ref ,_ ,n))
       (define base (gensym 'envref))
       (define ret (cons (symbol-append base '_ret) x))
       (define fp (symbol-append base '_fp))
       (define self-loc (symbol-append base '_self_loc))
       (define self (symbol-append base '_self))
       `((comment "Starting env-ref.")
         ((% ,fp) = load (** SinObj) (*** SinObj) (@ fpr))
         ((% ,self-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                        (% ,fp) (i64 ,(arg-i 0)))
         ((% ,self) = load (* SinObj) (** SinObj) (% ,self-loc))
         ((% ,(car ret)) = call (* SinObj) (@ closure_get_env) ([(* SinObj) (% ,self)] [i64 ,n]))
         (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
      [`(bind ,x (quote ,(? boolean? b)))
       (define which (if b 'true 'false))
       (define ret (cons (gensym 'ret) x))
       `((comment ,(format "Starting const-bool: ~a." which))
         ((% ,(car ret)) = call (* SinObj) (@ ,(symbol-append 'const_init_ which)) ())
         (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
      [`(bind ,x (quote ,(? integer? n)))
       (define ret (cons (gensym 'ret) x))
       `((comment ,(format "Starting const-int: ~a." n))
         ((% ,(car ret)) = call (* SinObj) (@ const_init_int) ([i64 ,n]))
         (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
      [`(bind ,x (quote ()))
       (define ret (cons (gensym 'ret) x))
       `((comment "Starting const-null.")
         ((% ,(car ret)) = call (* SinObj) (@ const_init_null) ())
         (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
      [`(bind ,x ',(or (? symbol? s) (? string? s)))
       ; TODO: use symbol and string in the global table.
       ;       instead of sym and str, so we dont have to do this dance below.
       (define type (cond [(symbol? s) 'sym] [(string? s) 'str]))
       (define type-call (cond [(symbol? s) "symbol"] [(string? s) "string"]))
       (define as-string (cond [(symbol? s) (symbol->string s)] [(string? s) s]))
       (match-define (cons global-name size) (hash-ref globals (cons type as-string)))
       (define base (gensym (symbol-append 'dat_ type)))
       (define ret (cons (symbol-append base '_ret) x))
       (define raw (gensym 'rawglobal))
       `((comment (format "Starting const-global-~a." type))
         ((% ,raw) = getelementptr inbounds (arr ,size i8) (* (arr ,size i8))
                   (@ ,global-name) (i64 0) (i64 0))
         ((% ,(car ret)) = call (* SinObj) (@ ,(symbol-append 'const_init_ type-call))
                         ([(* i8) (% ,raw)]))
         (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
      [`(bind ,x (prim ,op ,xs ...))
       (define base (gensym 'prim))
       (define ret (cons (symbol-append base '_ret) x))
       (define args (map (λ (a i) (cons (symbol-append base '_arg_ i) a))
                         xs (range (length xs))))
       `((comment "Starting prim.")
         ,@(map (λ (a) `((% ,(car a)) = load (* SinObj) (** SinObj) (% ,(cdr a)))) args)
         ((% ,(car ret)) = call (* SinObj) (@ ,(prim-name op))
                         ,(map (λ (a) `[(* SinObj) (% ,(car a))]) args))
         (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
      [`(bind ,x (apply-prim ,op ,xx))
       (define base (gensym 'applyprim))
       (define ret (cons (symbol-append base '_ret) x))
       (define arg (cons (symbol-append base '_arg) xx))
       `((comment "Starting apply-prim.")
         ((% ,(car arg)) = load (* SinObj) (** SinObj) (% ,(cdr arg)))
         ((% ,(car ret)) = call (* SinObj) (@ ,(applyprim-name op))
                         ([(* SinObj) (% ,(car arg))]))
         (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
      [`(bind ,(? number? x) ,(? number? y))
       (define alias (gensym 'alias))
       `((comment "Starting alias.")
         ((% ,alias) = load (* SinObj) (** SinObj) (% ,y))
         (store (* SinObj) (% ,alias) (** SinObj) (% ,x)))]

      ; clo-app stuff
      [`(jump ,subname (clo-app ,xf ,xx))
       (define base (gensym 'direct_cloapp))
       (define clo (cons (symbol-append base '_clo) xf))
       (define app-args (cons (symbol-append base '_app_args) xx))
       (define nslots (symbol-append base '_nslots))
       (define nslots-sinobj (symbol-append base '_nslots_sinobj))
       (define caller-fp (symbol-append base '_caller_fp))
       (define callee-fp (symbol-append base '_callee_fp))
       (define return-addr (symbol-append base '_return_addr))
       (define size-loc (symbol-append base '_size_loc))
       (define arg0-loc (symbol-append base '_arg0_loc))
       (define arg1-loc (symbol-append base '_arg1_loc))
       (define funcptr (symbol-append base '_funcptr))
       `((comment "Starting direct clo-app.")
         ,@(insert-overflow-check base procname)
         ((% ,(car clo)) = load (* SinObj) (** SinObj) (% ,(cdr clo)))
         ((% ,(car app-args)) = load (* SinObj) (** SinObj) (% ,(cdr app-args)))
         ,@(insert-get-prefix-data base procname nslots)
         ((% ,nslots-sinobj) = inttoptr i64 (% ,nslots) (* SinObj))
         ((% ,caller-fp) = load (** SinObj) (*** SinObj) (@ fpr))
         ((% ,callee-fp) = getelementptr inbounds (* SinObj) (** SinObj)
                         (% ,caller-fp) (i64 (% ,nslots)))
         (store (** SinObj) (% ,callee-fp) (*** SinObj) (@ fpr))
         ((% ,return-addr) = bitcast SinFunc (@ ,subname) (* SinObj))
         ((% ,size-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                        (% ,callee-fp) (i64 ,size-i))
         ((% ,arg0-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                        (% ,callee-fp) (i64 ,(arg-i 0)))
         ((% ,arg1-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                        (% ,callee-fp) (i64 ,(arg-i 1)))
         (store (* SinObj) (% ,return-addr) (** SinObj) (% ,callee-fp))
         (store (* SinObj) (% ,nslots-sinobj) (** SinObj) (% ,size-loc))
         (store (* SinObj) (% ,(car clo)) (** SinObj) (% ,arg0-loc))
         (store (* SinObj) (% ,(car app-args)) (** SinObj) (% ,arg1-loc))
         ((% ,funcptr) = call SinFunc (@ closure_get_fn_part) ([(* SinObj) (% ,(car clo))]))
         (call void (% ,funcptr) ())
         unreachable)]
      [`(return (clo-app ,xf ,xx))
       (define base (gensym 'tail_cloapp))
       (define clo (cons (symbol-append base '_clo) xf))
       (define app-args (cons (symbol-append base '_app_args) xx))
       (define fp (symbol-append base '_fp))
       (define nslots (symbol-append base '_nslots))
       (define size-loc (symbol-append base '_size_loc))
       (define arg0-loc (symbol-append base '_arg0_loc))
       (define arg1-loc (symbol-append base '_arg1_loc))
       (define nslots-int (symbol-append '_nslots_int))
       (define funcptr (symbol-append base '_funcptr))
       `((comment "Starting tail clo-app.")
         ((% ,(car clo)) = load (* SinObj) (** SinObj) (% ,(cdr clo)))
         ((% ,(car app-args)) = load (* SinObj) (** SinObj) (% ,(cdr app-args)))
         ((% ,fp) = load (** SinObj) (*** SinObj) (@ fpr))
         ((% ,size-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                        (% ,fp) (i64 ,size-i))
         ((% ,arg0-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                        (% ,fp) (i64 ,(arg-i 0)))
         ((% ,arg1-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                        (% ,fp) (i64 ,(arg-i 1)))
         ,@(insert-get-prefix-data base procname nslots)
         ((% ,nslots-int) = inttoptr i64 (% ,nslots) (* SinObj))
         (store (* SinObj) (% ,nslots-int) (** SinObj) (% ,size-loc))
         (store (* SinObj) (% ,(car clo)) (** SinObj) (% ,arg0-loc))
         (store (* SinObj) (% ,(car app-args)) (** SinObj) (% ,arg1-loc))
         ((% ,funcptr) = call SinFunc (@ closure_get_fn_part) ([(* SinObj) (% ,(car clo))]))
         (call void (% ,funcptr) ())
         unreachable)]

      ; call/cc stuff.
      [`(jump ,subname (call/cc ,xf))
       (define base (gensym 'direct_callcc))
       (define cont-record (symbol-append 'cont_record))
       `((comment "Starting direct call/cc.")
         ,@(insert-overflow-check base procname)
         ,@(insert-stack-splitter base procname cont-record `(@ ,subname) #f)
         ,@(insert-call/cc-code base cont-record xf)
         unreachable)]
      [`(return (call/cc ,xf))
       (define base (gensym 'tail_callcc))
       (define cont-record (symbol-append base '_cont_record))
       (define fp (symbol-append base '_fp))
       (define ret-addr-sinobj (symbol-append base '_ret_addr_sinobj))
       (define ret-addr (symbol-append base '_ret_addr))
       `((comment "Starting tail call/cc.")
         ((% ,fp) = load (** SinObj) (*** SinObj) (@ fpr))
         ((% ,ret-addr-sinobj) = load (* SinObj) (** SinObj) (% ,fp))
         ((% ,ret-addr) = bitcast (* SinObj) (% ,ret-addr-sinobj) SinFunc)
         ,@(insert-stack-splitter base procname cont-record `(% ,ret-addr) #t)
         ,@(insert-call/cc-code base cont-record xf)
         unreachable)]

      ; Actually returning for once.
      [`(jump ,subname ,(? number? x))
       (define base (gensym 'direct_return))
       (define ret (cons (symbol-append base '_ret) x))
       `((comment "Starting direct return.")
         ((% ,(car ret)) = load (* SinObj) (** SinObj) (% ,(cdr ret)))
         (store (* SinObj) (% ,(car ret)) (** SinObj) (@ retr))
         (call void (@ ,subname) ())
         unreachable)]
      [`(return ,(? number? x))
       (define base (gensym 'tail_return))
       (define ret (cons (symbol-append base '_ret) x))
       (define fp (symbol-append '_fp))
       (define ra-sinobj (symbol-append '_ra_sinobj))
       (define ra-sinfunc (symbol-append '_ra_sinfunc))
       `((comment "Starting tail return.")
         ((% ,(car ret)) = load (* SinObj) (** SinObj) (% ,(cdr ret)))
         (store (* SinObj) (% ,(car ret)) (** SinObj) (@ retr))
         ((% ,fp) = load (** SinObj) (*** SinObj) (@ fpr))
         ((% ,ra-sinobj) = load (* SinObj) (** SinObj) (% ,fp))
         ((% ,ra-sinfunc) = bitcast (* SinObj) (% ,ra-sinobj) SinFunc)
         (call void (% ,ra-sinfunc) ())
         unreachable)]))

  (define (insert-call/cc-code start-base cont-record xf-mapping)
    ; XXX: we could combine this with the stack-splitting code.
    (define base (symbol-append start-base '_call))
    (define fp (symbol-append base '_fp))
    (define cont-clo (symbol-append base '_cont_clo))
    (define size-loc (symbol-append base '_size_loc))
    (define arg0-loc (symbol-append base '_arg0_loc))
    (define arg1-loc (symbol-append base '_arg1_loc))
    (define arglist-null (symbol-append base '_arglist_null))
    (define arglist (symbol-append base '_arglist))
    (define clo (cons (symbol-append base '_clo) xf-mapping))
    (define clo-fn-part (symbol-append base 'clo_fn_part))
    `(((% ,cont-clo) = call (* SinObj) (@ make_continuation_closure)
                     ([(* SinRecord) (% ,cont-record)]
                      [SinFunc (@ __continuation_function_handler)]))
      ((% ,fp) = load (** SinObj) (*** SinObj) (@ fpr))
      ; get stack-slot locations
      ((% ,size-loc) = getelementptr inbounds (* SinObj) (** SinObj) (% ,fp) (i64 ,size-i))
      ((% ,arg0-loc) = getelementptr inbounds (* SinObj) (** SinObj) (% ,fp) (i64 ,(arg-i 0)))
      ((% ,arg1-loc) = getelementptr inbounds (* SinObj) (** SinObj) (% ,fp) (i64 ,(arg-i 1)))
      ; Create arg-list with the cont-clo
      ((% ,arglist-null) = call (* SinObj) (@ const_init_null) ())
      ((% ,arglist) = call (* SinObj) (@ prim_cons) ([(* SinObj) (% ,cont-clo)]
                                                     [(* SinObj) (% ,arglist-null)]))
      ; place data in stack-slots
      ((% ,(car clo)) = load (* SinObj) (** SinObj) (% ,(cdr clo)))
      (store (* SinObj) (bitcast [SinFunc (@ __underflow_handler)]) (** SinObj) (% ,fp))
      (store (* SinObj) (inttoptr [i64 -1]) (** SinObj) (% ,size-loc))
      (store (* SinObj) (% ,(car clo)) (** SinObj) (% ,arg0-loc))
      (store (* SinObj) (% ,arglist) (** SinObj) (% ,arg1-loc))
      ((% ,clo-fn-part) = call SinFunc (@ closure_get_fn_part) ([(* SinObj) (% ,(car clo))]))
      (call void (% ,clo-fn-part) ())))

  (define (insert-stack-splitter start-base procname record-to-use return-address tail-call)
    ; TODO: can this be done in the runtime?
    ; the `record-to-use` is only a symbol, we will wrap it in a register.
    ; the `return-address` should be fully formatted... for... reasons?
    (define base (symbol-append start-base '_stacksplit))
    (define shrinking-record (symbol-append base '_shrinking_record))
    (define at-base (symbol-append base '_at_base))
    (define dont-split (symbol-append base '_dont_split))
    (define next-record-loc (symbol-append base '_next_record_loc))
    (define next-record (symbol-append base '_next_record))
    (define finished-stack-splitting (symbol-append base '_finished_stack_splitting))
    (define do-split (symbol-append base '_do_split))
    (define slots-in-caller (symbol-append base '_slots_in_caller))
    (define caller-fp (symbol-append base '_caller_fp))
    (define new-base (symbol-append base '_new_base))
    (define new-base-int (symbol-append base '_new_base_int))
    (define shrinking-record-base-loc (symbol-append base '_shrinking_record_base_loc))
    (define shrinking-record-base (symbol-append base '_shrinking_record-base))
    (define shrinking-record-base-int (symbol-append base '_shrinking_record_base_int))
    (define shrunk-record-size-bytes (symbol-append base '_shrunk_record_size_bytes))
    (define shrunk-record-size (symbol-append base '_shrunk_record_size))
    (define slots-in-grand-caller-loc (symbol-append base '_slots_in_grand_caller_loc))
    (define slots-in-grand-caller-sinobj (symbol-append base '_slots_in_grand_caller_sinobj))
    (define shrunk-record-fp-offset (symbol-append base '_shrunk_record_fp_offset))
    (define new-top-segment (symbol-append base '_new_top_segment))
    `((comment "Starting stack-split.")
      ((% ,shrinking-record) = load (* SinRecord) (** SinRecord) (@ srr))
      ((% ,at-base) = call i1 (@ callcc_at_base) ([(** SinRecord) (@ srr)] [(*** SinObj) (@ fpr)]))
      (br i1 (% ,at-base) (label (% ,dont-split)) (label (% ,do-split)))
      (label ,dont-split)
      ; dont shrink record cause at base, just use the next record.
      ((% ,next-record-loc) = getelementptr inbounds SinRecord (* SinRecord)
                            (% ,shrinking-record) (i64 0) (i32 ,record-next-field-pos))
      ((% ,next-record) = load (* SinRecord) (** SinRecord) (% ,next-record-loc))
      (br (label (% ,finished-stack-splitting)))
      (label ,do-split)
      ; When we aren't at the base, we need to split at a specific point,
      ; based on if its a tail-call. So gather the arguments that way.
      ;
      ((% ,caller-fp) = load (** SinObj) (*** SinObj) (@ fpr))
      ,@(insert-get-prefix-data base procname slots-in-caller)
      ; Start with calculating the new base.
      ,@(if tail-call
            `(((% ,new-base) = getelementptr inbounds (* SinObj) (** SinObj)
                             (% ,caller-fp) (i64 0)))
            ; direct call
            `(((% ,new-base) = getelementptr inbounds (* SinObj) (** SinObj)
                             (% ,caller-fp) (i64 (% ,slots-in-caller)))))
      ; Calculate the size of the shrunk record
      ((% ,new-base-int) = ptrtoint (** SinObj) (% ,new-base) i64)
      ((% ,shrinking-record-base-loc) = getelementptr inbounds SinRecord (* SinRecord)
                                      (% ,shrinking-record) (i32 0) (i32 ,record-base-field-pos))
      ((% ,shrinking-record-base) = load (** SinObj) (*** SinObj) (% ,shrinking-record-base-loc))
      ((% ,shrinking-record-base-int) ptrtoint (** SinObj) (% ,shrinking-record-base) i64)
      ((% ,shrunk-record-size-bytes) = sub nuw nsw i64 (% ,new-base-int)
                                     (% ,shrinking-record-base-int))
      ((% ,shrunk-record-size) = lshr i64 (% ,shrunk-record-size-bytes) 3)
      ; Calculate the offset to the fp for the shrunk record
      ; In a direct-call, we just use the caller-fp and do the math
      ; In a tail-call, we need to get the offset to the grand-caller.
      ,@(if tail-call
            `(((% ,slots-in-grand-caller-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                                              (% ,caller-fp) (i64 ,size-i))
              ((% ,slots-in-grand-caller-sinobj) = load (* SinObj) (** SinObj)
                                                 (% ,slots-in-grand-caller-loc))
              ((% ,shrunk-record-fp-offset) = ptrtoint (* SinObj)
                                            (% ,slots-in-grand-caller-sinobj) i64))
            `((% ,shrunk-record-fp-offset) = bitcast i64 (% ,slots-in-caller) i64))
      ; the return address is provided by the caller of this helper function.
      ((% ,new-top-segment) = call (* SinRecord) (@ split_record)
                            ([(* SinRecord) (% ,shrinking-record)]
                             [(** SinObj) (% ,new-base)]
                             [i64 (% ,shrunk-record-size)]
                             [i64 (% ,shrunk-record-fp-offset)]
                             ; The `return-address` argument should be formatted already.
                             ; because it may be a global identifier, not a local.
                             [SinFunc ,return-address]))
      (store (* SinRecord) (% ,new-top-segment) (** SinRecord) (@ srr))
      (br (label (% ,finished-stack-splitting)))
      ; Done!
      (label (finished-stack-splitting))
      ((% ,record-to-use) = phi (* SinRecord)
                          [(% ,next-record) (label (% ,dont-split))]
                          [(% ,shrinking-record) (label (% ,do-split))])
      (comment "Finished stack-split.")))

  (define (insert-get-prefix-data start-base procname nslots)
    (define base (symbol-append start-base '_prefixdata))
    (define proc-ptr (symbol-append base '_proc_ptr))
    (define nslots-loc (symbol-append base '_nslots_loc))
    ; Utilize LLVM's prefix data to get the size of the current proc in stack-slots.
    ; https://llvm.org/docs/LangRef.html#prefix-data
    ; This mirrors what is done in Dybvig et al. 1990 ("Representing Control ...")
    `(((% ,proc-ptr) = bitcast SinFunc (@ ,procname) (* i64))
      ((% ,nslots-loc) = getelementptr inbounds i64 (* i64) (% ,proc-ptr) (i64 -1))
      ((% ,nslots) = load i64 (* i64) (% ,nslots-loc))))

  (define (insert-overflow-check start-base procname)
    (define base (symbol-append start-base '_overflowcheck))
    (define nslots (symbol-append base '_nslots))
    (define has-space (symbol-append base '_has_space))
    (define does-have-space (symbol-append base '_does_have_space))
    (define overflow-first (symbol-append base '_overflow_first))
    `((comment "Beginning overflow check.")
      ,@(insert-get-prefix-data base procname nslots)
      ((% ,has-space) = call i1 (@ check_for_overflow)
                      ([(*** SinObj) (@ fpr)]
                       [(*** SinObj) (@ spr)]
                       [i64 (% ,nslots)]))
      (br i1 (% ,has-space) (label (% ,does-have-space)) (label (% ,overflow-first)))
      (label ,overflow-first)
      (call void (@ handle_overflow) ([(** SinRecord) (@ srr)]
                                      [(*** SinObj) (@ fpr)]
                                      [(*** SinObj) (@ spr)]
                                      [SinFunc (@ __underflow_handler)]
                                      [i64 (% ,nslots)]))
      ; We have space now! Continue on your merry way.
      (br (label (% ,does-have-space)))
      (label ,does-have-space)
      (comment "End overflow check.")))

  (foldl (λ (f facc)
           (match f
             [`(sub ,_ ...) (append (layout-sub f) facc)]
             [`(proc ,_ ...) (append (layout-proc f) facc)]))
         '() lir))

; TODO: put __ before runtime functions in runtime.cpp/.h to avoid name conflicts?
;       Couldnt a user make a function called handle_continuation_function and it fuck with us?
(define global-registers-code
  `(";; Global registers! ;;"
    "; stack record register, linked list of stack records."
    "@srr = global %struct.SinRecord* null, align 8"
    "; frame pointer register."
    "@fpr = global %struct.SinObj** null, align 8"
    "; Return Value register"
    "@retr = global %struct.SinObj* null, align 8"
    "; Stack Protection Register"
    "@spr = global %struct.SinObj** null, align 8"
    ";; End global registers! ;;"))

(define underflow-handler-code
  `("define void @__underflow_handler() naked noreturn {"
    ,(string-append "  %underflow_ret_addr = call void ()* @handle_underflow"
                    "(%struct.SinRecord** @srr, %struct.SinObj*** @fpr, %struct.SinObj*** @spr)")
    "call void %underflow_ret_addr()"
    "  unreachable"
    "}"))

(define continuation-function-handler-code
  `("define void @__continuation_function_handler() naked noreturn {"
    "  ; Make the runtime do all the work!"
    ,(string-append "  %contfnhandler_ret_addr = call void ()* @handle_continuation_function"
                    "(%struct.SinRecord** @srr, %struct.SinObj*** @fpr, "
                    "%struct.SinObj*** @spr, %struct.SinObj** @retr)")
    "  call void %contfnhandler_ret_addr()"
    "  unreachable"
    "}"))

(define program-finished-code
  `("define void @__program_finished() naked noreturn {"
    "  ; Dont worry about cleanup, just halt out of here."
    "  %finished_program_output = load %struct.SinObj*, %struct.SinObj** @retr, align 8"
    "  call %struct.SinObj* @prim_halt(%struct.SinObj* %finished_program_output)"
    "  unreachable"
    "}"))

(define main-function-code
  `("define i32 @main() {"
    ,(string-append "  call void @start_runtime"
                    "(%struct.SinRecord** @srr, %struct.SinObj*** @fpr, %struct.SinObj*** @spr)")
    "  %base_fp = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8"
    ,(string-append "  store %struct.SinObj* bitcast "
                    "(void ()* @__program_finished to %struct.SinObj*), "
                    "%struct.SinObj** %base_fp, align 8")
    "  call void @__main()"
    "  unreachable"
    "}"))

(define program-start
  `(,@global-registers-code
    ";; supporting runtime functions ;;"
    ,@underflow-handler-code
    ""
    ,@continuation-function-handler-code
    ""
    ,@program-finished-code
    ""
    ,@main-function-code))

(define (llvm-conv lir)
  ; TODO: should we normalize function names?
  ;       Perhaps just use the LLVM ability to quote a name like:
  ;       @__main becomes @"__main"?
  #;(define norm-lir (normalize-names lir))
  (match-define globals (compute-globals lir))
  (define user-code (layout-program lir globals))
  (string-join
   `("; Begin user globals."
     ,@(layout-globals globals)
     "; End user globals."
     ""
     ,@program-start
     "; Start user program."
     ,@(map llvmir-ir->llvm-ir-string user-code)
     "; End user program.")
   "\n"))

(define (llvm-convert proc)
  (define lir (lir-convert proc))
  (llvm-conv lir))

