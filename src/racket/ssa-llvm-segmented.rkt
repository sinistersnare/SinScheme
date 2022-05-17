#lang racket

(provide lir-convert llvm-convert)

(require (only-in "utils.rkt" c-name prim-name applyprim-name
                  encoded-str-length c-hex-encode symbol-append
                  datum? prim?))

; Input Language (output of `ssa-closure-convert`):

; p ::= ((proc (x x x) e) b...)
; e ::= r
;     | (let ([x l]) e)
;     | (if x e e)
;     | (cond-bind x x e e e)
; l ::= r
;     | (make-closure x x ...)
;     | (env-ref x nat)
;     | (quote dat)
;     | (prim op x ...)
;     | (apply-prim op x)
; r ::= x
;     | (call/cc x)
;     | (clo-app x x)


; lir-convert =>

; p ::= ((proc (x nat x x) e) ...)
; e ::= (i ... (return r))
; i ::= (bind x l)
;     | (if x (label x) (label x))
;     | (jump (label x))
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

; Output language is LLVM IR! I will NOT write _that_ grammar out!
; See https://www.llvm.org/docs/LangRef.html

; low-level IR is a list-based, imperative style IR
; designed to be easily converted into an imperative asm language
; such as LLVM IR or ASM directly.
; the `nat` in the proc is for the amount of stack slots needed.
; This is the 2 arguments + the amount of locals.
(define (lir-convert procs)
  (define (conv-l l)
    (match l
      [(or `(make-closure ,_ . ,_) `(env-ref ,_ ,_) `(quote ,_)
           `(prim ,_ . ,_) `(apply-prim ,_ ,_)) l]
      [r (conv-r r)]))
  (define (conv-r r)
    (match r
      [`(call/cc ,x) `(call/cc ,x)]
      [`(clo-app ,xf ,xx) `(clo-app ,xf ,xx)]
      [(? symbol? x) x]))
  (define (conv-e e)
    (match e
      [`(let ([,x ,l]) ,e)
       (cons `(bind ,x ,(conv-l l))
             (conv-e e))]
      [`(if ,x ,et ,ef)
       (define label-base (gensym 'branch))
       (define t-label (symbol-append label-base '-true))
       (define f-label (symbol-append label-base '-false))
       `((if ,x (label ,t-label) (label ,f-label))
         (label ,t-label)
         ,@(conv-e et)
         (label ,f-label)
         ,@(conv-e ef))]
      [`(cond-bind ,x ,xc ,et ,ef ,ejoin)
       (define phi-base (symbol-append (gensym 'branch-tmp) '- x))
       (define phi-t (symbol-append phi-base '-true))
       (define phi-f (symbol-append phi-base '-false))
       (define branch-label-base (symbol-append (gensym 'branch) '- x '-))
       (define branch-label-true (symbol-append branch-label-base '-true))
       (define branch-label-false (symbol-append branch-label-base '-false))
       (define branch-label-join (symbol-append branch-label-base '-join))
       ; TODO: is there a better way? This is an ugly hack,
       ; we replace the output `return` with a jump to the join.
       (define t-conv
         (match-let ([`(,is ... (return ,tret)) (conv-e et)])
           `((label ,branch-label-true)
             ,@is
             (bind ,phi-t ,tret)
             (jump ,branch-label-join))))
       (define f-conv
         (match-let ([`(,is ... (return ,fret)) (conv-e ef)])
           `((label ,branch-label-false)
             ,@is
             (bind ,phi-f ,fret)
             (jump ,branch-label-join))))
       (define join-conv
         `((label ,branch-label-join)
           (bind ,x (phi (,phi-t (label ,branch-label-true))
                           (,phi-f (label ,branch-label-false))))
           ,@(conv-e ejoin)))
       `((if ,xc (label ,branch-label-true) (label ,branch-label-false))
         ,@t-conv
         ,@f-conv
         ,@join-conv)]
      ; else we are in tail position
      [r `((return ,(conv-r r)))]))
  (define (num-slots body)
    (define (locals-in-i i)
      (match i
        [`(bind . ,_) 1]
        [_ 0]))
    ; Closure needs 2 slots for return-address and #slots, 2 for args,
    ; so 4, then add the number required by locals.
    (foldl + 4 (map locals-in-i body)))
  (define (conv-proc proc)
    (match-define `(proc (,xname ,xclo ,xargs) ,ebody) proc)
    (define converted-body (conv-e ebody))
    (define nslots (num-slots converted-body))
    `(proc (,xname ,nslots ,xclo ,xargs) ,converted-body))
  (map conv-proc procs))

(define (all-return-points lir)
  ; returns a list of symbols, locations that can be returned to, including the
  ; underflow_handler and program_finished
  (define (proc-return-points p pacc)
    (match-define `(proc (,xname ,_ ,_ ,_) ,is) p)
    (define (i-return-points i iacc)
      ;(pretty-display i)
      (match i
        [(or `(bind ,_ (clo-app ,_ ,_)) `(bind ,_ (call/cc ,_)))
         (match-define (cons cur points) iacc)
         ;(displayln xname)
         (cons (add1 cur) (cons (symbol-append xname '_ret cur) points))]
        [_ iacc]))
    (define i-return-pts (foldl i-return-points (cons 0 '()) is))
    (append (cdr i-return-pts) pacc))
  (foldl proc-return-points '(underflow_handler program_finished) lir))

(define (all-call-points lir)
  ; returns a list of symbols, locations that can be called to, including main.
  (foldl (λ (p acc)
           (match-define `(proc (,xname ,_ ,_ ,_) ,_) p)
           (cons xname acc))
         '() lir))

; it would be nice to make labels something like `(label x e)`
; so we can properly indent the final LLVM IR.
; i think it would require the conv-* functions to take a `final` argument
; to determine what to do in the arguments tail position. We usually
; want to just `(return ,final)` but sometimes (like in a phi-join)
; we want other things. And for branches, we need to do both, not just the last,
; which is arbitrarily the false branch.

; The Scheme program is compiled into one giant LLVM function.
; Calls are handled with `indirectbr`, and the ABI is poorly documented.
; Hopefully its described in src/cpp/README.md or elsewhere.
;
; First, we need a pass converting local names into numbers
; (much like how clang does it).

(define (numerize-locals proc)
  ; Takes a single proc and returns a hash
  ; mapping the locals in that proc to numbers.
  (match-define `(proc (,xname ,_ ,xclo ,xarglist) ,body) proc)
  (define counter 0)
  (define (++!) (define val counter) (set! counter (add1 counter)) val)
  ; TODO: should these include the name somewhere?
  (define starting (hash xclo (format "~a_arg~a_loc"
                                      xname 0)
                         xarglist (format "~a_arg~a_loc"
                                          xname 1)))
  (foldl (λ (instr acc)
           (match instr
             [`(bind ,x ,_)
              (define num (++!))
              ; TODO: should `,x` be in the name here somewhere?
              ; TODO: Should locals have a gensymd base?
              ;       Or is the function name enough?
              ;       This should probably be computed per-function
              ;       Not on the whole program.
              (define stackname (format "~a_local~a_loc" xname num))
              (hash-set acc x stackname)]
             [_ acc]))
         starting body))

(define (normalize-names lir)
  ;; converts all variable names from a 'lisp style' (i.e. kebab case with any symbol)
  ;; into a c-style (only character class \w allowed basically)
  (define (normalize x) (string->symbol (c-name x)))
  (define (norm-proc proc)
    (match-define `(proc (,name ,nslots ,env ,args) ,e) proc)
    `(proc (,(normalize name) ,nslots ,(normalize env) ,(normalize args))
           ,(map norm-i e)))
  (define (norm-i i)
    (match i
      [`(bind ,x ,l)
       `(bind ,(normalize x) ,(norm-l l))]
      [`(if ,xc (label ,xt) (label ,xf))
       `(if ,(normalize xc) (label ,(normalize xt)) (label ,(normalize xf)))]
      [`(jump (label ,x)) `(jump (label ,(normalize x)))]
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
      [`(clo-app ,xf ,xx) `(clo-app ,(normalize xf) ,(normalize xx))]
      [(? symbol? x) (normalize x)]))
  (map norm-proc lir))
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
  ; TODO: we can take this list and do (sort xs string<?) to order it!
  (foldl (λ (k acc)
           (match-define (cons val _) k)
           (match-define (cons name size) (hash-ref globals k))
           (cons (format "@~a = private unnamed_addr constant [~a x i8] c\"~a\""
                         name size val)
                 acc))
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
    [`(label ,name) (format "~a:" name)]
    [`(comment ,str) (format "  ; ~a" str)]
    [`(,to = load ,ty0 ,ty1 ,from) (format "  ~a = load volatile ~a, ~a ~a, align 8"
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
    [`(call void ,name ,args)
     (format "  call void ~a(~a)"
             (conv-name name) (conv-args args))]
    [`(,to = call ,ty ,fn-name ,args)
     (format "  ~a = call ~a ~a(~a)"
             (conv-name to) (conv-type ty) (conv-name fn-name)
             (conv-args args))]
    [`(store ,tyfrom ,from ,tyto ,to)
     (format "  store volatile ~a ~a, ~a ~a, align 8"
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
; (NO-MATCH: ((% makeclo_anf_45bind41248624377335_funcaddr) = (* i8) blockaddress (@ main) (% clo4124867)))
(define (layout-program lir globals)
  (define call-points (all-call-points lir))
  (define return-points (all-return-points lir))
  (define start-of-program
    `(";; Global registers ;;"
      "; stack record register, linked list of stack records."
      "@srr = global %struct.SinRecord* null, align 8"
      "; frame pointer register."
      "@fpr = global %struct.SinObj** null, align 8"
      "; Return Value register"
      "@retr = global %struct.SinObj* null, align 8"
      "; Stack Protection Register"
      "@spr = global %struct.SinObj** null, align 8"
      ";; End global pointers ;;"
      ""
      "define i32 @main() {"
      ,(string-append "  call void @start_runtime(%struct.SinRecord** @srr,"
                      " %struct.SinObj*** @fpr, %struct.SinObj*** @spr)")
      "  br label %main"))
  (define end-of-program
    `("underflow_handler:"
      ,(string-append "  %underflow_ret_addr = call i8* @handle_underflow"
                      "(%struct.SinRecord** @srr, %struct.SinObj*** @fpr, %struct.SinObj*** @spr)")
      ,(format "  indirectbr i8* %underflow_ret_addr, [~a]"
               (string-join (map (λ (r) (format "label %~a" r)) return-points) ", "))
      "continuation_function_handler:"
      "  ; The runtime will handle this!!"
      ,(string-append "  %contfnhandler_ret_addr = call i8* @handle_continuation_function"
                      "(%struct.SinRecord** @srr, %struct.SinObj*** @fpr, "
                      "%struct.SinObj*** @spr, %struct.SinObj** @retr)")
      ,(format "  indirectbr i8* %contfnhandler_ret_addr, [~a]"
               (string-join (map (λ (r) (format "label %~a" r)) return-points) ", "))
      "program_finished:"
      "  ; Don't worry about cleanup, just get out."
      "  %finished_program_output = load %struct.SinObj*, %struct.SinObj** @retr, align 8"
      "  %finished_tmp = call %struct.SinObj* @prim_halt(%struct.SinObj* %finished_program_output)"
      "  ret i32 0"
      "}"))
  (define (layout-proc proc)
    (define local-mapping (numerize-locals proc))
    (match-define `(proc (,xname ,nslots ,xclo ,xarglist) ,instructions) proc)
    (define size-pos 1)
    (define (arg-i i) (+ 2 i))
    (define (local-i i) (+ 4 i))

    (define (layout-prologue)
      ; Need to add frame size into stack.
      ; frame size is:
      ; 1 for return-address
      ; 1 for frame size
      ; 2 for arguments (closure and args-list)
      ; M for locals
      ; so 1 + 1 + 2 + M = frame size.
      (define cur-fp (gensym 'prologue_cur_fp))
      `((label ,xname)
        ((% ,cur-fp) = load (** SinObj) (*** SinObj) (@ fpr))
        (comment "Create stack slots")
        ((% ,(hash-ref local-mapping xclo)) = getelementptr inbounds
                                            (* SinObj) (** SinObj)
                                            (% ,cur-fp) (i64 ,(arg-i 0)))
        ((% ,(hash-ref local-mapping xarglist)) = getelementptr inbounds
                                                (* SinObj) (** SinObj)
                                                (% ,cur-fp) (i64 ,(arg-i 1)))
        ,@(foldl (λ (n acc)
                   (cons `((% ,(symbol-append xname '_local n '_loc))
                           = getelementptr inbounds
                           (* SinObj) (** SinObj) (% ,cur-fp)
                           (i64 ,(local-i n)))
                         acc))
                 '() (range (- nslots (local-i 0))))
        (comment "End function Prologue")))
    (define (layout-body)
      ; MUTABLE CELL
      ; TODO: would be nice to get rid of this mutability.
      (define cur-return 0)
      (define (++!)
        (define cur cur-return)
        (set! cur-return (add1 cur-return))
        cur)
      (define (layout-i i)
        (match i
          [`(label ,x)
           `((label ,x))]
          [`(jump (label ,x))
           `((br (label (% ,x))))]
          [`(bind ,x (phi (,xtval (label ,xtlabel)) (,xfval (label ,xflabel))))
           ; TODO: Im not sure xtval and xfval are actually used in the program.
           (raise 'this-is-broken-i-think-look-above.)
           (define base (symbol-append 'phi_ x))
           ; TODO: xtval and xfval should be given the same stack slot?
           (define ret (cons (symbol-append base 'reg) (hash-ref local-mapping x)))
           `((comment "Starting phi")
             ((% ,(car ret)) = phi (* SinObj)
                             [(% ,xtval) (label (% ,xtlabel))] [(% ,xfval) (label (% ,xflabel))])
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(if ,xc (label ,xt) (label ,xf))
           (define base (gensym 'if))
           (define reg (cons (symbol-append base '_reg) (hash-ref local-mapping xc)))
           (define istrue (symbol-append base 'istrue))
           ; put xc in a register
           ; check truthiness of xc
           ; branch on truthiness.
           ; XXX: I bet we can use 1 less local stack slot by combining the slots of the
           ;      true and false branch values here. Only 1 is taken, why do we need 2 slots.
           `((comment "Starting if")
             ((% ,(car reg)) = load (* SinObj) (** SinObj) (% ,(cdr reg)))
             ((% ,istrue) = call i64 (@ is_truthy_value) ([(* SinObj) (% ,(car reg))]))
             (br i1 (% ,istrue) (label (% ,xt)) (label (% ,xf))))]
          [`(bind ,x (prim ,op . ,xargs))
           (define base (gensym (symbol-append 'primtmp_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define args (map (λ (a i) (cons (symbol-append base '_arg_ i)
                                          (hash-ref local-mapping a)))
                             xargs (range (length xargs))))
           `((comment "Starting prim")
             ,@(map (λ (a) `((% ,(car a)) = load (* SinObj) (** SinObj) (% ,(cdr a)))) args)
             ((% ,(car ret)) = call (* SinObj) (@ ,(prim-name op))
                             (,@(map (λ (a) `[(* SinObj) (% ,(car a))]) args)))
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(bind ,x (apply-prim ,op ,xarglist))
           (define base (gensym (symbol-append 'applyprimtmp_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define arg (cons (symbol-append base '_arg) (hash-ref local-mapping xarglist)))
           `((comment "starting apply-prim")
             ((% ,(car arg)) = load (* SinObj) (** SinObj) (% ,(cdr arg)))
             ((% ,(car ret)) = call (* SinObj) (@ ,(applyprim-name op))
                             ([(* SinObj) (% ,(car arg))]))
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(bind ,x (make-closure ,xname . ,xfrees))
           ; XXX: the `insertvalue` LLVM IR instruction may be useful.
           (define base (gensym (symbol-append 'makeclo_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define args (map (λ (a) (cons (symbol-append base '_arg) (hash-ref local-mapping a)))
                             xfrees))
           (define funcaddr (symbol-append base '_funcaddr))
           `((comment "Starting make-closure")
             ; load all free variables from stack
             ,@(map (λ (a) `((% ,(car a)) = load (* SinObj) (** SinObj) (% ,(cdr a)))) args)
             ; Get the address of the function we are calling
             ((% ,funcaddr) = (* i8) blockaddress (@ main) (% ,xname))
             ; allocate the closure
             ((% ,(car ret)) = call (* SinObj) (@ closure_alloc)
                             ([i64 ,(length xfrees)] [(* i8) (% ,funcaddr)]))
             ,@(map (λ (a i) `(call (@ closure_place_freevar) ([(* SinObj) ,(car ret)]
                                                               [(* SinObj) ,(cdr a)]
                                                               [i64 ,i])))
                    args (range (length args)))
             ; place fully formed closure on stack.
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(bind ,x (env-ref ,_ ,pos))
           ; The 1st argument to env-ref isn't needed here, because we can just refer
           ; to @fpr[1]. We dont actually place the argument as a name anywhere.
           ; woohoo to flat closures.
           (define base (gensym (symbol-append 'envref_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define clo (symbol-append base '_clo))
           (define clo-loc (symbol-append base '_cloloc))
           (define cur-fp (symbol-append base '_cur_fp))
           `((comment "Starting env-ref")
             ((% ,cur-fp) = load (** SinObj) (*** SinObj) (@ fpr))
             ((% ,clo-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                           (% ,cur-fp) (i64 ,(arg-i 0)))
             ((% ,clo) = load (* SinObj) (** SinObj) (% ,cur-fp))
             ((% ,(car ret)) = call (* SinObj) (@ closure_env_get) ([(* SinObj) (% ,clo)] [i64 ,pos]))
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(bind ,x ',(? boolean? b))
           (define base (gensym (symbol-append 'datbool_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define which (if b 'true 'false))
           `((comment "Starting const-bool")
             ((% ,(car ret)) = call (* SinObj) (@ ,(symbol-append 'const_init_ which)) ())
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(bind ,x '())
           (define base (gensym (symbol-append 'datnull_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           `((comment "Starting const-null")
             ((% ,(car ret)) = call (* SinObj) (@ const_init_null) ())
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(bind ,x ',(? integer? n))
           (define base (gensym (symbol-append 'datint_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           `((comment "Starting const-int")
             ((% ,(car ret)) = call (* SinObj) (@ const_init_int) ([i64 ,n]))
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(bind ,x ',(or (? symbol? s) (? string? s)))
           ; TODO: what do globals.
           (define type (cond [(symbol? s) 'sym] [(string? s) 'str]))
           (define type-call (cond [(symbol? s) "symbol"] [(string? s) "string"]))
           (define as-string (cond [(symbol? s) (symbol->string s)] [(string? s) s]))
           (match-define (cons global-name size) (hash-ref globals (cons type as-string)))
           (define base (gensym (symbol-append 'dat type '_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define raw (symbol-append base '_rawglobal))
           `((comment "Starting const-global")
             ((% ,raw) = getelementptr inbounds (arr ,size (* i8)) (* (arr ,size (* i8)))
                       (@ ,global-name) (i64 0) (i64 0))
             ((% ,(car ret)) = call (* SinObj) (@ ,(symbol-append 'const_init_ type-call))
                             ([(* i8) (% ,raw)]))
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(bind ,x (clo-app ,xf ,xarglist))
           (define base (gensym (symbol-append 'bind_app_ x)))
           (define cur-fp (symbol-append base '_cur_fp))
           (define clo (cons (symbol-append base '_closure) (hash-ref local-mapping xf)))
           (define arglist (cons (symbol-append base '_arglist)
                                 (hash-ref local-mapping xarglist)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define funcptr (symbol-append base '_funcptr))
           (define newfp (symbol-append base '_newfp))
           (define newsizeloc (symbol-append base '_newsizeloc))
           (define newarg0loc (symbol-append base '_newarg0loc))
           (define newarg1loc (symbol-append base '_newarg1loc))
           (define return-loc (symbol-append base '_return_loc))
           (define ret-label (symbol-append xname '_ret (++!)))
           (define slots-cast (symbol-append base '_slotscast))
           (define oldfp (symbol-append base '_oldfp))
           (define slots-loc (symbol-append base '_slots_loc))
           (define slots-ptr (symbol-append base '_slots_ptr))
           (define slots (symbol-append base '_slots))
           (define offset (symbol-append base '_offset))
           (define new-fp (symbol-append base '_new_fp))
           (define has-space (symbol-append base '_has_space))
           (define do-call (symbol-append base '_do_call))
           (define overflow-first (symbol-append base '_overflow_first))
           (define overflow-underflow-addr (symbol-append base '_overflow_underflow_addr))
           (define after-new-fp (symbol-append base '_after_new_fp))
           `((comment "Starting direct clo-app")
             ((% ,has-space) = call i1 (@ check_for_overflow)
                             ([(*** SinObj) (@ fpr)]
                              [(*** SinObj) (@ spr)]
                              [i64 ,nslots]))
             (br i1 (% ,has-space) (label (% ,do-call)) (label (% ,overflow-first)))
             (label ,overflow-first)
             ((% ,overflow-underflow-addr) = (* i8) blockaddress (@ main) (% underflow_handler))
             ; if not, call overflow handler
             (call void (@ handle_overflow)
                   ([(** SinRecord) (@ srr)]
                    [(*** SinObj) (@ fpr)]
                    [(*** SinObj) (@ spr)]
                    [(* i8) (% ,overflow-underflow-addr)]
                    [i64 ,nslots]))
             (br (label (% ,do-call)))
             (label ,do-call)
             ((% ,(car clo)) = load (* SinObj) (** SinObj) (% ,(cdr clo)))
             ((% ,(car arglist)) = load (* SinObj) (** SinObj) (% ,(cdr arglist)))
             ((% ,cur-fp) = load (** SinObj) (*** SinObj) (@ fpr))
             ; increase @fpr
             ((% ,new-fp) = getelementptr inbounds (* SinObj) (** SinObj) (% ,cur-fp) (i64 ,nslots))
             (store (** SinObj) (% ,new-fp) (*** SinObj) (@ fpr))
             ; lay out RA, size, and arguments.
             ; we can use the new fp as the new RA loc.
             ((% ,return-loc) = (* SinObj) blockaddress (@ main) (% ,ret-label))
             ((% ,newsizeloc) = getelementptr inbounds (* SinObj) (** SinObj)
                              (% ,new-fp) (i64 ,size-pos))
             ((% ,newarg0loc) = getelementptr inbounds (* SinObj) (** SinObj)
                              (% ,new-fp) (i64 ,(arg-i 0)))
             ((% ,newarg1loc) = getelementptr inbounds (* SinObj) (** SinObj)
                              (% ,new-fp) (i64 ,(arg-i 1)))
             ((% ,slots-cast) = inttoptr i64 ,nslots (* SinObj))
             (store (* SinObj) (% ,return-loc) (** SinObj) (% ,new-fp))
             (store (* SinObj) (% ,slots-cast) (** SinObj) (% ,newsizeloc))
             (store (* SinObj) (% ,(car clo)) (** SinObj) (% ,newarg0loc))
             (store (* SinObj) (% ,(car arglist)) (** SinObj) (% ,newarg1loc))
             ; Get the function pointer and branch to it
             ((% ,funcptr) = call (* i8) (@ closure_get_fn_part) ([(* SinObj) (% ,(car clo))]))
             (indirectbr (* i8) (% ,funcptr) ,(map (λ (l) `(label (% ,l))) call-points))
             ; Continue function processing, put @fpr back.
             (label ,ret-label)
             ((% ,oldfp) = load (** SinObj) (*** SinObj) (@ fpr))
             ((% ,slots-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                             (% ,oldfp) (i64 ,size-pos))
             ((% ,slots-ptr) = load (* SinObj) (** SinObj) (% ,slots-loc))
             ((% ,slots) = ptrtoint (* SinObj) (% ,slots-ptr) i64)
             ((% ,offset) = mul i64 (% ,slots) -1)
             ((% ,after-new-fp) = getelementptr inbounds (* SinObj) (** SinObj) (% ,oldfp) (i64 (% ,offset)))
             (store (** SinObj) (% ,after-new-fp) (*** SinObj) (@ fpr))
             ((% ,(car ret)) = load (* SinObj) (** SinObj) (@ retr))
             ; store @retr value into (hash-ref local-mapping x)
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(bind ,x (call/cc ,xf))
           (define base (gensym 'tailccc))
           (define return-value (cons (symbol-append base '_return_value)
                                      (hash-ref local-mapping x)))
           (define clo (cons (symbol-append base '_callee_clo) (hash-ref local-mapping xf)))
           (define has-space (symbol-append base '_has_space))
           (define do-call (symbol-append base '_do_call))
           (define overflow-first (symbol-append base '_overflow_first))
           (define old-record (symbol-append base '_old_record))
           (define at-base (symbol-append base '_at_base))
           (define dont-split (symbol-append base '_dont_split))
           (define do-split (symbol-append base '_do_split))
           (define old-record-next-loc (symbol-append base '_old_record_next_loc))
           (define dont-split-record (symbol-append base '_dont_split_record))
           (define finished-stack-splitting (symbol-append base '_finished_stack_splitting))
           (define old-record-loc (symbol-append base '_old_record_loc))
           (define old-record-stack (symbol-append base '_old_record_stack))
           (define caller-fp (symbol-append base '_caller_fp))
           (define callee-fp (symbol-append base '_callee_fp))
           (define caller-fp-int (symbol-append base '_caller_fp_int))
           (define callee-fp-int (symbol-append base '_callee_fp_int))
           (define old-record-stack-int (symbol-append base '_old_record_stack_int))
           (define cutoff-offset-bytes (symbol-append base '_cutoff_offset_bytes))
           (define cutoff-offset-slots (symbol-append base '_cutoff_offset_slots))
           (define return-fp-offset-bytes (symbol-append base '_return_fp_offset_bytes))
           (define return-fp-offset-slots (symbol-append base '_return_fp_offset_slots))
           (define return-address (symbol-append base '_return_address))
           (define new-top-segment (symbol-append base '_new_top_segment))
           (define stack-record-to-use (symbol-append base '_stack_record_to_use))
           (define continuation-function-handler '_continuation_function_handler)
           (define cont-clo (symbol-append base '_cont_clo))
           (define size-loc (symbol-append base '_size_loc))
           (define callee-clo-arg-loc (symbol-append base '_callee_clo_arg_loc))
           (define arglist-arg-loc (symbol-append base '_arglist_arg_loc))
           (define arglist-null (symbol-append base '_arglist_null))
           (define arglist (symbol-append base '_arglist))
           (define underflow-addr-void (symbol-append base '_underflow_addr_void))
           (define underflow-addr (symbol-append base '_underflow_addr))
           (define underflow-size-as-sinobj (symbol-append base '_underflow_size_as_sinobj))
           (define underflow-size-as-ptr (symbol-append base '_underflow_size_as_ptr))
           (define clo-fn-part (symbol-append base '_clo_fn_part))
           (define ret-label (symbol-append xname '_ret (++!)))
           (define overflow-underflow-addr (symbol-append base '_overflow_underflow_addr))
           ; Check if we have enough
           `((comment "Starting direct call/cc")
             ((% ,has-space) = call i1 (@ check_for_overflow)
                             ([(** SinRecord) (@ srr)]
                              [(*** SinObj) (@ fpr)]
                              [(*** SinObj) (@ spr)]
                              [i64 ,nslots]))
             (br i1 (% ,has-space) (label (% ,do-call)) (label (% ,overflow-first)))
             (label ,overflow-first)
             ; if not, call overflow handler
             ((% ,overflow-underflow-addr) = (* i8) blockaddress (@ main) (% underflow_handler))
             (call void (@ handle_overflow)
                   ([(** SinRecord) (@ srr)]
                    [(*** SinObj) (@ fpr)]
                    [(*** SinObj) (@ spr)]
                    [(* i8) (% ,overflow-underflow-addr)]
                    [i64 ,nslots]))
             (br (label (% ,do-call)))
             (label ,do-call)
             ((% ,old-record) = load (* SinRecord) (** SinRecord) (@ srr))
             ; Check if we are at the base,
             ((% ,at-base) = call i1 (@ callcc_at_base)
                           ([(** SinRecord) (@ srr)]
                            [(*** SinObj) (@ fpr)]))
             (br i1 (% ,at-base) (label (% ,dont-split)) (label (% ,do-split)))
             (label ,dont-split)
             ; Get the location of the stack records link field
             ; TODO: dont use magic-number 1 for the link field, make a constant variable!!
             ((% ,old-record-next-loc) = getelementptr inbounds SinRecord (* SinRecord)
                                       (% ,old-record) (i64 0) (i32 1))
             ; This is the continuation object if we do not split the stack.
             ((% ,dont-split-record) = load (* SinRecord) (** SinRecord) (% ,old-record-loc))
             (br (label (% ,finished-stack-splitting)))
             (label ,do-split)
             ; get the stack part of the record
             ; TODO: don't use magic-number 0 for the stack field, make a constant variable!!
             ((% ,old-record-loc) = getelementptr inbounds SinRecord (* SinRecord)
                                  (% ,old-record) (i32 0) (i32 0))
             ((% ,old-record-stack) = load (** SinObj) (*** SinObj) (% ,old-record-loc))
             ; get the frame-pointer
             ; increment it to get the fp of the callee (therefore, the fp of the new segment)
             ((% ,caller-fp) = load (** SinObj) (*** SinObj) (@ fpr))
             ((% ,callee-fp) = getelementptr inbounds (* SinObj) (** SinObj)
                             (% ,caller-fp) (i64 ,nslots))
             ; Get the number of slots to cutoff.
             ((% ,callee-fp-int) = ptrtoint (** SinObj) (% ,callee-fp) i64)
             ((% ,caller-fp-int) = ptrtoint (** SinObj) (% ,caller-fp) i64)
             ((% ,old-record-stack-int) = ptrtoint (** SinObj) (% ,old-record-stack) i64)
             ((% ,cutoff-offset-bytes) = sub nuw nsw i64 (% ,callee-fp-int) (% ,old-record-stack-int))
             ((% ,cutoff-offset-slots = lshr i64 (% ,cutoff-offset-bytes) 3))
             ; Get the number of slots to the previous frame start from base of old record.
             ((% ,return-fp-offset-bytes) = sub nuw nsw i64 (% ,caller-fp-int) (% old-record-stack-int))
             ((% ,return-fp-offset-slots) = lshr i64 (% ,return-fp-offset-bytes) 3)
             ; Get the return address so we can put it into the old-record's return_address field.
             ((% ,return-address) = (* i8) blockaddress (@ main) (% ,ret-label))
             ; Split the records!
             ((% ,new-top-segment) = call (* SinRecord) (@ split_record)
                                   ([(* SinRecord) (% ,old-record)]
                                    [(** SinObj) (% ,callee-fp)]
                                    [i64 (% ,cutoff-offset-slots)]
                                    [i64 (% ,return-fp-offset-slots)]
                                    [i8* (% ,return-address)]))
             ; place the new top segment in @srr
             (store volatile (* SinRecord) (% ,new-top-segment) (** SinRecord) (@ srr))
             (br (label (% ,finished-stack-splitting)))
             ;;; Stack splitting done!
             (label ,finished-stack-splitting)
             ((% ,stack-record-to-use) = phi (* SinRecord) ([(% ,old-record) (label ,do-split)]
                                                            [(% ,dont-split-record) (label ,dont-split)]))
             ; Create the continuation object
             ((% ,continuation-function-handler) = (* i8) blockaddress (@ main) (% continuation_function_handler))
             ((% ,cont-clo) = call (* SinObj) (@ make_continuation_closure)
                            ([(* SinRecord) (% ,stack-record-to-use)]
                             [(* i8) (% ,continuation-function-handler)]))
             ; Get argument locations
             ((% ,size-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                            ,callee-fp (i64 ,size-pos))
             ((% ,callee-clo-arg-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                                      ,callee-fp (i64 ,(arg-i 0)))
             ((% ,arglist-arg-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                                   ,callee-fp (i64 ,(arg-i 1)))
             ; Get closure we are calling (arg0 and the callee)
             ((% ,(car clo)) = load (* SinObj) (** SinObj) (% ,(cdr clo)))
             ; Create args
             ((% ,arglist-null) = call (* SinObj) (@ const_init_null) ())
             ((% ,arglist) = call (* SinObj) (@ prim_cons) ([(* SinObj) (% ,cont-clo)]
                                                            [(* SinObj) (% ,arglist-null)]))
             ;;;; place things on stack
             ; Use the underflow address in place of the return address
             ((% ,underflow-addr-void) = (* i8) blockaddress (@ main) (% underflow_handler))
             ((% ,underflow-addr) = bitcast (i8*) (% ,underflow-addr-void) to (* SinObj))
             ; Size here at underflow point is -1, this is helpful during overflow handling.
             ((% ,underflow-size-as-sinobj) = inttoptr i64 -1 to (* SinObj))
             ; Place arguments onto stack
             ; callee-fp[0] is return-address (here, underflow), then size, then args.
             (store (* SinObj) (% ,underflow-addr) (** SinObj) (% ,callee-fp))
             (store (* SinObj) (% ,underflow-size-as-ptr) (** SinObj) (% ,size-loc))
             (store (* SinObj) (% ,(car clo)) (** SinObj) (% ,callee-clo-arg-loc))
             (store (* SinObj) (% ,arglist) (** SinObj) (% ,arglist-arg-loc))
             ;;;;; Call the damn function.
             ((% ,clo-fn-part) = call (* i8) (@ closure_get_fn_part) ([(* SinObj) (% ,(car clo))]))
             (indirectbr (* i8) (% ,clo-fn-part) ,(map (λ (l) `(label (% ,l))) call-points))
             ; Make the return point, and clean up.
             (label ,ret-label)
             ((% ,(car return-value)) = load (* SinObj) (** SinObj) (@ retr))
             (store (* SinObj) (% ,(car return-value)) (** SinObj) (% ,(cdr return-value))))]
          [`(bind ,x0 ,x1)
           ;; Load x1 from stack
           ;; Store into x1 via a SinObj* -> SinObj* bitcast.
           (define base (gensym 'bind_alias))
           (define alias (hash-ref local-mapping x0))
           (define aliased (cons (symbol-append base '_aliased)
                                 (hash-ref local-mapping x1)))
           `((comment "Starting alias")
             ((% ,(car aliased)) = load (* SinObj) (** SinObj) (% ,(cdr aliased)))
             (store (* SinObj) (% ,(car aliased)) (** SinObj) (% ,alias)))]

          [`(return (call/cc ,xf))
           (define base (gensym 'tailccc))
           (define has-space (symbol-append base '_has_space))
           (define do-call (symbol-append base '_do_call))
           (define overflow-first (symbol-append base '_overflow_first))
           (define at-base (symbol-append base '_at_base))
           (define dont-split (symbol-append base '_dont_split))
           (define do-split (symbol-append base '_do_split))
           (define old-record-next-loc (symbol-append base '_old_stack_record_next_loc))
           (define dont-split-record (symbol-append base '_dont_split_record))
           (define finished-stack-stuff (symbol-append base '_finished_stack_stuff))
           (define old-record (symbol-append base '_old_stack_record))
           (define old-record-base-loc (symbol-append base '_old_stack_record_base_loc))
           (define old-record-base (symbol-append base '_old_stack_record_base))
           (define cur-fp (symbol-append base '_cur_fp))
           (define cur-fp-int (symbol-append base '_cur_fp_int))
           (define old-record-base-int (symbol-append base '_old_stack_record_base_int))
           (define cutoff-offset-bytes (symbol-append base '_cutoff_offset_bytes))
           (define cuttoff-offset-slots (symbol-append base '_cutoff_offset_slots))
           (define slots-in-caller-loc (symbol-append base '_slots_in_caller_loc))
           (define slots-in-caller-ptr (symbol-append base '_slots_in_caller_ptr))
           (define slots-in-caller (symbol-append base '_slots_in_caller))
           (define negated-slots-in-caller (symbol-append base '_negated_slots_in_caller))
           (define caller-fp-loc (symbol-append base '_caller_fp_loc))
           (define caller-fp-loc-int (symbol-append base '_caller_fp_loc_int))
           (define caller-fp-offset-bytes (symbol-append base '_caller_fp_offset_bytes))
           (define caller-fp-offset-slots (symbol-append base '_caller_fp_offset_slots))
           (define ret-addr-sinobjptr (symbol-append base '_ret_addr_sinobjptr))
           (define ret-addr (symbol-append base '_ret_addr))
           (define new-top-record (symbol-append base '_new_top_record))
           (define cutoff-offset-slots (symbol-append base '_cutoff_offset_slots))
           (define cont-clo (symbol-append base '_cont_clo))
           (define size-loc (symbol-append base '_size_loc))
           (define clo-arg-loc (symbol-append base '_clo_arg_loc))
           (define arglist-arg-loc (symbol-append base '_arglist_arg_loc))
           (define arglist-null (symbol-append base '_arglist_null))
           (define arglist (symbol-append base '_arglist))
           (define clo (cons (symbol-append base '_clo_arg_local) (hash-ref local-mapping xf)))
           (define underflow-addr-raw (symbol-append base '_underflow_addr_raw))
           (define underflow-addr (symbol-append base '_underflow_addr))
           (define underflow-size-as-ptr (symbol-append base '_underflow_size_as_ptr))
           (define clo-fn-part (symbol-append base '_clo_fn_part))
           (define record-to-use (symbol-append base '_stack_record_to_use))
           (define continuation-function-handler
             (symbol-append base '_continuation_function_handler))
           `((comment "Starting tail-call/cc")
             ((% ,old-record) = load (* SinRecord) (** SinRecord) (@ srr))
             ; Check if we are at the base,
             ((% ,at-base) = call i1 (@ callcc_at_base) ([(** SinRecord) (@ srr)]
                                                         [(*** SinObj) (@ fpr)]))
             (br i1 (% ,at-base) (label (% ,dont-split)) (label (% ,do-split)))
             (label ,dont-split)
             ; Get the location of the stack records link field
             ((% ,old-record-next-loc) = getelementptr inbounds SinRecord (* SinRecord)
                                       (% ,old-record) (i64 0) (i32 1))
             ; get the value in that location (the pointer to the link)
             ((% ,dont-split-record) = load (* SinRecord) (** SinRecord) (% ,old-record-next-loc))
             (br (label (% ,finished-stack-stuff)))
             (label ,do-split)
             ; get the stack part of the record
             ((% ,old-record-base-loc) = getelementptr inbounds SinRecord (* SinRecord)
                                       (% ,old-record) (i32 0) (i32 0))
             ((% ,old-record-base) = load (** SinObj) (*** SinObj) (% ,old-record-base-loc))
             ; get the frame-pointer, which will also be the base of the new segment.
             ((% ,cur-fp) = load (** SinObj) (*** SinObj) (@ fpr))
             ; Get the number of slots to cutoff.
             ((% ,cur-fp-int) = ptrtoint (** SinObj) (% ,cur-fp) to i64)
             ((% ,old-record-base-int) = ptrtoint (** SinObj) (% ,old-record-base) i64)
             ((% ,cutoff-offset-bytes) = sub i64 (% ,cur-fp-int) (% ,old-record-base-int))
             ((% ,cutoff-offset-slots) = lshr i64 (% ,cutoff-offset-bytes) 3)
             ; Get the number of slots to the previous frame start from base of old record.
             ((% ,slots-in-caller-loc) = getelementptr inbounds (* SinObj) (** SinObj) (% ,cur-fp)
                                       (i64 ,size-pos))
             ((% ,slots-in-caller-ptr) = load (* SinObj) (** SinObj) (% ,slots-in-caller-loc))
             ((% ,slots-in-caller) = ptrtoint (* SinObj) (% ,slots-in-caller-ptr) i64)
             ((% ,negated-slots-in-caller) = mul i64 (% ,slots-in-caller) -1)
             ((% ,caller-fp-loc) = getelementptr inbounds (* SinObj) (** SinObj) (% ,cur-fp)
                                 (i64 (% ,negated-slots-in-caller)))
             ((% ,caller-fp-loc-int) = ptrtoint (** SinObj) (% ,caller-fp-loc) i64)
             ((% ,caller-fp-offset-bytes) = sub i64 (% ,caller-fp-loc-int) (% ,old-record-base-int))
             ((% ,caller-fp-offset-slots) = lshr i64 (% ,caller-fp-offset-bytes) 3)
             ; Get the return address in the code stream for the old record.
             ((% ,ret-addr-sinobjptr) = load (* SinObj) (** SinObj) (% cur-fp))
             ((% ,ret-addr) = bitcast (* SinObj) (% ,ret-addr-sinobjptr) (* i8))
             ; Split the records!
             ((% ,new-top-record) = call (* SinRecord) (@ split_record)
                                  ([(* SinRecord) (% ,old-record)]
                                   [(** SinObj) (% ,cur-fp)]
                                   [i64 (% ,cutoff-offset-slots)]
                                   [i64 (% ,caller-fp-offset-slots)]
                                   [(* i8) (% ,ret-addr)]))
             ; place the new top segment in @srr
             (store (* SinRecord) (% ,new-top-record) (** SinRecord) (@ srr))
             (br (label (% ,finished-stack-stuff)))
             ;;; Stack splitting done!
             (label ,finished-stack-stuff)
             ((% ,record-to-use) = phi (* SinRecord)
                                 [(% ,old-record) (label (% ,do-split))]
                                 [(% ,dont-split-record) (label (% ,dont-split))])
             ; Create the continuation object
             ((% ,continuation-function-handler) = (* i8) blockaddress (@ main)
                                                 (% continuation_function_handler))
             ((% ,cont-clo) = call (* SinObj) (@ make_continuation_closure)
                            ([(* SinRecord) (% ,record-to-use)]
                             [(* i8) (% ,continuation-function-handler)]))
             ; Get argument locations
             ((% ,size-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                            (% ,cur-fp) (i64 ,size-pos))
             ((% ,clo-arg-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                               (% ,cur-fp) (i64 ,(arg-i 0)))
             ((% ,arglist-arg-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                                   (% ,cur-fp) (i64 ,(arg-i 1)))
             ; Create args
             ((% ,arglist-null) = call (* SinObj) (@ const_init_null) ())
             ((% ,arglist) = call (* SinObj) (@ prim_cons)
                           ([(* SinObj) (% ,cont-clo)] [(* SinObj) (% ,arglist-null)]))
             ; Get closure we are calling
             ((% ,(car clo)) = load (* SinObj) (** SinObj) (% ,(cdr clo)))
             ;;;; place things on stack
             ; place the underflow address in place of the return address
             ((% ,underflow-addr-raw) = (* i8) blockaddress (@ main) (% underflow_handler))
             ((% ,underflow-addr) = bitcast (* i8) (% ,underflow-addr-raw) (* SinObj))
             (store (* SinObj) (% ,underflow-addr) (** SinObj) (% ,cur-fp))
             ; Size here at underflow point is -1, this is helpful during overflow handling.
             ((% ,underflow-size-as-ptr) = inttoptr i64 -1 to (* SinObj))
             ; Store arguments
             (store (* SinObj) (% ,underflow-size-as-ptr) (** SinObj) (% ,size-loc))
             (store (* SinObj) (% ,(car clo)) (** SinObj) (% ,clo-arg-loc))
             (store (* SinObj) (% ,arglist) (** SinObj) (% ,arglist-arg-loc))
             ;;;;; Call the damn function.
             ((% ,clo-fn-part) = call (* i8) (@ closure_get_fn_part) ([(* SinObj) (% ,(car clo))]))
             (indirectbr (* i8) (% ,clo-fn-part) ,(map (λ (l) `(label (% ,l))) call-points)))]
          [`(return (clo-app ,xf ,xargs))
           ; we can do Tail-Call-Elimination!!!!
           ; BUT its not safe unless we ensure that there's enough stack space
           ; cause if we reuse the stack frame and the callee needs too much space...
           (define base (gensym (symbol-append '_tailapp_ xf)))
           (define clo (cons (symbol-append base '_clo) (hash-ref local-mapping xf)))
           (define args (cons (symbol-append base '_args) (hash-ref local-mapping xargs)))
           (define arg0loc (symbol-append base '_arg0))
           (define arg1loc (symbol-append base '_arg1))
           (define funcptr (symbol-append base '_funcptr))
           (define fp (symbol-append base '_fp))
           `((comment "Starting tail-clo-app")
             ; put arguments in registers
             ((% ,(car clo)) = load (* SinObj) (** SinObj) (% ,(cdr clo)))
             ((% ,(car args)) = load (* SinObj) (** SinObj) (% ,(cdr args)))
             ((% ,fp) = load (** SinObj) (*** SinObj) (@ fpr))
             ; get arg locations
             ((% ,arg0loc) = getelementptr inbounds (* SinObj) (** SinObj)
                           (% ,fp) (i64 ,(arg-i 0)))
             ((% ,arg1loc) = getelementptr inbounds (* SinObj) (** SinObj)
                           (% ,fp) (i64 ,(arg-i 1)))
             ; place arguments on the stack (xf and xarglist)
             (store (* SinObj) (% ,(car clo)) (** SinObj) (% ,arg0loc))
             (store (* SinObj) (% ,(car args)) (** SinObj) (% ,arg1loc))
             ; get function pointer, branch to it!!
             ((% ,funcptr) = call (* i8) (@ closure_get_fn_part) ([(* SinObj) (% ,(car clo))]))
             (indirectbr (* i8) (% ,funcptr) ,(map (λ (l) `(label (% ,l))) call-points)))]
          [`(return ,x)
           (define base (gensym (symbol-append 'retval_ x)))
           (define val (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define ra (symbol-append base '_ra))
           (define fp (symbol-append base '_fp))
           (define ra-raw (symbol-append base '_ra_raw))
           `((comment "Starting return value.")
             ; get value of x from stack
             ((% ,(car val)) = load (* SinObj) (** SinObj) (% ,(cdr val)))
             ; store it into @retr
             (store (* SinObj) (% ,(car val)) (** SinObj) (@ retr))
             ; load RA into a register from stack
             ((% ,fp) = load (** SinObj) (*** SinObj) (@ fpr))
             ((% ,ra) = load (* SinObj) (** SinObj) (% ,fp))
             ((% ,ra-raw) = bitcast (* SinObj) (% ,ra) (* i8))
             ; indirectbr to RA
             (indirectbr (* i8) (% ,ra-raw) ,(map (λ (l) `(label (% ,l))) return-points)))]))
      (foldl (λ (i acc)
               #;(pretty-display `(i: ,i))
               (append acc (layout-i i))) '() instructions))

    (define prologue (map llvmir-ir->llvm-ir-string (layout-prologue)))
    ;(pretty-display (layout-body))
    (define body (map llvmir-ir->llvm-ir-string (layout-body)))
    `(,@prologue ,@body ,(format "  ; Function ~a finished" xname)))
  (match-define `(,procs ...) lir)
  `(,@start-of-program
    ,@(append* (map layout-proc procs))
    ,@end-of-program))

(define (llvm-convert lir)
  (define norm-lir (normalize-names lir))
  (match-define globals (compute-globals norm-lir))
  #;(pretty-display (layout-program norm-lir globals))
  (string-join
   `("; Begin user globals"
     ,@(layout-globals globals)
     "; End user "
     #;,@helper-functions
     ,@(layout-program norm-lir globals))
   "\n"))


(define prog
  '((proc
     (main mainenv4964814 mainargs4964815)
     (let ((anf-bind4962025 (make-closure clo4964813)))
       (let ((anf-datum4962026 '5))
         (let ((args4964807$anf-bind4962025$0 '()))
           (let ((args4964807$anf-bind4962025$1
                  (prim cons anf-datum4962026 args4964807$anf-bind4962025$0)))
             (let ((g (clo-app anf-bind4962025 args4964807$anf-bind4962025$1)))
               (let ((anf-bind4962028 (make-closure clo4964812)))
                 (let ((anf-datum4962029 '7))
                   (let ((args4964809$anf-bind4962028$0 '()))
                     (let ((args4964809$anf-bind4962028$1
                            (prim cons anf-datum4962029 args4964809$anf-bind4962028$0)))
                       (let ((h (clo-app anf-bind4962028 args4964809$anf-bind4962028$1)))
                         (let ((admin-toplevel-bnd4964811 (prim + g h)))
                           admin-toplevel-bnd4964811))))))))))))
    (proc (clo4964813 clo4964813_cloarg arglist49648060) (let ((y (prim car arglist49648060))) y))
    (proc
     (clo4964812 clo4964812_cloarg arglist49648080)
     (let ((z (prim car arglist49648080)))
       (let ((anf-datum4962027 '3))
         (let ((admin-toplevel-bnd4964810 (prim + z anf-datum4962027))) admin-toplevel-bnd4964810))))))

(define lir (lir-convert prog))
