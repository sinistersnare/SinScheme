#lang racket

(require (only-in "utils.rkt" c-name prim-name applyprim-name
                  encoded-str-length c-hex-encode symbol-append))


; Input Language (output of `lir-convert`):

; p ::= ((proc (x nat x x) e) ...)
; e ::= (i ... (return r))
; i ::= (assign x l)
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


; The Scheme program is compiled into one giant LLVM function.
; Calls are handled with `indirectbr`, and the ABI is poorly documented.
; Hopefully its described in src/cpp/README.md or elsewhere.
;
; First, we need a pass converting local names into numbers
; (much like how clang does it).

(define (numerize-locals proc)
  ; Takes a single proc and returns a hash
  ; mapping the locals in that proc to numbers.
  (match-define `(proc (,xname ,_ ...) ,body) proc)
  (define counter 0)
  (define (++!) (define val counter) (set! counter (add1 counter)) val)
  (foldl (λ (instr acc)
           (match instr
             [`(assign ,x ,_)
              (define num (++!))
              (define stackname (format "~a_local~a_loc" xname num))
              (hash-set acc x stackname)]))
         (hash) body))

(define (normalize-names lir)
  ;; converts all variable names from a 'lisp style' (i.e. kebab case with any symbol)
  ;; into a c-style (only character class \w allowed basically)
  (define (normalize x) (string->symbol (c-name x)))
  (define (norm-proc proc)
    (match-define `(proc (,name ,nslots ,env ,args) ,e) proc)
    `(proc (,(normalize name) ,nslots ,(normalize env) ,(normalize args)) ,(map norm-i e)))
  (define (norm-i i)
    (match i
      [`(assign ,x ,l)
       `(assign ,(normalize x) ,(norm-l l))]
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
  ; TODO: we can take this list and do (sort xs string<?) to order it!
  (foldl (λ (k acc)
           (match-define (cons val _) k)
           (match-define (cons name size) (hash-ref globals k))
           (cons (format "@~a = private unnamed_addr constant [~a x i8] c\"~a\""
                         name size val)
                 acc))
         '() (hash-keys globals)))

(define ra-pos 0)
(define size-pos 1)
(define arg0-pos 2)
(define arg1-pos 3)
(define args-start-pos 4)

(define (layout-program lir globals)
  (define start-of-program
    `("define i32 main {"
      "  call void @morestack()"
      "  call void start_runtime()"))
  (define end-of-program
    `("program_finished:"
      "  ret i32 0"
      "}"))
  (define (layout-proc proc)
    (define local-mapping (numerize-locals proc))
    (match-define `(proc (,xname ,nslots ,xenv ,xargs) . ,instructions) proc)
    (define (layout-prologue)
      ; Need to add frame size into stack.
      ; frame size is:
      ; 1 for return-address
      ; 1 for frame size
      ; 2 for arguments (closure and args-list)
      ; M for locals
      ; so 1 + 1 + 2 + M = frame size.
      `(,(format "~a:" xname)
        ,(format "  %~a_fp = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8"
                 xname)
        "  ; Create stack slots"
        ,@(foldl (λ (n acc)
                   (cons (format (string-append
                                  "  %~a_local~a_loc = getelementptr inbounds "
                                  "%struct.SinObj*, %struct.SinObj** "
                                  "%~a_fp, i64 ~a")
                                 ; (+ n 3) because there are 3 slots before locals.
                                 xname n xname (+ n args-start-pos))
                         acc))
                 '() (range (- nslots args-start-pos)))
        "  ; End function prologue"))
    (define (layout-body)
      (define (layout-i i)
        (match i
          [`(label ,x)
           `(,(format "~a:" x))]
          [`(jump (label ,x))
           `(,(format "br label ~a" x))]
          [`(assign ,x (phi (,xtval (label ,xtlabel)) (,xfval (label ,xflabel))))
           (define base (symbol-append 'phi_ x))
           (define reg (cons (symbol-append base 'reg) (hash-ref local-mapping x)))
           (define t (cons (symbol-append base 't) (hash-ref local-mapping xtval)))
           (define f (cons (symbol-append base 'f) (hash-ref local-mapping xfval)))
           '(,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a, align 8"
                      (car t) (cdr t))
             ,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a, align 8"
                      (car f) (cdr f))
             ,(format "  %~a = phi %struct.SinObj* [%~a, %~a], [%~a, %~a]"
                      (car reg) (car t) xtlabel (car f) xflabel)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car ret) (cdr ret)))]
          [`(if ,xc (label ,xt) (label ,xf))
           (define base (gensym 'if))
           (define reg (cons (symbol-append base '_reg) (hash-ref local-mapping xc)))
           (define istrue (symbol-append base 'istrue))
           ; put xc in a register
           ; check truthiness of xc
           ; branch on truthiness.
           `(,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a, align 8"
                      (car reg) (cdr reg))
             ,(format "  %~a = call i64 @is_truthy_value(%struct.SinObj* %~a)"
                      istrue (car reg))
             ,(format "  br i1 %~a, label %~a, label %~a"
                      istrue xt xf))]
          ; I bet we can use 1 less local stack slot by combining the slots of the
          ; true and false branch values here. Only 1 is taken, why do we need 2 slots.
          [`(assign ,x (prim ,op . ,xargs))
           (define base (gensym (symbol-append 'primtmp_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define args (map (λ (a) (cons (symbol-append base '_arg)
                                          (hash-ref local-mapping a)))
                                 xargs))
           `(,@(map (λ (a) (format "  %~a = load %struct.SinObj* %~a, align 8"
                                   (car a) (cdr a)))
                    args)
             ,(format "  %~a = call %struct.SinObj* @~a(~a)"
                      (car ret) (prim-name op)
                      (string-join (map (λ (a) (format "%struct.SinObj* %~a" (car a))) args) ", "))
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car ret) (cdr ret)))]
          [`(assign ,x (apply-prim ,op ,xarglist))
           (define base (gensym (symbol-append 'applyprimtmp_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define arg (cons (symbol-append base '_arg) (hash-ref local-mapping xarglist)))
           `(,(format "  %~a = load %struct.SinObj* %~a, align 8"
                      (car arg) (cdr arg))
             ,(format "  %~a = call %struct.SinObj* @~a(%struct.SinObj ~a)"
                      (car ret) (applyprim-name op) (car arg))
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car ret) (cdr ret)))]
          [`(assign ,x (make-closure ,xname . ,xfrees))
           (define base (gensym (symbol-append 'makeclo_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define args (map (λ (a) (cons (symbol-append base '_arg) (hash-ref local-mapping a)))
                             xfrees))
           (define funcaddr (symbol-append base '_funcaddr))
           `(,@(map (λ (a) (format "  %~a = load %struct.SinObj* %~a, align 8"
                                   (car a) (cdr a)))
                    args)
             ; get block address of function we want.
             ,(format "  %~a = i8* blockaddress(@main, %~a)" funcaddr xname)
             ; allocate closure
             ,(format "  %~a = call %struct.SinObj* @closure_alloc(i64 ~a, i8* %~a)"
                      (car ret) (length xfrees) funcaddr)
             ; place free variables
             ,@(map (λ (a i) (format (string-append
                                      "  call @closure_place_freevar"
                                      "(%struct.SinObj* %~a, %struct.SinObj* ~a, i64 ~a)")
                                     (car ret) (cdr a) i))
                    args (range (length args)))
             ; store it onto stack
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car ret) (cdr ret)))]
          [`(assign ,x (env-ref ,_ ,pos))
           ; The 1st argument to env-ref isn't needed here, because we can just refer
           ; to @fpr[1]. We dont actually place the argument as a name anywhere.
           ; woohoo to flat closures.
           (define base (gensym (symbol-append 'envref_ x)))
           (define ret (symbol-append base '_val))
           (define clo (symbol-append base '_clo))
           (define clo-loc (symbol-append base '_cloloc))
           `(,(format (string-append
                       "  %~a = getelementptr inbounds"
                       " %struct.SinObj*, %struct.SinObj** %~a_fp, i64 1")
                      clo-loc xname)
             ,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a"
                      clo clo-loc)
             ,(format "  %~a = call %struct.SinObj* @closure_env_get(%struct.SinObj*, %~a, i64 %~a)"
                      ret clo pos)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      ret (hash-ref local-mapping x)))]
          [`(assign ,x ',(? boolean? b))
           (define base (gensym (symbol-append 'datbool_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           `(,(format "  %~a = call %struct.SinObj @const_init_~a()"
                      (car ret) (if b "true" "false"))
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car ret) (cdr ret)))]
          [`(assign ,x ''())
           (define base (gensym (symbol-append 'datnull_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           `(,(format "  %~a = call %struct.SinObj @const_init_null()" (car ret))
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car ret) (cdr ret)))]
          [`(assign ,x ',(? integer? n))
           (define base (gensym (symbol-append 'datint_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           `(,(format "  %~a = call %struct.SinObj @const_init_int(i64 ~a)" (car ret) n)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car ret) (cdr ret)))]
          [`(assign ,x ',(or (? symbol? s) (? string? s)))
           ; TODO: what is globals.
           (define type (cond [(symbol? s) 'sym] [(string? s) 'str]))
           (define type-call (cond [(symbol? s) "symbol"] [(string? s) "string"]))
           (define as-string (cond [(symbol? s) (symbol->string s)] [(string? s) s]))
           (match-define (cons global-name size) (hash-ref globals (cons type as-string)))
           (define base (gensym (symbol-append 'dat type '_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define raw (symbol-append base '_rawglobal))
           `(,(format "  %~a = getelementptr inbounds [~a x i8], [~a x i8]* @~a, i64 0, i64 0"
                      raw size size global-name)
             ,(format "  %~a = call %struct.SinObj* @const_init_~a(i8* %~a)"
                      (car ret) type-call raw)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car ret) (cdr ret)))]
          [`(assign ,x0 ,x1)
           ; TODO: Does this show up at this point in the compiler?
           (raise 'IMPL-ALIASES!)]
          [`(assign ,x (clo-app ,xf ,xarglist))
           (define base (gensym 'assign_app_ x))
           (define clo (cons (symbol-append base '_closure) (hash-ref local-mapping xf)))
           (define arglist (cons (symbol-append base '_arglist)
                                 (hash-ref local-mapping xarglist)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define funcptr (symbol-append base '_funcptr))
           (define newfp (symbol-append base '_newfp))
           (define newsizeloc (symbol-append base '_newsizeloc))
           (define newarg0loc (symbol-append base '_newarg0loc))
           (define newarg1loc (symbol-append base '_newarg1loc))
           (define returnloc (symbol-append base '_returnloc))
           (define return-label (gensym (symbol-append xname '_ret)))
           (define slots-cast (symbol-append base '_slotscast))
           (define oldfp (symbol-append base '_oldfp))
           (define slots-loc (symbol-append base '_slots_loc))
           (define slots-ptr (symbol-append base '_slots_ptr))
           (define slots (symbol-append base '_slots))
           (define offset (symbol-append base '_offset))
           (define new-fp (symbol-append base '_newfp))
           (define fp (symbol-append base '_fp))
           ; TODO: we need to check for possible stack overflow...somehow.
           `(,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a, align 8"
                      (car clo) (cdr clo))
             ,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a, align 8"
                      (car arglist) (cdr arglist))
             ; Get the function pointer
             ,(format "  %~a = call i8* @closure_get_fn_part(%struct.SinObj* %~a)"
                      funcptr (car clo))
             ; increase @fpr
             ,(format (string-append "  %~a = getelementptr inbounds"
                                     " %struct.SinObj*, %struct.SinObj** %~a_fp, i64 ~a")
                      newfp fp nslots)
             ,(format "  store volatile %struct.SinObj** %~a, %struct.SinObj*** @fpr, align 8"
                      newfp)
             ; lay out RA, size, and arguments.
             ; we can use the new fp as the new RA loc.
             ,(format "  %~a = i8* blockaddress(@main, %~a)" returnloc return-label)
             ,(format (string-append "  %~a = getelementptr inbounds "
                                     "%struct.SinObj*, %struct.SinObj** %~a, i64 ~a")
                      newsizeloc newfp size-pos)
             ,(format (string-append "  %~a = getelementptr inbounds "
                                     "%struct.SinObj*, %struct.SinObj** %~a, i64 ~a")
                      newarg0loc newfp arg0-pos)
             ,(format (string-append "  %~a = getelementptr inbounds "
                                     "%struct.SinObj*, %struct.SinObj** %~a, i64 ~a")
                      newarg1loc newfp arg1-pos)
             ,(format "  %~a = inttoptr i64 ~a to %struct.SinObj*"
                      slots-cast nslots)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      newfp returnloc)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      slots-cast newsizeloc)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car clo) newarg0loc)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car arglist) newarg1loc)
             ; indirectbr to function pointer
             ,(format "  indirectbr i8* %~a, [TODO: All function labels]"
                      funcptr)
             ; lay out return-label and
             ,(format "~a:" return-label)
             ; Load @fpr -> %old_fp
             ,(format " %~a = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8"
                      oldfp)
             ; %slots_loc = GEP %old_fp, i64 1
             ,(format (string-append "  %~a = getelementptr inbounds "
                                     "%struct.SinObj*, %struct.SinObj**, i64 1")
                      slots-loc)
             ; slots_ptr = load %slots_loc
             ,(format " %~a = load %struct.SinObj*, %struct.SinObj** %~a"
                      slots-ptr slots-loc)
             ; slots = ptrtoint slots_ptr to i64
             ,(format "  %~a = ptrtoint 164* %~a to i64"
                      slots slots-ptr)
             ; %offset = mul slots -1
             ,(format "  %~a = mul nsw i64 %~a, -1"
                      offset slots)
             ; %new_fp = GEP %old_fp, i64 %offset
             ,(format (string-append "  %~a = getelementptr inbounds"
                                     "  %struct.SinObj*, %struct.SinObj** %~a,"
                                     " i64 %~a")
                      new-fp oldfp offset)
             ; store %new_fp @fpr
             ,(format "  store volatile %struct.SinObj** %~a, %struct.SinObj*** @fpr, align 8"
                      newfp)
             ; load @retr value into register
             ,(format "  %~a load %struct.SinObj*, %struct.SinObj** @retr, align 8"
                      (car ret))
             ; store @retr value into (hash-ref local-mapping x)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car ret) (cdr ret)))]
          [`(return ,x)
           (define base (gensym (symbol-append 'retval_ x)))
           (define val (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define ra (symbol-append base '_ra))
           (define fp (symbol-append base 'fp))
           ; get value of x from stack
           `(,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a, align 8"
                      (car val) (cdr val))
             ; store it into @retr
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** @retr, align 8"
                      (car val))
             ; load RA into a register from stack
             ,(format "  %~a = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8"
                      fp)
             ,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a, align 8"
                      ra fp)
             ; indirectbr to RA
             ,(format "  indirectbr i8* %~a, [TODO: all return labels]" ra))]
           (define base (gensym 'tailccc))
           (define csr (symbol-append base '_csr))
           (define csrbp (symbol-append base '_csr_base_ptr))
           (define csr-base (symbol-append base '_csr_base))
           (define cur-fp (symbol-append base '_curfp))
           (define caller-slots-loc (symbol-append base '_caller_slots_loc))
           (define caller-slots (symbol-append base '_caller_slots))
           (define csr-int (symbol-append base '_csr_int))
           (define fp-int (symbol-append base '_fp_int))
           (define offset-fp-mem (symbol-append base '_offset_fp_mem))
           (define offset-fp-slots (symbol-append base '_offset_fp_slots))
           (define offset-caller-mem (symbol-append base '_offset_caller_mem))
           (define offset-caller-slots (symbol-append base '_offset_caller_slots))
           (define new-top-segment (symbol-append base '_new_top_segment))
           (define underflow-addr-raw (symbol-append base '_underflow_addr_raw))
           (define underflow-addr (symbol-append base '_underflow_addr))
           (define clo (cons (symbol-append base '_clo) (hash-ref local-mapping xf)))
           (define old-srr (symbol-append base '_oldsrr))
           (define ret-addr-cast (symbol-append base '_retaddrcast))
           (define ret-addr (symbol-append base '_retaddr))
           (define cont-func-handler-loc (symbol-append base '_cont_funchandlerloc))
           (define cont-clo (symbol-append base '_cont_clo))
           (define arglistnull (symbol-append base '_arglist_null))
           (define arglist (symbol-append base '_arglist))
           (define clo-arg-loc (symbol-append base '_clo_arg_loc))
           (define arglist-loc (symbol-append base '_arg_list_loc))
           (define clo-fn-part (symbol-append base '_clo_fn_part))
           [`(return (call/cc ,xf))
            ; TODO: when we are already at the base, we shouldnt do a split_record call...
           `(
             ; Get Current record
             ,(format "  %~a = load %struct.SinRecord*, %struct.SinRecord** @srr, align 8"
                      old-stack-record)
             ; get the stack part of the record
             ,(,(format (string-append "  %~a = getelementptr inbounds "
                                       "%struct.SinRecord, %struct.SinRecord* %~a, "
                                       "i32 0, i32 0")
                        old-stack-record-base-loc old-stack-record))
             ,(format "  %~a = load %struct.SinObj**, %struct.SinObj*** %~a, align 8"
                      old-stack-record-base old-stack-record-base-loc)
             ; get the frame-pointer, which will also be the base of the new segment.
             ,(format "  %~a = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8"
                      cur-fp)
             ; Get the number of slots to cutoff.
             ,(format "  %~a = ptrtoint %struct.SinObj** %~a to i64"
                      cur-fp-int cur-fp)
             ,(format "  %~a = ptrtoint %struct.SinObj** %~a to i64"
                      old-stack-record-base-int old-stack-record-base)
             ,(format "  %~a = sub i64 %~a, %~a"
                      cutoff-offset-bytes cur-fp-int old-stack-record-base-int)
             ,(format "  %~a = lshr i64 nuw nsw i64 %~a, 3"
                      cuttoff-offset-slots cutoff-offset-bytes)
             ; Get the number of slots to the previous frame start from base of old record.
             ,(format "  %~a = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %~a, i64 ~a"
                      slots-in-caller-loc cur-fp size-pos)
             ,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a"
                      slots-in-caller-ptr slots-in-caller-loc)
             ,(format "  %~a = ptrtoint %struct.SinObj* %~a to i64"
                      slots-in-caller slots-in-caller-ptr)
             ,(format "  %~a = mul i64 %~a, -1"
                      negated-slots-in-caller slots-in-caller)
             ,(format "  %~a = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %~a, i64 %~a"
                      caller-fp-loc cur-fp negated-slots-in-caller)
             ,(format "  %~a = ptrtoint %struct.SinObj** %~a to i64"
                      caller-fp-loc-int caller-fp-loc)
             ,(format "  %~a = sub i64 %~a, %~a"
                      caller-fp-offset-bytes caller-fp-loc-int old-stack-record-base-int)
             ,(format "  %~a = lshr nuw nsw i64 %~a, 3"
                      caller-fp-offset-slots caller-fp-offset-bytes)
             ; Get the return address in the code stream for the old record.
             ,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a, align 8"
                      ret-addr-sinobj cur-fp)
             ,(format "  %~a = bitcast %struct.SinObj* %~a i8*"
                      ret-addr ret-addr-sinobj)
             ; Split the records!
             ,(format (string-append "  %~a = call %struct.SinRecord* @split_record"
                                     "(%struct.SinRecord* %~a, SinObj** %~a, i64 %~a, i64 %~a, i8* %~a)")
                      new-top-segment old-stack-record cur-fp
                      cutoff-offset-slots caller-fp-offset-slots ret-addr)
             ; place the underflow address in place of the return address
             ,(format "  %~a = i8* blockaddress(@main, %underflow_handle)"
                      underflow-addr-raw)
             ,(format "  %~a = bitcast i8* %~a to %struct.SinObj*"
                      underflow-addr underflow-addr-raw)
             ,(format "  store %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      underflow-addr cur-fp)
             ; place the new top segment in @srr
             ,(format "  store volatile %struct.SinRecord* %~a, %struct.SinRecord** @srr, align 8"
                      new-top-segment)
             ; Create the continuation object
             ,(format (string-append "  %~a = call %struct.SinObj* @make_continuation_closure"
                                     "(%struct.SinRecord* %~a, i8* "
                                     "blockaddress(@main, %continuation_function_handler))")
                      cont-clo old-stack-record)
             ; TODO: Ensure there is enough stack space for the callee
             ; Get argument locations
             ,(format "  %~a = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %~a, i64 %~a"
                      clo-arg-loc cur-fp arg0-pos)
             ,(format "  %~a = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %~a, i64 %~a"
                      arglist-arg-loc cur-fp arg1-pos)
             ; Create args
             ,(format "  %~a = ge"))]
          [`(return (call/cc ,xf))
           `(
             ; call the damn function.
             ; TODO: ensure there is stack space!
             ,(format "  %~a = call %struct.SinObj* @const_init_null()"
                      arglistnull)
             ,(format "  %~a = call %struct.SinObj* @prim_cons(%struct.SinObj* %~a, %struct.SinObj* %~a)"
                      arglist cont-clo arglistnull)
             ; underflow handler is set at *@fpr[0]
             ; dont need to set size cause its underflow under this frame
             ; sooo we need to set the closure as *@fpr[2] and cont-clo as *@fpr[3]
             ,(format "  %~a = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %~a, i64 %~a"
                      clo-arg-loc cur-fp arg0-pos)
             ,(format "  %~a = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %~a, i64 %~a"
                      arglist-loc cur-fp arg1-pos)
             ,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a, align 8"
                      (car clo) (cdr clo))
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car clo) clo-arg-loc)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      arglist arglist-loc)
             ; indirectbr to the function!!!
             ,(format "  %~a = call i8* @closure_get_fn_part(%struct.SinObj* %~a)"
                      clo-fn-part (car clo))
             ,(format "  indirectbr i8* %~a [TODO: ALL FUNCTION LOCATIONS!]"
                      clo-fn-part))]
          [`(assign ,x (call/cc ,xf))
           `()]
          [`(return (clo-app ,xf ,xargs))
           ; we can do Tail-Call-Elimination!!!!
           ; BUT its not safe unless we ensure that there's enough stack space
           ; cause if we reuse the stack frame and the callee needs too much space...
           ; TODO: ensure there is stack space!
           (define base (gensym (symbol-append '_tailapp_ xf)))
           (define clo (cons (symbol-append base '_clo) (hash-ref local-mapping xf)))
           (define args (cons (symbol-append base '_args) (hash-ref local-mapping xargs)))
           (define arg0loc (symbol-append base '_arg0))
           (define arg1loc (symbol-append base '_arg1))
           (define funcptr (symbol-append base '_funcptr))
           (define fp (symbol-append base '_fp))
           `(; put arguments in registers
             ,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a, align 8"
                      (car clo) (cdr clo))
             ,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a, align 8"
                      (car args) (cdr args))
             ,(format "  %~a = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8"
                      fp)
             ; get arg locations
             ,(format (string-append "  %~a = getelementptr inbounds "
                                     "%struct.SinObj*, %struct.SinObj** "
                                     "%~a, i64 1")
                      arg0loc fp)
             ,(format (string-append "  %~a = getelementptr inbounds "
                                     "%struct.SinObj*, %struct.SinObj** "
                                     "%~a, i64 2")
                      arg1loc fp)
             ; place arguments on the stack (xf and xarglist)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car clo) arg0loc)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car args) arg1loc)
             ; get function pointer
             ,(format "  %~a = call i8* @closure_get_fn_part(%struct.SinObj* %~a)"
                      funcptr (car clo))
             ; indirectbr to it!
             ,(format "  indirectbr i8* %~a [TODO: ALL FUNCTIONS!]" funcptr))]))
      (foldl (λ (i acc) (append acc (layout-i i)))
             '() instructions))
    `(,@(layout-prologue proc)
      ,@(layout-body)
      (format "  ; Function ~a ended" xname)))
  (match-define `(,procs ...) lir)
  `(,@start-of-program
    ; TODO: I think this works...
    ,(append* (map layout-proc procs))
    ,@end-of-program))

(define morestack-func
  `("define void @morestack() {"
    "  ; get current register values"
    "  %cur_stack_record = load %struct.SinRecord*, %struct.SinRecord** @srr"
    "  %cur_frame_pointer = load %struct.SinObj**, %struct.SinObj*** @fpr"
    "  ; create a new stack record (node in the linked list)"
    (string-append "  %new_stack_record = call %struct.SinRecord*"
                   " @make_frame(%struct.SinRecord* %cur_stack_record,"
                   " %struct.SinObj** %cur_frame)")
    "  ; get the address of the srr part of a SinRecord"
    ; TODO: is this useful at all? Can I just load a SinObj** directly from %new_stack_record?
    "  %stack_addr = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %new_stack_record, i64 0"
    "  ; deref it to get the address of the start of the stack (the new @fpr)"
    "  %new_fpr = load %struct.SinObj**, %struct.SinObj*** %stack_addr, align 8"
    "  ; set the new created node as the new @srr and the stack addr as the new base of it, @fpr"
    "  store %struct.SinRecord* %new_stack_record, %struct.SinRecord** @srr"
    "  store %struct.SinObj** %new_fpr, %struct.SinObj*** @fpr"
    "}"))

(define (llvm-convert lir)
  (define norm-lir (normalize-names lir))
  (match-define globals (compute-globals norm-lir))
  (string-join
   `("; Begin user globals"
     ,(layout-globals globals)
     "; End user globals"
     ,@morestack-func
     ,@(layout-program norm-lir globals))))
