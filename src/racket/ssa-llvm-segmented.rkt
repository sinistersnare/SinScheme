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

(define (get-proc-sizes normalized-lir)
  ; (proc (xname nslots _ _) _)
  (foldl (λ (p acc) (hash-set acc (car (cadr p)) (cadr (cadr p))))
         (hash) normalized-lir))

(define ra-pos 0)
(define size-pos 1)
(define arg0-pos 2)
(define arg1-pos 3)
(define locals-start-pos 4)

(define (layout-program lir globals proc-sizes)
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
      (define cur-fp (gensym 'prologue_cur_fp))
      `((label ,xname)
        ((% ,cur-fp) = load (** SinObj) (*** SinObj) (@ fpr))
        (comment "Create stack slots")
        ,@(foldl (λ (n acc)
                   `((% ,(symbol-append xname '_local n '_loc)) = getelementptr inbounds
                                                                (* SinObj) (** SinObj) (% ,cur-fp)
                                                                (i64 ,(+ n locals-start-pos))))
                 '() (range (- nslots locals-start-pos)))
        (comment "End function Prologue")))
    (define (layout-body)
      (define (layout-i i)
        (match i
          [`(label ,x)
           `(label ,x)]
          [`(jump (label ,x))
           `((br (label (% ,x))))]
          [`(assign ,x (phi (,xtval (label ,xtlabel)) (,xfval (label ,xflabel))))
           ; TODO: Im not sure xtval and xfval are actually used in the program.
           (raise 'this-is-broken-i-think-look-above.)
           (define base (symbol-append 'phi_ x))
           ; TODO: xtval and xfval should be given the same stack slot?
           (define ret (cons (symbol-append base 'reg) (hash-ref local-mapping x)))
           `(((% ,(car ret)) = phi (* SinObj)
                             [(% ,xtval) (label (% ,xtlabel))] [(% ,xfval) (label (% ,xflabel))])
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(if ,xc (label ,xt) (label ,xf))
           (define base (gensym 'if))
           (define reg (cons (symbol-append base '_reg) (hash-ref local-mapping xc)))
           (define istrue (symbol-append base 'istrue))
           ; put xc in a register
           ; check truthiness of xc
           ; branch on truthiness.
           `(((% ,(car reg)) = load (* SinObj) (** SinObj) (% ,(cdr reg)))
             ((% ,istrue) = call i64 (@ is_truthy_value) ([(* SinObj) (% ,(car reg))]))
             (br i1 (% ,istrue) (label (% ,xt)) (label (% ,xf))))]
          ; I bet we can use 1 less local stack slot by combining the slots of the
          ; true and false branch values here. Only 1 is taken, why do we need 2 slots.
          [`(assign ,x (prim ,op . ,xargs))
           (define base (gensym (symbol-append 'primtmp_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define args (map (λ (a) (cons (symbol-append base '_arg)
                                          (hash-ref local-mapping a)))
                             xargs))
           `(,@(map (λ (a) `((% ,(car a)) = load (* SinObj) (** SinObj) (% ,(cdr a)))) args)
             ((% ,(car ret)) = call (* SinObj) (@ ,(prim-name op))
                             (,@(map (λ (a) `[(* SinObj) (% ,(car a))]) args)))
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(assign ,x (apply-prim ,op ,xarglist))
           (define base (gensym (symbol-append 'applyprimtmp_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define arg (cons (symbol-append base '_arg) (hash-ref local-mapping xarglist)))
           `(((% ,(car arg)) = load (* SinObj) (** SinObj) (% ,(cdr arg)))
             ((% ,(car ret)) = call (* SinObj) (@ ,(applyprim-name op))
                             ([(* SinObj) (% ,(car arg))]))
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(assign ,x (make-closure ,xname . ,xfrees))
           (define base (gensym (symbol-append 'makeclo_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define args (map (λ (a) (cons (symbol-append base '_arg) (hash-ref local-mapping a)))
                             xfrees))
           (define funcaddr (symbol-append base '_funcaddr))
           `(; load all free variables from stack
             ,@(map (λ (a) `((% ,(car a)) = load (* SinObj) (** SinObj) (% ,(cdr a)))) args)
             ; Get the address of the function we are calling
             ((% ,funcaddr) = (* i8) blockaddress (@ main) (% ,xname))
             ; allocate the closure
             ((% ,(car ret)) = call (* SinObj) (@ closure_alloc)
                             ([i64 ,(length xfrees)] [(* i8) ,funcaddr]))
             ,@(map (λ (a i) `(call (@ closure_place_freevar) ([(* SinObj) ,(car ret)]
                                                               [(* SinObj) ,(cdr a)]
                                                               [i64 ,i])))
                    args (range (length args)))
             ; place fully formed closure on stack.
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(assign ,x (env-ref ,_ ,pos))
           ; The 1st argument to env-ref isn't needed here, because we can just refer
           ; to @fpr[1]. We dont actually place the argument as a name anywhere.
           ; woohoo to flat closures.
           (define base (gensym (symbol-append 'envref_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define clo (symbol-append base '_clo))
           (define clo-loc (symbol-append base '_cloloc))
           (define cur-fp (symbol-append base '_cur_fp))
           `(((% ,cur-fp) = load (** SinObj) (*** SinObj) (@ fpr))
             ((% ,clo-loc) = getelementptr inbounds (* SinObj) (** SinObj) (% ,cur-fp) (i64 ,arg0-pos))
             ((% ,clo) = load (* SinObj) (** SinObj) (% ,cur-fp))
             ((% ,(car ret)) = call (* SinObj) (@ closure_env_get) ([(* SinObj) (% ,clo)] [i64 ,pos]))
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(assign ,x ',(? boolean? b))
           (define base (gensym (symbol-append 'datbool_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define which (if b 'true 'false))
           `(((% ,(car ret)) = call (* SinObj) (@ ,(symbol-append 'const_init_ which)) ())
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(assign ,x ''())
           (define base (gensym (symbol-append 'datnull_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           `(((% ,(car ret)) = call (* SinObj) (@ const_init_null) ())
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(assign ,x ',(? integer? n))
           (define base (gensym (symbol-append 'datint_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           `(((% ,(car ret)) = call (* SinObj) (@ const_init_int) ([i64 ,n]))
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(assign ,x ',(or (? symbol? s) (? string? s)))
           ; TODO: what is globals.
           (define type (cond [(symbol? s) 'sym] [(string? s) 'str]))
           (define type-call (cond [(symbol? s) "symbol"] [(string? s) "string"]))
           (define as-string (cond [(symbol? s) (symbol->string s)] [(string? s) s]))
           (match-define (cons global-name size) (hash-ref globals (cons type as-string)))
           (define base (gensym (symbol-append 'dat type '_ x)))
           (define ret (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define raw (symbol-append base '_rawglobal))
           `(((% ,raw) = getelementptr inbounds (arr ,size (* i8)) (* (arr ,size (* i8)))
                       (@ ,global-name) (i64 0) (i64 0))
             ((% ,(car ret)) = call (* SinObj) (@ ,(symbol-append 'const_init_ type-call))
                             ([(* i8) (% ,raw)]))
             (store (* SinObj) (% ,(car ret)) (** SinObj) (% ,(cdr ret))))]
          [`(assign ,x0 ,x1)
           ; TODO: Does this show up at this point in the compiler?
           (displayln 'WE-DID-AN-ASSIGN??!?!?!?!?)
           ;; Load x1 from stack
           ;; Store into x1 via a SinObj* -> SinObj* bitcast.
           (define base (gensym (symbol-append 'assign_alias)))
           (define alias (hash-ref local-mapping x0))
           (define aliased (cons (symbol-append base '_aliased) (hash-ref local-mapping x1)))
           `(((% ,(car aliased)) = load (* SinObj) (** SinObj) (% ,(cdr aliased)))
             (store (* SinObj) (% ,(car aliased)) (** SinObj) (% ,alias)))]
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

          [`(assign ,x (call/cc ,xf))
           (define base (gensym 'tailccc))
           (define return-value (cons (symbol-append base '_return_value)
                                      (hash-ref local-mapping x)))
           (define clo (cons (symbol-append base '_callee_clo) (hash-ref local-mapping xf)))
           (define has-space (symbol-append '_has_space))
           (define do-call (symbol-append '_do_call))
           (define overflow-first (symbol-append '_overflow_first))
           (define old-record (symbol-append '_old_record))
           (define at-base (symbol-append '_at_base))
           (define dont-split (symbol-append '_dont_split))
           (define do-split (symbol-append '_do_split))
           (define old-record-next-loc (symbol-append '_old_record_next_loc))
           (define dont-split-record (symbol-append '_dont_split_record))
           (define finished-stack-splitting (symbol-append '_finished_stack_splitting))
           (define old-record-loc (symbol-append '_old_record_loc))
           (define old-record-stack (symbol-append '_old_record_stack))
           (define caller-fp (symbol-append '_caller_fp))
           (define callee-fp (symbol-append '_callee_fp))
           (define caller-fp-int (symbol-append '_caller_fp_int))
           (define callee-fp-int (symbol-append '_callee_fp_int))
           (define old-record-stack-int (symbol-append '_old_record_stack_int))
           (define cutoff-offset-bytes (symbol-append '_cutoff_offset_bytes))
           (define cutoff-offset-slots (symbol-append '_cutoff_offset_slots))
           (define return-fp-offset-bytes (symbol-append '_return_fp_offset_bytes))
           (define return-fp-offset-slots (symbol-append '_return_fp_offset_slots))
           (define return-address (symbol-append '_return_address))
           (define new-top-segment (symbol-append '_new_top_segment))
           (define stack-record-to-use (symbol-append '_stack_record_to_use))
           (define continuation-function-handler (symbol-append '_continuation_function_handler))
           (define cont-clo (symbol-append '_cont_clo))
           (define size-loc (symbol-append '_size_loc))
           (define callee-clo-arg-loc (symbol-append '_callee_clo_arg_loc))
           (define arglist-arg-loc (symbol-append '_arglist_arg_loc))
           (define arglist-null (symbol-append '_arglist_null))
           (define arglist (symbol-append '_arglist))
           (define underflow-addr-void (symbol-append '_underflow_addr_void))
           (define underflow-addr (symbol-append '_underflow_addr))
           (define underflow-size-as-sinobj (symbol-append '_underflow_size_as_sinobj))
           (define underflow-size-as-ptr (symbol-append '_underflow_size_as_ptr))
           (define clo-fn-part (symbol-append '_clo_fn_part))
           (define return-label (symbol-append '_return_label))
           ; Check if we have enough
           `(((% ,has-space) = call i1 (@ check_for_overflow)
                             ([(** SinRecord) (@ srr)]
                              [(*** SinObj) (@ fpr)]
                              [i64 ,nslots]
                              ; TODO: use @spr AND NOT proc-sizes, GET RID OF proc-sizes!!
                              [i64 ,(hash-ref proc-sizes xf)]))
             (br i1 (% ,has-space) (label (% ,do-call)) (label (% ,overflow-first)))
             (label ,overflow-first)
             ; if not, call overflow handler
             (call void (@ handle_overflow)
                   ([(** SinRecord) (@ srr)]
                    [(*** SinObj) (@ fpr)]
                    [(*** SinObj) (@ spr)]
                    [(* i8) (blockaddress (@ main) (% underflow_handler))]
                    [i64 ,nslots]))
             (br (label (% ,do-call)))
             ; Get Current record
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
             ; TODO: compile this line to `%~a = bitcast i8* blockaddress(...) to i8*`!!!
             ((% ,return-address) = (* i8) blockaddress (@ main) (% ,return-label))
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
                                      ,callee-fp (i64 ,arg0-pos))
             ((% ,arglist-arg-loc) = getelementptr inbounds (* SinObj) (** SinObj)
                                   ,callee-fp (i64 ,arg1-pos))
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
             (indirectbr (* i8) (% ,clo-fn-part) [TODO: ALL FUNCTION LOCATIONS])
             ; Make the return point, and clean up.
             (label ,return-label)
             ((% ,(car return-value)) = load (* SinObj) (** SinObj) (@ retr))
             (store (* SinObj) (% ,(car return-value)) (** SinObj) (% ,(cdr return-value))))]

          [`(return ,x)
           (define base (gensym (symbol-append 'retval_ x)))
           (define val (cons (symbol-append base '_val) (hash-ref local-mapping x)))
           (define ra (symbol-append base '_ra))
           (define fp (symbol-append base 'fp))
           `(; get value of x from stack
             ((% ,(car val) = load (* SinObj) (** SinObj) (% ,(cdr val))))
             ; store it into @retr
             (store (* SinObj) (% ,(car val)) (** SinObj) (@ retr))
             ; load RA into a register from stack
             ((% ,fp) = load (** SinObj) (*** SinObj) (@ fpr))
             ((% ,ra) = load (* SinObj) (** SinObj) (% ,fp))
             ; indirectbr to RA
             (indirectbr (* i8) (% ,ra) [TODO: ALL RETURN LABELS!!]))]

          [`(return (call/cc ,xf))
           (define base (gensym 'tailccc))
           (define has-space (symbol-append base '_has_space))
           (define do-call (symbol-append base '_do_call))
           (define overflow-first (symbol-append base '_overflow_first))
           (define at-base (symbol-append base '_at_base))
           (define dont-split (symbol-append base '_dont_split))
           (define do-split (symbol-append base '_do_split))
           (define old-stack-record-next-loc (symbol-append base '_old_stack_record_next_loc))
           (define dont-split-record (symbol-append base '_dont_split_record))
           (define finished-stack-stuff (symbol-append base '_finished_stack_stuff))
           (define old-stack-record (symbol-append base '_old_stack_record))
           (define old-stack-record-base-loc (symbol-append base '_old_stack_record_base_loc))
           (define old-stack-record-base (symbol-append base '_old_stack_record_base))
           (define cur-fp (symbol-append base '_cur_fp))
           (define cur-fp-int (symbol-append base '_cur_fp_int))
           (define old-stack-record-base-int (symbol-append base '_old_stack_record_base_int))
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
           (define new-top-segment (symbol-append base '_new_top_segment))
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
           (define stack-record-to-use (symbol-append base '_stack_record_to_use))
           `(; Check if we will overflow, jump to overflow handler if we will.
             ((% ,has-space) = call i1 (@ check_for_overflow)
                             ([(** SinRecord) (@ srr)]
                              [(*** SinObj) (@ fpr)]
                              [(*** SinObj) (@ spr)]
                              [i64 0]))

             ,(format (string-append "  %~a = call i1 @check_for_overflow"
                                     "(%struct.SinRecord** @srr, %struct.SinObj*** @fpr"
                                     ", i64 ~a, i64 ~a)")
                      ; use 0 here because the number of caller slots doesn't matter!
                      ; because its a tail call!
                      has-space 0 (hash-ref proc-sizes xf))
             ,(format "  br i1 %~a label %~a, label %~a"
                      has-space do-call overflow-first)
             ,(format "%~a:" overflow-first)
             ,(format (string-append "  call void @handle_overflow(%struct.SinRecord** @srr, "
                                     "%struct.SinObj*** @fpr, %struct.SinObj*** @spr,"
                                     " i8* blockaddress(@main, %underflow_handler), i64 ~a)")
                      nslots)
             ,(format "br label %~a" do-call)
             ; Check if we have enough
             ; if not, call overflow handler
             ; dont need to add a frame though, cause tail-call. Just overwrite @fpr as we do here.
             ; Get Current record
             ,(format "%~a:" do-call)
             ,(format "  %~a = load %struct.SinRecord*, %struct.SinRecord** @srr, align 8"
                      old-stack-record)
             ; Check if we are at the base,
             ,(format (string-append "%~a = call i1 @callcc_at_base"
                                     "(%struct.SinRecord** @srr, %struct.SinObj*** @fpr)")
                      at-base)
             ,(format "  br i1 %~a, label %~a, label %~a"
                      at-base dont-split do-split)
             ,(format "%~a:" dont-split)
             ; Get the location of the stack records link field
             ,(format "  %~a = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %~a, i64 0, i32 1"
                      old-stack-record-next-loc old-stack-record)
             ; get the value in that location (the pointer to the link)
             ,(format "  %~a = load %struct.SinRecord*, %struct.SinRecord** %~a, align 8"
                      dont-split-record old-stack-record-next-loc)
             ,(format "  br label %~a"
                      finished-stack-stuff)
             ,(format "%~a:" do-split)
             ; get the stack part of the record
             ,(format (string-append "  %~a = getelementptr inbounds "
                                     "%struct.SinRecord, %struct.SinRecord* %~a, "
                                     "i32 0, i32 0")
                      old-stack-record-base-loc old-stack-record)
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
                      ret-addr-sinobjptr cur-fp)
             ,(format "  %~a = bitcast %struct.SinObj* %~a i8*"
                      ret-addr ret-addr-sinobjptr)
             ; Split the records!
             ,(format (string-append "  %~a = call %struct.SinRecord* @split_record"
                                     "(%struct.SinRecord* %~a, SinObj** %~a, "
                                     "i64 %~a, i64 %~a, i8* %~a)")
                      new-top-segment old-stack-record cur-fp
                      cutoff-offset-slots caller-fp-offset-slots ret-addr)
             ; place the new top segment in @srr
             ,(format "  store volatile %struct.SinRecord* %~a, %struct.SinRecord** @srr, align 8"
                      new-top-segment)
             ,(format "  br label %~a"
                      finished-stack-stuff)
             ;;; Stack splitting done!
             ,(format "%~a:" finished-stack-stuff)
             ,(format "  %~a = phi %struct.SinRecord* [%~a, %~a], [%~a, %~a]"
                      stack-record-to-use old-stack-record do-split dont-split-record dont-split)
             ; Create the continuation object
             ,(format (string-append "  %~a = call %struct.SinObj* @make_continuation_closure"
                                     "(%struct.SinRecord* %~a, i8* "
                                     "blockaddress(@main, %continuation_function_handler))")
                      cont-clo stack-record-to-use)
             ; Get argument locations
             ,(format "  %~a = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %~a, i64 ~a"
                      size-loc cur-fp size-pos)
             ,(format "  %~a = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %~a, i64 ~a"
                      clo-arg-loc cur-fp arg0-pos)
             ,(format "  %~a = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %~a, i64 ~a"
                      arglist-arg-loc cur-fp arg1-pos)
             ; Create args
             ,(format "  %~a = call %struct.SinObj* @const_init_null()"
                      arglist-null)
             ,(format "  %~a = call %struct.SinObj* @prim_cons(%struct.SinObj* %~a, %struct.SinObj %~a)"
                      arglist cont-clo arglist-null)
             ; Get closure we are calling
             ,(format "  %~a = load %struct.SinObj*, %struct.SinObj** %~a, align 8"
                      (car clo) (cdr clo))
             ;;;; place things on stack
             ; place the underflow address in place of the return address
             ,(format "  %~a = i8* blockaddress(@main, %underflow_handle)"
                      underflow-addr-raw)
             ,(format "  %~a = bitcast i8* %~a to %struct.SinObj*"
                      underflow-addr underflow-addr-raw)
             ,(format "  store %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      underflow-addr cur-fp)
             ; Size here at underflow point is -1, this is helpful during overflow handling.
             ,(format "  %~a = inttoptr i64 -1 to %struct.SinObj*"
                      underflow-size-as-ptr)
             ; Store arguments
             ,(format "  store volatile %struct.SinObj %~a, %struct.SinObj** %~a, align 8"
                      underflow-size-as-ptr size-loc)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      (car clo) clo-arg-loc)
             ,(format "  store volatile %struct.SinObj* %~a, %struct.SinObj** %~a, align 8"
                      arglist arglist-arg-loc)
             ;;;;; Call the damn function.
             ,(format "  %~a = call i8* @closure_get_fn_part(%struct.SinObj* %~a)"
                      clo-fn-part (car clo))
             ,(format "  indirectbr i8* %~a, [TODO: All function locations]"
                      clo-fn-part))]

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
