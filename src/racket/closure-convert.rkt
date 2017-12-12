#lang racket

; Written By Davis Silverman


(require "utils.rkt")

(provide closure-convert
         proc->llvm)

(define (disp s v)
  (display s) (displayln v))

; Pass that removes lambdas and datums as atomic and forces them to be let-bound
;   ...also performs a few small optimizations
(define (simplify-ae e)
  (define (wrap-aes aes wrap)
    (match-define (cons xs wrap+)
      (foldr (lambda (ae xs+wrap)
               (define gx (gensym 'arg))
               (if (symbol? ae)
                   (cons (cons ae (car xs+wrap))
                         (cdr xs+wrap))
                   (cons (cons gx (car xs+wrap))
                         (lambda (e)
                           (match ae
                             [`(lambda ,xs ,body)
                              `(let ([,gx (lambda ,xs ,(simplify body))])
                                 ,((cdr xs+wrap) e))]
                             [`',dat
                              `(let ([,gx ',dat])
                                 ,((cdr xs+wrap) e))])))))
             (cons '() wrap)
             aes))
    (wrap+ xs))
  (define (simplify e)
    (match e
      [`(let ([,x (lambda ,xs ,elam)]) ,e0)
       `(let ([,x (lambda ,xs ,(simplify-ae elam))]) ,(simplify e0))]
      [`(let ([,x ',dat]) ,e0)
       `(let ([,x ',dat]) ,(simplify e0))]
      [`(let ([,x (prim ,op ,aes ...)]) ,e0)
       (wrap-aes aes (lambda (xs) `(let ([,x (prim ,op ,@xs)]) ,(simplify e0))))]
      [`(let ([,x (apply-prim ,op ,aes ...)]) ,e0)
       (wrap-aes aes (lambda (xs) `(let ([,x (apply-prim ,op ,@xs)]) ,(simplify e0))))]
      [`(if (lambda . ,_) ,et ,ef)
       (simplify et)]
      [`(if '#f ,et ,ef)
       (simplify ef)]
      [`(if ',dat ,et ,ef)
       (simplify et)]
      [`(if ,(? symbol? x) ,et ,ef) ;TODO: optimization with 0-CFA to tell if some x is false & has no side effects?
       `(if ,x ,(simplify et) ,(simplify ef))]
      [`(apply ,ae0 ,ae1)
       (define out (wrap-aes (list ae0 ae1) (lambda (xs) `(apply . ,xs))))
       ; (disp "OUT:: " out)
       out]
      [`(,aes ...)
       (wrap-aes aes (lambda (xs) xs))]))
  (simplify e))


; Helper to remove vararg lambdas/callsites
(define (remove-varargs e)
  (define (layout-untagged fname args)
    (define (layout fname args consname)
      (match args
        [(? empty?)
         `(,fname ,consname)]
        [`(,head . ,tail)
         (define cname (gensym 'arglist))
         `(let ([,cname (prim cons ,head ,consname)]) ,(layout fname tail cname))]))
    (define consnil (gensym 'consnil))
    `(let ([,consnil '()]) ,(layout fname (reverse args) consnil)))
  (define (remove e)
    (match e
      [`(let ([,x ',dat]) ,e0)
       `(let ([,x ',dat]) ,(remove e0))]
      [`(let ([,x (prim ,op ,xs ...)]) ,e0)
       `(let ([,x (prim ,op ,@xs)]) ,(remove e0))]
      [`(let ([,x (apply-prim ,op ,y)]) ,e0)
       `(let ([,x (apply-prim ,op ,y)]) ,(remove e0))]
      [`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
       ; turns (xs ...) into x and immediately into (x)
       ; by adding the needed car/cdr calls and let bindings
       (define gx (gensym 'rvp))
       (define gx+e
         (foldr (lambda (x gx+e)
                  (define gx (gensym 'rvp))
                  (cons gx
                        `(let ([,x (prim car ,gx)])
                           (let ([,(car gx+e) (prim cdr ,gx)])
                             ,(cdr gx+e)))))
                (cons (gensym 'na) (remove body))
                xs))
       `(let ([,x (lambda (,(car gx+e)) ,(cdr gx+e))])
          ,(remove e0))]
      [`(let ([,x (lambda ,y ,body)]) ,e0)
       `(let ([,x (lambda (,y) ,(remove body))])
          ,(remove e0))]
      [`(if ,x ,e0 ,e1)
       `(if ,x ,(remove e0) ,(remove e1))]
      [`(apply ,f ,args)
       `(,f ,args)]
      [`(,fname ,args ...)
       ; (displayln (format "fname:: ~s :: args ~s" fname args))
       (layout-untagged fname args)]))
  (remove e))

; (f a b c) -> (clo-app f a b c) -> (C-style-app f[0] f a b c)
; call simplify-ae on input to closure convert, then remove vararg callsites/lambdas
(define (closure-convert cps)
  (define scps (simplify-ae cps))
  (define no-varargs-cps (remove-varargs scps))
  ; Exp x List[Proc] -> Exp x Set[Var] x List[Proc]
  (define (bottom-up e procs)
    (match e
      [`(let ([,x ',dat]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
         (bottom-up e0 procs))
       `((let ([,x ',dat]) ,e0+)
         ,(set-remove free+ x)
         ,procs+)]
      [`(let ([,x (prim ,op ,xs ...)]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
         (bottom-up e0 procs))
       `((let ([,x (prim ,op ,@xs)]) ,e0+)
         ,(set-remove (set-union free+ (list->set xs)) x)
         ,procs+)]
      [`(let ([,x (apply-prim ,op ,args)]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+) (bottom-up e0 procs))
       `((let ([,x (apply-prim ,op ,args)]) ,e0+)
         ,(set-remove (set-add free+ args) x) ; HELP: IS THIS RIGHT????
         ,procs+)]
      [`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
       (match-define `(,e0+ ,free0+ ,procs0+)
         (bottom-up e0 procs))
       (match-define `(,body+ ,freelam+ ,procs1+)
         (bottom-up body procs0+))
       (define env-vars (foldl (lambda (x fr) (set-remove fr x))
                               freelam+
                               xs))
       (define ordered-env-vars (set->list env-vars))
       (define lamx (gensym 'lam))
       (define envx (gensym 'env))
       (define body++ (cdr (foldl (lambda (x count+body)
                                    (match-define (cons cnt bdy) count+body)
                                    (cons (+ 1 cnt) `(let ([,x (env-ref ,envx ,cnt)]) ,bdy)))
                                  (cons 1 body+)
                                  ordered-env-vars)))
       `((let ([,x (make-closure ,lamx ,@ordered-env-vars)]) ,e0+)
         ,(set-remove (set-union free0+ env-vars) x)
         ((proc (,lamx ,envx ,@xs) ,body++) . ,procs1+))]
      [`(if ,(? symbol? x) ,e0 ,e1)
       (match-define `(,e0+ ,free0+ ,procs0+)
         (bottom-up e0 procs))
       (match-define `(,e1+ ,free1+ ,procs1+)
         (bottom-up e1 procs0+))
       `((if ,x ,e0+ ,e1+)
         ,(set-union free1+ free0+ (set x))
         ,procs1+)]
      [`(,(? symbol? xs) ...)
       `((clo-app ,@xs)
         ,(list->set xs)
         ,procs)]))
  (match-define `(,main-body ,free ,procs) (bottom-up no-varargs-cps '()))
  `((proc (main) ,main-body) . ,procs))


(define (unweirdify s) (string->symbol (c-name s)))
(define (normalize-names proc)
  ; dont unweirdify op names, because they will be converted in prim-name and applyprim-name
  (match proc
    [`(proc (,fname ,args ...) ,ebody)
     `(proc (,(unweirdify fname) ,@(map unweirdify args)) ,(normalize-names ebody))]
    [`(let ([,x (apply-prim ,op ,args)]) ,ebody)
     `(let ([,(unweirdify x) (apply-prim ,op #;,(unweirdify op) ,(unweirdify args))]) ,(normalize-names ebody))]
    [`(let ([,x (prim ,op ,args ...)]) ,ebody)
     `(let ([,(unweirdify x) (prim ,op #;,(unweirdify op) ,@(map unweirdify args))]) ,(normalize-names ebody))]
    [`(let ([,x (make-closure ,fname ,args ...)]) ,ebody)
     `(let ([,(unweirdify x) (make-closure ,(unweirdify fname) ,@(map unweirdify args))]) ,(normalize-names ebody))]
    [`(let ([,x (env-ref ,env ,(? natural? nat))]) ,ebody)
     `(let ([,(unweirdify x) (env-ref ,(unweirdify env) ,nat)]) ,(normalize-names ebody))]
    [`(let ([,x ',dat]) ,ebody)
     `(let ([,(unweirdify x) ',dat]) ,(normalize-names ebody))]
    [`(clo-app ,fname ,args ...)
     `(clo-app ,(unweirdify fname) ,@(map unweirdify args))]
    [`(if ,xcond ,et ,ef)
     `(if ,(unweirdify xcond) ,(normalize-names et) ,(normalize-names ef))]))

; Hex encodes a string and adds a null terminator.
(define (c-encode s-unknown)
  (define s (if (string? s-unknown) s-unknown (symbol->string s-unknown)))
  (string-append (string-join (map (lambda (c)
                                     (format "\\~a"
                                             (~r (char->integer c) #:base '(up 16) #:min-width 2 #:pad-string "0")))
                                   (string->list s)) "") "\\00"))
(define (enc-len s) (/ (string-length s) 3))

; Walk procedures and emit llvm code as a string		
; (string-append "  %r0 = opcode i64 %a, %b \n"		
;                "  %r1 = ... \n")
;; TODO/HELP: attributes?
;; HELP: define attributes and call attributes need to be fastcc for tail-calls i think.
; HELP: main probably needs to return i32.
(define (proc->llvm procs)
  (define (make-args args)
    (string-join (map (lambda (x) (format "i64 %~s" x)) args) ", "))
  (define (ind n [level 2]) ; indent based levels, default tab level is 2.
    (make-string (* n level) #\space))
  (define (to-llvm proc globals [indent-level 0])
    (match proc
      [`(proc (,fname ,xs ...) ,e)
       (format "~adefine void @proc_~a(~a) {\n~a\n}"
               (ind indent-level) fname
               (make-args xs) (to-llvm e globals (add1 indent-level)))]
      [`(let ([,x (apply-prim ,op ,xs)]) ,e) ; TODO: TEST this (becuase make-args may give wrong output).
       (format "~a%~s = call i64 @~a(~a)\n~a"
               (ind indent-level) x (prim-applyname op)
               (make-args (list xs)) (to-llvm e globals indent-level))]
      [`(let ([,x (prim ,op ,xs ...)]) ,e)
       (format "~a%~s = call i64 @~a(~a)\n~a"
               (ind indent-level) x (prim-name op)
               (make-args xs) (to-llvm e globals indent-level))]
      ; TODO: can const_init_string be used here AKA is there another case of string?
      ; TODO: Also void (check all const_init_*).
      [`(let ([,x '#t]) ,e)
       (format "~a%~s = load i64, i64* @TRUE\n~a" (ind indent-level) x (to-llvm e globals indent-level))]
      [`(let ([,x '#f]) ,e)
       (format "~a%~s = load i64, i64* @FALSE\n~a" (ind indent-level) x (to-llvm e globals indent-level))]
      [`(let ([,x ',(? integer? n)]) ,e)
       (format "~a%~s = call i64 @const_init_int(i64 ~a)\n~a" (ind indent-level) x n (to-llvm e globals indent-level))]
      [`(let ([,x '()]) ,e)
       (format "~a%~s = load i64, i64* @NULL\n~a" (ind indent-level) x (to-llvm e globals indent-level))]
      [`(let ([,x ',(? symbol? dat)]) ,e)
       (define encoded (c-encode (symbol->string dat)))
       (define val (hash-ref globals encoded))
       (define stackregname (gensym 'stackreg))
       (define rawval (gensym 'rawsymstr))
       (if (hash-has-key? globals encoded)
           (format "\n~a\n~a\n~a\n~a\n~a"
                   (format "~a%~s = alloca i8*, align 8" (ind indent-level) stackregname)
                   (format "~astore i8* getelementptr inbounds ([~a x i8], [~a x i8]* @~a, i32 0, i32 0), i8** %~s, align 8"
                           (ind indent-level) (enc-len encoded) (enc-len encoded) val stackregname)
                   (format "~a%~s = load i8*, i8** %~s, align 8" (ind indent-level) rawval stackregname)
                   (format "~a%~s = call i64 @const_init_symbol(i8* %~s)" (ind indent-level) x rawval)
                   (to-llvm e globals indent-level))
           ((disp "DONT KNOW HOW TO LLVM-ENCODE:: " dat)
            "BAD-LLVM-IR-SORRY"))] ;; maybe const_init_symbol?
      [`(let ([,x ',(? string? dat)]) ,e)
       (define encoded (c-encode dat))
       (define val (hash-ref globals encoded))
       (define stackregname (gensym 'stackreg))
       (define rawval (gensym 'rawstrstr))
       (if (hash-has-key? globals encoded)
           (format "\n~a\n~a\n~a\n~a\n~a"
                   (format "~a%~s = alloca i8*, align 8" (ind indent-level) stackregname)
                   (format "~astore i8* getelementptr inbounds ([~a x i8], [~a x i8]* @~a, i32 0, i32 0), i8** %~s, align 8"
                           (ind indent-level) (enc-len encoded) (enc-len encoded) val stackregname)
                   (format "~a%~s = load i8*, i8** %~s, align 8" (ind indent-level) rawval stackregname)
                   (format "~a%~s = call i64 @const_init_string(i8* %~s)" (ind indent-level) x rawval)
                   (to-llvm e globals indent-level))
           ((disp "DONT KNOW HOW TO LLVM-ENCODE:: " dat)
            "BAD-LLVM-IR-SORRY"))]
      [`(let ([,x (env-ref ,env ,nat)]) ,e)
       (define envptr (gensym 'envptr))
       (define freevarloc (gensym 'freevar))
       (format "\n~a\n~a\n~a\n~a" ; optimization, use the same envptr name inttoptr for the whole function???
               (format "~a%~s = inttoptr i64 %~s to i64*" (ind indent-level) envptr env)
               (format "~a%~s = getelementptr inbounds i64, i64* %~s, i64 ~a" (ind indent-level) freevarloc envptr nat)
               (format "~a%~s = load i64, i64* %~s, align 8" (ind indent-level) x freevarloc)
               (to-llvm e globals indent-level))] ;;HELP: is nat quoting right?
      [`(let ([,x (make-closure ,fname ,freevars ...)]) ,ebody)
       (define (layout-freevars indent-level cloname freevars pos)
         (define (layout indent-level cloname freevars pos)
           (match freevars
             [(? empty?) (format "~a; no more free vars" (ind indent-level))]
             [`(,head . ,tail)
              (define freevarloc (gensym 'freevarloc))
              (format "~a\n~a\n~a"
                      (format "~a%~s = getelementptr inbounds i64, i64* %~s, i64 ~a" (ind indent-level) freevarloc cloname pos)
                      (format "~astore i64 %~s, i64* %~s" (ind indent-level) head freevarloc)
                      (layout-freevars indent-level cloname tail (add1 pos)))]))
         (if (empty? freevars)
             ""
             (string-append "\n" (layout indent-level cloname freevars pos))))
       (define cloname (gensym 'cloobject))
       (define funptrloc (gensym 'funptrloc))
       (define fintname (gensym 'fintname))
       (format "\n~a~a\n~a\n~a\n~a\n~a\n~a" ; ~a~a because layout-freevars places its own \n
               (format "~a%~s = call i64* @alloc(i64 ~a)" (ind indent-level) cloname (* 8 (add1 (length freevars)))) ; add1 because need room for fname too.
               (layout-freevars indent-level cloname freevars 1)
               (format "~a%~s = getelementptr inbounds i64, i64* %~s, i64 0" (ind indent-level) funptrloc cloname)
               (format "~a%~s = ptrtoint void(i64, i64)* @proc_~s to i64" (ind indent-level) fintname fname)
               (format "~astore i64 %~s, i64* %~s" (ind indent-level) fintname funptrloc)
               (format "~a%~s = ptrtoint i64* %~s to i64" (ind indent-level) x cloname)
               (to-llvm ebody globals indent-level))]
      [`(clo-app ,fname ,arglist)
       (define cloname (gensym 'appcloptr))
       (define fnptrpart (gensym 'fnptrpart))
       (define fintname (gensym 'fintname))
       (define funptrname (gensym 'funptr))
       (format "\n~a\n~a\n~a\n~a\n~a\n~a"
               (format "~a%~s = inttoptr i64 %~s to i64*" (ind indent-level) cloname fname)
               (format "~a%~s = getelementptr inbounds i64, i64* %~s, i64 0" (ind indent-level) fnptrpart cloname)
               (format "~a%~s = load i64, i64* %~s, align 8" (ind indent-level) fintname fnptrpart)
               (format "~a%~s = inttoptr i64 %~s to void (i64,i64)*" (ind indent-level) funptrname fintname)
               (format "~amusttail call fastcc void %~s(i64 %~a, i64 %~a)" (ind indent-level) funptrname fname arglist)
               (format "~aret void" (ind indent-level)))]
      [`(clo-app ,f ,xs ...)
       (disp "CLO-APP: IDK HOW TO HANDLE NON-RUNTIME-ARG-LIST:: " proc) 'BADLOL]
      [`(if ,grd ,et ,ef)
       (define cmpr (gensym 'cmp))
       (define thenblock (gensym 'thenblock))
       (define elseblock (gensym 'elseblock))
       (format "\n~a\n~a\n~a\n~a"
               (format "~a%~s = icmp ne i64 %~s, 15" (ind indent-level) cmpr grd) ; 15 is false. TODO: globals.
               (format "~abr i1 %~s, label %~s, label %~s" (ind indent-level) cmpr thenblock elseblock)
               (format "~a~s:\n~a" (ind indent-level) thenblock (to-llvm et globals (add1 indent-level)))
               (format "~a~s:\n~a" (ind indent-level) elseblock (to-llvm ef globals (add1 indent-level))))]))
  (define (layout-procs procs globals) (map (lambda (proc) (to-llvm (normalize-names proc) globals)) procs))
  (define (setup-main) "define i32 @main() {\n  call fastcc void @proc_main()\n  ret i32 0\n}") ;; TODO: indentation changes wont affect this
  (define globals (get-globals procs))
  (string-append (layout-globals globals) "\n"
                 "@FALSE = global i64 15" "\n"
                 "@TRUE = global i64 31" "\n"
                 "@NULL = global i64 0" "\n"
                 (setup-main) "\n"
                 (string-join (layout-procs procs globals) "\n")))

(define (layout-globals g-hash)
  (define (layout gs)
    (match gs
      [(? empty?) "; end globals"]
      [`((,k . ,v) . ,tail)
       (format "~a\n~a"
               (format "@~a = private unnamed_addr constant [~a x i8] c\"~a\", align 8"
                       v (enc-len k) k)
               (layout tail))]))
  (layout (hash->list g-hash)))

(define (get-globals procs)
  (define (layout unsorted hs)
    (match unsorted
      [(? empty?) hs]
      [`(,head . ,tail)
       (define h (layout tail hs)) ; recurse to base before we start building list.
       (match head
         [(? string? str) (hash-set h (c-encode str) (gensym '.str.global))]
         [(? symbol? sym) (hash-set h (c-encode sym) (gensym '.sym.global))]
         [else (displayln (format "DONT KNOW HOW TO ENCODE:: " head))])]))
  (define unsorted-globals (foldr set-union (set) (filter (lambda (s) (not (set-empty? s))) (map get-symbols procs))))
  (layout (set->list unsorted-globals) (hash)))

(define (get-symbols proc)
  (define (get proc st)
    (match proc
      [`(proc (,fname ,args ...) ,ebody)
       (get ebody st)]
      [`(let ([,x (apply-prim ,op ,args)]) ,ebody)
       (get ebody st)]
      [`(let ([,x (prim ,op ,args ...)]) ,ebody)
       (get ebody st)]
      [`(let ([,x (make-closure ,fname ,args ...)]) ,ebody)
       (get ebody st)]
      [`(let ([,x (env-ref ,env ,(? natural? nat))]) ,ebody)
       (get ebody st)]
      [`(let ([,x '#t]) ,ebody)
       (get ebody st)]
      [`(let ([,x '#f]) ,ebody)
       (get ebody st)]
      [`(let ([,x ',(? integer? n)]) ,ebody)
       (get ebody st)]
      [`(let ([,x '()]) ,ebody)
       (get ebody st)]
      ;; useful cases
      [`(let ([,x ',(? symbol? dat)]) ,ebody)
       (get ebody (set-add st dat))]
      [`(let ([,x ',(? string? dat)]) ,ebody)
       (get ebody (set-add st dat))]
      [`(let ([,x ',dat]) ,ebody)
       (displayln (format "IDK HOW TO CONVERT THIS:: ~a" dat))
       (get ebody st)]
      [`(clo-app ,fname ,args ...)
       st] ;; base case
      [`(if ,xcond ,et ,ef)
       (set-union (get et st) (get ef st))]))
  (get proc (set)))

;; cid=175
;; 8 Byte align all Strings!!!!


;; TODO: clean up proc->llvm inner functions, its gross



;; FIXME: Bug when symbols have same contents as a string.
;;  Fix is to have separate global lists for each global type.





