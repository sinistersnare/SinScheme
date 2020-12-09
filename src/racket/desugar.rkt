#lang racket

(provide desugar)
(require (only-in "utils.rkt" symbol-append prim? prims->list datum?))


; Input Language
;
; e ::= (letrec* ([x e] ...) e)
;     | (letrec ([x e] ...) e)
;     | (let* ([x e] ...) e)
;     | (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (lambda (x ...+ . x) e)
;     | (dynamic-wind e e e)
;     | (guard (x cond-claue ...) e)
;     | (raise e)
;     | (delay e)
;     | (force e)
;     | (and e ...)
;     | (or e ...)
;     | (cond cond-clause ...)
;     | (case e case-clause ...)
;     | (if e e e)
;     | (when e e)
;     | (unless e e)
;     | (set! x e)
;     | (begin e ...+)
;     | (call/cc e)
;     | (apply e e)
;     | (e e ...)
;     | x
;     | op
;     | (quote dat)

; cond-clause ::= (e) | (e e) | (else e)
; case-clause ::= ((dat ...) e) | (else e)
; dat is a datum satisfying datum?
; x is a variable satisfing symbol?
; op satisfys prim?

; desugar =>

; e ::= (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (apply e e)
;     | (prim op e ...)
;     | (apply-prim op e)
;     | (if e e e)
;     | (set! x e)
;     | (call/cc e)
;     | (e e ...)
;     | x
;     | (quote dat)


; Desugarging takes a program and removes a lot of 'redundant' cases.
; This includes things like `cond`, `when`, and `if` which can all be
; desugared into various usages of `if`.
; The result is a much smaller grammar, which makes more complicated
; compiler passes easier to implement.

;;;;;;;;;;;;;;;; UTILITY ITEMS ;;;;;;;;;;;;;;;;

;;; These structs are used in the desugaring env to
;;; denote which kind of value they are
;;; A value in the env struct can be any of these three.
; a transformer function
(struct t (desugar-func) #:transparent)
; marker value (used in a hash)
; stating that the key is a non-overridden primitive.
(struct p () #:transparent)
; an overridden value. The key may not be overriding a special form,
; it may just be a regular value and not truly override anything
; but whatever, i think you get the idea, this is a value that the
; user placed into the key.
(struct o (v) #:transparent)

; we prefix symbols when adding them to the env to ensure
; that they do not shadow special forms and prims in future passes.
; TODO: in the original solution, they didnt add the $ if it was already there
;       it didnt seem necessary so i removed that. Is it needed??
(define (prefix x) (symbol-append '$ x))

; extends the env of mapped symbols
; wraps the value in an o struct to signify that its a user mapping
(define (extend env keys vals)
  (foldl (λ (k v res) (hash-set res k (o v))) env keys vals))

;;;;;;;;;;;;;;;; END UTILITY ITEMS ;;;;;;;;;;;;;;;;

;; TODO support vector literals (see old bottom of old desugar-aux)

(define (desugar e)
  (desugar-aux (wrap e) (initial-env)))
(define (d e) (desugar-aux e (initial-env))) ; helper

; e is the expression that we are desugaring into the IR language.
; env is the hash of bound identifiers in the current scope.
;
; This function dispatches to the correct desugaring function
; it is mutually recursive with each desugaring function.
(define (desugar-aux e env)
  (match e
    ; match a special form that has not been overridden,
    [`(,(? (λ (ef) (t? (hash-ref env ef #f))) tft) ,_ ...)
     (match-define (t tf) (hash-ref env tft))
     (tf e env)]
    ; match a primitive operator in function position that has not been overridden.
    [`(,(? prim? op) ,es ...)
     #:when (p? (hash-ref env op #f))
     (desugar-aux `(%%prim ,op ,@es) env)]
    ; the expression we are evaluating is an atomic symbol, not a list.
    ; We need to check if the symbol is a primitive, a shadowed symbol,
    ; or a non-shadowed symbol.
    [(? symbol? x)
     (define mapsto (hash-ref env x #f))
     (match mapsto
       ; the symbol has not been overridden, so we just output it.
       [#f x]
       ; a non-shadowed primitive. When not in function position (AKA here),
       ; we need to eta-expand this to an apply-prim form.
       ; (see: https://stackoverflow.com/questions/55996764/ for eta-expansion)
       [(p) (desugar-aux `(%%lambda vararg (%%apply-prim ,x vararg)) env)]
       ; the symbol has set to v
       [(o v) v]
       [(t badform) (error (format "special form '~a' in wrong position" badform))])]
    ; a vector literal, the datums here arent quoted, so during expansion we quote them.
    [`#(,(? datum? dats) ...)
     (desugar-aux `(%%prim vector ,@(map (λ (d) (list '%%quote d)) dats)) env)]
    ; simple application, we desugar each part in the current env.
    [`(,es ...)
     (map (λ (e) (desugar-aux e env)) es)]
    [_ (raise `(bad-expression: ,e))]))

; the starting env for desugaring. This is needed
; to allow shadowing of primitives and special forms.
; We use %%SPECIAL to denote that the special form SPECIAL
; is using the 'true' meaning, not the possibly overridden meaning.
; (its a function so I can make forward-definitions in it)
(define (initial-env)
  (let*
      ; the base is the set of symbols that a user can overwrite.
      ; By default, their behavior is to do their usual function
      ; as a transformer for a special form of the language.
      ([base
        (hash
         'lambda (t desugar-lambda) 'λ (t desugar-lambda)
         'letrec (t desugar-letrec) 'letrec* (t desugar-letrec*)
         'let (t desugar-let) 'let* (t desugar-let*)
         'if (t desugar-if) 'and (t desugar-and) 'or (t desugar-or)
         'set! (t desugar-set!) 'quote (t desugar-quote)
         'begin (t desugar-begin) 'cond (t desugar-cond) 'case (t desugar-case)
         'when (t desugar-when) 'unless (t desugar-unless)
         'delay (t desugar-delay) 'force (t desugar-force)
         'dynamic-wind (t desugar-dynamic-wind) 'raise (t desugar-raise)
         'guard (t desugar-guard) 'call/cc (t desugar-call/cc)
         'let/cc (t desugar-let/cc)
         'prim (t desugar-prim) 'apply-prim (t desugar-apply-prim))]
       ; this map adds a set of symbols to base that are 'override safe'
       ; we prefix each key of base with %%. If a user makes a variable like
       ; %%begin then its UNDEFINED BEHAVIOR. %% variables are reserved by
       ; the compiler.
       ; if no shadowing occurs these mappings should all be equivalent
       ; to the non %% ones.
       [shadow-safe
        (foldl (λ (k v res) (hash-set res (symbol-append '%% k) v))
               base (hash-keys base) (hash-values base))]
       ; this map adds primitives so they can be shadowed
       ; it gives them a marker value because their transformation
       ; is all the same, we only need the key and the knowledge that they
       ; have not been overridden.
       [prim-env (foldl (λ (k res) (hash-set res k (p)))
                        shadow-safe (prims->list))])
    prim-env))

(define (desugar-lambda e env)
  (match e
    [`(,_ (,(? symbol? xs) ...) ,body)
     (define pxs (map prefix xs))
     `(lambda ,pxs ,(desugar-aux body (extend env xs pxs)))]
    [`(,_ ,(? symbol? vararg) ,body)
     (define pv (prefix vararg))
     `(lambda ,pv ,(desugar-aux body (extend env (list vararg) (list pv))))]
    [`(,_ (,x0 ,xrest ... . ,xvararg) ,body)
     (define pos (cons x0 xrest))
     (define vararg (gensym '%%vararg))
     `(lambda ,vararg
        ,(desugar-aux
          `(%%let* ,(foldr
                     (lambda (x binds) `([,x (car ,vararg)] [,vararg (cdr ,vararg)]
                                                            . ,binds))
                     `([,xvararg ,vararg])
                     pos)
                   ,body)
          env))]))

(define (desugar-letrec e env)
  (match-define `(,_ ([,xs ,es] ...) ,body) e)
  (define temps (map (lambda (x) (gensym (symbol-append 'letrec-temp- x))) xs))
  (if (empty? xs) (desugar-aux body env)
      (desugar-aux
       `(%%let ,(map (λ (x) `(,x (%%quote ()))) xs)
               (%%let ,(map list temps es)
                      (%%begin
                       ,@(map (lambda (x e) `(%%set! ,x ,e)) xs temps)
                       ,body)))
       env)))

(define (desugar-letrec* e env)
  (match-define `(,_ ([,xs ,es] ...) ,body) e)
  (desugar-aux
   `(%%let ,(map (lambda (x) `(,x (%%quote ()))) xs)
           (%%begin ,@(map (lambda (x e) `(%%set! ,x ,e)) xs es) ,body))
   env))

(define (desugar-let e env)
  (match e
    [`(,_ () ,body) (desugar-aux body env)]
    [`(,_ ([,xs ,es] ...) ,body)
     ; prefix to ensure no shadowing of special forms / prims.
     (define pxs (map prefix xs))
     `(let ,(map (lambda (px ev) `(,px ,(desugar-aux ev env))) pxs es)
        ,(desugar-aux body (extend env xs pxs)))]
    [`(,_ ,loopvar ([,xs ,es] ...) ,body)
     (desugar-aux
      `(%%letrec* ([,loopvar (%%lambda ,xs ,body)])
                  (,loopvar ,@es))
      env)]))

(define (desugar-let* e env)
  (match e
    [`(,_ () ,body) (desugar-aux body env)]
    [`(,_ ([,x0 ,e0] ,rest ...) ,body)
     (desugar-aux `(%%let ([,x0 ,e0]) (%%let* ,rest ,body)) env)]))

(define (desugar-if e env)
  (match-define `(,_ ,ec ,et ,ef) e)
  `(if ,(desugar-aux ec env)
       ,(desugar-aux et env)
       ,(desugar-aux ef env)))

(define (desugar-and e env)
  (match e
    [`(,_) (desugar-aux `(%%quote #t) env)]
    [`(,_ ,e0) (desugar-aux e0 env)]
    [`(,_ ,e0 ,rest ...) (desugar-aux `(%%if ,e0 (%%and ,@rest) (%%quote #f)) env)]))

(define (desugar-or e env)
  (match e
    [`(,_) (desugar-aux `(%%quote #f) env)]
    [`(,_ ,e0) (desugar-aux e0 env)]
    [`(,_ ,e0 ,rest ...)
     (define cur (gensym '%%or))
     (desugar-aux `(%%let ([,cur ,e0]) (%%if ,cur ,cur (%%or . ,rest))) env)]))

(define (desugar-set! e env)
  (match-define `(,_ ,(? symbol? x) ,set-to) e)
  `(set! ,(prefix x) ,(desugar-aux set-to env)))

(define (desugar-quote e env)
  (match-define `(,_ ,(? datum? d)) e)
  `(quote ,d))

(define (desugar-begin e env)
  (match e
    [`(,_) (desugar-aux `(%%prim void) env)]
    [`(,_ ,e0) (desugar-aux e0 env)]
    [`(,_ ,e0 ,es ...)
     (desugar-aux `(%%let ([,(gensym '%%_) ,e0]) (%%begin ,@es)) env)]))

(define (desugar-cond e env)
  (match-define `(,_ ,clauses ...) e)
  (match clauses
    ['() (desugar-aux `(%%prim void) env)]
    [`((else ,expr)) (desugar-aux expr env)]
    [`((,etest) ,rest ...)
     (define condvalname (gensym '%%condval))
     (desugar-aux
      `(%%let ([,condvalname ,etest])
              (%%if ,condvalname ,condvalname
                    (%%cond ,@rest)))
      env)]
    [`((,etest => ,ef) ,rest ...)
     (define condvalname (gensym '%%condval))
     (desugar-aux
      `(%%let ([,condvalname ,etest])
              (%%if ,condvalname (,ef ,condvalname)
                    (%%cond ,@rest)))
      env)]
    [`((,e0 ,e1) ,rest ...)
     (desugar-aux `(%%if ,e0 ,e1 (%%cond ,@rest)) env)]))

(define (desugar-case e env)
  (match-define `(,_ ,key ,clauses ...) e)
  ; need to ensure that the key is evaluated only once
  ; so if its a symbol, it doesnt need evaluation
  ; but if its not, we let bind it and re-do the case
  ; but with the binding.
  (if (symbol? key)
      (match clauses
        ['() (desugar-aux `(%%prim void) env)]
        [`((else ,expr)) (desugar-aux expr env)]
        [`(((,(? datum? dats) ...) ,expr) ,rest ...)
         (desugar-aux
          `(%%if (%%prim memv ,key (%%quote ,dats))
                 ,expr
                 (%%case ,key ,@rest))
          env)])
      (let ([casekeyname (gensym '%%casekey)])
        (desugar-aux
         `(%%let ([,casekeyname ,key]) (%%case ,casekeyname ,@clauses))
         env))))

(define (desugar-when e env)
  (match-define `(,_ ,ec ,et) e)
  (desugar-aux `(%%if ,ec ,et (%%prim void)) env))

(define (desugar-unless e env)
  (match-define `(,_ ,ec ,ef) e)
  (desugar-aux `(%%if ,ec (%%prim void) ,ef) env))

(define (desugar-delay e env)
  (match-define `(,_ ,edelay) e)
  (desugar-aux
   `(%%prim vector (%%quote %%promise) (%%quote #f) (%%lambda () ,edelay))
   env))

; TODO: make a test like ((let ([quote 3]) (force (delay (lambda (x) x)))) '8)
; TODO: bug here if `promise?` is shadowed? And/or vector operations?
(define (desugar-force e env)
  (match-define `(,_ ,eprom) e)
  (define promname (gensym '%%promname))
  (define computed (gensym '%%computed))
  (desugar-aux
   `(%%let ([,promname ,eprom])
           (%%if (promise? ,promname)
                 (%%if (%%prim vector-ref ,promname (%%quote 1))
                       (%%prim vector-ref ,promname (%%quote 2))
                       (%%begin (%%prim vector-set! ,promname
                                        (%%quote 1) (%%quote #t))
                                (%%let ([,computed ((%%prim vector-ref ,promname (%%quote 2)))])
                                       (%%prim vector-set! ,promname (%%quote 2) ,computed))
                                (%%prim vector-ref ,promname (%%quote 2))))
                 (%%raise (%%quote "Value given is not a promise"))))
   env))

(define (desugar-dynamic-wind e env)
  (match-define `(,_ ,fpre ,fbody ,fpost) e)
  (match-define `(,pre ,body ,post ,val)
    (list (gensym '%%pre) (gensym '%%body) (gensym '%%post) (gensym '%%val)))
  (desugar-aux
   `(%%let ([,pre ,fpre] [,body ,fbody] [,post ,fpost])
           (%%begin
            (,pre)
            (%%set! %wind-stack (%%prim cons (%%prim cons ,pre ,post) %wind-stack))
            (%%let ([,val (,body)])
                   (%%begin
                    (%%set! %wind-stack (%%prim cdr %wind-stack))
                    (,post)
                    ,val))))
   env))

(define (desugar-raise e env)
  (match-define `(,_ ,err) e)
  (desugar-aux `(%raise-handler ,err) env))

(define (desugar-guard e env)
  (match-define `(,_ (,gvar ,clauses ...) ,body) e)
  (match clauses
    ['() (desugar-aux body env)]
    [`(,clauses ...)
     (define old-handler (gensym '%%old-handler))
     (define cc (gensym '%%cc))
     (desugar-aux
      `(%%let ([,old-handler %raise-handler] [,cc (%%call/cc (%%lambda (k) k))])
              (%%cond
               ; cc is a procedure when we run it initially.
               [(%%prim procedure? ,cc)
                (%%dynamic-wind
                 (%%lambda () (%%set! %raise-handler
                                      ; we call the current continuation when we raise
                                      ; and give it the cons
                                      (%%lambda (x) (,cc (%%prim cons x (%%quote ()))))))
                 (%%lambda () ,body)
                 (%%lambda () (%%set! %raise-handler ,old-handler)))]
               ; cc is a cons when an exception is raised
               [(%%prim cons? ,cc)
                (%%let ([,gvar (%%prim car ,cc)]) (%%cond ,@clauses))]))
      env)]))

(define (desugar-call/cc e env)
  (match-define `(,_ ,ef) e)
  (define kvar (gensym '%%k))
  (define oldstack (gensym '%%oldstack))
  (define xvar (gensym '%%x))
  `(call/cc
    ,(desugar-aux
      `(%%lambda
        (,kvar)
        (,ef (%%let ([,oldstack %wind-stack])
                    (%%lambda (,xvar)
                              (%%begin
                               (%%unless (%%prim eq? ,oldstack %wind-stack)
                                         (%do-wind ,oldstack))
                               (,kvar ,xvar))))))
      env)))

(define (desugar-let/cc e env)
  (match-define `(,_ ,letkvar ,body) e)
  (desugar-aux `(%%call/cc (%%lambda (,letkvar) ,body)) env))

; These two are not part of the source language
; but adding them makes life easier! No harm done :)
(define (desugar-prim e env)
  (match-define `(,_ ,(? prim? op) ,es ...) e)
  `(prim ,op ,@(map (λ (e) (desugar-aux e env)) es)))

(define (desugar-apply-prim e env)
  (match-define `(,_ ,(? prim? op) ,elist) e)
  `(apply-prim ,op ,(desugar-aux elist env)))

; Wraps an expression with an initial library of items
; these are low level things, used by the special forms.
(define (wrap e)
  `(let*
       ([promise? (lambda (p?) (and (vector? p?)
                                    (equal? (vector-length p?) '3)
                                    (equal? (vector-ref p? '0) '%%promise)))]
        [%wind-stack '()]
        [%raise-handler
         (lambda (%%uncaught-raise-arg)
           (begin (print '"Uncaught Exception: ")
                  (print %%uncaught-raise-arg)
                  (print '"\n")
                  (halt (void))))]
        [%common-tail (lambda (xs ys)
                        (let ([lx (length xs)] [ly (length ys)])
                          (let loop ([x (if (> lx ly) (drop xs (- lx ly)) xs)]
                                     [y (if (> ly lx) (drop ys (- ly lx)) ys)])
                            (if (eq? x y) x (loop (cdr x) (cdr y))))))]
        [%do-wind
         (lambda (new-stack)
           (begin
             (unless (eq? new-stack %wind-stack)
               (let ([tail (%common-tail new-stack %wind-stack)])
                 (begin
                   ; wind down the old stack, calling post-thunks as we go down
                   (let loop ([st %wind-stack])
                     (unless (eq? st tail)
                       (begin (set! %wind-stack (cdr st))
                              ((cdr (car st)))
                              (loop (cdr st)))))
                   ; wind up the new stack, calling pre-thunks as we go up
                   (let loop ([st new-stack])
                     (unless (eq? st tail)
                       (begin (loop (cdr st))
                              ((car (car st)))
                              (set! %wind-stack st)))))))))])
     ,e))
