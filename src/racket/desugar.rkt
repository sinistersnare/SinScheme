#lang racket

(provide desugar desugar-aux)
(require "utils.rkt")

;Input Language

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

(define (desugar e) (desugar-aux (wrap e)))

(define (desugar-aux e)
  (match e
    ; THIS IS A HACK!!!! I didnt want to figure out how to
    ; properly desugar prim, and this was ez.
    ; apply-prim is in the target lang, and not source
    ; so if this is seen, its already been desugared...
    ; BAD DAVIS, BAD!
    [`(apply-prim ,rest ...) `(apply-prim ,@rest)]
    ; match for prim anywhere in a form, and desugar it generally to apply-prim.
    [(? prim? op) (desugar-prim op)]
    [(? symbol? x) x]
    [`(quote ,(? datum? dat)) `(quote ,dat)]
    [`(letrec* (,bindings ...) ,body) (desugar-letrec* bindings body)]
    [`(letrec (,bindings ...) ,body) (desugar-letrec bindings body)]
    [`(let* (,bindings ...) ,body) (desugar-let* bindings body)]
    [`(let (,bindings ...) ,body) (desugar-let bindings body)]
    [`(let ,loop (,bindings ...) ,body) (desugar-let-loop loop bindings body)]
    [`(lambda (,binding ...) ,body) `(lambda ,binding ,(desugar-aux body))]
    [`(lambda (,b1 ,brest ... . ,blist) ,body) (desugar-lambda-list (cons b1 brest) blist body)]
    [`(lambda ,binding ,body) `(lambda ,binding ,(desugar-aux body))]
    [`(guard (,var ,clauses ...) ,body) (desugar-guard var clauses body)]
    [`(raise ,expression) (desugar-raise expression)]
    [`(delay ,expression) (desugar-delay expression)]
    [`(force ,expression) (desugar-force expression)]
    [`(and ,expressions ...) (desugar-and expressions)]
    [`(or ,expressions ...) (desugar-or expressions)]
    [`(cond ,clauses ...) (desugar-cond clauses)]
    [`(case ,exp ,clauses ...) (desugar-case exp clauses)]
    [`(if ,if ,then ,else) `(if ,(desugar-aux if) ,(desugar-aux then) ,(desugar-aux else))]
    [`(when ,if ,then) `(if ,(desugar-aux if) ,(desugar-aux then) ,(desugar-aux '(void)))]
    [`(unless ,if ,else) `(if ,(desugar-aux if) ,(desugar-aux '(void)) ,(desugar-aux else))]
    [`(set! ,x ,body) `(set! ,(desugar-aux x) ,(desugar-aux body))]
    [`(begin ,es ...) (desugar-begin es)]
    [`(call/cc ,e) (desugar-call/cc e)]
    [`(let/cc ,k ,e) (desugar-call/cc `(lambda (,k) ,e))]
    [`(apply ,func ,args) `(apply ,(desugar-aux func) ,(desugar-aux args))]
    [`(,func ,args ...) (map desugar-aux (cons func args))]
    [`#(,vecitems ...) (desugar-aux `(vector ,@(map (lambda (x) `',x) vecitems)))]
    [else (raise `('bad-syntax ,e))]))

(define (desugar-prim op)
  (let ([vararg (gensym 'aprim)]) (desugar-aux `(lambda ,vararg (apply-prim ,op ,vararg)))))

(define (desugar-letrec* bindings body)
  `(let ,(map (lambda binding `(,(caar binding) 'undefined)) bindings)
     ,(desugar-aux `(begin
                      ; TODO dont use a cons here, just use a quasiquote.
                      ,@(map (lambda binding (cons 'set! (cons (caar binding) (cdar binding))))
                             bindings)
                      ,body))))

(define (desugar-letrec bindings body)
  (define (desugar-letrec-body bindings body)
    (if (empty? bindings) (desugar-aux body)
        (let ([binding-name (gensym 'letrec-binding)])
          (desugar-aux
           `(let ([,binding-name ,(cadar bindings)])
              (begin (set! ,(caar bindings) ,binding-name)
                     ,(desugar-letrec-body (cdr bindings) body)))))))
  `(let ,(map (lambda binding `[,(caar binding) ',(gensym 'unset)]) bindings)
     ,(desugar-letrec-body bindings body)))

(define (desugar-let* bindings body)
  (if (empty? bindings) (desugar-aux body)
      `(let ([,(caar bindings) ,(desugar-aux (cadar bindings))])
         ,(desugar-let* (cdr bindings) body))))

(define (desugar-let bindings body)
  (define (desugar-bindings bindings)
    (match bindings
      ['() '()]
      [`((,name ,val) ,rest ...) (cons `[,name ,(desugar-aux val)] (desugar-bindings rest))]
      [else 'BAD-SYNTAX!]))
  (if (empty? bindings) (desugar-aux body) `(let ,(desugar-bindings bindings) ,(desugar-aux body))))

(define (desugar-let-loop loop bindings body)
  (desugar-aux `(letrec ([,loop (lambda ,(map car bindings) ,body)]) (,loop ,@(map cadr bindings)))))

(define (desugar-lambda-list varnamed varlist body)
  (define (desugar-let-bindings varnamed varlist curcar)
    (if (empty? varnamed)
        (desugar-aux `((,varlist ,curcar)))
        (desugar-aux `((,(car varnamed) (car ,curcar))
                       ,@(desugar-let-bindings (cdr varnamed) varlist `(cdr ,curcar))))))
  (let ([argvar (gensym 'argvar)])
    `(lambda ,argvar (let ,(desugar-let-bindings varnamed varlist argvar) ,(desugar-aux body)))))

(define (desugar-guard var clauses body)
  (match clauses
    ['() (desugar-aux body)]
    [`(,clauses ... [else ,onelse])
     (let ([cc (gensym 'cc)])
       (desugar-aux
        `(let ([%old-handler %raise-handler] [,cc (call/cc (lambda (k) k))])
           (if (cons? ,cc)
               ; an exception has been raised if it's a cons.
               (let ([,var (car ,cc)]) ,(desugar-cond `(,@clauses [else ,onelse])))
               (dynamic-wind
                (lambda () (set! %raise-handler (lambda (ex) (,cc (cons ex '()))))) ; set up handler
                (lambda () ,body)
                (lambda () (set! %raise-handler %old-handler)))))))]
    ; give an explicit else if not provided.
    [`(,clauses ...) (desugar-guard var `(,@clauses [else (raise ,var)]) body)]))

(define (desugar-raise e) (desugar-aux `(%raise-handler ,e)))

(define (desugar-delay expression)
  (desugar-aux `(vector '%%promise '#f (lambda () ,expression))))

(define (desugar-force expression)
  (let ([promname (gensym 'promname)])
    (desugar-aux `(let ([,promname ,expression])
                    (if (promise? ,promname)
                        (if (vector-ref ,promname '1)
                            (vector-ref ,promname '2)
                            (begin (vector-set! ,promname '1 '#t)
                                   (vector-set! ,promname '2 ((vector-ref ,promname '2)))
                                   (vector-ref ,promname '2)))
                        (raise '"value given is not a promise."))))))

(define (desugar-and expressions)
  (match expressions
    [`() ''#t]
    [`(,oneval) (desugar-aux oneval)]
    [`(,first ,rest ...) `(if ,(desugar-aux first) ,(desugar-and rest) '#f)]))

(define (desugar-or expressions)
  (match expressions
    [`() ''#f]
    [`(,oneval) (desugar-aux oneval)]
    [`(,first ,rest ...) (let ([orval (gensym 'orval)])
                           `(let ([,orval ,(desugar-aux first)])
                              (if ,orval ,orval ,(desugar-or rest))))]))

(define (desugar-cond expressions)
  (if (empty? expressions) (desugar-aux '(void))
      (match (car expressions)
        [`(else ,expr) (desugar-aux expr)]
        [`(,test) (let ([condvalname (gensym 'condval)])
                    `(let ([,condvalname ,(desugar-aux test)])
                       (if ,condvalname ,condvalname ,(desugar-cond (cdr expressions)))))]
        [`(,test ,expr) `(if ,(desugar-aux test)
                             ,(desugar-aux expr)
                             ,(desugar-cond (cdr expressions)))]
        [else (raise `('bad-syntax ,(car expressions)))])))

(define (desugar-case key clauses)
  (define (desugar-case-inner keyvar clauses)
    (if (empty? clauses) (desugar-aux '(void))
        (match (car clauses)
          [`((,dats ...) ,expr) (desugar-aux `(if (memv ,keyvar ',dats)
                                                  ,(desugar-aux expr)
                                                  ,(desugar-case-inner keyvar (cdr clauses))))]
          [`(else ,expr) (desugar-aux expr)]
          [else (raise `('bad-syntax ,(car clauses)))])))
  (let ([keyvar (gensym 'keyvar)])
    `(let ([,keyvar ,(desugar-aux key)])
       ,(desugar-case-inner keyvar clauses))))

(define (desugar-begin expressions)
  (match expressions
    [`() (desugar-aux '(void))]
    [`(,oneval) (desugar-aux oneval)]
    [`(,oneval ,rest ...) (let ([varname (gensym 'unused-begin)])
                            `(let ([,varname ,(desugar-aux oneval)])
                               ,(desugar-begin rest)))]))

(define (desugar-call/cc e)
  (let ([kvar (gensym 'k)] [oldstack (gensym 'oldstack)] [xvar (gensym 'x)])
    `(call/cc
      ,(desugar-aux
        `(lambda (,kvar)
           (,e (let ([,oldstack %wind-stack])
                 (lambda (,xvar)
                   (begin
                     (unless (eq? ,oldstack %wind-stack)
                       (%do-wind ,oldstack))
                     (,kvar ,xvar))))))))))

; wraps the expression in the runtime functions needed for various capabilities.
(define (wrap e)
  `(let* ([promise? (lambda (p?) (and (vector? p?)
                                      (eq? (vector-length p?) '3)
                                      (eq? (vector-ref p? '0) '%%promise)))]
          [%raise-handler (lambda (%%uncaught-raise-arg) (begin (print '"Uncaught Exception: ")
                                                                (print %%uncaught-raise-arg)
                                                                (print '"\n")
                                                                (halt (void))))]
          [%wind-stack '()]
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
                         (begin (set! %wind-stack (cdr st)) ((cdr (car st))) (loop (cdr st)))))
                     ; wind up the new stack, calling pre-thunks as we go up
                     (let loop ([st new-stack])
                       (unless (eq? st tail)
                         (begin (loop (cdr st)) ((car (car st))) (set! %wind-stack st)))))))))]
          [dynamic-wind (lambda (pre body post)
                          (begin
                            (pre)
                            (set! %wind-stack (cons (cons pre post) %wind-stack))
                            (let ([val (body)])
                              (begin (set! %wind-stack (cdr %wind-stack))
                                     (post)
                                     val))))])
     ,e))

; made for ez debugging ;)
(define d desugar-aux)
(define (ed e) (eval-ir (desugar e)))