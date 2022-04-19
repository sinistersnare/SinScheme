#lang racket

(provide ssa-convert)

(require (only-in "utils.rkt" symbol-append))

; This file converts the output of `sym-anf-convert`
; and turns it into SSA form!
; One main thing it does is remove any branching
; (`if`, applications) from let.
; It now runs the branch(es), binds the value,
; and then uses a phi-node (if necessary) to join the branch(es).
; also removed (in a middle-phase) is `apply`, by turning all function
; calls into apply.

; sym-anf-convert =>

; e ::= (let ([x e]) e)
;     | (apply x x)
;     | (prim op x ...)
;     | (apply-prim op x)
;     | (if x e e)
;     | (call/cc x)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (quote dat)
;     | x
;     | (x x ...)


; toplevel-calls =>

; e ::= (let ([x l]) e)
;     | (if x e e)
;     | c
; l ::= (lambda (x ...) e)
;     | (lambda x e)
;     | (quote dat)
;     | (prim op x ...)
;     | (apply-prim op x)
;     | e
; c ::= x
;     | (apply x x)
;     | (call/cc x)
;     | (x x ...)


; TODO: this phase-name sucks.
; This function takes us one step closer to SSA by partitioning the grammar so that
; the non-stack-extending things (atomic expressions sans symbols, and prim applications)
; can only be realized through `let` bindings. So top level expressions are only
; function calls, symbols (both of these act as return points),
; `if` branches, and `let` bindings.
(define (toplevel-calls e)
  (define (process-single e)
    (match e
      [`(lambda ,xs ,elambody) `(lambda ,xs ,(toplevel-calls elambody))]
      [`(if ,xc ,et ,ef) `(if ,xc ,(toplevel-calls et) ,(toplevel-calls ef))]
      [(or (? symbol?) `(quote ,_) `(call/cc ,_) `(apply ,_ ,_)
           `(prim ,_ . ,_) `(apply-prim ,_ ,_) `(,_ . ,_)) e]))
  (match e
    [`(let ([,x ,ebnd]) ,ebody)
     `(let ([,x ,(process-single ebnd)]) ,(toplevel-calls ebody))]
    [(or `(lambda ,_ ,_) `(quote ,_) `(prim ,_ . ,_) `(apply-prim ,_ ,_))
     ; these need to be put into a let binding! and then returned!
     (define admin-bnd (gensym 'admin-toplevel-bnd))
     `(let ([,admin-bnd ,(process-single e)]))]
    [(or (? symbol?) `(if ,_ ,_ ,_) `(apply ,_ ,_) `(call/cc ,_) `(,_ . ,_))
     ; return points!
     (process-single e)]))

; TODO: Need a pass to remove `if` and `let` from let bindings, so they are 1-step computable.
;       i.e. only a atomic-expr or call, not nested things (the `if` or `let`).
;       Removing `if` I think necessitates phi-nodes...
;       And `let` I think is fine as long as the program is alphatized.

; simplify-let =>

; e ::= r
;     | (let ([x l]) e)
;     | (if x e e)
;     | (cond-bind x x e e e)
; l ::= r
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (quote dat)
;     | (prim op x ...)
;     | (apply-prim op x)
; r ::= x
;     | (apply x x)
;     | (call/cc x)
;     | (x x ...)

; this phase removes `if` and `let` from `let` binding-exprs.
; This way, `let` expressions are always very simple 1-step things,
; either a prim call, an atomic-eval, or a call.
; to remove `if`, we add a special branch-then-join construct, `cond-bind`
;   which is the new-binding, the condition, the true then false branches, then the join point.
; after this, we are effectively in SSA form, if we interpret `cond-bind` generously.
(define (simplify-let e)
  (match e
    [`(let ([,x (let ([,y ,e0]) ,e1)]) ,e2)
     ; I am 83% sure this transformation is OK because everything is alphatized!
     (simplify-let `(let ([,y ,e0]) (let ([let ([,x ,e1])]) ,e2)))]
    [`(let ([,x (if ,xc ,et ,ef)]) ,eb)
     ; use this special construct that binds `x` based on `xc`, then
     ; executes `eb`.
     `(cond-bind ,x ,xc ,(simplify-let et) ,(simplify-let ef) ,(simplify-let eb))]
    [`(let ([,x (lambda ,xs ,elambody)]) ,ebody)
     `(let ([,x (lambda ,xs ,(simplify-let elambody))]) ,(simplify-let ebody))]
    [`(let ([,x ,xbnd]) ,ebody)
     `(let ([,x ,xbnd]) ,(simplify-let ebody))]
    [(or (? symbol?) `(apply ,_ ,_) `(call/cc ,_) `(,_ . ,_)) e]))


; unify-lambdas =>

; e ::= r
;     | (let ([x l]) e)
;     | (if x e e)
;     | (cond-bind x x e e e)
; l ::= r
;     | (lambda (x) e)
;     | (quote dat)
;     | (prim op x ...)
;     | (apply-prim op x)
; r ::= x
;     | (call/cc x)
;     | (x x)

; turn (λ (xs ...) e) -> (λ xs e) -> (λ (xs) e)
; turn (λ xs e) -> (λ (xs) e)
; AKA turn every λ into a 1-arg lambda, that takes an explicit list.
; turn (x x ...) -> (x x)
; by explicitly cons'ing together a list and giving it to the function.
; by doing this, we remove the `apply` form (by making everything apply!).
(define (unify-lambdas e)
  (define (process-r r)
    (match r
      [(? symbol? x) x]
      [`(apply ,xf ,xx)
       `(,xf ,xx)]
      [`(call/cc ,xf)
       `(call/cc ,xf)]
      [`(,xf . ,xs)
       (define (layout-callsite xf todo argsname cur)
         ; to create a list in CPS, we need to construct
         ; each cons-cell 1-by-1. All the appending is just
         ; to make nice arg-names like 'argsXXXX-fname-N
         ; so we can more easily see how far into the args list we are.
         (match todo
           ['() `(,xf ,(symbol-append argsname cur))]
           [`(,hd . ,tl)
            `(let ([,(symbol-append argsname (add1 cur))
                    (prim cons ,hd ,(symbol-append argsname cur))])
               ,(layout-callsite xf tl argsname (add1 cur)))]))
       (define argsname (symbol-append (gensym 'args) '$ xf '$))
       `(let ([,(symbol-append argsname 0) '()])
          ,(layout-callsite xf (reverse xs) argsname 0))]))
  (define (process-l l)
    (match l
      [`(lambda (,xs ...) ,elambody)
       (define (listify-lam elambody todo base-restarg cur)
         (define cur-restarg (symbol-append base-restarg cur))
         (define next-restarg (symbol-append base-restarg (add1 cur)))
         (match todo
           ['() (unify-lambdas elambody)]
           ; special case so we dont have a useless binding at the end
           [`(,h)
            `(let ([,h (prim car ,cur-restarg)]) ,(unify-lambdas elambody))]
           [`(,hd . ,tl)
            `(let ([,hd (prim car ,cur-restarg)])
               (let ([,next-restarg (prim cdr ,cur-restarg)])
                 ,(listify-lam elambody tl base-restarg (add1 cur))))]))
       (define base-restarg (symbol-append (gensym 'arglist)))
       (define restarg-0 (symbol-append base-restarg 0))
       `(lambda (,restarg-0) ,(listify-lam elambody xs base-restarg 0))]
      [`(lambda ,xvararg ,elambody)
       `(lambda (,xvararg) ,(unify-lambdas elambody))]
      [(or `(quote ,_) `(prim ,_ . ,_) `(apply-prim ,_ ,_)) l]
      [r (process-r r)]))
  (match e
    [`(let ([,x ,l]) ,ebody)
     `(let ([,x ,(process-l l)]) ,(unify-lambdas ebody))]
    [`(if ,xc ,et ,ef)
     `(if ,xc ,(unify-lambdas et) ,(unify-lambdas ef))]
    [`(cond-bind ,x ,xc ,et ,ef ,ejoin)
     `(cond-bind ,x ,xc ,(unify-lambdas et) ,(unify-lambdas ef) ,(unify-lambdas ejoin))]
    [r (process-r r)]))

; our SSA form is a much simplified version that doesn't explicitly include phi nodes...
; it still is expression oriented, so I didn't need to have phi,
; I just made this `cond-bind` form to make it explicit.
(define (ssa-convert anf)
  (unify-lambdas (simplify-let (toplevel-calls anf))))
