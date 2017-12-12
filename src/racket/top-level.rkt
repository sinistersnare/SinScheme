#lang racket

(provide top-level)

(require "utils.rkt")

; By Davis Silverman

(define (top-level e)
  (define (layout-bodies bodies)
    (define (get-bindings bodies)
      (match bodies
        [(? empty?) '()]
        [`((define (,fname ,params ...) ,ebodies ...) . ,tail)
         `((,fname ,(T `(lambda ,params (begin ,@ebodies)))) ,@(get-bindings tail))]
        [`((define (,fname ,argnames ... . ,restvarname) ,ebodies ...) . ,tail)
         `((,fname ,(T `(lambda (,@argnames . ,restvarname) (begin ,@ebodies)))) ,@(get-bindings tail))]
        [`((define ,name ,exp) . ,tail)
         `((,name ,(T exp)) ,@(get-bindings tail))]
        [`((begin ,newbodies ...) . ,tail)
         (get-bindings (append newbodies tail))]
        [`(,head . ,tail)
         (define binding (gensym 'begin-binding))
         `((,binding ,(T head)) ,@(get-bindings tail))]
        [else
         (displayln (format "COULDNT FIGURE OUT WHAT BINDING WAS! ~s" bodies))
         'BAD-GET-BODIES-OUTPUT]))
    (define bindings (get-bindings bodies))
    (match-define `(,last-binding ,_last-val) (last bindings))
    `(letrec* ,bindings ,last-binding))
  (define (layout-lettype lettype bindings bodies)
    (define (bind bind+e)
      (match-define `(,binding ,exp) bind+e)
      `(,binding ,(T exp)))
    `(,lettype ,(map bind bindings) ,(layout-bodies bodies)))
  (define (layout-lambda params bodies)
    (define (separate-params params) ;; TODO: test this
      (foldr (lambda (el acc)
               (match-define `(,pos ,def) acc)
               (match el
                 [`(,(? symbol? var) ,edefault)
                  `(,pos ((,var ,(T edefault)) ,@def))]
                 [(? symbol? var) (list (cons var pos) def)]))
             `(() ()) params))
    ; layout-regular for regular lambda (lambda (a b c) c)
    ; layout-default for default arguments (lambda (a b c [d 1]) d)
    (define (layout-regular pos bodies)
      `(lambda ,pos ,(layout-bodies bodies)))
    (define (layout-default pos defaults bodies)
      (define (layout-case amt defrestname defaults bodies)
        (define (layout-given-default-args defrestname given body)
          (match given
            [(? empty?) body]
            [`(,head . ,tail)
             (define nextrestname (gensym 'rest-of-defaults))
             `(let* ([,head (car ,defrestname)] [,nextrestname (cdr ,defrestname)])
                ,(layout-given-default-args nextrestname tail body))]))
        (define-values (given notgiven) (split-at defaults amt))
        `[[,amt]
          ,(layout-given-default-args
            defrestname (map car given)
            `(let* ,notgiven ,(layout-bodies bodies)))])
      (define defaultvarargsname (gensym 'defaultvarargs))
      `(lambda (,@pos . ,defaultvarargsname)
         (case (length ,defaultvarargsname)
           ,@(append
              (map (lambda (n) (layout-case n defaultvarargsname defaults bodies))
                   (build-list (add1 (length defaults)) values))
              '([else (raise '"Too many arguments given!")])))))
    (define (layout params bodies)
      (match-define `(,pos ,default) (separate-params params))
      (if (= 0 (length default))
          (layout-regular pos bodies)
          (layout-default pos default bodies)))
    (layout params bodies))
  (define (layout-cond clauses) ;; TEST
    (define (layout clause)
      (match clause
        [`(else ,e ,es ...)
         `(else ,(layout-bodies (cons e es)))]
        [`(,e0 ,e1 ,es ...)
         `(,(T e0) ,(layout-bodies (cons e1 es)))]
        [`(,e0) `(,(T e0))]))
    (map layout clauses))
  (define (layout-case exp clauses) ;; TEST
    (define (layout clause)
      (match clause
        [`((,(? datum? ds) ...) ,e0 ,es ...)
         `(,ds ,(layout-bodies (cons e0 es)))]
        [`(else ,e0 ,es ...)
         `(else ,(layout-bodies (cons e0 es)))]))
    `(case ,(T exp) ,@(map layout clauses)))
  (define (layout-qq qq level)
    ; (displayln (format "matching... ~s" qq))
    (match qq
      [(? empty?) ''()]
      [(list 'quasiquote e)
       ; (displayln (format "QuasiQuote:: ~s" e))
       `(list 'quasiquote ,(layout-qq e (add1 level)))]
      [(list 'unquote e)
       ; (displayln (format "Unquote:: ~s" e))
       (cond
         [(<= level 0) (raise (format "CANT UNQUOTE MORE! level ~s" level))]
         [(= 1 level) (T e)]
         [else (list 'list ''unquote (layout-qq e (sub1 level)))])]
      [`(,head . ,tail)
       ; (displayln (format "cons:: ~s" head))
       `(cons ,(layout-qq head level) ,(layout-qq tail level))]
      [else ; single element that we need to quote.
       ; (displayln (format "outerElse:: ~s" qq))
       (list 'quote qq)]))
  ; FIXME: will not work with side-effect matchvars!
  ; (match (set! a b) clauses) will fail!
  (define (layout-match e clauses)
    (define (layout pairing)
      (match pairing
        [`(, conditions ,bindings ,bodies) `((and ,@conditions) (let ,bindings ,(layout-bodies bodies)))]
        [else (displayln (format "BAD PAIRING!!! ~s" pairing)) 'BADBADBADBADBADBADBADBADBADBADBAD]))
    (define (get-conditions clause matchvar)
      (match clause
        ['else '()]
        [(? natural? n) `((equal? ,(T n) ,(T matchvar)))]
        [(? string? s) `((equal? ,(T s) ,(T matchvar)))]
        [''() `((null? ,(T matchvar)))]
        [`(quote ,(? cons? quotedlist))
         `((equal? ,(list 'quote quotedlist) ,(T matchvar)))]
        [`(cons ,p1 ,p2)
         `((cons? ,matchvar) ,@(get-conditions p1 `(car ,matchvar)) ,@(get-conditions p2 `(cdr ,matchvar)))] ; TODO make sure this works
        [#t `((equal? '#t ,(T matchvar)))] ; TODO make sure this works
        [#f `((equal? '#f ,(T matchvar)))] ; TODO make sure this works
        [`',dat `((equal? ,dat ,matchvar))] ; TODO make sure this works
        [(? symbol? x) `()] ; TODO make sure this works
        [`(? ,predicate ,otherpat) `((,predicate ,matchvar) ,@(get-conditions otherpat matchvar))] ; TODO make sure this works
        [`(quasiquote ,qqpat) (get-conditions (layout-qq qqpat 1) matchvar)]))
    (define (get-bindings clause matchvar)
      (match clause
        ['else '()]
        [(or (? natural?) (? string?) #t #f) '()]
        [`',dat '()] ; idk why this cant be in or clause
        [(? symbol? x) `([,(T x) ,(T matchvar)])]
        [`(? ,predicate ,otherpat) (get-bindings otherpat matchvar)]
        [`(cons ,p1 ,p2) `(,@(get-bindings p1 `(car ,(T matchvar))) ,@(get-bindings p2 `(cdr ,(T matchvar))))]
        [`(quasiquote ,qqpat) (get-bindings (layout-qq qqpat 1) matchvar)]))
    (define genmatchvar (gensym 'matchvarbinding))
    (define leftsides (map car clauses))
    (define bodies (map cdr clauses))
    (define conditions (map (lambda (x) (get-conditions x genmatchvar)) leftsides))
    (define bindings (map (lambda (x) (get-bindings x genmatchvar)) leftsides))
    (define paired (map list conditions bindings bodies))
    (define no-else-clause (empty? (filter (lambda (el) (equal? el 'else)) leftsides)))
    (if no-else-clause
        `(let ([,genmatchvar ,(T e)]) (cond ,@(map layout paired)
                                            (else (raise '"no matching clause! please give ec."))))
        `(let ([,genmatchvar ,(T e)]) (cond ,@(map layout paired))))) ; TODO make sure this cond fits desugar language
  (define (T e)
    ;(displayln (format "got E:: ~s " e))
    (match e
      [`(letrec* (,bindings ...) ,ebody ,erest ...) (layout-lettype 'letrec* bindings (cons ebody erest))]
      [`(letrec (,bindings ...) ,ebody ,erest ...) (layout-lettype 'letrec bindings (cons ebody erest))]
      [`(let* (,bindings ...) ,ebody ,erest ...) (layout-lettype 'let* bindings (cons ebody erest))]
      [`(let (,bindings ...) ,ebody ,erest ...)
       (layout-lettype 'let bindings (cons ebody erest))]
      [`(lambda (,params ...) ,ebody ,erest ...) (layout-lambda params (cons ebody erest))]
      [`(lambda (,named ,restnames ... . ,restvar) ,ebody ,erest ...)
       `(lambda (,named ,@restnames . ,restvar) ,(layout-bodies (cons ebody erest)))]
      [`(lambda ,restvar ,ebody ,erest ...) `(lambda ,restvar ,@(map T (cons ebody erest)))]
      [`(dynamic-wind ,epre-thunk ,evalue-thunk ,epost-thunk)
       `(dynamic-wind ,(T epre-thunk) ,(T evalue-thunk) ,(T epost-thunk))]
      [`(guard (,x ,cond-clauses ...) ,ebody ,erest ...)
       `(guard (,x ,@(layout-cond cond-clauses)) ,(layout-bodies (cons ebody erest)))]
      [`(raise ,eraiseval) `(raise ,(T eraiseval))]
      [`(delay ,edelayval) `(delay ,(T edelayval))]
      [`(force ,eforceval) `(force ,(T eforceval))]
      [`(and ,andvals ...) `(and ,@(map T andvals))]
      [`(or ,orvals ...) `(or ,@(map T orvals))]
      [`(match ,matchexp ,matchclauses ...) (layout-match matchexp matchclauses)]
      [`(cond ,condclauses ...) `(cond ,@(layout-cond condclauses))]
      [`(case ,exp ,caseclauses ...) (layout-case exp caseclauses)]
      [`(if ,econd ,et ,ef) `(if ,(T econd) ,(T et) ,(T ef))]
      [`(when ,econd ,whenexp ,restexps ...) `(when ,(T econd) ,(layout-bodies (cons whenexp restexps)))]
      [`(unless ,econd ,unlessexp ,restexps ...) `(unless ,(T econd) ,(layout-bodies (cons unlessexp restexps)))]
      [`(set! ,varname ,exp) `(set! ,varname ,(T exp))]
      [`(define ,args ,ebody ,erest ...)
       (displayln (format "WTF GOT TO A DEFINE:: ~s" args))
       'BADBADBAD]
      [`(begin ,ebody ,erest ...)
       (layout-bodies (cons ebody erest))]
      [`(call/cc ,exp)
       `(call/cc ,(T exp))]
      [`(apply ,efun ,eargs)
       `(apply ,(T efun) ,(T eargs))] ;; HELP how to top-level a list? will that be in the quote match?
      [`',dat
       ; (displayln (format "got dat: ~s" dat))
       e]
      [`(quasiquote ,qdat) (layout-qq qdat 1)]
      [(? prim? op) op] ;; HELP does this work? TEST
      [(? symbol? x) x] ;; HELP does this work? TEST
      [(? natural? nat) `',nat] ;; HELP does this work?
      [(? string? str) `',str] ;; HELP does this work?
      [#t ''#t] ;; HELP does this work?
      [#f ''#f] ;; HELP does this work?
      #;[(? datum? d)
       (displayln (format "COULD NOT FIGURE OUT HOW TO QUOTE DATUM ~s" d))
       `',d] ; HELP: why doesnt this work for all datums?
      [`(,ef ,eargs ...) `(,(T ef) ,@(map T eargs))]))
  (T e))
