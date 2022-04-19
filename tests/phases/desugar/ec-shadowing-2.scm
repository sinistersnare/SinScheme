

(let ([lambda (lambda (x) x)]
      [foldr (lambda (x) x)]
      [set! (lambda (x) x)]
      [append (lambda (x) x)]
      [if (lambda (x) x)]
      [let (lambda (x) x)]
      [car (lambda (x) x)]
      [cons (lambda (x) x)]
      [letrec* (lambda (x) x)]
      [letrec (lambda (x) x)]
      [eq? (lambda (x) x)]
      [not (lambda (x) x)]
      [promise? (lambda (x) x)]
      [vector-ref (lambda (x) x)]
      [case (lambda (x) x)]
      [cond (lambda (x) x)]
      [unless (lambda (x) x)]
      [when (lambda (x) x)])
  (+ (force (delay (force (delay (foldl + '0 '(1 2 3 4 5 6))))))
     (guard (x [else (* x x)]) (+ '1 (+ '4 (raise '3))))))
