

(let ([list (lambda (x) x)]
      [foldr (lambda (x) x)]
      [map (lambda (x) x)]
      [append (lambda (x) x)]
      [- (lambda (x) x)]
      [* (lambda (x) x)]
      [car (lambda (x) x)]
      [cons (lambda (x) x)]
      [cdr (lambda (x) x)]
      [member (lambda (x) x)]
      [eq? (lambda (x) x)]
      [not (lambda (x) x)]
      [promise? (lambda (x) x)]
      [vector-ref (lambda (x) x)]
      [vector (lambda (x) x)]
      [make-vector (lambda (x) x)]
      [vector-set! (lambda (x) x)]
      [reverse (lambda (x) x)])
  (+ (force (delay (force (delay (foldl + '0 '(1 2 3 4 5 6))))))
     (guard (x [else (+ x x)]) (+ '1 (+ '4 (raise '3))))))
