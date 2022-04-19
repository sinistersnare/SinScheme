

(letrec ([gen (lambda (n) (cons n (delay (gen (- n '1)))))]
         [lazy-take (lambda (llst n) (if (= n '0) '() (cons (car llst) (lazy-take (force (cdr llst)) (- n '1)))))])
    (lazy-take (gen '8) '4))

