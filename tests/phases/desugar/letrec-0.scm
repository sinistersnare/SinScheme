(letrec ([odd? (lambda (n)
                 (if (= n '0) '#f
                     (if (= n '1) '#t
                         (even? (- n '1)))))]
         [even? (lambda (n)
                  (if (= n '0) '#t
                      (if (= n '1) '#f
                          (odd? (- n '1)))))])
  (odd? '5))
