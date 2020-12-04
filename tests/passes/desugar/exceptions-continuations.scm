

(let ([x '1])
  (letrec ([k (letrec ([f (lambda (n)
                            (guard (z [else (begin (set! x (+ x n)) (raise z))])
                                   (if (<= n '0)
                                       (raise (call/cc (lambda (u) (u u))))
                                       (f (- n '1)))))])
                (guard (x [else x]) (f '4)))])
    (if (number? k)
        (+ k x)
        (k '5))))



