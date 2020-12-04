



(letrec ()
  (letrec* ([x '1])
           (letrec ([a (set! x (+ x '3))]
                    [b (set! x (* x '2))])
             (letrec* ([a (set! x (+ x '3))]
                       [b (set! x (* x '2))])
               (let* ([a (set! x (+ x '3))]
                      [b (set! x (* x '2))])
                 (let ([e '2])
                   (letrec* ([e '5]
                             [f e])
                            (+ x f))))))))
