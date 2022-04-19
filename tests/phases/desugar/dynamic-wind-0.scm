

(let ([x '1] [y '0])
  (let ([k (dynamic-wind
               (lambda () (set! x (* '3 (+ '3 x))))
               (lambda () (dynamic-wind
                              (lambda () (set! x (* '3 (+ '1 x))))
                              (lambda () (dynamic-wind
                                             (lambda () (set! x (* '5 (+ '1 x))))
                                             (lambda () (call/cc (lambda (u) (u u))))
                                             (lambda () (set! x (* '11 (+ '1 x))))))
                              (lambda () (set! x (* '7 (+ '2 x))))))
               (lambda () (set! x (* '3 (+ '2 x)))))])
    (begin
      (set! y (+ y '1))
      (dynamic-wind
          (lambda () (set! x (* '3 (- x '12303))))
          (lambda () (dynamic-wind
                         (lambda () (set! x (* '3 (- x '5311))))
                         (lambda () (if (< y '2) (k k) x))
                         (lambda () (set! x (* '3 (- x '1137))))))
                    (lambda () (set! x (* '3 (- x '12271))))))))
