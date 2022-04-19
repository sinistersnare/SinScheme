

; various order of evaluation tests


(let ([x '8])


  (begin

    (if (void? (guard (x [else (set! x (- x '3))]) (cond))) (set! x (* '2 x)) (case '0))

    (or (set! x (* x '7))
        (set! x (* x '8)))

    (and (set! x (- x '1))
         (eq? '#t '#f)
         (set! x (- x '7)))

    (guard (z ((begin (set! x (+ '23 x)) (force z) (force z))))
           (raise (call/cc (lambda (k) (guard (y [(+ '1 y)]) (k (delay (begin (set! x (* '3 (+ x '1))) x))))))))
    x)

  )

