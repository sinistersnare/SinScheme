

(let ([x '1])
  (begin
    (set! x (* (+ x '3) '5))
    (when (> x '2) (set! x (* (+ x '3) '5)))
    (unless (> x '2) (set! x (* (+ x '4) '2)))
    (when (< x '4) (set! x (* (+ x '9) '4)))
    (unless '#f (set! x (* (+ x '2) '7)))
    (if x (set! x (* x x)) '7)
    (if '#f (set! x '0) '9)
    x))
