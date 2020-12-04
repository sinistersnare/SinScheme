
(+

 (foldl +
        '0
        (map (lambda (b) (if b '1 '2))
             (map promise?
                  (list
                   '#f
                   '#t
                   (delay '0)
                   (list)
                   (list '() '())
                   '#()
                   '#(0 1)
                   'yes))))
 
  (let ([x '0])
    (let ([p (delay (begin (set! x (+ '1 x)) x))])
      (let ([v (+ (force p) (force p) (force p))])
        (+ v x)))))


