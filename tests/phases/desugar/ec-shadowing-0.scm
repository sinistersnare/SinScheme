
(let ([lambda '2] [quote (lambda (x) (+ '1 x))]) (force (delay 'lambda)))
