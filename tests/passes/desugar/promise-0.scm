
(let ([p (delay '3)])
  (+ (force p)
     (if (promise? p) '1 '0)))
