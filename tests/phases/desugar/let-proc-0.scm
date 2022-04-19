(let loop ([m '1] [n '2])
  (cons m
        (cons n (cons (+ m n) '()))))
