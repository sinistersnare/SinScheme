(let loop ([m '1] [n '2])
  (if (> m '1000)
      '()
      (cons m (loop n (+ m n)))))
