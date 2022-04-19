

(case (+ '2 '3)
  [(2 4 6 8 10) '1]
  [(a b c d e f) '2]
  [else ((lambda (x) (case x [(3 5 7) '3] [else '4])) (+ '2 '3))])

