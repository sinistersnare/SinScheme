(+
 (apply (lambda (x y) (- y x)) '(4 10))
 ((apply (lambda (u)
          (lambda (v w)
            (* u (+ v w))))
        '(4))
  '5 '6))
