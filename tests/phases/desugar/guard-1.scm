(list (+ '1 (guard (_ ('6)) '3))
      (+ '8 (guard (x ((x '9 '3))) (raise -)))
      (+ '7 (guard (_ ('6)) (raise '1))))
