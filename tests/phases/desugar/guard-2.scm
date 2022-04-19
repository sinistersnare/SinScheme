(cons (+ '1 (guard (_ (else '6)) '3))
      (+ '1
         (guard (_ (else '6)) (raise '1))
         (guard (x (else x)) (+ '1 (guard (y) (* '7 (raise '3)))))))
