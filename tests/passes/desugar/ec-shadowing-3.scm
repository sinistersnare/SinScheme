


((lambda (let letrec)
   ((lambda (if cond prim)
      (guard (z (else (* z z))) (* '5 (raise (force (delay (call/cc (lambda (k) (dynamic-wind
                                                                                    (lambda () '9)
                                                                                    (k (+ (if) (letrec (prim))))
                                                                                    (lambda () (set! cond (lambda () (+ '1 (cond))))))))))))))
    (lambda () '3)
    (lambda () '5)
    (lambda () '7)))
 (lambda (a . b) b)
 (lambda x (car x)))

