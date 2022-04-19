
(+
 '1
 (call/cc
  (lambda (final)
    (let ((z '1))
      (let ((cc (call/cc (lambda (u) (u u)))))
        (begin
          (guard
           (x
            ((> '1 '0) (begin (set! z (* z z)) (final (+ x z))))
            (else (final x)))
           (call/cc
            (lambda (fall)
              (let* ((next
                      (lambda (raise-k)
                        (guard
                         (x (else (begin (set! z (* z '3)) (raise (+ z '2)))))
                         (if (> z '1000) (raise-k z) '1))))
                     (_ (raise (call/cc (lambda (k) (begin (next k) (fall (promise? z))))))))
                (set! z (+ z '5))))))
          (cc
           (letrec ()
             (let* ()
               (letrec* ()
                        (guard
                         (y (y))
                         (begin
                           (set! z (* '3 (+ '1 z)))
                           (raise (call/cc (lambda (u) (u u))))))))))))))))


