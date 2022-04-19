(let ([v (vector '0 '1 '2)])
  (+
   (guard (_ [else '1])
          (vector-ref))
   (guard (_ [else '1])
          (vector-ref v '13))
   (guard (_ [else '1]) ; not enough args
          (apply vector-ref '()))
   (guard (_ [else '1]) ; too many args
          (apply vector-ref '('1 '2 '3 '4)))
   (apply vector-ref (list v '1))
   (vector-ref v '0)
   (vector-ref v '1)
   (vector-ref v '2)
   (guard (_ [else 'BAD])
          (vector-ref v '1))
   ; taken from passing-vector-general
   (let ([v2 (vector '0 '1 '2 '3 '4 '5)])
     (let ([a (vector-ref v2 '1)])
       (let ([b (vector-ref v2 '5)])
         (let ([l (vector-length v2)])
           (let ([sm (+ a b l)])
             (let ([unused (vector-set! v2 '0 sm)])
               (vector-ref v2 '0)))))))))
