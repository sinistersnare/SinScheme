

; This test case goes through most forms in the
; target language to ensure at least these work properly
((lambda (x y)
     (call/cc
      (lambda (ret)
        ; and then requires prims desugared as in start-0
        (+ (let () (let ([a (if (set! y (+ y '1))
                                '2
                                '4)]
                         [b '3])
                     (ret (+ y (* a b)))))
           y
           x))))
   '1
   '2)
