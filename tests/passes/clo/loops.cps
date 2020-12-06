(let ((hh (prim make-vector '1 '1)))
   (let ((h
          (lambda (cont5728 nn)
            (let ((cnd (prim <= nn '1)))
              (if cnd
                (cont5728 '() '716)
                (let ((f (prim make-vector '1 '1)))
                  (let ((ff
                         (lambda (cont5730 x0 x1 x2 x3 x4 x5 x6 x7 x8)
                           (let ((cnd (prim = '0 x0)))
                             (if cnd
                               (cont5730 '() x1)
                               (let ((g (prim vector-ref f '0)))
                                 (g cont5730 x1 x2 x3 x4 x5 x6 x7 x8 x0)))))))
                    (let ((_ (prim vector-set! f '0 ff)))
                      (let ((gg (prim vector-ref f '0)))
                        (gg
                         (lambda (_5729 ___)
                           (let ((nnn (prim - nn '1)))
                             (let ((hhh (prim vector-ref hh '0)))
                               (hhh cont5728 nnn))))
                         '3 '4 '5 '9 '1 '4 '12 '88 '0))))))))))
     (let ((______ (prim vector-set! hh '0 h)))
       (h (lambda (_0 x) (let ((_1 (prim halt x))) (_1 _1))) '99999))))
