
(let ((box '1))
  (let ((x
         (* (and '3)
            (if (and) '5 '7)
            (if (and '3 '11) (and '3 '11) '2)
            (or '7 '13)
            (if (or) '2 '7)
            (or '11 (begin (set! box (* '3 box)) '77))
            (begin (and (> '5 '3) (set! box (* '3 box)) '#f (begin (set! box (* '3 box)) '1)) '2)
            (begin (and '#f (begin (set! box (* '3 box)) '2)) '1))))
    (+ x box)))
