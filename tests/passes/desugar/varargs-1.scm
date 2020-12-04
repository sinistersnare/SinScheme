




(foldl (lambda (lst f a)
         (* (apply f lst) (car (cdr lst))))
       '0
       '((0 1 2 3) (4 5 6 7) (2 5 7 9))
       ((lambda a a)
        (lambda (a b . c) (+ a b (first c) (second c)))
        (lambda (a b c d) (* (+ a b) (+ c d)))
        (lambda a (foldr + '0 a))))
