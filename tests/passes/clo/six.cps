(let ((a '4))
  (let ((b '2))
    (let ((c (lambda (d) (let ((e (prim + b d)))
    						(let ((f (prim halt e))) (f f))))))
      (c a))))
