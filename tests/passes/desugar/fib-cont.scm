


; Based on fibc.scm by Kent Dybvig
; Peano arithmatic and first-class continuations
(letrec ([nat->peano (lambda (n) (if (= '0 n) '() (cons (nat->peano (- n '1)) '())))]
         [peano->nat (lambda (n) (if (z? n) '0 (+ '1 (peano->nat (pred n)))))]
         [succ (lambda (n) (cons n '()))]
         [pred car]
         [z? (lambda (n) (null? n))]
         [addc (lambda (x y k)
                 (if (z? y)
                     (k x)
                     (addc (succ x) (pred y) k)))]
         [fibc (lambda (x c)
                 (cond
                  [(z? x)
                   (c (nat->peano '0))]
                  [(z? (pred x))
                   (c (nat->peano '1))]
                  [else
                   (addc (call/cc (lambda (c) (fibc (pred x) c)))
                         (call/cc (lambda (c) (fibc (pred (pred x)) c)))
                         c)]))])
  (fibc (nat->peano '13) (lambda (x) (peano->nat x))))


