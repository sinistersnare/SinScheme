(letrec ([ccstack '()]
         [fail (lambda ()
                 (if (null? ccstack)
                     (raise 'no-solution)
                     (let ([next-cc (car ccstack)])
                       (begin
                         (set! ccstack (cdr ccstack))
                         (next-cc next-cc)))))]
         [assert (lambda (b) (if b '#t (fail)))]
         [amb (lambda (lst) (let ([cc (call/cc (lambda (u) (u u)))])
                              (if (null? lst)
                                  (fail)
                                  (let ([head (car lst)])
                                    (begin
                                      (set! lst (cdr lst))
                                      (set! ccstack (cons cc ccstack))
                                      head)))))])

  (let ([a (amb `(2 3 4 5))]
        [b (amb `(2 3 4 5 6))]
        [c (amb `(2 3 4 5))])
    (begin

      (assert (= (+ (* a a) (* b b)) (* c c)))

      `(solution ,a ,b ,c))))

