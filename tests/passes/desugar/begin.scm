(let [(n '1)]
  (begin
    (set! n (+ n '1))
    (set! n (* '2 n))
    (set! n (* n n n))
    n))
