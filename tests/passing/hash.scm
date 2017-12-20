(define h (hash))

(define c0 (hash-count h))

(define hh (hash-set h "a" 1))

(define c1 (hash-count hh))

(define hs (hash "a" 1 "b" 2 "c" 3 "d" 4))

(define cs (hash-count hs))

(define d (hash-ref hs "d"))

(define b1 (if (hash? h) 12 13))
(define b2 (if (hash? #f) 12 13))

(+ c0 c1 cs d b1 b2)