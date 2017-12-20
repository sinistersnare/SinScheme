(define h (hash 1 2 3 4))

(define got (hash-ref h 1 123))

(define didnt (hash-ref h 2 71))

(+ got didnt)
