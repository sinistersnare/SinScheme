;#lang racket

; Functional sodoku solver (constraint propagation and depth-first search)
; Thomas Gilray

; Allows arbitrary grouping for an 9x9 grid (i.e. 3x3, other block shapes, "jigsaw sodoku", etc)
; Sets up 3x3 blocks
(define blocks (set (set (cons 0 0) (cons 1 0) (cons 2 0) (cons 0 1) (cons 1 1) (cons 2 1) (cons 0 2) (cons 1 2) (cons 2 2))
                    (set (cons 3 0) (cons 4 0) (cons 5 0) (cons 3 1) (cons 4 1) (cons 5 1) (cons 3 2) (cons 4 2) (cons 5 2))
                    (set (cons 6 0) (cons 7 0) (cons 8 0) (cons 6 1) (cons 7 1) (cons 8 1) (cons 6 2) (cons 7 2) (cons 8 2))
                    (set (cons 0 3) (cons 1 3) (cons 2 3) (cons 0 4) (cons 1 4) (cons 2 4) (cons 0 5) (cons 1 5) (cons 2 5))
                    (set (cons 3 3) (cons 4 3) (cons 5 3) (cons 3 4) (cons 4 4) (cons 5 4) (cons 3 5) (cons 4 5) (cons 5 5))
                    (set (cons 6 3) (cons 7 3) (cons 8 3) (cons 6 4) (cons 7 4) (cons 8 4) (cons 6 5) (cons 7 5) (cons 8 5))
                    (set (cons 0 6) (cons 1 6) (cons 2 6) (cons 0 7) (cons 1 7) (cons 2 7) (cons 0 8) (cons 1 8) (cons 2 8))
                    (set (cons 3 6) (cons 4 6) (cons 5 6) (cons 3 7) (cons 4 7) (cons 5 7) (cons 3 8) (cons 4 8) (cons 5 8))
                    (set (cons 6 6) (cons 7 6) (cons 8 6) (cons 6 7) (cons 7 7) (cons 8 7) (cons 6 8) (cons 7 8) (cons 8 8))))


; Define row-groups, columns-groups, and the set of peer-groups overall
(define rows (foldl (lambda (row rows) (set-add rows (foldl (lambda (col g) (set-add g (cons col row))) (set) '(0 1 2 3 4 5 6 7 8)))) (set) '(0 1 2 3 4 5 6 7 8)))
(define cols (foldl (lambda (col cols) (set-add cols (foldl (lambda (row g) (set-add g (cons col row))) (set) '(0 1 2 3 4 5 6 7 8)))) (set) '(0 1 2 3 4 5 6 7 8)))
(define groups (set-union blocks rows cols))


; Hashmap indicating a list of peer cells for each given cell
(define peers (let ([peersets (foldl (lambda (group peersets)
                                       (foldl (lambda (k peersets)
                                                (hash-set peersets k (set-union group (hash-ref peersets k (lambda () (set))))))
                                              peersets
                                              (set->list group)))
                                     (hash)
                                     (set->list groups))])
                (foldl (lambda (k h)
                         (hash-set h k (set->list (set-remove (hash-ref peersets k) k))))
                       (hash)
                       (hash-keys peersets))))


; Solves a sodoku given in its list representation
(define (solve sodoku)
  ; Turns a list-of-lists representation into a hash-based equivalent
  (define (hashify-grid sodoku)
    (let ([h (foldl (lambda (row y h)
                      (foldl (lambda (n x h)
                               (hash-set h (cons x y) (if (= n 0) (set 1 2 3 4 5 6 7 8 9) (set n))))
                             h
                             row
                             '(0 1 2 3 4 5 6 7 8)))
                    (hash)
                    sodoku
                    '(0 1 2 3 4 5 6 7 8))])
      (foldl (lambda (k h)
               (if (= 1 (set-count (hash-ref h k)))
                   (assign-cell h k (set-first (hash-ref h k)))
                   h))
             h
             (hash-keys h))))

  ; Assigns a cell and propagates constraints
  (define (assign-cell h k n)
    (foldl (lambda (peer h)
             (let* ([s (hash-ref h peer)]
                    [s+ (set-remove s n)]
                    [h+ (hash-set h peer s+)])
               (if (and (> (set-count s) (set-count s+)) (= 1 (set-count s+)))
                   (assign-cell h+ peer (set-first s+))
                   h+)))
           (hash-set h k (set n))
           (hash-ref peers k)))

  ; Returns false if there is a key mapping to the empty-set
  (define (dead-end? h)
    (foldl (lambda (k b)
             (if (> (set-count (hash-ref h k)) 0)
                 b
                 #t))
           #f
           (hash-keys h)))

  ; Selects a key or returns false if a solution has already been found
  (define (select-key h)
    (foldl (lambda (k m)
             (define sk (hash-ref h k))
             (define sm (hash-ref h m (lambda () (set 1 2 3 4 5 6 7 8 9 0))))
             (if (and (> (set-count sk) 1) (< (set-count sk) (set-count sm))) k m))
           #f
           (hash-keys h)))

  ; Performs a depth-first search
  (define (search h)
    (define k (select-key h))
    (if k
        (foldl (lambda (n soln)
                 (if soln
                     soln
                     (let ([h+ (assign-cell h k n)])
                       (if (dead-end? h+)
                           #f
                           (search h+)))))
               #f
               (set->list (hash-ref h k)))
        h))

  ; Hashify and solve
  (search (hashify-grid sodoku)))


; Displays a sodoku grid ("_" where there are multiple possible solutions)
(define (print-sodoku h)
  (display "\n")
  (display "'(\n")
  (map (lambda (row)
         (display "(")
         (map (lambda (col)
                (let ([s (hash-ref h (cons col row))])
                  (if (= 1 (set-count s))
                      (begin (display " ") (display (set-first s)))
                      (display " _"))))
              '(0 1 2 3 4 5 6 7 8))
         (display ")\n"))
       '(0 1 2 3 4 5 6 7 8))
    (display ")\n\n"))



;;; Define sodoku problems to solve
; An easy one... constraint propagation alone does the trick (http://en.wikipedia.org/wiki/Sudoku)
(define sodoku1 '((5 3 0 0 7 0 0 0 0)
                  (6 0 0 1 9 5 0 0 0)
                  (0 9 8 0 0 0 0 6 0)
                  (8 0 0 0 6 0 0 0 3)
                  (4 0 0 8 0 3 0 0 1)
                  (7 0 0 0 2 0 0 0 6)
                  (0 6 0 0 0 0 2 8 0)
                  (0 0 0 4 1 9 0 0 5)
                  (0 0 0 0 8 0 0 7 9)))


; "Worlds hardest sudoku" by Dr Arto Inkala (http://www.winatsudoku.com/sudoku-worlds-hardest.html)
(define sodoku2 '((0 0 5 3 0 0 0 0 0)
                  (8 0 0 0 0 0 0 2 0)
                  (0 7 0 0 1 0 5 0 0)
                  (4 0 0 0 0 5 3 0 0)
                  (0 1 0 0 7 0 0 0 6)
                  (0 0 3 2 0 0 0 8 0)
                  (0 6 0 5 0 0 0 0 9)
                  (0 0 4 0 0 0 0 3 0)
                  (0 0 0 0 0 9 7 0 0)))


; An empty / unconstrained sodoku
(define sodoku3 '((0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0)))




;(print-sodoku (solve sodoku1))
(print-sodoku (solve sodoku2))
;(print-sodoku (solve sodoku3))

