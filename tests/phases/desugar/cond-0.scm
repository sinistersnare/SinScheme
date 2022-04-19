
(let ([a '6]
      [b '8])
  (cond [(> a b) 'gt]
        [(= a b) 'eq]
        [else 'lt]))

