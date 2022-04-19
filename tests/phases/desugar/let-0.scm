
(let ([a '2]
      [b '3])
  (let ([a b]
        [b a])
    (+ a b)))

