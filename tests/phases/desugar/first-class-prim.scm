

(- (foldl + '0 '(1 2 3 4)) (car (map - '(2))) ((lambda (x f y) (f x y)) '2 * '3))


