

(cond [(< '1 '1) '0]
      [else (cond [(< '0 '1) (cond [(void? (cond)) (cond ['#t ((lambda (a) a) '11)]
                                                          [else '5])]
                                    [else '3])]
                  [else '2])])



