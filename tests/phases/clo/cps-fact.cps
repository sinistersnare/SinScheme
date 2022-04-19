(let ((arg5255 (lambda (cont5247 SYf$u) (SYf$u cont5247 SYf$u))))
  (let ((arg5254
         (lambda (_5242 a5237)
           (let ((arg5260
                  (lambda (_5241 M3b$fact)
                    (let ((arg5263
                           (lambda (_0 x) (let ((_1 (prim halt x))) (_1 _1)))))
                      (let ((arg5262 '5)) (M3b$fact arg5263 arg5262))))))
             (let ((arg5259
                    (lambda (cont5243 htO$fact)
                      (let ((arg5269 '0))
                        (let ((arg5268
                               (lambda (cont5244 qiK$n)
                                 (let ((arg5271 '0))
                                   (let ((a5238 (prim = qiK$n arg5271)))
                                     (if a5238
                                       (let ((arg5274 '0))
                                         (let ((arg5273 '1))
                                           (cont5244 arg5274 arg5273)))
                                       (let ((arg5276 '1))
                                         (let ((a5239 (prim - qiK$n arg5276)))
                                           (let ((arg5279
                                                  (lambda (_5245 a5240)
                                                    (let ((retprim5246
                                                           (prim
                                                            *
                                                            qiK$n
                                                            a5240)))
                                                      (let ((arg5284 '0))
                                                        (cont5244
                                                         arg5284
                                                         retprim5246))))))
                                             (htO$fact arg5279 a5239))))))))))
                          (cont5243 arg5269 arg5268))))))
               (a5237 arg5260 arg5259))))))
    (let ((arg5253
           (lambda (cont5248 BEk$y)
             (let ((arg5287 '0))
               (let ((arg5286
                      (lambda (cont5249 HaX$f)
                        (let ((arg5289
                               (lambda (cont5250 FDK$x)
                                 (let ((arg5293
                                        (lambda (_5251 a5235)
                                          (let ((arg5296
                                                 (lambda (_5252 a5236)
                                                   (a5236 cont5250 FDK$x))))
                                            (a5235 arg5296 HaX$f)))))
                                   (BEk$y arg5293 BEk$y)))))
                          (HaX$f cont5249 arg5289)))))
                 (cont5248 arg5287 arg5286))))))
      (arg5255 arg5254 arg5253))))
