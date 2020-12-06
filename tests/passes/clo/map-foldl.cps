((lambda (cont6074 H6m$yu) (H6m$yu cont6074 H6m$yu))
 (lambda (_5910 kXW$Ycmb)
   (kXW$Ycmb
    (lambda (_5911 b0u$%foldr1)
      (kXW$Ycmb
       (lambda (_5912 hLK$%map1)
         (kXW$Ycmb
          (lambda (_5913 RPA$%take)
            (kXW$Ycmb
             (lambda (_5914 qiP$%length)
               (kXW$Ycmb
                (lambda (_5915 DeK$%foldl1)
                  (let ((ePl$%last
                         (lambda (cont5916 Mai$lst)
                           (DeK$%foldl1
                            cont5916
                            (lambda (cont5917 h1M$x eA5$y)
                              (cont5917 '() h1M$x))
                            '()
                            Mai$lst))))
                    (let ((HUA$%drop-right
                           (lambda (cont5918 ahD$lst pYL$n)
                             (qiP$%length
                              (lambda (_5919 a5840)
                                (let ((a5841 (prim - a5840 pYL$n)))
                                  (RPA$%take cont5918 ahD$lst a5841)))
                              ahD$lst))))
                      (kXW$Ycmb
                       (lambda (_5920 Z2q$%foldr)
                         (let ((f7k$%map1
                                (lambda (cont5921 FDJ$f KfG$lst)
                                  (b0u$%foldr1
                                   cont5921
                                   (lambda (cont5922 vsG$v oWe$r)
                                     (FDJ$f
                                      (lambda (_5923 a5850)
                                        (let ((retprim5924
                                               (prim cons a5850 oWe$r)))
                                          (cont5922 '() retprim5924)))
                                      vsG$v))
                                   '()
                                   KfG$lst))))
                           (let ((Ohy$%map
                                  (lambda kQo$args5926
                                    (let ((cont5925 (prim car kQo$args5926)))
                                      (let ((kQo$args (prim cdr kQo$args5926)))
                                        (let ((N96$f (prim car kQo$args)))
                                          (let ((aFW$lsts (prim cdr kQo$args)))
                                            (let ((a5854
                                                   (prim cons '() aFW$lsts)))
                                              (let ((a5855
                                                     (prim
                                                      cons
                                                      (lambda doH$fargs5928
                                                        (let ((cont5927
                                                               (prim
                                                                car
                                                                doH$fargs5928)))
                                                          (let ((doH$fargs
                                                                 (prim
                                                                  cdr
                                                                  doH$fargs5928)))
                                                            (HUA$%drop-right
                                                             (lambda (_5929
                                                                      a5851)
                                                               (let ((cps-lst5933
                                                                      (prim
                                                                       cons
                                                                       (lambda (_5930
                                                                                a5852)
                                                                         (ePl$%last
                                                                          (lambda (_5931
                                                                                   a5853)
                                                                            (let ((retprim5932
                                                                                   (prim
                                                                                    cons
                                                                                    a5852
                                                                                    a5853)))
                                                                              (cont5927
                                                                               '()
                                                                               retprim5932)))
                                                                          doH$fargs))
                                                                       a5851)))
                                                                 (apply
                                                                  N96$f
                                                                  cps-lst5933)))
                                                             doH$fargs
                                                             '1))))
                                                      a5854)))
                                                (let ((cps-lst5934
                                                       (prim
                                                        cons
                                                        cont5925
                                                        a5855)))
                                                  (apply
                                                   Z2q$%foldr
                                                   cps-lst5934)))))))))))
                             (kXW$Ycmb
                              (lambda (_5935 iqd$%foldl)
                                (let ((tDm$%>
                                       (lambda (cont5936 SMs$a KXl$b)
                                         (let ((a5863 (prim <= SMs$a KXl$b)))
                                           (let ((retprim5937
                                                  (prim not a5863)))
                                             (cont5936 '() retprim5937))))))
                                  (let ((E54$%>=
                                         (lambda (cont5938 x6K$a zkv$b)
                                           (let ((a5864 (prim < x6K$a zkv$b)))
                                             (let ((retprim5939
                                                    (prim not a5864)))
                                               (cont5938 '() retprim5939))))))
                                    (let ((qpa$%append
                                           (prim make-vector '1 '())))
                                      (let ((vRb$_0
                                             (prim
                                              vector-set!
                                              qpa$%append
                                              '0
                                              (lambda (cont6007
                                                       QzI$ls0
                                                       cs7$ls1)
                                                (let ((a5865
                                                       (prim null? QzI$ls0)))
                                                  (if a5865
                                                    (cont6007 '() cs7$ls1)
                                                    (let ((a5866
                                                           (prim car QzI$ls0)))
                                                      (let ((a5867
                                                             (prim
                                                              vector-ref
                                                              qpa$%append
                                                              '0)))
                                                        (let ((a5868
                                                               (prim
                                                                cdr
                                                                QzI$ls0)))
                                                          (a5867
                                                           (lambda (_6008
                                                                    a5869)
                                                             (let ((retprim6009
                                                                    (prim
                                                                     cons
                                                                     a5866
                                                                     a5869)))
                                                               (cont6007
                                                                '()
                                                                retprim6009)))
                                                           a5868
                                                           cs7$ls1))))))))))
                                        (let ((retprim6010
                                               (prim
                                                vector-ref
                                                qpa$%append
                                                '0)))
                                          ((lambda (_5940 ixv$%append)
                                             (let ((UHe$%list?
                                                    (lambda (cont5941 Cle$a)
                                                      (let ((TcY$a
                                                             (prim
                                                              make-vector
                                                              '1
                                                              Cle$a)))
                                                        ((lambda (cont5947
                                                                  kbh$k)
                                                           (cont5947
                                                            '()
                                                            kbh$k))
                                                         (lambda (_5942 jLY$cc)
                                                           (let ((a5870
                                                                  (prim
                                                                   vector-ref
                                                                   TcY$a
                                                                   '0)))
                                                             (let ((a5871
                                                                    (prim
                                                                     null?
                                                                     a5870)))
                                                               (if a5871
                                                                 (cont5941
                                                                  '()
                                                                  '#t)
                                                                 (let ((a5872
                                                                        (prim
                                                                         vector-ref
                                                                         TcY$a
                                                                         '0)))
                                                                   (let ((a5873
                                                                          (prim
                                                                           cons?
                                                                           a5872)))
                                                                     (if a5873
                                                                       (let ((a5874
                                                                              (prim
                                                                               vector-ref
                                                                               TcY$a
                                                                               '0)))
                                                                         (let ((retprim5946
                                                                                (prim
                                                                                 cdr
                                                                                 a5874)))
                                                                           ((lambda (_5943
                                                                                     OBM$b)
                                                                              (let ((a5875
                                                                                     (prim
                                                                                      vector-ref
                                                                                      TcY$a
                                                                                      '0)))
                                                                                (let ((a5876
                                                                                       (prim
                                                                                        cdr
                                                                                        a5875)))
                                                                                  (let ((retprim5945
                                                                                         (prim
                                                                                          vector-set!
                                                                                          TcY$a
                                                                                          '0
                                                                                          a5876)))
                                                                                    ((lambda (_5944
                                                                                              wxF$_0)
                                                                                       (jLY$cc
                                                                                        cont5941
                                                                                        jLY$cc))
                                                                                     '()
                                                                                     retprim5945)))))
                                                                            '()
                                                                            retprim5946)))
                                                                       (cont5941
                                                                        '()
                                                                        '#f))))))))
                                                         (lambda (_5942 jLY$cc)
                                                           (let ((a5870
                                                                  (prim
                                                                   vector-ref
                                                                   TcY$a
                                                                   '0)))
                                                             (let ((a5871
                                                                    (prim
                                                                     null?
                                                                     a5870)))
                                                               (if a5871
                                                                 (cont5941
                                                                  '()
                                                                  '#t)
                                                                 (let ((a5872
                                                                        (prim
                                                                         vector-ref
                                                                         TcY$a
                                                                         '0)))
                                                                   (let ((a5873
                                                                          (prim
                                                                           cons?
                                                                           a5872)))
                                                                     (if a5873
                                                                       (let ((a5874
                                                                              (prim
                                                                               vector-ref
                                                                               TcY$a
                                                                               '0)))
                                                                         (let ((retprim5946
                                                                                (prim
                                                                                 cdr
                                                                                 a5874)))
                                                                           ((lambda (_5943
                                                                                     OBM$b)
                                                                              (let ((a5875
                                                                                     (prim
                                                                                      vector-ref
                                                                                      TcY$a
                                                                                      '0)))
                                                                                (let ((a5876
                                                                                       (prim
                                                                                        cdr
                                                                                        a5875)))
                                                                                  (let ((retprim5945
                                                                                         (prim
                                                                                          vector-set!
                                                                                          TcY$a
                                                                                          '0
                                                                                          a5876)))
                                                                                    ((lambda (_5944
                                                                                              wxF$_0)
                                                                                       (jLY$cc
                                                                                        cont5941
                                                                                        jLY$cc))
                                                                                     '()
                                                                                     retprim5945)))))
                                                                            '()
                                                                            retprim5946)))
                                                                       (cont5941
                                                                        '()
                                                                        '#f)))))))))))))
                                               (let ((rOC$%drop
                                                      (lambda (cont5948
                                                               NGQ$lst
                                                               h8i$n)
                                                        (let ((aax$lst
                                                               (prim
                                                                make-vector
                                                                '1
                                                                NGQ$lst)))
                                                          (let ((Nvg$n
                                                                 (prim
                                                                  make-vector
                                                                  '1
                                                                  h8i$n)))
                                                            ((lambda (cont5955
                                                                      f1k$u)
                                                               (f1k$u
                                                                cont5955
                                                                f1k$u))
                                                             (lambda (_5949
                                                                      Mcy$cc)
                                                               (let ((a5877
                                                                      (prim
                                                                       vector-ref
                                                                       Nvg$n
                                                                       '0)))
                                                                 (let ((a5878
                                                                        (prim
                                                                         =
                                                                         '0
                                                                         a5877)))
                                                                   (if a5878
                                                                     (let ((retprim5950
                                                                            (prim
                                                                             vector-ref
                                                                             aax$lst
                                                                             '0)))
                                                                       (cont5948
                                                                        '()
                                                                        retprim5950))
                                                                     (let ((a5879
                                                                            (prim
                                                                             vector-ref
                                                                             aax$lst
                                                                             '0)))
                                                                       (let ((a5880
                                                                              (prim
                                                                               cdr
                                                                               a5879)))
                                                                         (let ((retprim5954
                                                                                (prim
                                                                                 vector-set!
                                                                                 aax$lst
                                                                                 '0
                                                                                 a5880)))
                                                                           ((lambda (_5951
                                                                                     o30$_0)
                                                                              (let ((a5881
                                                                                     (prim
                                                                                      vector-ref
                                                                                      Nvg$n
                                                                                      '0)))
                                                                                (let ((a5882
                                                                                       (prim
                                                                                        -
                                                                                        a5881
                                                                                        '1)))
                                                                                  (let ((retprim5953
                                                                                         (prim
                                                                                          vector-set!
                                                                                          Nvg$n
                                                                                          '0
                                                                                          a5882)))
                                                                                    ((lambda (_5952
                                                                                              SQX$_1)
                                                                                       (Mcy$cc
                                                                                        cont5948
                                                                                        Mcy$cc))
                                                                                     '()
                                                                                     retprim5953)))))
                                                                            '()
                                                                            retprim5954))))))))
                                                             (lambda (_5949
                                                                      Mcy$cc)
                                                               (let ((a5877
                                                                      (prim
                                                                       vector-ref
                                                                       Nvg$n
                                                                       '0)))
                                                                 (let ((a5878
                                                                        (prim
                                                                         =
                                                                         '0
                                                                         a5877)))
                                                                   (if a5878
                                                                     (let ((retprim5950
                                                                            (prim
                                                                             vector-ref
                                                                             aax$lst
                                                                             '0)))
                                                                       (cont5948
                                                                        '()
                                                                        retprim5950))
                                                                     (let ((a5879
                                                                            (prim
                                                                             vector-ref
                                                                             aax$lst
                                                                             '0)))
                                                                       (let ((a5880
                                                                              (prim
                                                                               cdr
                                                                               a5879)))
                                                                         (let ((retprim5954
                                                                                (prim
                                                                                 vector-set!
                                                                                 aax$lst
                                                                                 '0
                                                                                 a5880)))
                                                                           ((lambda (_5951
                                                                                     o30$_0)
                                                                              (let ((a5881
                                                                                     (prim
                                                                                      vector-ref
                                                                                      Nvg$n
                                                                                      '0)))
                                                                                (let ((a5882
                                                                                       (prim
                                                                                        -
                                                                                        a5881
                                                                                        '1)))
                                                                                  (let ((retprim5953
                                                                                         (prim
                                                                                          vector-set!
                                                                                          Nvg$n
                                                                                          '0
                                                                                          a5882)))
                                                                                    ((lambda (_5952
                                                                                              SQX$_1)
                                                                                       (Mcy$cc
                                                                                        cont5948
                                                                                        Mcy$cc))
                                                                                     '()
                                                                                     retprim5953)))))
                                                                            '()
                                                                            retprim5954))))))))))))))
                                                 (let ((lgR$%memv
                                                        (lambda (cont5956
                                                                 VTn$v
                                                                 nzO$lst)
                                                          (let ((zJw$lst
                                                                 (prim
                                                                  make-vector
                                                                  '1
                                                                  nzO$lst)))
                                                            ((lambda (cont5961
                                                                      myj$u)
                                                               (myj$u
                                                                cont5961
                                                                myj$u))
                                                             (lambda (_5957
                                                                      Cb6$cc)
                                                               (let ((a5883
                                                                      (prim
                                                                       vector-ref
                                                                       zJw$lst
                                                                       '0)))
                                                                 (let ((a5884
                                                                        (prim
                                                                         null?
                                                                         a5883)))
                                                                   (if a5884
                                                                     (cont5956
                                                                      '()
                                                                      '#f)
                                                                     (let ((a5885
                                                                            (prim
                                                                             vector-ref
                                                                             zJw$lst
                                                                             '0)))
                                                                       (let ((a5886
                                                                              (prim
                                                                               car
                                                                               a5885)))
                                                                         (let ((a5887
                                                                                (prim
                                                                                 eqv?
                                                                                 a5886
                                                                                 VTn$v)))
                                                                           (if a5887
                                                                             (let ((retprim5958
                                                                                    (prim
                                                                                     vector-ref
                                                                                     zJw$lst
                                                                                     '0)))
                                                                               (cont5956
                                                                                '()
                                                                                retprim5958))
                                                                             (let ((a5888
                                                                                    (prim
                                                                                     vector-ref
                                                                                     zJw$lst
                                                                                     '0)))
                                                                               (let ((a5889
                                                                                      (prim
                                                                                       cdr
                                                                                       a5888)))
                                                                                 (let ((retprim5960
                                                                                        (prim
                                                                                         vector-set!
                                                                                         zJw$lst
                                                                                         '0
                                                                                         a5889)))
                                                                                   ((lambda (_5959
                                                                                             Ze2$_0)
                                                                                      (Cb6$cc
                                                                                       cont5956
                                                                                       Cb6$cc))
                                                                                    '()
                                                                                    retprim5960))))))))))))
                                                             (lambda (_5957
                                                                      Cb6$cc)
                                                               (let ((a5883
                                                                      (prim
                                                                       vector-ref
                                                                       zJw$lst
                                                                       '0)))
                                                                 (let ((a5884
                                                                        (prim
                                                                         null?
                                                                         a5883)))
                                                                   (if a5884
                                                                     (cont5956
                                                                      '()
                                                                      '#f)
                                                                     (let ((a5885
                                                                            (prim
                                                                             vector-ref
                                                                             zJw$lst
                                                                             '0)))
                                                                       (let ((a5886
                                                                              (prim
                                                                               car
                                                                               a5885)))
                                                                         (let ((a5887
                                                                                (prim
                                                                                 eqv?
                                                                                 a5886
                                                                                 VTn$v)))
                                                                           (if a5887
                                                                             (let ((retprim5958
                                                                                    (prim
                                                                                     vector-ref
                                                                                     zJw$lst
                                                                                     '0)))
                                                                               (cont5956
                                                                                '()
                                                                                retprim5958))
                                                                             (let ((a5888
                                                                                    (prim
                                                                                     vector-ref
                                                                                     zJw$lst
                                                                                     '0)))
                                                                               (let ((a5889
                                                                                      (prim
                                                                                       cdr
                                                                                       a5888)))
                                                                                 (let ((retprim5960
                                                                                        (prim
                                                                                         vector-set!
                                                                                         zJw$lst
                                                                                         '0
                                                                                         a5889)))
                                                                                   ((lambda (_5959
                                                                                             Ze2$_0)
                                                                                      (Cb6$cc
                                                                                       cont5956
                                                                                       Cb6$cc))
                                                                                    '()
                                                                                    retprim5960)))))))))))))))))
                                                   (let ((Nhd$%/
                                                          (lambda Tlk$args5963
                                                            (let ((cont5962
                                                                   (prim
                                                                    car
                                                                    Tlk$args5963)))
                                                              (let ((Tlk$args
                                                                     (prim
                                                                      cdr
                                                                      Tlk$args5963)))
                                                                (let ((a5890
                                                                       (prim
                                                                        null?
                                                                        Tlk$args)))
                                                                  (if a5890
                                                                    (cont5962
                                                                     '()
                                                                     '1)
                                                                    (let ((a5891
                                                                           (prim
                                                                            cdr
                                                                            Tlk$args)))
                                                                      (let ((a5892
                                                                             (prim
                                                                              null?
                                                                              a5891)))
                                                                        (if a5892
                                                                          (let ((retprim5964
                                                                                 (prim
                                                                                  car
                                                                                  Tlk$args)))
                                                                            (cont5962
                                                                             '()
                                                                             retprim5964))
                                                                          (let ((a5893
                                                                                 (prim
                                                                                  car
                                                                                  Tlk$args)))
                                                                            (let ((a5894
                                                                                   (prim
                                                                                    cdr
                                                                                    Tlk$args)))
                                                                              (DeK$%foldl1
                                                                               cont5962
                                                                               (lambda (cont5965
                                                                                        nq8$n
                                                                                        W6p$v)
                                                                                 (let ((retprim5966
                                                                                        (prim
                                                                                         /
                                                                                         W6p$v
                                                                                         nq8$n)))
                                                                                   (cont5965
                                                                                    '()
                                                                                    retprim5966)))
                                                                               a5893
                                                                               a5894)))))))))))))
                                                     (let ((XhW$%first
                                                            (lambda (cont5967
                                                                     C1T$x)
                                                              (let ((retprim5968
                                                                     (prim
                                                                      car
                                                                      C1T$x)))
                                                                (cont5967
                                                                 '()
                                                                 retprim5968)))))
                                                       (let ((Pqh$%second
                                                              (lambda (cont5969
                                                                       IVS$x)
                                                                (let ((a5895
                                                                       (prim
                                                                        cdr
                                                                        IVS$x)))
                                                                  (let ((retprim5970
                                                                         (prim
                                                                          car
                                                                          a5895)))
                                                                    (cont5969
                                                                     '()
                                                                     retprim5970))))))
                                                         (let ((Rhm$%third
                                                                (lambda (cont5971
                                                                         LoF$x)
                                                                  (let ((a5896
                                                                         (prim
                                                                          cdr
                                                                          LoF$x)))
                                                                    (let ((a5897
                                                                           (prim
                                                                            cdr
                                                                            a5896)))
                                                                      (let ((retprim5972
                                                                             (prim
                                                                              car
                                                                              a5897)))
                                                                        (cont5971
                                                                         '()
                                                                         retprim5972)))))))
                                                           (let ((Jc0$%fourth
                                                                  (lambda (cont5973
                                                                           fly$x)
                                                                    (let ((a5898
                                                                           (prim
                                                                            cdr
                                                                            fly$x)))
                                                                      (let ((a5899
                                                                             (prim
                                                                              cdr
                                                                              a5898)))
                                                                        (let ((a5900
                                                                               (prim
                                                                                cdr
                                                                                a5899)))
                                                                          (let ((retprim5974
                                                                                 (prim
                                                                                  car
                                                                                  a5900)))
                                                                            (cont5973
                                                                             '()
                                                                             retprim5974))))))))
                                                             ((lambda bSJ$lst6006
                                                                (let ((cont6005
                                                                       (prim
                                                                        car
                                                                        bSJ$lst6006)))
                                                                  (let ((bSJ$lst
                                                                         (prim
                                                                          cdr
                                                                          bSJ$lst6006)))
                                                                    (cont6005
                                                                     '()
                                                                     bSJ$lst))))
                                                              (lambda (_5975
                                                                       a5901)
                                                                ((lambda Gk1$lst6004
                                                                   (let ((cont6003
                                                                          (prim
                                                                           car
                                                                           Gk1$lst6004)))
                                                                     (let ((Gk1$lst
                                                                            (prim
                                                                             cdr
                                                                             Gk1$lst6004)))
                                                                       (cont6003
                                                                        '()
                                                                        Gk1$lst))))
                                                                 (lambda (_5976
                                                                          a5902)
                                                                   ((lambda nvW$lst6002
                                                                      (let ((cont6001
                                                                             (prim
                                                                              car
                                                                              nvW$lst6002)))
                                                                        (let ((nvW$lst
                                                                               (prim
                                                                                cdr
                                                                                nvW$lst6002)))
                                                                          (cont6001
                                                                           '()
                                                                           nvW$lst))))
                                                                    (lambda (_5977
                                                                             a5903)
                                                                      ((lambda Zz2$lst6000
                                                                         (let ((cont5999
                                                                                (prim
                                                                                 car
                                                                                 Zz2$lst6000)))
                                                                           (let ((Zz2$lst
                                                                                  (prim
                                                                                   cdr
                                                                                   Zz2$lst6000)))
                                                                             (cont5999
                                                                              '()
                                                                              Zz2$lst))))
                                                                       (lambda (_5978
                                                                                a5904)
                                                                         ((lambda yVT$el5997
                                                                            (let ((cont5996
                                                                                   (prim
                                                                                    car
                                                                                    yVT$el5997)))
                                                                              (let ((yVT$el
                                                                                     (prim
                                                                                      cdr
                                                                                      yVT$el5997)))
                                                                                (let ((retprim5998
                                                                                       (apply-prim
                                                                                        vector
                                                                                        yVT$el)))
                                                                                  (cont5996
                                                                                   '()
                                                                                   retprim5998)))))
                                                                          (lambda (_5979
                                                                                   a5905)
                                                                            ((lambda AbW$el5994
                                                                               (let ((cont5993
                                                                                      (prim
                                                                                       car
                                                                                       AbW$el5994)))
                                                                                 (let ((AbW$el
                                                                                        (prim
                                                                                         cdr
                                                                                         AbW$el5994)))
                                                                                   (let ((retprim5995
                                                                                          (apply-prim
                                                                                           vector
                                                                                           AbW$el)))
                                                                                     (cont5993
                                                                                      '()
                                                                                      retprim5995)))))
                                                                             (lambda (_5980
                                                                                      a5906)
                                                                               ((lambda IZn$lst5992
                                                                                  (let ((cont5991
                                                                                         (prim
                                                                                          car
                                                                                          IZn$lst5992)))
                                                                                    (let ((IZn$lst
                                                                                           (prim
                                                                                            cdr
                                                                                            IZn$lst5992)))
                                                                                      (cont5991
                                                                                       '()
                                                                                       IZn$lst))))
                                                                                (lambda (_5981
                                                                                         a5907)
                                                                                  (f7k$%map1
                                                                                   (lambda (_5982
                                                                                            a5908)
                                                                                     (f7k$%map1
                                                                                      (lambda (_5983
                                                                                               a5909)
                                                                                        (DeK$%foldl1
                                                                                         (lambda (_0
                                                                                                  x)
                                                                                           (let ((_1
                                                                                                  (prim
                                                                                                   halt
                                                                                                   x)))
                                                                                             (_1
                                                                                              _1)))
                                                                                         (lambda YMW$args5985
                                                                                           (let ((cont5984
                                                                                                  (prim
                                                                                                   car
                                                                                                   YMW$args5985)))
                                                                                             (let ((YMW$args
                                                                                                    (prim
                                                                                                     cdr
                                                                                                     YMW$args5985)))
                                                                                               (let ((retprim5986
                                                                                                      (apply-prim
                                                                                                       +
                                                                                                       YMW$args)))
                                                                                                 (cont5984
                                                                                                  '()
                                                                                                  retprim5986)))))
                                                                                         '0
                                                                                         a5909))
                                                                                      (lambda (cont5987
                                                                                               yYF$b)
                                                                                        (if yYF$b
                                                                                          (cont5987
                                                                                           '()
                                                                                           '1)
                                                                                          (cont5987
                                                                                           '()
                                                                                           '2)))
                                                                                      a5908))
                                                                                   (lambda fQx$arg5989
                                                                                     (let ((cont5988
                                                                                            (prim
                                                                                             car
                                                                                             fQx$arg5989)))
                                                                                       (let ((fQx$arg
                                                                                              (prim
                                                                                               cdr
                                                                                               fQx$arg5989)))
                                                                                         (let ((retprim5990
                                                                                                (apply-prim
                                                                                                 number?
                                                                                                 fQx$arg)))
                                                                                           (cont5988
                                                                                            '()
                                                                                            retprim5990)))))
                                                                                   a5907))
                                                                                '#f
                                                                                '#t
                                                                                '7
                                                                                a5901
                                                                                a5904
                                                                                a5905
                                                                                a5906
                                                                                'yes))
                                                                             '0
                                                                             '1))))
                                                                       a5902
                                                                       a5903))))))))))))))))
                                           '()
                                           retprim6010)))))))
                              (lambda (cont6011 rD2$%foldl)
                                (cont6011
                                 '()
                                 (lambda rgE$args6013
                                   (let ((cont6012 (prim car rgE$args6013)))
                                     (let ((rgE$args (prim cdr rgE$args6013)))
                                       (let ((Tyf$f (prim car rgE$args)))
                                         (let ((a5856 (prim cdr rgE$args)))
                                           (let ((retprim6032
                                                  (prim car a5856)))
                                             ((lambda (_6014 U23$acc)
                                                (let ((a5857
                                                       (prim cdr rgE$args)))
                                                  (let ((retprim6031
                                                         (prim cdr a5857)))
                                                    ((lambda (_6015 zag$lsts)
                                                       (b0u$%foldr1
                                                        (lambda (_6016 a5858)
                                                          (if a5858
                                                            (cont6012
                                                             '()
                                                             U23$acc)
                                                            (f7k$%map1
                                                             (lambda (_6017
                                                                      BiT$lsts+)
                                                               (f7k$%map1
                                                                (lambda (_6018
                                                                         t44$vs)
                                                                  (let ((a5859
                                                                         (prim
                                                                          cons
                                                                          U23$acc
                                                                          '())))
                                                                    (Z2q$%foldr
                                                                     (lambda (_6021
                                                                              a5860)
                                                                       (let ((cps-lst6022
                                                                              (prim
                                                                               cons
                                                                               (lambda (_6019
                                                                                        ly4$acc+)
                                                                                 (let ((a5861
                                                                                        (prim
                                                                                         cons
                                                                                         ly4$acc+
                                                                                         BiT$lsts+)))
                                                                                   (let ((a5862
                                                                                          (prim
                                                                                           cons
                                                                                           Tyf$f
                                                                                           a5861)))
                                                                                     (let ((cps-lst6020
                                                                                            (prim
                                                                                             cons
                                                                                             cont6012
                                                                                             a5862)))
                                                                                       (apply
                                                                                        rD2$%foldl
                                                                                        cps-lst6020)))))
                                                                               a5860)))
                                                                         (apply
                                                                          Tyf$f
                                                                          cps-lst6022)))
                                                                     (lambda (cont6023
                                                                              zsy$a
                                                                              bZp$b)
                                                                       (let ((retprim6024
                                                                              (prim
                                                                               cons
                                                                               zsy$a
                                                                               bZp$b)))
                                                                         (cont6023
                                                                          '()
                                                                          retprim6024)))
                                                                     a5859
                                                                     t44$vs)))
                                                                (lambda (cont6025
                                                                         Ejk$x)
                                                                  (let ((retprim6026
                                                                         (prim
                                                                          car
                                                                          Ejk$x)))
                                                                    (cont6025
                                                                     '()
                                                                     retprim6026)))
                                                                zag$lsts))
                                                             (lambda (cont6027
                                                                      ObL$x)
                                                               (let ((retprim6028
                                                                      (prim
                                                                       cdr
                                                                       ObL$x)))
                                                                 (cont6027
                                                                  '()
                                                                  retprim6028)))
                                                             zag$lsts)))
                                                        (lambda (cont6029
                                                                 Wzg$lst
                                                                 RKa$b)
                                                          (if RKa$b
                                                            (cont6029
                                                             '()
                                                             RKa$b)
                                                            (let ((retprim6030
                                                                   (prim
                                                                    null?
                                                                    Wzg$lst)))
                                                              (cont6029
                                                               '()
                                                               retprim6030))))
                                                        '#f
                                                        zag$lsts))
                                                     '()
                                                     retprim6031))))
                                              '()
                                              retprim6032)))))))))))))
                       (lambda (cont6033 Cci$%foldr)
                         (cont6033
                          '()
                          (lambda Nx3$args6035
                            (let ((cont6034 (prim car Nx3$args6035)))
                              (let ((Nx3$args (prim cdr Nx3$args6035)))
                                (let ((IU9$f (prim car Nx3$args)))
                                  (let ((a5842 (prim cdr Nx3$args)))
                                    (let ((retprim6054 (prim car a5842)))
                                      ((lambda (_6036 lWT$acc)
                                         (let ((a5843 (prim cdr Nx3$args)))
                                           (let ((retprim6053
                                                  (prim cdr a5843)))
                                             ((lambda (_6037 WQH$lsts)
                                                (b0u$%foldr1
                                                 (lambda (_6038 a5844)
                                                   (if a5844
                                                     (cont6034 '() lWT$acc)
                                                     (hLK$%map1
                                                      (lambda (_6039 v8z$lsts+)
                                                        (hLK$%map1
                                                         (lambda (_6040 te1$vs)
                                                           (let ((a5845
                                                                  (prim
                                                                   cons
                                                                   lWT$acc
                                                                   v8z$lsts+)))
                                                             (let ((a5846
                                                                    (prim
                                                                     cons
                                                                     IU9$f
                                                                     a5845)))
                                                               (let ((cps-lst6046
                                                                      (prim
                                                                       cons
                                                                       (lambda (_6041
                                                                                a5847)
                                                                         (let ((a5848
                                                                                (prim
                                                                                 cons
                                                                                 a5847
                                                                                 '())))
                                                                           (b0u$%foldr1
                                                                            (lambda (_6042
                                                                                     a5849)
                                                                              (let ((cps-lst6043
                                                                                     (prim
                                                                                      cons
                                                                                      cont6034
                                                                                      a5849)))
                                                                                (apply
                                                                                 IU9$f
                                                                                 cps-lst6043)))
                                                                            (lambda (cont6044
                                                                                     voD$a
                                                                                     cGr$b)
                                                                              (let ((retprim6045
                                                                                     (prim
                                                                                      cons
                                                                                      voD$a
                                                                                      cGr$b)))
                                                                                (cont6044
                                                                                 '()
                                                                                 retprim6045)))
                                                                            a5848
                                                                            te1$vs)))
                                                                       a5846)))
                                                                 (apply
                                                                  Cci$%foldr
                                                                  cps-lst6046)))))
                                                         (lambda (cont6047
                                                                  yKv$x)
                                                           (let ((retprim6048
                                                                  (prim
                                                                   car
                                                                   yKv$x)))
                                                             (cont6047
                                                              '()
                                                              retprim6048)))
                                                         WQH$lsts))
                                                      (lambda (cont6049 IM2$x)
                                                        (let ((retprim6050
                                                               (prim
                                                                cdr
                                                                IM2$x)))
                                                          (cont6049
                                                           '()
                                                           retprim6050)))
                                                      WQH$lsts)))
                                                 (lambda (cont6051
                                                          NhC$lst
                                                          J44$b)
                                                   (if J44$b
                                                     (cont6051 '() J44$b)
                                                     (let ((retprim6052
                                                            (prim
                                                             null?
                                                             NhC$lst)))
                                                       (cont6051
                                                        '()
                                                        retprim6052))))
                                                 '#f
                                                 WQH$lsts))
                                              '()
                                              retprim6053))))
                                       '()
                                       retprim6054)))))))))))))
                (lambda (cont6055 Vzr$%foldl1)
                  (cont6055
                   '()
                   (lambda (cont6056 YVK$f SUk$acc nho$lst)
                     (let ((a5836 (prim null? nho$lst)))
                       (if a5836
                         (cont6056 '() SUk$acc)
                         (let ((a5837 (prim car nho$lst)))
                           (YVK$f
                            (lambda (_6057 a5838)
                              (let ((a5839 (prim cdr nho$lst)))
                                (Vzr$%foldl1 cont6056 YVK$f a5838 a5839)))
                            a5837
                            SUk$acc)))))))))
             (lambda (cont6058 yOK$%length)
               (cont6058
                '()
                (lambda (cont6059 Cln$lst)
                  (let ((a5833 (prim null? Cln$lst)))
                    (if a5833
                      (cont6059 '() '0)
                      (let ((a5834 (prim cdr Cln$lst)))
                        (yOK$%length
                         (lambda (_6060 a5835)
                           (let ((retprim6061 (prim + '1 a5835)))
                             (cont6059 '() retprim6061)))
                         a5834)))))))))
          (lambda (cont6062 w85$%take)
            (cont6062
             '()
             (lambda (cont6063 OCZ$lst Ood$n)
               (let ((a5827 (prim = Ood$n '0)))
                 (if a5827
                   (cont6063 '() '())
                   (let ((a5828 (prim null? OCZ$lst)))
                     (if a5828
                       (cont6063 '() '())
                       (let ((a5829 (prim car OCZ$lst)))
                         (let ((a5830 (prim cdr OCZ$lst)))
                           (let ((a5831 (prim - Ood$n '1)))
                             (w85$%take
                              (lambda (_6064 a5832)
                                (let ((retprim6065 (prim cons a5829 a5832)))
                                  (cont6063 '() retprim6065)))
                              a5830
                              a5831)))))))))))))
       (lambda (cont6066 G8p$%map)
         (cont6066
          '()
          (lambda (cont6067 uUL$f Wri$lst)
            (let ((a5822 (prim null? Wri$lst)))
              (if a5822
                (cont6067 '() '())
                (let ((a5823 (prim car Wri$lst)))
                  (uUL$f
                   (lambda (_6068 a5824)
                     (let ((a5825 (prim cdr Wri$lst)))
                       (G8p$%map
                        (lambda (_6069 a5826)
                          (let ((retprim6070 (prim cons a5824 a5826)))
                            (cont6067 '() retprim6070)))
                        uUL$f
                        a5825)))
                   a5823)))))))))
    (lambda (cont6071 moM$%foldr1)
      (cont6071
       '()
       (lambda (cont6072 bgR$f L4Y$acc G20$lst)
         (let ((a5818 (prim null? G20$lst)))
           (if a5818
             (cont6072 '() L4Y$acc)
             (let ((a5819 (prim car G20$lst)))
               (let ((a5820 (prim cdr G20$lst)))
                 (moM$%foldr1
                  (lambda (_6073 a5821) (bgR$f cont6072 a5819 a5821))
                  bgR$f
                  L4Y$acc
                  a5820))))))))))
 (lambda (cont6075 pWE$y)
   (cont6075
    '()
    (lambda (cont6076 abq$f)
      (abq$f
       cont6076
       (lambda l0n$args6078
         (let ((cont6077 (prim car l0n$args6078)))
           (let ((l0n$args (prim cdr l0n$args6078)))
             (pWE$y
              (lambda (_6079 a5816)
                (a5816
                 (lambda (_6080 a5817)
                   (let ((cps-lst6081 (prim cons cont6077 l0n$args)))
                     (apply a5817 cps-lst6081)))
                 abq$f))
              pWE$y)))))))))
