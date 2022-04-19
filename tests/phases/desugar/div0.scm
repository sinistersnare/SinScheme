(+ (guard (x [else '1])
          (/ '1 '2 '3 '0))
   (guard (x [else '1])
          (/ '0))
   (guard (_ [else '1002]) ; shouldnt fail
          (/ '1))
   (guard (x [else '1])
          (/)))
