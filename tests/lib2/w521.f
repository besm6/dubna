      PROGRAM W521
      DATA PIM/0.1396/ , RAD/57.296/
      PRINT 5
      DO  10  J=1,21
      W = 1.1 + 0.02*(J-1)
      S = W*W
      Q2 = 0.25*( S - 1.1623 )*( S - 0.6379 )/S
      A = SQRT(Q2)/PIM
      B = W/PIM
      CALL ROSEN(A,B,C)
      P33 = C*RAD
      PRINT 15 ,   W , P33
   10 CONTINUE
    5 FORMAT (1H1,40X,16HTOTAL CMS ENERGY,14X,15HP33 PHASE SHIFT)
  15  FORMAT(1H0,40X,F10.3,20X,F10.1)
      END
*EXECUTE
*
*
