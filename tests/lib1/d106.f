         PROGRAM  D106
         DIMENSION  Y(5),Z(96),W(96)
         EXTERNAL  FUN
      PRINT 1
  1   FORMAT (1H1, ' * * * * * N-POINT GAUSSIAN QUADRATURE * * * * *
     *TEST  D106 * * * * * ')
         Y(1) =GQUAD(FUN,0.,1.,96)
         PRINT 5,Y(1)
  5   FORMAT (//10X,2HY=,F10.6,//)
         CALL GSET(0.,1.,96,Z,W)
         PRINT 6,(Z(I),W(I),I=1,96)
  6   FORMAT (10X,1HZ,14X,1HW,14X,1HZ,14X,1HW,14X,1HZ,14X,1HW,
     *14X,1HZ,14X,1HW,/4(2F15.6))
         END
         FUNCTION  FUN(Y)
         FUN=1./(1.+Y*Y)
         RETURN
         END
*EXECUTE
*
*
