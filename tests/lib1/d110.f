      PROGRAM D110
      DIMENSION C(5), Y(5), IOP(5)
      EXTERNAL F
      PRINT 6
  6   FORMAT (1H1, ' * * * * * GENERAL PURPOSE INTEGRATION IN SINGLE
     *PRECISION * * * * * TEST  D110 * * * * * ')
      A=0.0
      C(1)=1.570796
      EPSIN=0.1E-05
          IOP(1)=2
      IOP(3)=3
          IOP(2)=3
          IOP(4)=3
          IOP(5)=3
      B=C(1)
      Y(1)=GPINSP(A,B,EPSIN,EPSOUT,F,IOP(1))
         PRINT 5,Y(1)
  5   FORMAT (//3X,5HY(1)=, F14.8)
      DO 1  I=2,5
      C(I)=C(I-1)+C(1)
 1    CONTINUE
      PRINT 2,C
  2   FORMAT (//12X,4HB(1),10X,4HB(2),10X,4HB(3),10X,4HB(4),
     *10X,4HB(5),/5X,5F14.8)
      DO 3  I=2,5
      B=C(I)
      Y(I)=GPINSP(A,B,EPSIN,EPSOUT,F,IOP(I))
 3    CONTINUE
      PRINT 4,Y
  4   FORMAT (//12X,4HY(1),10X,4HY(2),10X,4HY(3),10X,4HY(4),
     *10X,4HY(5),/5X,5F14.8)
      STOP
      END
      FUNCTION F(X)
      F=SIN(X)
      RETURN
      END
*EXECUTE
*
*
