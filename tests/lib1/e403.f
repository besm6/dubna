      PROGRAM E403
      DOUBLE PRECISION CHSUM2,X,C,F
      DIMENSION X(5),C(8)
      DATA X/-1.D0,-0.5D0,0.0D0,0.5D0,1.0D0/
      P=3.1415926D0
      C(1)=P/4.D0
      C(2)=-4.D0/P
      C(3)=0.0D0
      C(4)=-4.D0/(9.D0*P)
      C(5)=0.0D0
      C(6)=-4.D0/(25.D0*P)
      C(7)=0.0D0
      C(8)=-4.D0/(49.D0*P)
      PRINT 10
      N=8
      DO 30  I=1,5
      F=CHSUM2(X(I),C,N)
      PRINT 20,X(I),F
  30  CONTINUE
      STOP
  10  FORMAT(///50X,9HTEST E403//33X,1HX,35X,1HF//)
  20  FORMAT(30X,D9.1,10X,D43.23)
      END
*EXECUTE
 -1.0D0
 -0.5D0
  0.0D0
  0.5D0
  1.0D0
*
*
