      PROGRAM E402
      DIMENSION X(5),C(8)
      DATA X/-1.,-0.5,0.0,0.5,1.0/
      P=3.1415926
      C(1)=P/4.0
      C(2)=-4/P
      C(3)=0.0
      C(4)=-4.0/(9*P)
      C(5)=0.0
      C(6)=-4.0/(25*P)
      C(7)=0.0
      C(8)=-4.0/(49*P)
      N=8
      PRINT 10
      DO 30 I=1,5
      F=CHSUM1(X(I),C,N)
      PRINT 20,X(I),F
 30   CONTINUE
      STOP
  10  FORMAT(///50X,9HTEST E402//33X,1HX,20X,1HF//)
  20  FORMAT(30X,F5.1,10X,E20.11)
      END
*EXECUTE
   -1.0
   -0.5
    0.0
   0.5
    1.0
*
*
