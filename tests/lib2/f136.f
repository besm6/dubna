      PROGRAM F136
      COMPLEX CDOTC,A(3),B(3),Z(5),V,W
      PRINT 10
      A(1)=(1.,1.)
      A(2)=(2.,2.)
      A(3)=(3.,3.)
      B(1)=(4.,4.)
      B(2)=(5.,5.)
      B(3)=(6.,6.)
      Z(1)=(0.,0.)
      Z(2)=(2.,3.)
      Z(3)=(-1.,2.)
      Z(4)=(3.,-1.)
      Z(5)=(1.,3.)
      DO 2 I=1,5
      V=Z(I)
      W=CDOTC(V,A,B,3,1,1)
  2   PRINT 1,Z(I),V,W
  1   FORMAT(10X,4F7.1,10X,2E20.11/)
  10  FORMAT(//50X,9HTEST F136//17X,1HZ,14X,1HV,35X,5HCDOTC//)
      STOP
      END
*EXECUTE
*
*
