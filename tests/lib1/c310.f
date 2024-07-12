      PROGRAM C310
      COMPLEX Z(5),A(5),B(5),C(5),D(5),CRAGAM,RGAM
      PRINT 10
      Z(1)=(1.,2.)
      A(1)=(0.,-1.)
      B(1)=(0.,-1.5)
      DO 1 I=1,5
      C(I)=CRAGAM(Z(1),A(I),B(1))
      D(I)=RGAM(Z(1),A(I),B(1))
      PRINT 5,Z(1),A(I),B(1),C(I),D(I)
  1   A(I+1)=A(I)+(0.,-0.5)
      Z(1)=(-2.,2.)
      A(1)=(1.,-2.)
      B(1)=(3.,-2.)
      DO 2 I=1,4
      C(I)=CRAGAM(Z(I),A(1),B(1))
      D(I)=RGAM(Z(I),A(1),B(1))
      PRINT 5,Z(I),A(1),B(1),C(I),D(I)
  2   Z(I+1)=Z(I)+(-2.,0.)
      Z(1)=(-1.,2.)
      A(1)=(1.,-2.)
      B(1)=(-3.,-2.)
      C(1)=CRAGAM(Z(1),A(1),B(1))
      D(1)=RGAM(Z(1),A(1),B(1))
      PRINT 5,Z(1),A(1),B(1),C(1),D(1)
  5   FORMAT(4X'('F4.1','F4.1')'4X'('F4.1','F4.1')'4X'('F4.1','F4.1')'7X
     1'('F12.9','F12.9')'7X'('F12.9','F12.9')'//)
  10  FORMAT(1H1//50X'TEST C310'///10X'Z'15X'A'15X'B'20X'CRAGAM(Z,A,B)'2
     10X'RGAM(Z,A,B)'//)
      END
*EXECUTE
*
*
