       PROGRAM F133
       COMPLEX CDOT,Z(5),W,A(3,1),B(3,2)
      DIMENSION D(6),E(12),C(10)
      EQUIVALENCE (A,D),(B,E),(Z,C)
      DATA D/0.,1.,-1.,1.,2.,1./,E/0.,0.,1.,1.,2.,2.,0.,0.,1.,1.,0.,0./,
     *C/2.,3.,-1.,2.,1.,3.,2.,5.,3.,-1./
      PRINT 10
      DO 2 I=1,5
       W=CDOT(Z(I),A,B,3,1,2)
  2   PRINT  1,Z(I),W
  1   FORMAT(10X,F4.1,1H,,F4.1,10X,E20.11,1H,,E20.11/)
  10  FORMAT(40X,9HTEST F133///14X,1HZ,30X,4HCDOT//)
      STOP
       END
*EXECUTE
*
*
