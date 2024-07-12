      PROGRAM F301
      DIMENSION A(4,4),B(4,4),N(3),ALFA(5)
      DATA B/2.,2.,4.,1.,5.,-7.,0.,0.,9.,12.,3.,6.,4.,8.,-5.,4./,
     * N/4,3,2/,ALFA/0.,0.5,1.,2.,3./
      PRINT 10,B
      DO 1 I=1,3
      DO 1 J=1,5
      DO 5 L=1,4
      DO 5 K=1,4
  5   A(L,K)=B(L,K)
      Y=DET(A,4,N(I),ALFA(J))
  1   PRINT 2,N(I),ALFA(J),Y
  2   FORMAT(10X,I3,10X,F4.1,10X,E20.11)
  10  FORMAT(//30X,9HTEST F301//30X,1HA//4(10X,4F10.1/)//11X,1HN,12X,
     * 4HALFA,20X,1HY/)
      STOP
       END
*EXECUTE
*
*
