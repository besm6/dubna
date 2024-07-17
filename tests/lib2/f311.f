*       tape:12/librar,32
*table:liblist(determ)
*liblist:320375
      PROGRAM  F311
      DIMENSION A(4,6),B(4,6),N(3)
      DATA B/2.,2.,4.,1.,5.,-7.,0.,0.,9.,12.,3.,6.,4.,8.,-5.,4.,8*0./,
     * N/4,3,2/
      PRINT 10,B
      DO 1 I=1,3
      DO 5 L=1,4
      DO 5 K=1,6
  5   A(L,K)=B(L,K)
      Y=DETERM(A,4,N(I))
  1   PRINT 2,N(I),Y
  2   FORMAT(10X,I3,10X,E20.11)
  10  FORMAT(//30X,9HTEST F311//30X,1HA//6(10X,4F10.1/)//11X,1HN,20X,
     * 1HY/)
      STOP
      END
*EXECUTE
*
*
