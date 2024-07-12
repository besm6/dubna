      PROGRAM F111
      DIMENSION A(4,4),B(4,4),C(4,4)
      DO 10 I=1,16
      A(I)=I
  10  B(I)=I+10
      CALL MXADD(A,B,C,4,4)
      PRINT11
      PRINT 12,(A(I),I=1,16)
      PRINT 14,(B(I),I=1,16)
      PRINT  15,(C(I),I=1,16)
      CALL MXSUB(B,A,C,4,4)
      PRINT 16
      PRINT 17,(C(I),I=1,16)
      CALL MXTRA(A,B,C,4,4)
      PRINT 18
      PRINT 17,(C(I),I=1,16)
      CALL MXMTR(A,10.0,C,4,4)
      PRINT 19
      PRINT 17,(C(I),I=1,16)
      CALL MXNTR(A,B,C,4,4)
      PRINT 20
      PRINT 17,(C(I),I=1,16)
   11 FORMAT(1H1,3X,5HMXADD,//)
   12 FORMAT(3X,1HA,//1(4F12.2))
   14 FORMAT(3X//3X,1HB,//1(F12.2))
   15 FORMAT(//3X,1HC,//1(F12.2))
   16 FORMAT(///5HMXSUB//)
   17 FORMAT(4F12.2)
   18 FORMAT(///5HMXTRA//)
   19 FORMAT(///5HMXMTR//)
   20 FORMAT(///5HMXNTR//)
      END
*EXECUTE
*
*                                                                      T
