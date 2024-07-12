      PROGRAM C313
C        CM.  TEST C312
      DIMENSION X(10)
      DATA X/-20.,-10.,-5.,-1.,0.1,0.3,0.5,10.,20.,30./
      PRINT 10
       DO 2 I=1,10
      Y1=BESI0(X(I))
      Y2=BESI1(X(I))
      Y3=BESK0(X(I))
      Y4=BESK1(X(I))
      E1=EBESI0(X(I))
      E2=EBESI1(X(I))
      E3=EBESK0(X(I))
      E4=EBESK1(X(I))
      PRINT 5,X(I),Y1,Y2,Y3,Y4,E1,E2,E3,E4
  2      CONTINUE
      XX=50.
      BK0=BESK0(50.)
      PRINT 1,XX,BK0
      XX=-60.
      BK1=BESK1(-60.)
      PRINT 1,XX,BK1
      XX=100.
      BI0=BESI0(100.)
      PRINT 1,XX,BI0
      XX=80.
      BI1=BESI1( 80.)
      PRINT 1,XX,BI1
      XX=-70.
      BI2=BESI1(-70.)
      PRINT 1,XX,BI2
 1    FORMAT( /'  XX='F10.1,5X,'   Y=',E14.2 )
  5   FORMAT(1X,F7.1,8E14.7)
  10  FORMAT(//50X,9HTEST C313///5X,1HX,7X,2HY1,14X,2HY2,12X,2HY3,12X,
     *       2HY4,12X,2HE1,12X,2HE2,12X,2HE3,12X,2HE4,/)
      STOP
      END
*EXECUTE
*
*
