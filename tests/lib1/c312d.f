      PROGRAM C312 D
      DOUBLE PRECISION DX,BJ0,B0,BJ1,B1,DBESJ0,DBESJ1,
     *                 BY0,B2,BY1,B3,DBESY0,DBESY1
      PRINT 3
      DO 1 I=1,100
      DX=I
      BJ0=DBESJ0(DX)
      B0=DBESJ0(-DX)
      PRINT 2,DX,BJ0,B0
 1    CONTINUE
      PRINT 5
      DO 10 I=1,100
      DX=I
      BJ1=DBESJ1(DX)
      B1=DBESJ1(-DX)
      PRINT 2,DX,BJ1,B1
 10   CONTINUE
      PRINT 7
      DO 20 I=1,100
      DX=I
      BY0=DBESY0(DX)
      BY1=DBESY1(DX)
      PRINT 2,DX,BY0,BY1
 20   CONTINUE
      B2=DBESY0(-2.D0)
      B3=DBESY1(-2.D0)
 2    FORMAT(5X,D7.1,2D30.20)
 3    FORMAT(//5X,'X',18X,'J0',24X,'J0(-X)'///)
 5    FORMAT(//5X,'X',18X,'J1',24X,'J1(-X)'///)
 7    FORMAT(//5X,'X',18X,'Y0',24X,'Y1'///)
      STOP
      END
*EXECUTE
*
*
