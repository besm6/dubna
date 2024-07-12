      PROGRAM C313 D
      DOUBLE PRECISION DX,BI0,BEI0,B0,DBESI0,BI1,BEI1,B1,DBESI1,
     *                 BK0,BEK0,DBESK0,BK1,BEK1,DBESK1
      PRINT 3
      DO 1 I=1,100
      DX=I
      BI0=DBESI0(DX)
      B0=DBESI0(-DX)
      BEI0=DEBSI0(DX)
      PRINT 2,DX,BI0,B0,BEI0
 1    CONTINUE
      PRINT 5
      DO 10 I=1,100
      DX=I
      BI1=DBESI1(DX)
      B1=DBESI1(-DX)
      BEI1=DEBSI1(DX)
      PRINT 2,DX,BI1,B1,BEI1
 10   CONTINUE
      PRINT 7
      DO 20 I=1,100
      DX=I
      BK0=DBESK0(DX)
      BEK0=DEBSK0(DX)
      PRINT 4,DX,BK0,BEK0
 20   CONTINUE
      B1=DBESK0(-2.D0)
      B1=DEBSK0(-2.D0)
      PRINT 9
      DO 30 I=1,100
      DX=I
      BK1=DBESK1(DX)
      BEK1=DEBSK1(DX)
      PRINT 4,DX,BK1,BEK1
 30   CONTINUE
      B1=DBESK1(-2.D0)
      B1=DEBSK1(-2.D0)
 2    FORMAT(5X,D7.1,3D30.20)
 3    FORMAT(//7X,'X',16X,'I0',24X,'I0(-X)',23X,'EXP(-ABS(X))*I0(X)'///)
 4    FORMAT(5X,D7.1,2D30.20)
 5    FORMAT(//7X,'X',16X,'I1',24X,'I1(-X)',23X,'EXP(-ABS(X))*I1(X)'///)
 7    FORMAT(//7X,'X',16X,'K0',24X,             'EXP(-ABS(X))*K0(X)'///)
 9    FORMAT(//7X,'X',16X,'K1',24X,             'EXP(-ABS(X))*K1(X)'///)
      STOP
      END
*EXECUTE
*
*
