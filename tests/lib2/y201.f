      PROGRAM Y201
      DATA XLOW,DX,NX/1.,12.,10/
      X=0.
      DO 1 I=1,10
      N=IUCHAN(X,XLOW,DX,NX)
      PRINT 2,X,N
 1    X=X+15.
 2    FORMAT(1X,'X=',F17.10,5X,'N=',I4)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
