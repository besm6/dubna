      PROGRAM Y202
      DIMENSION PAR(3)
      LOGICAL SPILL
      DATA PAR/10,12.,1./
      X=0.
      DO 1 I=1,12
      N=IUBIN(X,PAR,SPILL)
      PRINT 2,X,N,SPILL
 1    X=X+15.
 2    FORMAT(1X,'X=',F17.10,5X,'N=',I4,5X,'SPILL=',L3)
      STOP
      END
*EXECUTE
*
*
