      PROGRAM Y203
      DIMENSION PAR(3)
      LOGICAL SPILL
      DATA PAR/10,.08333,1./
      X=0.
      DO 1 I=1,12
      N=IUHIST(X,PAR,SPILL)
      PRINT 2,X,N,SPILL
 1    X=X+15.
 2    FORMAT(1X,'X=',F17.10,5X,'N=',I4,5X,'SPILL=',L3)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*