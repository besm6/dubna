      PROGRAM M224
      DIMENSION X(10) ,FMT(2),W(12)
      DATA FMT/12H(I4,10F12.0)/
      DO 1 I=1,2
      X(I)=RNDM(-1.)
      X(I+2 )=50*RNDM(-1.)
      X(I+4 )=-X(I+2 )*10**5
      X(I+6 )=500+RNDM(-1.)
 1    X(I+8 )=0.5*RNDM(-1.)
      PRINT 3,X
 3    FORMAT(1H1//1X,10F12.2//)
      DO 2 I=5,10
      CALL UBLOW(FMT,W,12)
      CALL SETFMT(W(7),W(11),I,X,10)
      CALL UBUNCH(W,FMT,12)
      K=I-4
      PRINT FMT,K,X
 2    CONTINUE
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
