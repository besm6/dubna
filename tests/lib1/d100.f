      PROGRAM D100
      DIMENSION A(5),B(5)
      DATA (B=1.,1.2,1.4,1.6,1.8)
      H=0.2
      DO 2 I=1,5$A(I)=B(I)**4/4.
    2 CONTINUE
      CALL ARSIMP(5,H,A,R)
      PRINT 1,A,R,H
    1 FORMAT(10X,3H A=,5F10.5//10X,3H R=,F10.5//10X,3H H=,F5.1)
      END
*EXECUTE
*
*
