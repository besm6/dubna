      PROGRAM C332 B
      DOUBLE PRECISION RHO,PHI,PHI1,A,PI
      COMMON J
      DATA PI / 3.1415926535897932384626433832D0 /
      M  = 0
      PHI1 = PI/1.8D+02
      READ 1001, MM
 1001 FORMAT (I2)
    1 M = M + 1
      READ 1000, NMAX,RHO
 1000 FORMAT (I2,  D10.3)
      N = 0
    2 N = N + 1
      READ 1002,A
 1002 FORMAT (D10.3)
      JMIN = -150
      DO 10        J=JMIN,150,30
      PRINT 2000
 2000 FORMAT(1H1//50X,9HTEST C332//)
      PHI = PHI1 * DBLE(FLOAT(J))
   10 CALL COBESJ(RHO,PHI,A,NMAX)
      IF (N   .LT.   4)                GOTO 2
      IF (M   .LT.   MM)               GOTO 1
      STOP
      END
      SUBROUTINE COBESJ(RHO,PHI,A,NN)
      DOUBLE PRECISION RHO,PHI,A,S1,S2,C,S,X,Y,SUM1,SUM2,P,Q,R,DGAMMA,
     1                 K1,U(101),V(101)
      COMMON JJ
      P = (RHO/2.D 0)**A
      C = DCOS(A*PHI) * P
      S = DSIN(A*PHI) * P
      X = RHO * DCOS(PHI)
      Y = RHO * DSIN(PHI)
      CALL DBESCJ(X,Y,0.D 0,0,23,U,V)
      SUM1 = C * U(1)  -  S * V(1)
      SUM2 = C * V(1)  +  S * U(1)
      CALL TIMEZB(I1)
      Y1=I1
      CALL DBESCJ(X,Y,A,NN+NN,23,U,V)
      CALL TIMEZB(I2)
      Y2=I2-Y1
      PRINT 2010,Y2
 2010 FORMAT(10X,  11HTIME TAKEN=F10.4,  4HSECS)
      Q  = DGAMMA(1.D 0 + A)
      S1 = Q * U(1)
      S2 = Q * V(1)
      P  = Q/A
      PRINT 1001, RHO,JJ,X,Y
 1001 FORMAT (/, 3X, D25.18, 8X, I5, 8X, 2(3X, D25.18), /////)
      PRINT 1002, SUM1,SUM2
 1002 FORMAT (/, 2(3X, D35.23), /////)
      K = 0
   10 R  = DSQRT(((SUM1-S1)**2 + (SUM2-S2)**2)/(SUM1**2 + SUM2**2))
      PRINT 1000, A,K,R,S1,S2
 1000 FORMAT (/, 3X, D9.2, 3X, I3, 10X, D9.2, 8X, 2(3X, D35.23))
      K = K + 1
      IF (K   .GT.   NN)               RETURN
      K1 = DBLE(FLOAT(K))
      P  = -P * ((K1+A-1.D 0)/K1)**2
      Q  =  P *  (K1+K1+A)
      S1 = S1  +  Q * U(K+K+1)
      S2 = S2  +  Q * V(K+K+1)
      GOTO 10
      END
*EXECUTE
 1
13   2.0D+00
   2.0D-01
   4.0D-01
   6.0D-01
   8.0D-01
*
*
