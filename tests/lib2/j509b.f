      PROGRAM J509 B
      DIMENSION F(115,20)
      PI=4.*ATAN(1.)
      DO 5 K=1,2
      IF(K.EQ.2) GO TO 10
      FMIN=-0.22
      FMAX=0.32
      NTC=10
      GO TO 15
 10   FMIN=0.
      FMAX=0.
      NTC=8
 15   M=113
      N=17
      PRINT 1008
 1008 FORMAT (1H1)
      DX=1.
      DY=1.
      DO 1 I=1,M
      X=FLOAT(I-1)*DX
      DO 1 J=1,N
      Y=FLOAT(J-1)*DY
 1    F(I,J)=-0.3+(0.5+0.3*SIN(PI*X/56.))*(0.5+0.3*SIN(PI*Y/8.))
      DO 2 I=1,M
 2    PRINT 1009,(F(I,J),J=1,N)
 1009 FORMAT(/(10F9.3))
      CALL CONPRT(F,115,20,M,N,NTC,FMIN,FMAX)
 5    CONTINUE
      STOP
      END
*EXECUTE
*
*
