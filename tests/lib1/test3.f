      PROGRAM TEST3
      EXTERNAL K,COS
      REAL F(37),X(37),Y(37),DY(37),A(1443),R(37)
      REAL P,PI,H,XI,A1
      INTEGER N,I,IER
      COMMON /MULT/ P
      PI=3.14159265359
      P=.3/PI
      N=37
      H=2.*PI/FLOAT(N-1)
      DO 1 I=1,N
        XI=-PI+H*FLOAT(I-1)
        X(I)=XI
        A1=128./17.*COS(2.*XI)
 1      F(I)=COS(XI)*(8.5+A1)+16.5-16.*SIN(XI)**2-A1
      CALL FREST1(F,X,N,K,COS,Y,DY,IER,A)
      PRINT 2,IER
 2    FORMAT(6H TEST3/14H FREST1   IER=,I2)
      IF(IER.GT.0) GO TO 6
      PRINT 3,(Y(I),I=1,N)
 3    FORMAT(14H SOLUTON Y(X)=/(8E15.7))
      PRINT 4,(DY(I),I=1,N)
 4    FORMAT(34H ERRORS OF SOLUTON ABS(DELTAY(X))=/(8E15.7))
      CALL REM(Y,X,N,K,R)
      PRINT 5,(R(I),I=1,N)
 5    FORMAT(32H REM   REMAINDERS OF QUADRATURE=/(8E15.7))
      PRINT 20
 20   FORMAT(///50X,16H E N D     TEST3)
 6    CALL EXIT
      END
      REAL FUNCTION K(X,S)
      REAL X,S,P
      COMMON /MULT/ P
      K=P/(.64*COS((X+S)/2.)**2-1.)
      RETURN
      END
*EXECUTE
