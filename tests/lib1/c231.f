      PROGRAM C231
      C=-1.5
      X=-1.E-18
      EPS=0.
      XB=0.
   1  CONTINUE
      X=C*X
      Y=CBRT(X)
      Z=Y**3
      D=ABS((X-Z)/(X+Z))/3.
      PRINT 105,LL,E,R,F,FP,G,GP,S
 100  FORMAT(1X,4E25.12)
      IF(D.LE.EPS)  GO TO 10
      EPS=D
      XB=X
  10  CONTINUE
      IF(ABS(X).LT.1.E17) GO TO 1
      PRINT 50,EPS,XB
  50  FORMAT(' EPS='E12.3,5X'XBAD='E12.3)
      N=100 000
      X=1.
      C=1.-1.E-5
      CALL CTIME(CT,RT)
      DO 20 I=1,N
      X=-C*X
      Y=CBRT(X)
  20  CONTINUE
      CALL CTIME(CT,RT1)
      TIME=(RT1-RT)/N*1.E6
      PRINT 200,TIME
 200  FORMAT(' BPEMЯ CЧETA PABHO'E10.2,5X'MKCEK.')
      X=1.
      CALL CTIME(CT,RT)
      DO 30 I=1,N
      X= C*X
      Y=X**.33333333333
   30 CONTINUE
      CALL CTIME(CT,RT1)
      TIME=(RT1-RT)/N*1.E6
      PRINT 200,TIME
      X=1.
      CALL CTIME(CT,RT)
      DO 40 I=1,N
      X=-C*X
   40 CONTINUE
      CALL CTIME(CT,RT1)
      TIME=(RT1-RT)/N*1.E6
      PRINT 200,TIME
      END
*EXECUTE
*
*
