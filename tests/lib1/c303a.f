      PROGRAM C303 A
      DIMENSION BJRE(50),BJIM(50),YRE(50),YIM(50)
      READ 100,NUMBER
      DO 20 I=1,NUMBER
      READ 200,N,ALPHA,BETA,X,Y
      PRINT 300,X,Y,ALPHA,BETA
      PRINT 400
C
      CALL COMBES(X,Y,ALPHA,BETA,N,BJRE,BJIM,YRE,YIM,NY)
C     CALL CBESJY(X,Y,ALPHA,BETA,N,BJRE,BJIM,YRE,YIM,3)
      N1=N+1
      DO 10 L=1,N1
      K=L-1
      PIX=ALPHA+BETA+K
  10  PRINT 500,PIX,BJRE(K+1),BJIM(K+1),YRE(K+1),YIM(K+1)
  20  CONTINUE
  100 FORMAT(I3)
  200 FORMAT(I2,8X,4F10.5)
  300  FORMAT(1H4,1X,20H THE RESULTS FOR X =,F10.6,8H     Y =,F10.6,
     *   9H   ARE :  ///29H        REAL PART OF ORDER = ,F10.6/
     *   29H   IMADINARY PART OF ORDER = ,F10.6//)
  400 FORMAT( 9X, 5HORDER,11X,7HJ(REAL),15X,8HJ(IMAG.),17X,7HY(REAL),
     *   15X,8HY(IMAG.)/)
  500 FORMAT(1H0,4X,F6.3,5X,E17.10,5X,E17.10,8X,E17.10,5X,E17.10)
      STOP
      END
*EXECUTE
 14
10         0.0       0.0       0.2       0.0
10         0.0       0.0       1.5       0.0
10         0.0       0.0      10.4       0.0
10         0.0       0.0       1.4       0.0
 2         0.0       0.0        0.0866   0.05
 1         0.0       0.0       1.38563   0.8
 1         0.0       0.0       6.2354    3.6
 2         0.0       0.0       0.77643   2.89776
 4         0.0       0.0       2.98128   1.92834
 2         0.0       0.0       6.12832   5.14224
 2         0.0       0.0       2.71898   1.26783
 3         0.0       0.0       7.25048   3.38086
 2         0.0       0.0       3.0       0.0
 2         0.0       0.0       8.0       0.0
*
*
