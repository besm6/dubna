      PROGRAM C303 B
      DIMENSION BJRE(50),BJIM(50),YRE(50),YIM(50)
      PRINT 1
      N=6
      M=1
      A=0.
      B=1.
      X=1.
      Y=1.
      DO 10 K=M,N
      CALL COMBES(X,Y,A,B,K,BJRE,BJIM,YRE,YIM,NY)
      K1=K+1
      PRINT 2,X,Y,A,B,K,(BJRE(L),BJIM(L),YRE(L),YIM(L),L=1,K1)
      X=X+0.01
      Y=Y+0.02
      A=A+0.001
  10  B=B+0.005
  1   FORMAT(30X,20H TEST FOR C303         //2X,3H  X,5X,3H  Y,10X,
     *6H ALPHA,10X,5H BETA, 5X,3HORD,7X,5H BJRE,12X,5H BJIM,12X,4H YRE,
     *12X,4H YIM)
  2   FORMAT(///1X,2F8.3,2E15.6,I4,4E17.8,(/48X,4E17.8))
      STOP
      END
*EXECUTE
*
*
