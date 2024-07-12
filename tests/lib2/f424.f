      PROGRAM F424
C         *FILE:SCRATCH,F01,W,100
C         *FILE:SCRATCH,F02,W,100
      DIMENSION A(100),A1(100),III(20),NN(20),C(20)
      EXTERNAL FUNCT
      NDIM=20
      N1=NDIM-1
      C(1)=2.*1+1.*2
      DO 1 K=2,N1
  1   C(K)=(K-1)*1.+K*2.+(K+1)*1.
      C(NDIM)=N1*1.+NDIM*2.
      LB=100
      IND=1
      CALL SQROOT(A,A1,III,NN,NDIM,C,LB,IND,FUNCT)
      PRINT 100,C
 100  FORMAT(1X,5E20.10)
      STOP
      END
      FUNCTION FUNCT(L,M)
      IF(L.EQ.M) GO TO 1
      IF(IABS(L-M).EQ.1) GO TO 2
      FUNCT=0.
      RETURN
  1   FUNCT=2.
      RETURN
  2   FUNCT=1.
      RETURN
      END
*CALL PTIME
*EXECUTE
*
*
