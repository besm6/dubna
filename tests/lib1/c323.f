      PROGRAM C323
      DIMENSION Y(4,4),P(4),N(4),Z(4,4),T(4,4)
      INTEGER P
      P(1)=1
      DO 2 J=1,3
      N(1)=1
      DO 1 I=1,3
      X=0.5
      Y(I,J)=GPLOG(N(I),P(J),0.5)
      PRINT 6,X,N(I),P(J),Y(I,J)
      X=-1.
      Z(I,J)=GPLOG(N(I),P(J),-1.)
      PRINT 6,X,N(I),P(J),Z(I,J)
      X=1.
      T(I,J)=GPLOG(N(I),P(J),1.)
      PRINT 6,X,N(I),P(J),T(I,J)
      N(I+1)=N(I)+1
  1   CONTINUE
       P(J+1)=P(J)+1
  2   CONTINUE
  6   FORMAT(2X,3H X=,F7.3,10X,3H N=,I4,10X,3H P=,I4,10X,3H Y=,F17.11)
      END
*EXECUTE
*
*
