      PROGRAM C324
      DIMENSION N(4),X(20)
      INTEGER P(4)
      COMPLEX CGPLOG
      COMPLEX Y(10,10)
      DO 3 K=1,5
      X(1)=-3.0
      P(1)=1
      DO 2 J=1,3
      N(1)=1
      DO 1 I=1,3
      Y(I,J)=CGPLOG(N(I),P(J),X(K))
      N(I+1)=N(I)+1
      PRINT 6,X(K),N(I),P(J)
      PRINT 5,Y(I,J)
    1 CONTINUE
      P(J+1)=P(J)+1
    2 CONTINUE
      X(K+1)=X(K)+1.0
    3 CONTINUE
    5 FORMAT(3X,7HCGPLOG=,2F17.10,//)
    6 FORMAT(///,10X,3H X=,F17.10,3H N=,I4,3H P=,I4,//)
      END
*CALL PTIME
*EXECUTE
*
