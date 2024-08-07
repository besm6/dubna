      PROGRAM C206 B
      DIMENSION A(20),ROOT(20),H(20)
      COMPLEX A, ROOT
      COMPLEX F,X
      A(1)=1.0
      DO 30 L=1,19
 30   A(L+1)=A(L)+1.0
      MAXFUN=1000
      MAXFUN=200
      MAXFUN=100
      MODE=0
      N=5
      DO 20 K=1,10
      J=N+1
      PRINT 10, N
      PRINT 11, (A(I),I=1,J)
      CALL POLY2(A,N,ROOT,H,MAXFUN,MODE)
      PRINT 12
      PRINT 11, (ROOT(I),I=1,N)
      PRINT 13
      PRINT 14, (H(I),I=1,N)
      PRINT 102
      DO 100 L=1,N
      X=ROOT(L)
      F=A(1)*X+A(2)
      DO 101  M=3,J
 101   F=F*X+A(M)
      PRINT 103  ,X,F
 100   CONTINUE
      N=N+1
 20   CONTINUE
  10  FORMAT(1H1//50X'TEST C206'//10X'N='I3//10X'COEFFICIENTS'//)
  11  FORMAT(3X,3(2D15.7,3X))
  12  FORMAT(//10X'ROOT'//)
  13  FORMAT(//10X'ESTIMATION OF ERROR'//)
  14  FORMAT(3X,6E18.7)
 102  FORMAT(//10X,11HERROR CHECK//)
103    FORMAT(3X,2HX=,2D20.10,5X,5HF(X)=,2D20.10)
      END
*CALL PTIME
*EXECUTE
*
*
