      PROGRAM V100
      DIMENSION A(30)
      DO 1 K=1,30,2
      CALL RANNOR(X,Y)
      A(K)=X
  1   A(K+1)=Y
      PRINT 2,A
  2   FORMAT(3(10F12.7/))
      END
*EXECUTE
*
*
