      PROGRAM C330 A
       DOUBLE PRECISION X,A,J(101)
      DIMENSION NN(5)
      DATA NN/-2,0,2,10,20/
      PRINT 10
      X=1.D0
  6   DO 3 K=1,5
      A=0.D0
      DO 2 I=1,4
      CALL DBESJN(X,A,NN(K),23,J)
      N1=IABS(NN(K))+1
      PRINT 4,X,A,NN(K),J(2),J(N1)
      A=A+0.2D0
 2     CONTINUE
 3     CONTINUE
      X=X+1.D0
      IF(X.GE.3.D0) GO TO 5
      GO TO 6
  4   FORMAT(1X,2D10.1,I4,2D40.23)
  10  FORMAT(1H1//50X,9HTEST C 30//8X,1HX,7X,1HA,6X,2HNN,17X,4HJ(2),
     *     40X,7HJ(NN+1)/)
  5   STOP
       END
*EXECUTE
*
*
