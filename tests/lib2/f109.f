      PROGRAM F109
      DIMENSION A(3,3),B(3,1)
      DATA M,N/3,1/
      DO 5  I=1,M
  5   READ 100,(A(I,J),J=1,M)
 100  FORMAT(13(F10.6))
      DO 10  I=1,M
  10  READ 100,(B(I,J),J=1,N)
      PRINT 200
 200  FORMAT(1H1,20X,8HMATRIX A//)
      DO 15  I=1,M
  15  PRINT 300,(A(I,J),J=1,M)
 300  FORMAT(1H0,9(4X,F10.6))
      PRINT 400
 400  FORMAT(//1H0,20X,8HMATRIX B//)
      DO 20  I=1,M
  20  PRINT 500,(B(I,J),J=1,N)
 500  FORMAT(1H0,10X,9(F10.6,4X))
      I=M
      J=N
      CALL MXEQU(A,B,I,J)
      PRINT 600
 600  FORMAT(//1H0,20X,8HMATRIX X//)
      DO 25  I=1,M
  25  PRINT 500,(B(I,J),J=1,N)
      END
*EXECUTE
10.0       1.0       2.0
1.0         5.0      2.0
2.0       2.0         7.0
13.0
8.0
11.0
*
*
