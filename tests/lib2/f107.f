      PROGRAM F107
      DIMENSION A(4,4)
      DATA (((A(I,J),I=1,4),J=1,4)=5(1.),3(2.),1.,2.,3.,3.,1.,2.,3.,4.)
      PRINT 10,((A(I,J),J=1,4),I=1,4)
      CALL SMXINV(A,4,R)
      PRINT 11,((A(I,J),J=1,4),I=1,4)
 10   FORMAT(//3X,6HMATRIX//1(4F8.2))
 11   FORMAT(//3X,14HINVERSE MATRIX//1(4F8.2))
      END
*EXECUTE
*
*
