      PROGRAM F240
      DIMENSION H ( 4,4 ), U ( 4,4 ), X ( 4 )
      PRINT 8
      M=1
      ACCURA = 0.65
  10  A=A
      H ( 1,1 ) = 1.00
      H ( 1,2 ) = 0.42
      H ( 1,3 ) = 0.54
      H ( 1,4 ) = 0.66
      H ( 2,1 ) = 0.42
      H ( 2,2 ) = 1.00
      H ( 2,3 ) = 0.32
      H ( 2,4 ) = 0.44
      H ( 3,1 ) = 0.54
      H ( 3,2 ) = 0.32
      H ( 3,3 ) = 1.00
      H ( 3,4 ) = 0.22
      H ( 4,1 ) = 0.66
      H ( 4,2 ) = 0.44
      H ( 4,3 ) = 0.22
      H ( 4,4 ) = 1.00
      CALL KIM ( H, 4, 0, U, NR, 4, X, 0, ACCURA )
      PRINT 100, H
      PRINT 100, U
      PRINT 101, X, NR, M
      ACCURA = 0.00161637
      IF ( M - 2 ) 20,20,30
  20  M=3
      GO TO 10
   30 IF ( M - 4 ) 40,40,50
  40  M=5
      ACCURA = 0.0000001
      GO TO 10
   50 X (1) = 2.32274880
      X ( 2 ) = 0.79670669
      X ( 3 ) = 0.63828380
      X ( 4 ) = 0.24226071
      PRINT 102, X
      STOP
  8      FORMAT( 30X,'EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC M
     1ATRIX',20X,'TEST F220'/30X,55(1H*),20X,9(1H*)//)
  100 FORMAT ( //   (4 (  F19.9 )))
  101 FORMAT ( 6X, 4F19.9, 6X3HNR=I5, 6X2HM=I5 )
  102 FORMAT ( 6X4F19.9, 12HEXACT EVALUE )
      END
*EXECUTE
*
*
