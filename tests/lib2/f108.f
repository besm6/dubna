      PROGRAM F108
      DIMENSION A(3,3,3) ,B(3,3),C(3,3)
      COMPLEX A,B,C
      READ1,A  $  PRINT 12  $  PRINT 2, (((A(I,J,K),J=1,3),I=1,3),K=1,1)
      PRINT 3  $  PRINT 2,(((A(I,J,K),J=1,3),I=1,3),K=2,2)
      PRINT 5  $  PRINT 2,(((A(I,J,K),J=1,3),I=1,3),K=3,3)
      M=3  $  N=3
      CALL MUXMAC(A,C,M,N,B)
      PRINT 4  $  PRINT 2,((C(I,J),J=1,3),I=1,3)   $  STOP
 1    FORMAT(20F4.1)
 2    FORMAT(3(10X,2F9.2,5X))
 3    FORMAT(///3X,'MATRIX 2'//)
   4   FORMAT(///3X' RESULT'//)
  5   FORMAT(///3X,'MATRIX 3'//)
 12   FORMAT(1H1,3X,'MATRIX 1'//)
      END
*EXECUTE
 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.010.011.012.013.014.015.016.017.018.
 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 1.0 1.0
 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
*
*
