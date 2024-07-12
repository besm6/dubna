      PROGRAM F115
      DIMENSION A(3,3),D(3),B(3,3)
      DATA(A=0.5,0.1,0.4,0.1,1.0,0.3,2.,3.,1.)
      D(1)=1.
      D(2)=1.5
      D(3)=0.5
      CALL MXDMA(A,2,D,B,3)
      PRINT 1,A,D,B
  1   FORMAT(///20X,8HMATRIX A//20X,3(3F10.3)///20X,8HVECTOR D//20X,
     13F10.3///20X,8HMATRIX B//20X,3(3F10.3))
      END
*EXECUTE
*
*
