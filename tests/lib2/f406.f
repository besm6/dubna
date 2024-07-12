      PROGRAM F406
      DIMENSION ABAND(3,3),F(3,1)
      DIMENSION A(10,3) ,B(10,5),A1(10,3) ,C(10,5)
      DOUBLE PRECISION D(10,3) ,E(10,3)
      DATA ABAND/1.,2.,1.,2.,2.,1.,0.,1.,0./,F/3.,6.,3./
      DATA A/2.,10*-1.,9*2.,0.,8*-1.,0./,
     * A1/2.005,-0.997,-0.994,-0.991,-0.988,-0.985,-0.982,-0.979,-0.976,
     1    -0.973,-0.997,2.01,2.015,2.02,2.025,2.03,2.035,2.04,2.045,2.05
     2  ,0.,-0.994,-0.991,-0.988,-0.985,-0.982,-0.979,-0.976,-0.973,0./,
     * D/2.005D0,-0.997D0,-0.994D0,-0.991D0,-0.988D0,-0.985D0,-0.982D0,
     1   -0.979D0,-0.976D0,-0.973D0,-0.997D0,2.01D0,2.015D0,2.02D0,
     2   2.025D0,2.03D0,2.035D0,2.04D0,2.045D0,2.05D0,0.D0,-0.994D0,
     3   -0.991D0,-0.988D0,-0.985D0,-0.982D0,-0.979D0,-0.976D0,-0.973D0,
     4   0.D0/,
     *     B/1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,
     1       1.,-2.,3.,-4.,5.,-6.,7.,-8.,9.,-10.,
     2       10.,9.,8.,7.,6.,5.,4.,3.,2.,1.,
     3       10.,-9.,8.,-7.,6.,-5.,4.,-3.,2.,-1.,
     4      1.111,2.222,3.333,4.444,5.555,6.666,7.777,8.888,9.999,11.11/
C                  T E C T   F403
      CALL RBEQN(3,1,ABAND,3,IFAIL,1,F)
      PRINT 1,IFAIL
      PRINT 6,F
C                  T E C T   F420
      DO 10 I=1,10
      DO 10 J=1,5
  10  C(I,J)=B(I,J)
      DO 30 I=1,10
      DO 30 J=1,3
  30  E(I,J)= B(I,J)
      PRINT 3,((A(I,J),J=1,3),I=1,10)
      PRINT 4
      PRINT 2,((B(I,J),J=1,5),I=1,10)
      CALL RBEQN(10,1,A,10,IFAIL,5,B)
      PRINT 1,IFAIL
      PRINT 5
      PRINT 2,((B(I,J),J=1,5),I=1,10)
      PRINT 3,((A1(I,J),J=1,3),I=1,10)
      PRINT 4
      PRINT 2,((C(I,J),J=1,5),I=1,10)
      CALL RBEQN(10,1,A1,10,IFAIL,5,C)
      PRINT 1,IFAIL
      PRINT 5
      PRINT 2,((C(I,J),J=1,5),I=1,10)
      PRINT 13,((D(I,J),J=1,3),I=1,10)
      PRINT 4
      PRINT 12,((E(I,J),J=1,3),I=1,10)
      CALL DBEQN(10,1,D,10,IFAIL,3,E)
      PRINT 1,IFAIL
      PRINT 5
      PRINT 12,((E(I,J),J=1,3),I=1,10)
      STOP
  1   FORMAT(//10X,6HIFAIL=,I2)
  2   FORMAT(10X,5E20.10)
  3   FORMAT(1H1,50X,11HBAND MATRIX//(25X,3E20.10))
  4   FORMAT(//50X,16HRIGHT-HAND SIDES//)
  5   FORMAT(//50X,9HSOLUTIONS//)
  6    FORMAT(10X,3E20.10)
 12   FORMAT(10X,3D30.20)
  13  FORMAT(1H1,50X,11HBAND MATRIX//(25X,3D30.20))
      END
*CALL PTIME
*EXECUTE
*
*
