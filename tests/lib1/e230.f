      PROGRAM E230
      DIMENSION A (6,5),B (6,3),X (5,3),AUX(10),IPIV(6),E(5,5)
      COMMON /TLSDIM/M1,M,N,L,IER
      READ 100,NO
      PRINT 100,NO
 100  FORMAT(I2)
      DO 35  INT=1,NO
      READ 150,M1,M,N,L,EPS
      PRINT 150,M1,M,N,L,EPS
 150  FORMAT(4I5,F15.3)
      I1=1
      I2=N
      DO 5  J=1,M
      READ 200,(A (I),I=I1,I2)
 200  FORMAT(8F10.5)
      I2=I2+N
  5   I1=I1+N
      I1=1
      I2=L
      DO 10  J=1,M
      READ 300,(B (I),I=I1,I2)
 300  FORMAT(5F15.12)
      I2=I2+L
  10  I1=I1+L
      PRINT 350,INT
 350  FORMAT(    25H**** TEST EXAMPLE NUMBER ,I2,5H ****////)
      PRINT 400
 400  FORMAT(1H0,35X,8HMATRIX A///)
      I1=1
      I2=N
      DO 15  J=1,M
      PRINT 500,(A (I),I=I1,I2)
 500  FORMAT(1H0,7(E14.7,5X))
      I2=I2+N
  15  I1=I1+N
      PRINT 600
 600  FORMAT(////1H ,35X,8HMATRIX B///)
      I1=1
      I2=L
      DO 20  J=1,M
      PRINT 700,(B (I),I=I1,I2)
 700  FORMAT(1H0,10X,6(E14.7,5X))
      I2=I2+L
  20  I1=I1+L
      IF(M1.EQ.0) GO TO 1
      PRINT 810
 810  FORMAT(50X,'CALL TLSC')
      CALL TLSC(A,B,AUX,IPIV,EPS,X)
      GO TO 2
  1   PRINT 820
 820  FORMAT(50X,'CALL TLS')
      CALL TLS (A,B,AUX,IPIV,EPS,X)
 2    PRINT 800
 800  FORMAT(1H0,35X,8HMATRIX X///)
      I1=1
      I2=L
      DO 30  J=1,N
      PRINT 900,(X (I),I=I1,I2)
 900  FORMAT(1H0,10X,5(E17.10,5X))
      I2=I2+L
  30  I1=I1+L
      PRINT 3,IER
  3   FORMAT(5X,4HIER=,I6)
      IF(IER.EQ.-1001) GO TO 35
      CALL TLERR(A,E,AUX,IPIV)
      PRINT 910
 910  FORMAT(1H0,10X,'COVARIANCE  MATRIX')
      I1=1
      I2=N
      DO 31 J=1,N
      PRINT 900,(E(I),I=I1,I2)
      I2=I2+N
  31  I1=I1+N
      CALL TLRES(A,B,AUX)
      PRINT 920
 920  FORMAT(///1H0,10X,15HRESIDUAL MATRIX)
      I1=1
      I2=L
      DO 34  J=1,M
      PRINT 900,(B (I),I=I1,I2)
      I2=I2+L
  34  I1=I1+L
  35  CONTINUE
      STOP
      END
*CALL PTIME
*EXECUTE
 7
    2    6    5    2          0.1
     3.6E1    -6.3E2    3.36E3   -7.56E3    7.56E3
    -6.3E2    1.47E4   -8.82E4  2.1168E5  -2.205E5
    3.36E3   -8.82E4  5.6448E5 -1.4112E6   1.512E6
   -7.56E3  2.1168E5 -1.4112E6  3.6288E6  -3.969E6
    7.56E3  -2.205E5   1.512E6  -3.969E6    4.41E6
  -2.772E3   8.316E4 -5.8212E5 1.55232E6-1.74636E6
         4.63E2       -4.157E3
       -1.386E4       -1.782E4
        9.702E4       9.3555E4
      -2.5872E5       -2.618E5
       2.9106E5      2.88288E5
     -1.16424E5     -1.18944E5
    2    2    2    1          0.1
6.0       13.5
13.5      39.75
63.5
184.0
    2    2    2    1          0.1
 2.0        1.0
 2.0        1.0
 10.0
 8.0
    2    2    2    1          0.1
  4.0      10.0
 10.0      30.0
  40.0
  50.0
    0    3    3    2          0.1
  9.0     42.0      248.5
 42.0     248.5     1656.78
 248.5    1656.78   11806.36
  22.5            63.0
  127.75         345.0
     839.64        2217.50
    1    3    3    2          0.1
  6.0     34.0      380.82
  34.0    380.82    4859.62
 380.82   4859.62   66335.51
  9845.7       9841.1
 121962.5      121901.64
1646182.842    1645105.67
    0    6    2    1          0.001
1.0         1.0
1.0        2.0
1.0        3.0
1.0         4.0
1.0         5.0
1.0        6.0
1.0
1.0
2.0
2.0
3.0
3.0
*
*
