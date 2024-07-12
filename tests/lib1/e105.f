      PROGRAM E105
      DIMENSION F(10),X(10)
      DATA F/1.0,1.21,1.44,1.69,1.96,2.25,2.56,2.89,3.24,3.61/
      DATA X/1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9/
      PRINT 10,F,X
      N=10
      Z=1.3855
      M=2
      PRINT 1,Z,M
      Y=DIVDIF(F,X,N,Z,M)
      PRINT 2,Y
      M=11
      PRINT 1,Z,M
      Y=DIVDIF(F,X,N,Z,M)
      M=1
      Z=2.
      PRINT 1,Z,M
      Y=DIVDIF(F,X,N,Z,M)
  1   FORMAT(20X,2HZ=,F8.4,5X,2HM=,I2)
  2   FORMAT(20X,2HY=,F15.10/)
  10  FORMAT(//50X,9HTEST E105///20X,2HF=,10(F6.2,1H,)//20X,2HX=,10(F6.2
     1,1H,)//)
      END
*EXECUTE
*
*
