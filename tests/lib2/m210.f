         PROGRAM M210
         DIMENSION A(10),ADB(10)
         DATA A/3.0,10.0,3.5,6.,100.,4.6,5.7,64.,0.5,1./
      PRINT 10,A
      M=1
      CALL DEZBEL(A,ADB,10,M)
      PRINT 1,M,ADB
      M=2
      CALL DEZBEL(A,ADB,10,M)
      PRINT 1,M,ADB
  1   FORMAT(5X,2HM=,I2//5X,4HADB=/2(3X,5F17.11/)///)
  10  FORMAT(//50X,9HTEST M210///10X,2HA=,10F7.2////)
      STOP
      END
*EXECUTE
*
*
