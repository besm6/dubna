      PROGRAM V104
         DIMENSION A(100),IB(100)
      DO 111   IX=1,3
         DO 2 I=1,100
 2       A(I) = RNDM(-1.0)
         DO 1 I=1,100
  1   IB(I)=RNDM(1.0)
         PRINT 10
         PRINT 11,(A(I),I=1,100)
         PRINT 12
         PRINT 13,(IB(I),I=1,100)
         PRINT 14
      PRINT 15,(IB(I),I=1,100)
 111   CONTINUE
  10     FORMAT(1H1,10X,19HRANDOM NUMBER FLOAT/)
 11      FORMAT(5X,5F20.12)
  12     FORMAT(/10X,14HNUMBER INTEGER/)
 13   FORMAT(5X,5I18)
  14     FORMAT(/10X,10HOCT NUMBER/)
  15  FORMAT(5X,5O20)
      END
*EXECUTE
*
*
