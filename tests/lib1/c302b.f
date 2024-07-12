      PROGRAM C302 B
      DIMENSION B(4),X(4)
      DATA X/6.,-45.,45.,-20./
      DO 2 I=1,4
      CALL BESSEL(B,X(I),1.)
      PRINT 1,B
 2    CONTINUE
      DO 20 I=1,4
      CALL BESSEL(B,X(I),-1.)
      PRINT 1,B
 20   CONTINUE
 1    FORMAT(///'     B=',4E20.11)
      STOP
      END
*EXECUTE
*
*
