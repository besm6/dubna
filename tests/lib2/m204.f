      PROGRAM M204
      COMPLEX Z
      INTEGER CFIX
      PRINT 10
      Z=(-2.2,5.3)
      DO 2 I=1,10
      Z=Z+(0.5,-0.5)
      M=IFIX(REAL(Z))
      K=CFIX(Z)
  2   PRINT 1,Z,K,M
  1   FORMAT(20X,1H(,F6.2,1H,,F6.2,1H),10X,I3,10X,I3)
  10  FORMAT(//50X,9HTEST M204///27X,1HZ,20X,1HK,12X,1HM/)
      STOP
      END
*EXECUTE
*
*
