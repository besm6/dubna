      PROGRAM M502
      DIMENSION ICODE(2),IOPT(20)
      DATA ICODE /4HWDM.,5HSVNP. /
C--                TEST FOR UOPT
      PRINT 1000
      PRINT 100
      DO 2 I=1,2
      CALL UOPT (ICODE(I),13HSVWMDCHEFVONP,IOPT,13)
      PRINT 20,(IOPT(J),J=1,13)
    2 CONTINUE
   20 FORMAT (20X,13I3)
  100 FORMAT(20X,13HTEST FOR UOPT)
 1000 FORMAT(1H1)
      END
*EXECUTE
*
*
