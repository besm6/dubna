      PROGRAM V305
      DIMENSION IA1(5)
      DATA IA1/2,2,2,2,2/
C--                TEST FOR IULAST
      PRINT 50
      N=5
      ITEXT=2
      K1=1
    9 IPOS=IULAST(ITEXT,IA1,N)
      PRINT 60,(IA1(J),J=1,5),IPOS
      IF (K1.EQ.0) GO TO 10
      IA1(2)=3
      IA1(3)=3
      K1=K1-1
      GO TO 9
   10 CONTINUE
  50  FORMAT(//30X'TEST FOR IULAST'//)
   60 FORMAT (20X,5I5/ 20X,7HIULAST=,I5)
      END
*EXECUTE
*
*
