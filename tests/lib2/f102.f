      PROGRAM F102
      DIMENSION A(10,10), B(10,10), X(10,10), A1(10,10), SCR1(10,10)
      DIMENSION SCR2(10), SCR3(10), SCR4(10), SCR5(10), SCR6(10)
      LOGICAL SING
      INTEGER EX
C
C** READ IN ORIGINAL MATRIX A
C
 1    READ 1000, N
      IF(N.EQ.0) STOP
      DO  10  I=1,N
 10   READ 1010, (A(I,J),J=1,N)
      PRINT 2050
C
C** SAVE ORIGINAL MATRIX A IN A1
C
      DO  20  I=1,N
      DO  20  J=1,N
 20   A1(I,J) = A(I,J)
C
C** PRINT ORIGINAL MATRIX A
C
      PRINT 1020
      DO  30  I=1,N
 30   PRINT 1030, (A(I,J),J=1,N)
C
C** CALCULATION OF THE DETERMINANT OF A
C
      CALL LINVER (A,B,X,N,-1,DET,EX,CNR,SING,10,SCR1,SCR2,SCR3,SCR4,
     1 SCR5,SCR6)
C
C** TEST FOR SINGULARITY
C
      IF  (SING)  GO TO 40
      PRINT 1050
      GO TO 50
 40   PRINT 1060
C
C** PRINT DETERMINANT
C
 50   DET = DET * 10.**EX
      PRINT 1040, DET
      DO  60  I=1,N
      DO  60  J=1,N
 60   A(I,J) = A1(I,J)
C
C** INVERT A
C
      CALL LINVER (A,B,X,N,0,DET,EX,CNR,SING,10,SCR1,SCR2,SCR3,SCR4,
     1 SCR5,SCR6)
C
C** TEST FOR SINGULARITY
C
      IF  (SING)  GO TO 70
      PRINT 1050
      GO TO 75
 70   PRINT 1060
      GO TO 85
C
C** PRINT THE INVERSE OF A
C
 75   PRINT 1070
      DO  80  I=1,N
 80   PRINT 1030, (X(I,J),J=1,N)
C
C** PRINT ESTIMATE OF SPECTRAL NORM AND DETERMINANT
C
      PRINT 1080, CNR
 85   DET = DET * 10.**EX
      PRINT 1040, DET
      IF  (SING)  GO TO 1
C
C** VERIFY THE SOLUTION BY MULTIPLYING A WITH A-INVERSE
C
      DO  100  I=1,N
      DO  100  J=1,N
      SUM = 0.
      DO  90  K=1,N
 90   SUM = SUM + A1(I,K) * X(K,J)
 100  A(I,J) = SUM
C
C** PRINT PRODUCT (SHOULD BE UNITY MATRIX)
C
      PRINT 1090
      DO  110  I=1,N
 110  PRINT 1030, (A(I,J),J=1,N)
      DO  120  I=1,N
      DO  120  J=1,N
 120  A(I,J) = A1(I,J)
C
C** GENERATE N RIGHT HAND SIDES
C
      DO  130  I=1,N
      DO  130  J=1,N
 130  B(J,I) = I
C
C** PRINT RIGHT HAND SIDES
C
      PRINT 2000, N
      DO  140  I=1,N
 140  PRINT 1030, (B(I,J),J=1,N)
      PRINT 2030
C
C** SOLVE LINEAR SYSTEM WITH N RIGHT HAND SIDES
C
      CALL LINVER (A,B,X,N,N,DET,EX,CNR,SING,10,SCR1,SCR2,SCR3,SCR4,
     1 SCR5,SCR6)
C
C** TEST FOR SINGULARITY
C
      IF  (SING)  GO TO 150
      PRINT 1050
      GO TO 155
 150  PRINT 1060
      GO TO 170
C
C** PRINT SOLUTION
C
 155  PRINT 2010
      DO  160  I=1,N
 160  PRINT 1030, (X(I,J),J=1,N)
C
C** PRINT ESTIMATE OF SPECTRAL NORM AND DETERMINANT
C
      PRINT 1080, CNR
 170  DET = DET * 10.**EX
      PRINT 1040, DET
C
C** VERIFY SOLUTION BY MULTIPLYING A WITH SOLUTION X
C** TO OBTAIN RIGHT HAND SIDES AGAIN
C
      DO  190  I=1,N
      DO  190  J=1,N
      SUM = 0.
      DO  180  K=1,N
 180  SUM = SUM + A1(I,K) * X(K,J)
 190  A(I,J) = SUM
C
C** PRINT VERIFICATION
C
      PRINT 2020
      DO  200  I=1,N
 200  PRINT 1030, (A(I,J),J=1,N)
      GO TO 1
 1000 FORMAT (I2)
 1010 FORMAT (10F5.0)
 1020 FORMAT(/// 18HOORIGINAL MATRIX A///)
 1030 FORMAT (1H0 5E20.11)
 1040 FORMAT (/// 17H0DETERMINANT OF A,E25.16///70(1H-)///)
 1050 FORMAT (///  18H0A IS NOT SINGULAR)
 1060 FORMAT (///  53H0A IS FOUND TO BE SINGULAR DURING FURTHER CALCULAT
     1ION)
 1070 FORMAT (///  13H0INVERSE OF A///)
 1080 FORMAT (///  48H0ESTIMATE OF SPECTRAL NORM, WHICH IS THE PRODUCT//
     1/    43H OF THE NORM OF A AND THE NORM OF A-INVERSE,E25.16)
 1090 FORMAT (   ///  27H0PRODUCT OF A AND A-INVERSE///)
 2000 FORMAT(70(1H-)///I2, 17H RIGHT HAND SIDES///)
 2010 FORMAT    (///  70H0SOLUTION OF THE LINEAR SYSTEM WITH THE ABOVE P
     1RINTED RIGHT HAND SIDES///)
 2020 FORMAT (///  34H0VERIFIED SOLUTION - RESULT OF A*X///)
 2030 FORMAT(70(1H-)///)
 2050 FORMAT(1H1///50X,9HTEST F102///)
      END
      SUBROUTINE FINISH
      STOP
      RETURN
      END
*EXECUTE
 5
  129 -231  312 -175  276
 -348  624 -842  473 -745
  194 -347  468 -263  414
  271 -485  655 -368  580
 -206  370 -499  280 -442
 4
    1    2    3    4
    2    1    4    3
    3    4    1    2
    4    3    2    1
 3
    2   -6    2
   -6    1   -4
    2   -4   -3
*
*