      PROGRAM C330 B
      DOUBLE PRECISION X,A
      EXTERNAL DBESJN
      READ 2000, MM
      M = 0
      X = 1.D 0
    1 M = M + 1
      PRINT 2001
      READ 2002, A
      DO 10 J=1,91,5
      K=J-1
   10 CALL BESSEL(X,A,K,DBESJN)
      IF (M   .LT.   MM)               GOTO 1
 2000 FORMAT (I3)
 2001 FORMAT(1H1//50X,9HTEST C330//6X,1HX,10X,1HA,10X,2HNN,20X,2HY1,30X,
     *    2HY2,25X,2HY3/)
 2002 FORMAT(D10.1)
      STOP
      END
      SUBROUTINE BESSEL(X,A,NN,TRBES)
      DOUBLE PRECISION X,A,J10(101),J11(101),J12(101),SUM,Y1,Y2,Y3,AA
      N2 = 2
      AA = A + A
      IF (AA   .LT.   1.D 0)           GOTO 1
      AA = AA - 1.D 0
      N2 = N2 + 1
    1 CALL TRBES(X+X,AA,N2,23,J10)
      CALL TRBES(X,A,NN+2,23,J11)
      CALL TRBES(X,A,-NN,23,J12)
      Y1 = J10(N2+1)
      Y2 = J11(2) * J11(2)
      SUM = 0.0D 0
      NN1 = NN + 1
      DO 10        KK=1,NN1
   10 SUM = SUM  +  J12(KK) * J11(KK+2)
      Y2 = Y2 + SUM + SUM
      Y3 = DABS(Y1 - Y2)/Y1
      PRINT 1000, X,A,NN,Y1,Y2,Y3
 1000 FORMAT (/, 2(3X, D9.2), 3X, I3, 8X, 2(2X, D30.23),  5X, D10.3)
      RETURN
      END
*EXECUTE
  5
   2.0D-01
   4.0D-01
   6.0D-01
   8.0D-01
   0.0D-01
*
*
