      PROGRAM D109
        DOUBLE PRECISION DGAUSS,DY(5),F,A,B,C(5),EPS
      EXTERNAL F
      PRINT 5
  5   FORMAT (1H1, ' * * * * * DOUBLE PRECISION GAUSSIAN INTEGRATION
     * * * * * * TEST  D109 * * * * * ')
      A=0.D0
      C(1)=1.570796 D0
      EPS=0.1D-4
      B=C(1)
      DY(1)=DGAUSS(F,A,B,EPS)
         DO  1  I=2,5
      C(I)=C(I-1)+C(1)
 1    CONTINUE
      PRINT 2, C
  2   FORMAT (//14X,4HB(1),20X,4HB(2),20X,4HB(3),20X,4HB(4),
     *20X,4HB(5),/   5D24.10)
      DO 3 I=2,5
      B=C(I)
      DY(I)=DGAUSS(F,A,B,EPS)
 3    CONTINUE
      PRINT 4 , DY
  4   FORMAT (//14X,4HY(1),20X,4HY(2),20X,4HY(3),20X,4HY(4),20X,4HY(5),
     */   5D24.10)
      STOP
      END
        DOUBLE PRECISION FUNCTION  F(X)
      DOUBLE PRECISION X
      F=DSIN (X)
      RETURN
      END
*EXECUTE
*
*
