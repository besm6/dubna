      PROGRAM D104
      EXTERNAL XF
      COMMON C
      PRINT 3
    3 FORMAT(1H1/1X  84HCAUCHY PRINCIPLE VALUE INTEGRATION OF  F(X) = X/
     1(X-C)  BETWEEN  A = -10  AND  B = 10//1X  35HTHE SINGULARITY IS AT
     2  C = -20(1)20//
     3        60H              C              RESULT               EXACT
     4     ,  15H          ERROR/)
      I=-21
    2 I=I+1
      C=I
      RA=CAUCHY(XF,-10.,10.,C,1.0E-6)
      RE=0.
      IF(IABS(I) .NE. 10) RE=20.0+C*ALOG(ABS((10.0-C)/(10.0+C)))
      D=RA-RE
      PRINT 1, C,RA,RE,D
    1 FORMAT(5XF10.0,5F20.10)
      IF(I .LT. 20) GO TO 2
      STOP
      END
      FUNCTION XF(X)
      COMMON C
      XF=X/(X-C)
      RETURN
      END
*EXECUTE
*
*
