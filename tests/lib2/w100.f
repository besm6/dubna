      PROGRAM W100
C        TEST  FOR  ZERO
      DIMENSION A(3),B(2),C(3)
      C(1)=1.0
      C(2)=0.5
      C(3)=1.5
      CALL ZERO (A(1),B(2))
      PRINT 2,A,B,C
  2   FORMAT (4F7.2)
      END
*
