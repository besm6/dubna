      PROGRAM V116
      IY=0
      DO 10 I=1,10
         IF(URAND(IY).LT.0.5) GO TO 5
         PRINT 1
         GO TO 10
  5      PRINT 2
  10  CONTINUE
   1  FORMAT(6H HEADS)
   2  FORMAT(6H TAILS)
      STOP
      END
*EXECUTE
*
*
