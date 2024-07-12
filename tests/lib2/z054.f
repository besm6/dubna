      PROGRAM Z054
      DIMENSION M(100),MM(20)
      PRINT 11
      DO 1 I=1,100
  1   M(I)=I
      CALL SUPLIS
      DO 2 I=1,2
      DO 3 J=1,100
      DO 4 K=1,20
  4   MM(K)=M(J)
  3   PRINT 10,MM
  10  FORMAT(20I6)
      CALL RESLIS
  2   PRINT 11
  11  FORMAT(1H1)
      STOP
      END
*EXECUTE
*
*
