      PROGRAM J550
      DIMENSION N(50,40)
      DO 1 I=1,50
      DO 1 J=1,40
      N(I,J)=I+J-2
  1   CONTINUE
      CALL AREAPT(50,40,N,8)
      END
*EXECUTE
*
*
