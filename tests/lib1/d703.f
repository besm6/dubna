      PROGRAM D703
      COMPLEX C(65)
      REAL Y(128)
      EQUIVALENCE(C,Y)
      DO 1 I=1,50
      I1=78+I
      Y(I)=0.
  1   Y(I1)=0.
      DO 2 I=51,78
  2   Y(I)=1.
      PRINT 6
      DO 7 I=1,64
      I1=64+I
  7   PRINT 4,I,Y(I),I1,Y(I1)
      PRINT 6
      CALL RFFT(C,-7)
      PRINT 3,C
      PRINT 6
      CALL RFFT(C,7)
      DO 5 I=1,64
      I1=64+I
  5   PRINT 4,I,Y(I),I1,Y(I1)
  3   FORMAT(10X,2F10.4)
  4   FORMAT(10X,2(I10,F10.4))
  6   FORMAT(1H1)
      END
*EXECUTE
*
*
