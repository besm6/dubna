      PROGRAM V302
      INTEGER A,B
      DIMENSION A(20),B(40)
      DATA(A=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
      PRINT 10
   10 FORMAT (X,'DATA')
      PRINT 20,A
   20 FORMAT (10X,2HA=,20I3)
      CALL UCOCOP(A,B,5,4,4,6)
      PRINT 30
   30 FORMAT (X,'UCOCOP')
      PRINT 40,B
   40 FORMAT (10X,2HB=,40I3)
      DO 1 I=1,40
    1 B(I)=0
      CALL UCOCOP (A,B,2,5,10,5)
      PRINT 40,B
      DO 2 I=1,40
    2 B(I)=0
      CALL UCOCOP(A,B,10,1,2,1)
      PRINT 40,B
      DO 3 I=1,40
    3 B(I)=0
      CALL UCOCOP(A,B,4,3,5,3)
      PRINT 40,B
      DO 4 I=1,40
    4 B(I)=0
      PRINT 50
   50 FORMAT (X,'UDICOP')
      CALL UDICOP(A,B,4,4,5,7)
      PRINT 40,B
      DO 5 I=1,40
    5 B(I)=0
      CALL UDICOP(A,B,5,4,4,7)
      PRINT 40,B
      DO 6 I=1,40
    6 B(I)=0
      CALL UDICOP(A,B,10,2,2,4)
      PRINT 40,B
      DO 7 I=1,40
    7 B(I)=0
      CALL UDICOP(A,B,2,1,10,3)
      PRINT 40,B
      DO 8 I=1,40
    8 B(I)=0
      CALL UDICOP(A,B,5,2,4,2)
      PRINT 40,B
      END
*EXECUTE
*
*
