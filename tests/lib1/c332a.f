      PROGRAM C332 A
      DOUBLE PRECISION A,X(5),Y(5),U(101),V(101)
      DATA X/1.D0,5.D0,-1.D0,5.D1,-1.D0/,
     *     Y/0.D0,1.D0,1.D0,-5.D1,0.D0/
      N=20
  6   DO 3 I=1,5
      PRINT 9,X(I),Y(I),N
      A=0.D0
      DO 4 J=1,5
      CALL DBESCJ (X(I),Y(I),A,N,23,U,V)
      PRINT 8,A,U(1),U(N+1),V(1),V(N+1)
      A=A+0.2D0
  4   CONTINUE
  3   CONTINUE
      N=N+20
      IF(N.GE.100) GO TO 7
      GO TO 6
  7   STOP
  8   FORMAT (1X,D7.0,4D28.20)
  9   FORMAT (///50X,11HTEST C332 A//
     1        30X,2HX=,D9.1,5X,2HY=,D9.1,10X,5HNMAX=,I3//
     2 5X,1HA,15X,4HU(1),30X,6HU(N+1),25X,4HV(1),25X,6HV(N+1)/)
       END
*EXECUTE
*
*
