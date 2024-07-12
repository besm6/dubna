      PROGRAM F223
      DIMENSION AR(10,10),WR(10),WI(10),ZR(10,10),BR(10,10),WORK(100)
      DIMENSION B(10,10),C(10,10),D(10,10)
      DATA(((AR(I,J),I=1,3),J=1,3)=
     * 8.,-4.,18.,-1.,4.,-5.,-5.,-2.,-7.)
      DATA(((B(I,J),I=1,3),J=1,3)=
     * 4.,2.,0.,1.,4.,1.,1.,1.,4.)
      DATA(((C(I,J),I=1,3),J=1,3)=
     * 33.,-24.,-8.,16.,-10.,-4.,72.,-57.,-17.)
      K=3
      DO 20 I=1,3
      DO 20 J=1,3
 20   D(I,J)=AR(I,J)
      PRINT 5
      CALL EISRG1(10,3,AR,WR,WI,ZR,IERR,WORK)
  44  PRINT 3,IERR
      PRINT 2,(WR(I),WI(I),I=1,3)
      PRINT 4
      DO 10 J=1,3
  10  PRINT 1,(ZR(I,J),I=1,3)
      IF(K.LE.2) GO TO 55
      CALL EISRG1(10,3,B,WR,WI,ZR,IERR,WORK)
      K=K-1
      GO TO 44
  55  CONTINUE
      IF(K.EQ.1) GO TO 66
      CALL EISRG1(10,3,C,WR,WI,ZR,IERR,WORK)
      K=K-1
      GO TO 44
  66  CONTINUE
      CALL EISRG2(10,3,D,WR,WI,IERR)
      PRINT 3,IERR
      PRINT 2,(WR(I),WI(I),I=1,3)
  1   FORMAT(5X,4E20.11/)
  2   FORMAT(5X,2E20.11//)
   3   FORMAT(///10X,5HIERR=,I3/15X,2HWR,20X,2HWI/)
  4   FORMAT (/40X,2HZR/)
  5   FORMAT(50X,9HTEST F223)
      STOP
      END
*EXECUTE
*
*
