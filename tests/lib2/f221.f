      PROGRAM F221
      DIMENSION AR(10,10),AI(10,10),WR(10),WI(10),ZR(10,10),ZI(10,10),
     *   WORK(100),BR(10,10),BI(10,10)
      DATA (((AR(I,J),I=1,4),J=1,4)=
     *   5.,3.,2.,1.,5.,6.,3.,2.,-6.,-5.,-1.,-3.,-7.,-6.,-5.,0.)
      DATA (((AI(I,J),I=1,4),J=1,4)=
     *    9.,3.,2.,1.,5.,10.,3.,2.,-6.,-5.,3.,-3.,-7.,-6.,-5.,4.)
      DO 20 I=1,4
      DO 20 J=1,4
      BR(I,J)=AR(I,J)
      BI(I,J)=AI(I,J)
  20  CONTINUE
      CALL EISCG1(10, 4,AR,AI,WR,WI,ZR,ZI,IERR,WORK)
      PRINT 3,IERR
      PRINT 2,(WR(I),WI(I),I=1,4)
      DO 10 J=1,4
      PRINT 1,(ZR(I,J),I=1,4)
 10   PRINT 1,(ZI(I,J),I=1,4)
      CALL EISCG2(10,4,BR,BI,WR,WI,IERR)
      PRINT 2,(WR(I),WI(I),I=1,4)
  1   FORMAT(5X,4E20.11/)
  2   FORMAT(5X,2E20.11/)
  3   FORMAT(10X,5HIERR=,I3)
      STOP
      END
*EXECUTE
*
*
