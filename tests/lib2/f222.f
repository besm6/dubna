      PROGRAM F222
      DIMENSION AR(10,10),AI(10,10),WR(10),ZR(10,10),ZI(10,10),BR(10,10)
     * ,BI(10,10),CR(10,10),CI(10,10),DR(10,10),DI(10,10),WORK(100)
      DATA(((AR(I,J),I=1,4),J=1,4)=
     * 3.,1.,0.,0.,1.,3.,0.,0.,0.,0.,1.,1.,0.,0.,1.,1.)
      DATA(((AI(I,J),I=1,4),J=1,4)=
     * 0.,0.,0.,-2.,0.,0.,2.,0.,0.,-2.,0.,0.,2.,0.,0.,0.)
      DO 20 I=1,4
      DO 20 J=1,4
      BR(I,J)=AR(I,J)
      CR(I,J)=AR(I,J)
      DR(I,J)=AR(I,J)
      BI(I,J)=AI(I,J)
      CI(I,J)=AI(I,J)
  20  DI(I,J)=AI(I,J)
      CALL EISCH1(10,4,AR,AI,WR,ZR,ZI,IERR,WORK)
      PRINT 3,IERR
      PRINT 2,(WR(I),I=1,4)
       DO 10 J=1,4
      PRINT 1,(ZR(I,J),I=1,4)
 10   PRINT 1,(ZI(I,J),I=1,4)
      CALL EISCH2(10,4,BR,BI,WR,IERR,WORK)
      PRINT 2,(WR(I),I=1,4)
      CALL EISCH3(10,4,CR,CI,-1.,2.,4,M,WR,ZR,ZI,IERR,WORK)
      PRINT 5,M
      PRINT 2,(WR(I),I=1,M)
      DO 30 J=1,M
      PRINT 1,(ZR(I,J),I=1,4)
  30  PRINT 1,(ZI(I,J),I=1,4)
      CALL EISCH4(10,4,DR,DI,-1.,2.,4,M,WR,IERR,WORK)
      PRINT 5,M
      PRINT 2,(WR(I),I=1,M)
  1   FORMAT(10X,4E20.11)
  2   FORMAT(5X,4E20.11/)
  3   FORMAT(//15X,5HIERR=,I3//)
  5   FORMAT(/10X,2HM=,I2)
      STOP
      END
*EXECUTE
*
*
