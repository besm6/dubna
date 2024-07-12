      PROGRAM F225
      DIMENSION AR(10,10),WR(10),ZR(10,10),BR(10,10),CR(10,10),DR(10,10)
     *  ,WORK(100)
      AR(2,1)=4.
      AR(3,1)=6.
      AR(4,1)=6.
      AR(5,1)=4.
      AR(1,2)=-4.
      AR(2,2)=-10.
      AR(3,2)=-12.
      AR(4,2)=-10.
      AR(5,2)=-4.
      DO 20 J=1,2
      DO 20 I=1,5
      BR(I,J)=AR(I,J)
      CR(I,J)=AR(I,J)
  20  DR(I,J)=AR(I,J)
      PRINT 8
      PRINT 7,(AR(I,1),I=2,5)
      PRINT 7,(AR(I,2),I=1,5)
      CALL EISST1(10,5,AR,WR,ZR,IERR,WORK)
      PRINT 3,IERR
      PRINT 2,(WR(I),I=1,5)
      DO 10 J=1,5
  10  PRINT 1,(ZR(I,J),I=1,5)
      CALL EISST2(10,5,BR,WR,IERR,WORK)
      PRINT 2,(WR(I),I=1,5)
      CALL EISST3(10,5,CR,-1.,2.,5,M,WR,ZR,IERR,WORK)
      PRINT 3,IERR
      PRINT 5,M
      PRINT 2,(WR(I),I=1,M)
      DO 15 J=1,M
  15  PRINT 1,(ZR(I,J),I=1,5)
      CALL EISST4(10,5,DR,-1.,2.,5,M,WR,IERR,WORK)
      PRINT 5,M
      PRINT 2,(WR(I),I=1,M)
  1   FORMAT(4X,5E20.11)
  2   FORMAT(5X,5E20.11/)
  3   FORMAT(//15X,5HIERR=,I2//)
  5   FORMAT(/10X,2HM=,I2/)
  7   FORMAT(30X,5F10.3)
  8   FORMAT(30X,6HMATRIX//)
      STOP
      END
*EXECUTE
*
*
