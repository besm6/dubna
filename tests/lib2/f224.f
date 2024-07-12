      PROGRAM F224
      DIMENSION AR(10,10),WR(10),ZR(10,10),BR(10,10),CR(10,10),DR(10,10)
     *   ,WORK(100)
      DATA(((AR(I,J),I=1,4),J=1,4)=
     *  5.,4.,1.,1.,4.,5.,1.,1.,1.,1.,4.,2.,1.,1.,2.,4.)
      DO 20 I=1,4
      DO 20 J=1,4
      BR(I,J)=AR(I,J)
      CR(I,J)=AR(I,J)
  20  DR(I,J)=AR(I,J)
      CALL EISRS1(10,4,AR,WR,ZR,IERR,WORK)
      PRINT 3,IERR
      PRINT 2,(WR(I),I=1,4)
      DO 10 J=1,4
  10  PRINT 1,(ZR(I,J),I=1,4)
      CALL EISRS2(10,4,BR,WR,IERR,WORK)
      PRINT 2,(WR(I),I=1,4)
      CALL EISRS3(10,4,CR,-1.,2.,4,M,WR,ZR,IERR,WORK)
      PRINT 3,IERR
      PRINT 5,M
      PRINT 2,(WR(I),I=1,M)
      DO 15 J=1,M
  15  PRINT 1,(ZR(I,J),I=1,4)
      CALL EISRS4(10,4,DR,-1.,2.,4,M,WR,IERR,WORK)
      PRINT 2,(WR(I),I=1,M)
  1   FORMAT(10X,4E20.11)
  2   FORMAT(5X,4E20.11/)
  3   FORMAT(//15X,5HIERR=,I2//)
  5   FORMAT(/10X,2HM=,I2/)
      STOP
      END
*EXECUTE
*
*
