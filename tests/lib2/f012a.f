      PROGRAM F012 A
      DIMENSION A1(4,4),A2(4,4),A3(4,4),B1(4,2),B2(4,2),B3(4,2)
      DATA A1/1.,.42,.54,.66,.42,1.,.32,.44,.54,.32,1.,.22,.66,.44,.22,
     *        1./,B1/4.,3.,2.,1.,3.,3.,2.,1./
      DO 10 I=1,4
      DO 10 J=1,4
      A2(I,J)=A1(I,J)
 10   A3(I,J)=A1(I,J)
      DO 20 I=1,4
      DO 20 J=1,2
      B2(I,J)=B1(I,J)
 20   B3(I,J)=B1(I,J)
      CALL RSINV(4,A1,4,IFAIL)
      PRINT 1,IFAIL,((A1(I,J),J=1,4),I=1,4)
      CALL RSEQN(4,A2,4,IFAIL,2,B1)
      PRINT 2,IFAIL,((B1(I,J),J=1,2),I=1,4)
      CALL RSFACT(4,A3,4,IFAIL,DET,JFAIL)
      PRINT 3,IFAIL,JFAIL,DET
      CALL RSFEQN(4,A3,4,1,B2)
      PRINT 4,(B2(I,1),I=1,4)
      CALL RSFEQN(4,A3,4,2,B3)
      PRINT 5,((B3(I,J),J=1,2),I=1,4)
      CALL RSFINV(4,A3,4)
      PRINT 6,((A3(I,J),J=1,4),I=1,4)
 1    FORMAT(1H1,1X,'RSINV',5X,'IFAIL=',I2,15X,'A'/(5X,4E20.11/))
 2    FORMAT(/1X,'RSEQN',5X,'IFAIL=',I2,10X,'A'/(5X,2E20.11/))
 3    FORMAT(/1X,'RSFACT'/5X,'IFAIL=',I2,5X,'JFAIL=',I2,5X,'DET=',
     *       E17.11)
 4    FORMAT(/1X,'RSFEQN'/1X,'B=',4E20.11)
 5    FORMAT(/1X,'RSFEQN',10X,'B '/(5X,2E20.11/))
 6    FORMAT(/1X,'RSFINV',15X,'A'/(5X,4E20.11/))
      CALL EXIT
      END
*CALL PTIME
*EXECUTE
