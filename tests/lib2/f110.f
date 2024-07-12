      PROGRAM F110
C        TEST  FOR  F110,F111,F112,F113,F114,F115
       DIMENSION AR(4),AI(4),BR(4),BI(4),CR(4),CI(4)
        AR(1)=3.
       AR(2)=6.
       AR(3)=2.
       AR(4)=4.
       AI(1)=2.
       AI(2)=5.
       AI(3)=1.
       AI(4)=2.
       BR(1)=1.
       BR(2)=3.
        BR(3)=4.
       BR(4)=2.
        BI(1)=1.
       BI(2)=2.
      BI(3)=4.
       BI(4)=3.
      PRINT 1004
 1004 FORMAT(////,15X,26H=TEST LI SANG HO= 5,5,1972,////)
       PRINT 5
  5   FORMAT(5X,5HMXADD,//)
      CALL MXADD(AR,BR,CR,2,2)
      PRINT 4,CR(1),CR(3)
      PRINT 4,CR(2),CR(4)
       PRINT 6
  6   FORMAT(5X,5HMXSUB,//)
      CALL MXSUB(AR,BR,CR,2,2)
      PRINT 4,CR(1),CR(3)
      PRINT 4,CR(2),CR(4)
      B=2.0
       PRINT 7
  7   FORMAT(5X,5HMXMTR,//)
      CALL MXMTR(AR,B ,CR,2,2)
      PRINT 4,CR(1),CR(3)
      PRINT 4,CR(2),CR(4)
      PRINT 8
  8   FORMAT(5X,5HMXNTR,//)
      CALL MXNTR(AR,B ,CR,2,2)
      PRINT 4,CR(1),CR(3)
      PRINT 4,CR(2),CR(4)
      PRINT 14
  14  FORMAT(5X,5HMXTRP,//)
       CALL MXTRP(AR,CR,2,2)
      PRINT 4,CR(1),CR(3)
      PRINT 4,CR(2),CR(4)
       PRINT 9
  9   FORMAT(5X,5HMXUTY,//)
      CALL MXUTY(CI,2)
      PRINT 4,CI(1),CI(3)
      PRINT 4,CI(2),CI(4)
      PRINT 16
      PRINT 15,AR(1),AR(3),BR(1),BR(3),CI(1),CI(3)
      PRINT 15,AR(2),AR(4),BR(2),BR(4),CI(2),CI(4)
       PRINT 10
  10   FORMAT(5X,6HMXDMAL,//)
      CALL MXDMAL(AR,BR,CI,2,2)
      PRINT 4,CI(1),CI(3)
      PRINT 4,CI(2),CI(4)
      PRINT 16
      PRINT 15,AR(1),AR(3),BR(1),BR(3),CI(1),CI(3)
      PRINT 15,AR(2),AR(4),BR(2),BR(4),CI(2),CI(4)
       PRINT 11
  11   FORMAT(5X,6HMXDMAR,//)
      CALL MXDMAR(AR,BR,CI,2,2)
      PRINT 4,CI(1),CI(3)
      PRINT 4,CI(2),CI(4)
      PRINT 16
       PRINT 12
  12    FORMAT(5X,5HMXDML,//)
      CALL MXDML (AR,BR,CI,2,2)
      PRINT 15,AR(1),AR(3),BR(1),BR(3),CI(1),CI(3)
      PRINT 15,AR(2),AR(4),BR(2),BR(4),CI(2),CI(4)
      PRINT 16
       PRINT 13
  13     FORMAT(5X,5HMXDMR,//)
      CALL MXDMR (AR,BR,CI,2,2)
      PRINT 15,AR(1),AR(3),BR(1),BR(3),CI(1),CI(3)
      PRINT 15,AR(2),AR(4),BR(2),BR(4),CI(2),CI(4)
 16   FORMAT(15H***************,/,12X,5HAR(I),22X,5HBR(I),22X,5HCI(I),
     */)
  15  FORMAT(5X,2F10.3,5X,2F10.3,5X,2F10.0,//)
   4   FORMAT(5X,2F10.3,//)
      END
*EXECUTE
*
*
