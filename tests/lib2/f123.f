      PROGRAM F123
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
       CALL RRMPY(AR,BR,CR,2,2,2)
      PRINT 1001
 1001 FORMAT(5X,26HCALL RRMPY(AR,BR,CR,2,2,2))
       PRINT 6,(CR(IS),IS=1,4)
   6   FORMAT(5X,2F15.5,//)
  7   CALL CCMPY(AR,AI,BR,BI,CR,CI,2,2,2)
      PRINT 1002
 1002 FORMAT(3X,35HCALL CCMPY(AR,AI,BR,BI,CR,CI,2,2,2))
      PRINT 4,CR(1),CI(1),CR(3),CI(3)
      PRINT 4,CR(2),CI(2),CR(4),CI(4)
   8   CALL CCMPY1(AR,AI,BR,BI,CR,CI,2,2,2)
      PRINT 1003
 1003 FORMAT(3X,36HCALL CCMPY1(AR,AI,BR,BI,CR,CI,2,2,2))
      PRINT 4,CR(1),CI(1),CR(3),CI(3)
      PRINT 4,CR(2),CI(2),CR(4),CI(4)
   9   CALL CCMPY2(AR,AI,BR,BI,CR,CI,2,2,2)
      PRINT 1006
 1006 FORMAT(3X,36HCALL CCMPY2(AR,AI,BR,BI,CR,CI,2,2,2))
      PRINT 4,CR(1),CI(1),CR(3),CI(3)
      PRINT 4,CR(2),CI(2),CR(4),CI(4)
  10   CALL CCMPY3(AR,AI,BR,BI,CR,CI,2,2,2)
      PRINT 1005
 1005 FORMAT(3X,36HCALL CCMPY3(ARTAI,BR,BI,CR,CI,2,2,2))
      PRINT 4,CR(1),CI(1),CR(3),CI(3)
      PRINT 4,CR(2),CI(2),CR(4),CI(4)
   4   FORMAT(5X,2F10.3,3X,2F10.3,//)
       END
*EXECUTE
*
*
