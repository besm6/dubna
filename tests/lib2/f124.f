      PROGRAM F124
      COMPLEX A,B
      DIMENSION A(2,2),AR(2,2),AI(2,2),B(2,2)
       A(1,1)=(-2.9,1.6E-4)
       A(1,2)=(3.E4,120.)
       A(2,1)=(-1.4,-3.E-3)
       A(2,2)=(1.5,-5.E-2)
       CALL CXSPLT(A,AR,AI,2,2,2)
      PRINT 4
      PRINT 301,AR(1,1)
      PRINT 301,AR(2,1)
      PRINT 301,AR(1,2)
      PRINT 301,AR(2,2)
      PRINT 301,AI(1,1)
      PRINT 301,AI(2,1)
      PRINT 301,AI(1,2)
      PRINT 301,AI(2,2)
 301  FORMAT(5X,5HTEST=,F15.5)
    4 FORMAT(////,20X,26H=TEST LI SANG HO= 4,4,1972,///)
       CALL CXJOIN(AR,AI,B,2,2,2)
       DO 6 I=1,2
       PRINT 7,(A(I,J),J=1,2)
   7   FORMAT(5X,4F15.5,//)
   6   CONTINUE
       END
*EXECUTE
*
*
