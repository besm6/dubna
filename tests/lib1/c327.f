      PROGRAM C327
      PRINT 102
  102 FORMAT(///'    L  ETA  RHO',8X,'F',13X,'FP',12X,'G',13X,'GP')
      LM=5
      LE=5
      LR=5
      DR=2.5
      DE=2.5
      R=0.5
      DO 1 IR=1,LR
      E=0.
      DO 2 IE=1,LE
      DO 3 L=1,LM
      LL=L-1
      CALL FGETA(LL,E,R,G,GP,F,FP,S)
      PRINT 105,LL,E,R,F,FP,G,GP
    3 CONTINUE
    2 E=E+DE
    1 R=R+DR
 105  FORMAT(I5,F5.1,F6.2,5E14.6)
      END
*EXECUTE
*
*
