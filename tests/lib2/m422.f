      PROGRAM M422
C        TEST  FOR  M421,M422
      DIMENSION M(200),MPACK(3)
      DATA MPACK/3,16,7/
      PRINT 1000
      N=5
      ID=   5431234567B
      JA=0
      DO 2 L = 1,4
      JA = JA + 1
      CALL UPKBYT (ID,JA,M(1),N,MPACK)
      PRINT 19,JA
      PRINT 20,ID,(M(J),J=1,N)
      IX=0
      CALL PKBYT (M(1),IX,JA,N,MPACK)
      PRINT 21,IX
    2 CONTINUE
   19 FORMAT (20X,19HTEST FOR UPKBYT JA=,I3)
 20   FORMAT(6(2X,O20))
 21   FORMAT(20X,15HRUN SUB.  PKBYT 2X,O20)
 1000 FORMAT (1H1)
      END
*EXECUTE
*
*
