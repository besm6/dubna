      PROGRAM M423
      DIMENSION MPACK(3),IPBYT(7),LOST(7)
      DATA MPACK/3,16,7/
      PRINT 1000
      ID=   5431234567B
      JTH=0
      DO 4 K = 1,7
      JTH=JTH+1
      INC=0
      DO 3 I=1,7,2
      INC = INC + 1
      ID1 =ID
      LOST(I)=INCBYT(INC,ID1,JTH,MPACK)
      IPBYT(I) = ID1
    3 CONTINUE
      PRINT 29,JTH
      PRINT 30,(IPBYT(I),I=1,7)
      PRINT 30,(LOST(I),I=1,7)
    4 CONTINUE
   29 FORMAT (20X,20HTEST FOR INCBYT JTH=,I3)
  30  FORMAT(2X,4O20/)
 1000 FORMAT (1H1)
      END
*EXECUTE
*
*