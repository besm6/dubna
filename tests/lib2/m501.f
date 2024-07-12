      PROGRAM M501
      DIMENSION IA1(5)
      DATA IA1 /1,2,3,4,5/
C--                TEST FOR IUSAME
      PRINT 30
      J1=1
      J1=1
      J2=5
      MIN=5
      JSAME=1
      KK=1
    3 NSAME=IUSAME (IA1,J1,J2,MIN,JSAME)
      PRINT 40,(IA1(K),K=1,5),NSAME,JSAME
      IF (KK) 8,7,6
    6 IA1(1)=IA1(2)
      IA1(3)=IA1(2)
      KK=KK-1
      GO TO 3
    7 IA1(4)=IA1(2)
      IA1(5)=IA1(2)
      KK=KK-1
      GO TO 3
    8 CONTINUE
  30  FORMAT(1H1//30X'TEST FOR IUSAME'//)
   40 FORMAT (20X,10HVECTOR A=,5I5/20X,6HNSAME=,I2,3X,6HJSAME=,I2)
      END
*EXECUTE
*
*
