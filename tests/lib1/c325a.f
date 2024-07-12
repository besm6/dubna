*  ПPOГPAMMA  PAБOTAET C KOMПЛEKCHOЙ APИФMETИKOЙ  Г.MAЗHOГO
      PROGRAM C325A
      COMPLEX AK,AM,Z,W,WHIT,C1,C2,C3
      Z=CMPLX(0.,0.)
      C1=CMPLX(0.,10.)
      AM=CMPLX(5.,0.)
      C2=CMPLX(5.,0.)
      AK=CMPLX(0.,1.)
      C3=CMPLX(0.,5.)
      DO 6 K=1,5
      PRINT 1
      DO 5 J=1,3
      PRINT 10,AM,AK
      PRINT 20
      DO 4 I=1,11
      W=WHIT(AK,AM,Z)
      PRINT 30,Z,W
      Z=Z+C1
  4   CONTINUE
      Z=CMPLX(0.,0.)
      AM=AM+C2
  5   CONTINUE
      Z=CMPLX(0.,0.)
      AM=CMPLX(5.,0.)
      AK=AK+C3
  6   CONTINUE
  1   FORMAT(1H1)
  10  FORMAT(//20X,3HAM=,2D10.2,10X,3HAK=,2D10.2//)
  20  FORMAT(17X,1HZ,35X,1HW/)
  30  FORMAT(10X,2D10.2,10X,2D20.10)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
