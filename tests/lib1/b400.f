      PROGRAM B400
      DIMENSION A(8)
      PRINT 1
      CALL POWEZE(0.5,A,8)
      PRINT 2,A
  1   FORMAT(//40X,'POWER SERIES GENERATOR',40X,'TEST B400'/40X,22(1H*),
     140X,9(1H*)//)
  2   FORMAT(40X,'X=0.5',10X,'N=8'//20X,'A=',8F12.7)
      END
*EXECUTE
*
*
