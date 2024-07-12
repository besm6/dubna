      PROGRAM E250
      DIMENSION X(4),Y(4),W(4),C(3)
      DATA X/1.,2.,3.,4./,Y/2.,3.,4.,0./,W/4*1./
      DO 2 MM=1,500
      CALL LFIT(X,Y,4,0,A,B,E)
  2   CONTINUE
      PRINT 1,A,B,E
      DO 3 MM=1,500
      CALL LFIT(X,Y,4,1,A,B,E)
  3   CONTINUE
      PRINT 1,A,B,E
      CALL LFITW(X,Y,W,4,0,A,B,E)
      PRINT 1,A,B,E
      CALL LFITW(X,Y,W,4,1,A,B,E)
      PRINT 1,A,B,E
  1   FORMAT(//4F10.2)
      STOP
      END
*EXECUTE
*
*
