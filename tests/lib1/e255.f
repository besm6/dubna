      PROGRAM E255
      DIMENSION X(4),Y(4),W(4),C(3)
       DATA X/1.,2.,3.,4./,Y/2.,3.,4.,0./,W/4*1./
      CALL PARLSQ(X,Y,4,C,S)
      PRINT 1,C,S
    1 FORMAT(//4F10.2)
      END
*EXECUTE
*
*
