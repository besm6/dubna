      PROGRAM C319
      DIMENSION X(6),AM(6)
      DATA X/-0.8,-0.4,0.,0.5,0.8,1./,AM/0.,0.4,0.2,0.6,0.8,1./
      DO 1 I=1,6
      Y=ELIN1(X(I),AM(I))
  1   PRINT 2,X(I),AM(I),Y
  2   FORMAT(5X,2HX=,F5.1,5X,3HAM=,F5.1,5X,2HY=,E20.10)
      STOP
      END
*EXECUTE
*
*
