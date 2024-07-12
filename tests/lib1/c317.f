      PROGRAM C317
      DIMENSION X(15),Y(15)
      PRINT 10
      X(1)=-3.5
      DO 1 I=1,15
      Y(I) = ADIGAM (X(I))
      PRINT   5,X(I), Y(I)
  1   X(I+1)=X(I)+0.5
  5   FORMAT(30X,2F20.11)
  10  FORMAT(1H1//50X,9HTEST C317///40X,1HX,20X,1HY//)
      END
*EXECUTE
*
*
