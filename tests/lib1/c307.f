      PROGRAM C307
      DIMENSION Y(10),Z(10)
      COMPLEX CDIGAM,Y,Z
      PRINT 10
      Z(1)=(-3.,-1.)
      DO 1 I=1,5
      Y(I)=CDIGAM(Z(I))
      PRINT 2,Z(I),Y(I)
  1   Z(I+1)=Z(I)+(2.,1.)
  2   FORMAT(20X'('F5.1','F5.1')'10X'('F15.11','F15.11')'/)
  10  FORMAT(1H1//50X'TEST C307'///25X'Z'30X'CDIGAM(Z)'//)
      END
*EXECUTE
*
*
