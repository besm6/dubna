      PROGRAM C300
      DIMENSION X(10),Y(10),Z(10),W(10)
      DATA X/0.5,1.,1.5,2.,2.5,3.,3.5,4.,4.5,5./
      DO 10 I=1,10
       Y(I)=ERF(X(I))
      Z(I)=ERFC(X(I))
      W(I)=FREQ(X(I))
  10  CONTINUE
      PRINT 11,X,Y,Z,W
  11  FORMAT(1H1//50X'TEST C300'//3X'X='10F6.1//3X'ERF='5F19.15/8X,5F19.
     115//3X'FREC='5F19.15/9X,5F19.15//3X'FREQ='5F19.15/9X,5F19.15)
      END
*EXECUTE
*
*
