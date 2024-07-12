      PROGRAM C315
CALCULATION OF THE ASSOCIATED LEGENDRE FUNCTIONS
      DIMENSION Y(11),L(3),M(3)
      DATA (Y=-1.,-0.8,-0.6,-0.4,-0.2,0.,0.2,0.4,0.6,0.8,1.),
     1(L=0,2,5),(M=-3,-1,1)
      PRINT 10
      DO 1 I=1,3
      DO 1 K=1,3
      DO 1 J=1,11
      DO 1 IJK=1,2
      I1=IJK-2
      IF(I1) 3,2,2
  2      NORM=1
         GO TO 4
  3   NORM=0
  4   Z=ALEGF(L(I),M(K),Y(J),NORM)
      PRINT 5,L(I),M(K),Y(J),NORM,Z
  1   CONTINUE
  5   FORMAT(2(12X,I3),5X,F10.1,12X,I3,10X,F20.11)
  10  FORMAT(1H1//50X'TEST C315'///12X'L'14X'M'15X'Y'10X'NORM'15X'ALEGF(
     1L,M,Y,NORM)'//)
      END
*EXECUTE
*
*                                                                    A
