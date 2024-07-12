      PROGRAM C360
      DIMENSION N1(3),N2(3),A(3),B(3)
      DATA((N1(I),I=1,3)=-2,-4,-2),((N2(I),I=1,3)=-2,-2,-4)
      DATA ((A(I),I=1,3)=-1.5,-2.5,-2.5),((B(I),I=1,3)=0.33,0.20,0.20)
      PRINT 1
    1 FORMAT(//16X,25HFUNCTIILE HIPERGEOMETRICE/14X,29(1H*)//16X,2H N,5X
     * ,2HNP,5X,5H  C  ,5X,3H Z ,5H FHYP,5X,5H FLIC)
      DO 3 I=1,3
      NN1=-N1(I)
      NN2=-N2(I)
      AA=A(I)
      F=H2F1(NN1,NN2,AA,0.5)
      C=0.5
      FF=B(I)
      PRINT 2,N1(I),N2(I),A(I),C,F,FF
    2 FORMAT(//16X,2(I2,5X),4(F5.2,5X))
    3 CONTINUE
      END
*EXECUTE
*
*
