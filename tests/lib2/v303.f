      PROGRAM V303
      DIMENSION A(10),B(10)
      DATA ((A(I),I=1,10)=1.,2.,3.,4.,5.,6.,7.,8.,9.,10.)
      DATA ((B(J),J=1,10)=21.,22.,23.,24.,25.,26.,27.,28.,29.,30.)
      PRINT 1, A,B
  1   FORMAT (10X,2HA=,10(F5.1,3X)/10X,2HB=,10(F5.1,3X))
      CALL USWOP (A,B,10)
      PRINT 1, A,B,
      END
*EXECUTE
*
*
