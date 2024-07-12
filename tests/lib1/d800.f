      PROGRAM D800
      COMPLEX A
      DIMENSION A(128)
      PRINT 6
  6   FORMAT ( 1H1,20X,11HFFT  OUTPUT,///)
      DO 1 I=1,50
      I1=78+I
      A(I)=(0.,0.)
  1   A(I1)=(0.,0.)
      DO 2 I=51,78
  2   A(I)=(1.,0.)
C
C----------FOURIER TRANSFORM----------
C
      CALL FFT (A,7,1.,1.,128)
      DO 3 I=1,64
      I1=I+64
      R1=REAL(A(I))
      R2=REAL(A(I1))
      AIM1=AIMAG(A(I))
      AIM2=AIMAG(A(I1))
      PRINT 4,I,R1,AIM1,I1,R2,AIM2
  4   FORMAT (10X,I10,2F10.4,I10,2F10.4)
  3   CONTINUE
C
C----------INVERSE  FOURIER  TRANSFORM----------
C
      PRINT 7
  7   FORMAT(1H1,20X,20HINVERSE  FFT  OUTPUT,///)
      CALL FFT (A,7,-1.,128.,128)
      DO 5 I=1,64
      I1=I+64
      R1=REAL(A(I))
      R2=REAL(A(I1))
      AIM1=AIMAG(A(I))
      AIM2=AIMAG(A(I1))
      PRINT 4,I,R1,AIM1,I1,R2,AIM2
  5   CONTINUE
      END
*EXECUTE
*
*
