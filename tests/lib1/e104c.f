      PROGRAM E104 C
C        ПPИMEP B OПИCAHИИ 1985 Г.
      DIMENSION X(2),NA(2),A(25),F(150)
      DATA NA/10,15/
C    STORE  ARGUMENT  ARRAY  .
      NA1=NA(1)
      NA2=NA(2)
      L=0
      DO 1 I1=1,NA1
      L=L+1
      A(L)=SQRT(FLOAT(I1))
  1   CONTINUE
      DO 2 I2=1,NA2
      L=L+1
      A(L)=ALOG(FLOAT(I2))
  2   CONTINUE
C    STORE  FUNCTION ARRAY  .
      K1=0
      K2=K1+NA1
      L=0
      DO 4 I2=1,NA2
      DO 3 I1=1,NA1
      L=L+1
      F(L)=G(A(I1+K1),A(I2+K2))
  3   CONTINUE
  4   CONTINUE
C    INTERPOLATE  IN  TABLE  .
      X(1)=1.7
      X(2)=2.9
      GINT=FINT(2,X,NA,A,F)
      PRINT 10,GINT
 10   FORMAT(///10X,'GINT=',E20.11)
      STOP
      END
      FUNCTION G(A1,A2)
      G=SIN(A1)+SIN(A2)
      RETURN
      END
*EXECUTE
*
*
