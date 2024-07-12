      PROGRAM D113
      EXTERNAL F
      COMPLEX F,CGAUSS,A(4),B(4),Z
      DATA(A=0.,0.,0.,1.,0.,-1.,-1.,2.),(B=1.,1.,1.,2.,1.,2.,2.,3.)
      PRINT 10
      EPS=1.E-7
  3   DO 1 I=1,4
      Z=CGAUSS(F,A(I),B(I),EPS)
  1   PRINT 5,A(I),B(I),Z
  5   FORMAT(10X,1H(,F4.1,1H,,F4.1,1H),10X,1H(,F4.1,1H,,F4.1,1H),10X,1H(
     1,F17.11,1H,,F17.11,1H))
  10  FORMAT(//50X,9HTEST D113///10X,10HF=Z**2+2*Z,10X,9HEPS=1.E-7//15X,
     11HA,20X,1HB,30X,1HZ)
      END
      COMPLEX FUNCTION F(Z)
      COMPLEX Z
      F=Z**2+2*Z
      RETURN
      END
*EXECUTE
*
*
