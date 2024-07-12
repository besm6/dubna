      PROGRAM C100
      DIMENSION A(10),B(40),F(40)
      DATA A/-1.0,-2.0,-3.0,-4.0,-5.0,1.0,2.0,3.0,4.0,5.0/
      PRINT 1
      M=10
      X=-10.5
      DO 2 I=1,40
      X=X+0.5
      B(I)=X
      F(I)=POLY(X,M,A)
  2   CONTINUE
      PRINT 10,(A(K),K=1,10)
      PRINT 11,(B(K),F(K),K=1,40)
  1   FORMAT(1H1,49X'POLYNOMIAL FUNCTION'40X,'TEST C100'/50X,19(1H*),40X
     1,9(1H*)//)
  10  FORMAT(3X,'POLYNOM A=',10F8.2/)
  11  FORMAT(2(10X,2HA(,F8.3,3H) =,E16.4))
      END
*EXECUTE
*
*
