      PROGRAM C304
      DIMENSION X(10),G(10)
      PRINT 10
      X=-10.
      DO 1 I=1,10
      G(I)=DILOG(X(I))
      PRINT 2,X(I),G(I)
  1   X(I+1)=X(I)+5.
  2   FORMAT(40X,F5.1,F20.11/)
  10  FORMAT(1H1//50X'TEST C304'///42X'X'15X'DILOG(X)'//)
      END
*EXECUTE
*
*
