      PROGRAM C308
      DIMENSION X(10),G(10),C(10)
      PRINT 10
      X(1)=-1.
      DO 1 I=1,10
      G(I)=ELLICK(X(I))
      C(I)=ELLICE(X(I))
      PRINT 2,X(I),G(I),C(I)
  1   X(I+1)=X(I)+0.25
  2   FORMAT(30X,F5.2,10X,2F20.11/)
  10  FORMAT(1H1//50X'TEST C308'///32X'X'25X'ELLICK(X)'10X'ELLICE(X)'//)
      END
*EXECUTE
*
*
