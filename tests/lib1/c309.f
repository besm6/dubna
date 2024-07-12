      PROGRAM C309
      DIMENSION X(11),U(11),V(11),C(11),S(11)
      PRINT 10
      X(1)=-1.
      DO 5 K=1,11
       X(K+1)=X(K)+0.2
      CALL FRECS(X(K),U(K),V(K))
      PRINT 1,X(K),U(K),V(K)
      X(K)=X(K)*X(K)*1.57079633
      CALL FRICS(X(K),C(K),S(K))
  5   PRINT 2,X(K),C(K),S(K)
  1   FORMAT(10X,F10.7,2F20.12)
  2   FORMAT(10X,F10.7,40X,2F20.12)
  10  FORMAT(1H1//50X'TEST C309'///17X'X'12X'C'20X'S'20X'C2'20X'S2'//)
      END
*EXECUTE
*
*
