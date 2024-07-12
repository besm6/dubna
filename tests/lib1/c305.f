      PROGRAM C305
      DIMENSION X(500),Y(500),Z(500)
      X(1)=-1.
       DO 2 I=1,38
      Y(I)=GAMMA(X(I))
      X(I+1)=X(I)-0.5
      PRINT 200,X(I),Y(I)
 200  FORMAT(10X,F13.3,10X,E20.11)
  2    CONTINUE
      X(1)=1.
      DO 1 I=1,40
      Y(I)=GAMMA(X(I))
      X(I+1)=X(I)+0.5
      PRINT 100,X(I),Y(I)
  1   CONTINUE
 100  FORMAT(10X,F13.3,10X,E20.11)
      END
*EXECUTE
*
*
