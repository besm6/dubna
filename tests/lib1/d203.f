      PROGRAM D203
      DIMENSION Y(20),F(20),H(6)
      DATA H/0.1,0.2,0.3,0.5,1.,2./
      EXTERNAL EXTERN
      DO 5 I=1,6
      X=0.0
      PRINT 10
      CALL INTSTP(3,H(I),X,Y,EXTERN)
      PRINT 2,X,Y(1),Y(2),Y(3)
      Y(1)=X**3*SIN(X)
      Y(2)=X**2*(3*SIN(X)+X*COS(X))
      Y(3)=X**2*(6*COS(X)-X*SIN(X))+6*X*SIN(X)
      PRINT 3
      PRINT 2,X,Y(1),Y(2),Y(3)
  5   CONTINUE
  2   FORMAT(10X,F3.1,3F25.10/////)
  3   FORMAT(45X,17HANALYTIC SOLUTION//)
  10  FORMAT(//50X,9HTEST D203///10X,1HX,17X,4HY(1),22X,4HY(2),
     122X,4HY(3)//)
      END
      SUBROUTINE EXTERN (X,Y,F)
      DIMENSION Y(20),F(20)
      F(1)=X**2*(3*SIN(X)+X*COS(X))
      F(2)=X**2*(6*COS(X)-X*SIN(X))+6*X*SIN(X)
      F(3)=-X**2*(X*COS(X)+9*SIN(X))+18*X*COS(X)+6*SIN(X)
      RETURN
      END
*EXECUTE
*
*
