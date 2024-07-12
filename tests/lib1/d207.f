      PROGRAM D207
      DIMENSION Y(3),F(3)
      EXTERNAL EXTERN
      X=0.0
      XEND=2.0
      PRINT 10
      CALL BULSTO(3,X,XEND,Y,0.1,10E-09,EXTERN)
      PRINT 2,X,Y
      Y(1)=X**3*SIN(X)
      Y(2)=X**2*(3*SIN(X)+X*COS(X))
      Y(3)=X**2*(6*COS(X)-X*SIN(X))+6*X*SIN(X)
      PRINT 3
      PRINT 2,X,Y
  2   FORMAT(10X,F3.1,3F25.10)
  3   FORMAT(//45X,17HANALYTIC SOLUTION//)
  10  FORMAT(//50X,9HTEST D207///10X,1HX,17X,4HY(1),22X,4HY(2),
     122X,4HY(3)//)
      END
      SUBROUTINE EXTERN (X,Y,F)
      DIMENSION Y(3),F(3)
      F(1)=X**2*(3*SIN(X)+X*COS(X))
      F(2)=X**2*(6*COS(X)-X*SIN(X))+6*X*SIN(X)
      F(3)=-X**2*(X*COS(X)+9*SIN(X))+18*X*COS(X)+6*SIN(X)
      RETURN
      END
*EXECUTE
*
*
