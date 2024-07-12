      PROGRAM C314
         Y=1.0
         PRINT 10
         X=-5.0
         DO 1 I=1,10
         DO 1 K=1,10
         X=X+1.5708
         Z1=THETA1(X,Y)
         Z2=THETA2(X,Y)
         Z3=THETA3(X,Y)
         Z4=THETA4(X,Y)
         PRINT 11,X,Y,Z1,Z2,Z3,Z4
  1      CONTINUE
  10     FORMAT(//12X,1HX,17X,1HY,17X,6HTHETA1,12X,6HTHETA2,12X,
     *   6HTHETA3,12X,6HTHETA4//)
  11     FORMAT(6F18.4)
      END
*EXECUTE
*
*
