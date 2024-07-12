      PROGRAM M205
      COMPLEX CSIGN,B,C,E
      B=(2.1,-3.2)
      C=(-2.2,3.5)
      PRINT 10
      E=CSIGN(B,C)
      X=SIGN(REAL(B),REAL(C))
      Y=SIGN(AIMAG(B),AIMAG(C))
      PRINT 1,B,C,E,X,Y
  1   FORMAT(10X,3HB=(,F4.1,1H,,F4.1,1H),10X,3HC=(,F4.1,1H,,F4.1,1H),15X
     *  ,3HE=(,F4.1,1H,,F4.1,1H)//60X,2HX=,F4.1,10X,2HY=,F4.1)
  10  FORMAT(//50X,9HTEST M205///)
      STOP
       END
*EXECUTE
*
*
