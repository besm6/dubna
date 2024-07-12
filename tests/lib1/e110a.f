      PROGRAM E110 A
      DIMENSION A(10),B(10),XA(10)
      DO 1 I=1,10
      XA(I)=I*0.1
      B(I)=I*I
 1    CONTINUE
      PRINT10,XA,B
 10   FORMAT(1X,10F10.3)
      H=.1
      DO 3 J=1,2
      N2=1./H+3.
      DO 2 I=1,N2
      X=(I-1)*H
      CALL PARINT(X,.1,.1,B,10,R)
      PRINT 11,X,R
11    FORMAT(1X,8HTEST X,R,5X,2F20.12)
 2    CONTINUE
      H=H/2.
 3    CONTINUE
      STOP
      END
*EXECUTE
*
*
