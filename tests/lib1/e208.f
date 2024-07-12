      PROGRAM E208
      DIMENSION A(4),X(6),Y(6),M(3)
      DATA X/-1.05,-0.37,0.82,0.98,1.67,3.5/  ,
     1Y/0.0,2.1,0.78,0.0,-0.85,3.99/,M/4,7,21/
      NAX=6
      PRINT 1,X,Y
      DO 5 I=1,3
      MAX=M(I)
      PRINT 2,NAX,MAX
      CALL LSQ(NAX,X,Y,MAX,A)
      PRINT 3,A
      MAX=MAX+10
  5   CONTINUE
  1    FORMAT(//50X,9HTEST E208///30X,2HX=,6(F5.1)/30X,2HY=,6(F5.1)//)
  2    FORMAT(30X,4HNAX=,I2,10X,4HMAX=,I2/)
  3   FORMAT(10X,2HA=,4(F20.12)//)
      STOP
      END
*EXECUTE
*
*
