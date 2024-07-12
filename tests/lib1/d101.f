      PROGRAM D101
      EXTERNAL F
      CALL WTS
      Z=GS10(F,0.0,1.57)
      Y=GS12(F,0.0,1.57)
      PRINT 1,Z,Y
 1    FORMAT(//30X'TEST D101'//10X'F=SIN(X)'/20X'GS10(F,0.,1.57)='F20.10
     1/20X'GS12(F,0.,1.57)='F20.10)
      STOP
      END
      FUNCTION F(X)
      F=SIN(X)
      RETURN
      END
*EXECUTE
*
*
