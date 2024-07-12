      PROGRAM D221
      DIMENSION Y(20),W(2,3)
      EXTERNAL NAME
      X=0.0  $  Y(1)=1.0  $  Y(2)=1.0
      CALL INSAUT(2,X,Y,0.1,0.01,1.0,0.0,NAME,W)
      PRINT 12,X,Y(1),Y(2)
      X=0.0  $  Y(1)=1.0  $  Y(2)=1.0
      CALL INSINT(2,X,Y,0.1,0.01,1.0,0.0,NAME,W)
      PRINT 14,X,Y(1),Y(2)
      STOP
 12   FORMAT(///'INSAUT'/3X,2HX=,F5.2,5X,3HY1=,E20.11,5X,3HY2=E20.11//)
 14   FORMAT(///'INSINT'/3X,2HX=,F5.2,5X,3HY1=,E20.11,5X,3HY2=E20.11//)
      END
      SUBROUTINE NAME(X,Y,F)
      DIMENSION Y(20),F(20)
      F(1)=SIN(X)
      F(2)=EXP(X)
      RETURN
      END
*EXECUTE
*
*
