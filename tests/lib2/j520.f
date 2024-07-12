      PROGRAM J520
      DIMENSION X(20),Y(20),Z(20),U(20),W(20)
      DO 1 J=1,20
      X(J)=.1*J*J
      Y(J)=SQRT (X(J)) +1.
      U(J)=SQRT (X(J)) +2.
      Z(J)=SQRT (X(J)) +3.
      W(J)=SQRT (X(J)) +4.-.1*X(J)
   01 CONTINUE
      PRINT 10,X,Y,U,Z,W
      X(3)=0 .3
      CALL GRAPH1(20,X,Y)
      X(3) =0.5
      CALL GRAPH4(20,X,Y,Z,U,W)
      PRINT 5
      X(3)=0.9
      CALL GRAPH1(20,X,Y)
      CALL GRAPH2(20,X,Y,Z)
      CALL GRAPH3(20,X,Y,Z,U)
      CALL GRAPH4 (20,X,Y,Z,U,W)
  5   FORMAT(1H1)
  10  FORMAT(1H1//50X,9HTEST J520///1X,2HX=,20F5.1//1X,2HY=,20F5.1//1X,
     *       2HU=,20F5.1//1X,2HZ=,20F5.1//1X,2HW=,20F5.1//)
        STOP
      END
*EXECUTE
*
*
