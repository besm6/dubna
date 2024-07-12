      PROGRAM J551
      DIMENSION A(50,2)
      DX=2.*3.14159265359/50.
      DO 1 I=1,50
      X=DX*(I-1)
      A(I,1)=SIN(X)
    1 A(I,2)=COS(X)
      CALL FIGURE(2,50,A,0,5HI-XSC)
      CALL FIGABS(2,50,A,0,5H  XSC)
      CALL FIGREL(2,50,A,0,5H  OSC)
      STOP
      END
*EXECUTE
*
*
