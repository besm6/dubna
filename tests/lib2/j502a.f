      PROGRAM J502A
      DIMENSION STEP(3),X(31),Y(30)
      DATA(STEP=.01,.1,5.)
      EXTERNAL ALOG10
      A=5.
      XN=-1.5
      J=1
      X(1)=.01
      DO 1 I=1,30
      Y(I)=A*X(I)**XN
      X(I+1)=X(I)+STEP(J)
      IF(I.EQ.9)J=2
      IF(I.EQ.19)J=3
    1 CONTINUE
      DO 2 I=1,30
      X(I)=ALOG10(X(I))
    2 Y(I)=ALOG10(Y(I))
      CALL SCALES(X,30,7.,XMIN,DX,1)
      CALL SCALES(Y,30,5.,YMIN,DY,1)
      DO 3 I=1,30
      X(I)=X(I)+2.
    3 Y(I)=Y(I)+2.
      CALL PLTIN(9.)
      CALL LINEAR(X,Y,30,1)
      CALL AXILOG(2.,2.,12HY=A*X**(-XN),12,5.,90.,YMIN,DY,ALOG10)
      CALL AXILOG(2.,2.,1HX,1,7.,0.,XMIN,DX,ALOG10)
      CALL ENDPZD
      STOP
      END
*EXECUTE
BERCEANU    LNF,LVTA      1200
*
*
