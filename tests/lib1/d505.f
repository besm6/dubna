      PROGRAM D505
C        CARDS  OF  DATAS  6
      CALL MINCON
      STOP
      END
      SUBROUTINE FCN (NPAR,G,F,X,IFLAG)
      DIMENSION X(60), G(60)
      IF (IFLAG.EQ.1) GO TO 10
  101 CONTINUE
      RADIK=1.-((X(1)-1.)/2.)**2-(X(2)-2.)**2
      IF (1.-((X(1)-1.)/2)**2)  120,102,102
  120 CONTINUE
      X(1)=0.
      GO TO 101
  102 IF (1.-(X(2)-2)**2)  122,110,110
  122 CONTINUE
      X(2)=1.3
      GO TO 101
  110 IF (RADIK)  124,103,103
  124 X(1)=0.
      X(2)=1.3
      GO TO 101
  103 F=-9*SQRT(RADIK)
      IF (IFLAG.EQ.4) GO TO 20
      IF (IFLAG.EQ.3) GO TO 30
      G(1)=-20.25*(X(1)-1)/F
      G(2)=-81.*(X(2)-2)/F
   20 RETURN
   10 G(1)=0.
      G(2)=0.
      RETURN
   30 PRINT 1
    1 FORMAT (//64H THE FOLLOW VALUE FOR F IS SATISFACTORY  THE PARAMETE
     1RS EQUAL   )
      PRINT 2,F,X(1),X(2)
    2 FORMAT (1X,3E12.5)
      STOP
      END
*CALL PTIME
*EXECUTE
000111
 OB ES WAS WIRD
         2       100     10000        10
    200000    275000
     10000     10000

*
*           D516  ИЗMEHИЛИ  HA  D506       07/1985
