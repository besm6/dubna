      PROGRAM D504
C        CARDS  OF  DATAS    6
      CALL MINROS
      STOP
      END
      SUBROUTINE FCN(NPAR,G,F,X,IFLAG)
      DIMENSION X(20)
      IF (IFLAG.EQ.1) GO TO 20
      RADIK=1.-((X(1)-1)/2)**2-(X(2)-2)**2
      F=-9.*SQRT(RADIK)
      IF (IFLAG.EQ.4) GOTO 20
      PRINT 1
  1   FORMAT(//62H THE FOLLOW VALUE FOR F TO SATISFACTORY   THE PARAMETE
     1RS EQUAL)
      PRINT 2,F,X(1),X(2)
    2 FORMAT(1X,3E12.5)
      STOP
   20 RETURN
      END
*EXECUTE
 1 1
 OB ES WAS WIRD
         2       100     10000        10                               D
    200000    275000                                                   D
     10000     10000                                                   D

*
*
