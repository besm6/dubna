      PROGRAM I312 B
C   ПPИMEP B OПИCAHИИ
      DIMENSION A(6)
      COMMON /AA/C(21)
      DATA A/0.2787,-98765.,-2.8641E-5,0.27E-19,2E18,-1.9E-18/
      PRINT 1,A
  1   FORMAT(6E20.7)
      CALL PRINTF(A(1),A(6),3,4)
      CALL PRINTL(A(1),A(6),3,4,1,6H TECT )
      CALL PRNT(6,A(2),A(4),A(6),3,2,10H+TECT PRNT)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
