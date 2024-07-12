      PROGRAM W505
C   USER  MUST  REQUIRE  LIBRARY 1  AND  LIBRARY 2
C        CARDS  OF  DATAS    17
      CALL FOWL
      STOP
      END
      SUBROUTINE USER
C-----------------  EXAMPLE  2  ----------------------------------------
      COMMON/UTIL/PLACE(150),WEIGHT(150),NP,TECM,AMASS(18),PCM(5,18),WT
      COMMON/FDUMP/QQ1,QQ2,TDM,DUMMY(47)
      QQ1 = FMASS(1,2,0,0,0,0)
      QQ2 = FMASS(3,2,0,0,0,0)
      I1 = 0
      I2 = 0
      IF(QQ1 .GT. 1.200 .AND. QQ1 .LT. 1.300) I1 = 1
      IF(QQ2 .GT. 1.200 .AND. QQ2 .LT. 1.300) I2 = 1
      IF(I1 + I2 .NE. 1) GO TO 200
      IF(I1 .EQ. 1) GO TO 50
      PLACE(6) = FMASS(1,4,0,0,0,0)
      TDM = DELSQ(3,2,0,0,0,0)
      GO TO 100
   50 PLACE(6) = FMASS(3,4,0,0,0,0)
      TDM = DELSQ(1,2,0,0,0,0)
  100 CONTINUE
      IBANDE = TDM/0.1 + 1.0
      IF(IBANDE .GT. 5) IBANDE = 5
      PLACE(IBANDE) = PLACE(6)
      RETURN
  200 WT = 0.0
      RETURN
      END
*EXECUTE
     10000      5000         5
 0.139     0.9382    2.7500
         4  0.1396   0.9382    0.1396    0.1396
         1   0.200    1.300    0.020
 MASS PIPI WITH ISOBAR   DELSQ BETWEEN 0.0 AND 0.1
         2  0.200    1.500     0.020
 MASS PIPI WITH ISOBAR   DELSQ BETWEEN 0.1 AND 0.2
         3
 MASS PIPI WITH ISOBAR   DELSQ BETWEEN 0.2 AND 0.3
         4
 MASS PIPI WITH ISOBAR   DELSQ BETWEEN 0.3 AND 0.4
         5
 MASS PIPI WITH ISOBAR   DELSQ ABOVE 0.4
         6
 MASS PIPI WITH ISOBAR       ALL DELSQ
SCAT     1         6         6
         0
*
*
