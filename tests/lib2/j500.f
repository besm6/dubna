      PROGRAM J500
      DIMENSION TITLE(36)
      DIMENSION LA(50)
      DIMENSION LB(50)
      REAL LA,LB
      DATA(TITLE=19HASYMMETRY PARAMETER)
      DATA(LA=3.5,5.4,2.2,3.4,3.2,3.0,2.2,4.0,2.2,3.2,2.0,2.0,2.0,
     12.0,1.7,1.3,1.9,0.9,0.5,1.8,1.6,1.3,0.5,8.0,9.1,4.3,2.6,2.9,
     22.7,2.6,2.9,1.9,2.3,4.5,2.4,2.4,2.5,2.5,3.6,1.8,2.0,1.9,2.0,
     37.0,1.5,1.5,1.5,2.0,16.0,3.2)
      DATA(LB=1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     11.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     21.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     31.0,1.0,1.0,1.0,1.0,1.0,1.0)
      HLIMIT=15.0
      SLIMIT=1.0
      RESOL=1.0
      IDIME=50
      CALL WHISTO(LA,HLIMIT,SLIMIT,RESOL,IDIME,LB,TITLE)
      END
*EXECUTE
*
*
