        PROGRAM J504
      CALL HIST(6,0.,0.,0.,0,0)
         DO 10 I=1,500
      Y=RNDM(-1)
      CALL HIST(1,1.,Y,0.,1,0)
      X=RNDM(-1)**2
      CALL HIST(1,1.,X,0.,2,0)
      X=ASIN(RNDM(-1))*57.3
      CALL HIST(1,1.,X,0.,5,0)
      CALL HIST(2,1.,Y,X,4,5)
   10 CONTINUE
      CALL HIST(3,0.,0.,0.,0,0)
      END
*EXECUTE
  0123456789ABCDEFGHIJKLMNOPQRSTUVWXY0.-XX
 ----Y(HI00)-X(HI00)-PLOT(60X60),A=10-19,B=20-29,Z.GE.260----
1100 3    THIS   IS   HISTOGRAM   OF   RANDOM   NUMBER   DISTRIBUTION
          FROM   0   TO   1.0                  1.0        .0       .01
       0.0       .01
1 60 5    THIS   IS   TWO-DIMENSION   HISTOGRAM     WITH   PROJECTS
          ON   AXIS                            4.0       0.0       .02
       0.0        2.        0.        0.        0.        0.        0.
     0
        .0        .0        .0        .0        .0        .0        .0
        .0        .0        .0        .0        .0        .0        .0
*
*
