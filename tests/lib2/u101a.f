      PROGRAM U101 A
      C=CLEBSH(200.,200.,0.,0.,200.,0.)
      PRINT 4,C
      C=CLEBSH(0.,0.,0.,1.,0.,1.)
      PRINT 4,C
      C=CLEBSH(0.,0.,1.,1.,0.,2.)
      PRINT 4,C
      C=CLEBSH(1.,0.,1.,0.,2.,1.)
      PRINT 4,C
      C=CLEBSH(1.,0.,1.,0.,0.,1.)
      PRINT 4,C
      C=CLEBSH(1.,1.,1.,1.,1.,1.)
      PRINT 4,C
    4 FORMAT(10X,7HCLEBSH=,F14.9)
      DO 5 I=1,16
    2 READ 1,BETA,GAMMA,A,B,C
    1 FORMAT(5F4.1)
      PRINT 1,BETA,GAMMA,A,B,C
      D=GAMMA-BETA
      E=CLEBSH(A,B,D,BETA,C,GAMMA)
      PRINT 3,A,B,D,BETA,C,GAMMA,E
    3 FORMAT(10X,7HCLEBSH(,5(F5.1,1H,),F5.1,2H)=,F14.9)
    5 CONTINUE
      END
*EXECUTE
-.5 0.  .5  .5  1.
.5  0.  .5  .5  1.
.0  .0  1.  1.  1.
.0  .5  .5  1.  1.5
1.  1.5 .5  1.  1.5
1.5 1.5 1.  1.5 1.5
1.  2.  1.  1.  2.
-.5 0.  1.5 1.5 2.
.5  0.  1.5 1.5 2.
2.  1.  1.  2.  2.
1.5 3.  1.5 1.5 3.
2.5 1.5 1.  2.5 2.5
-.5 .5  2.  2.5 2.5
-1.50.  1.5 2.5 3.
2.  3.  3.  3.  3.
1.5 3.5 3.  3.5 3.5
*
*
