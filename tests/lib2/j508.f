      PROGRAM J508
      DIMENSION Q(50), TITLE(20)
      DATA (TITLE=9HHISTOGRAM,5H TEST)
      DATA (Q=0.1,1.0,5.2,2.3,10.5,15.7,3.5,2.2,1.0,30.5,
     1-1.5,6.7,2.0,8.5,9.1,-2.0,-0.5,0.0,2.2,3.5,
     22.5,4.7,4.1,2.0,6.5,4.5,3.7,4.2,7.3,11.5,
     315.0,12.5,13.7,14.8,15.0,19.0,22.0,-2.5,-0.2,-0.25,
     425.0,28.5,35.0,27.0,32.0,10.5,-1.0,-0.9,30.0,17.7)
      NQ=50
      QMIN=-2.5
      RESOL=2.0
      NBIN=25
      CALL PLOTH(Q,NQ,QMIN,RESOL,NBIN,TITLE)
      QMIN=0.0
      RESOL=5.0
      NBIN=10
      CALL PLOTH(Q,NQ,QMIN,RESOL,NBIN,TITLE)
      QMIN=1.0
      RESOL=5.0
      NBIN=10
      CALL PLOTH(Q,NQ,QMIN,RESOL,NBIN,TITLE)
      QMIN=-0.5
      RESOL=10.0
      NBIN=5
      CALL PLOTH(Q,NQ,QMIN,RESOL,NBIN,TITLE)
      QMIN=5.5
      RESOL=2.0
      NBIN=25
      CALL PLOTH(Q,NQ,QMIN,RESOL,NBIN,TITLE)
      QMIN=-35.0
      RESOL=2.0
      NBIN=25
      CALL PLOTH(Q,NQ,QMIN,RESOL,NBIN,TITLE)
      END
*EXECUTE
*
*