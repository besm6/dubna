      PROGRAM D111
      DOUBLE PRECISION TEND,UMID,F,A,B,X,Y,EPSIN,EPSOUT
      COMMON /GPINT/ TEND,UMID,N,LINE,IOUT,JOP,KOP,T
      DOUBLE PRECISION G,H,Q,W
      EXTERNAL F
      EXTERNAL G, H,Q,W
      JOP=1
      A=0.0D0
      B=1.0D0
       EPSIN=1.0D-19
      IOP=3
   20 CONTINUE
      PRINT 4
      PRINT 23,IOP,EPSIN
  23  FORMAT(/////,3X,4HIOP=,I2,30H         .....................,
     16HEPSIN=,D28.20,25H........................,,///)
       Y=GPINDP(A,B,EPSIN,EPSOUT,F,IOP)
    4 FORMAT(////,10X,26H=TEST LI SANG HO= 4,4,1972,///)
      PRINT 2,Y
   2  FORMAT (3X,25HINTERGRAL 1.0D0/(1.0D0+X),///,3X,D36.20,//,
     155H*******************************************************,////)
       Y=GPINDP(A,B,EPSIN,EPSOUT,G,IOP)
       PRINT 7,Y
       Y=GPINDP(A,B,EPSIN,EPSOUT,Q,IOP)
       PRINT 6,Y
       Y=GPINDP(A,B,EPSIN,EPSOUT,W,IOP)
       PRINT 5,Y
       Y=GPINDP(A,B,EPSIN,EPSOUT,H,IOP)
       PRINT 3,Y
      IOP=IOP-1
      IF(IOP) 21,21,20
  21   CONTINUE
    3 FORMAT (3X,25HINTERGRAL DSQRT(X)       ,///,3X,D36.20,//,
     155H*******************************************************,////)
    6 FORMAT (3X,25HINTERGRAL DEXP(X)        ,///,3X,D36.20,//,
     155H*******************************************************,////)
    5 FORMAT (3X,25HINTERGRAL DSIN(X)        ,///,3X,D36.20,//,
     155H*******************************************************,////)
    7 FORMAT (3X,25HINTERGRAL DCOS(X)        ,///,3X,D36.20,//,
     155H*******************************************************,////)
      END
      DOUBLE PRECISION FUNCTION F(X)
      DOUBLE PRECISION X
      F=1.0D0/(1.0D0+X)
      RETURN
      END
      DOUBLE PRECISION FUNCTION H(X)
      DOUBLE PRECISION X
       H=DSQRT(X)
       RETURN
      END
      DOUBLE PRECISION FUNCTION G(X)
      DOUBLE PRECISION X
      G=DCOS(X)
       RETURN
      END
      DOUBLE PRECISION FUNCTION Q(X)
      DOUBLE PRECISION X
      Q=DEXP(X)
       RETURN
      END
      DOUBLE PRECISION FUNCTION W(X)
      DOUBLE PRECISION X
       W=DSIN(X)
      RETURN
      END
*EXECUTE
*
*
