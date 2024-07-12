      PROGRAM D115
      DOUBLE PRECISION  F,A,B,X,Y, EPSIN,EPSOUT
      DOUBLE PRECISION G,H,Q,W
      EXTERNAL F
      EXTERNAL G, H,Q,W
      A=0.0D0
      B=1.0D0
       EPSIN=1.0D-19
      JOP=1
C     PRINT 4
      Y=CHEBQU(A,B,EPSIN,EPSOUT,JOP,F)
      PRINT 2,Y
      Y=CHEBQU(A,B,EPSIN,EPSOUT,JOP,G)
      PRINT 7,Y
C     Y=CHEBQU(A,B,EPSIN,EPSOUT,JOP,Q)
C     PRINT 6,Y
C     Y=CHEBQU(A,B,EPSIN,EPSOUT,JOP,W)
C     PRINT 5,Y
      Y=CHEBQU(A,B,EPSIN,EPSOUT,JOP,H)
       PRINT 3,Y
    3 FORMAT (3X,25HINTERGRAL DSQRT(X)       ,///,3X,D36.20,//,
     155H*******************************************************,////)
    5 FORMAT (3X,25HINTERGRAL DSIN(X)        ,///,3X,D36.20,//,
     155H*******************************************************,////)
    6 FORMAT (3X,25HINTERGRAL DEXP(X)        ,///,3X,D36.20,//,
     155H*******************************************************,////)
    7 FORMAT (3X,25HINTERGRAL DCOS(X)        ,///,3X,D36.20,//,
     155H*******************************************************,////)
   2  FORMAT (3X,25HINTERGRAL 1.0D0/(1.0D0+X),///,3X,D36.20,//,
     155H*******************************************************,////)
      END
      DOUBLE PRECISION FUNCTION F(X)
      DOUBLE PRECISION X
      F=1.0D0/(1.0D0+X)
      RETURN
      END
      DOUBLE PRECISION FUNCTION G(X)
      DOUBLE PRECISION X
      G=DCOS(X)
       RETURN
      END
      DOUBLE PRECISION FUNCTION H(X)
      DOUBLE PRECISION X
       H=DSQRT(X)
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
