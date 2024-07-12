      PROGRAM U500
      DIMENSION DKEEP(20)
      N=1000
      DBETA = 360./(N-1)
      BETA = -DBETA
      DO 6 J= 1, 3
      AJ = FLOAT(J)/2.
      AM = AJ
      AN = 0.
      DO 5 I= 1, N
      BETA = -DBETA
    5 DSM = DSMALL(AJ,AM,AN,BETA)
    6  CONTINUE
      N = 20
      K = 0
      DBETA = 360./(N-1)
   10 K = K+1
      PRINT 30
      IF(K.GE.9) GO TO 70
      BETA = -DBETA
      GO TO (11,12,13,14,15,16,17,18),K
   11 AJ = 0.5
      AM = 0.5
      AN  = 0.5
      GO TO 20
   12 AJ = 0.5
      AM = 0.5
      AN  = -0.5
      GO TO 20
   13 AJ = 1.
      AM = 0.
      AN  = 0.
      GO TO 20
   14 AJ = 1.5
      AM = 1.5
      AN  = -0.5
      GO TO 20
   15 AJ = 1.5
      AM = 0.5
      AN  = -0.5
      GO TO 20
   16 AJ = 2.
      AM = 2.
      AN  = 0.
      GO TO 20
   17 AJ = 2.
      AM = 1.
      AN  = 0.
      GO TO 20
   18 AJ = 2.
      AM = 0.
      AN  = 0.
   20 PRINT 30
   30 FORMAT (1H0)
      PRINT 40
   40 FORMAT (13X,1HJ,7X,1HM,6X,2H N,4X,4HBETA,5X,5HDSMAL,7X,5HDTEST,
     * 6X,5HDIFF.)
      DO 50 I=1,N
      BETA = BETA+DBETA
      DSML = DSMALL(AJ,AM,AN,BETA)
      DTST = FTEST(BETA,K)
      DIFF = DTST-DSML
   50 PRINT 60,   AJ,AM,AN,BETA,DSML,DTST,DIFF
   60 FORMAT( 9X,4(F5.1,3X),2(F8.5,3X),E12.5)
      GO TO 10
   70 DSM1 = DSMALL(1.0,2.0,0.0,45.)
C        TEST FOR HIGH VALUES IF SPIN INDICES
      PRINT 149
  149 FORMAT (50H1 TEST DSMALL FOR LARGE VALUES OF INDICES            /)
      BET45 = 1.
      DO 200 I= 1, 12
      AI= I
      IPLUS1 = I + 1
      DO 190 J= 1, IPLUS1
      JM = J-1
      AJ = J-1
      CALL VZERO(DKEEP,J)
      DO 180 K= 1, IPLUS1
      AK = K-1
      DKEEP(K) = DSMALL(AI,AJ,AK,BET45)
  180 CONTINUE
      PRINT 210, I,JM,(DKEEP(KK),KK=1,J)
  190 CONTINUE
      PRINT 30
  200 CONTINUE
  210 FORMAT (2X,2I4,10F12.8/5X,10F12.8)
      END
      FUNCTION FTEST(BETA,K)
      BETB  = BETA*3.1415926535898/180.
      GO TO (11,12,13,14,15,16,17,18),K
   11 FTEST = COS(BETB/2.)
      RETURN
   12 FTEST = -SIN(BETB/2.)
      RETURN
   13 FTEST = COS(BETB)
      RETURN
   14 FTEST = SQRT(3.)*(1.-COS(BETB))*COS(BETB/2.)/2.
      RETURN
   15 FTEST = -(3.*COS(BETB)+1.)*SIN(BETB/2.)/2.
      RETURN
   16 FTEST = SQRT(1.5)*(SIN(BETB))**2/2.
      RETURN
   17 FTEST = -SQRT(1.5)*SIN(BETB)*COS(BETB)
      RETURN
   18 FTEST = 1.5*(COS(BETB))**2-0.5
      RETURN
      END
*EXECUTE
*
*
