      PROGRAM C340
      EXTERNAL BIINT1,BIINT2,BKINT
      COMMON /FORINT/ X,P
      DIMENSION XX(16),N(6)
      DATA XX /0.00001,0.0001,0.001,0.01 ,0.1,0.5,1.,2.,3.,4.,
     1 4.999999999999,5.,6.,7.,8.,8.00000000001/
      DATA N /1,2,-1,-2,1,2/
      DATA EPS /1.0E-13/, DEL /1.0E-14/
      DATA PI /3.141592653589793/
      TEIR3(X,P)=(GAUSS (BIINT1,0.,PI,EPS*H    )-SIN(PI*P)*GAUSS (BIINT2
     1 ,0.,TM,EPS*H    ))/PI
      TEKR3(X,P)=GAUSS (BKINT,0.,TM,EPS*H)
      NFIGUR(U,V)=MIN0(14,IABS(INT(ALOG10(AMAX1(1.0E-12,ABS(U))))-
     1                         INT(ALOG10(AMAX1(1.0E-12,ABS(V))))))
      DO 11 J = 1,6
      NU=N(J)
      P=FLOAT(NU)/3.0
      IF(J .EQ. 1) PRINT 12
      IF(J .EQ. 5) PRINT 13
      IF(J .NE. 1 .AND. J .NE. 5) PRINT 3
      DO 11 I=1,16
      X=XX(I)
      TM=0.
      IF(J .GT. 4) GO TO 4
      F=0.
      G=0.
      H=EBSIR3(X,NU)
      IF(X .LT. 1.0) GO TO 9
      IF(X .GE.  300.0) GO TO 5
      TM=TMAX(X,P,1)
      T=TEIR3(X,P)
      GO TO 6
    5 T=AEIKR3(X,P,1)
      GO TO 6
    9 T=EXP(-X)*SERIP(X,P)
    6 IF(X .GT. 600.0) GO TO 1
      F=BSIR3(X,NU)
      G=EXP(X)*H
      GO TO 1
    4 F=0.
      G=0.
      H=EBSKR3(X,NU)
      IF(X .GE.  300.0) GO TO 7
      TM=TMAX(X,P,2)
      T=TEKR3(X,P)
      GO TO 8
    7 T=AEIKR3(X,P,2)
    8 IF(X .GT. 600.0) GO TO 1
      F=BSKR3(X,NU)
      G=EXP(-X)*H
    1 NF=NFIGUR(H,H-T)
      PRINT 2, NU,X,F,G,H,T,NF,TM
   11 CONTINUE
      PRINT 3
      H=BSIR3(1.0,+3)
      H=BSIR3(1.0,-3)
      H=BSIR3(0.0,+1)
      H=BSIR3(0.0,-1)
      H=BSIR3(0.0,-3)
      H=BSKR3(1.0,+3)
      H=BSKR3(1.0,-3)
      H=BSKR3(0.0,+3)
      H=BSKR3(0.0,-1)
    2 FORMAT(1X,I4,E10.2,4E23.13,I5,F6.1)
    3 FORMAT(1H4)
   12 FORMAT(1H1/ 5X  10HI NU/3 (X)//
     1     60H   NU      X             I (X)           EXP(X)*(EXP(-X)*I
     2(X,  56H))          EXP(-X)*I(X)               TEST           CD/)
   13 FORMAT(1H1/ 5X  10HK NU/3 (X)//
     1     60H   NU      X               I (X)         EXP(X)*(EXP(-X)*K
     2(X,  56H))          EXP(-X)*K(X)               TEST           CD/)
      STOP
      END
      FUNCTION TMAX(X,P,L)
      DATA EPS /1.0E-12/
      TMAX=0.6
      IF(X .GT. 400.0) RETURN
      K=1
      DO 10 I = 1,3000,K
      IF(I .GE. 21) K=10
      IF(I .GE. 101) K=100
      TMAX=0.1*FLOAT(I)
      CH=EXP(TMAX)
      CH=0.5*(CH+1.0/CH)
      GO TO (1,2), L
    1 F=EXP(-X*(1.0+CH)-P*TMAX)
      IF(F .LT. EPS*EXP(-2.0*X)) RETURN
      GO TO 10
    2 F=EXP(AMAX1(X*(1.0-CH)+P*TMAX,-675.81))
      IF(F .LT. EPS) RETURN
   10 CONTINUE
      RETURN
      END
      FUNCTION AEIKR3(X,P,L)
      DATA PI /3.141592653589793/
      Q=4.0*P**2
      Y=8.0*X
      V=(-1.0)**L
      V1=Q-1.0
      V2=V1*(Q-9.0)
      V3=V2*(Q-25.0)
      V4=V3*(Q-49.0)
      V5=V4*(Q-81.0)
      V6=V5*(Q-121.0)
      V7=V6*(Q-169.0)
      AEIKR3=1.0+V*V1/Y+V2/(2.0*Y**2)+V*V3/(6.0*Y**3)+V4/(24.0*Y**4)+
     1 V*V5/(120.0*Y**5)+V6/(720.0*Y**6)+V*V7/(5040.0*Y**7)
      GO TO (1,2), L
    1 AEIKR3=AEIKR3/SQRT(2.0*PI*X)
      RETURN
    2 AEIKR3=AEIKR3*SQRT(PI/(2.0*X))
      RETURN
      END
      FUNCTION BIKINT(T)
      COMMON /FORINT/ X,P
      ENTRY BIINT1
      BIKINT=EXP(X*(COS(T)-1.0))*COS(P*T)
      RETURN
      ENTRY BIINT2
      CH=EXP(T)
      CH=0.5*(CH+1.0/CH)
      BIKINT=EXP(-X*(1.0+CH)-P*T)
      RETURN
      ENTRY BKINT
      CH=EXP(T)
      CH=0.5*(CH+1.0/CH)
      CHP=EXP(P*T)
      CHP=0.5*(CHP+1.0/CHP)
      BIKINT=EXP(AMAX1(X*(1.0-CH),-675.81))*CHP
      RETURN
      END
      FUNCTION SERIP(X,P)
      Y=(0.5*X)**2
      A=1.0/GAMMA(P+1.0)
      S=A
      K=-1
    1 K=K+1
      FK=K+1
      A=(1.0/(FK*(P+FK)))*A*Y
      S=S+A
      IF(ABS(A) .GT. 1.0E-12) GO TO 1
      SERIP=(0.5*X)**P*S
      RETURN
      END
*CALL PTIME
*EXECUTE
*
*
