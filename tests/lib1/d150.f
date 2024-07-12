      PROGRAM D150
      COMMON/NFUN/NFUN
      IND=10
      ERR=0.
      SUM=0.
      NFUN=1
      DO 30 NF=1,2
      DO 20 I=1,IND
      IDEG=0
      EDIG=1
      IDEG=2
      IDEG=5
      IDEG=3
      SPRDMX=1.5
      CALL PARTN(I,SPRDMX,10000)
      CALL INTGRL(IDEG,RLINE,ERROR)
      SUM=SUM+RLINE
      ERR=ERR+ERROR**2
  20  CONTINUE
      NFUN=9
      ERR=SQRT(ABS(ERR))
      PRINT 40,SUM,ERR
  30  CONTINUE
  40  FORMAT(1X,6HVALUES,5X,2E15.8)
      STOP
      END
      FUNCTION FUN(KR,V)
      DIMENSION V(KR),FUNCT(10),R(10,2)
      DIMENSION FAC(10)
      COMMON/NFUN/NFUN
      DATA FLAG/0./
      DATA(PI=3.14159265)
      DATA(EPS1=1.E-6)
      IF(FLAG-5000.)1000,1001,1001
 1001 CONTINUE
      FLAG=-FLAG
 1000 CONTINUE
      S=KR
      DO 1 K=1,10
  1   FAC(K)=1.
      FAC1=2.
      FAC(2)=3./(2.*S)
      FAC3=1./(EXP(1.)-1.)
      FAC(4)=1./(S*(2.*ALOG(2.)-1.))
      FAC(5)=12./(S*(3*S+1.))
      FAC6=3./2.
      FAC7=0.5
      FAC8=0.25
      FAC9=2./PI
      FAC10=SQRT(2.*FAC9)
      SUM2=0.
      SUM20=0.
      SUM10=0.
      P1=1.
      P4=1.
      P9=1.
      P10=1.
      DO 10 I=1,KR
      V1=1.-V(I)
      SUM20=SUM20+V(I)
      IF(V1-EPS1)3,3,2
  2   CONTINUE
      SUM10=SUM10+(V(I)/V1)**2
      GO TO 4
  3   CONTINUE
      SUM10=100.
  4   CONTINUE
      P1=P1*V(I)
      P4=P4*(1.+V(I))
      P9=P9*(1.-V(I)**2)
      P10=P10*V1
      FAC(1)=FAC(1)*FAC1
       FAC(3)=FAC(3)*FAC3
      FAC(6)=FAC(6)*FAC6
      FAC(7)=FAC(7)*FAC7
      FAC(8)=FAC(8)*FAC8
      FAC(9)=FAC(9)*FAC9
   10 FAC(10)=FAC(10)*FAC10
      FUNCT(1)=FAC(1)*P1
      FUNCT(2)=FAC(2)*SUM2
      FUNCT(3)=FAC(3)*EXP(SUM20)
      FUNCT(4)=FAC(4)*ALOG(P4)
      FUNCT(5)=FAC(5)*SUM20**2
      FUNCT(6)=0.
      FUNCT(7)=0.
      FUNCT(8)=0.
      FUNCT(9)=0.
      IF(P9-EPS1)200,200,201
 201  CONTINUE
      FUNCT(9)=FAC(9)/SQRT(P9)
 200  CONTINUE
      FUNCT(10)=0.
      IF(P10-EPS1)102,102,101
 101  CONTINUE
      FUNCT(10)=FAC(10)*EXP(-SUM10)/P10**2
 102  CONTINUE
      FUN=FUNCT(NFUN)
      RETURN
      END
*CALL ALLMEMORY
*EXECUTE
*
*
