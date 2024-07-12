      PROGRAM D121
      COMMON/MITIME/TIME
      DIMENSION T(2)
      DIMENSION V(10),FUN(10),R(10,2),AX(20),BX(20)
      EXTERNAL FUNCT
      DO 1 I=1,20
      CALL DATKOR(I,1)
   1  CONTINUE
      KOL=10
      EPS=1.E-12
      KR=1
      KR=4
      KR=6
      KR=7
      KR=8
      KR=9
      KR=10
      KR=11
      T=10000.
      T=200000.
      L=0
      L=1
      L=3
      L=4
      L=6
      L=20
      L=7
      H=3.
      H=5.
      H=20.
      L=5
      H=10.
      H=1.
      H=0.
      L=2
      T=1000.
      KR=2
      T=0.
      T(2)=1000.
      KR=5
      EPS=1.E-3
      EPS=0.
      KR=3
      DO 99 I=1,20
      AX(I)=0.
 99   BX(I)=1.
      DO 2 K=1,2
      T=-1. $ L=-1
      TIME=20.
      DO 10 J=1,11
      CALL MIKOR(KR,KOL,V,FUN,R,T,L,H,EPS,FLAG,FUNCT,AX,BX)
      L=L+1
      IF(FLAG)301,301,300
 300  CONTINUE
      DO 20 I=1,10
      R(I)=R(I)-1.
      IF(EPS)29,19,29
 19   CONTINUE
      R(I+10)=R(I+10)-1.
 29   CONTINUE
 20   CONTINUE
      KP=FLAG
      PRINT 801,L,KR,KP,H
      PRINT 800,R
801   FORMAT(1X,2HL=,I3,5X,3HKR=,I3,5X,3HKP=,I10,5X,2HH=,F6.2/)
 800  FORMAT(1X,3HR1=,10E10.2/1X,3HR2=,10E10.2/)
 301  CONTINUE
  10  CONTINUE
      EPS=-1.E-3
      EPS=-1.E-2
  2   CONTINUE
      END
      SUBROUTINE FUNCT(KR,KOL,V,FUN,R,EPS,FLAG)
      DIMENSION V(KR),FUN(10),R(10,2)
      DIMENSION FAC(10)
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
      SUM2=SUM2+SQRT(V(I))
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
      SP1=SQRT(P1)
      FAC(1)=FAC(1)*FAC1
       FAC(3)=FAC(3)*FAC3
      FAC(6)=FAC(6)*FAC6
      FAC(7)=FAC(7)*FAC7
      FAC(8)=FAC(8)*FAC8
      FAC(9)=FAC(9)*FAC9
   10 FAC(10)=FAC(10)*FAC10
      FUN(1)=FAC(1)*P1
      FUN(2)=FAC(2)*SUM2
      FUN(3)=FAC(3)*EXP(SUM20)
      FUN(4)=FAC(4)*ALOG(P4)
      FUN(5)=FAC(5)*SUM20**2
      FUN(6)=FAC(6)*SP1
      FUN(7)=0.
      FUN(8)=0.
      IF(P1-EPS1)100,100,99
 99   CONTINUE
      FUN(7)=FAC(7)/SP1
      FUN(8)=FAC(8)/SQRT(SP1)**3
 100  CONTINUE
      FUN(9)=0.
      IF(P9-EPS1)200,200,201
 201  CONTINUE
      FUN(9)=FAC(9)/SQRT(P9)
 200  CONTINUE
      FUN(10)=0.
      IF(P10-EPS1)102,102,101
 101  CONTINUE
      FUN(10)=FAC(10)*EXP(-SUM10)/P10**2
 102  CONTINUE
      RETURN
      END
*EXECUTE
*
*
