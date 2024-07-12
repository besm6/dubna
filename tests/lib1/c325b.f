      PROGRAM C325 B
       COMPLEX AK,AM,Z,W,WHIT
      REAL K1,K2,M1,M2
      DIMENSION AK(150),Z(150),AM(150)
      DIMENSION X(25),Y(25),M2(150),K1(150),W(150,20),N2(20)
      DATA((X(I),I=1,16)=0.01,0.1,1.,10.,20.,0.5,0.5,0.5,0.5,0.5,-0.01,
     A-0.1,-1.,-10.,-20.,-50.)
      DATA((Y(I),I=1,16)=0.5,0.1,1.,10.,20.,1.,6.,10.,20.,50.,0.5,0.5,
     B1.,10.,20.,50.)
      TM=0.6
      TK=1.
      JG=3
      IG=10
      J21=16
      DO 1 J=1,IG
       K1(J)=TK/FLOAT(IG)*FLOAT(J)
   1   AK(J)=CMPLX(K1(J),0.)
      DO 2 J1=1,JG
       M2(J1)=TM/FLOAT(JG)*FLOAT(J1)
  2    AM(J1)=CMPLX(0.,M2(J1))
      DO 3 J2=1,J21
      Z(J2)=CMPLX(X(J2),Y(J2))
      TE=1.
      IF(X(J2).EQ.20.AND.Y(J2).EQ.20.)TE=0.5
      DO 10 J3=1,JG
      DO 10 J4=1,IG
      IF(REAL(AK(J4)) .GT . TE) GO TO 10
      N2(J3)=J4
      W(J4,J3)=WHIT(AK(J4),AM(J3),Z(J2))
  10   CONTINUE
      DO 3 K=1,JG
      J5=N2(K)
       PRINT 1973
      PRINT 110,Z(J2),AM(K)
       PRINT 111
       PRINT 112,(AK(IU),W(IU,K),IU=1,J5)
  110 FORMAT(10X,2HZ=,2D10.2,10X,3HAM=,2D10.2,//)
 111  FORMAT(2( 8X,2HK1,5X,2HK2,16X,2HW1,17X,2HW2)/)
  112 FORMAT(2(5X,2D7.0,2D19.10))
 1973   FORMAT(///)
   3   CONTINUE
      STOP
      END
*EXECUTE
*
*
