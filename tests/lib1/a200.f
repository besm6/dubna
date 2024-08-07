*
      PROGRAM A200
      DOUBLE PRECISION DREAL,DAIMAG,DCABS,Z(2),Y,X,F(2),Z1(2),Z2(2)
      Z(1)=2.52343183811242525822321D0
      Z(2)=3.125424320115743825743D0
      PRINT 1
      PRINT 2,Z
      Y=DREAL(Z)
      PRINT 3,Y
      Y=DAIMAG(Z)
      PRINT 4,Y
      Y=DCABS(Z)
      PRINT 5,Y
      X=1.123456789121326D0
      Y=0.223427862345861D0
      CALL DCMPLX(X,Y,Z)
      PRINT 6,X,Y,Z
      CALL DCONJG(Z)
      PRINT 7,Z
      Z1(1)=0.12345627112618326D0
      Z1(2)=3.23165472126181362D0
      Z2(1)=3.23165472126181362D0
      Z2(2)=0.12345627112618326D0
      PRINT 8,Z1,Z2
      CALL DCADD(Z1,Z2,Z)
      PRINT 9,Z
      CALL DCSUB(Z1,Z2,Z)
      PRINT 10,Z
      CALL DCMPY(Z1,Z2,Z)
      PRINT 11,Z
      CALL DCDIV(Z1,Z2,Z)
      PRINT 12,Z
      Z(1)=5.805D0
      Z(2)=2.735D0
      PRINT 2,Z
      CALL DCSQRT(Z,F)
      PRINT 13,F
      CALL DCEXP(Z,F)
      PRINT 14,F
      CALL DCLOG (Z,F)
      PRINT 15,F
      CALL DCSIN(Z,F)
      PRINT 16,F
      CALL DCCOS(Z,F)
      PRINT 17,F
  1   FORMAT(1H1,40X,'DOUBLE PRECISION COMPLEX ARITHMETIC',34X,'TEST A20
     10'/40X,40(1H*),30X,9(1H*)/40X,40(1H*)/)
  2   FORMAT(33X,'Z=('D24.17','D24.17')'/)
   3  FORMAT(10X,'DREAL='D24.17)
   4  FORMAT(41X,'DAIMAG='D24.17)
   5  FORMAT(73X,'DCABS=',D24.17/30X,60(1H*)/)
   6  FORMAT(50X,'DCMPLX(X,Y,Z)'/5X,'X=',D24.17,2X,'Y='D24.17,2X,'Z=('D2
     14.17,',',D24.17,')'/30X,60(1H*))
   7  FORMAT(/50X,'DCONJG(Z)'/40X,'Z=(',D24.17,',',D24.17,')'/30X,60(1H*
     1)/)
  8   FORMAT(33X,'Z1=('D24.17','D24.17')'/33X,'Z2=('D24.17','D24.17')'/)
  9   FORMAT(10X,'DCADD(Z1,Z2,Z)',9X,'Z=('D24.17','D24.17')'/)
  10  FORMAT(10X,'DCSUB(Z1,Z2,Z)',9X,'Z=('D24.17','D24.17')'/)
  11  FORMAT(10X,'DCMPY(Z1,Z2,Z)',9X,'Z=('D24.17','D24.17')'/)
 12   FORMAT(' M =',I3/' N =',I3/' NC=',I3)
     1/)
  13  FORMAT(10X,'DCSQRT(Z,F)',12X,'F=('D24.17','D24.17')'/)
  14  FORMAT(10X,'DCEXPT(Z,F)',12X,'F=('D24.17','D24.17')'/)
  15  FORMAT(10X,'DCLOGT(Z,F)',12X,'F=('D24.17','D24.17')'/)
  16  FORMAT(10X,'DCSINT(Z,F)',12X,'F=('D24.17','D24.17')'/)
  17  FORMAT(10X,'DCCOST(Z,F)',12X,'F=('D24.17','D24.17')'/30X,60(1H*)/)
      END
*EXECUTE
*
*
