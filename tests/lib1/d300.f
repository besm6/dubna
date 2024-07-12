      PROGRAM D300
C        CARDS  OF  DATAS  12
      CALL EPDE1
      STOP
      END
      SUBROUTINE GETCO (X,Y,HL,M,KODE,COFFT,JEQU)
C
C   CALCULATION OF DIFFERENCE EQUATION COEFFICIENTS FOR LAPLACES
C   EQUATION IN CYLINDRICAL CO-ORDINATES WITH AXIAL SYMMETRY (X=Z, Y=R)
C
      COMMON DUMMY,DX,DY
      DIMENSION HL(4),COFFT(5),DUMMY(7028)
C
C   INTERIOR POINTS HAVE CODE 100 , POINTS ON AXIS,CODE 101
C
      CLAM=1.0/(HL(1)*HL(3)*(HL(1)+HL(3))*DX**2)
      IF(KODE-100) 6,5,6
    5 CMU=1.0/(HL(2)*HL(4)*(HL(2)+HL(4))*DY**2)
      CKAY=(2.0+(HL(4)-HL(2))*DY/Y)/(HL(2)*HL(4)*DY**2)
     1+2.0/(HL(1)*HL(3)*DX**2)
      COFFT(1)=2.0*CLAM*HL(3)/CKAY
      COFFT(2)=CMU*HL(4)*(2.0+HL(4)*DY/Y)/CKAY
      COFFT(3)=2.0*CLAM*HL(1)/CKAY
      COFFT(4)=CMU*HL(2)*(2.0-HL(2)*DY/Y)/CKAY
    8 COFFT(5)=0.0
      RETURN
    6 IF(KODE-101) 10,7,10
    7 CKAY=2.0+((DY*HL(2)/DX)**2)/(HL(1)*HL(3))
      COFFT(1)=(DY*HL(2))**2*CLAM*HL(3)/CKAY
      COFFT(2)=2.0/CKAY
      COFFT(3)=(DY*HL(2))**2*CLAM*HL(1)/CKAY
      COFFT(4)=0.0
      GO TO 8
   10 PRINT 100,M,KODE
      STOP
  100 FORMAT (7H0 POINT, I5,26H HAS UNDEFINED CODE NUMBER, I5)
      END
      SUBROUTINE USER1
      RETURN
      END
      SUBROUTINE USER2
      STOP
      END
*EXECUTE
     2.0            2.0                          1 1234
                                               100
                                                 8
     0.0            0.0           30.0         101
    25.0           55.0           30.0
    30.0           55.0           60.0
    40.0           30.0           60.0
    80.0           30.0           60.0
    95.0           55.0           60.0
   100.0           55.0           80.0
   100.0            0.0           80.0         101
     0.0            0.5            0.0
*
*
