      PROGRAM D612
C  PEШEHИE ИHTEГPAЛЬHOГO YPABHEHИЯ 1-ГO POДA
C  ЯДPO ЗAДAETCЯ ПOД.-ФYHKЦИEЙ FA .ПPABAЯ ЧACTЬ B TOЧKAX (ЦИKЛ 3),
C  ПOЭTOMY ПAPAMETP IVEC=1  . C.K.O. PABHA 0.02  .XT - TOЧHOE PEШEHИE
      DIMENSION A(15,15),Y(15),X(15),RAB(3400),XT(15),YT(15)
      EXTERNAL FA,FY
      XF(T)=4.-T*T
      YF(T)=4.*T-T*T*T/3.
      X1=0.
      X2=2.
      Y1=0.
      Y2=2.
      N=15
      M=15
      DO 1 I=1,M
1     XT(I)=XF(X1+(I-0.5)*(X2-X1)/M)
      DO 2 I=1,M
 2    YT(I)=YF(Y1+(I-0.5)*(Y2-Y1)/N)
      DO 3 I=1,N
 3    Y(I)=YT(I)+0.02*SIN(50.*I)
      Y(5)=15.
      CALL GRAFIK(M,XT,XT)
       CALL GRAFIK(N,Y,YT )
      CALL ROBAST(N,M,3400,A,Y,X,RAB,4,0,X1,X2,Y1,Y2,0.02,1,1,FA,FY)
      CALL GRAFIK(M,XT,X)
      CALL EXIT
      END
      FUNCTION FA(Y,X)
      IF(Y-X   ) 1,2,2
1     FA=0.
      RETURN
2     FA=1.
      RETURN
      END
      FUNCTION FY(T)
      FY=4.*T-T*T*T/3.
      IF(T.LE.(0.1)) FY=14.3
      RETURN
      END

      SUBROUTINE GRAFIK(N,Y1,Y2)
C     IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION Y1(N),Y2(N),IB(19,60)
      DATA ISTAR/1H*/,IEQ/1H=/,IBLAN/1H  /,IPLU/1H+/,IUND/1H_/
     *,IPOI/1H./
      YMIN=Y1(1)
      YMAX=Y2(1)
      DO 1 I=1,N
      YMIN=AMIN1(YMIN,Y1(I))
      YMIN=AMIN1(YMIN,Y2(I))
      YMAX=AMAX1(YMAX,Y1(I))
      YMAX=AMAX1(YMAX,Y2(I))
1     CONTINUE
      DELY=YMAX-YMIN
      IF(DELY.LE.(1.E-15)) DELY=0.0001
      NH=19
      NG=60
      NH1=NH+1
      NH0=NH-1
      ND=NG/N
      NUP=ND*N
      DO 11 I=1,NH0
      DO 11 J=1,NG
11    IB(I,J)=IBLAN
      DO 111 J=1,NG
111   IB(NH,J)=IUND
      DO 112 I=5,NH0,5
      DO 112 J=1,NUP
112   IB(NH1-I,J)=IPOI
      DO 113 J=5,NUP,5
      DO 113 I=2,NH
113   IB(NH1-I,J)=IPOI
      DY=DELY/NH
      PRINT 901,NH,NG,DY
901   FORMAT(/13X,'      ГPAФИK : ',I2,'*',I2,'  ЦEHA _ OPДИHATЫ=',
     * E12.5/12X,62('_'))
      IGO=0
      DO 12 I=ND,NUP,ND
      IGO=IGO+1
      NY1=        (Y1(IGO)-YMIN)/DY
      NY1=MAX0(NY1,1)
      NY2=(Y2(IGO)-YMIN)/DY
      NY2=MAX0(NY2,1)
      IF(NY1-NY2) 3,5,3
3     IB(NH1-NY1,I  )=ISTAR
      IB(NH1-NY2,I  )=IEQ
      GO TO 4
5     IB(NH1-NY1,I  )=IPLU
4     CONTINUE
12    CONTINUE
      DO 7 I=1,NH
      NHEI=NH1-I
      YCUR=YMIN+    NHEI*DY
      PRINT 10,YCUR,(IB(I,J),J=1,NG)
10    FORMAT(E12.4,1HI,60A1,1HI)
7     CONTINUE
       RETURN
      END
*EXECUTE
