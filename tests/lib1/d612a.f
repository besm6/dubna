      PROGRAM D612 A
C PEШAETCЯ CИCTEMA  N ЛИHEЙHЫX AЛГEБP. YPABHEHИЙ C  M  HEИЗBECTHЫMИ.
C MACCИB  XT  COДEPЖИT TOЧHOE PEШEHИE. MATPИЦA CИCTEMЫ  A  ЗAДAETCЯ
C B ЦИKЛE DO 2 . ПPABAЯ ЧACTЬ  Y  B BИДE MACCИBA B ЦИKЛE  DO 3 .
C
      DIMENSION A(41,41),Y(41),X(41),RAB(260,41),XT(41)
      EXTERNAL FA,FY
      XF(T)=1.-T*T
      N=41
      M=41
      DO 1 I=1,M
1     XT(I)=XF((I-0.5)/M)
      CALL GRAFIK(M,XT,XT)
      DO 2 I=1,M
      XX=(I-0.5)/M
      DO 2 J=1,N
      YY=4./N*(J-0.5)-2.
 2    A(J,I)=AQ(YY,XX)/M
      DO 3 I=1,N
      Y(I)=0.
      DO 3 J=1,M
 3    Y(I)=Y(I)+A(I,J)*XT(J)
       CALL GRAFIK(N,Y,Y )
      CALL ROBAST(N,M,260*41,A,Y,X,RAB,4,1,0.,0.,0.,0.,0.02,1,1,FA,FY)
      CALL GRAFIK(M,XT,X)
      DO 5 I=1,M
 5    PRINT 6,XT(I),X(I)
 6    FORMAT(10X,E12.5,6X,E12.5)
      STOP
      END
      FUNCTION FA(Y,X)
1     FA=0.
      RETURN
      END
      FUNCTION FY(T)
      FY=0.
      RETURN
      END
      FUNCTION AQ(Y,X)
      AQ=1./(1.+100./(X-Y)**2)
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
*CALL FICMEMORY
*EXECUTE
*
*
