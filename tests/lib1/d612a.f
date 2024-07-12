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
*CALL FICMEMORY
*EXECUTE
*
*
