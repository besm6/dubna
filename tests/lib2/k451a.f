      PROGRAM K451A
C1       YПPABЛЯЮЩИE KAPTЫ
C2       *ASSIGN FTAPE 01 W(1)
C3       *CHECK(1) 000 NO CHECK
C4       *MAIN BUFMT9
      COMMON/BUFMT9/IB(1024)
      COMMON /ECTAPE/IW(20)
      DIMENSION MA(10),MER(10),MC(10)
      DATA(READ=1),(WRITE=2),(FORWD=3),(REVERS=4)
     *(FTOMF=5),(RTOMF=6),(REWIND=7),(RWDALL=8),(WRTMF=9)
      NT=1
      IPLOTN=1
      CALL DNSMT9(IPLOTN)
      NN=0
      CALL MT9EC(NT,REWIND)
      PRINT 6,IW(4)
    6 FORMAT(10X,'ПPOBEPKA MФ 'O2)
    8 NN=NN+1
      CALL MT9EC(NT,REWIND)
      CALL MT9EC(NT,WRTMF)
      NZON=8
      K=1
      DO 1 I=1,NZON
      DO 2 J=1,256
      IB(J)=K
    2 K=K+1
      CALL MT9EC(NT,WRITE)
    1 CONTINUE
      CALL MT9EC(NT,WRTMF)
      CALL MT9EC(NT,REVERS)
      CALL MT9EC(NT,RTOMF)
      CALL MT9EC(NT,FORWD)
      K=1
      DO 3 NZ=1,NZON
      DO 4 J=1,256
    4 IB(J)=1
      CALL MT9EC(NT,READ)
      NSL=IW(10)
      N=0
      DO 5 J=1,256
      IF(IB(J).EQ.K) GO TO 5
      IF(N.LT.6) N=N+1
      MA(N)=J $ MER(N)=IB(J) $ MC(N)=K
    5 K=K+1
      IF(N.EQ.0) GO TO 3
      PRINT 1,NN,NZ,NSL
    1 FORMAT(//'ЦИKЛ',I5,'.ЗOHA',I4,'.ПPOЧИTAЛИ',I4,
     *'  CЛOB.',20X,'OШИБKИ:')
      PRINT 2,(MA(J),J=1,N)
    2 FORMAT(' AДPECA:',10(8X,O3,7X))
      PRINT 3,(MC(J),J=1,N)
    3 FORMAT(' ПИCAЛИ:',10O18)
      PRINT 4,(MER(J),J=1,N)
    4 FORMAT(' ЧИTAEM:',10O18)
    3 CONTINUE
      CALL MT9EC(NT,FTOMF)
      NP=IFPULT(1)
      IF(NP.EQ.0) GO TO 8
      CALL MT9EC(NT,WRTMF)
      CALL MT9EC(NT,REWIND)
      PRINT 5,NN
    5 FORMAT(' KOHEЦ ЦИKЛA',I6)
      END
*MAIN BUFMT9
*EXECUTE
*
*
