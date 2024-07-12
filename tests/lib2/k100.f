      PROGRAM K100
C        *FILE:SCRATCH,F1,W,40
C        *FILE:SCRATCH,F2,W,40
C        *FILE:SCRATCH,F3,W,40
C        *FILE:SCRATCH,F8,W,40
           COMMON/IFLAG/IFL(4)
           DIMENSION MA(40),MAMA(2400)
        COMMON/NTEST/NTEST,NT1
           DO 77 I77=1,2
        PRINT 27$CALL PTIME
  27    FORMAT(10X,'***** HAЧAЛO CEPИИ TECTOB *****')
        NT1=I77
           NTEST=1
           DO 200 I=1,4
 200        IFL(I)=0
         IFL(4)=1
      N8=3
      N2048=3
C... ПEPBЫE 4 TECTA ПPOBEPЯЮT БЛOK ПOДKAЧKИ OБM.ЛИCTOB
           REWIND 16
           DO 100 I=1,2400
 100       MAMA(I)=5H*****
           MA(2)=1$WRITE(16)MAMA
       DO 1 I=1,N8$LUN=I$CALL TESTRD
  1   CALL FSWOP(LUN)
       DO 2 J=1,N2048$DO 3 I=1,N8$LUN=I$CALL TESTRD$N=I*J
  3        CALL BWRITE(LUN,N,1)
       DO 4 I=1,N8$LUN=I$CALL TESTRD
  4        CALL BACK(LUN,1)
       DO 5 I=1,N8$LUN=I$CALL TESTRD$M=0$CALL BREAD(LUN,M,1,LEN)
           IF(LEN.NE.1.OR.M.NE.J*LUN)GOTO 9999
  5        CONTINUE
  2        CONTINUE
            CALL ETEST
       DO 6 I=1,N8$LUN=I$N=300*I$CALL TESTRD
  6         CALL BACK(LUN,N)
       DO 7 I=1,N8$LUN=I$CALL TESTRD$CALL EWR(LUN,LEN)
        IF(LEN.NE.N2048)GOTO 9999
  7     CONTINUE
          CALL ETEST
           MA(2)=0
            IFL(4)=0
        DO 8 I=1,N8$LUN=I$CALL TESTRD$CALL INPOS(LUN,LUN-1)
        CALL BREAD(LUN,MA,2,LEN)
           PRINT 81,LUN,MA(1),LEN
  81      FORMAT(3I6)
  8     CONTINUE
           CALL ETEST
       DO 9 I=1,N8$LUN=I$CALL TESTRD
  9     CALL SWOP(LUN)
        CALL ETEST
        CALL SOWWT(5)
        CALL LOADGO(4HTRWS)
        CALL SOWWT(5)
        CALL LOADGO(4HTIBE)
C... TECT MEЛKИX ДEЯHИЙ
        IFL(4)=3$CALL SOWWT(4)
        IFL(1)=0
 601       CONTINUE
        IFL(4)=1
      CALL FSWOP(16)
        DO 600 I=1,48
        CALL BWRITE(16,MAMA,2400)
        IF(IFL(1).EQ.0)GOTO 600
        IFL(1)=0$CALL SOWWT(1)$GOTO 601
 600    CONTINUE
        PRINT 600,IFL(2),IFL(3)
 600    FORMAT(2I10)
        CALL ETEST
           CALL SWOP(16)
        CALL SOWWT(5)
  77       CALL SOWWT(0)
        DO 199 I=1,2400
 199    MAMA(I)=1H+
           CALL SOWWT(6)
           CALL SOWWT(66)
        IFL(4)=4
        CALL SOWWT(4)
        CALL SOWWT(1)
        CALL SOWWT(2)
        LUN=31
      CALL FSWOP(LUN)
        CALL BWRITE(LUN,MAMA,2400)
      DO 300 I=1,20
        CALL BWRITE(LUN,J,1)
        CALL INPOS (LUN,0)
             IFL(4)=1
        CALL BWRITE(LUN,J,1)
        CALL BACK  (LUN,  2)
        CALL BREAD (LUN,MA,40,LEN)
        DO 301 II=2,40$IF(MA(II).NE.0B)GOTO 9999
 301    CONTINUE
        MA(11)=12345
        IF(LEN.NE.2400.OR.MA(1).NE.J)GOTO 9999
        CALL EWR(LUN,LEN)
        IF(LEN.NE.J+1)X=1./0.
 300    CONTINUE
        CALL INPOS (LUN,1)
        DO 302 I=1,2$IFL(4)=I-1
 302    CALL BWRITE(LUN,MAMA,10)
        CALL BACK  (LUN,     1 )
        CALL BREAD (LUN,MA,100,LEN)
        IF(LEN.NE.1.OR.MA(1).NE.1H+)GOTO 9999
        CALL ETEST
            STOP
 9999       CALL ERTEST
       END
            SUBROUTINE TRWS
             COMMON/IFLAG/IFL(4)/NTEST/NT,NT1
          DIMENSION AR(2000)
          DIMENSION A(7000)
          DO 10 I=1,2000
 10       AR(I)=1.*I
           IF(NT1.EQ.2)CALL SOWWT(0)
          DO 1 L=1,2$LUN=L
         IFL(1)=0
         IFL(4)=1
      CALL FSWOP(LUN)
          DO 2 I=1,512
  2       CALL BWRITE(LUN,AR(I),1)
         CALL SWOP  (LUN)
         CALL INPOS (LUN,0)
         DO 3 I=1,512$A(I)=0.$A(1)=0.$J=I
         CALL BREAD (LUN,A,J,LEN)
        IF(LEN.NE.1.OR.A(1).NE.1.*I)PRINT 1,LUN,LEN,I,A(1)
  3      CONTINUE
  1     FORMAT('  ***',3I5,F10.1)
          CALL BWRITE (LUN,AR,1020)
          CALL BWRITE (LUN,AR,2000)
          CALL BACK   (LUN,2)
          II=1
  8       CALL BREAD  (LUN,A,2000,LEN)
          DO 4 I=1,1020
        IF(A(I).NE.1.*I)GOTO 55
  4      CONTINUE
        GOTO 5
  55    PRINT 1,LUN,LEN,I,A(I)
  5        CALL BACK(LUN,3)
             IF(II.EQ.2)GOTO 1
           IFL(4)=1
          CALL BWRITE(LUN,3.62,1)
           IFL(4)=1
          CALL BWRITE(LUN,AR,2000)
          CALL SWOP  (LUN)
          CALL INPOS (LUN,511)
          CALL BREAD (LUN,A,5000,LEN)
            II=2$GOTO 8
  1         CONTINUE
            CALL ETEST
             RETURN
            ENTRY TIBE
            IF(NT1.EQ.2)CALL SOWWT(0)
             CALL INPOS(1,10000)
             IFL(1)=0
               CALL INPOS(1,20)
              CALL EWR(1,LEN)
               RETURN
            END
        SUBROUTINE TESTRD
           COMMON/NTEST/NTEST
         COMMON/IFLAG/IFL(4)
            DIMENSION MM(2400)
            DATA(NTEST=1),(NBEG=0),(NB1=0)
        IF(IFL(1).EQ.0)GOTO 1$PRINT 1$STOP
  1      FORMAT(2X,'----  AЙ-ЯЙ-ЯЙ, TOB. CAПOЖHИKOB  ')
  1      CONTINUE
       IF(NBEG.EQ.0)CALL PAHOM1$NBEG=NBEG+1$READ 101,NSTAR
       IF(NSTAR.NE.3H***)GOTO13$NB1=NB1+1$REWIND 16
          IF(NB1.NE.3)GOTO2$NB1=0$CALL PAHOM2
  2       READ(16),MM$DO 3 I=1,2400$IF(MM(I).NE.5H*****)GOTO13
  3      CONTINUE
          RETURN
           ENTRY ETEST
        CALL PTIME
           PRINT 2,NTEST$NTEST=NTEST+1$CALL SOWWT(3)$RETURN
           ENTRY ERTEST
           PRINT 3,NTEST$STOP
  13       PRINT 4,NTEST$STOP
 101    FORMAT(A3)
  2         FORMAT(2X,'TECT ',I2,'    ПPOШEЛ')
  3         FORMAT(2X,'TECT ',I2,' HE ПPOШEЛ')
  4         FORMAT(2X,'TECT ',I2,' - ПOPЧA OБM.ЛИCTA')
          END
*ASSEM
  PAHOM1:,NAME,
  10,VTM,
  ,UJ,PAHOM
  PAHOM2:,ENTRY,
  10,VTM,1
  RFMRTFLG:,LC,1
 KCOUNT*:,LC,4
  PAHOM:13,MTJ,11
            ,XTA,RFMRTFLG
           ,ATX,KCOUNT*+2
  ,ITA,10
  ,CALL,SAVE*
  11,UJ,
  ,END,
*EXECUTE
***
***
***
***
***
***
***
*
*
