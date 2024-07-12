      PROGRAM J531 A
C-----ЭTA ПPOГPAMMA COДEPЖИT ПPИMEP OБPAЩEHИЯ K COPLOT ДЛЯ ПOCTPOEHИЯ
C-----ГPAФИKOB ПOCЛE HAKOПЛEHИЯ B ПAMЯTИ TAБЛИЦ ЗHAЧEHИЙ ФYHKЦИЙ
      DIMENSION ALPHA(100),BETA(100),GAMMA(100),DELTA(100)
      DIMENSION YMIN(4),YMAX(4)
      DIMENSION TITLES(8)
      DATA(YMIN  = 11., 0.09E-9, 0.2E7, -0.605E-3),
     *    (YMAX  =135., 1.27E-9, 2.1E7,  0.7E-3),
     *    (TITLES=12H*****ALPHA
     *           ,12H+++++BETA
     *           ,12H-----GAMMA
     *           ,12H<<<<<DELTA  ),
     *    (SCALE =7400 0000 0000 0000 B)
C-----ПOЛYЧEHИE TAБЛИЦ ЗHAЧEHИЙ ФYHKЦИЙ: ALPHA,BETA,GAMMA,DELTA ПPИ
C-----ИЗMEHEHИИ APГYMEHTA T OT 0.1 ДO 1.94 C ШAГOM 0.02
      TFIRST=0.1
      STEP=0.02
      T=TFIRST
      K=1
1     AR=3.1415826525*T
      ALPHA(K)=    80.*SIN(AR)+70.
      BETA(K) = 0.4E-9*SIN(AR)+0.7E-9
      GAMMA(K)= 0.24E7*SIN(AR)+1.4E7
      DELTA(K)=0.12E-3*SIN(AR)*SIN(10.*AR)
      T=T+STEP
      K=K+1
      IF(T-1.94)1,1,2
C-----ПOCTPOEHИE ГPAФИKOB ФYHKЦИЙ: ALPHA,BETA,GAMMA,DELTA
2     CALL COPLOT(ALPHA,BETA,GAMMA,DELTA,4,TITLES,YMIN,YMAX,SCALE,TFIRST
     *,STEP,K-1)
      END
*EXECUTE
*
*
