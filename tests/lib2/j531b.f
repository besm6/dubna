      PROGRAM J531 B
C-----ЭTA ПPOГPAMMA COДEPЖИT ПPИMEP ИCПOЛЬЗOBAHИЯ COPLOT ДЛЯ ПOCTPOEHИЯ
C-----ГPAФИKOB БEЗ ПPEДBAPИTEЛЬHOГO HAKOПЛEHИЯ B ПAMЯTИ TAБЛИЦ ЗHAЧEHИЙ
C-----ФYHKЦИЙ
      DIMENSION YMIN(4),YMAX(4)
      DIMENSION TITLES(8)
      DATA(YMIN  =0.1, 0.1E-9, 0.2E7, -0.6E-3),
     *    (YMAX  =1.3, 1.3E-9, 2.0E7,  0.6E-3),
     *    (TITLES=12H*****ALPHA
     *           ,12H+++++BETA
     *           ,12H-----GAMMA
     *           ,12H<<<<<DELTA  ),
     *    (SCALE =7400 0000 0000 0000 B)
      T=0.1
      STEP=0.02
1     AR=3.14158226525*T
      ALPHA=    0.8*SIN(AR)+0.7
      BETA =0.4 E-9*SIN(AR)+0.7E-9
      GAMMA=0.24E7 *SIN(AR)+1.4E7
      DELTA=0.12E-3*SIN(AR)*SIN(AR*10.)
C-----ЗДECЬ ПPИ OДHOM OБPAЩEHИИ K COPLOT ГPAФИK ПPOДBИГAETCЯ HA OДHO
C-----ДEЛEHИE OCИ X
      CALL COPLOT(ALPHA,BETA,GAMMA,DELTA,4,TITLES,YMIN,YMAX,SCALE,T,
     *STEP,1)
      SCALE=0.
C-----SCALE ПPИCBAИBAETCЯ HYЛEBOE ЗHAЧEHИE,ЧTOБЫ ЗAПPETИTЬ ПOBTOPHYЮ
C-----BЫДAЧY OCEЙ OPДИHAT
      T=T+STEP
      IF(T-1.96)1,1,2
2     CONTINUE
      END
*EXECUTE
*
*
