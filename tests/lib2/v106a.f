      PROGRAM V106 A
C  TECT ДЛЯ ПPИMEPA B OПИCAHИИ
      DIMENSION A(10)
C        ГEHEPAЦИЯ 10 CЛYЧAЙHЫX ЧИCEЛ
      DO 1 I=1,10
   1  A(I)=RN32(I)
      PRINT 2,A
C  ГEHEPAЦИЯ CЛYЧAЙHЫX ЧИCEЛ C HAЧAЛЬHЫM  CПEЦИAЛЬHЫM  ЧИCЛOM
      CALL RN32IN(65539)
      DO 3 I=1,5
   3  A(I)=RN32(I)
      PRINT 4,(A(K),K=1,5)
C   ЗAЛOMHИTЬ    ПEЦИAЛHOE   ЧИCЛO ПOCЛE 5 OБPAЩEHИЙ
      CALL RN32OT(IX)
      PRINT 5,IX
C  ПPOДOЛЖИTЬ  ГEHEPAЦИЮ 10 CЛYЧAЙHЫX ЧИCEЛ
      DO 6 I=1,5
   6  A(I)=RN32(I)
      PRINT 4,(A(K),K=1,5)
C   BOЗOБHOBИTЬ BЫЧИCЛEHИE, HAЧИHAЯ C 5-ГO  CЛYЧAЙHOГO  ЧИCЛA
      CALL RN32IN(IX)
      DO 7 I=1,5
   7  A(I)=RN32(I)
      PRINT 4,(A(K),K=1,5)
   2  FORMAT(1X,10F12.9)
   4  FORMAT(10X,5F12.9)
  5   FORMAT(5X,3HIX=,I12)
      STOP
      END
*EXECUTE
*
*
