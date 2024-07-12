      PROGRAM G901
      REAL X(10),Y(10),Z(10),X1(3),Y1(3),Z1(3),R1(3)
      RADIUS=2.0
      DO 1 I=1,10
      CALL RAN2VS(RADIUS,X(I),Y(I))
  1   CONTINUE
      PRINT 2,X,Y
      RADIUS=0.5
      DO 3 I=1,10
      CALL RAN3VS(RADIUS,X(I),Y(I),Z(I))
  3   CONTINUE
      PRINT 4,X,Y,Z
      RADIUS=1.5
      CALL VRAN2S(RADIUS,3,X1,Y1,R1)
      PRINT 5,X1,Y1
      RADIUS=3.0
      CALL VRAN3S(RADIUS,2,X1,Y1,Z1,R1)
      PRINT 6,(X1(I),I=1,2), (Y1(K),K=1,2), (Z1(J),J=1,2)
 2    FORMAT(1H1//10X,'TEST OF G901(RAN2VS)',/5X,'X=',10(2X,F8.5),
     *      /5X,'Y=',10(2X,F8.5)//)
  4   FORMAT(//10X,'TEST OF G901(RAN3VS)',/5X,'X=',10(2X,F8.5),
     *      /5X,'Y=',10(2X,F8.5),/5X,'Z=',10(2X,F8.5)//)
  5   FORMAT(//10X,'TEST OF G901(VRAN2S)',/5X,'X=',3F12.5/
     *       5X,'Y=',3F12.5//)
  6   FORMAT(//10X,'TEST OF G901(VRAN3S)',/5X,'X=',2F12.5/
     *       5X,'Y=',2F12.5/5X,'Z=',2F12.5//)
      STOP
      END
*EXECUTE
*
*
