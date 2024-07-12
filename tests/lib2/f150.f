      PROGRAM F150
      DIMENSION A(4,4),B(4,4),C(16,16),D(16,16)
      PRINT 102
  10  READ 100,IA,KA,JB,LB
      IF(IA.EQ. 0) STOP
      PRINT 101,IA,KA,JB,LB
      DO 1 I = 1,IA
      DO 1 K = 1,KA
  1   A(I,K)=100*(10*I+K)
      DO 2 J = 1,JB
      DO 2 L = 1,LB
  2   B(J,L)=10*J+L
      IAJB=IA*JB
      KALB=KA*LB
      DO 3 I = 1,IA
   3  PRINT 103, (A(I,K),K=1,KA)
      PRINT 103
      DO 4 J = 1,JB
  4   PRINT 103, (B(J,L),L=1,LB)
      CALL MXDISM(A,B,D,4,4,16,IA,KA,JB,LB)
      PRINT 103
      DO 5 I = 1,IAJB
  5   PRINT 103, (D(I,K),K=1,KALB)
      CALL MXDIPR(A,B,C,4,4,16,IA,KA,JB,LB)
      PRINT 103
      DO 6 I = 1,IAJB
  6   PRINT 103, (C(I,K),K=1,KALB)
      GO TO 10
  100 FORMAT(4I5)
  101 FORMAT(///7H  IA = ,I3,7H  KA = ,I3,7H  JB = ,I3,7H  LB = ,I3/)
  102 FORMAT(1H1)
  103 FORMAT(1X16F8.0)
      END
      SUBROUTINE MXDISM(A,B,C,IAD,JBD,IJD,IA,KA,JB,LB)
      DIMENSION A(IAD,1),B(JBD,1),C(IJD,1)
      IAJB=IA*JB
      KALB=KA*LB
      IF(IAJB*KALB .EQ. 0) RETURN
      JB1=JB-1
      LB1=LB-1
      DO 1 I = 1,IAJB
      IIA=(I+JB1)/JB
      IIB=MOD(I-1,JB)+1
      DO 1 K = 1,KALB
      KKA=(K+LB1)/LB
      KKB=MOD(K-1,LB)+1
  1   C(I,K)=A(IIA,KKA)+B(IIB,KKB)
      RETURN
      END
*EXECUTE
   2   2   2   2
   3   3   2   2
   3   2   2   3
   2   1   2   2
   4   3   4   2
   4   4   4   4
   4   1   2   1
   3   4   1   4
   1   2   3   4
   4   3   2   1
   1   1   1   1
   0
*
*
