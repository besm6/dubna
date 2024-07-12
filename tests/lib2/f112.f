      PROGRAM F112
      DIMENSION WS(6),WT(6),WS1(3),WS2(6)
      DIMENSION R(6),S(6),A1(4),A(9),B(9),A2(9)
C
C     ИCXOДHAЯ TPEYГOЛЬHAЯ MATPИЦA
C
      DATA WT/1.,0.5,0.5,1.,1.,2./
       DATA WS/2.,1.,1.,1.,1.,2./
       DATA WS1/2.,1.,1./
      DATA A/1.,3.,5.,2.,4.,6.,3.,5.,7./
       DATA A1/1.,3.,5.,2./
       DATA WS2/2.,1.,1.,1.,1.,2./
      DATA A2/1.,3.,5.,2.,4.,6.,3.,5.,7./
       M=3
       PRINT 31
 31     FORMAT(//5X,11HTEST TRSMUL,//)
      CALL TRSMUL(WT,S,3)
      CALL PR(S,M)
      PRINT 32
 32     FORMAT(//5X,11HTEST TRSMLU,//)
       CALL TRSMLU (WT,R,3)
       CALL PR(R,M)
C---------------------------------------
       PRINT 61
 61     FORMAT(//5X,10HTEST TRINV,//)
       CALL TRINV(WT,R,M)
       CALL PR(R,M)
C
C      ИCXOДHAЯ CИMMETPИЧHAЯ MATPИЦA
C
       PRINT 98
 98     FORMAT(//5X,11HTEST TRCHUL,//)
       CALL TRCHUL(S,WS,M)
       CALL PR(WS,M)
C
       PRINT 99
       CALL TRCHLU(S,WS,M)
       CALL PR(WS,M)
C
 99     FORMAT(//5X,11HTEST TRCHLU,//)
C------------------------------------------
       PRINT 7
 7       FORMAT(//5X,11HTEST TRSINV,//)
       CALL TRSINV(WS1,R,2)
       CALL PR(R,2)
C------------------------------------------
C     ИCXOДHAЯ PACПAKOBAHHAЯ MATPИЦA
C
       CALL TRLA(WT,A,B,M,M)
       PRINT 10,B
 10     FORMAT(//5X,9HTEST TRLA,//(5X,3E11.3)/)
C
       CALL TRLTA(WT,A,B,M,M)
       PRINT 11,B
 11     FORMAT(//5X,10HTEST TRLTA,//(5X,3E11.3)/)
C
        CALL TRAL(A,WT,B,M,M)
       PRINT 12,B
 12     FORMAT(//5X,9HTEST TRAL,//(5X,3E11.3)/)
C
       CALL TRALT(A,WT,B,M,M)
       PRINT 13,B
 13     FORMAT(//5X,10HTEST TRALT,//(5X,3E11.3)/)
C-----------------------------------
       M=2
       CALL TRSA(WS1,A1,B,M,M)
       PRINT 14,(B(I),I=1,4)
 14     FORMAT(//5X,9HTEST TRSA,//(5X,2E11.3)/)
C
       CALL TRAS(A1,WS1,B,M,M)
       PRINT 15,(B(I),I=1,4)
 15     FORMAT(//5X,9HTEST TRAS,//(5X,2E11.3)/)
C
       CALL TRSAT(WS1,A1,B,M,M)
       PRINT 16,(B(I),I=1,4)
 16     FORMAT(//5X,10HTEST TRSAT,//(5X,2E11.3)/)
C
       CALL TRATS(A1,WS1,B,M,M)
       PRINT 17,(B(I),I=1,4)
 17     FORMAT(//5X,10HTEST TRATS,//(5X,2E11.3)/)
C
       PRINT 93
 93     FORMAT(//5X,11HTEST TRASAT,//)
       CALL TRASAT(A1,WS1,R,M,M)
       CALL PR(R,M)
C
       PRINT 94
 94     FORMAT(//5X,11HTEST TRATSA,//)
       CALL TRATSA(A1,WS1,R,M,M)
       CALL PR(R,M)
C
       PRINT 95
 95     FORMAT(//5X,10HTEST TRQSQ,//)
       CALL TRQSQ(WS1,WS1,R,M)
       CALL PR(R,M)
C----------------------------------------
       M=3
       PRINT 91
 91     FORMAT(//5X,10HTEST TRAAT,//)
       CALL TRAAT(A2,WS2,M,M)
       CALL PR(WS2,M)
C
       PRINT 92
 92     FORMAT(//5X,10HTEST TRATA,//)
       CALL TRATA(A2,WS2,M,M)
       CALL PR(WS2,M)
C---------------------------------------
       PRINT 96
 96     FORMAT(//5X,10HTEST TRPCK,//)
       CALL TRPCK(A2,S,M)
       CALL PR(S,M)
C
       PRINT 97
 97     FORMAT(//5X,11HTEST TRUPCK,//)
       CALL TRUPCK(S,A2,M)
       PRINT3,A2
 3     FORMAT(5X,3E11.3//)
C
       STOP
       END
       SUBROUTINE PR(A,M)
       DIMENSION A(10)
       I1=1
       I2=1
       DO 1 K=1,M
       PRINT 2,(A(I),I=I1,I2)
       I1=2*I1
       I2=I2+K+1
 1     CONTINUE
 2      FORMAT(5X,3E11.3//)
       RETURN
       END
*EXECUTE
*                                                        TESTF112
*
