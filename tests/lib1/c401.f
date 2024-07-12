      PROGRAM C401
C
       DIMENSION Z(20,20),ZL(8,8),YR(100),XX(20)
       COMMON/KSEL/KPI,KPF,KGS,L/DROPX/DX(100)
       COMMON/NPN/NPN/OUTN/ITN,KPN,KPFN
       COMMON/ITS/ITS/MARQ/MARQ
       COMMON/ARPN/TN,A1N
C
C                  EXAMPLE  1
C
C             SOLVING OF THE SYSTEM
C                 X+Y**2=0   (1)
C                 X**2+Y=2   (2)
C   BY MEANS OF  A R P  WITH INITIAL APPROXIMATIONS
C                 X=0.5  AND  Y=-0.5
C
       NP=1
       M=2
       N=2
       NN=N
       K=4
       KK=1
       IT=30
       E=1.
       T=0.00001
       A1=1.
       A2=1.
       KPI=1
       KP=1
       YR(1)=2.
       YR(2)=0.
       XX(1)=-0.5
       XX(2)=-0.5
       PRINT 101
       CALL REGN
     *(NP,M,N,NN,K,KK,IT,KP,T,AD,S,TT,D,E,Z,ZL,YR,XX,A1,A2)
C
C                EXAMPLE  2
C
C      EXAMPLE OF  C.  L A N C Z O S
C            SOLVING BY REGN
C
       T=0.000001
       NP=2
       M=72
       N=8
       NN=8
       K=3
       KK=3
       E=10.
      YR(1)=2.5134
                     YR(2)=1.
                                YR(3)=0.0
      YR(4)=2.044333373
                          YR(5)=1.
                                     YR(6)=0.05
      YR(7)=1.668404436
                          YR(8)=1.
                                     YR(9)=0.1
      YR(10)=1.366418021
                           YR(11)=1.
                                       YR(12)=0.15
      YR(13)=1.123232487
                           YR(14)=1.
                                       YR(15)=0.2
      YR(16)=0.926889718
                           YR(17)=1.
                                       YR(18)=0.25
      YR(19)=0.767933856
                           YR(20)=1.
                                       YR(21)=0.3
      YR(22)=0.638877552
                           YR(23)=1.
                                       YR(24)=0.35
      YR(25)=0.533783531
                           YR(26)=1.
                                       YR(27)=0.4
      YR(28)=0.447936361
                           YR(29)=1.
                                       YR(30)=0.45
      YR(31)=0.377584788
                           YR(32)=1.
                                       YR(33)=0.5
      YR(34)=0.319739319
                           YR(35)=1.
                                       YR(36)=0.55
      YR(37)=0.272013077
                           YR(38)=1.
                                       YR(39)=0.6
      YR(40)=0.232496552
                           YR(41)=1.
                                       YR(42)=0.65
      YR(43)=0.199658954
                           YR(44)=1.
                                       YR(45)=0.7
      YR(46)=0.172270412
                           YR(47)=1.
                                       YR(48)=0.75
      YR(49)=0.149340566
                           YR(50)=1.
                                       YR(51)=0.8
      YR(52)=0.130070020
                           YR(53)=1.
                                       YR(54)=0.85
      YR(55)=0.113811932
                           YR(56)=1.
                                       YR(57)=0.9
      YR(58)=0.100041558
                           YR(59)=1.
                                       YR(60)=0.95
      YR(61)=0.088332090
                           YR(62)=1.
                                       YR(63)=1.
      YR(64)=0.078335440
                           YR(65)=1.
                                       YR(66)=1.05
      YR(67)=0.069766937
                           YR(68)=1.
                                       YR(69)=1.1
      YR(70)=0.062393125
                           YR(71)=1.
                                       YR(72)=1.15
       XX(1)=0.12
       XX(2)=1.1
       XX(3)=0.9
       XX(4)=0.6
       XX(5)=1.3
       XX(6)=2.8
       XX(7)=4.7
       XX(8)=4.7
       KP=0
       KGS=0
       KPF=1
       PRINT 102
       CALL REGN
     *(NP,M,N,NN,K,KK,IT,KP,T,AD,S,TT,D,E,Z,ZL,YR,XX,A1,A2)
       XX(4)=0.
       DX(4)=1.
       DX(8)=1.
       E=0.05
       KPF=3
       L=3
       CALL REGN
     *(NP,M,N,NN,K,KK,IT,KP,T,AD,S,TT,D,E,Z,ZL,YR,XX,A1,A2)
C
C                 EXAMPLE  3
C
C      EXAMPLE OF  C.  L A N C Z O S
C           SOLVING BY COMPIL
C
       XX(1)=0.12
       XX(2)=1.1
       XX(3)=0.9
       XX(4)=0.6
       XX(5)=1.3
       XX(6)=2.8
       XX(7)=4.7
       XX(8)=4.7
       DX(4)=0.
       DX(8)=0.
       NPN=2
       KPFN=2
       ITN=10
       A1N=-1.
       PRINT 103
       CALL COMPIL (M,N,D,EI,Z,ZL,XX,YR)
C
C
  101  FORMAT(///20X,22HSOLUTION OF EXAMPLE  1,/)
  102  FORMAT(///20X,22HSOLUTION OF EXAMPLE  2,/)
  103  FORMAT(///20X,22HSOLUTION OF EXAMPLE  3,/)
       END
       SUBROUTINE RELADI (N,NP,INDEX,A,Y,DF)
C
       DIMENSION A(N),DF(N)
       COMMON/X/X(10)
       GO TO (100,200),NP
  100  CONTINUE
       GO TO (1,2),INDEX
    1  Y=A(1)**2+A(2)
       DF(1)=2.*A(1)
       DF(2)=1.
       RETURN
    2  Y=A(1)+A(2)**2
       DF(1)=1.
       DF(2)=2.*A(2)
       RETURN
C
  200  CONTINUE
       Y=A(1)*EXP(-A(5)*X(1))+A(2)*EXP(-A(6)*X(1))+
     * A(3)*EXP(-A(7)*X(1))+A(4)*EXP(-A(8)*X(1))
       DF(1)=EXP(-A(5)*X(1))
       DF(2)=EXP(-A(6)*X(1))
       DF(3)=EXP(-A(7)*X(1))
       DF(4)=EXP(-A(8)*X(1))
       DF(5)=-A(1)*X(1)*DF(1)
       DF(6)=-A(2)*X(1)*DF(2)
       DF(7)=-A(3)*X(1)*DF(3)
       DF(8)=-A(4)*X(1)*DF(4)
       RETURN
       END
*EXECUTE
*
*
