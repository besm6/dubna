      PROGRAM D510
       COMMON/G/G(100)          /A/A(100)/PL/PL(100)/SIGMA/SIGMA(100)
     //R/R(100)/DA/DA(100)/EXDA/EXDA(1500)/NED/KED(2)/DF/DF(100)
      COMMON/AU/AMX(100)/AL/AMN(100)
      A(1)=0.
      A(2)=8.
      A(3)=11.
      PL(1)=100.
      PL(2)=4.
      PL(3)=4.
      EXDA(1)=10.
      EXDA(2)=.02
      EXDA(3)=1.
      EXDA(4)=18.
      EXDA(5)=.025
      EXDA(6)=4.
      EXDA(7)=40.
      EXDA(8)=.02
      EXDA(9)=6.
      EXDA(10)=74.
      EXDA(11)=.125
      EXDA(12)=7.
      EXDA(13)=82.
      EXDA(14)=.0222
      EXDA(15)=8.
      EXDA(16)=136.
      EXDA(17)=.01
      EXDA(18)=9.
      EXDA(19)=168.
      EXDA(20)=.0083
      EXDA(21)=10.
      EXDA(22)=168.
      EXDA(23)=.0143
      EXDA(24)=11.
      EXDA(25)=160.
      EXDA(26)=.0083
      EXDA(27)=12.
      EXDA(28)=130.
      EXDA(29)=.0182
      EXDA(30)=13.
      EXDA(31)=114.
      EXDA(32)=.01
      EXDA(33)=13.5
      EXDA(34)=120.
      EXDA(35)=.0167
      EXDA(36)=14.
      EXDA(37)=81.
      EXDA(38)=.0222
      EXDA(39)=15.
      EXDA(40)=46.
      EXDA(41)=.02
      EXDA(42)=16.
      EXDA(43)=29.
      EXDA(44)=.0222
      EXDA(45)=17.
      EXDA(46)=2.
      EXDA(47)=.04
      EXDA(48)=18.
      EXDA(49)=.0
      EXDA(50)=.05
      EXDA(51)=20.
       KED(1)=17
       KED(2)=3
       DO 80 I=2,50,3
   80 EXDA(I)=1./SQRT (EXDA(I))
      AMX(1)=1200.
      AMN(2)=4.
      AMX(3)=11.04
       N1=1
       N2=1
       N3=100
       EPS=.1
      M=3
      IT=1
   11 CALL FUMILI(S,M,N1,N2,N3,EPS,AKAPPA,ALAMBD,IT,MC)
      CALL ERRORF(M)
      END
      FUNCTION FUNCT(X)
      DIMENSION X(10)
      COMMON/A/A(100)
      FUNCT=(A(1)/(SQRT(6.2832)*A(2)))*EXP(-((X-A(3))**2)/
     /(2.*(A(2)**2)))
      RETURN
      END
*CALL PTIME
*EXECUTE
*
*
