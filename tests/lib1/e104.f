      PROGRAM E104
      DIMENSION ARG(2),NENT(2),ENT(30),A(3),TABLE1(225),TABLE2(225),
     1 TABLE3(225)
      DATA ENT/1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,
     1         1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15./,
     2       A/6HTABLE1,6HTABLE2,6HTABLE3/,NENT/15,15/
      NARG=2
      PRINT 10,ENT
      L=0
      DO 6 I=1,15
      DO 1 J=1,15
      X=ENT(I)
      Y=ENT(J+15)
      K=J+L
      TABLE1(K)=ALOG(X)+Y
      TABLE2(K)=SIN(180.*Y/3.14)+X
   1  TABLE3(K)=SQRT(X)+Y**2
      L=L+15
   6  CONTINUE
      PRINT 9,A(1),TABLE1
C
      ARG(1)=0.53
      ARG(2)=-2.21
      DO 2 N=1,20
      F=FINT(NARG,ARG,NENT,ENT,TABLE1)
      PRINT 4,ARG,F
      ARG(1)=ARG(1)+0.5
      ARG(2)=ARG(2)+1.0
   2  CONTINUE
      PRINT 9,A(2),TABLE2
      ARG(1)=0.53
      ARG(2)=-2.21
      DO 3 N=1,20
      F=FINT(NARG,ARG,NENT,ENT,TABLE2)
      PRINT 4,ARG,F
      ARG(1)=ARG(1)+0.5
      ARG(2)=ARG(2)+1.0
   3  CONTINUE
      PRINT 9,A(3),TABLE3
      ARG(1)=0.53
      ARG(2)=-2.21
      DO 5 N=1,20
      F=FINT(NARG,ARG,NENT,ENT,TABLE3)
      PRINT 4,ARG,F
      ARG(1)=ARG(1)+0.5
      ARG(2)=ARG(2)+1.0
   5  CONTINUE
      STOP
   4  FORMAT(2(20X,F5.2),20X,E20.11)
   9  FORMAT(/50X,A6/15(1X,15E8.1/)//21X,6HARG(1),20X,6HARG(2),23X,1HA/)
  10  FORMAT(///50X,9HTEST E104///50X,3HENT//2(30X,15F4.0/)//)
      END
*EXECUTE
*
