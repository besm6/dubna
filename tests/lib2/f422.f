      PROGRAM F422
      DIMENSION S(99,4),B(6),Y(101),C(100,3)
      DATA (S=99(1.),99(0.),99(1.),99(0.)),
     *(B=2(1.,0.,0.)),(H=0.01),
     *(N=101),(L=100),(M=99)
      E=SQRT(2.)*100.$B(6)=-E*H*COS(E)
      PRINT 5,E
      CALL TSYS(S,B,Y,C,M,N,L)
      PRINT 3,(C(I,1),I=1,100)
      PRINT 4
      PRINT 3,Y
      PRINT 4
      DO 2 K=90,100,10
      E=K$Z=H*E$B(6)=-Z*COS(E)
      Z=2.-Z*Z$B(5)=Z*0.5
      DO 1 I=1,99
    1 S(I,2)=Z
      PRINT 5,E
      CALL TSYS(S,B,Y,C,M,N,L)
      PRINT 3,(C(I,1),I=1,100)
      PRINT 4
      PRINT 3,Y
      PRINT 4
    2 CONTINUE
      B(3)=B(1)-B(2)$B(6)=B(4)-B(5)
      DO 6 I=1,99
    6 S(I,4)=S(I,2)-(S(I,1)+S(I,3))
      CALL TSYS(S,B,Y,C,M,N,L)
      PRINT 3,Y
      PRINT 4
    3 FORMAT(10F12.7)
    4 FORMAT(//)
    5 FORMAT(2HK=,F10.3//)
      END
*EXECUTE
*
*
