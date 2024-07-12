      PROGRAM F106 B
      DIMENSION A(210)
      DIMENSION B(20,20),C(20,20),D(20,20)
      DATA A/20*1.,19*2.,18*3.,17*4.,16*5.,15*6.,14*7.,13*8.,
     (12*9.,11*10.,10*11.,9*12.,8*13.,7*14.,6*15.,5*16.,4*17.,
     U3*18.,2*19.,20./
      N=20
      KOKI=20
      DO 111 IIR=1,KOKI
      K=1
      DO 1013 J=1,N
      DO 1013 I=J,N
      B(I,J)=A(K)
      B(J,I)=A(K)
      K=K+1
1013  CONTINUE
      CALL SPXINV(A,20,R)
      K=1
      DO 1113 J=1,N
      DO 1113I=J,N
      D(I,J)=A(K)
      D(J,I)=A(K)
      K=K+1
1113  CONTINUE
      DO 1302 I=1,N
      DO 1302 K=1,N
      S=0.
      DO 1303 J=1,N
      S=S+D(I,J)*B(J,K)
1303  C(I,K)=S
1302  CONTINUE
 111   CONTINUE
      PRINT 4303,C
4303  FORMAT (///3X,11HMATRIX MULT//1(10E10.2))
 10   FORMAT(//3X,6HMATRIX//10F10.2)
 11   FORMAT(///3X,14HMATRIX INVERSE//10F10.2)
      END
*EXECUTE
*
*
