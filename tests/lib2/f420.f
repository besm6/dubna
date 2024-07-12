       PROGRAM F420
      DIMENSION A(10),B(10),A1(10),B1(10),C(10,10),D(10),E(10)
      DATA((B(I),I=1,10)=10(2.)),((A(I),I=1,10)=9(-1.),0.),
     1((C(1,I),I=1,10)=2(1.),-1.,2(10.),-10.,1.1,1.11,2(1.111))
      N=10
      N1=N-1
      M=10
      A1(1)=A(1)
      B1(1)=B(1)
      DO 1 I=2,N
      I1=I-1
      A1(I)=A(I)
      B1(I)=B(I)
      C(I,1)=C(I1,1)+1
      C(I,2)=C(I,1)
      C(I,3)=C(I,1)
      C(I,4)=C(I1,4)-1
      C(I,5)=C(I,4)
       C(I,6)=C(I,4)
      C(I,7)=C(I1,7)+1.1
      C(I,8)=C(I1,8)+1.11
      C(I,9)=C(I1,9)+1.111
    1 C(I,10)=C(I,9)
      DO 2 I=2,N,2
      C(I,2)=-C(I,2)
      C(I,5)=-C(I,5)
    2 C(I,10)=-C(I,10)
      DO 3 I=1,N1,2
      C(I,3)=-C(I,3)
    3 C(I,6)=-C(I,6)
      PRINT 33
      PRINT 35,((C(I,J),J=1,M),I=1,N)
      TAS=0.
      TAK=0.
      DO 49 IJK=1,10
      SAT=0.
      SAK=0.
      DO 48 I=1,N
      SAT=SAT+TAS
      SAK=SAK+TAK
      A1(I)=A1(I)+SAT
      B1(I)=B1(I)+SAK
      A(I)=A1(I)
   48 B(I)=B1(I)
      PRINT 30
      PRINT 22,(A(I),I=1,N1)
      PRINT 32
      PRINT 22,(B(I),I=1,N)
      CALL PATVIL(N,M,1.,DET,A,B,C,D)
      PRINT 21
      PRINT 22,(B(I),I=1,N)
      PRINT 23
      PRINT 22,(A(I),I=1,N)
      PRINT 24
      PRINT 25,DET
      PRINT 26
      DO 52 I=1,N
      DO 50 J=1,N
      IF(J.GE.I) GO TO 51
      E(J)=A(I)*B(J)
      GO TO 50
   51 E(J)=B(I)*A(J)
   50 CONTINUE
   52 PRINT 35,(E(J),J=1,N)
      PRINT 27
      PRINT 35,((C(I,J),J=1,M),I=1,N)
      PRINT 36
      DO 56 I=1,N
      DO 54 J=1,N
      IF(J.GE.I) GO TO 53
      E(J)=A(I)*B(J)
      GO TO 54
   53 E(J)=B(I)*A(J)
   54 CONTINUE
      D(1)=E(1)*B1(1)+E(2)*A1(1)
      DO 55 J=2,N1
   55 D(J)=E(J-1)*A1(J-1)+E(J)*B1(J)+E(J+1)*A1(J)
      D(N)=E(N1)*A1(N1)+E(N)*B1(N)
   56 PRINT 35,(D(J),J=1,N)
      DO 45 I=1,M
      D(1)=B1(1)*C(1,I)+A1(1)*C(2,I)
      DO 46 J=2,N1
   46 D(J)=A1(J-1)*C(J-1,I)+B1(J)*C(J,I)+A1(J)*C(J+1,I)
      D(N)=A1(N1)*C(N1,I)+B1(N)*C(N,I)
      DO 47 J=1,N
   47 C(J,I)=D(J)
   45 CONTINUE
      PRINT 28
      PRINT 35,((C(I,J),J=1,M),I=1,N)
      TAS=0.003
   49 TAK=0.005
   21 FORMAT(//46X,26HTHE COMPONENTS OF VECTOR V/)
   22 FORMAT(10(X,E11.4)//)
   23 FORMAT(46X,26HTHE COMPONENTS OF VECTOR W/)
   24 FORMAT(40X,34HDETERMINANT OF TRI DIAGONAL MATRIX/)
   25 FORMAT(50X,E15.8//)
   26 FORMAT(50X,16HMATRIX INVERSION/)
   27 FORMAT(//20X,79HTHE SOLUTIONS OF A LINEAR ALGEBRAIC SYSTEM WITH TR
     1I DIAGONAL COEFFICIENT MATRIX/)
   28 FORMAT(//55X,8H(AB)*X=C/)
   30 FORMAT(49X,23HVECTOR OF COEFFICIENT A/)
   31 FORMAT(2X,10(7X,F4.1)//)
   32 FORMAT(48X,24HVECTOR OF COEFFICIENTS B/)
   33 FORMAT(43X,34HMATRIX OF COEFFICIENTS RIGHT PARTS/)
   35 FORMAT(10(X,E11.4))
   36 FORMAT(//53X,13HE=(AB)*(AB)-1/)
      END
*EXECUTE
*
*
