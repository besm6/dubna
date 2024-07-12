      PROGRAM Q800
C   USER  MUST  REQUIRE  LIBRARY 1  AND  LIBRARY 2
C   USER  MUST  REQUIRE  TAPE 1  AND  TAPE 2
CARDS  OF  ANY  FORTRAN  PROGRAMS,SUBROUTINES OR  FUNCTIONS
C   FOR  EXAMPLE  KIM(F220)
      CALL FLODIA
      STOP
      END
*EXECUTE
      SUBROUTINE TRED2(N,D,E,Z,FAIL)
      DIMENSION D(N),E(N),Z(N,N)
      COMMON/PREC/TOL,EPS,AA,BB
      LOGICAL FAIL
      I=N
    1 L=I-2
      F=Z(I,I-1)
      G=0.
      DO2K=1,L
    2 G=G+Z(I,K)*Z(I,K)
      H=G+F*F
      IF(G-TOL)3,3,4
    3 E(I)=F
      H=0.
      GOTO 9
    4 L=L+1
      P=SQRT(H)
      IF(F.GE.0.)P=-P
      G=P
      E(I)=P
      H=H-F*G
      Z(I,I-1)=F-G
      F=0.
      DO7J=1,L
      Z(J,I)=Z(I,J)/H
      G=0.
      DO5K=1,J
    5 G=G+Z(J,K)*Z(I,K)
      M=J+1
      IF(M.GT.L)GOTO 61
      DO 6 K=M,L
    6 G=G+Z(K,J)*Z(I,K)
   61 E(J)=G/H
    7 F=F+G*Z(J,I)
      HH=F/(H+H)
      DO8 J=1,L
      F=Z(I,J)
      G=E(J)-HH*F
      E(J)=G
      DO8 K=1,J
    8 Z(J,K)=Z(J,K)-F*E(K)-G*Z(I,K)
    9 D(I)=H
      I=I-1
      IF(I-3)10,1,1
   10 E(2)=Z(2,1)
      D(2)=0.
      D(1)=0.
      E(1)=0.
      DO17I=1,N
      L=I-1
      IF(D(I))11,14,11
   11 DO13J=1,L
      G=0.
      DO12K=1,L
   12 G=G+Z(I,K)*Z(K,J)
      DO13K=1,L
   13 Z(K,J)=Z(K,J)-G*Z(K,I)
   13 Z(K,J)=Z(K,J)-G*Z(K,I)
   14 D(I)=Z(I,I)
      Z(I,I)=1.
      IF(L)17,17,15
   15 DO16J=1,L
      Z(I,J)=0.
   16 Z(J,I)=0.
   17 CONTINUE
      DO21I=2,N
   21 E(I-1)=E(I)
      ENTRY TGL2
      E(N)=0.
      B=0.
      F=0.
      DO36L=1,N
      J=0
      H=EPS*(ABS(D(L))+ABS(E(L)))
      IF(B.LT.H)B=H
      DO22M=L,N
      IF(ABS(E(M))-B)23,23,22
   22 CONTINUE
   23 IF(M-L)24,36,24
   24 IF(J-30)25,37,37
   25 J=J+1
      P=(D(L+1)-D(L))/(2.*E(L))
      IF(ABS(P)-1.E+6)261,261,260
  260 Q=2.*P
      GOTO 28
  261 R=SQRT(P*P+1.)
      IF(P)26,27,27
   26 Q=P-R
      GOTO28
   27 Q=P+R
   28 H=D(L)-E(L)/Q
      DO29I=L,N
   29 D(I)=D(I)-H
      F=F+H
      P=D(M)
      C=1.
      S=0.
      I=M-1
   30 Q=E(I)
      G=C*Q
      H=C*P
      IF(ABS(P)-ABS(Q))32,31,31
   31 Q=Q/P
      R=SQRT(Q*Q+1.)
      E(I+1)=S*P*R
      C=1./R
      S=Q*C
      GOTO33
   32 C=P/Q
      R=SQRT(C*C+1.)
      E(I+1)=S*Q*R
      S=1./R
      C=C*S
   33 P=C*D(I)-S*G
      D(I+1)=H+S*(C*G+S*D(I))
      DO34K=1,N
      H=Z(K,I+1)
      Z(K,I+1)=S*Z(K,I)+C*H
   34 Z(K,I)=C*Z(K,I)-S*H
      I=I-1
      IF(I-L)35,30,30
   35 E(L)=S*P
      D(L)=C*P
      IF(ABS(E(L))-B)36,36,24
   36 D(L)=D(L)+F
      IF(.NOT.FAIL)GOTO46
      M=N-1
      DO 45 I=1,M
      K=I
      P=D(I)
      L=I+1
      DO42J=L,N
      IF(D(J)-P)41,42,42
   41 K=J
      P=D(J)
   42 CONTINUE
      IF(K-I)43,45,43
   43 D(K)=D(I)
      D(I)=P
      DO44J=1,N
      P=Z(J,I)
      Z(J,I)=Z(J,K)
   44 Z(J,K)=P
   45 CONTINUE
   46 FAIL=.FALSE.
      RETURN
   37 FAIL=.TRUE.
      RETURN
      END
      EXIT

*
*
