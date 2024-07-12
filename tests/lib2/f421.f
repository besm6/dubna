      PROGRAM F421
       COMMON/OUTL/ITL,KPL,KPFL
       DIMENSION Z(30,30),ZL(10,10),XX(10),YR(10)
       KPL=1
       M=10
       N=10
       R=0.000000000001
       FR=0.0000001
       DO 10 K=1,2
       DO 2 I=1,M
       DO 1 J=1,N
   1   ZL(I,J)=1.
       IF(I-N) 3,3,2
   3   CONTINUE
       ZL(I,I)=ZL(I,I)+R*I+(K-1)*FR*(-1)**I
   2   YR(I)=N+R*I+(K-1)*FR*(-1)**(I-1)
       KPFL=1
       CALL SILLC(M,N,Z,ZL,XX,YR)
  10   CONTINUE
       END
       SUBROUTINE RELADI(N,NP,INDEX,A,Y,DF)
       DIMENSION A(N),DF(N)
       RETURN
       END
*EXECUTE
*
*
