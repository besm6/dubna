      PROGRAM C110
      DIMENSION A(100),B(100),D(100),G(200)
      A(1)=1.
      A(2)=2.
      A(3)=4.
      A(4)=3.
       A(5)=6.
      B(1)=2.
      B(2)=4.
       B(3)=5.
       B(4)=5.
        M=3
        N=4
      YC=5.
       MN=M+N+1
       CALL ABMNS(A,B,M,N,D,MN2)
       PRINT 10,(D(I),I=1,MN)
  10   FORMAT(F20.10)
       CALL AMYCS(D,MN2,YC,G)
        PRINT 1500
 1500    FORMAT(//////,17HPOLINOM  *  CONST,//)
       PRINT 10,(G(I),I=1,MN)
      END
      SUBROUTINE AMYCS(A,M,YC,S)
      DIMENSION A(100),S(100)
      M1=M+1
      DO 1 I=1,M1
  1    S(I)=A(I)*YC
      RETURN
      END
*EXECUTE
*
*
