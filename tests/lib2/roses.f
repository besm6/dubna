      PROGRAM ROSES
       DIMENSION X(6),Y(6),N(6),A(6),B(6)
      DATA(X=42.,48.,92.,92.,126.,140.)
      DATA(Y=40.,68.,56.,108.,82.,34.)
      DATA(N=16,8,24,16,8,8)
      DATA(A=11.,10.,18.,11.,10.,10.)
      DATA(B=2.,5.,3.,2.,5.,5.)
      CALL MMS
      CALL PAGE(180.,140.,5HROГOB,5,1)
      DO 1 J=1,6
      M = N(J)
      DO 3 K=1,M
      L=INT(A(J))+1
      DO 2 I=1,L
      CALL BEGLEV
      CALL PSCALE(X(J),Y(J),1.-FLOAT(I-1)/A(J))
      CALL ELIPS(X(J),Y(J),A(J),B(J),0.,-180.,180.)
   2  CALL ENDLEV
   3  CALL ROTATE(X(J),Y(J),360./FLOAT(M))
   1  CONTINUE
      CALL ENDPG(5HPOГOB)
      END
*EXECUTE
*
