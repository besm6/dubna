         PROGRAM D508
      DIMENSION A(10),X(1,100),Y(100),W(100),DA(10),H(10,11)
      DOUBLE PRECISION H
          K=5
      N=20
      M=1
      COV=1.0
      QPRINT=1.0
      DO 1 J=1,N
      X(1,J)=FLOAT(J)
      Y(J)=3.5+1.5*X(1,J)**2.0+0.5*X(1,J)+2.0*X(1,J)**3.0+X(1,J)**4.0
      W(J)=1.0
  1   CONTINUE
      PRINT 2,(J,X(1,J),Y(J),W(J),J=1,N)
  2   FORMAT(35X,I3,3E15.3)
      CALL LINSQ(K,N,M,A,X,Y,W,DA,H,COV,QPRINT,QMIN)
      END
      SUBROUTINE FCN(K,M,F,X,IFLAG)
      DIMENSION F(10),X(1)
      F(1)=1.0
           F(2)=X(1)**2.0
         F(3)=X(1)
          F(4)=X(1)**3.0
          F(5)=X(1)**4.0
      RETURN
      END
*EXECUTE
*
*
