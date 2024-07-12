      PROGRAM D209
      DIMENSION Y(3),H(6),W(3,3)
      DOUBLE PRECISION DH(6),DX,DY(3),DW(3,3)
      DATA H/0.1,0.2,0.3,0.5,1.,2./
      EXTERNAL EXTERN
      EXTERNAL EXT
      DO 1 K=1,6
 1    DH(K)=H(K)
      DO 5 I=1,6
      DO 6 L=1,3
      Y(L)=0.0
  6   DY(L)=Y(L)
      X=0.0
      DX=0.D0
      PRINT 10
      CALL RKSTP(3,H(I),X,Y,EXTERN,W)
      PRINT 2,X,Y
      CALL DRKSTP(3,DH(I),DX,DY,EXT,DW)
      PRINT 4,X,DY
      Y(1)=X**3*SIN(X)
      Y(2)=X**2*(3*SIN(X)+X*COS(X))
      Y(3)=X**2*(6*COS(X)-X*SIN(X))+6*X*SIN(X)
      PRINT 3
      PRINT 2,X,Y
      DY(1)=DX**3*DSIN(DX)
      DY(2)=DX**2*(3*DSIN(DX)+DX*DCOS(DX))
      DY(3)=DX**2*(6*DCOS(DX)-DX*DSIN(DX))+6*DX*DSIN(DX)
      PRINT 4,X,DY
  5   CONTINUE
  2   FORMAT(10X,F3.1,3E25.11/)
  3   FORMAT(45X,17HANALYTIC SOLUTION//)
 4    FORMAT(10X,F3.1,3D25.16//)
  10  FORMAT(//50X,9HTEST D209///10X,1HX,17X,4HY(1),22X,4HY(2),
     122X,4HY(3)//)
      END
      SUBROUTINE EXTERN (X,Y,F)
      DIMENSION Y(20),F(20)
      F(1)=X**2*(3*SIN(X)+X*COS(X))
      F(2)=X**2*(6*COS(X)-X*SIN(X))+6*X*SIN(X)
      F(3)=-X**2*(X*COS(X)+9*SIN(X))+18*X*COS(X)+6*SIN(X)
      RETURN
      END
      SUBROUTINE EXT(DX,DY,DF)
      DOUBLE PRECISION DX,DY(3),DF(3)
      DF(1)=DX**2*(3*DSIN(DX)+DX*DCOS(DX))
      DF(2)=DX**2*(6*DCOS(DX)-DX*DSIN(DX))+6*DX*DSIN(DX)
      DF(3)=-DX**2*(DX*DCOS(DX)+9*DSIN(DX))+18*DX*DCOS(DX)+6*DSIN(DX)
      RETURN
      END
*EXECUTE
*
*
