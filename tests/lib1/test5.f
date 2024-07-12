      PROGRAM TEST5
      EXTERNAL KERN,G2
      REAL F(11,7),X1(11),X2(7),Y(11,7),A(6024)
      REAL ALPHA,X1I,X2J,A1,A2
      INTEGER I,J,IER
      COMMON /MULT/ ALPHA
      ALPHA=.1
      DO 1 I=1,11
 1      X1(I)=-2.+1.*FLOAT(I-1)
      DO 2 J=1,7
 2      X2(J)=-1.+1.*FLOAT(J-1)
      DO 3 I=1,11
        X1I=X1(I)
        A1=50.-(X1I-3.)**2
        A2=ALPHA*(X1I+7.)*(.044*((8.-X1I)**3+(X1I+2.)**3)-.25*X1I**2+
     +  1.5*X1I-6.)
        DO 3 J=1,7
          X2J=X2(J)
          Y(I,J)=A1-2.*(X2J-2.)**2
          F(I,J)=Y(I,J)+A2+ALPHA*(X2J+8.)*(.625/9.*((5.-X2J)**3+(X2J+
     +    1.)**3)-.18*X2J**2+.72*X2J-1.692)-2140.*ALPHA
 3        CONTINUE
      CALL FREST3(F,X1,11,X2,7,KERN,G2,Y,IER,A)
      PRINT 4,IER
 4    FORMAT(21H TEST5(FREST3)   IER=,I2)
      IF(IER.EQ.0) PRINT 5,Y
 5    FORMAT(18H SOLUTON Y(X1,X2)=/(8E15.7))
      PRINT 20
 20   FORMAT(///50X 16H E N D     TEST5)
      CALL EXIT
      END
      REAL FUNCTION KERN(X1,S1,X2,S2)
      REAL X1,S1,X2,S2,ALPHA
      COMMON /MULT/ ALPHA
      KERN=ALPHA*(1.-.5E-3*((X1-S1)**2*(7.+X1)+(X2-S2)**2*(8.+X2)))
      RETURN
      END
      REAL FUNCTION G2(X1,X2)
      REAL X1,X2
      G2=1.
      RETURN
      END
*EXECUTE
