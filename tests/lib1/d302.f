      PROGRAM D302
      DIMENSION F(33,30),BWEST(30),BEAST(30),BNORTH(33),
     1BSOUTH(33),COEFS(3)
      IBX=3 $ JBY=1
      DX=2./32.
      DY=1./29.
      COEFS(1)=1.
      COEFS(2)=0.
      COEFS(3)=0.
      DO 10 J=1,30
      Y=(J-1)*DY
      BWEST(J)=Y*Y+Y-1.
   10 BEAST(J)=Y*Y+Y+1.
      DO 20 I=1,33
      X=(I-1)*DX-1.
      BSOUTH(I)=X
   20 BNORTH(I)=X*X+X+1.
      DO 30 I=1,33
      X=-1.+(I-1)*DX
      DO 30 J=1,30
      Y=(J-1)*DY
      F(I,J)=2.*(X*X+Y*Y)
   30 CONTINUE
      CALL ELPAHY(F,33,30,DX,DY,COEFS,IBX,BWEST,BEAST,
     1JBY,BSOUTH,BNORTH)
      PRINT 50
      PRINT 100,((I,J,F(I,J),J=1,30),I=1,33)
  100 FORMAT(5('F(',I2,',',I2,')=',E13.6,3X))
      DO 40 I=1,33
      X=-1.+(I-1)*DX
      DO 40 J=1,30
      Y=(J-1)*DY
      F(I,J)=X**2*Y**2+X+Y
   40 CONTINUE
      PRINT 50
      PRINT 100, ((I,J,F(I,J),J=1,30),I=1,33)
   50 FORMAT (1H1)
      IBX=3 $ JBY=1
      COEFS(1)=0.
      COEFS(2)=-1.
      COEFS(3)=0.
      DX=1./32.
      DY=0.003
      DO 11 J=1,30
      Y=(J-1)*DY
      BWEST(J)=Y
   11 BEAST(J)=Y+1.
      DO 21 I=1,33
      X=(I-1)*DX
      BSOUTH(I)=X*X
   21 CONTINUE
      DO 31 I=1,33
      DO 31 J=1,30
      F(I,J)=1.
   31 CONTINUE
      CALL ELPAHY(F,33,30,DX,DY,COEFS,IBX,BWEST,
     1BEAST,JBY,BSOUTH,BNORTH)
      PRINT 50
      PRINT 100,((I,J,F(I,J),J=1,30),I=1,33)
      DO 41 I=1,33
      X=(I-1)*DX
      DO 41 J=1,30
      Y=(J-1)*DY
      F(I,J)=X*X+Y
   41 CONTINUE
      PRINT 50
      PRINT 100,((I,J,F(I,J),J=1,30),I=1,33)
      IBX=3 $ JBY=1
      COEFS(1)=-1.
      COEFS(2)=0.
      COEFS(3)=0.
      DX=2./32.
      DY=0.05
      DO 12 J=1,30
      Y=(J-1)*DY
      BWEST(J)=0.5+0.5*Y**2
   12 BEAST(J)=0.5+0.5*Y**2
      DO 22 I=1,33
      X=-1.+(I-1)*DX
      BSOUTH(I)=0.5*X**2
      BNORTH(I)=0.
   22 CONTINUE
      DO 32 I=1,33
      DO 32 J=1,30
      F(I,J)=0.
   32 CONTINUE
      CALL ELPAHY(F,33,30,DX,DY,COEFS,IBX,BWEST,
     1BEAST,JBY,BSOUTH,BNORTH)
      PRINT 50
      PRINT 100,((I,J,F(I,J),J=1,30),I=1,33)
      DO 42 I=1,33
      X=-1.+(I-1)*DX
      DO 42 J=1,30
      Y=(J-1)*DY
      F(I,J)=0.5*X**2+0.5*Y**2
   42 CONTINUE
      PRINT 50
      PRINT 100,((I,J,F(I,J),J=1,30),I=1,33)
      END
*EXECUTE
*
*
