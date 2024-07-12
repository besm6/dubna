      PROGRAM C316 A
CALCULATION OF REGULAR COULOMB WAVE FUNCTIONS
      DIMENSION F(101),KA(20)
      DOUBLE PRECISION F
      READ10,LMAX,ETA0,RO,A,B,I1,I2,N
      DO 1 I=1,I1 $ RO=RO+A
      ETA=ETA0
      PRINT 20 $ ILMAX=LMAX+1 $ PRINT 30,N
      PRINT 40,LMAX,RO $ PRINT 50 $ PRINT 60
      KA(1)=0 $ DO 2 IL=2,ILMAX $ KA(IL)=K A(IL-1)+1
    2 CONTINUE
      PRINT 70,(KA(IL),IL=1,ILMAX)
      PRINT 80
      PRINT 90
      PRINT 50
        DO  1  J=1,I2
      ETA=ETA+B
      IF (IFOVFL(1).EQ.1) GOTO 1
      CALL COUL 1 (ETA,RO,LMAX,N,F)
  3   PRINT 100,ETA,(F(L),L=1,ILMAX)
      PRINT 50
    1 CONTINUE
   10 FORMAT (I4,4(F4.1),3(I6))
   20 FORMAT (1H1,31H REGULAR COLOMB WAVE FUNCTIONS )
   30 FORMAT (11X,32H NUMBER OF SIGNIFICANT DIGITS N=,I6 )
   40 FORMAT ( 5X,11H TABL LMAX=,I4,4H RO=,F4.1)
  50  FORMAT( 5X,114H---------------------------------------------------
     1---------------------------------------------------------------)
   60 FORMAT ( 5X,1HI,1H*,3HFOL,1HI,9(11X,1HI))
   70 FORMAT ( 5X,1HI,1X,1H*,2X,1HI,9(5X,I1,5X,1HI))
   80 FORMAT ( 5X,1HI,2X,1H*,1X,1HI,9(11X,1HI))
   90 FORMAT ( 5X,1HI,5HETA*I,  9(11X,1HI))
  100 FORMAT ( 5X,1HI,F4.1,1HI,9(D11.4,1HI))
      END
*EXECUTE
   8-1.0-1.0 1.0 1.0  21    21    15
*
*                                                                    B
