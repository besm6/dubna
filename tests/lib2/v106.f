      PROGRAM V106
      DIMENSION POUT(50,50), NOUT(50)
      DATA NC1,NC2,NC3/ 2H  , 2H.., 2H++/
C         PRINT 500 RANDOM NUMBERS
      CALL PRT500
C                            . . READ MEAN VALUE AND INITIALIZE ........
      XMEAN = 4.
      MEAN = XMEAN
      QDEV = SQRT(XMEAN)
      N1 = XMEAN - QDEV
      N2 = XMEAN + QDEV
C
      DO 500 IX= 2, 5
      IXX = IX - 1
      DO 490 IY= 1, IXX
      LEN = IX
      CALL VZERO(POUT,2500)
C                            . . GENERATE RANDOM DISTRIBUTION ..........
      DO 200 IL= 1, MEAN
      DO 180 JL= 1, 2500
      A = RN32(JL)
      B = RN32(JL+1)
      IF (RN32(JL+2) .GT. 0.9)  B=RN32(JL+3)
      I = A*50. + 1.0
      J = B*50. + 1.0
  180 POUT(I,J) = POUT(I,J) + 1.0
  200 CONTINUE
C                                   ..   FIND CHISQUARE
      CHI2 = 0.000
      DO 300 J= 1, 50
      DO 280 I= 1, 50
      CHI2 = CHI2 + (POUT(I,J)-XMEAN)**2 / XMEAN
  280 CONTINUE
  300 CONTINUE
      DEV = (CHI2-2500.)/SQRT(5000.)
      PRINT 1003, IX,IY,XMEAN,CHI2, DEV
C                            . . PRINT VISUAL PATTERN DENSITY ..........
      DO 400 J= 1, 50
      DO 380 I= 1, 50
      N = POUT(I,J)
      NOUT(I) = NC1
      IF (N .GE. N1)  NOUT(I) = NC2
      IF (N .GT. N2)  NOUT(I) = NC3
  380 CONTINUE
  400 PRINT 1004, NOUT
  490 CONTINUE
  500 CONTINUE
  600 CONTINUE
      CALL RN32OT(IOUT)
      PRINT 1025, IOUT
      CALL PRT500
      IX = 12335
      CALL RN32IN(IX)
      PRINT 1025, IX
C         PRINT 500 RANDOM NUMBERS
      CALL PRT500
      IX =        2147483647
      CALL RN32IN(IX)
      PRINT 1025,IX
      CALL PRT500
C
 1000 FORMAT (4F10.0)
 1003 FORMAT (1H1,2I10,2X,F10.2,5X,5HCHI2=,F10.1,5X,4HDEV=,F10.5/)
 1004 FORMAT (20X ,50A2)
 1005 FORMAT (1H1///2I10,20X,F10.5///)
 1025 FORMAT (15H0RN32OT VALUE         I12)
      STOP
      END
      SUBROUTINE PRT 500
C         PRINT 500 RANDOM NUMBERS
      DIMENSION VEC(20)
COMMENT                                                              1 =
      DO 5 I= 1, 500,10
      DO 4 J= 1, 10
    4 VEC(J) =  RN32 (J)
      PRINT 1006, I,(VEC(L),L=1,10)
 1006 FORMAT (I5,10F12.9)
    5 CONTINUE
      RETURN
      END
*EXECUTE
*
*
