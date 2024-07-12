      PROGRAM X202
      DIMENSION XX(4),Y(4)
      DATA Y/.13501,74.,43.,23./
      PRINT 5
      DO 2 I=1,21
      X=VOMAS(I)
      IX=IOFMAS(X)
 2    PRINT 1,IX,X
      DO 4 I=1,4
 4    XX(I)=VOMAS(Y(I))
      PRINT 3,Y,XX
 1    FORMAT(5X,I2, 5X,E15.6)
 3    FORMAT(/5X,'Y =',4F10.3/5X,'XX=',4F10.3)
 5    FORMAT(/5X,'IX',12X,'X'//)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
