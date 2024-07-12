      PROGRAM M405
      DATA(I=0007123445216647B)
      K=1
      DO 1 L=1,48
      J=INVERT(I,K)
      PRINT 10,I,J
      K=K+1
  1   CONTINUE
      K=0
      J=INVERT(I,K)
      PRINT 10,I,J
  10  FORMAT(2X,O16,5X,O16)
      END
*EXECUTE
*
*
