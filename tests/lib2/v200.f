      PROGRAM V200
      N=10.0
      DO 2 M=1,10
      I=KBINOM(N,M)
      PRINT 3,N,M,I
  3   FORMAT(2F10.2)
  2   CONTINUE
      END
*EXECUTE
*
*
