      PROGRAM V202
      DIMENSION IA(3)
      N=3
      IA(1)=0
 1    CALL PERMU(IA,N)
      IF(IA(1))2,3,2
 2    PRINT 10(IA(K),K=1,N)
 10   FORMAT(1HB3I5)
      GO TO 1
 3    CONTINUE
      END
*EXECUTE
*
*
