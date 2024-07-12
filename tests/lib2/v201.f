        PROGRAM V201
         DIMENSION IA(7)
         IA(1)=0
       IR=3
      IN=6
 1     CALL COMBI(IA,IN,IR)
      IF(IA(1)) 2,3,2
 2    PRINT 10,(IA(K),K=1,IR)
 10   FORMAT(1HB3I5)
      GO TO 1
 3    CONTINUE
      END
*EXECUTE
*
*
