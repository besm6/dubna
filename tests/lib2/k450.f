      PROGRAM K450
      DIMENSION IA(256)
      COMMON/ICHECK/IC(6)
      CALL DNS556
      IC(1)=1
      CALL BUF608
      DO 1 I=1,10
      IA=I
      CALL WRTBUF(IA,256)
1     CONTINUE
      CALL MKFILE
      DO 2 I=1,10
      IA=10+I
      CALL WRTBUF(IA,256)
2     CONTINUE
      CALL MKFILE
       CALL SCHMFB
      CALL SCHMFB
      CALL SCHMKF
      DO 3 I=1,10
      CALL RDBUF(IA,N)
      PRINT 5,N,IA(1)
3     CONTINUE
5     FORMAT (2I10)
       CALL REWIND
      DO 4 I=1,10
      CALL RDBUF(IA,N)
      PRINT 5,N,IA(1)
4     CONTINUE
      END
*EXECUTE
*
*
