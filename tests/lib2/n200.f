      PROGRAM N200
      DIMENSION N1(1),N2(2),N3(3),N4(4),N5(5),N6(6),N7(7),N24(24),N31
     1       (31)
      EQUIVALENCE(N31,N1,N2,N3,N4,N5,N6,N7,N24)
        DO 1 K=1,31
   1  N31(K)=6HXA-XA
      WRITE(16)N1
      WRITE(16)N2
      WRITE(16)N3
      WRITE(16)N4
      WRITE(16)N5
      WRITE(16)N6
      WRITE(16)N7
      WRITE(16)N24
      WRITE(16)N31
      REWIND 16
      CALL DUMPZD(16,7,0)
      REWIND 16
      CALL DUMPZD(16,7,1)
      REWIND 16
      CALL DUMPZD(16,0,0)
      REWIND 16
      CALL DUMPZD(16,0,1)
      END
*EXECUTE
*
*