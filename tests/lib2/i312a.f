          PROGRAM I312 A
C         COCTABИЛ  MИЛЛEP B.B.   1986 Г.
          REAL MB,MS,CS
          INTEGER II(10)
          DATA II/1,45,-2,.4,98888888888, .2,0,-75,12,-12/
      DATA MB,MS/7757777777777776B,7777777777777777B/
     *,CS/0010000000000000B/
          COMMON/B/B(200)
          COMMON/AA/A(21)
          CALL PRINTL(II,II(10),16,6,2,'II-II(10)=' )
          CALL PRINTL(II,II( 6),6,15,2,'II-II( 6)=' )
          B=1.1E18$DO2 I=2,40
    2 B(I)=B(I-1)*(-.1213)
          B(9)=.2
          B(7)=2.E-3
          B(8)=1.E-7
          B(5)=.1$B(6)=1.E-8
          PRINT88,(B(I),I=1,40)
   88 FORMAT(10E12.5)
          CALL PRINTF(B,B(25),21,6)
          CALL PRINTF(B,B(40),12,5)
          CALLPRINTF(B,B(5),16,6)
          CALL PRINTF(B,B(35),15,8)
          CALL PRINTL(B,B(20),18,7,1,6H+TEST  )
       CALL PRINTF(B(2),B(1),12,4)
          CALL PRINTF(B(2),B(0),15,4)
          CALLPRINTL(B,B(40),16,6,4,24H+TECT ПEЧATИTECT ПEЧATИ  )
          CALL PRINTL(B,B(40),18,7,2,12H+TECT ПEЧATИ )
          B(11)=0.
          CALL PRINTO( B, B,7,17)
          CALL PRINTF(B,B(37),20,4)
      CALL PRNT(11,B,B(3),B(5),B(7),B(9),B(2),B(4),B,B(10),B
     *,B(6),11,4,24H+TECT ПEЧATИ,TECTПEЧATИ )
          CALL PRNT(16,B(2),B(15),B(25),B(26),4,4,
     *24H+B(2),B(15),B(25),B(26)=  )
      CALL PRNT(21,B,B(3),B(5),B(7),B(9),B(2),B(4),B,B(10),B
     *,B,B,B,B,B,B,B,B,B,19,2,10H+B(1-10)=  )
          CALL PRINTF(B,B(16),16,6)
      CALL PRNT(21,B,B(3),B(5),B(7),B(9),B(2),B(4),B,B(10),B
     *,B,B,B,B,B,B,B,B,B,B,B(8),21,0,0)
          CALL PRINTL(B,B(26),16,9,11
     *,'+TEKCT                       PPPP                           RR')
          PRINT 45,MB,MS,CS
   45 FORMAT(3E20.13)
          CALL PRINTE(MB,MB,2,15)
          CALL PRINTF(MB,MB, 2,20)
          CALL PRINTE(CS,CS,2,20)
          CALL PRINTF(CS,CS,2,15)
          CS=1./MB
          PRINT 45,CS
          CALL PRINTO(CS,CS,7,17)
          CALL PRINTE(CS,CS,2,13)
          CALL PRINTF(CS,CS,2,15)
          CALL PR NT(15,B,B(5),B(6),B(7),4,10
     *,'+TEKCT                       PPPP                           RR')
          MB=1./CS
          CALL PRINTE(MB,MB,2,13)
          CALL PRINTF(MB,MB,2,15
     *)
          PRINT 45,MB
          CALL PRINTO(MB,MB,7,17)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*   M257  ИЗMEHИЛИ HA I312   07/1985
