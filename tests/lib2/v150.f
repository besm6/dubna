      PROGRAM V150
      DIMENSION Y(1000)
      N = 1000
      DO 100 I= 1, N
      Y(I) = FLOAT(I)
      IF (2*(I/2) .EQ. I)  Y(I) = 0.
      IF (3*(I/3) .EQ. I)  Y(I) = 0.
  100 CONTINUE
      XLO = -10.
      XWID = 2.
      NTHROW = 100
      N = 300
      CALL HRTSUB(Y,N,XLO,XWID,NTHROW)
      N = 200
      CALL HRTSUB(Y,N,XLO,XWID,NTHROW)
      XLO = 0.
      N = 100
      CALL HRTSUB(Y,N,XLO,XWID,NTHROW)
      N = 50
      CALL HRTSUB(Y,N,XLO,XWID,NTHROW)
      N = 10
      CALL HRTSUB(Y,N,XLO,XWID,NTHROW)
      N = 1
      CALL HRTSUB(Y,N,XLO,XWID,NTHROW)
      END
      SUBROUTINE HRTSUB(Y,N,XLO,XWID,NTHROW)
C         SUBROUTINE TO TEST HISRAN (V150)
      DIMENSION Y(N),YSAVE(100)
C         FIRST CALL TO GET CIM. PROB. DISTR. SET UP.  PRINT NEW Y
      PRINT 1,(Y(I),I=1,N)
      CALL HISPRE(Y,N,XLO,XWID,XRAN)
      PRINT 2
      PRINT 1,(Y(I),I=1,N)
  1   FORMAT(20X,1HY/10(5X,10F10.5/))
  2   FORMAT(/50X,6HHISPRE/)
      CALL CTIME(CT1,RT1)
C         MAIN LOOP TO GENERATE NTHROW NUMBERS
      DO 200 I= 1, NTHROW
      CALL HISRAN(Y,N,XLO,XWID,XRAN)
      YSAVE(I) = XRAN
  200 CONTINUE
C         END OF GENERATION.   PRINT TIMING
      CALL CTIME(CT2,RT2)
      RT=RT2-RT1
      PRINT 1004,NTHROW,N,RT,YSAVE
 1004 FORMAT (///  22H0++   HISRAN GENERATEDI8,  13H NUMBERS OVERI6,   8
     1H BINS IN    F10.2,   9H SECONDS.//50X,4HXRAN//10(5X,10F10.5/))
      END
*EXECUTE
*
*
