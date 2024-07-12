      PROGRAM D500
C        CARDS  OF  DATAS  33
      CALL MALIK
      STOP
      END
      SUBROUTINE FUNCT(F,I)
C*****DIESE SUBROUTINE BERECHNET DIE MATRIXELEMENTE FUER 1-RESONANZEN***
      DIMENSION PAR(10)
      DIMENSION X(2,150)
      COMMON PAR,X
      COMMON /MAX/ NMAX
      F=0
      DO 1 K=1,NMAX
      WB=X(1,K)
      WC=X(2,K)
      W=(.25/3.141592)*((3.-PAR(1))/2.*(1.-WB**2)+PAR(1)*WB**2
     1 +3.46414*PAR(2)*WB-4.899*PAR(3)*SQRT(1.-WB**2)*COS(WC)
     2 -4.242634*2.*PAR(4)*WB*SQRT(1.-WB**2)*COS(WC)-3.*PAR(5)*(1.
     1 -WB**2)*COS(2.*WC))
      AST=-1.0E+18
      IF(W)2,2,3
    3 AST=ALOG(W)
    2 F=F+AST
 1    CONTINUE
      RETURN
      END
      SUBROUTINE INPUT
      DIMENSION PAR(10)
      DIMENSION X(2,150)
      COMMON PAR,X
      COMMON /MAX/ NMAX
      READ102,NMAX
      I=0
 40   I=I+1
      READ101,(X(KF,I),KF=1,2)
      IF(NMAX.NE.I) GO TO 40
 30   RETURN
  101 FORMAT(2F10.5)
  102 FORMAT(I10)
      END
*EXECUTE
                             5
                2.0       0.1
                           0.1
                           0.1
                           0.1
                           0.1
                           0.1       100       0.1
        25
    -39450    290150
    -51050    147040
     20330     50730
     80430    212220
     45990    217280
    -63100     93790
    -99210     56960
    -91000    219060
     84010     20580
     79160     71020
     92830    226220
     80510    227330
     95640    184470
    -52720    301590
    -65080     97790
    -80190    225410
     26000     28510
     43000    232270
     71860     12410
     80390     93880
      5370    128570
      8930    156690
     12440    172500
     38030    239100
     75280     83110
*
*
