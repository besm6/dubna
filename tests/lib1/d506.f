      PROGRAM D506
C        CARDS  OF  DATAS  11
      CALL MINUIL
      STOP
      END
      SUBROUTINE FCN(NPAR,G,F,X,IFLAG)
      DIMENSION G(3),X(3),XI(5),YI(5)
      IF(IFLAG.NE.6) GO TO 1
      READ 100,XI
 100  FORMAT(5F10.3)
      GO TO 20
   1  GO TO (10,20,20,20),IFLAG
  10  READ 100,YI
      DO 5 I=1,5
    5 XI(I)=1.E3
  20  F=0. $ G=0. $ G(2)=0. $ G(3)=0.
      DO 21 I=1,5
      A=X*XI(I)**2+X(2)*XI(I)+X(3)-YI(I)
      F=F+A**2
      G=G+2.*A*XI(I)**2 $ G(2)=G(2)+2.*A*XI(I) $ G(3)=G(3)+2.*A
  21  CONTINUE
      IF(IFLAG-3)40,30,40
  30  PRINT 101,F,X,G
 101  FORMAT(1X,'F='E10.2,3E15.4/13X,3E15.4)
  40  RETURN
      END
*CALL PTIME
*EXECUTE
D506 HA БЭCM-6. TOЧHЫE ЗHAЧEHИЯ ПAPAMETPOB 1,1,1
         1         A       1.2       0.5        0.       10.
         2         B       1.4       1.0        0.       10.
         3         C       1.6       1.0        0.       10.
         0
        3.        7.       13.       21.       31.
CALL FCN           6
        1.        2.        3.        4.        5.
GRADIENT
SEEK
SIMPLEX
MIGRAD           100
END
D506 HA БЭCM-6. TOЧHЫE ЗHAЧEHИЯ ПAPAMETPOB 1,1,1
         1         A       1.2       0.5        0.       10.
         2         B       1.4       1.0        0.       10.
         3         C       1.6       1.0        0.       10.
         0
       2.9       7.1      12.9      21.1      30.9
GRADIENT
SEEK
SIMPLEX
MIGRAD           100
END
D506 HA БЭCM-6. TOЧHЫE ЗHAЧEHИЯ ПAPAMETPOB 1,1,1
         1         A       1.2       0.5        0.       10.
         2         B       1.4       1.0        0.       10.
         3         C       1.6       1.0        0.       10.
         0
       3.1       6.9      13.1      20.9      31.1
GRADIENT
SEEK
SIMPLEX
MIGRAD           100
EXIT
*
*             CTAPAЯ  BEPCИЯ  ПPOГPAMMЫ
      SUBROUTINE FCN (NPAR,G,F,X,IFLAG)
      DIMENSION X(60),G(60),FKOEF(10)
      DATA ((FKOEF(I),I=1,6)=2.,0.5,10.,0.75,0.2,0.3)
      A=0.0
      DO 1 I=1,NPAR
   1  A=A+FKOEF(I)*(X(I)-FLOAT(I))**2
      F=A+1.
      IF (IFLAG.NE.2)  GO TO 3
      DO 2 I=1,NPAR
    2 G(I) = 2.*FKOEF(I)*(X(I)-FLOAT(I))
      RETURN
    3 IF (IFLAG.NE.3)   RETURN
      PRINT 4
    4 FORMAT (2H0 120(1H*))
      PRINT 5,F,(X(I),I=1,6)
    5 FORMAT(2H0 37H THE SOLUTION    F  AND   PARAMETERS   /7E15.7)
      PRINT 4
      RETURN
      END
*EXECUTE
   11 1
  SIXDIMENSIONAL  PARABOLOID *** OCTOBER **** 1969 ****
      1000       500     0.5
         1 X(1)                1.0                          11
         2 X(2)                1.0                          11
         3 X(3)                1.0                          11
         4 X(4)                1.0                          1
         5 X(5)                1.0                          1
         6 X(6)                1.0                          1
         0
*
*
