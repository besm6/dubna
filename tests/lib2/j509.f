      PROGRAM J509
      DIMENSION F(30,30)
      IDIM1=30
      IDIM2=30
      FMIN=0.0
      FMAX=0.0
      DX=1.0
      DY=1.0
      X=0.0
      R=10.0
      DO 5 I=1,21
      Y=0.0
      DO 4 J=1,21
      RADXY=SQRT((10.-X)**2+(10.-Y)**2)
      IF(R-RADXY) 2,2,1
  2   F(I,J)=0.0
      GO TO 4
    1 F(I,J)=SQRT(R**2-RADXY**2)
  4   Y=Y+DY
  5   X=X+DX
      PRINT 13
      DO 6 I=1,21
      PRINT 10,(F(I,J),J=1,21)
  6   CONTINUE
  7   READ 11,M,N,NC
      CALL CONPRT(F,IDIM1,IDIM2,M,N,NC,FMIN,FMAX)
      PRINT 12,M,N,NC
      READ 11,ISW,IDUM,IDUM
      IF (ISW)  7,8,7
  8   STOP
  10  FORMAT(21F6.2)
  11  FORMAT(3I5)
  12  FORMAT(ДM = Д,I3/ДN = Д,I3/ДNC= Д,I3)
  13  FORMAT(1H1)
      END
*EXECUTE
   21   21   10
    1
   21   11   10
    0
*
*
