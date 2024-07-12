      PROGRAM F 203
C     TEST PROGRAM  F 203
      COMPLEX A(8,8),EW(8),EV(8,8)
      DIMENSION ME(8),AD(2,8,8)
      PRINT 13
      READ 10,N
      DO 1 I=1,N
      READ 11,(A(I,J),J=1,N)
      PRINT12,(A(I,J),J=1,N)
    1 CONTINUE
      NCE=20
      NORM1=1
      DWS=0.01
      IWRIT=1
      CALL EIMA(A,EW,EV,N,NCE,NORM1,DWS,ME,IWRIT,8,8,AD)
      PRINT 14
      DO 2 I=1,N
    2 PRINT 15,EW(I)
      PRINT 16
      DO 3 I=1,N
    3 PRINT 19,(EV(I,J),J=1,N)
      PRINT 17
      PRINT 18,(ME(I),I=1,N)
      STOP
   10 FORMAT(I5)
   11 FORMAT(8F10.2)
   12 FORMAT(1X,4('(',F5.2,' ,',F5.2,' )',5X))
   13 FORMAT( 1H1,'GIVEN MATRIX'///)
   14 FORMAT(1H1,' EIGENVALUES'//)
   15 FORMAT(1X,'(',E15.3,' ,',E15.3,' )')
   16 FORMAT(///' EIGENVECTORS'//)
   17 FORMAT(///' ORDER OF EIGENVALUES IN EW'//)
   18 FORMAT(1X,20I5)
   19  FORMAT(1X,1H(,E10.2,2H ,,E10.2,2H ),5X)
C  19 FORMAT(1X,'('(',E10.2,' ,',E10.2,' )',5X)
      END
*EXECUTE
    4
      2.00     -1.00      3.00      2.00      1.00      1.00      5.00
      0.40      1.50     -4.70      5.45      0.18     -4.00      0.10
      6.00      5.00      1.00     -1.00     -2.00     -2.00     -1.00
      1.25     -0.66      3.42      1.11     -2.50      4.85      2.33
*
*
