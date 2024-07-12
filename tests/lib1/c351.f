      PROGRAM C351
      PRINT 2
      DO 1 I=1,500
      X=0.01*I
      Z=2.*SQRT(X)**3/3.
      ZI=0.
      IF(X.GT.1.E-5) ZI=1./Z
      CALL AIRY(X,AI,BI)
      CALL AIRY(-X,AM,BM)
  1   PRINT 5,X,Z,ZI,AI,BI,AM,BM
  2   FORMAT(/45X,'TAБЛИЦA ФYHKЦИЙ ЭЙPИ'/1X,126('-')//
     *5X,'X',10X,'Z',9X,'1/Z',12X,'AI(X)',10X,'BI(X)',15X,'AI(-X)',
     *10X,'BI(-X)'/)
  5   FORMAT(1X,F9.5,2F13.6,2E16.7,2X,2E16.7)
      END
*EXECUTE
*
*
