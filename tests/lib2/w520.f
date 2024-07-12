      PROGRAM W520
           COMPLEX DE
      COMMON /OST/ DE(2,2,5) , ET(2,2,5)
      DIMENSION S(6) , TMRY(2,2,5)
      DO  1  I=1,2
      DO 1 J=1,2
      READ 15 , ( TMRY(I,J,K) , K=1,5 )
    1 CONTINUE
   15 FORMAT  ( 5F16.2 )
      DO  2  I=1,2
      DO  2  J=1,2
      PRINT 16 , ( TMRY(I,J,K) , K=1,5 )
    2 CONTINUE
   16 FORMAT  ( 1H0,10X,5F16.2 )
      DO  20  I=1,2
      DO  20  J=1,2
      DO  20  K=1,5
      DE(I,J,K) = TMRY(I,J,K)*(0.,2.)/57.296
      ET(I,J,K) = 1.0
   20 CONTINUE
      ET(1,1,2) = 0.75
      ET(1,1,3) = 0.98
      P = 2.417
      PRINT  25
      DO  10  J=1,19
      TETA = 10.*(J-1)
      T = COS (TETA/57.296 )
      CALL  PSCS( P, T, S )
      PRINT 30 , TETA , T , ( S(K) , K=1,5 )
   10 CONTINUE
   25 FORMAT(1H0,10X,4HTETA,5X,8HCOS TETA,2X,8HNEGATIVE,4X,8HPOSITIVE,
     ,3X,7HNEUTRAL ,2X,9HNEG TOTAL,4X,9HPOS TOTAL)
   30 FORMAT ( 1H0,10X,F5.1 , 5X , F7.4 , 5( 2X , F9.3 ) )
      END
*EXECUTE
             0.0            34.3             7.2             1.5
            16.4            -5.1             1.2             0.1
             0.0           -11.6             0.2             0.0
           -24.4           144.8            -1.3             0.8
*
*
