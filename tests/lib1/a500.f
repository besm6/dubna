      PROGRAM A500
      DOUBLE PRECISION D,VALUE
      COMPLEX R,S,T,RATIO,DIR,REC,INV,SUMM,PROD,DIFF,QUOT,POW,NEG
      PRINT 20
       R=RATIO(5,6) $ S=RATIO(2,3) $ T=RATIO(3,4)
      PRINT 1,R,S,T
       K=INUM(R) $ L=IDEN(R)
      PRINT 2,K,L
      R=DIR(K) $ PRINT 3,R
      R=REC(L) $ PRINT 4,R
      R=INV(S) $ PRINT 5,R
      R=SUMM(S,T) $ PRINT 6,R
      R=PROD(S,T) $ PRINT 7,R
      R=DIFF(S,T) $ PRINT 8,R
      R=QUOT(S,T) $ PRINT 9,R
      R=NEG(S) $ PRINT 10,R
      R=POW(S,3) $ PRINT 11,R
      D=VALUE(T)  $  PRINT 12 , D
  1   FORMAT(10X,'RATIO(5,6)=(',O17,',',O17')'/20X,'RATIO(2,3)=(',O17,',
     1'O17')'/30X,'RATIO(3,4)=(',O17,','O17')'/)
  2   FORMAT(10X,'INUM(R)=',O17,5X,'IDEN(R)=',O17/)
  3   FORMAT(10X,'DIR(K)=(',O17','O17')'/)
  7   FORMAT(10X,'PROD(S,T)=(',O17','O17')'/)
  4   FORMAT(10X,'REC(L)=(',O17','O17')'/)
  5   FORMAT(10X,'INV(S)=(',O17','O17')'/)
  6   FORMAT(10X,'SUMM(S,T)=(',O17','O17')'/)
  8   FORMAT(10X,'DIFF(S,T)=(',O17','O17')'/)
   9  FORMAT(10X,'QUOT(S,T)=(',O17','O17')'/)
  10  FORMAT(10X,'NEG(S)=(',O17','O17')'/)'/)
  11  FORMAT(10X,'POW(S,3)=(',O17','O17')'/)
  12  FORMAT(10X,'VALUE(T)='D30.23)
  20  FORMAT(//45X,'RATIONAL ALGEBRA',45X,'TEST A500'/45X,16(1H*),45X,9(
     11H*)//)
      END
*EXECUTE
*
*
