      PROGRAM B101
      X=1.0
      PI=3.1415926539
      PRINT 20
      DO 11 I=1,4
      AT=ATAN(SIN(X)/COS(X))
      ATT=ATG(SIN(X),COS(X))
      PRINT 10, X,AT,ATT
 11   X=X+PI/2.0
      ATT=ATG(-1.0,0.0)
      PRINT 12, ATT
 10   FORMAT(3X,2HX=,F15.11 ,10X,5HATAN=,F15.11,10X,4HATG=,F15.11)
  12  FORMAT(/10X,'ATG(-1.0,0.0)='E13.7)
  20  FORMAT(//45X,'ARC TANGENT',45X,'TEST B101'/45X,11(1H*),45X,9(1H*)/
     1/)
      END
*EXECUTE
*
*