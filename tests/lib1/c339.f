      PROGRAM C339
      X=0.5
      PRINT 4
    4 FORMAT(////,20X,26H=TEST LI SANG HO= 4,4,1972,///)
    1 A=DAWSON(X)
      PRINT 3,X,A
    3 FORMAT (10X,2HX=,E23.9,10X,7HDAWSON=,E23.9)
      X=X+0.5
      IF(X-20.0) 1,1,2
    2 STOP
      END
*EXECUTE
*
*
