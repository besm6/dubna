      PROGRAM C341
      X=0.5
      PRINT 4
  1    A=ALOGAM(X)
      PRINT 3,X,A
      X=X+0.5
      IF(X-20.0) 1,1,2
  2   X=-1.
      A=ALOGAM(X)
      PRINT 3,X,A
    3 FORMAT (5X,2HX=,E23.9,10X,7HALOGAM=,E23.9)
    4 FORMAT(////,20X,26H=TEST LI SANG HO= 4,4,1972,///)
      END
*CALL PTIME
*EXECUTE
*
*
