      PROGRAM F118
      DIMENSION A(3),B(3)
      A(1)=1.
      A(2)=1.5
      A(3)=0.5
      PRINT 10
      CALL ROT(A,0.5,B)
      PRINT 1,A,B
   1  FORMAT(20X,2HA=,3F10.3,10X,6HTH=0.5//20X,2HB=,3F10.3)
  10  FORMAT(///50X,9HTEST F118///)
      END
*EXECUTE
*
*
