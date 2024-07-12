      PROGRAM F119
      DIMENSION A(3),B(3)
      A(1)=4.
      A(2)=5.
      A(3)=6.
      B(1)=1.
      B(2)=2.
      B(3)=3.
      PRINT 10,A,B
      D=DIST(A,B)
      P=VMODUL(A)
      PRINT 1,D,P
   1  FORMAT(///20X,10HDIST(A,B)=,F10.3//20X,10HVMODUL(A)=,F10.3)
  10  FORMAT(///50X,9HTEST F119///20X,2HA=,3F10.3,10X,2HB=,3F10.3)
      END
*EXECUTE
*
*
