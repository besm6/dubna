      PROGRAM C306
      COMPLEX Z,Y,W,V,ZFACT,CGAMMA,CLOG
      PRINT 1
      Z=(1.2,0.2)
      Y=ZFACT(Z)
      W=CGAMMA(Z)
      PRINT 10,Z,Y,W
      V=CLOG(W)
      PRINT 20,V
      Z=(-0.5,0.5)
      Y=ZFACT(Z)
      W=CGAMMA(Z)
      PRINT 10,Z,Y,W
      V=CLOG(W)
      PRINT 20,V
 1    FORMAT(//50X'TEST C306'///)
  10  FORMAT(30X'Z=('F7.3','F7.3')'///30X'ZFACT(Z)=('E20.13','E20.13')'/
     1/30X'CGAMMA(Z)=('E20.13','E20.13')'/)
  20  FORMAT(30X'CLOG(W)=('E20.13','E20.13')'10X'W=CGAMMA(X)'///)
      END
*EXECUTE
*
*
