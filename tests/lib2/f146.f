      PROGRAM F146
      DIMENSION X1(3),XN(3),G1(9),GN(9)
       DATA((X1(I1),I1=1,3)=1.,2.,3.)
      DATA((G1(I),I=1,9)=1.,5.,6.,5.,2.,7.,6.,7.,3.)
       CALL ERNORM(X1,XN,G1,GN)
       PRINT 101,(X1(I1),I1=1,3)
       PRINT 112,(XN(I3),I3=1,3)
      PRINT 105,(G1(I2),I2=1,9)
       PRINT 113,(GN(J),J=1,9)
  101 FORMAT(15X,5HX1(I),//,3F10.3,//)
  105 FORMAT(15X,5HG1(I),//,3F10.3,//)
  112 FORMAT(15X,5HXN(I),//,3F10.3,//)
  113 FORMAT(15X,5HGN(I),//,3F10.3,//)
      END
*EXECUTE
*
*
