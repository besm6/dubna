      PROGRAM F143
      DIMENSION X1(3),X2(3),X3(3),X(3),G1(9),G2(9),G3(9),G(9),G12(9),
     MG13(9),G23(9),XN(9),GN(9),GU1(3)
       DATA((X1(I1),I1=1,3)=1.,2.,3.)
      DATA((G1(I),I=1,9)=1.,5.,6.,5.,2.,7.,6.,7.,3.)
      DATA((GU1(IU),IU=1,3)=2.,4.,5.)
       GU=2.0
      U=5.0
      CALL ERSCAL(U,X1,X,GU,G1,GU1,G)
       PRINT 115,U
       PRINT 101,(X1(I1),I1=1,3)
       PRINT 104,(X(J),J=1,3)
       PRINT 114,GU
      PRINT 105,(G1(I2),I2=1,9)
       PRINT 116,(GU1(K),K=1,3)
      PRINT 108,(G(J),J=1,9)
  101 FORMAT(15X,5HX1(I),//,3F10.3,//)
  104 FORMAT(15X,5HX (I),//,3F10.3,//)
  105 FORMAT(15X,5HG1(I),//,3F10.3,//)
  108 FORMAT(15X,5HG (I),//,3F10.3,//)
  114 FORMAT(10X,2HGU,//,5X,F15.5,//)
  115 FORMAT(10X,2H U,//,5X,F15.5,//)
 116  FORMAT(10X,6HGU1(I),//,5X,3F15.5,//)
       END
*EXECUTE
*
*
