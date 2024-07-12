      PROGRAM C320
      COMPLEX Z(5),SN,CN,DN
      DIMENSION X(5),Y(5),AM(5)
      DATA X/.2,-0.2,5.,-15.,44./,Y/-2.,4.,15.,-44.,.2/,
     *     AM/.2,.4,.5,.7,.9/
      PRINT 5
      DO 1 I=1,5
      Z(I)=CMPLX(X(I),Y(I))
      CALL CELFUN(Z(I),AM(I),SN,CN,DN)
  1   PRINT 2,Z(I),AM(I),SN,CN,DN
  2   FORMAT(1X,3F7.1,1X,6E17.9)
  5   FORMAT(10X,1HZ,7X ,2HAM,20X,2HSN,31X,2HCN,31X,2HDN//)
      STOP
      END
*EXECUTE
*
*
