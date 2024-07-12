      PROGRAM  C101
      DIMENSION A(6) ,B(6) , IM(6) , X(6,6) , ISI(2)
      DATA ((A(I),I=1,6)=1.,-3.,-5.,15.,4.,-12.)
      DATA (M=6),(T=-3.),(N=4)
      T1=5.
      PRINT 1
      DO 10 K=1,5
      T=T+1.         $  R= T
      PRINT 20, (A(I), I = 1,6) , T , T1
      DO 110 I= 1,2
      CALL CHAIM(M,A,B,IRE,IM,X)
      PRINT 30,(B(J),J=1,6),IRE,(IM(J),J=1,6),((X(L,J),J=1,6),L=1,6)
      CALL STURM(R,M,B,IRE,IM,X,IS)
      PRINT  40,R,IS     $  R=5.
  110 ISI(I) = IS  $ R=5.
      NUMB=ISI(2)-ISI(1)
      PRINT 50,NUMB
   10 CONTINUE
C  *********************************************
      B(1)=1. $ B(2)=-3. $ B(3)=-4. $ B(4)=12. $ B(5)=.0
      PRINT 60,(A(I),I=1,6),(B(I),I=1,4)
C      *****END OF STURM
      CALL EUKLID(M,A,N,B,IRE,IM,X)
      PRINT 70,IRE,(IM(I),I=1,6),((X(I,J),J=1,6),I=1,6)
      PRINT 80
   20 FORMAT(12H POLYNOM A= ,6F6.1//4H T0=,F4.1,4H T1=,F3.1)
   30 FORMAT( 9H ARRAY B ,6F6.2//5H IRE=,I3//4H IM=,6I6//
     *26H MATRIX POLYNOMIALS X(I,J)//(6F6.2))
   40 FORMAT(3H T=,F6.1,21H INDEX STURM AT T ID=,I3)
   50 FORMAT(58H NUMBER OF ZEROS OF POLYNOMIAL A BETWEEN T0 AND T1 NUM
     *B=,I3)
   60 FORMAT(52H DETERMINATION OF THE G.C.D. OF TWO POLYNOMIALS A, B/
     *//3H A=,6F6.1,17H      DEGREE M= 6//
     *3H B=,4F6.1,29H                  DEGREE N= 4)
  70  FORMAT(5H IRE=,I3/4H IM=,6I6  //26H MATRIX POLYNOMIALS X(I,J)//
     *(6F6.2))
   80 FORMAT(31H END EXECUTION OF PROGRAM STURM)                       .
  1   FORMAT(1H1,36X'PROGRAM TO COMPUTE STURM CHAIN AND STURM INDEX'27X,
     1'TEST C101'/37X,46(1H*),27X,9(1H*)//)
      END
*EXECUTE
*
*
