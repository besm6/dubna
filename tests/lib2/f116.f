       PROGRAM F116
        DIMENSION A(4),B(4)
         DATA A/1.5,0.8,2.0,2.5/
        DATA B/1.0,2.0,3.0,3.5/
      PRINT 10
         P1=DOT(A,B)
        P2=DOT4(A,B)
        P3=DOTI(A,B)
         P4=DOTNOR(A,B)
         PRINT 2,P1,P2,P3,P4
      STOP
   2  FORMAT(15X,8HDOT(A,B),11X,9HDOT4(A,B),11X,9HDOTI(A,B),10X,
     1     11HDOTNOR(A,B)//5X,4E20.11)
  10  FORMAT(///50X,9HTEST F116//40X,23HA=1.5 , 0.8 , 2.0 , 2.5//40X,
     1      23HB=1.0 , 2.0 , 3.0 , 3.5///)
        END
*EXECUTE
*
*
