      PROGRAM G106
      DIMENSION P(1000),N2(1000),U(1000),P1(1000),P2(1000)
      DATA L/0/
      DATA((U(N),N=1,30)=0.455,0.713,1.005,1.06,1.14,1.13,1.24,2.03,
     A4.86,7.0,9.0,12.3,16.2,14.3,18.4,16.3,14.4,13.7,12.7,12.4,11.6,
     B10.6,10.2,12.0,14.6,17.3,20.7,23.6,28.3,33.5)
      DO 1 K=1,10
      T=0.
      DO 1 M=1,50
      L=L+1
      N2(L)=L
      IF(N2(L).LE.30)GO TO 4
       U(L)=FLOAT(N2(L))* RND(T)
   4  P(L)=PROB(U(L),N2(L))
      P1(L)=1.0-GAMDIS(U(L)/2.,FLOAT(N2(L))/2.)
      P2(L)=(P1(L)-P(L))/P(L)
    1 CONTINUE
      PRINT 10,(P2(J),J=1,500)
  10  FORMAT((2X,5E20.10)/)
      END
      FUNCTION RND(T)
       DIMENSION U(2)
      IF(T)1,2,1
   2  U(1)=3.14159265
      U(2)=0.542101887
    1 T=U(1)+U(2)
      U(1)=U(2)
        IF(T-4.)3,4,4
  4   T=T-4.
  3   U(2)=T
      RND=T/4.
       RETURN
       END
*EXECUTE
*
*
