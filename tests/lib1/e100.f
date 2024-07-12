      PROGRAM E100
         DIMENSION  F(11),A(11)
      DATA(A=0.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
         DO 100  I=1,10
      F(I)=SIN(A(I)*15.707963)
 100     CONTINUE
      PRINT 10,A,F
         B=0.35
         N=11
         CALL POLINT(F,A,N,B,R)
      PRINT 1,B,R
      B=0.735
      CALL POLINT(F,A,N,B,R)
      PRINT 1,B,R
      STOP
  1   FORMAT(20X,2HB=,F6.3,10X2HR=,F13.8/)
  10  FORMAT(//50X,9HTEST E100///20X,4HN=11//20X,2HA=,11(F6.1,1H,)//
     120X,2HF=,6(F13.8,1H,)/22X,5(F13.8,1H,))
          END
*EXECUTE
*
*
