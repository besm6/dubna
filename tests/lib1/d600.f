      PROGRAM D600
      DIMENSION Q(16,17)
      EXTERNAL F,G,H
      PRINT 20
      PI=3.1415926
      A=0.0 $ B=PI/2.0
      N=15 $ NQDIM=16
      R=FRED2(F,G,X)
      R=FRED3(F,G,X)
      CALL FRED1(F,G,A,B,N,Q,NQDIM)
      X=0.0
      DO 1 IR=1,20
      R=FRED2(F,G,X)
      S=FRED3(H)
C
      R1=(PI**2/(PI-1.0))*SIN(X)**2+2.0*X-PI
      PRINT 10,X,R,R1,S
 1     X=X+0.1
  10  FORMAT(20X,F5.1,3F20.9)
  20  FORMAT(//50X,9HTEST D600///23X,1HX,10X,5HFRED2,15X,2HR1,20X,5HFRED
     13)
      END
      FUNCTION F(X)
      F=2.0*X-3.1415926
      RETURN
      END
      FUNCTION G(X,T)
      G=4*(SIN(X)**2)
      RETURN
      END
      FUNCTION H(T)
      H=2.5*T**2
      RETURN
      END
*EXECUTE
*
*
