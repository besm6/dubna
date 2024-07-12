      PROGRAM C206 C
       COMPLEX A(10),ROOT(10),X,F
       DIMENSION H(10)
       N=4
       A(1)=(1.,0.)
       A(2)=CMPLX(-8.,0.)
       A(3)=12.*CMPLX(0.117038E-2,-0.1709)
       A(4)=CMPLX(8.,0.)
       A(5)=CMPLX(-1.,0.)
       NP=N+1
       PRINT 100,(A(I),I=1,NP)
100    FORMAT(///' A='/ (5X,2E20.10))
       CALL POLY2(A,N,ROOT,H,2500,0)
       DO 200 L=1,N
       X=ROOT(L)
       F=A(1) * X + A(2)
       DO 201 M=3,NP
201    F=F * X + A(M)
       PRINT 103,X,F
200    CONTINUE
103    FORMAT(3X,2HX=,2E20.10,5X,5HF(X)=,2E20.10)
       PRINT 102,(H(I),I=1,N)
102    FORMAT(' H=',4E25.10//)
       END
*CALL PTIME
*EXECUTE
*
*
