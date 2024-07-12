      PROGRAM E207
      DIMENSION A(100),B(100)
      PRINT 10
      N=100
      M=100
C                                                IOP=3
      IOP=3
      PRINT 15
      AN=0.
      DO1 I=1,100
      AN=AN+1.
      B(I)=1./AN
 1    CONTINUE
      PRINT 16,B
      X=-2.5
      DO 20 J=1,10
      X=X+0.5
      F=TRISUM(X,A,N,B,M,IOP)
      PRINT 50,X,F
  20  CONTINUE
C
      AN=0.
      DO2I=1,100
      AN=AN+1.
      B(I)=2.*((-1.)**IFIX(AN+1.)/AN)
 2    CONTINUE
      PRINT 16,B
      X=-2.5
      DO 25 J=1,10
      X=X+0.5
      F=TRISUM(X,A,N,B,M,IOP)
      PRINT 50,X,F
  25  CONTINUE
C                                                IOP=2
      IOP=2
      PRINT 17
      AN=0.
      DO3I=2,100
      AN=AN+1.
      A(I)=4.*((-1.)**IFIX(AN+1.)/AN**2)
 3    CONTINUE
      PRINT 18,A
      DO4 I=1,100
      B(I)=0.
 4    CONTINUE
      A(1)=4./3.*9.8696
      X=-2.5
      DO 30 J=1,10
      X=X+0.5
      F=TRISUM(X,A,N,B,M,IOP)
      PRINT 50,X,F
  30  CONTINUE
C
      AN=0.
      DO 5 I=1,100
      AN=AN+1.
      A(I)=4.*((-1.)**IFIX(AN)/AN**2)
 5    CONTINUE
      A(1)=2./3.*9.8696
      PRINT 18,A
      X=-2.5
      DO 35 J=1,10
      X=X+0.5
      F=TRISUM(X,A,N,B,M,IOP)
      PRINT 50,X,F
  35  CONTINUE
C                                                IOP=1
      IOP=1
      PRINT 19
      AN=0.
      DO 6 I=2,100
      AN=AN+1.
      B(I)=2.*((-1.)**IFIX(AN+1.)/AN)
 6    CONTINUE
      PRINT 40,A,B
      X=-2.5
      DO 45 J=1,10
      X=X+0.5
      F=TRISUM(X,A,N,B,M,IOP)
      PRINT 50,X,F
  45  CONTINUE
C
  10  FORMAT(///50X,9HTEST E207///)
  15  FORMAT(5X,3HA=0,5X,5HIOP=3/5X,13(1H-)//)
  16  FORMAT(5X,2HB=/10(5X,10E11.3/)//10X,1HX,20X,1HY/)
  17  FORMAT(5X,3HB=0,5X,5HIOP=2/5X,13(1H-)//)
  18  FORMAT(5X,2HA=/10(5X,10E11.3/)//10X,1HX,20X,1HY/)
  19  FORMAT(5X,5HIOP=1/5X,5(1H-)//)
  40  FORMAT(5X,2HA=/10(5X,10E11.3/)//5X,2HB=/10(5X,10E11.3/)//10X,1HX,
     1      20X,1HY/)
  50  FORMAT(10X,F4.1,10X,E20.11)
      STOP
      END
*EXECUTE
*
*
