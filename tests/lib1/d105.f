      PROGRAM D105
      DIMENSION R(2)
      COMMON FN
      EXTERNAL PHI
      PRINT 10
      DO 1  N=1,5
      FN=N
      DO 2  M=1,2
      FM=M
  2   R(M)=2.0*FN*TRIINT(PHI,1,64,1.E-7,0.,0.,1.,0.,1.,FM)
    1 PRINT 3,N,R(1),R(2)
  3   FORMAT(30X,I5,2F20.10/)
  10  FORMAT(1H1//50X,9HTEST D105///35X,1HN,10X,3HM=1,20X,3HM=2,20X,12HE
     1PSILN=1.E-7///)
      STOP
       END
      FUNCTION PHI(X,Y)
       COMMON FN
       PHI=X*X*SIN(FN*X*Y)
      RETURN
      END
*EXECUTE
*
*
