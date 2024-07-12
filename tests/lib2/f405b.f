      PROGRAM F405 B
      DIMENSION A(2,2),B(2),X(2),C(2),WORK(20)
      LOGICAL LP,LF
      DATA A/2.,1.,1.,-2./,B/3.,1./,C/9.,-2/
      LF=.TRUE.
      LP=.TRUE.
      PRINT 1
      CALL LINSYS(A,2,2,B,X,IS,LF,LP,WORK)
      PRINT 2,X,IS
      LF=.FALSE.
      CALL LINSYS(A,2,2,C,X,IS,LF,LP,WORK)
      PRINT 2,X,IS
  1   FORMAT(50X,9HTEST F405///)
  2   FORMAT(10X,2HX=,2F20.10,10X,3HIS=,I2///)
      STOP
      END
*EXECUTE
*
*
