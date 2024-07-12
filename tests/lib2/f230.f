      PROGRAM F230
      DIMENSION A(3,3),B(4,4),C(10),EIG(3),EIGV(4)
      DATA A/8.,-4.,18.,-1.,4.,-5.,-5.,-2.,-7./,EIG/2.,-1.,-1./
      DATA B/5.,4.,1.,1.,4.,5.,1.,1.,1.,1.,4.,2.,1.,1.,2.,4./
      DATA C/5.,4.,5.,1.,1.,4.,1.,1.,2.,4./,EIGV/-1.,-1.,2.,2./
      CALL DEFL(A,3,3,1.,EIG)
      CALL DEFLS(B,4,4,5.,EIGV)
      CALL DEFLSS(C,4,5.,EIGV)
      PRINT 1,A
      PRINT 2,B
      PRINT 3,C
  1   FORMAT(/30X,13HTEST FOR DEFL/3(2X,3E20.11/))
  2   FORMAT(/30X,14HTEST FOR DEFLS/4(2X,4E20.11/))
  3   FORMAT(/30X,15HTEST FOR DEFLSS/2(2X,5E20.11/))
      STOP
      END
*EXECUTE
*
*
