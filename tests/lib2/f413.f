       PROGRAM F413
      COMPLEX A,B,C,D,E,F,G,H,DET
      DIMENSION A(2,2),B(2,2),C(2,2),D(2,2),E(2,2),F(2,2),G(2,2),H(2,2),
     1V(2)
      DATA(A=1.,2.,3.,2.,-1.,-1.,1.,-1.),(E=-1.,0.,5.,0.,-0.5,0.,2.5,0.)
      DATA(B=1.,2.,3.,2.,-1.,-1.,1.,-1.),(F=-1.,0.,5.,0.,-0.5,0.,2.5,0.)
      DATA(C=1.,2.,3.,2.,-1.,-1.,1.,-1.),(G=-1.,0.,5.,0.,-0.5,0.,2.5,0.)
      DATA(D=1.,2.,3.,2.,-1.,-1.,1.,-1.),(H=-1.,0.,5.,0.,-0.5,0.,2.5,0.)
      PRINT 5,A,E
      CALL CMLIN(A,E,2,2,2,DET,V,0)
      PRINT 1,A,E,DET
      PRINT 6,B,F
      CALL CMLIN(B,F,2,2,2,DET,V,1)
      PRINT 2,B,F,DET
      PRINT 7,C,G
      CALL CMLIN(C,G,2,2,2,DET,V,2)
      PRINT 3,C,G,DET
      PRINT 8,D,H
      CALL CMLIN(D,H,2,2,2,DET,V,3)
      PRINT 4,D,H,DET
  1   FORMAT(30X,6HMODE=0//25X,2HA=,8F7.2/25X,2HE=,8F7.2/25X,4HDET=,
     12F7.2//)
  2   FORMAT(30X,6HMODE=1//25X,2HB=,8F7.2/25X,2HF=,8F7.2/25X,4HDET=,
     12F7.2//)
  3   FORMAT(30X,6HMODE=2//25X,2HC=,8F7.2/25X,2HG=,8F7.2/25X,4HDET=,
     12F7.2//)
  4   FORMAT(30X,6HMODE=3//25X,2HD=,8F7.2/25X,2HH=,8F7.2/25X,4HDET=,
     12F7.2//)
  5   FORMAT(30X,2HA=,8F7.2/30X,2HE=,8F7.2//)
  6   FORMAT (30X,2HB=,8F7.2/30X,2HF=,8F7.2//)
  7   FORMAT(30X,2HC=,8F7.2/30X,2HG=,8F7.2//)
  8   FORMAT(30X,2HD=,8F7.2/30X,2HH=,8F7.2//)
      END
*EXECUTE
*
*
