      PROGRAM   F121
      DIMENSION A(3),X(3),W(3),G(3,3),H(3,3),EX(3),B(3)
      DIMENSION Q(3),JA(3),IA(9),T(9),NN(32)
      DATA NN/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
     *23,24,25,26,27,28,29,30,31,32/
      DATA A/-5.,1.,0./
      DATA B/1.,4.,2./
      DATA G/1.,2.,3.,4.,2.,1.,3.,2.,5./
      DATA H/1.,3.,2.,4.,6.,1.,7.,2.,5./
      DATA Q/5.2,1.5,3.4/
      DATA JA/1,2,3/
      ALPHA=6.
      BETA=8.
      N=3
      K=3
      PRINT 10,A,B,G,H,Q,JA
      CALL VADD (A,B,X,3)
      PRINT 5,X,NN(1)
      CALL VSUB (A,B,X,3)
      PRINT 5,X,NN(2)
      CALL VMUL (A,B,X,N)
      PRINT 5,X,NN(3)
      CALL VBIAS (A,ALPHA,X,N)
      PRINT 5,X,NN(4)
      CALL VSCALE (A,ALPHA,X,N)
      PRINT 5,X,NN(5)
      CALL VLINE (A,ALPHA,B,BETA,X,N)
      PRINT 5,X,NN(6)
      CALL VUNIT (A,X,N)
      PRINT 5,X,NN(7)
      CALL VMATR (A,G,W,N,K)
      PRINT 5,W,NN(8)
      CALL VMATL (H,A,W,K,N)
      PRINT 5,W,NN(9)
      CALL VFILL (X,N,ALPHA)
      PRINT 5,X,NN(10)
      CALL VZERO(X,N)
      PRINT 5,X,NN(11)
      CALL VBLANK (X,N)
      PRINT 20,X,NN(12)
      CALL VEXCUM (A,EX,N)
      PRINT 5,EX,NN(13)
      Y=VDOT(A,B,N)
      PRINT 7,Y,NN(14)
      Y = VDOTN (A,B,N)
      PRINT 7,Y,NN(15)
      Y = VMOD (A,N)
      PRINT 7,Y,NN(16)
      Y = VASUM (A,N)
      PRINT 7,Y,NN(17)
      Y = VSUM (A,N)
      PRINT 7,Y,NN(18)
      Y = VMAXA (A,N)
      PRINT 7,Y,NN(19)
      Y = VMAX (A,N)
      PRINT 7,Y,NN(20)
      Y = VMINA (A,N)
      PRINT 7,Y,NN(21)
      Y = VMIN (A,N)
      PRINT 7,Y,NN(22)
      I = LVMAXA (A,N)
      PRINT 6,I,NN(23)
      I = LVMAX (A,N)
      PRINT 6,I,NN(24)
      I = LVMINA (A,N)
      PRINT 6,I,NN(25)
      I = LVMIN (A,N)
      PRINT 6,I,NN(26)
      Y=VDOTN2(A,B,N)
      PRINT 7,Y,NN(27)
      CALL VFLOAT(JA,T,N)
      PRINT 8,T,NN(28)
      CALL VFIX(Q,IA,N)
      PRINT 9,IA,NN(29)
      CALL VCOPYN(A,X,N)
      PRINT 5,X,NN(30)
      Y=VDIST2(A,B,N)
      PRINT 7,Y,NN(31)
      Y=VDIST(A,B,N)
      PRINT 7,Y,NN(32)
  5   FORMAT(3X,3E20.11,10X,3HNN ,I2)
  6   FORMAT(3X,I3,67X,3HNN ,I2)
  7   FORMAT(3X,E20.11,50X,3HNN ,I2)
  8   FORMAT(2(/5X,3E20.11)/5X,3E20.11, 8X,3HNN ,I2)
  9   FORMAT(2(/10X,3I5),/10X,3I5,48X,3HNN ,I2)
  10  FORMAT(///50X,9HTEST F121///20X,2HA=,3F10.3/20X,2HB=,3F10.3/
     120X,2HG=,9F10.3/20X,2HH=,9F10.3/20X,2HQ=,3F10.3/20X,3HJA=,3I4,
     210X,3HN=3,10X,3HK=3,20X,8HALPHA=6.,10X,7HBETA=8.///)
  20  FORMAT(3X,3A6,52X,3HNN ,I2)
      END
*CALL PTIME
*EXECUTE
*
*
