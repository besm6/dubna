      PROGRAM F010 A
      DIMENSION A1(3,3),A2(3,3),A3(3,3),B1(3),B2(3),B3(3),R(3)
      DATA A1/2.,1.,3.,1.,-2.,2.,3.,1.,2./,B1/9.,-2.,7./
      DO 1 I=1,3
      DO 1 J=1,3
      A2(I,J)=A1(I,J)
 1    A3(I,J)=A1(I,J)
      DO 2 K=1,3
 2    B3(K)=B1(K)
      CALL REQN(3,A1,3,R,IFAUL,1,B1)
      PRINT 3,B1,IFAUL
      CALL RINV(3,A2,3,R,IFAUL)
      PRINT 4,((A2(I,J),J=1,3),I=1,3)
      CALL REQINV(3,A3,3,R,IFAUL,1,B3)
      PRINT 5,B3,IFAUL
      PRINT 6,((A3(I,J),J=1,3),I=1,3)
 3    FORMAT(1H1,1X,'REQN'/1X,2HB=,3E20.11,5X,6HIFAUL=,I3/)
 4    FORMAT(/1X,'RINV',1X,6HA(3,3)/3(10X,3E20.10/))
 5    FORMAT(/1X,'REQINV'/1X,2HB=,3E20.11,5X,6HIFAUL=,I3/)
 6    FORMAT(/1X,'REQINV',1X,6HA(3,3)/3(10X,3E20.10/))
      CALL EXIT
      END
*CALL PTIME
*EXECUTE
