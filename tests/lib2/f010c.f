      PROGRAM F010 C
      COMPLEX   A1(3,3),A2(3,3),A3(3,3),B1(3),B2(3),B3(3)
      DIMENSION R(3)
      DATA A1/2.,0.,1.,0.,3.,0.,1.,0.,-2.,0.,2.,0.,3.,0.,1.,0.,2.,0./,
     *     B1/9.,0.,-2.,0.,7.,0./
      DO 1 I=1,3
      DO 1 J=1,3
      A2(I,J)=A1(I,J)
 1    A3(I,J)=A1(I,J)
      DO 2 K=1,3
 2    B3(K)=B1(K)
      CALL CEQN(3,A1,3,R,IFAUL,1,B1)
      PRINT 3,B1,IFAUL
      CALL CINV(3,A2,3,R,IFAUL)
      PRINT 4,((A2(I,J),J=1,3),I=1,3)
      CALL CEQINV(3,A3,3,R,IFAUL,1,B3)
      PRINT 5,B3,IFAUL
      PRINT 6,((A3(I,J),J=1,3),I=1,3)
 3    FORMAT(1H1,'CEQN',1X,2HB=,6E17.10,2X,6HIFAUL=,I3/)
 4    FORMAT(/1X,'CINV',1X,6HA(3,3)/3(10X,6E17.10/))
 5    FORMAT(/1X, 'CEQINV',1X,2HB=,6E17.10,2X,6HIFAUL=,I3/)
 6    FORMAT(/1X,'CEQINV',1X,6HA(3,3)/3(10X,6E17.10/))
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
