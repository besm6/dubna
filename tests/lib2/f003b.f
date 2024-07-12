      PROGRAM F003 B
      DIMENSION        X(2,3),Z(2,3),Y(2,3),D(2),U(3,3),V(3),N(19)
      DOUBLE PRECISION X,Z,Y,D,U,V,A,B,S,F,DMBIL
      DATA X/1.5D0,0.5D0,0.5D0,2.D0,0.2D0,0.4D0/,A,B,S/0.D0,1.D0,10.D0/
      DATA Y/1.5D0,1.5D0,0.2D0,0.7D0,0.5D0,0.1D0/,D/2.D0,3.D0/
      DATA U/1.D0,0.D0,0.D0,2.D0,4.D0,0.D0,3.D0,5.D0,0.D0/
      DATA V/1.D0,2.D0,3.D0/
      N(1)=5HDMSET
      CALL DMSET(2,3,S,Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(1),((Z(I,J),J=1,3),I=1,2)
      N(2)=5HDMRAN
      CALL DMRAN(2,3,A,B,Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(2),((Z(I,J),J=1,3),I=1,2)
      N(3)=5HDMCPY
      CALL DMCPY(2,3,X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(3),((Z(I,J),J=1,3),I=1,2)
      N(4)=5HDMUTL
      CALL DMUTL(3,U(1,1),U(1,2),U(2,1))
      PRINT 1,N(4),((U(K,L),L=1,3),K=1,3)
      N(5)=5HDMADD
      CALL DMADD(2,3,X(1,1),X(1,2),X(2,1),Y(1,1),Y(1,2),Y(2,1),Z(1,1),
     *           Z(1,2),Z(2,1))
      PRINT 1,N(5),((Z(I,J),J=1,3),I=1,2)
      N(6)=5HDMSUB
      CALL DMSUB(2,3,X(1,1),X(1,2),X(2,1),Y(1,1),Y(1,2),Y(2,1),Z(1,1),
     *           Z(1,2),Z(2,1))
      PRINT 1,N(6),((Z(I,J),J=1,3),I=1,2)
      N(7)=5HDRSCL
      CALL DRSCL(2,3,D(1),D(2),X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),
     *           Z(2,1))
      PRINT 1,N(7),((Z(I,J),J=1,3),I=1,2)
      N(8)=5HDMSCL
      CALL DMSCL(2,3,S,X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(8),((Z(I,J),J=1,3),I=1,2)
      N(9)=5HDMMPY
      CALL DMMPY(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(9),(Z(I),I=1,2)
      N(10)=5HDMMPA
      CALL DMMPA(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(10),(Z(I),I=1,2)
      N(11)=5HDMMPS
      CALL DMMPS(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(11),(Z(I),I=1,2)
      N(12)=5HDMMNA
      CALL DMMNA(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(12),(Z(I),I=1,2)
      N(13)=5HDMMNS
      CALL DMMNS(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(13),(Z(I),I=1,2)
      N(14)=5HDUMPY
      CALL DUMPY(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(14),(Z(J),J=1,3)
      N(15)=5HDUMPA
      CALL DUMPA(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(15),(Z(J),J=1,3)
      N(16)=5HDUMPS
      CALL DUMPS(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(16),(Z(J),J=1,3)
      N(17)=5HDUMNA
      CALL DUMNA(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(17),(Z(J),J=1,3)
      N(18)=5HDUMNS
      CALL DUMNS(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(18),(Z(J),J=1,3)
      N(19)=5HDMBIL
      F=DMBIL(3,V(1),V(2),U(1,1),U(1,2),U(2,1),Y(1),Y(2))
      PRINT 2,N(19),F
 1    FORMAT(15X,A6/(3X,3D25.16)/)
 2    FORMAT(15X,A6/ 3X, D25.16)
 3    FORMAT(15X,A6/(3X,2D25.16))
 4    FORMAT(15X,A6/(3X,3D25.16))
      CALL EXIT
      END
*EXECUTE
