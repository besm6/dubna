      PROGRAM F003 C
      DIMENSION        X(2,3),Z(2,3),Y(2,3),D(2),U(3,3),V(3),N(19)
      COMPLEX          X,Z,Y,D,U,V,A,B,S,F,CMBIL
      DATA X/1.5,1.,0.5,2.,0.5,2.,2.,0.,0.2,0.5,0.4,0./,D/2.,1.,3.,0.5/
      DATA Y/1.5,1.,1.5,0.5,0.2,01.,0.7,1.,0.5,0.,0.1,0.5/
      DATA U/1.,1.,0.,0.,0.,0.,2.,0.1,4.,0.5,0.,0.,3.,0.,5.,0.5,0.,0./
      DATA V/1.,1.,2.,0.5,3.,1./,A,B,S/0.,0.,1.,0.,10.,0./
      N(1)=5HCMSET
      CALL CMSET(2,3,S,Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(1),((Z(I,J),J=1,3),I=1,2)
      N(2)=5HCMRAN
      CALL CMRAN(2,3,A,B,Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(2),((Z(I,J),J=1,3),I=1,2)
      N(3)=5HCMCPY
      CALL CMCPY(2,3,X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(3),((Z(I,J),J=1,3),I=1,2)
      N(4)=5HCMUTL
      CALL CMUTL(3,U(1,1),U(1,2),U(2,1))
      PRINT 1,N(4),((U(K,L),L=1,3),K=1,3)
      N(5)=5HCMADD
      CALL CMADD(2,3,X(1,1),X(1,2),X(2,1),Y(1,1),Y(1,2),Y(2,1),Z(1,1),
     *           Z(1,2),Z(2,1))
      PRINT 1,N(5),((Z(I,J),J=1,3),I=1,2)
      N(6)=5HCMSUB
      CALL CMSUB(2,3,X(1,1),X(1,2),X(2,1),Y(1,1),Y(1,2),Y(2,1),Z(1,1),
     *           Z(1,2),Z(2,1))
      PRINT 1,N(6),((Z(I,J),J=1,3),I=1,2)
      N(7)=5HCRSCL
      CALL CRSCL(2,3,D(1),D(2),X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),
     *           Z(2,1))
      PRINT 1,N(7),((Z(I,J),J=1,3),I=1,2)
      N(8)=5HCMSCL
      CALL CMSCL(2,3,S,X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(8),((Z(I,J),J=1,3),I=1,2)
      N(9)=5HCMMPY
      CALL CMMPY(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(9),(Z(I),I=1,2)
      N(10)=5HCMMPA
      CALL CMMPA(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(10),(Z(I),I=1,2)
      N(11)=5HCMMPS
      CALL CMMPS(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(11),(Z(I),I=1,2)
      N(12)=5HCMMNA
      CALL CMMNA(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(12),(Z(I),I=1,2)
      N(13)=5HCMMNS
      CALL CMMNS(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(13),(Z(I),I=1,2)
      N(14)=5HCUMPY
      CALL CUMPY(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(14),(Z(J),J=1,3)
      N(15)=5HCUMPA
      CALL CUMPA(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(15),(Z(J),J=1,3)
      N(16)=5HCUMPS
      CALL CUMPS(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(16),(Z(J),J=1,3)
      N(17)=5HCUMNA
      CALL CUMNA(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(17),(Z(J),J=1,3)
      N(18)=5HCUMNS
      CALL CUMNS(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(18),(Z(J),J=1,3)
      N(19)=5HCMBIL
      F=CMBIL(3,V(1),V(2),U(1,1),U(1,2),U(2,1),Y(1),Y(2))
      PRINT 2,N(19),F
 1    FORMAT(15X,A6/(3X,6E15.7)/)
 2    FORMAT(15X,A6/ 3X,2E20.10)
 3    FORMAT(15X,A6/(3X,4E15.7))
 4    FORMAT(15X,A6/(3X,6E15.7))
      STOP
      END
*EXECUTE
*
*
