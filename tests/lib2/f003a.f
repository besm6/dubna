      PROGRAM F003 A
      DIMENSION X(2,3),Z(2,3),Y(2,3),D(2),U(3,3),V(3),N(19)
      DATA X/1.5,0.5,0.5,2.,0.2,0.4/,Y/1.5,1.5,0.2,0.7,0.5,0.1/
      DATA A,B/0.,1./,D/2.,3./,U/1.,0.,0.,2.,4.,0.,3.,5.,0./,S/10./
      DATA V/1.,2.,3./
      N(1)=5HRMSET
      CALL RMSET(2,3,S,Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(1),((Z(I,J),J=1,3),I=1,2)
      N(2)=5HRMRAN
      CALL RMRAN(2,3,A,B,Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(2),((Z(I,J),J=1,3),I=1,2)
      N(3)=5HRMCPY
      CALL RMCPY(2,3,X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(3),((Z(I,J),J=1,3),I=1,2)
      N(4)=5HRMUTL
      CALL RMUTL(3,U(1,1),U(1,2),U(2,1))
      PRINT 1,N(4),((U(K,L),L=1,3),K=1,3)
      N(5)=5HRMADD
      CALL RMADD(2,3,X(1,1),X(1,2),X(2,1),Y(1,1),Y(1,2),Y(2,1),Z(1,1),
     *           Z(1,2),Z(2,1))
      PRINT 1,N(5),((Z(I,J),J=1,3),I=1,2)
      N(6)=5HRMSUB
      CALL RMSUB(2,3,X(1,1),X(1,2),X(2,1),Y(1,1),Y(1,2),Y(2,1),Z(1,1),
     *           Z(1,2),Z(2,1))
      PRINT 1,N(6),((Z(I,J),J=1,3),I=1,2)
      N(7)=5HRRSCL
      CALL RRSCL(2,3,D(1),D(2),X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),
     *           Z(2,1))
      PRINT 1,N(7),((Z(I,J),J=1,3),I=1,2)
      N(8)=5HRMSCL
      CALL RMSCL(2,3,S,X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(8),((Z(I,J),J=1,3),I=1,2)
      N(9)=5HRMMPY
      CALL RMMPY(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(9),(Z(I),I=1,2)
      N(10)=5HRMMPA
      CALL RMMPA(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(10),(Z(I),I=1,2)
      N(11)=5HRMMPS
      CALL RMMPS(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(11),(Z(I),I=1,2)
      N(12)=5HRMMNA
      CALL RMMNA(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(12),(Z(I),I=1,2)
      N(13)=5HRMMNS
      CALL RMMNS(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(13),(Z(I),I=1,2)
      N(14)=5HRUMPY
      CALL RUMPY(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(14),(Z(J),J=1,3)
      N(15)=5HRUMPA
      CALL RUMPA(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(15),(Z(J),J=1,3)
      N(16)=5HRUMPS
      CALL RUMPS(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(16),(Z(J),J=1,3)
      N(17)=5HRUMNA
      CALL RUMNA(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(17),(Z(J),J=1,3)
      N(18)=5HRUMNS
      CALL RUMNS(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(18),(Z(J),J=1,3)
      N(19)=5HRMBIL
      F=RMBIL(3,V(1),V(2),U(1,1),U(1,2),U(2,1),Y(1),Y(2))
      PRINT 2,N(19),F
 1    FORMAT(15X,A6/(3X,3E20.10)/)
 2    FORMAT(15X,A6/ 3X, E20.10)
 3    FORMAT(15X,A6/(3X,2E20.10))
 4    FORMAT(15X,A6/(3X,3E20.10))
      CALL EXIT
      END
*EXECUTE
