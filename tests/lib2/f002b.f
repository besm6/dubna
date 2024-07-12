      PROGRAM F002 B
      DIMENSION X(6),Z(3),N(16)
      DOUBLE PRECISION X,Z,S,F,DVMPY,DVMPA,DVSUM
      DATA X/-5.D0,1.D0,1.D0,4.D0,0.D0,2.D0/
      S=10.D0
      N(1)=5HDVSET
      CALL DVSET(3,S,Z(1),Z(2))
      PRINT 1,N(1),Z
      N(2)=5HDVRAN
      CALL DVRAN(3,0.D0,1.D0,Z(1),Z(2))
      PRINT 1,N(2),Z
      N(3)=5HDVCPY
      CALL DVCPY(3,X(1),X(3),Z(1),Z(2))
      PRINT 1,N(3),Z
      N(4)=5HDVXCH
      CALL DVXCH(3,X(1),X(3),X(2),X(4))
      PRINT 2,N(4),X
      N(5)=5HDVADD
      CALL DVADD(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(5),Z
      N(6)=5HDVSUB
      CALL DVSUB(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(6),Z
      N(7)=5HDVMUL
      CALL DVMUL(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(7),Z
      N(8)=6HDVMULA
      CALL DVMULA(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(8),Z
      N(9)=6HDVMUNA
      CALL DVMUNA(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(9),Z
      N(10)=5HDVDIV
      CALL DVDIV(3,X(1),X(3),X(2),X(4),Z(1),Z(2),IFAIL)
      PRINT 4,N(10),Z,IFAIL
      N(11)=5HDVSCL
      CALL DVSCL(3,S,X(1),X(2),Z(1),Z(2))
      PRINT 1,N(11),Z
      N(12)=5HDVSCA
      CALL DVSCA(3,S,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(12),Z
      N(13)=5HDVSCS
      CALL DVSCS(3,S,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(13),Z
      N(14)=5HDVSUM
      F=DVSUM(3,X(1),X(3))
      PRINT 3,N(14),F
      N(15)=5HDVMPY
      F=DVMPY(3,X(1),X(3),X(2),X(4))
      PRINT 3,N(15),F
      N(16)=5HDVMPA
      F=DVMPA(3,X(1),X(3),X(2),X(4),S)
      PRINT 3,N(16),F
 1    FORMAT(3X,A6,10X,2HZ=,3D30.17)
 2    FORMAT(3X,A6,5X,2HX=,6D17.9)
 3    FORMAT(3X,A6,10X,2HF=,D30.17)
 4    FORMAT(3X,A6,10X,2HZ=,3D25.17,5X,6HIFAIL=,I2)
      CALL EXIT
      END
*EXECUTE
