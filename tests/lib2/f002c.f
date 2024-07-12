      PROGRAM F002 C
      DIMENSION X(6),Z(3),N(16)
         COMPLEX   X,Z,S,A,B,F,CVMPY,CVMPA,CVSUM
      DATA X/-5.,1.,1.,2.,1.,1.,4.,4.,0.,1.,2.,3./,  S/10.,0./
      DATA A/0.,0./, B/1.,0./
      N(1)=5HCVSET
      CALL CVSET(3,S,Z(1),Z(2))
      PRINT 1,N(1),Z
      N(2)=5HCVRAN
      CALL CVRAN(3,A,B,Z(1),Z(2))
      PRINT 1,N(2),Z
      N(3)=5HCVCPY
      CALL CVCPY(3,X(1),X(3),Z(1),Z(2))
      PRINT 1,N(3),Z
      N(4)=5HCVXCH
      CALL CVXCH(3,X(1),X(3),X(2),X(4))
      PRINT 2,N(4),X
      N(5)=5HCVADD
      CALL CVADD(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(5),Z
      N(6)=5HCVSUB
      CALL CVSUB(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(6),Z
      N(7)=5HCVMUL
      CALL CVMUL(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(7),Z
      N(8)=6HCVMULA
      CALL CVMULA(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(8),Z
      N(9)=6HCVMUNA
      CALL CVMUNA(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(9),Z
      N(10)=5HCVDIV
      CALL CVDIV(3,X(1),X(3),X(2),X(4),Z(1),Z(2),IFAIL)
      PRINT 4,N(10),Z,IFAIL
      N(11)=5HCVSCL
      CALL CVSCL(3,S,X(1),X(2),Z(1),Z(2))
      PRINT 1,N(11),Z
      N(12)=5HCVSCA
      CALL CVSCA(3,S,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(12),Z
      N(13)=5HCVSCS
      CALL CVSCS(3,S,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(13),Z
      N(14)=5HCVSUM
      F=CVSUM(3,X(1),X(3))
      PRINT 3,N(14),F
      N(15)=5HCVMPY
      F=CVMPY(3,X(1),X(3),X(2),X(4))
      PRINT 3,N(15),F
      N(16)=5HCVMPA
      F=CVMPA(3,X(1),X(3),X(2),X(4),S)
      PRINT 3,N(16),F
 1    FORMAT(3X,A6,10X,2HZ=,6E14.7)
 2    FORMAT(1X,A6,1X,2HX=,12E9.2)
 3    FORMAT(3X,A6,10X,2HF=,2E14.7)
 4    FORMAT(3X,A6,10X,2HZ=,6E14.7,3X,6HIFAIL=,I2)
      STOP
      END
*EXECUTE
*
*
