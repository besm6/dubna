*
      PROGRAM F002 A
      DIMENSION X(6),Z(3),N(16)
      DATA X/-5.,1.,1.,4.,0.,2./
      S=10.
      N(1)=5HRVSET
      CALL RVSET(3,S,Z(1),Z(2))
      PRINT 1,N(1),Z
      N(2)=5HRVRAN
      CALL RVRAN(3,0.,1.,Z(1),Z(2))
      PRINT 1,N(2),Z
      N(3)=5HRVCPY
      CALL RVCPY(3,X(1),X(3),Z(1),Z(2))
      PRINT 1,N(3),Z
      N(4)=5HRVXCH
      CALL RVXCH(3,X(1),X(3),X(2),X(4))
      PRINT 2,N(4),X
      N(5)=5HRVADD
      CALL RVADD(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(5),Z
      N(6)=5HRVSUB
      CALL RVSUB(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(6),Z
      N(7)=5HRVMUL
      CALL RVMUL(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(7),Z
      N(8)=6HRVMULA
      CALL RVMULA(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(8),Z
      N(9)=6HRVMUNA
      CALL RVMUNA(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(9),Z
      N(10)=5HRVDIV
      CALL RVDIV(3,X(1),X(3),X(2),X(4),Z(1),Z(2),IFAIL)
      PRINT 4,N(10),Z,IFAIL
      N(11)=5HRVSCL
      CALL RVSCL(3,S,X(1),X(2),Z(1),Z(2))
      PRINT 1,N(11),Z
      N(12)=5HRVSCA
      CALL RVSCA(3,S,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(12),Z
      N(13)=5HRVSCS
      CALL RVSCS(3,S,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(13),Z
      N(14)=5HRVSUM
      F=RVSUM(3,X(1),X(3))
      PRINT 3,N(14),F
      N(15)=5HRVMPY
      F=RVMPY(3,X(1),X(3),X(2),X(4))
      PRINT 3,N(15),F
      N(16)=5HRVMPA
      F=RVMPA(3,X(1),X(3),X(2),X(4),S)
      PRINT 3,N(16),F
 1    FORMAT(3X,A6,10X,2HZ=,3E20.11)
 2    FORMAT(3X,A6,5X,2HX=,6E17.9)
 3    FORMAT(3X,A6,10X,2HF=,E20.11)
 4    FORMAT(3X,A6,10X,2HZ=,3E20.11,5X,6HIFAIL=,I2)
      CALL EXIT
      END
*EXECUTE
