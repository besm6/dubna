         PROGRAM D507
        COMMON /A/A(4)/B/B(2)/C/C(2)/D/D(2)/E/E(100)
          DATA ((A(I),I=1,4)=1.,2.,3.,4.)
         C(1)=1.0
          C(2)=5.0
          D(1)=0.5
          D(2)=0.5
         IPRINT=1
         CALL MINSQ(2,2,B,C,D,IPRINT,100,100,E,1.0,0.05)
          STOP
         END
          SUBROUTINE  FCN(M,N,B,C,IFLAG)
         DIMENSION B(2),C(2)
          COMMON /A/A(4)
         B(1)=A(1)+A(2)*C(1)+A(3)*C(1)**3+A(4)*C(2)**2
         B(2)=A(1)+A(2)*C(1)+A(3)*C(1)**3
         RETURN
      END
*EXECUTE
*
*
