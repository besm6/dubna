      PROGRAM D206
      COMMON /XYZ/T,R
      DIMENSION Y(20),F(20)
      EXTERNAL EXTERN
      LOGICAL DEBUT
      PRINT 15
      Y(1)=1.0
      Y(2)=0.0
      Y(3)=0.0
            Y(4)=1.0
      Y(5)=1.0
      X=0.1
      T=0.01
      H=0.1
      N=5
      XEND=2.
      ERROR=.001
      DEBUT=.FALSE.
      PRINT 10
      PRINT 20
      PRINT 30
      PRINT 40
      PRINT 50
      PRINT 60
         CALL NORSIK(N,X,XEND,Y,H,ERROR,DEBUT,EXTERN)
      PRINT 11,X,(Y(I),I=1,5),R
      PRINT 21
 10   FORMAT (31H      DX1/DZ=-X3/(R**3)+T/(M*M))
 11   FORMAT(5H   Z=,F6.3,4H X1=,F6.3,4H X2=,F6.3,4H X3=,F6.3,4H X4=,
     *F8.3,4H  M=,F8.3,4H  R=,F6.3)
  15  FORMAT(//50X,9HTEST D206///)
 20   FORMAT (31H      DX2/DZ=-X4/(R**3)+T/(M*M))
  21  FORMAT(/50X,9HALL RIGHT)
   30 FORMAT (16H      DX3/DZ= X1)
 40   FORMAT (16H      DX4/DZ= X2)
 50   FORMAT (16H      DM/DZ=  -T)
  60  FORMAT (46H    X1(0)=1  X2(0)=0  X3(0)=0  X4(0)=1  M(0)=1)
      END
       SUBROUTINE   EXTERN(X,Y,F)
       COMMON   /XYZ/T,R
       DIMENSION   Y(20), F(20)
      R=SQRT(Y(3)*Y(3)+Y(4)*Y(4))
       F(1)=T/(Y(5)**2)-Y(3)/(R**3)
       F(2)=T/(Y(5)**2)-Y(4)/(R**3)
       F(3)=Y(1)   $   F(4)=Y(2)   $   F(5)=-T
      RETURN
      END
*EXECUTE
*
*
