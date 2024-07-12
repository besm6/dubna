      PROGRAM D204
      COMMON /XYZ/ T,R
      DIMENSION Y(20),F(20)
      EXTERNAL EXTERN
      PRINT 15
      Y(1)=1.0
      Y(2)=0.0
      Y(3)=0.0
      Y(4)=1.0
      Y(5)=1.0
      X=0.1
      T=0.01
      N=5
      H=0.1
      CALL DFEQS1 (1,N,H,X,Y,EXTERN)
      PRINT 11,X,(Y(I),I=1,5),R
      CALL DFEQS1(2,N,H,X,Y,EXTERN)
      PRINT 11,X,(Y(I),I=1,5),R
      PRINT 21
 11   FORMAT(5H   X=,F6.3,4H  Y=,5F10.3,4H  R=,F6.3)
  15  FORMAT(//50X,9HTEST D204///)
  21  FORMAT(///50X,9HALL RIGHT)
      STOP
      END
      SUBROUTINE EXTERN(X,Y,F)
      COMMON/XYZ/T,R
      DIMENSION Y(20),F(20)
      R=SQRT(Y(3)*Y(3)+Y(4)*Y(4))
      F(1)=T/(Y(5)**2)-Y(3)/(R**3)
      F(2)=T/(Y(5)**2)-Y(4)/(R**3)
      F(3)=Y(1)
      F(4)=Y(2)
      F(5)=-T
      RETURN
      END
*EXECUTE
*
*
