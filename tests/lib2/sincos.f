      PROGRAM SINCOS
      DIMENSION X(100),Y(100),Z(100)
      X(1) = -1.9
      Y(1) = SIN(-1.9)
      Z(1) = COS(-1.9)*1.5
      DO 5 I=2,90
      X(I) = X(I-1) + .1
      Y(I) = SIN(X(I))
   5  Z(I) = COS(X(I))*1.5
      CALL PAGE(15.,20.,11HPOГOB - ЛHФ,11,0)
       CALL LIMITS(-2.,7.,-1.5,1.5)
      CALL REGION(3.,3.,10.,15.,6HREGION,6,1)
      CALL AXES(4HOCЬX,4,1.,5,4HOCЬY,4,.3,4,0)
      CALL LINEMO(X,Y,85,2,10)
      CALL BROKEN(.5,.2,.3,.2)
      CALL BRLINE(X,Z,85)
      CALL ENDPG(4H0001)
      END
*EXECUTE
*
*
