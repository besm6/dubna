      PROGRAM V151
      DIMENSION FSPACE(100),X(10),Y(10)
      EXTERNAL FUNC
      COMMON/FUNINT/TFTOT
      XLOW=-2.
      XHIGH=2.
      CALL FUNPRE(FUNC,FSPACE,XLOW,XHIGH)
      DO 1 I=1,10
      CALL FUNRAN(FSPACE,XRAN)
      X(I)=XRAN
      Y(I)=TFTOT
 1    CONTINUE
      PRINT 2,X
      PRINT 3,Y
 2    FORMAT(/1X,'XRAN=',10F11.3)
 3    FORMAT(/1X,'TFTOT=',10F11.3)
      STOP
      END
      FUNCTION FUNC(X)
      FUNC=(1./SQRT(2.*3.14))*EXP(-(X**2)/2.)
      RETURN
      END
*CALL PTIME
*EXECUTE
*
*
