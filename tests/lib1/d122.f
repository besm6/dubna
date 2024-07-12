      PROGRAM D122
      DIMENSION F1(4),AINT(4)
      EXTERNAL F
      CALL GROUPI(0.,1.,.1,4,F,X,F1,AINT)
      PRINT 1,(AINT(I),I=1,4)
    1 FORMAT(2X,4F20.5)
      CALL GROUPI(0.,1.,1.,4,F,X,F1,AINT)
      PRINT 1,(AINT(I),I=1,4)
      CALL GROUPI(0.,0.,1.,4,F,X,F1,AINT)
      PRINT 1,(AINT(I),I=1,4)
      CALL GROUPI(0.,1.,.15,4,F,X,F1,AINT)
      PRINT 1,(AINT(I),I=1,4)
      STOP
      END
      SUBROUTINE F(X,F1,N)
      DIMENSION F1(N)
      F1(1)=X*X
      DO 1 I=2,N
    1 F1(I)=F1(I-1)*X
      PRINT 2,X,(F1(I),I=1,4)
    2 FORMAT(2X,4HX F1,5F20.10)
      RETURN
      END
*EXECUTE
*
*
