      PROGRAM J511A
      EXTERNAL F
      A=0.0
      B=10.0
      C=0.5
      CALL MAP(F,A,B,C)
      STOP
      END
      FUNCTION F(X)
      F=EXP(X)
      RETURN
      END
*EXECUTE
*
*
