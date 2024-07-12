      PROGRAM J511B
      EXTERNAL F
      A=0.0
      B=10.0
      C=0.5
      CALL MAP(F,A,B,C)
      STOP
      END
      FUNCTION F(X)
      F=(X-1.0)/(0.2*X*X-X+0.5)
      RETURN
      END
*EXECUTE
*
*
