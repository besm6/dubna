      PROGRAM V300
      DIMENSION X(7)
      INTEGER X
      X(1)=1
      X(2)=1
      X(3)=1
      X(4)=1
      X(5)=1
      X(6)=1
      X(7)=1
      PRINT 4,X
      CALL UBLANK(X,3,5)
      PRINT 1,X
      CALL UZERO(X,3,6)
      PRINT 2,X
      CALL UFILL(X,2,5,2)
      PRINT 3,X
  1   FORMAT(20X,14HRESULT  UBLANK///10X,2I5,3A5,2I5///)
  2   FORMAT(20X,13HRESULT  UZERO///10X,7I5///)
  3   FORMAT(20X,13HRESULT  UFILL///10X,7I5///)
  4   FORMAT(1H1,20X,8HARRAY  X///10X,7I5///)
      END
*EXECUTE
*
*
