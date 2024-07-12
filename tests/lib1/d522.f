         PROGRAM D522
      EXTERNAL Y1,Y2
      PRINT 10
      A=-0.5$B=1.5
      DO 5 I=1,2
      PRINT 1,A,B
      CALL KOR(X,A,B,Y1,1.E-9,K)
      PRINT 2,X,K
      CALL KOR(X,A,B,Y2,1.E-9,K)
      PRINT 3,X,K
      A=-3.2$B=3.2
      PRINT 4
  5   CONTINUE
      A=-0.5$B=0.5
      CALL KOR(X,A,B,Y2,1.E-9,K)
      PRINT 1,A,B
      PRINT 3,X,K
        STOP
  1   FORMAT(30X,2HA=,F5.2,10X,2HB=,F5.2/30X,24(1H-)/)
  2   FORMAT(30X,9HY1=SIN(X),10X,5HROOT=,F10.7,10X,2HK=,I2/30X,9(1H-))
  3   FORMAT(30X,10HY2=X**2-1.,10X,5HROOT=F10.7,10X,2HK=,I2/
     130X,10(1H-)///)
  4   FORMAT(20X,45(1H-)///)
  10  FORMAT(//50X,9HTEST D522///)
          END
      FUNCTION Y1(X)
      Y1=SIN(X)
      RETURN
      END
      FUNCTION Y2(X)
      Y2=X*X-1.0
      RETURN
      END
*EXECUTE
*
*
