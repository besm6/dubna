       PROGRAM C342
        X=-20000.0
      PRINT 10
        DO 1 J=1,40
        A=STRH0(X)
        B=STRH1(X)
      PRINT 2,X,A,B
         X=X+1000.0
 1      CONTINUE
 2    FORMAT(10X,F10.1,10X,2F40.20)
 10   FORMAT(1H1//'TEST C342'//15X'X'35X'STRH0(X)'35X'STRH1(X)')
        END
*EXECUTE
*
*
