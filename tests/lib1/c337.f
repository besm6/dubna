       PROGRAM C337
        X=-0.3725024108
       PRINT 10
       DO 1 L=1,10
        A=EXPINT(X)
       PRINT 2,X,A
       X=X+0.2
 1      CONTINUE
       X=0.
       A=EXPINT(X)
       PRINT 2,X,A
 2     FORMAT(30X,F15.10,F20.10)
 10    FORMAT(1H1//50X'TEST C337'//40X'X'15X'EXPINT(X)'/)
       END
*EXECUTE
*
*
