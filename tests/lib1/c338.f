         PROGRAM C338
         DIMENSION AB(4)
        DATA(AB=-0.5,0.5,1.5,2.0)
       PRINT 10
        DO 1 L=1,4
       P=AB(L)
       X=-1.0
       DO 2 I=1,5
      C=FERDIR(X,P)
       PRINT 3,X,P,C
        X=X+2.0
 2      CONTINUE
   1    CONTINUE
 3     FORMAT(40X,2F10.1,F20.10)
 10    FORMAT(1H1//50X'TEST C338'//47X'X'10X'P'10X'FERDIR(X,P)'/)
       END
*EXECUTE
*
*
