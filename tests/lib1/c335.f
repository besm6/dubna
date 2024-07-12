       PROGRAM C335
      COMPLEX CWERF,Z
      COMPLEX A
       Z=(1.0,0.0)
      PRINT 10
       DO 1 L=1,10
       A=CWERF(Z)
      PRINT 2,Z,A
       Z=Z+(0.1,0.1)
 1      CONTINUE
  2   FORMAT(30X'('F4.1','F4.1')'5X'('F15.10','F15.10')')
 10   FORMAT(1H1//45X'TEST C335'//35X'Z'23X'CWERF(Z)'//)
        END
*EXECUTE
*
*
