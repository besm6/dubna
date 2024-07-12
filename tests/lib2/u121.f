      PROGRAM U121
         DIMENSION A(3),B(3),C(3)
         A(1)=0.0298/0.105655
         A(2)=0.0
         A(3)=0.0
         B(1)=-7.23329*0.99040
         B(2)=0.0
         B(3)=0.0
         CALL LORENC(A,B,C)
         PRINT 2,C
  2      FORMAT(1X,1F10.5)
         END
*EXECUTE
*
*
