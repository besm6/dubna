      PROGRAM E110
      DIMENSION A(20),F(20),X(8)
       DATA X/0.1,-0.2,1.5,0.65,1.89,1.0,0.135,1.9/
      A(1)=0.0
      F(1)=0.0
      DO 100 I=1,19
      A(I+1)=A(I)+0.1
      F(I+1)=SIN(A(I+1))
100   CONTINUE
      PRINT 13,F
  13  FORMAT(/30X,25H TAБЛИЦA ЗHAЧEHИЙ ФYHKЦИИ/(20X,5F10.6 ))
      PRINT 15
  15  FORMAT(/20X,9H APГYMEHT,11X,10H PEЗYЛЬTAT)
      DO 101 K=1,8
      CALL PARINT (X(K),0.0,0.1,F,20,R)
      PRINT 14,X(K),R
  14  FORMAT(22X,F6.3,13X,F8.6)
101   CONTINUE
         END
*EXECUTE
*
*
