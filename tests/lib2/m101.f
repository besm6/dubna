      PROGRAM M101
      DIMENSION A(200),IA(200),INDEX(5),IN(5),IND(5),ID(5)
      DATA IN/10,50,100,150,200/,IND/10,50,100,150,200/
     1  ,  ID/10,50,100,150,200/
      DO 5 I=1,200
      IA(I)=I
  5   A(I)=RNDM(-1.)
      PRINT 2,(A(J),J=1,20)
      CALL SORTZV(A,INDEX,5,1,0,0)
      PRINT 1,INDEX
      PRINT 2,(A(K),K=10,200,10)
      CALL SORTZV(A,IN,5,1,2,3)
      PRINT 1,IN
      CALL SORTZV(IA,INDEX,5,1,0,0)
      PRINT 1,INDEX
      CALL SORTZV(IA,IND,5,1,2,2)
      PRINT 1,IND
      IA(10)=4HJOHN
      IA(50)=3HTOM
      IA(100)=3HJIM
      IA(150)=4HANNA
      IA(200)=4H1TOM
      CALL SORTZV(IA,INDEX,5,0,1,0)
      PRINT 1,INDEX
      CALL SORTZV(IA,ID,5,0,0,1)
      PRINT 1,ID
  1   FORMAT(2X,6HINDEX=,5I5)
  2   FORMAT(2(1X,10F10.5/))
      STOP
      END
*EXECUTE
*
*