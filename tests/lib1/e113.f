      PROGRAM E113
      DIMENSION X(15),Y(15),F(15,15,3)
      DATA X/1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15./,
     1     Y/1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15./
      DO 6 I=1,15
      DO 6 J=1,15
      F(J,I,1)=ALOG(X(I))+Y(J)
      F(J,I,2)=SIN(180.*Y(J)/3.14)+X(I)
   6  F(J,I,3)=SQRT(X(I))+Y(J)**2
      PRINT 10,X,Y
      DO 8 KK=1,3
      NN=20
      XX=0.53
      YY=-2.21
      PRINT 9,((F(J,I,KK),J=1,15),I=1,15)
C
      DO 3 NNN=1,NN
      CALL EXTINT(KK,X,Y,F,15,15,3,XX,YY,FF)
      PRINT 4,XX,YY,FF
          XX=XX+0.5
      YY=YY+1.0
    3 CONTINUE
    8 CONTINUE
  4   FORMAT(2(20X,F5.2),20X,E20.11)
  9   FORMAT(/50X,22HTHE TABLE OF FUNCTIONS/15(1X,15E8.1/)//21X,2HXX,23X
     1 ,2HYY,23X,2HFF/)
  10  FORMAT(1H1,50X,9HTEST E113///30X,2HX=,15F4.0/30X,2HY=,15F4.0/)
      END
*EXECUTE
*
*
