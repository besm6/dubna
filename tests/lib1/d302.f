      PROGRAM D302
      DIMENSION F(33,30),BWEST(30),BEAST(30),BNORTH(33),
     1BSOUTH(33),COEFS(3)
      IBX=3 $ JBY=1
      DX=2./32.
      DY=1./29.
      COEFS(1)=1.
      COEFS(2)=0.
      COEFS(3)=0.
      DO 10 J=1,30
      Y=(J-1)*DY
      BWEST(J)=Y*Y+Y-1.
   10 BEAST(J)=Y*Y+Y+1.
      DO 20 I=1,33
      X=(I-1)*DX-1.
      BSOUTH(I)=X
   20 BNORTH(I)=X*X+X+1.
      DO 30 I=1,33
      X=-1.+(I-1)*DX
      DO 30 J=1,30
      Y=(J-1)*DY
      F(I,J)=2.*(X*X+Y*Y)
   30 CONTINUE
      CALL ELPAHY(F,33,30,DX,DY,COEFS,IBX,BWEST,BEAST,
     1JBY,BSOUTH,BNORTH)
      PRINT 50
      PRINT 100,((I,J,F(I,J),J=1,30),I=1,33)
  100 FORMAT(5('F(',I2,',',I2,')=',E13.6,3X))
      DO 40 I=1,33
      X=-1.+(I-1)*DX
      DO 40 J=1,30
      Y=(J-1)*DY
      F(I,J)=X**2*Y**2+X+Y
   40 CONTINUE
      PRINT 50
      PRINT 100, ((I,J,F(I,J),J=1,30),I=1,33)
   50 FORMAT (1H1)
      IBX=3 $ JBY=1
      COEFS(1)=0.
      COEFS(2)=-1.
      COEFS(3)=0.
      DX=1./32.
      DY=0.003
      DO 11 J=1,30
      Y=(J-1)*DY
      BWEST(J)=Y
   11 BEAST(J)=Y+1.
      DO 21 I=1,33
      X=(I-1)*DX
      BSOUTH(I)=X*X
   21 CONTINUE
      DO 31 I=1,33
      DO 31 J=1,30
      F(I,J)=1.
   31 CONTINUE
      CALL ELPAHY(F,33,30,DX,DY,COEFS,IBX,BWEST,
     1BEAST,JBY,BSOUTH,BNORTH)
      PRINT 50
      PRINT 100,((I,J,F(I,J),J=1,30),I=1,33)
      DO 41 I=1,33
      X=(I-1)*DX
      DO 41 J=1,30
      Y=(J-1)*DY
      F(I,J)=X*X+Y
   41 CONTINUE
      PRINT 50
      PRINT 100,((I,J,F(I,J),J=1,30),I=1,33)
      IBX=3 $ JBY=1
      COEFS(1)=-1.
      COEFS(2)=0.
      COEFS(3)=0.
      DX=2./32.
      DY=0.05
      DO 12 J=1,30
      Y=(J-1)*DY
      BWEST(J)=0.5+0.5*Y**2
   12 BEAST(J)=0.5+0.5*Y**2
      DO 22 I=1,33
      X=-1.+(I-1)*DX
      BSOUTH(I)=0.5*X**2
      BNORTH(I)=0.
   22 CONTINUE
      DO 32 I=1,33
      DO 32 J=1,30
      F(I,J)=0.
   32 CONTINUE
      CALL ELPAHY(F,33,30,DX,DY,COEFS,IBX,BWEST,
     1BEAST,JBY,BSOUTH,BNORTH)
      PRINT 50
      PRINT 100,((I,J,F(I,J),J=1,30),I=1,33)
      DO 42 I=1,33
      X=-1.+(I-1)*DX
      DO 42 J=1,30
      Y=(J-1)*DY
      F(I,J)=0.5*X**2+0.5*Y**2
   42 CONTINUE
      PRINT 50
      PRINT 100,((I,J,F(I,J),J=1,30),I=1,33)
      END
*assem
 ELPAHY  :  , NAME,        DTRAN  /01.06.84/
 SYNT    :  , SUBP,
 SOLVE6  :  , SUBP,
 ANAL    :  , SUBP,
 NEWRO   :  , SUBP,
 I*MUI*  :  , SUBP,
          14, VTM ,*0001B
            , UJ  ,*0320B
 *0001B  :  , WTC ,*0416B
            , XTA ,
           7, ATX ,7
            , WTC ,*0416B
            , XTA ,1
           7, ATX ,10B
            , WTC ,*0416B
            , XTA ,2
           7, ATX ,11B
           7, XTA ,7
            , ASN ,64-7
            , UZA ,*0014B
            , ARX ,
            , UZA ,*0011B
            , UJ  ,*0017B
c
 *0011B  : 7, XTA ,3
           7, ATX ,12B
            , NTR ,3
            , NTR ,22B
            , UJ  ,*0021B
c
 *0014B  : 7, XTA ,2
           7, ATX ,12B
            , NTR ,3
            , NTR ,22B
            , UJ  ,*0021B
c
 *0017B  : 7, XTA ,1
           7, ATX ,12B
            , NTR ,3
            , NTR ,22B
 *0021B  :  , WTC ,*0414B
            , XTA ,
            , WTC ,*0415B
            , A/X ,
           7, ATX ,13B
           7, A*X ,13B
           7, ATX ,14B
            , WTC ,*0414B
            , XTA ,
            , WTC ,*0414B
            , A*X ,
           7, ATX ,15B
           7, XTA ,14B
           7, A*X ,7
           7, ATX ,16B
           7, XTA ,13B
            , WTC ,*0414B
            , A*X ,
           7, A*X ,10B
           7, ATX ,17B
           7, XTA ,15B
           7, A*X ,11B
           7, ATX ,20B
           7, XTA ,1
           7, ATX ,21B
            , NTR ,3
          13, VJM ,*0265B
c
            , NTR ,22B
            , UJ  ,*0057B
 *0040B  : 7, XTA ,1
           7, ATX ,22B
            , NTR ,3
          13, VJM ,*0301B
            , NTR ,22B
            , UJ  ,*0052B
 *0043B  : 7, XTA ,15B
            , WTC ,*0354B  .=I0
           1, VTM ,
            , WTC ,*0411B
           1, A*X ,-1
            , WTC ,*0411B
           1, ATX ,-1
           7, XTA ,22B
            , NTR ,3
           7, A+X ,1
           7, ATX ,22B
          13, VJM ,*0301B
            , NTR ,22B
c
 *0052B  :  , WTC ,*0413B
            , X-A ,
            , UZA ,*0043B
           7, XTA ,21B
            , NTR ,3
           7, A+X ,1
           7, ATX ,21B
          13, VJM ,*0265B
            , NTR ,22B
c
 *0057B  :  , WTC ,*0412B
            , X-A ,
            , UZA ,*0040B
            , WTC ,*0422B
 *0061B  :  , WTC ,
            , UJ  ,*0061B
            , UJ  ,*0066B
c
         :  , UJ  ,*0073B
c
         :  , UJ  ,*0100B
c
         :  , UJ  ,*0105B
c
 *0066B  : 7, XTA ,5
           7, ATX ,23B
           7, XTA ,
           7, ATX ,24B
           7, XTA ,5
           7, ATX ,25B
           7, XTA ,
           7, ATX ,26B
            , UJ  ,*0111B
c
 *0073B  : 7, XTA ,
           7, ATX ,23B
           7, XTA ,5
           7, ATX ,24B
           7, XTA ,
           7, ATX ,25B
           7, XTA ,5
           7, ATX ,26B
            , UJ  ,*0111B
c
 *0100B  : 7, XTA ,5
           7, ATX ,23B
           7, XTA ,
           7, ATX ,24B
           7, XTA ,
           7, ATX ,25B
           7, XTA ,5
           7, ATX ,26B
            , UJ  ,*0111B
c
 *0105B  : 7, XTA ,
           7, ATX ,23B
           7, XTA ,5
           7, ATX ,24B
           7, XTA ,5
           7, ATX ,25B
           7, XTA ,
           7, ATX ,26B
 *0111B  :  , WTC ,*0412B
          14, VTM ,
            , ITS ,14
            , WTC ,*0413B
          14, VTM ,
            , ITS ,14
            , WTC ,*0417B
          14, VTM ,
            , ITS ,14
            , WTC ,*0414B
          14, VTM ,
            , ITS ,14
            , WTC ,*0411B
          14, VTM ,
            , ITS ,14
            , WTC ,*0420B
          14, VTM ,
            , ITS ,14
            , WTC ,*0421B
          14, VTM ,
            , ITS ,14
          13, VJM ,NEWRO
          13, VJM ,*0254B
c
            , WTC ,*0412B
          14, VTM ,
            , ITS ,14
            , WTC ,*0413B
          14, VTM ,
            , ITS ,14
            , WTC ,*0417B
          14, VTM ,
            , ITS ,14
            , WTC ,*0411B
          14, VTM ,
            , ITS ,14
            , WTC ,*0423B
          14, VTM ,
            , ITS ,14
            , WTC ,*0424B
          14, VTM ,
            , ITS ,14
          13, VJM ,ANAL
c
          13, VJM ,*0254B
c
            , WTC ,*0412B
          14, VTM ,
            , ITS ,14
            , WTC ,*0413B
          14, VTM ,
            , ITS ,14
          14, VTM ,*0375B
            , ITS ,14
          14, VTM ,*0376B
            , ITS ,14
          14, VTM ,*0377B
            , ITS ,14
            , WTC ,*0415B
          14, VTM ,
            , ITS ,14
            , WTC ,*0417B
          14, VTM ,
            , ITS ,14
          14, VTM ,*0402B
            , ITS ,14
          14, VTM ,*0403B
            , ITS ,14
          14, VTM ,*0404B
            , ITS ,14
          14, VTM ,*0405B
            , ITS ,14
            , WTC ,*0411B
          14, VTM ,
            , ITS ,14
          14, VTM ,*0371B
            , ITS ,14
          13, VJM ,SOLVE6
            , NTR ,3
            , NTR ,22B
          13, VJM ,*0254B
c
            , WTC ,*0412B
          14, VTM ,
            , ITS ,14
            , WTC ,*0413B
          14, VTM ,
            , ITS ,14
            , WTC ,*0417B
          14, VTM ,
            , ITS ,14
            , WTC ,*0411B
          14, VTM ,
            , ITS ,14
          13, VJM ,SYNT
c
          13, VJM ,*0254B
c
            , WTC ,*0417B
            , WTC ,
 *0173B  :  , UJ  ,*0173B
c
         :  , UJ  ,*0200B
c
         :  , UJ  ,*0253B
c
         :  , UJ  ,*0215B
c
         :  , UJ  ,*0236B
c
 *0200B  : 7, XTA ,1
           7, ATX ,22B
            , NTR ,3
          13, VJM ,*0301B
            , NTR ,22B
            , UJ  ,*0213B
 *0203B  :  , WTC ,*0356B  .=I0
           1, VTM ,
            , WTC ,*0411B
           1, XTA ,
            , WTC ,*0355B  .=I0
           2, VTM ,
            , WTC ,*0411B
           2, ATX ,-1
           7, XTA ,22B
            , NTR ,3
           7, A+X ,1
           7, ATX ,22B
          13, VJM ,*0301B
c
            , NTR ,22B
c
 *0213B  :  , WTC ,*0413B
            , X-A ,
            , UZA ,*0203B
            , UJ  ,*0253B
 *0215B  : 7, XTA ,1
           7, ATX ,22B
            , NTR ,3
          13, VJM ,*0301B
            , NTR ,22B
            , UJ  ,*0234B
 *0220B  :  , WTC ,*0401B
           1, VTM ,
            , WTC ,*0420B
           1, XTA ,-1
            , WTC ,*0356B  .=I0
           2, VTM ,
            , WTC ,*0411B
           2, ATX ,
            , WTC ,*0401B
           1, VTM ,
            , WTC ,*0421B
           1, XTA ,-1
            , WTC ,*0355B  .=I0
           2, VTM ,
            , WTC ,*0411B
           2, ATX ,-1
           7, XTA ,22B
            , NTR ,3
           7, A+X ,1
           7, ATX ,22B
          13, VJM ,*0301B   . These
c                           . three words
            , NTR ,22B      . are
c                           . damaged
 *0234B  :  , WTC ,*0413B   . on tape
            , X-A ,         . 12/monsys
            , UZA ,*0220B
            , UJ  ,*0253B
 *0236B  : 7, XTA ,1
           7, ATX ,22B
            , NTR ,3
          13, VJM ,*0301B
            , NTR ,22B
            , UJ  ,*0251B
 *0241B  :  , WTC ,*0401B
           1, VTM ,
            , WTC ,*0420B
           1, XTA ,-1
            , WTC ,*0356B  .=I0
           2, VTM ,
            , WTC ,*0411B
           2, ATX ,
           7, XTA ,22B
            , NTR ,3
           7, A+X ,1
           7, ATX ,22B
          13, VJM ,*0301B
c
            , NTR ,22B
c
 *0251B  :  , WTC ,*0413B
            , X-A ,
            , UZA ,*0241B
c
 *0253B  :  , UJ  ,*0351B
c
 *0254B  :13, MTJ ,12
            , WTC ,*0412B
            , XTA ,
            , NTR ,3
          13, VJM ,*0314B
c
            , NTR ,22B
          12, UJ  ,
 *0260B  : 7, XTA ,1
           7, A+X ,-3
           7, ATX ,-3
           7, XTA ,1
           7, A+X ,27B
           7, ATX ,27B
          13, UJ  ,
c
         :  , UJ  ,*0260B
c
 *0265B  : 7, A-X ,27B
           7, ATX ,27B
           7, A+X ,-3
           7, ATX ,-3
           7, XTA ,21B
           7, ATX ,27B
          13, UJ  ,
c
 *0271B  : 7, XTA ,50B
           7, A+X ,-3
           7, ATX ,-3
           7, XTA ,47B
           7, A+X ,-1
           7, ATX ,-1
           7, XTA ,46B
           7, A+X ,-2
           7, ATX ,-2
           7, XTA ,1
           7, A+X ,30B
           7, ATX ,30B
          13, UJ  ,
c
         :  , UJ  ,*0271B
c
 *0301B  : 7, A-X ,30B
           7, ATX ,30B
           7, XTS ,50B
          14, VJM ,I*MUI*
           7, A+X ,-3
           7, ATX ,-3
           7, XTA ,30B
           7, XTS ,47B
          14, VJM ,I*MUI*
c
           7, A+X ,-1
           7, ATX ,-1
           7, XTA ,30B
           7, XTS ,46B
          14, VJM ,I*MUI*
c
           7, A+X ,-2
           7, ATX ,-2
           7, XTA ,22B
           7, ATX ,30B
          13, UJ  ,
c
 *0314B  : 7, A-X ,31B
           7, ATX ,31B
           7, A+X ,-2
           7, ATX ,-2
            , WTC ,*0412B
            , XTA ,
           7, ATX ,31B
          13, UJ  ,
 *0320B  : 7, MTJ ,8
          14, MTJ ,10
           7, VTM ,*0357B  .=0
           9, VTM ,14B
 *0322B  : 9, UTM ,-1
           9, UTC ,
           7, STX ,32B
           9, V1M ,*0322B
            , ITS ,13
            , ITS ,1
            , ITS ,2
            , ITS ,8
            , ITS ,
            , NTR ,3
            , WTC ,*0412B
            , XTA ,
           7, ATX ,46B
            , WTC ,*0412B
            , XTA ,
           7, ATX ,47B
            , WTC ,*0412B
            , XTA ,
           7, ATX ,50B
          13, VJM ,*0254B
            , NTR ,3
           7, XTA ,30B
           7, A+X ,4
            , WTC ,*0412B
            , XTS ,
          14, VJM ,I*MUI*
           7, A+X ,27B
           7, ATX ,-3
           7, XTA ,30B
           7, A+X ,4
            , WTC ,*0412B
            , XTS ,
          14, VJM ,I*MUI*
c
           7, A+X ,31B
           7, ATX ,-2
           7, XTA ,30B
           7, A+X ,4
            , WTC ,*0412B
            , XTS ,
          14, VJM ,I*MUI*
c
           7, ATX ,-1
            , NTR ,2
          10, UJ  ,
c
 *0351B  :  , STI ,
            , STI ,7
            , STI ,2
            , STI ,1
            , STI ,8
           8, UJ  ,
 *0354B  :  , INT ,0
 *0355B  :  , INT ,0
 *0356B  :  , INT ,0
 *0357B  :  , LOG ,
            , INT ,1
            , INT ,2
            , INT ,3
            , INT ,-1
            , OCT ,405
            , BSS ,4
 *0371B  :  , BSS ,4
 *0375B  :  , BSS ,1
 *0376B  :  , BSS ,1
 *0377B  :  , BSS ,2
 *0401B  :  , BSS ,1
 *0402B  :  , BSS ,1
 *0403B  :  , BSS ,1
 *0404B  :  , BSS ,1
 *0405B  :  , BSS ,4
 *0411B  :  , BSS ,1
 *0412B  :  , BSS ,1
 *0413B  :  , BSS ,1
 *0414B  :  , BSS ,1
 *0415B  :  , BSS ,1
 *0416B  :  , BSS ,1
 *0417B  :  , BSS ,1
 *0420B  :  , BSS ,1
 *0421B  :  , BSS ,1
 *0422B  :  , BSS ,1
 *0423B  :  , BSS ,1
 *0424B  :  , BSS ,4
            , END ,
*EXECUTE
*
*
