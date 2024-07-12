      PROGRAM      F201
      DIMENSION H(4,4),RR(4),RI(4),GAMMA(4)
      H ( 1,1 ) = 1.00
      H ( 1,2 ) = 0.42
      H ( 1,3 ) = 0.54
      H ( 1,4 ) = 0.66
      H ( 2,1 ) = 0.42
      H ( 2,2 ) = 1.00
      H ( 2,3 ) = 0.32
      H ( 2,4 ) = 0.44
      H ( 3,1 ) = 0.54
      H ( 3,2 ) = 0.32
      H ( 3,3 ) = 1.00
      H ( 3,4 ) = 0.22
      H ( 4,1 ) = 0.66
      H ( 4,2 ) = 0.44
      H ( 4,3 ) = 0.22
      H ( 4,4 ) = 1.00
         KA=4
         STARTR=1.0
         STARTI=0.0
         PRIN=7.0
      CALL EIG5(H,4,4,RR,RI,STARTR,STARTI,PRIN,GAMMA,KA)
         PRINT  10,(RR(I),I=1,4) ,(RI(K),K=1,4)
  10     FORMAT(10X,5HEIGEN/(15X,4F20.5))
         END
*EXECUTE
*
*
