      PROGRAM E206
      DIMENSION A(10),B(10),F(10),G(10),E(10),C(3)
      DATA F/-0.6,-0. 9,-1.12,-1.25,0.,1.25,1.12,0.89,0.6,0.298/,
     1     G/6.52,6.21,6.03,5.9,5.85,5.9,6.03,6.21,6.52,7.15/,
     2     E/1.49,2.15,2.75,3.39,4.02,4.55,4.94,5.17,5.21,0./,
     3     C/2HF=,2HG=,2HE=/
      PRINT 10
      IOP=3
      NF=10
      NO=10
      NE=0
      PRINT 1,C(1),F,IOP
      CALL TRICOF (F,NF,A,NE,B,NO,IOP)
      PRINT 2,NO,B
      IOP=2
      NE=10
      NO=0
      PRINT 1,C(2),G,IOP
      CALL TRICOF (G,NF,A,NE,B,NO,IOP)
      PRINT 3,NE,A
      IOP=1
      NF=9
      NE=5
      NO=3
      PRINT 1,C(3),E,IOP
      CALL TRICOF (E,NF,A,NE,B,NO,IOP)
      PRINT 4,NE,NO,(A(I),I=1,5),(B(J),J=1,3)
      NF=1
      PRINT 5,NF,IOP
      CALL TRICOF (F,NF,A,NE,B,NO,IOP)
      NF=10
      PRINT 5,NF,IOP
      CALL TRICOF (F,NF,A,NE,B,NO,IOP)
      IOP=2
      NE=1
      PRINT 6,NE,IOP
      CALL TRICOF (G,NF,A,NE,B,NO,IOP)
      IOP=1
      NO=1
      NF=9
      NE=5
      PRINT 7,NO,IOP
      CALL TRICOF (E,NF,A,NE,B,NO,IOP)
      STOP
   1  FORMAT(10X,A6,10F8.3/10X,4HIOP=,I1)
   2  FORMAT(10X,3HNO=,I2//5X,2HB=/2(5X,5E20.11/)//5X,100(1H-)//)
   3  FORMAT(10X,3HNE=,I2//5X,2HA=/2(5X,5E20.11/)//5X,100(1H-)//)
   4  FORMAT(10X,3HNE=,I2/10X,3HNO=,I2//5X,2HA=,5E20.11//
     1       5X,2HB=,3E20.11//5X,100(1H-)//)
   5  FORMAT(//2X,3HNF=,I2,5X,4HIOP=,I1)
   6  FORMAT(//2X,3HNE=,I2,5X,4HIOP=,I1)
   7  FORMAT(//2X,3HNO=,I2,5X,4HIOP=,I1)
  10  FORMAT(//50X,9HTEST E206//)
      END
*EXECUTE
*
*
