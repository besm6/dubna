      PROGRAM F403
      DIMENSION B(5,3),R(5)
      DATA B/1.,2.,1.,1.,1.,2.,1.,3.,5.,2.,0.,1.,1.,1.,0./,
     *     R/1.,2.,1.,3.,2./
      N=5
      MM=3
      K=1
      IDIM=5
      PRINT 10,((B(I,J),J=1,3),I=1,5)
       CALL BLEQ(B,N,MM,K,IDIM,R,PIV)
      PRINT 1,R,PIV
      STOP
  10  FORMAT(//50X,9HTEST F403//30X,1HA//5(10X,3F10.1/))
  1   FORMAT(5X,2HR=,5E20.11/5X,4HPIV=,E20.11)
      END
*EXECUTE
*
*
