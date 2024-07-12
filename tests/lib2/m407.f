      PROGRAM M407
      MEM=0
      CALL STOREB (MEM,3,8,11)
      K=JTHB(MEM,3,11)
      PRINT 10, K,K
      MEM=0
      CALL STORE6(MEM,2,15)
      K=JTH(MEM,2)
      PRINT 10, K,K
      MEM=0
      CALL STORE1(MEM,4,1)
      K=JTH1(MEM,4)
      PRINT 10, K,K
      MEM=0
      CALL STOREN(MEM,3,4,10)
      K=JTHN(MEM,3,10)
      PRINT 10,K,K
      K=JTH10(MEM,3)
      PRINT 10, K,K
 10   FORMAT(O20,3X,I10)
      END
*EXECUTE
*
*
