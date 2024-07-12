       PROGRAM D114
       COMMON /PARAMS/ACC,NDIM,NSUB,ITER
       EXTERNAL EXAMPL
       ACC=1.
       NDIM=2
       NSUB=100
       ITER=5
       CALL RIWIAD(EXAMPL)
       END
      FUNCTION EXAMPL(Q)
      DIMENSION Q(20)
       EXAMPL=Q(1)*(Q(2)**2)
       END
*EXECUTE
*
*
