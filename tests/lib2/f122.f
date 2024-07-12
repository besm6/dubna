      PROGRAM F122
      DIMENSION B(4,5)
      DIMENSION A(4,5),NOIS(10),NOJS(10)
      DATA(((A(I,J),I=1,4),J=1,5)=1.,2.,7.,4.,2.,3.,8.,5.,3.,4.,9.,6.,
     M4.,5.,10.,7.,5.,6.,11.,8.)
      DATA(((B(I,J),I=1,4),J=1,5)=1.,2.,7.,4.,2.,3.,8.,5.,3.,4.,9.,6.,
     M4.,5.,10.,7.,5.,6.,11.,8.)
      NI=4
      NJ=5
      NIS=1
      NJS=3
      NOIS(1)=3
      NOJS(1)=2
      NOJS(2)=4
      NOJS(3)=5
      PRINT 10
      PRINT 20
       PRINT 2,((A(I,J),J=1,5),I=1,4)
      CALL MATRED(A,NI,NJ,NIS,NJS,NOIS,NOJS)
      PRINT 13,NIS,NJS
      PRINT 20
       PRINT 2,((A(I,J),J=1,5),I=1,4)
      CALL UCO(A,B,4,5)
      NIS=1
      NJS=0
      CALL MATRED(A,NI,NJ,NIS,NJS,NOIS,NOJS)
      PRINT 13,NIS,NJS
      PRINT 20
       PRINT 2,((A(I,J),J=1,5),I=1,4)
      CALL UCO(A,B,4,5)
      NIS=0
      NJS=3
      CALL MATRED(A,NI,NJ,NIS,NJS,NOIS,NOJS)
      PRINT 13,NIS,NJS
      PRINT 20
       PRINT 2,((A(I,J),J=1,5),I=1,4)
  2   FORMAT (10X,5F10.3)
  10  FORMAT (///50X,9HTEST F122//)
  13  FORMAT (///20X,4HNIS=,I2,10X,4HNJS=,I2/)
  20  FORMAT (30X,6HA(I,J)//)
      STOP
      END
      SUBROUTINE UCO(A,B,I,J)
      DIMENSION A(I,J),B(I,J)
      DO 30 M=1,I
      DO 30 N=1,J
      A(M,N)=B(M,N)
   30 CONTINUE
      RETURN
      END
*EXECUTE
*
*
