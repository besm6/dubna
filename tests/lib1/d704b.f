      PROGRAM D704 B
      COMPLEX A(128)
      DO 1 I=1,50
      I1=78+I
      A(I)=(0.,0.)
 1    A(I1)=(0.,0.)
      DO 2 I=51,78
 2    A(I)=(1.,0.)
      PRINT 6
      CALL CFFT(A,-7)
      PRINT 10,A(1),A(50),A(51),A(78),A(79),A(128)
      CALL CFFT(A,7)
      PRINT 10,A(1),A(50),A(51),A(78),A(79),A(128)
      DO 3 I=1,128
 3    A(I)=A(I)/128
      PRINT 10,A(1),A(50),A(51),A(78),A(79),A(128)
 6    FORMAT(///15X,4HA(1),15X,5HA(50),15X,5HA(51),15X,5HA(78),15X,
     *        5HA(79),14X,6HA(128)//)
 10   FORMAT((5X,12E10.3)/)
      STOP
      END
*EXECUTE
*
*
