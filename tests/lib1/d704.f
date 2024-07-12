      PROGRAM D704
      COMPLEX A(128)
      DO 1 I=1,50
      I1=78+I
      A(I)=(0.,0.)
  1   A(I1)=(0.,0.)
      DO 2 I=51,78
  2   A(I)=(1.,0.)
      PRINT 6
      CALL CFFT(A,-7)
      DO 3 I=1,64
      I1=64+I
      A(I)=A(I)/128
      A(I1)=A(I1)/128
  3   PRINT 4,I,A(I),I1,A(I1)
      PRINT 6
      CALL CFFT(A,7)
      DO 5 I=1,64
      I1=64+I
  5   PRINT 4,I,A(I),I1,A(I1)
  4   FORMAT(10X,2(I10,2F10.4))
  6   FORMAT(1H1)
      END
*EXECUTE
*
*
