      PROGRAM F011 C
      DIMENSION R(3)
      COMPLEX A(3,3),B(3),DET,Z(3,2)
      DATA A/2.,0.,1.,0.,3.,0.,1.,0.,-2.,0.,2.,0.,3.,0.,1.,0.,2.,0./,
     *     B/9.,0.,-2.,0.,7.,0./,
     *     Z/9.,0.,-2.,0.,7.,0.,9.12345,0.,-2.1258,0.,7.123,0./
      CALL CFACT(3,A,3,R,IF,DET,JF)
      PRINT 1,IF,JF,DET
      CALL CFEQN(3,A,3,R,1,B)
      PRINT 2,B
      CALL CFEQN(3,A,3,R,2,Z)
      PRINT 3,((Z(I,J),J=1,2),I=1,3)
      CALL CFINV(3,A,3,R)
      PRINT 4,((A(I,J),J=1,3),I=1,3)
 1    FORMAT(1H1,1X,'CFACT'/5X,'IFAIL=',I1,5X,'JFAIL=',I1,
     *       5X,'DET=',2E17.11/)
 2    FORMAT(/1X,'CFEQN'/5X,'B=',6E17.10/)
 3    FORMAT(/1X,'CFEQN',5X,'Z'/(5X,4E20.11)/)
 4    FORMAT(/1X,'CFINV',5X,'A'/(5X,6E17.10)/)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
