      PROGRAM F011 A
      DIMENSION A(3,3),B(3),R(3),Z(3,2)
      DATA A/2.,1.,3.,1.,-2.,2.,3.,1.,2./,B/9.,-2.,7./,Z/9.,-2.,7.,
     *       9.12345,-2.1258,7.123/
      CALL RFACT(3,A,3,R,IF,DET,JF)
      PRINT 1,IF,JF,DET
      CALL RFEQN(3,A,3,R,1,B)
      PRINT 2,B
      CALL RFEQN(3,A,3,R,2,Z)
      PRINT 3,((Z(I,J),J=1,2),I=1,3)
      CALL RFINV(3,A,3,R)
      PRINT 4,((A(I,J),J=1,3),I=1,3)
 1    FORMAT(1H1,1X,'RFACT'/5X,'IFAIL=',I1,5X,'JFAIL=',I1,
     *       5X,'DET=',E17.11/)
 2    FORMAT(/1X,'RFEQN'/5X,'B=',3E20.11/)
 3    FORMAT(/1X,'RFEQN',5X,'Z'/(5X,2E20.11)/)
 4    FORMAT(/1X,'RFINV',5X,'A'/(5X,3E20.11)/)
      CALL EXIT
      END
*CALL PTIME
*EXECUTE
