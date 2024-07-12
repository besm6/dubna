      PROGRAM C203
      INTEGER  NZ
      COMPLEX FCN,Z
      EXTERNAL FCN
      Z=(0.,0.)
      R=4.
      PRINT 10
      CALL NZEROS(NZ,Z,R,FCN)
      PRINT 1,NZ,R
  1   FORMAT(10X'NZ='I5,2X'R='F4.1)
 10   FORMAT(//50X'TEST C203'//10X'FCN=Z*CSIN(Z)'/10X'Z=(0.,0.)'/10X'R=4
     1.'//)
      END
      COMPLEX FUNCTION FCN(Z,I)
      COMPLEX Z
      FCN=Z*CSIN(Z)
      RETURN
      END
*EXECUTE
*
*
