      PROGRAM J300
      DIMENSION A(5,5)
      DATA(A=3.0,4.0,2.0,1.5,9.2,7.0,6.5,1.0,1.5,3.0,2.8,9.0,5.0,6.0,
     110.0,8.0,7.5,1.0,3.2,4.3,9.0,3.4,1.1,0.95,2.0)
         IDIM1=5
      IDIM2=5
         IROW=5
         ICOL=5
         NCOL=3
         CALL MXPRNT(A,IDIM1,IDIM2,IROW,ICOL,NCOL,5HF10.1)
      END
*EXECUTE
*
*
