      PROGRAM C311
      COMPLEX V(5),Z(6),P,Q
      PRINT 10
      V(1)=(-2.,1.5)
      DO 1 J=1,5
      Z(1)=(2.5,-3.)
      DO 2 I=1,6
      CALL LEGFN(V(J),Z(I),P,Q,NC,NF)
      PRINT 5,V(J),Z(I),P,Q,NC,NF
  2   Z(I+1)=Z(I)+(-0.5,1.)
  1   V(J+1)=V(J)+(1.,-0.5)
  5   FORMAT(5X'('F4.1','F4.1')'5X'('F4.1','F4.1')'7X'('F10.5','F10.5')'
     17X'('F10.5','F10.5')'7X,2I10/)
  10  FORMAT(1H1//50X'TEST C311'///10X'V'15X'Z'22X'P'30X'Q'27X'NC'10X'NF
     1'//)
      END
*EXECUTE
*
*
