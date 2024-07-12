      PROGRAM C205
       EXTERNAL FCN
      PRINT 10
      CALL RZERO(-4,0.,X,R,10.E-6,100,FCN)
      PRINT 1,X,R
      CALL RZERO(0.,1.,X,R,10.E-6,1,FCN)
      PRINT 2,X,R
      CALL RZERO(-2.,0.,X,R,10.E-6,100,FCN)
      PRINT 3,X,R
  1   FORMAT(25X'A=-4.     B=0.'/25X'X='F11.7,5X'R='F11.7//)
  2   FORMAT(25X'A=0.      B=1.'/25X'X='F11.7,5X'R='F11.7//)
  3   FORMAT(25X'A=-2.     B=0.'/25X'X='F11.7,5X'R='F11.7)
  10  FORMAT(1H1//50X'TEST C205'///30X'FCN=-X**2-3*X+2'///)
      END
      FUNCTION FCN(X,I)
      FCN=-X**2-3*X+2
       RETURN
      END
*CALL PTIME
*EXECUTE
*
