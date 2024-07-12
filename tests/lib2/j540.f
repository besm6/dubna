        PROGRAM J540
         DIMENSION T(300)
        H=6.28/50.
        DO 12 I1=1,51
        I=I1*2-1
        T(I)=H*I1-H
        T(I+1)=SIN(T(I))
        T(102+I)=T(I)
 12     T(103+I)=5*T(I+1)
        PRINT 1,T
 1     FORMAT(2X,10F10.4)
        CALL XYPLOT(T,300,0.,6.28,-5.,5.,50,6H******)
       END
*EXECUTE
*
*
