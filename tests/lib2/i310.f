      PROGRAM I310
      DIMENSION K(18),R(18)
      EQUIVALENCE(K(1),R(1))
      CALL UREAD(K,1,18)
      PRINT2,(R(I),I=1,3),(R(I),I=7,9)
       PRINT1,(K(I),I=4,6),(K(I),I=13,15)
      PRINT3,(K(I),I=10,12),(K(I),I=16,18)
    1 FORMAT(3X,8I6)
    2 FORMAT(1H1,3X,8E11.3)
    3 FORMAT(3X,10A6)
      STOP
      END
*CALL PTIME
*EXECUTE
1.,200.,3E3,-1,-2,-3, -.4E5, 5  .E3, . 6 E0 8,ABCDEFGH,IJKLMNOP,
), 4 ,  5  ,  6   1, A 1 2 B, 3 4 C D ,  END  END,
*
*
