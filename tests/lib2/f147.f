      PROGRAM F147
      DIMENSION A1(2),A2(2),B1(3),B2(3),GA1(9),GA2(9),GA12(9),GB1(9),
     F  GB2(9),GB12(9)
      A1(1)=0.5
      A1(2)=0.333333
      A2(1)=0.33333
      A2(2)=0.5
      GA1(5)=1.
      GA1(6)=2.
      GA1(8)=3.
      GA1(9)=4.
      GA2(5)=2.
      GA2(6)=3.
      GA2(8)=4.
      GA2(9)=5.
      GA12(5)=3.
      GA12(6)=4.
      GA12(8)=5.
      GA12(9)=6.
      PRINT 10
      CALL EDICO (A1,A2,B1,B2,GA1,GA2,GA12,GB1,GB2,GB12)
      PRINT103,(GA1(IA),GA2(IA),GA12(IA),GB1(IA),GB2(IA),GB12(IA),IA=1,9
     *)
  10  FORMAT(/50X,9HTEST F147//10X,3HGA1,15X,3HGA2,15X,4HGA12,15X,3HGB1,
     * 15X,3HGB2,15X,4HGB12//)
  103 FORMAT(2X,6E19.11/)
      END
*EXECUTE
*
*
