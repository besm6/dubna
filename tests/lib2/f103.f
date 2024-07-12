      PROGRAM      F103
         DIMENSION A(5,7),B(10,10),C(10,10),D(10,10),E(10,10)
        DATA(((A(I,J),I=1,4),J=1,4)=
     1   1.00 , 0.42 , 0.54 , 0.66,
     2   0.42 , 1.00 , 0.32 , 0.44 ,
     3   0.54 , 0.32 , 1.00 , 0.22 ,
     4   0.66 , 0.44 , 0.22 , 1.00 )
         DATA (((A(I,K),K=5,6),I=1,5) =
     *    0.76 ,  0.08 ,  1.12 ,  0.68 ,  3.14  ,
     *    1.52 ,  0.16 ,  2.24 ,  1.36 ,  6.28  )
      DATA (((B(I,K),I=1,4),K=1,4)=
     1   4.0,3.0,2.0,1.0,
     2   3.0,3.0,2.0,1.0,
     3   2.0,2.0,2.0,1.0,
     4   1.0,1.0,1.0,1.0)
         PRINT 9
         PRINT 10,((A(I,K),K=1,7),I=1,5)
         PRINT 12
         PRINT  11,((B(I,K),K=1,10),I=1,10)
         KA=5
         KB=10
         KC=10
         M=4
         N=4
         CALL MATRIX(20,4,4,4,A,5,B,10,C,10)
         CALL MATRIX(10,4,6,0,A,KA,D,0,0,0)
         PRINT 19
      PRINT 10, ((A(I,K),K=1,4),I=1,4)
         PRINT 21,((A(I,K),I=1,4),K=5,6)
         PRINT 22,D(1)
         CALL MATRIX(10,4,4,0,C,KC,D,0,0,0)
         CALL MATRIX(10,4,4,0,B,KB,E,0,0,0)
         CALL MATRIX(20,4,4,4,B,KB,A,KA,A,KA)
         CALL MATRIX(22,4,4,0,A,KA,C,KC,D,KC)
         PRINT 23
         PRINT  11,((D(I,K),K=1,10),I=1,10)
 9       FORMAT(1H1,10X,8HMATRIX A/)
 10      FORMAT(5X,7F10.3)
 11      FORMAT(5X,10F10.3)
 12      FORMAT(/10X,8HMATRIX B/)
 13      FORMAT(/10X,13H****  ADD A,B/)
 14      FORMAT(/10X,17H*** (A,B)-(A,)B=0/)
 15      FORMAT(/10X,21HSYMMETRIC PRODUCT AMA/)
 16      FORMAT(/10X,28HSUBTRACT A AND PACK-UNPACK A)
 17      FORMAT(/10X,18HMOVE A TO D,B TO E/)
 18      FORMAT(/10X,15HAB-A, AB-B, A-B/)
 19      FORMAT(/10X,24HINVERS A,SOLUTION SYSTEM/)
 20      FORMAT(10X,4F20.5)
  21    FORMAT((/5X,2HX=,4F20.5/))
 22      FORMAT(/5X,7HDETERM=,F10.5)
 23      FORMAT(/5X,24HINV(A*B)-INV(B)*INV(A)=0)
         END
*EXECUTE
*
*
