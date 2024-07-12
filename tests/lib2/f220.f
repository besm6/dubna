      PROGRAM F220
      DIMENSION AR(10,10),AI(10,10),WR(10),WI(10),ZR(10,10),ZI(10,10),
     X WORK(100)
      NM=10
      RLB=-1.
      RUB=2.
      CALL MCR(NM,N,AR,AI)
      CALL MCP(NM,N,AR,AI)
C     ALL EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A COMPLEX
C     GENERAL MATRIX
      CALL EISCG1(NM,N,AR,AI,WR,WI,ZR,ZI,IERR,WORK)
      NR=1
      PRINT 100,NR,IERR
      IF(IERR.NE.0) GO TO 2
      CALL VALCP(N,WR,WI)
      CALL VECCP(NM,N,N,ZR,ZI)
    2 CALL MCR(NM,N,AR,AI)
      CALL MCP(NM,N,AR,AI)
C     ALL EIGENVALUES OF A COMPLEX GENERAL MATRIX
      CALL EISCG2(NM,N,AR,AI,WR,WI,IERR)
      NR=2
      PRINT 100,NR,IERR
      IF(IERR.NE.0) GO TO 4
      CALL VALCP(N,WR,WI)
    4 CALL MCR(NM,N,AR,AI)
      CALL MCP(NM,N,AR,AI)
C     ALL EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A COMPLEX
C     HERMITIAN MATRIX
      CALL EISCH1(NM,N,AR,AI,WR,ZR,ZI,IERR,WORK)
      NR=1
      PRINT 110,NR,IERR
      IF(IERR.NE.0) GO TO 6
      CALL VALRP(N,WR)
      CALL VECCP(NM,N,N,ZR,ZI)
    6 CALL MCR(NM,N,AR,AI)
      CALL MCP(NM,N,AR,AI)
C     ALL EIGENVALUES OF A COMPLEX HERMITIAN MATRIX
      CALL EISCH2(NM,N,AR,AI,WR,IERR,WORK)
      NR=2
      PRINT 110,NR,IERR
      IF(IERR.NE.0) GO TO 8
      CALL VALRP(N,WR)
    8 CALL MCR(NM,N,AR,AI)
      CALL MCP(NM,N,AR,AI)
C     SOME EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A COMPLEX
C     HERMITIAN MATRIX
      CALL EISCH3(NM,N,AR,AI,RLB,RUB,N,M,WR,ZR,ZI,IERR,WORK)
      NR=3
      PRINT 110,NR,IERR
      IF(IERR.NE.0) GO TO 10
      IF(M.EQ.0) GO TO 9
      CALL VALRP(M,WR)
      CALL VECCP(NM,N,M,ZR,ZI)
    9 PRINT 500,M
   10 CALL MCR(NM,N,AR,AI)
      CALL MCP(NM,N,AR,AI)
C     SOME EIGENVALUES OF A COMPLEX HERMITIAN MATRIX
      CALL EISCH4(NM,N,AR,AI,RLB,RUB,N,M,WR,IERR,WORK)
      NR=4
      PRINT 110,NR,IERR
      IF(IERR.NE.0) GO TO 12
      IF(M.EQ.0) GO TO 11
      CALL VALRP(M,WR)
   11 PRINT 500,M
   12 CALL MRR(NM,N,AR)
      CALL MRP(NM,N,AR)
C     ALL EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A REAL GENERAL
C     MATRIX
      CALL EISRG1(NM,N,AR,WR,WI,ZR,IERR,WORK)
      NR=1
      PRINT 120,NR,IERR
      IF(IERR.NE.0) GO TO 14
      CALL VALCP(N,WR,WI)
      CALL VECRP(NM,N,N,ZR)
   14 CALL MRR(NM,N,AR)
      CALL MRP(NM,N,AR)
C     ALL EIGENVALUES OF A REAL GENERAL MATRIX
      CALL EISRG2(NM,N,AR,WR,WI,IERR)
      NR=2
      PRINT 120,NR,IERR
      IF(IERR.NE.0) GO TO 16
      CALL VALCP(N,WR,WI)
   16 CALL MRR(NM,N,AR)
      CALL MRP(NM,N,AR)
C     ALL EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A REAL
C     SYMMETRIC MATRIX
      CALL EISRS1(NM,N,AR,WR,ZR,IERR,WORK)
      NR=1
      PRINT 130,NR,IERR
      IF(IERR.NE.0) GO TO 18
      CALL VALRP(N,WR)
      CALL VECRP(NM,N,N,ZR)
   18 CALL MRR(NM,N,AR)
      CALL MRP(NM,N,AR)
C     ALL EIGENVALUES OF A REAL SYMMETRIC MATRIX
      CALL EISRS2(NM,N,AR,WR,IERR,WORK)
      NR=2
      PRINT 130,NR,IERR
      IF(IERR.NE.0) GO TO 20
      CALL VALRP(N,WR)
   20 CALL MRR(NM,N,AR)
      CALL MRP(NM,N,AR)
C     SOME EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A REAL
C     SYMMETRIC MATRIX
      CALL EISRS3(NM,N,AR,RLB,RUB,N,M,WR,ZR,IERR,WORK)
      NR=3
      PRINT 130,NR,IERR
      IF(IERR.NE.0) GO TO 22
      IF(M.EQ.0) GO TO 21
      CALL VALRP(M,WR)
      CALL VECRP(NM,N,M,ZR)
   21 PRINT 500,M
   22 CALL MRR(NM,N,AR)
      CALL MRP(NM,N,AR)
C     SOME EIGENVALUES OF A REAL SYMMETRIC MATRIX
      CALL EISRS4(NM,N,AR,RLB,RUB,N,M,WR,IERR,WORK)
      NR=4
      PRINT 130,NR,IERR
      IF(IERR.NE.0) GO TO 24
      IF(M.EQ.0) GO TO 23
      CALL VALRP(M,WR)
   23 PRINT 500,M
   24 CALL MTR(NM,N,AR)
      CALL MTP(NM,N,AR)
C     ALL EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A REAL
C     SYMMETRIC TRIDIAGONAL MATRIX
      CALL EISST1(NM,N,AR,WR,ZR,IERR,WORK)
      NR=1
      PRINT 140,NR,IERR
      IF(IERR.NE.0) GO TO 26
      CALL VALRP(N,WR)
      CALL VECRP(NM,N,N,ZR)
   26 CALL MTR(NM,N,AR)
      CALL MTP(NM,N,AR)
C     ALL EIGENVALUES OF A REAL SYMMETRIC TRIDIAGONAL MATRIX
      CALL EISST2(NM,N,AR,WR,IERR,WORK)
      NR=2
      PRINT 140,NR,IERR
      IF(IERR.NE.0) GO TO 28
      CALL VALRP(N,WR)
   28 CALL MTR(NM,N,AR)
      CALL MTP(NM,N,AR)
C     SOME EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A REAL
C     SYMMETRIC TRIDIAGONAL MATRIX
      CALL EISST3(NM,N,AR,RLB,RUB,N,M,WR,ZR,IERR,WORK)
      NR=3
      PRINT 140,NR,IERR
      IF(IERR.NE.0) GO TO 30
      IF(M.EQ.0) GO TO 29
      CALL VALRP(M,WR)
      CALL VECRP(NM,N,M,ZR)
   29 PRINT 500,M
   30 CALL MTR(NM,N,AR)
      CALL MTP(NM,N,AR)
C     SOME EIGENVALUES OF A REAL SYMMETRIC TRIDIAGONAL MATRIX
      CALL EISST4(NM,N,AR,RLB,RUB,N,M,WR,IERR,WORK)
      NR=4
      PRINT 140,NR,IERR
      IF(IERR.NE.0) GO TO 32
      IF(M.EQ.0) GO TO 31
      CALL VALRP(M,WR)
   31 PRINT 500,M
   32 CONTINUE
  100 FORMAT(/,10X,8H***EISCG,I1,18H CALLED***   IERR=,I2)
  110 FORMAT(/,10X,8H***EISCH,I1,18H CALLED***   IERR=,I2)
 120  FORMAT(/,10X,8H***EISRG,I1,18H CALLED***   IERR=,I2)
  130 FORMAT(/,10X,8H***EISRS,I1,18H CALLED***   IERR=,I2)
  140 FORMAT(/,10X,8H***EISST,I1,18H CALLED***   IERR=,I2)
  500 FORMAT(/,10X,   2HM=,I3)
      END
      SUBROUTINE VALCP(N,WR,WI)
      DIMENSION WR(N),WI(N)
      PRINT 500
      PRINT 600,(WR(I),WI(I),I=1,N)
      RETURN
  500 FORMAT(//,15X,  19HCOMPLEX EIGENVALUES,/)
 600  FORMAT(5X,2E25.11,5X,2E25.11)
      END
      SUBROUTINE VALRP(N,WR)
      DIMENSION WR(N)
      PRINT 500
      PRINT 600,(WR(I),I=1,N)
      RETURN
  500 FORMAT(//,15X,  16HREAL EIGENVALUES,/)
 600  FORMAT(5X,5E21.11)
      END
      SUBROUTINE VECCP(NM,N,M,ZR,ZI)
      DIMENSION ZR(NM,NM),ZI(NM,NM)
      PRINT 500
      DO 20 I=1,M
      PRINT 700
   20 PRINT 600,(ZR(J,I),ZI(J,I),J=1,N)
      RETURN
  500 FORMAT(//,15X,  20HCOMPLEX EIGENVECTORS)
 600  FORMAT(5X,2E21.11,5X,2E21.11)
  700 FORMAT(/)
      END
      SUBROUTINE VECRP(NM,N,M,ZR)
      DIMENSION ZR(NM,NM)
      PRINT 500
      DO 20 I=1,M
      PRINT 700
   20 PRINT 600,(ZR(J,I),J=1,N)
      RETURN
  500 FORMAT(//,15X,  17HREAL EIGENVECTORS)
  600 FORMAT(5X,5E21.11)
  700 FORMAT(/)
      END
      SUBROUTINE MRP(NM,N,AR)
      DIMENSION AR(NM,NM)
      PRINT 300
      DO 15 I=1,N
   15 PRINT 400,(AR(I,J),J=1,N)
      RETURN
  300 FORMAT(////,15X,  11HREAL MATRIX,/)
  400 FORMAT(5X,12F10.3)
      END
      SUBROUTINE MTP(NM,N,AR)
      DIMENSION AR(NM,2)
      PRINT 300
      PRINT 400,(AR(I,1),I=2,N)
      PRINT 400,(AR(I,2),I=1,N)
      RETURN
  300 FORMAT(////,15X,  54HSYMMETRIC TRIDIAGONAL MATRIX, SUBDIAGONAL AND
     1 DIAGONAL,/)
  400 FORMAT(5X,12F10.3)
      END
      SUBROUTINE MCP(NM,N,AR,AI)
      DIMENSION AR(NM,NM),AI(NM,NM)
      PRINT 300
      DO 15 I=1,N
   15 PRINT 400,(AR(I,J),AI(I,J),J=1,N)
      RETURN
  300 FORMAT(////,15X,  14HCOMPLEX MATRIX,/)
  400 FORMAT(5X,5(2F10.3,2X))
      END
      SUBROUTINE MCR(NM,N,AR,AI)
      DIMENSION AR(NM,NM),AI(NM,NM)
      READ 100,N
      DO 5 I=1,N
    5 READ 200,(AR(I,J),AI(I,J),J=1,N)
      RETURN
  100 FORMAT(I2)
  200 FORMAT(10F8.0)
      END
      SUBROUTINE MRR(NM,N,AR)
      DIMENSION AR(NM,NM)
      READ 100,N
      DO 5 I=1,N
    5 READ 200,(AR(I,J),J=1,N)
      RETURN
  100 FORMAT(I2)
  200 FORMAT(10F8.0)
      END
      SUBROUTINE MTR(NM,N,AR)
      DIMENSION AR(NM,2)
      READ 100,N
      READ 200,(AR(I,1),I=2,N)
      READ 200,(AR(I,2),I=1,N)
      RETURN
  100 FORMAT(I2)
  200 FORMAT(10F8.0)
      END
*EXECUTE
 4
5.      9.      5.      5.      -6.     -6.     -7.     -7.
3.      3.      6.      10.     -5.     -5.     -6.     -6.
2.      2.      3.      3.      -1.     3.      -5.     -5.
1.      1.      2.      2.      -3.     -3.     0.      4.
 4
5.      9.      5.      5.      -6.     -6.     -7.     -7.
3.      3.      6.      10.     -5.     -5.     -6.     -6.
2.      2.      3.      3.      -1.     3.      -5.     -5.
1.      1.      2.      2.      -3.     -3.     0.      4.
 4
3.      0.      1.      0.      0.      0.      0.      2.
1.      0.      3.      0.      0.      -2.     0.      0.
0.      0.      0.      2.      1.      0.      1.      0.
0.      -2.     0.      0.      1.      0.      1.      0.
 4
3.      0.      1.      0.      0.      0.      0.      2.
1.      0.      3.      0.      0.      -2.     0.      0.
0.      0.      0.      2.      1.      0.      1.      0.
0.      -2.     0.      0.      1.      0.      1.      0.
 4
3.      0.      1.      0.      0.      0.      0.      2.
1.      0.      3.      0.      0.      -2.     0.      0.
0.      0.      0.      2.      1.      0.      1.      0.
0.      -2.     0.      0.      1.      0.      1.      0.
 4
3.      0.      1.      0.      0.      0.      0.      2.
1.      0.      3.      0.      0.      -2.     0.      0.
0.      0.      0.      2.      1.      0.      1.      0.
0.      -2.     0.      0.      1.      0.      1.      0.
 3
8.      -1.     -5.
-4.     4.      -2.
18.     -5.     -7.
 3
8.      -1.     -5.
-4.     4.      -2.
18.     -5.     -7.
 4
5.      4.      1.      1.
4.      5.      1.      1.
1.      1.      4.      2.
1.      1.      2.      4.
 4
5.      4.      1.      1.
4.      5.      1.      1.
1.      1.      4.      2.
1.      1.      2.      4.
 4
5.      4.      1.      1.
4.      5.      1.      1.
1.      1.      4.      2.
1.      1.      2.      4.
 4
5.      4.      1.      1.
4.      5.      1.      1.
1.      1.      4.      2.
1.      1.      2.      4.
 5
4.      6.      6.      4.
-4.     -10.    -12.    -10.    -4.
 5
4.      6.      6.      4.
-4.     -10.    -12.    -10.    -4.
 5
4.      6.      6.      4.
-4.     -10.    -12.    -10.    -4.
 5
4.      6.      6.      4.
-4.     -10.    -12.    -10.    -4.
*
*
