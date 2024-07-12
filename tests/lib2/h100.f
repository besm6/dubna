      PROGRAM H100
      DOUBLE PRECISION A(20,30), AA(20,30), B(20), BB(20), C(30), CC(30)
      DOUBLE PRECISION Z0, ZZ0, Q(100)
      INTEGER          IROW(20), ICOL(30), LCV(80), MN(4), FALL
      DIMENSION        X(50), Y(30), TITLE(8)
      MD = 20
      READ 100, NUMBER
      DO 50  NUM = 1, NUMBER
      READ 101, TITLE
      READ 102, MN
      M = MN(2)
      N = MN(4)
      DO 5  K = 1, N
    5 READ 103, (A(I,K),  I = 1,M), C(K)
      READ 103, (B(I),  I = 1, M),  Z0
      DO 7  K = 1, N
      DO 6  I = 1, M
    6 AA(I,K) = A(I,K)
    7 CC(K) = C(K)
      DO 8  I = 1, M
    8 BB(I) = B(I)
      ZZ0 = Z0
      CALL SIMPLE (AA,BB,CC,ZZ0,MN,MD,IROW,ICOL,LCV,Q,MODE,2)
      PRINT 110, TITLE
      CALL SIMPLE (A,B,C,Z0,MN,MD,IROW,ICOL,LCV,Q,FALL,0)
      IF(FALL .EQ. 0)  PRINT 111
      IF(FALL .EQ. 1)  PRINT 112
      IF(FALL .EQ. 2)  PRINT 113
      IF(FALL .EQ. 3)  PRINT 114
      IF(FALL .EQ. 1  .OR.  FALL .EQ. 2)  GOTO 50
      IND = M+N
      DO 10  I = 1, IND
   10 X(I) = SNGL(Q(I))
      Z = SNGL(Z0)
      DO 30  K = 1, N
      IND = M + K
   30 Y(K) = X(IND)
      IF(M-N)  35,35,40
   35 PRINT 120, (IND, X(IND), IND, Y(IND), IND = 1, M)
      IF(M .EQ. N)  GOTO 45
      M1 = M + 1
      PRINT 121, (IND, Y(IND),  IND = M1, N)
      GOTO 45
   40 PRINT 120, (IND, X(IND), IND, Y(IND),  IND = 1, N)
      N1 = N + 1
      PRINT 122, (IND, X(IND),  IND = N1, M)
   45 PRINT 123,  Z
   50 CONTINUE
  100 FORMAT(I5)
  101 FORMAT(10A6)
  102 FORMAT(4I5)
  103 FORMAT(8D10.0)
  110 FORMAT( // 10A6/1H ,70 (1H-)// )
  111 FORMAT(  36H THE PROBLEM HAS ONE FINITE SOLUTION///)
  112 FORMAT(  52H THE PROBLEM HAS NO FINITE SOLUTION (MAX = INFINITE))
  113 FORMAT(  52H NO FEASIBLE INITIAL SILUTION EXISTS FOR THE PROBLEM)
  114 FORMAT(  38H MORE THAN ONE MAXIMUM SOLUTION EXISTS///)
  120 FORMAT(   2H X,I2,   3H = ,E22.12,22X,   1HY,I2,   3H = ,E22.12)
  121 FORMAT(51X,   1HY,I2,   3H = ,E22.12)
  122 FORMAT(   2H X,I2,   3H = ,E22.12)
  123 FORMAT(  44H0MAXIMUM VALUE OF THE OBJECTIVE FUNCTION  = ,E22.12)
      END
*CALL PTIME
*EXECUTE
    9
EXAMPLE 1,  DEGENERACIES
    4    4    3    3
   -0.04    -60.     0.25       9.        0.
   -0.02     -90.     0.5      3.         0.
    1.      0.        0.        0.        1.
   -0.02     150.     -0.75        6.      0.
EXAMPLE 2,   NO RESTRICTIONS FOR THE VARIABLES X(I)
    0    2    6    6
    -2.      5.       20.
    -2.      15.       0.
     2.       5.       -40.
    2.      1.        -16.
    6.      -1.       0.
     1.      -1.      10.
     -1.      -1.      0.
EXAMPLE 3,   EQUATION CONSTRAINT
    0    2    6    7
    -2.      5.       20.
    -2.      15.       0.
     2.       5.     -40.
    2.      1.        -16.
    6.      -1.       0.
     1.      -1.      10.
    -10.     3.       0.
     -1.      -1.      0.
EXAMPLE 4,  MORE THAN ONE SOLUTION
    2    2    3    3
    10.      20.      1100.
    1.      4.       160.
     1.       1.      100.
    -1.      -2.       0.
EXAMPLE 5,    NO FEASIBLE INITIAL SOLUTION EXISTS
    0    2    7    7
    -2.      5.       20.
    -2.      15.       0.
     2.       5.       -40.
    2.      1.        -16.
    6.      -1.       0.
     1.      -1.      10.
   -1.       0.       -1.
     -1.      -1.      0.
EXAMPLE 6,   MAXIMUM = INFINITE
    0    2    6    6
    -2.      5.       20.
    -2.      15.       0.
     2.       5.       -40.
    2.      1.        -16
    6.      -1.       0.
     1.      -1.      10.
     1.      1.      0.
EZAMPLE 7,  MAXIMUM IN DEGENERATED COIN
    0    2    6    6
    -2.      5.       20.
    -2.      15.       0.
     2.       5.       -40.
    2.      1.        -16.
    6.      -1.       0.
     1.      -1.      10.
    0.     -1.      0.
EXAMPLE 8,   ALL THE  C(K) ARE NEGATIVE
    2    2    3    3
    -1.     -4.      -8.
   -2.      -3.      -12.
    -2.     -1.       -6.
    1.      1.       0.
EXAMPLE 9,  ONLY EQUALITIES AS CONSTRAINTS
    2    4    0    4
    -4.      -9.       3.        -7.       2.
    -5.     2.        -7.        -1.        12.
    -10.       -5.       2.       -1.      -35.
     -2.        3.        -2.     3.        -11.
    -4.      -10.       4.        5.       -14.
*
*
