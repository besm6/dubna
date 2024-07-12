      PROGRAM D515
          DIMENSION     CB(16),RB(12),P(20),AM1(4,4),AMD(5),Y(5),F(4)
          DIMENSION ANP1(32),ANP2(75)
      EXTERNAL ARIA,ARIB
          DATA CB/3.,8.,4.,1.,0.,1.,1.,0.,0.,0.,0.,0.,0.,0.,3.,.5/
      DATA ANP1 /7615., 0., 0., 200., 9373., 0., 200.,200.,
     *    6134., 0., 400.,200., 3288.,0.,600.,200.,1767.,0.,800.,200.,
     *    1503.,0.,1000.,500.,270.,0.,1500.,500.,50.,0.,2000.,100000./
      DATA ANP2 /100.,0.,1.,111.,0.,0.,112.,0.,0.,91.,0.,0.,108.,0.,0.,
     *    110.,0.,0.,132.,0.,0.,194.,0.,0.,305.,0.,0.,336.,0.,0.,
     *    381.,0.,0.,354.,0.,0.,384.,0.,0.,347.,0.,0.,264.,0.,0.,
     *    228.,0.,0.,173.,0.,0.,131.,0.,0.,98.,0.,0.,99.,0.,0.,
     *    109.,0.,0.,111.,0.,0.,104.,0.,0.,113.,0.,0.,116.,0.,0.   /
          DO 101 I=1,29,4
 101      ANP1(I+1)=1./ANP1(I)
          DO 102 I=1,73,3
          ANP2(I+2)=FLOAT(I/3+1)
 102      ANP2(I+1)=1./ANP2(I)
          CALL INPUT(P,1,9)
          CB(5)=50.
          CB(5)=500.
          CB(11)=.05
          CB(12)=.1
C   CASE = 'MOTHER-DOUGHTER' DECAY. THE REGRESSION FUNCTION IS
C         A1*F1(TM,TD,TI1,TI2) + A2*F2(TD,TI1,TI2)
C WHERE F1 AND F2 ARE FUNCTIONS OF MOTHER-DOUGHTER AND DOUGHTER DECAY.
C  TI1,TI2 = BEGINNING AND AND OF THE ITH TIME INTERVAL
C  TD = DOUGHTER HALFLIFE TIME IS KNOWN AND GIVEN
C  TM=MOTHER HALFLIFE TIME IS UNKNOWN. IT AND A1 AND A2 ARE PARAMETERS
C TRUE VALUES ARE.  A1 = 30000  TM=100  A2 =0
      CALL ROLSM(CB,RB,P,AM1,AMD,Y,F,P,AM1,3,ANP1,ARIA)
          CALL       BUIC(CB,RB,P,AM1,F,ANP1,ARIA)
          CALL INPUT(P,1,12)
          CALL INPUT(CB,1,16)
          ANP2(19)=1032.
          ANP2(25)=1305.
C   CASE = PEAK EVALUATION.         THE REGRESSION FUNCTION IS
C         C + A*EXP(-((X-P)/W)**2)
C C,A,P,W ARE PARAMETERS.
C TRUE VALUES ARE.  C=100  A=300  P=12  W=4
      CALL ROLSM(CB,RB,P,AM1,AMD,Y,F,P,AM1,4,ANP2,ARIB)
          CALL       BUIC(CB,RB,P,AM1,F,ANP2,ARIB)
      STOP
      END
          SUBROUTINE BUIC(CB,RB,P,AM1,F,ANP,ARIA)
C BUIC OUTPUTS THE RESULTS OF THE FITTING AND BUILDS THE FITTED CURVE
          DIMENSION CB(20),RB(20),P(20),AM1(2,2),F(2),ANP(100)
          N=CB(1)
          M=CB(2)
          NFD=CB(3)
          RITUS=RB(5)+RB(6)
      PRINT 1092,RB(1),RB(4),RITUS,RB(8),RB(9),RB(10),RB(11)
 1092 FORMAT(1X,6HCONV.=,F6.0,2X,4HEPS=,E17.10,2X,8HN ITER.=,
     * F6.0/2X,8HCHI2/DF=,E17.10,2X,5HCHI2=,E17.10,2X,5HCHI+=,E17.10,
     *     2X,3HM+=,E17.10//)
          PRINT 1093
 1093     FORMAT(8X,27HPARAMETERS AND THEIR ERRORS)
      PRINT 7,(P(I),I=1,N)
      PRINT 7,(F(I),I=1,N)
 7        FORMAT(2X,10F15.2)
          PRINT 1091
 1091     FORMAT(4X,4X,4HDATA,7X,3HFIT,6X,4HCHI2)
          DO 5 I=1,M
          K=(I-1)*NFD+1
          Z=ANP(K  )
          W=ANP(K+1)
          X=ANP(K+2)
          CALL ARIA(CB,RB,P,AM1,F,N,ANP,I,X,3.)
          C=(Z-X)**2*W
          PRINT 6,Z,X,C
 6    FORMAT(2X,2F10.1,E12.5)
 5        CONTINUE
          RETURN
          END
          SUBROUTINE ARIA (CB,RB,P,AM1,F,NP,ANP,K1,X,R)
          DIMENSION AM1(3,3),AM2(3),ANP(100),P(9),CB(16),RB(30),F(3)
          F1(T,X)=EXP(-.6931*X/T)
          F2(T,X1,X2)=F1(T,X1)-F1(T,X2)
          F3(T,X1,X2)=X1*F1(T,X1)-X2*F1(T,X2)
          T1(TM,TD)=TD/(TM-TD)
          T2(TM,TD)=TD/(TM-TD)**2
          TD=200
          TM=P(2)
          I=(K1-1)*4+3
          X1=ANP(I  )
          X2=ANP(I+1)+X1
          R1=F2(TM,X1,X2)
          R2=F2(TD,X1,X2)
          C1=   T1(TM,TD)
          C2=T2(TM,TD)
          F(1)=((1.+C1)*R1-C1*R2)
          F(2)=-C2*R1+(1.+C1)*.6931*F3(TM,X1,X2)/TM**2+C2*R2
          F(2)=F(2)*P(1)
          F(3)=R2
          X=P(1)*F(1)+P(3)*F(3)
          RETURN
          END
          SUBROUTINE ARIB (CB,RB,P,AM1,F,NP,ANP,K1,X,R)
          DIMENSION AM1(4,4),ANP(75),P(12),CB(16),RB(12),F(4)
          Z=(X-P(3))/P(4)
          FF=EXP(-Z*Z)
          FZ=-2.*Z*FF
          FZZ=(4.*Z*Z-2.)*FF
          F(1)=1.
          F(2)=FF
          W2=P(2)/P(4)
          F(3)=-W2*FZ
          F(4)=Z*F(3)
          IF(R.NE.2.)GOTO 2
          DO 1 I=1,4
          DO 1 J=I,4
 1        AM1(J,I)=0.
          W2=W2/P(4)
          AM1(3,2)=-W2*FZ
          AM1(4,2)=AM1(3,2)*Z
          AM1(3,3)=W2*FZZ
          AM1(4,3)=W2*(FZ+Z*FZZ)
          AM1(4,4)=W2*Z*(2.*FZ+FZZ)
 2        X=P(2)*FF+P(1)
          RETURN
          END
*CALL PTIME
*EXECUTE
 10000.   80.    10000.
 50.   60.    50.
 1.EД15   150.   1.EД15
90.   200.   11.   5.
 20.     100.     9.     2.5
  1.EД15   1.EД15   20.    6.
 4.   25  3  1  8  1  1  0  1  1  1.00  .000001  0 0 3  .5
*
*
