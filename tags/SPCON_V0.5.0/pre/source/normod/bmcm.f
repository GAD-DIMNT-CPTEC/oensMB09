      SUBROUTINE BMCM(TOV,P1,P2,H1,H2,DEL,CI,BM,CM,DT,SV,AM,KMAX,KMAXP)
C*
C***********************************************************************
C
C     BMCM : CALCULATES ARRAYS USED IN THE SEMI-IMPLICIT INTEGRATION
C            SCHEME DUE TO TERMS ARISING FROM THE THERMODYNAMIC AND
C            DIVERGENCE EQUATIONS.
C
C***********************************************************************
C
C     BMCM CALLS NO SUBROUTINES.
C
C***********************************************************************
C
C
C    ARGUMENT(DIMENSIONS)                       DESCRIPTION
C
C          TOV(KMAX)              INPUT : = 300 DEG. K AT ALL LEVELS.
C                                         SET IN MAIN ROUTINE "SMF".
C           P1(KMAX)              INPUT : RECIPROCALS OF "PI" RATIOS,
C                                         COMPUTED IN ROUTINE "AMHMTM".
C           P2(KMAX)              INPUT : "PI" RATIOS CALCULATED IN
C                                         ROUTINE "AMHMTM".
C           H1(KMAX)             OUTPUT : H1(L)=P1(L)*TOV(L+1)-TOV(L).
C                                         H1(KMAX)=0.
C           H2(KMAX)             OUTPUT : H2(L)=TOV(L)-P2(L)*TOV(L-1).
C                                         H2(1)=0.
C          DEL(KMAX)              INPUT : SIGMA SPACING FOR EACH LAYER,
C                                         COMPUTED IN ROUTINE "SETSIG".
C          CI(KMAXP)              INPUT : SIGMA VALUE FOR EACH LEVEL,
C                                         COMPUTED IN ROUTINE "SETSIG".
C         BM(KMAX,KMAX)          OUTPUT : MATRIX RELATING TEMPERATURE
C                                         TENDENCY TO THE DIVERGENCE.
C         CM(KMAX,KMAX)          OUTPUT : MATRIX RESULTING FROM PRODUCT
C                                         OF "AM" AND "BM" AND FURTHER
C                                         ADDITION OF "TOV" TERMS.
C            DT                   INPUT : TIME STEP (SEC). SET IN MAIN
C                                         ROUTINE "SMF".
C           SV(KMAX)              INPUT : NEGATIVE OF "DEL", COMPUTED IN
C                                         ROUTINE "AMHMTM".
C         AM(KMAX,KMAX)           INPUT : MATRIX RELATING GEOPOTENTIAL
C                                         TO TEMPERATURE, COMPUTED IN
C                                         ROUTINE "AMHMTM".
C
C***********************************************************************
C*
      INTEGER KMAX,KMAXP,K,I,J
      REAL RK,RAA,DT
C*
      REAL ZERO,HALF
      DATA ZERO /0.0E0/, HALF /0.5E0/
C*
      REAL CP,GASR,ER
      DATA CP /1004.6E0/, GASR /287.05E0/, ER /6.37E6/
C*
      REAL X1(64),X2(64)
      REAL BM(KMAX,KMAX),CM(KMAX,KMAX),
     *     TOV(KMAX),P1(KMAX ),P2(KMAX),H1(KMAX),H2(KMAX),
     *     DEL(KMAX),CI(KMAXP),SV(KMAX),AM(KMAX,KMAX)
C
      RK=GASR /CP
C
CVD$R NOVECTOR
C*vdir novector
      DO 1 K=1,KMAX-1
      H1(K)=P1(K)*TOV(K+1)-TOV(K)
1     CONTINUE
      H1(KMAX)=ZERO
      H2(1)=ZERO
      DO 2 K=2,KMAX
      H2(K)=TOV(K)-P2(K)*TOV(K-1)
2     CONTINUE
C*
      DO 7 K=1,KMAX
      X1(K)=RK*TOV(K)+HALF*(CI(K+1)*H1(K)+CI(K)*H2(K))/DEL(K)
      X2(K)=HALF*(H1(K)+H2(K))/DEL(K)
7     CONTINUE
C*
      DO 33 J=1,KMAX
      DO 33 K=1,KMAX
      BM(K,J)=-X1(K)*DEL(J)
33    CONTINUE
      DO 34 K=1,KMAX
      DO 34 J=1,K
      BM(K,J)=BM(K,J)+X2(K)*DEL(J)
34    CONTINUE
      DO 3 K=1, KMAX
      BM(K,K)=BM(K,K)-HALF*H2(K)
3     CONTINUE
C*
      RAA= GASR/ER**2
      DO 4 I=1,KMAX
      DO 4 J=1,KMAX
      CM(I,J)=ZERO
      DO 44 K=1,KMAX
      CM(I,J)=CM(I,J)+AM(I,K)*BM(K,J)
44    CONTINUE
      CM(I,J)=(CM(I,J)+RAA*TOV(I)*SV(J))*DT*DT
4     CONTINUE
C*
      RETURN
      END
