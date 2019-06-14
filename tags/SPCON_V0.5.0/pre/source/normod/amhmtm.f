      SUBROUTINE AMHMTM(DEL,RPI,SV,P1,P2,AM,HM,TM,KMAX,KMAXM)
C*
C***********************************************************************
C
C     AMHMTM : CALCULATES THE MATRIX NECESSARY TO INTEGRATE THE
C              HYDROSTATIC EQUATION ACCORDING TO THE ARAKAWA VERTICAL
C              FINITE DIFFERENCING SCHEME.  ALSO CALCULATES THE "PI"
C              RATIOS NECESSARY TO INTEGRATE THE THERMODYNAMIC
C              EQUATION IN A SEMI-IMPLICIT MANNER.
C
C***********************************************************************
C
C     AMHMTM CALLS THE FOLLOWING SUBROUTINE :  IMINV
C
C***********************************************************************
C
C    ARGUMENT(DIMENSIONS)                       DESCRIPTION
C
C          DEL(KMAX)              INPUT : SIGMA SPACING FOR EACH LAYER.
C                                         COMPUTED IN ROUTINE "SETSIG".
C          RPI(KMAXM)             INPUT : RATIOS OF "PI" AT ADJACENT
C                                         LAYERS. COMPUTED IN ROUTINE
C                                         "SETSIG".
C           SV(KMAX)             OUTPUT : SV(L)=-DEL(L).
C           P1(KMAX)             OUTPUT : P1(L)=1/RPI(L); P1(KMAX) = 0.
C           P2(KMAX)             OUTPUT : P2(L+1)=RPI(L); P2(1) = 0.
C         AM(KMAX,KMAX)          OUTPUT : "HM" MATRIX DIVIDED BY SQUARE
C                                         OF EARTH RADIUS FOR LAPLACIAN.
C         HM(KMAX,KMAX)          OUTPUT : MATRIX RELATING GEOPOTENTIAL
C                                         TO TEMPERATURE.
C         TM(KMAX,KMAX)          OUTPUT : INVERSE OF "HM" MATRIX.
C
C***********************************************************************
C*
      INTEGER KMAX,KMAXM,K,I,J
      REAL DET,ERSQIV
C*
      REAL ZERO,HALF,ONE
      DATA ZERO /0.0E0/, HALF /0.5E0/, ONE /1.0E0/
C*
      REAL CP,GASR,ER
      DATA CP /1004.6E0/, GASR /287.05E0/, ER /6.37E6/
C*
      REAL DEL(KMAX),RPI(KMAXM),SV(KMAX),P1(KMAX),P2(KMAX),
     *          AM (KMAX,KMAX),HM(KMAX,KMAX),TM(KMAX,KMAX)
      INTEGER LLL(1024),MMM(1024)
C
      ERSQIV=ONE/(ER*ER)
C
      DO 5 K=1,KMAX*KMAX
      HM(K,1)=ZERO
      TM(K,1)=ZERO
5     CONTINUE
C*
CVD$R NOVECTOR
C*vdir novector
      DO 6 K=1,KMAXM
      HM(K,K)=ONE
      TM(K,K)=HALF*CP*(RPI(K)-ONE)
6     CONTINUE
C*
      DO 66 K=1,KMAXM
      HM(K,K+1)=-ONE
      TM(K,K+1)=HALF*CP*(ONE-ONE/RPI(K))
66    CONTINUE
      DO 7 K=1,KMAX
      HM(KMAX,K)=DEL(K)
      TM(KMAX,K)=GASR*DEL(K)
7     CONTINUE
C*
      CALL IMINV(HM,KMAX,DET,LLL,MMM)
C*
      DO 8 I=1,KMAX
      DO 8 J=1,KMAX
      AM(I,J)=ZERO
      DO 8 K=1,KMAX
      AM(I,J)=AM(I,J)+HM(I,K)*TM(K,J)
8     CONTINUE
C*
C*    HERE IS A GOOD PLACE TO DIVIDE BY A**2 FOR LAPLACIAN
C*    STORE AM IN TM AND DIVIDE AM
C*
CVD$L VECTOR
      DO 10 K=1,KMAX*KMAX
      TM(K,1)=AM(K,1)
      HM(K,1)=AM(K,1)
      AM(K,1)=AM(K,1)*ERSQIV
10    CONTINUE
C*
      CALL IMINV(TM,KMAX,DET,LLL,MMM)
C*
CVD$R NOVECTOR
C*vdir novector
      DO 9 K=1,KMAX
      SV(K)=-DEL(K)
9     CONTINUE
      DO 11 K=1,KMAXM
      P1(K)=ONE/RPI(K)
      P2(K+1)=RPI(K)
11    CONTINUE
      P1(KMAX)=ZERO
      P2(1)=ZERO
C*
      PRINT 333
333   FORMAT(/,'SHALOM  AMHMTM',/)
C*
      RETURN
      END
