      SUBROUTINE SETSIG(CI,SI,DEL,SL,CL,RPI,KMAX,KMAXM,KMAXP)
C*
C***********************************************************************
C
C     SETSIG : CALCULATES QUANTITIES RELATED TO THE DISCRETIZATION OF
C              THE SIGMA COORDINATE AXIS IN THE NMC SPECTRAL MODEL.
C
C***********************************************************************
C
C     SETSIG CALLS NO SUBROUTINES.
C
C***********************************************************************
C
C    ARGUMENT(DIMENSIONS)                       DESCRIPTION
C
C           CI(KMAXP)            OUTPUT : SIGMA VALUE AT EACH LEVEL.
C           SI(KMAXP)            OUTPUT : SI(L)=1.0-CI(L).
C           DEL(KMAX)            OUTPUT : SIGMA SPACING FOR EACH LAYER.
C           SL(KMAX)             OUTPUT : SIGMA VALUE AT MIDPOINT OF
C                                         EACH LAYER : (K=287/1005)
C
C                                                                     1
C                                             +-                   + ---
C                                             !     K+1         K+1!  K
C                                             !SI(L)   - SI(L+1)   !
C                                     SL(L) = !--------------------!
C                                             !(K+1) (SI(L)-SI(L+1)!
C                                             +-                  -+
C
C           CL(KMAX)             OUTPUT : CL(L)=1.0-SL(L).
C           RPI(KMAXM)           OUTPUT : RATIOS OF "PI" AT ADJACENT
C                                         LAYERS :
C
C                                                  +-     -+ K
C                                                  !SL(L+1)!
C                                         RPI(L) = !-------!
C                                                  ! SL(L) !
C                                                  +-     -+
C
C***********************************************************************
C*
      INTEGER KMAX,KMAXM,KMAXP,K
      REAL RK,RK1,DIF,SIRK,SIRK1
C*
      REAL ZERO,ONE
      DATA ZERO /0.0E0/, ONE /1.0E0/
C*
      REAL CP,GASR
      DATA CP /1004.6E0/, GASR /287.05E0/
C*
      REAL CI(KMAXP),SI(KMAXP),DEL(KMAX),SL(KMAX),CL(KMAX),RPI(KMAXM)
C
      WRITE(6,98) KMAX,KMAXP,KMAXM
   98 FORMAT(1X,'BEGIN SETSIG  KMAX=',I2,'  KMAXP=',I2,'  KMAXM=',I2)
C
CVD$R NOVECTOR
C*vdir novector
      CI(1)=ZERO
      DO 10 K=1,KMAX
      CI(K+1)=CI(K)+DEL(K)
10    CONTINUE
      CI(KMAXP)=ONE
C*
      DO 20 K=1,KMAXP
      SI(K)=ONE-CI(K)
20    CONTINUE
C*
      RK=GASR/CP
      RK1=RK+ONE
      DO 30 K=1,KMAX
C     DIF=SI(K)**RK1-SI(K+1)**RK1
      SIRK=EXP(RK1*LOG(SI(K)))
      IF(K.LE.KMAX-1) THEN
      SIRK1=EXP(RK1*LOG(SI(K+1)))
      ELSE
      SIRK1=ZERO
      ENDIF
      DIF=SIRK-SIRK1
      DIF=DIF/(RK1*(SI(K)-SI(K+1)))
C     SL(K)=DIF**(ONE/RK)
      SL(K)=EXP(LOG(DIF)/RK)
      CL(K)=ONE-SL(K)
30    CONTINUE
C
C     COMPUTE PI RATIOS FOR TEMP. MATRIX.
      DO 40 K=1,KMAXM
C40   RPI(K)=(SL(K+1)/SL(K))**RK
40    RPI(K)=EXP(RK*LOG(SL(K+1)/SL(K)))
C
      DO 50 K=1, KMAXP
      WRITE(6,100) K, CI(K), SI(K)
100   FORMAT (1X, 'LEVEL=', I2, 2X, 'CI=', F6.3, 2X, 'SI=', F6.3)
50    CONTINUE
      WRITE(6,97)
97    FORMAT (1X)
      DO 60 K=1,KMAX
      WRITE(6,101) K, CL(K), SL(K), DEL(K)
101   FORMAT (1X, 'LAYER=', I2, 2X, 'CL=', F6.3, 2X, 'SL=', F6.3, 2X,
     1 'DEL=', F6.3)
60    CONTINUE
      WRITE(6,102) (RPI(K), K=1, KMAXM )
102   FORMAT (1X, 'RPI=',  8(F6.3,2X))
      WRITE(6,99)
99    FORMAT (/,'SHALOM SETSIG',/)
C*
      RETURN
      END
