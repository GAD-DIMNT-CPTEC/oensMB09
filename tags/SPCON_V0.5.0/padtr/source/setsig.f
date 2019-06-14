      SUBROUTINE SETSIG(DEL,SI,SL,CI,KMAX,KMAXP)
C*
C*    SETSIG : CALCULATES QUANTITIES RELATED TO THE
C*             DISCRETIZATION OF THE SIGMA COORDINATE AXIS.
C*
C*    CI(KMAXP)            OUTPUT : SIGMA VALUE AT EACH LEVEL.
C*    SI(KMAXP)            OUTPUT : SI(L)=1.0-CI(L).
C*    DEL(KMAX)            OUTPUT : SIGMA SPACING FOR EACH LAYER.
C*    SL(KMAX)             OUTPUT : SIGMA VALUE AT MIDPOINT OF
C*                                  EACH LAYER : (K=287/1005)
C*
C*                                        1
C*                +-                  -+ ---
C*                |     K+1         K+1|  K
C*                |SI(L)   - SI(L+1)   |
C*    SL(L)   =   |--------------------|
C*                |(K+1) (SI(L)-SI(L+1)|
C*                +-                  -+
C*
      INTEGER KMAX,KMAXP,K
C*
      REAL CP,GASR,RK,RK1,SIRK,SIRK1,DIF,DELS
C*
      REAL CI(KMAXP),SI(KMAXP),DEL(KMAX),SL(KMAX)
C*
      DELS=0.0E0
      CI(1)=0.0E0
      DO 10 K=1,KMAX
      DELS=DELS+DEL(K)
      CI(K+1)=CI(K)+DEL(K)
   10 CONTINUE
      CI(KMAXP)=1.0E0
C*
      CP=1005.0E0
      GASR=287.05E0
C*
      RK=GASR/CP
      RK1=RK+1.0E0
      DO 20 K=1,KMAXP
      SI(K)=1.0E0-CI(K)
   20 CONTINUE
C*
      DO 30 K=1,KMAX
C*    DIF=SI(K)**RK1-SI(K+1)**RK1
      SIRK =EXP(RK1*LOG(SI(K)))
      IF(K.LT.KMAX) THEN
      SIRK1=EXP(RK1*LOG(SI(K+1)))
      ELSE
      SIRK1=0.0E0
      ENDIF
      DIF=SIRK-SIRK1
      DIF=DIF/(RK1*(SI(K)-SI(K+1)))
C*    SL(K)=DIF**(1.0/RK)
      SL(K)=EXP(LOG(DIF)/RK)
   30 CONTINUE
C*
      RETURN
      END
