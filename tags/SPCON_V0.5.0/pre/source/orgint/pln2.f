      SUBROUTINE PLN2(SLN,COLRAD,LAT,EPS,MEND1,NEND2,JEND2,
     *                MNWV1,JMAXHF,LA1)
C*
C*    CALCULATES THE ASSOCIATED LEGENDRE FUNCTIONS AT ONE
C*    SPECIFIED LATITUDE.
C*
C*
C*    ARGUMENT(DIMENSIONS)                       DESCRIPTION
C
C        SLN (MNWV1)             OUTPUT : VALUES OF ASSOCIATED LEGENDRE
C                                         FUNCTIONS AT ONE GAUSSIAN
C                                         LATITUDE SPECIFIED BY THE
C                                         ARGUMENT "LAT".
C                                         USED FOR U,V,D(LNP)/D(PHI)_
C
C        COLRAD(JMAXHF)           INPUT : COLATITUDES OF GAUSSIAN GRID
C                                         (IN RADIANS). CALCULATED
C                                         IN ROUTINE "GLATS".
C           LAT                   INPUT : GAUSSIAN LATITUDE INDEX. SET
C                                         BY CALLING ROUTINE.
C           EPS                   INPUT : FACTOR THAT APPEARS IN RECUSIO
C                                         FORMULA OF A.L.F.
C           MEND1
C           NEND2 }               INPUT : TRUNCATION WAVE NUMBERS
C           JEND2
C           MNWV1                 INPUT : NUMBER OF ELEMENTS
C        LA1(MEND1,NEND2)         INPUT : NUMBERING ARRAY OF SLN1
C*
C*              X(1024)                   WORK AREA
C*              Y(1024)                   WORK AREA
C*
C*    PLN2 IS CALLED BY THE SUBROUTINES GRD2SP AND SP2GRD.
C*    PLN2 CALLS NO SUBROUTINES.
C*
      INTEGER LAT,MEND1,NEND2,JEND2,MNWV1,JMAXHF
      DOUBLE PRECISION SLN(MNWV1),COLRAD(JMAXHF),EPS(MNWV1)
      INTEGER LA1(MEND1,NEND2)
C*
      INTEGER IFP,MM,L,NN,MMAX,LX,LY,LZ
      DOUBLE PRECISION RTHF,ZERO,PT5,ONE,TWO,
     *                 COLR,SINLAT,COSLAT,PROD
      DOUBLE PRECISION X(1024),Y(1024)
C
      SAVE IFP,X,Y,RTHF,ZERO,PT5,ONE,TWO
      DATA IFP/1/, ZERO /0.0D0/, PT5 /0.5D0/,
     *             ONE /1.0D0/, TWO /2.0D0/
C

      IF(IFP.EQ.1) THEN
      IFP=0
      DO 10 MM=1,MEND1
      X(MM)=SQRT(TWO*DBLE(MM)+ONE)
      Y(MM)=SQRT(ONE+PT5/DBLE(MM))
   10 CONTINUE
      RTHF=SQRT(PT5)
      ENDIF
C
      COLR  =COLRAD(LAT)
      SINLAT=COS(COLR)
      COSLAT=SIN(COLR)
      PROD  =ONE
C
CVD$R NODEPCHK
      DO 20 MM=1,MEND1
      SLN(MM)=RTHF*PROD
C     LINE BELOW SHOULD ONLY BE USED WHERE EXPONENT RANGE IS LIMTED
C     IF(PROD.LT.FLIM) PROD=0.0D0
      PROD=PROD*COSLAT*Y(MM)
   20 CONTINUE

      DO 30 MM=1,MEND1
      SLN(MM+MEND1)=X(MM)*SINLAT*SLN(MM)
   30 CONTINUE
C
      IF(JEND2.EQ.NEND2+MEND1-1) THEN
      L=MEND1*2
      DO 40 NN=3,NEND2
      DO 40 MM=1,MEND1
      L=L+1
      SLN(L) =(SINLAT*SLN(L-MEND1)-EPS(L-MEND1)*SLN(L-MEND1*2))
     1        / EPS(L)
   40 CONTINUE
C
      ELSE
      DO 50 NN=3,NEND2
      IF(NN.LE.JEND2+1-MEND1) THEN
      MMAX=MEND1
      ELSE
      MMAX=JEND2+1-NN
      END IF
      DO 50 MM=1,MMAX
      LX=LA1(MM,NN  )
      LY=LA1(MM,NN-1)
      LZ=LA1(MM,NN-2)
      SLN(LX) =(SINLAT*SLN(LY)-EPS(LY)*SLN(LZ))/EPS(LX)
   50 CONTINUE
      END IF
C
      RETURN
      END
