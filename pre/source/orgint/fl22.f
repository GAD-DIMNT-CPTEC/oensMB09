      SUBROUTINE FL22(FP,FM,FLN,QLN,
     *                IMAX,MEND1,NEND1,JEND1,MNWV2,KMAX,S)
C*
C*    CALCULATES SPECTRAL REPRESENTATIONS OF GLOBAL FIELDS FROM
C*    FOURIER REPRESENTATIONS OF SYMMETRIC AND ANTI-SYMMETRIC
C*    PORTIONS OF GLOBAL FIELDS.
C*
C*    ARGUMENT(DIMENSIONS)                       DESCRIPTION
C*
C         FP(IMAX,KMAX)           INPUT :  FOURIER REPRESENTATION OF
C                                          SYMMETRIC PORTION OF A
C                                          GLOBAL FIELD AT ONE GAUSSIAN
C                                          LATITUDE.
C         FM(IMAX,KMAX)           INPUT :  FOURIER REPRESENTATION OF
C                                          ANTI-SYMMETRIC PORTION OF A
C                                          GLOBAL FIELD AT ONE GAUSSIAN
C                                          LATITUDE.
C        FLN(MNWV2,KMAX)          INPUT :  SPECTRAL REPRESENTATION OF
C                                          (THE LAPLACIAN OF) A
C                                          GLOBAL FIELD. INCLUDES
C                                          CONTRIBUTIONS FROM GAUSSIAN
C                                          LATITUDES UP TO BUT NOT
C                                          INCLUDING CURRENT ITERATION
C                                          OF GAUSSIAN LOOP IN CALLING
C                                          ROUTINE.
C                                OUTPUT :  SPECTRAL REPRESENTATION OF
C                                          (THE LAPLACIAN OF) A
C                                          GLOBAL FIELD. INCLUDES
C                                          CONTRIBUTIONS FROM GAUSSIAN
C                                          LATITUDES UP TO AND
C                                          INCLUDING CURRENT ITERATION
C                                          OF GAUSSIAN LOOP IN CALLING
C                                          ROUTINE.
C         QLN(MNWV2)              INPUT :  VALUES OF (THE LAPLACIAN OF)
C                                          THE ASSOCIATED LEGENDRE
C                                          FUNCTIONS AT ONE GAUSSIAN
C                                          LATITUDE.
C         S  (MNWV2,2)            INPUT :  WORK AREA
C
C             IMAX                INPUT :  NUMBER OF GRIDS ON ONE GAUSS-
C                                          IAN LATITUDE
C             MEND1                        ZONAL TRUNCATION WAVE NUMBER
C             NEND1  }            INPUT :  TRUNCATION WAVE NUMBER
C             JEND1
C             MNWV2               INPUT :  (NUMBER OF OF WAVE
C                                          COEFFICIENTS) *2, (REAL,IMAG)
C             KMAX                INPUT :  NUMBER OF VERTICAL LEVELS.
C*
C*    FL22 IS CALLED BY THE SUBROUTINES GRDTOS.
C*    FL22 CALLS NO SUBROUTINES.
C*
      INTEGER IMAX,MEND1,NEND1,JEND1,MNWV2,KMAX
      REAL FP(IMAX,KMAX),FM(IMAX,KMAX),
     *     QLN(MNWV2),FLN(MNWV2,KMAX),S(MNWV2,2)
C*
      INTEGER IFP,MEND2,IFAC,NFAC,NRES,MNSTR,MNRES
      INTEGER ITR,K,MM,MNEND,MN,NN,MMAX,L
      INTEGER MNROW(10)
C*
      SAVE IFP,MEND2,IFAC,NFAC,NRES,MNSTR,MNRES,MNROW
      DATA IFP/1/
C*
      IF(IFP.EQ.1) THEN
C*
      MEND2=2*MEND1
C*    IFAC IS THE LARGEST FACTOR THAT SATISFIES  2**IFAC.LE.NEND1
      IFAC=0
    1 CONTINUE
      IFAC=IFAC+1
      IF (2**IFAC .LE. NEND1) GOTO 1
      IFAC=IFAC-1
C*
      NFAC=2**IFAC
      NRES=NEND1-NFAC
      MNSTR=MEND2*NFAC
      MNRES=MEND2*NRES
CVD$L NOVECTOR
      DO 2 ITR=1,IFAC
      MNROW(ITR)=MEND2*2**ITR
    2 CONTINUE
C*
      IFP=0
      END IF
C*
      IF(JEND1.EQ.MEND1+NEND1-1) THEN
C================================C
C...PARALLELOGRAMIC TRUNCATION...C
C================================C
C
CVD$R NODEPCHK
CVD$R NOLSTVAL
      DO 100 K=1,KMAX
C
      DO 120 MM=1,MEND2
      S(MM      ,1)= FP(MM,K)
      S(MM+MEND2,1)= FM(MM,K)
  120 CONTINUE
C
      IF(IFAC.GE.2) THEN
      DO 140 ITR=1,IFAC-1
      MNEND=MNROW(ITR)
      DO 140 MN=1,MNEND
      S(MN+MNEND,1)=S(MN,1)
  140 CONTINUE
      END IF
C
      MNEND=MNROW(IFAC)
      DO 160 MN=1,MNEND
      S(MN      ,2)=S(MN,1)*QLN(MN      )
  160 CONTINUE
C
      IF(MNRES.GT.0) THEN
      DO 180 MN=1,MNRES
      S(MN+MNSTR,2)=S(MN,1)*QLN(MN+MNSTR)
  180 CONTINUE
      END IF
C
      DO 190 MN=1,MNWV2
      FLN(MN,K)=FLN(MN,K)+S(MN,2)
  190 CONTINUE
C
  100 CONTINUE
C
      ELSE
C============================
C...PENTAGONAL TRUNCATION...=
C============================
      DO 200 K=1,KMAX
      L=0
      DO 220 NN=1,NEND1
        IF(NN.LE.JEND1+1-MEND1) THEN
        MMAX=MEND2
        ELSE
        MMAX=2*(JEND1+1-NN)
        END IF
      IF(MOD(NN-1,2).EQ.0) THEN
      DO 240 MM=1,MMAX
      L=L+1
      S(L,2)=FP(MM,K)
  240 CONTINUE
      ELSE
      DO 260 MM=1,MMAX
      L=L+1
      S(L,2)=FM(MM,K)
  260 CONTINUE
      END IF
  220 CONTINUE
C
      DO 280 MN=1,MNWV2
      FLN(MN,K)=FLN(MN,K)+S(MN,2)*QLN(MN)
  280 CONTINUE
C
  200 CONTINUE
C
      END IF
C
      RETURN
      END
