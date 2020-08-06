      SUBROUTINE FL22S(FP,FM,FLN,KDIM,LAT)
C*
C*    CALCULATES SPECTRAL REPRESENTATIONS OF
C*    GLOBAL FIELDS FROM FOURIER REPRESENTATIONS
C*    OF SYMMETRIC AND ANTI-SYMMETRIC PORTIONS
C*    OF GLOBAL FIELDS.
C*
C*    ARGUMENT(DIMENSIONS)        DESCRIPTION
C*
C*    FP(IMAX+2,KDIM)      INPUT: FOURIER REPRESENTATION OF
C*                                SYMMETRIC PORTION OF A
C*                                GLOBAL FIELD AT ONE GAUSSIAN
C*                                LATITUDE.
C*    FM(IMAX+2,KDIM)      INPUT: FOURIER REPRESENTATION OF
C*                                ANTI-SYMMETRIC PORTION OF A
C*                                GLOBAL FIELD AT ONE GAUSSIAN
C*                                LATITUDE.
C*    FLN(MNWV2,KDIM)      INPUT: SPECTRAL REPRESENTATION OF
C*                                (THE LAPLACIAN OF) A GLOBAL
C*                                FIELD. INCLUDES CONTRIBUTIONS
C*                                FROM GAUSSIAN LATITUDES UP TO
C*                                BUT NOT INCLUDING CURRENT
C*                                ITERATION OF GAUSSIAN LOOP
C*                                IN CALLING ROUTINE.
C*                        OUTPUT: SPECTRAL REPRESENTATION OF
C*                                (THE LAPLACIAN OF) A GLOBAL
C*                                FIELD. INCLUDES CONTRIBUTIONS
C*                                FROM GAUSSIAN LATITUDES UP TO
C*                                AND INCLUDING CURRENT
C*                                ITERATION OF GAUSSIAN LOOP
C*                                IN CALLING ROUTINE.
C*    KDIM                 INPUT: NUMBER OF VERTICAL LAYERS.
C*    LAT                  INPUT: CURRENT INDEX OF GAUSSIAN
C*                                LOOP IN CALLING ROUTINE.
C*
      include "decanl.h"
C*
      INTEGER IMX,JMAXHF,MEND1,MEND2,MNWV2
      PARAMETER (IMX=IMAX+2,JMAXHF=JMAX/2,
     *           MEND1=MEND+1,MEND2=MEND+2,
     *           MNWV2=MEND1*MEND2)
C*
      INTEGER KDIM,LAT
      REAL FP(IMX,KDIM),FM(IMX,KDIM),FLN(MNWV2,KDIM),
     *     QLNW(MNWV2,JMAXHF),QDER(MNWV2,JMAXHF),
     *     QLNWCS(MNWV2,JMAXHF),S(MNWV2)
C*
      INTEGER K,L,NN,MMAX,MM,MN
C*
      COMMON /PLNDRV/ QLNW,QDER,QLNWCS
C*
      DO K=1,KDIM
C*
      L=0
      DO NN=1,MEND1
      MMAX=2*(MEND2-NN)
C*
      IF(MOD(NN-1,2).EQ.0) THEN
      DO MM=1,MMAX
      L=L+1
      S(L)=FP(MM,K)
      ENDDO
      ELSE
      DO MM=1,MMAX
      L=L+1
      S(L)=FM(MM,K)
      ENDDO
      ENDIF
C*
      ENDDO
C*
      DO MN=1,MNWV2
      FLN(MN,K)=FLN(MN,K)+S(MN)*QLNW(MN,LAT)
      ENDDO
C*
      ENDDO
C*
      RETURN
      END
