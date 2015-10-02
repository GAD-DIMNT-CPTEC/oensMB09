      SUBROUTINE FL22S(FP,FM,FLN,KDIM,LAT)
!*
!*    CALCULATES SPECTRAL REPRESENTATIONS OF
!*    GLOBAL FIELDS FROM FOURIER REPRESENTATIONS
!*    OF SYMMETRIC AND ANTI-SYMMETRIC PORTIONS
!*    OF GLOBAL FIELDS.
!*
!*    ARGUMENT(DIMENSIONS)        DESCRIPTION
!*
!*    FP(IMAX+2,KDIM)      INPUT: FOURIER REPRESENTATION OF
!*                                SYMMETRIC PORTION OF A
!*                                GLOBAL FIELD AT ONE GAUSSIAN
!*                                LATITUDE.
!*    FM(IMAX+2,KDIM)      INPUT: FOURIER REPRESENTATION OF
!*                                ANTI-SYMMETRIC PORTION OF A
!*                                GLOBAL FIELD AT ONE GAUSSIAN
!*                                LATITUDE.
!*    FLN(MNWV2,KDIM)      INPUT: SPECTRAL REPRESENTATION OF
!*                                (THE LAPLACIAN OF) A GLOBAL
!*                                FIELD. INCLUDES CONTRIBUTIONS
!*                                FROM GAUSSIAN LATITUDES UP TO
!*                                BUT NOT INCLUDING CURRENT
!*                                ITERATION OF GAUSSIAN LOOP
!*                                IN CALLING ROUTINE.
!*                        OUTPUT: SPECTRAL REPRESENTATION OF
!*                                (THE LAPLACIAN OF) A GLOBAL
!*                                FIELD. INCLUDES CONTRIBUTIONS
!*                                FROM GAUSSIAN LATITUDES UP TO
!*                                AND INCLUDING CURRENT
!*                                ITERATION OF GAUSSIAN LOOP
!*                                IN CALLING ROUTINE.
!*    KDIM                 INPUT: NUMBER OF VERTICAL LAYERS.
!*    LAT                  INPUT: CURRENT INDEX OF GAUSSIAN
!*                                LOOP IN CALLING ROUTINE.
!*
      include "decanl.h"
!*
      INTEGER IMX,JMAXHF,MEND1,MEND2,MNWV2
      PARAMETER (IMX=IMAX+2,JMAXHF=JMAX/2,
     *           MEND1=MEND+1,MEND2=MEND+2,
     *           MNWV2=MEND1*MEND2)
!*
      INTEGER KDIM,LAT
      REAL FP(IMX,KDIM),FM(IMX,KDIM),FLN(MNWV2,KDIM),
     *     QLNW(MNWV2,JMAXHF),QDER(MNWV2,JMAXHF),
     *     QLNWCS(MNWV2,JMAXHF),S(MNWV2)
!*
      INTEGER K,L,NN,MMAX,MM,MN
!*
      COMMON /PLNDRV/ QLNW,QDER,QLNWCS
!*
      DO K=1,KDIM
!*
      L=0
      DO NN=1,MEND1
      MMAX=2*(MEND2-NN)
!*
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
!*
      ENDDO
!*
      DO MN=1,MNWV2
      FLN(MN,K)=FLN(MN,K)+S(MN)*QLNW(MN,LAT)
      ENDDO
!*
      ENDDO
!*
      RETURN
      END
