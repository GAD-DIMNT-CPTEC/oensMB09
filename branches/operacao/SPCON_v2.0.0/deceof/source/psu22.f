      SUBROUTINE PSU22(AM,AP,BM,BP,FLN,LAT)
!*
!*    CALCULATES THE SPECTRAL REPRESENTATIONS OF THE HORIZONTAL
!*    VORTICITY   OF PSEUDO-VECTOR FIELDS FROM THE FOURIER
!*    REPRESENTATIONS OF THE SYMMETRIC AND ANTI-SYMMETRIC
!*    PORTIONS OF THE TWO INDIVIDUAL FIELDS.
!*
!*    ARGUMENT(DIMENSIONS)         DESCRIPTION
!*
!*    AM(IMAX+2,KMAX)       INPUT: FOURIER REPRESENTATION OF
!*                                 ANTI-SYMMETRIC PORTION OF
!*                                 ZONAL PSEUDO-WIND FIELD AT
!*                                 ONE GAUSSIAN LATITUDE.
!*    AP(IMAX+2,KMAX)       INPUT: FOURIER REPRESENTATION OF
!*                                 SYMMETRIC PORTION OF ZONAL
!*                                 PSEUDO-WIND FIELD AT ONE
!*                                 GAUSSIAN LATITUDE.
!*    BM(IMAX+2,KMAX)       INPUT: FOURIER REPRESENTATION OF
!*                                 ANTI-SYMMETRIC PORTION OF
!*                                 MERIDIONAL PSEUDO-WIND FIELD
!*                                 AT ONE GAUSSIAN LATITUDE.
!*    BP(IMAX+2,KMAX)       INPUT: FOURIER REPRESENTATION OF
!*                                 SYMMETRIC PORTION OF
!*                                 MERIDIONAL PSEUDO-WIND FIELD
!*                                 AT ONE GAUSSIAN LATITUDE.
!*    FLN(MNWV2,KMAX)       INPUT: SPECTRAL REPRESENTATION OF
!*                                 THE VORTICITY  OF THE GLOBAL
!*                                 WIND FIELD. INCLUDES
!*                                 CONTRIBUTIONS FROM GAUSSIAN
!*                                 LATITUDES UP TO BUT NOT
!*                                 INCLUDING CURRENT ITERATION
!*                                 OF GAUSSIAN LOOP IN CALLING
!*                                 ROUTINE.
!*                         OUTPUT: SPECTRAL REPRESENTATION OF
!*                                 THE VORTICITY  OF THE GLOBAL
!*                                 WIND FIELD. INCLUDES
!*                                 CONTRIBUTIONS FROM GAUSSIAN
!*                                 LATITUDES UP TO AND
!*                                 INCLUDING CURRENT ITERATION
!*                                 OF GAUSSIAN LOOP IN CALLING
!*                                 ROUTINE.
!*    LAT                   INPUT: CURRENT INDEX OF GAUSSIAN
!*                                 LOOP IN CALLING ROUTINE.
!*
      include "decanl.h"
!*
      INTEGER IMX,JMAXHF,MEND1,MEND2,MNWV2
      PARAMETER (IMX=IMAX+2,JMAXHF=JMAX/2,
     *           MEND1=MEND+1,MEND2=MEND+2,
     *           MNWV2=MEND1*MEND2)
!*
      INTEGER LAT,K,L,NN,MMAX,MM,MN
      REAL AM(IMX,KMAX),AP(IMX,KMAX),
     *     BM(IMX,KMAX),BP(IMX,KMAX),
     *     FLN(MNWV2,KMAX),S(MNWV2),
     *     QLNW(MNWV2,JMAXHF),QDER(MNWV2,JMAXHF),
     *     QLNWCS(MNWV2,JMAXHF)
!*
      COMMON /PLNDRV/ QLNW,QDER,QLNWCS
!*
      DO K=1,KMAX
!*
      L=0
      DO NN=1,MEND1
      MMAX=2*(MEND2-NN)
!*
      IF (MOD(NN-1,2) .EQ. 0) THEN
      DO MM=1,MMAX
      L=L+1
      S(L)=AM(MM,K)
      ENDDO
      ELSE
      DO MM=1,MMAX
      L=L+1
      S(L)=AP(MM,K)
      ENDDO
      ENDIF
!*
      ENDDO
!*
      DO MN=1,MNWV2
      FLN(MN,K)=FLN(MN,K)+S(MN)*QDER(MN,LAT)
      ENDDO
!*
      L=0
      DO NN=1,MEND1
      MMAX=2*(MEND2-NN)
!*
      IF (MOD(NN-1,2) .EQ. 0) THEN
      DO MM=1,MMAX,2
      L=L+1
      S(2*L-1)=-BP(MM+1,K)
      S(2*L  )=+BP(MM  ,K)
      ENDDO
      ELSE
      DO MM=1,MMAX,2
      L=L+1
      S(2*L-1)=-BM(MM+1,K)
      S(2*L  )=+BM(MM  ,K)
      ENDDO
      ENDIF
!*
      ENDDO
!*
      DO MN=1,MNWV2
      FLN(MN,K)=FLN(MN,K)+S(MN)*QLNWCS(MN,LAT)
      ENDDO
!*
      ENDDO
!*
      RETURN
      END
