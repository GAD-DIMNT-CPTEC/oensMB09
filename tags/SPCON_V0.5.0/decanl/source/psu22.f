      SUBROUTINE PSU22(AM,AP,BM,BP,FLN,LAT)
C*
C*    CALCULATES THE SPECTRAL REPRESENTATIONS OF THE HORIZONTAL
C*    VORTICITY   OF PSEUDO-VECTOR FIELDS FROM THE FOURIER
C*    REPRESENTATIONS OF THE SYMMETRIC AND ANTI-SYMMETRIC
C*    PORTIONS OF THE TWO INDIVIDUAL FIELDS.
C*
C*    ARGUMENT(DIMENSIONS)         DESCRIPTION
C*
C*    AM(IMAX+2,KMAX)       INPUT: FOURIER REPRESENTATION OF
C*                                 ANTI-SYMMETRIC PORTION OF
C*                                 ZONAL PSEUDO-WIND FIELD AT
C*                                 ONE GAUSSIAN LATITUDE.
C*    AP(IMAX+2,KMAX)       INPUT: FOURIER REPRESENTATION OF
C*                                 SYMMETRIC PORTION OF ZONAL
C*                                 PSEUDO-WIND FIELD AT ONE
C*                                 GAUSSIAN LATITUDE.
C*    BM(IMAX+2,KMAX)       INPUT: FOURIER REPRESENTATION OF
C*                                 ANTI-SYMMETRIC PORTION OF
C*                                 MERIDIONAL PSEUDO-WIND FIELD
C*                                 AT ONE GAUSSIAN LATITUDE.
C*    BP(IMAX+2,KMAX)       INPUT: FOURIER REPRESENTATION OF
C*                                 SYMMETRIC PORTION OF
C*                                 MERIDIONAL PSEUDO-WIND FIELD
C*                                 AT ONE GAUSSIAN LATITUDE.
C*    FLN(MNWV2,KMAX)       INPUT: SPECTRAL REPRESENTATION OF
C*                                 THE VORTICITY  OF THE GLOBAL
C*                                 WIND FIELD. INCLUDES
C*                                 CONTRIBUTIONS FROM GAUSSIAN
C*                                 LATITUDES UP TO BUT NOT
C*                                 INCLUDING CURRENT ITERATION
C*                                 OF GAUSSIAN LOOP IN CALLING
C*                                 ROUTINE.
C*                         OUTPUT: SPECTRAL REPRESENTATION OF
C*                                 THE VORTICITY  OF THE GLOBAL
C*                                 WIND FIELD. INCLUDES
C*                                 CONTRIBUTIONS FROM GAUSSIAN
C*                                 LATITUDES UP TO AND
C*                                 INCLUDING CURRENT ITERATION
C*                                 OF GAUSSIAN LOOP IN CALLING
C*                                 ROUTINE.
C*    LAT                   INPUT: CURRENT INDEX OF GAUSSIAN
C*                                 LOOP IN CALLING ROUTINE.
C*
#include <decanl.h>
C*
      INTEGER IMX,JMAXHF,MEND1,MEND2,MNWV2
      PARAMETER (IMX=IMAX+2,JMAXHF=JMAX/2,
     *           MEND1=MEND+1,MEND2=MEND+2,
     *           MNWV2=MEND1*MEND2)
C*
      INTEGER LAT,K,L,NN,MMAX,MM,MN
      REAL AM(IMX,KMAX),AP(IMX,KMAX),
     *     BM(IMX,KMAX),BP(IMX,KMAX),
     *     FLN(MNWV2,KMAX),S(MNWV2),
     *     QLNW(MNWV2,JMAXHF),QDER(MNWV2,JMAXHF),
     *     QLNWCS(MNWV2,JMAXHF)
C*
      COMMON /PLNDRV/ QLNW,QDER,QLNWCS
C*
      DO K=1,KMAX
C*
      L=0
      DO NN=1,MEND1
      MMAX=2*(MEND2-NN)
C*
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
C*
      ENDDO
C*
      DO MN=1,MNWV2
      FLN(MN,K)=FLN(MN,K)+S(MN)*QDER(MN,LAT)
      ENDDO
C*
      L=0
      DO NN=1,MEND1
      MMAX=2*(MEND2-NN)
C*
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
C*
      ENDDO
C*
      DO MN=1,MNWV2
      FLN(MN,K)=FLN(MN,K)+S(MN)*QLNWCS(MN,LAT)
      ENDDO
C*
      ENDDO
C*
      RETURN
      END
