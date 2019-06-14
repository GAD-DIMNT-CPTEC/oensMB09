      SUBROUTINE SUMPLN(FLN,AP,AM,QLN,S)
C @(#)sumpls.v1 1.7
C***********************************************************************
C
C     SUMPLS : CALCULATES THE FOURIER REPRESENTATION OF A FIELD AT A
C              PAIR OF LATITUDES SYMMETRICALLY LOCATED ABOUT THE
C              EQUATOR.  THE CALCULATION IS MADE USING THE SPECTRAL
C              REPRESENTATION OF THE FIELD AND THE VALUES OF THE
C              ASSOCIATED LEGENDRE FUNCTIONS AT THAT LATITUDE.
C
C***********************************************************************
C
C     SUMPLS IS CALLED BY THE SUBROUTINES GLOOP AND GWATER.
C
C     SUMPLS CALLS NO SUBROUTINES.
C
C***********************************************************************
C
C    ARGUMENT(DIMENSIONS)                       DESCRIPTION
C
C             FLN(MNWV2)          INPUT :  SPECTRAL REPRESENTATION OF A
C                                          GLOBAL FIELD.
C              AP(IMAX)          OUTPUT :  FOURIER REPRESENTATION OF
C                                          A GLOBAL FIELD AT THE
C                                          LATITUDE IN THE NORTHERN
C                                          HEMISPHERE AT WHICH THE
C                                          ASSOCIATED LEGENDRE FUNCTIONS
C                                          HAVE BEEN DEFINED. (SEE
C                                          DESCRIPTION OF "QLN" BELOW).
C              AM(IMAX)          OUTPUT :  FOURIER REPRESENTATION OF
C                                          A GLOBAL FIELD AT THE
C                                          LATITUDE IN THE SOUTHERN
C                                          HEMISPHERE AT WHICH THE
C                                          ASSOCIATED LEGENDRE FUNCTIONS
C                                          HAVE BEEN DEFINED (SEE
C                                          DESCRIPTION OF "QLN" BELOW).
C              QLN(MNWV2)         INPUT :  VALUES OF THE ASSOCIATED
C                                          LEGENDRE FUNCTIONS AT ONE
C                                          LATITUDE IN THE NORTHERN
C                                          HEMISPHERE. IN THE SOUTHERN
C                                          HEMISPHERE, THE SAME VALUE
C                                          IS USED FOR SYMMETRIC
C                                          FUNCTIONS; THE NEGATIVE OF
C                                          THE VALUE IS USED FOR
C                                          ANTI-SYMMETRIC FUNCTIONS.
C
C              S  (MNWV2)                  WORK AREA
C
C                  IMAX                 :  NUMBER OF GRIDS ON A GAUSSIAN
C                                          LATITUDE
C                  MEND1                :
C                  NEND1                : } TRUNCATION WAVE NUMBERS
C                  JEND1                :
C
C               JEND1=NEND1___________
C                          !         /
C                          !        /
C                          !       /
C                          !      /
C                          !     /
C                          !    /          TRIANGULAR TRUNCATION
C                          !   /
C                          !  /
C                          ! /
C                          !/________!
C                          1         MEND1=JEND1
C
C                  MNWV2                :  (NUMBER OF WAVECOEFFICIENTS)
C                                           * 2 ,   (REAL,IMAG)
C***********************************************************************
C*
      INCLUDE "reshsst.h"
C*
      PARAMETER(MEND2=2*MEND1,MMAX1=MEND2-2,MEND3=MEND2+MMAX1,
     *          MEND4=MEND2-4,NEND2=(NEND1/2)*2,
     *          MNWV2=MEND1*(MEND1+1),IMX=IMAX+2)
C
      DIMENSION AP(IMX),AM(IMX),QLN(MNWV2),FLN(MNWV2),S(MNWV2)
C
      DO MN=1,MNWV2
      S(MN)=QLN(MN)*FLN(MN)
      ENDDO
C*
      L=MEND3
      MSTR=MEND2
      MMAX=MEND4
C*
      DO NN=3,NEND2,2
C*
*vdir nodep
      DO MM=1,MMAX
      S(MM)=S(MM)+S(MM+L)
      ENDDO
C*
      L=L+MMAX
      MMAX=MMAX-2
C*
*vdir nodep
      DO MM=1,MMAX
      S(MM+MSTR)=S(MM+MSTR)+S(MM+L)
      ENDDO
C*
      L=L+MMAX
      MMAX=MMAX-2

      ENDDO
C*
      IF (MOD(NEND1,2) .NE. 0) THEN
*vdir novector
      DO MM=1,MMAX
      S(MM)=S(MM)+S(MM+L)
      ENDDO
      ENDIF
C*
      DO MM=1,MEND2
      AP(MM)=S(MM)
      AM(MM)=S(MM)
      ENDDO
      DO MM=1,MMAX1
      AP(MM)=AP(MM)+S(MM+MEND2)
      AM(MM)=AM(MM)-S(MM+MEND2)
      ENDDO
C*
      RETURN
      END
