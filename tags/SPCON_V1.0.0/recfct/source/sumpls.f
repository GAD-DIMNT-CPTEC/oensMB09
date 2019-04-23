      SUBROUTINE SUMPLS(FLN,AP,AM,KDIM,LAT)
C*
C*    CALCULATES THE FOURIER REPRESENTATION OF A FIELD AT A
C*    PAIR OF LATITUDES SYMMETRICALLY LOCATED ABOUT THE
C*    EQUATOR. THE CALCULATION IS MADE USING THE SPECTRAL
C*    REPRESENTATION OF THE FIELD AND THE VALUES OF THE
C*    ASSOCIATED LEGENDRE FUNCTIONS AT THAT LATITUDE.
C*    IT IS DESIGNED TO TRIANGULAR TRUNCATION ONLY AND
C*    FOR SCALAR FIELDS.
C*
C*    ARGUMENT(DIMENSIONS)            DESCRIPTION
C*
C*    FLN(MNWV2,KDIM)        INPUT: SPECTRAL REPRESENTATION OF A
C*                                  GLOBAL FIELD.
C*    AP(IMAX+2,KDIM)       OUTPUT: FOURIER REPRESENTATION OF
C*                                  A GLOBAL FIELD AT THE
C*                                  LATITUDE IN THE NORTHERN
C*                                  HEMISPHERE AT WHICH THE
C*                                  ASSOCIATED LEGENDRE FUNCTIONS
C*                                  HAVE BEEN DEFINED.
C*    AM(IMAX+2,KDIM)       OUTPUT: FOURIER REPRESENTATION OF
C*                                  A GLOBAL FIELD AT THE
C*                                  LATITUDE IN THE SOUTHERN
C*                                  HEMISPHERE AT WHICH THE
C*                                  ASSOCIATED LEGENDRE FUNCTIONS
C*                                  HAVE BEEN DEFINED.
C*    KDIM                   INPUT: NUMBER OF VERTICAL LEVELS.
C*    LAT                    INPUT: CURRENT INDEX OF GAUSSIAN
C*                                  LOOP IN CALLING ROUTINE.
C*
      include "recfct.h"
C*
      INTEGER IMX,JMAXHF,MEND1,MEND2,MNWV2,MNWV3
      PARAMETER (IMX=IMAX+2,JMAXHF=JMAX/2,
     *           MEND1=MEND+1,MEND2=MEND+2,
     *           MNWV2=MEND1*MEND2,MNWV3=MNWV2+2*MEND1)
      INTEGER KDIM,LAT
      REAL AP(IMX,KDIM),AM(IMX,KDIM),FLN(MNWV2,KDIM),
     *     QLN(MNWV2,JMAXHF),QLNV(MNWV3,JMAXHF),S(MNWV2)
C*
      COMMON /PLNCMN/ QLN,QLNV
C*
      INTEGER MEND1D,MMAX1,L,K,MN,NN,MMAX,MSTR,MM
C*
      DO K=1,KDIM
C*
      MEND1D=2*MEND1
      MMAX1=2*MEND
      L=MEND1D+MMAX1
C*
      DO MN=1,MNWV2
      S(MN)=QLN(MN,LAT)*FLN(MN,K)
      ENDDO
C*
      DO NN=3,MEND1
      MMAX=2*(MEND2-NN)
      IF(MOD(NN-1,2).EQ.0) THEN
      MSTR=0
      ELSE
      MSTR=MEND1D
      ENDIF
      DO MM=1,MMAX
      L=L+1
      S(MM+MSTR)=S(MM+MSTR)+S(L)
      ENDDO
      ENDDO
C*
      DO MM=1,MEND1D
      AP(MM,K)=S(MM)
      AM(MM,K)=S(MM)
      ENDDO
      DO MM=1,MMAX1
      AP(MM,K)=AP(MM,K)+S(MM+MEND1D)
      AM(MM,K)=AM(MM,K)-S(MM+MEND1D)
      ENDDO
C*
      ENDDO
C*
      RETURN
      END
