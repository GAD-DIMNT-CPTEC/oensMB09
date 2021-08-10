      SUBROUTINE EPSLON(EPS)
C*
C*    EPSLON : CALCULATES AN ARRAY WHICH IS USED IN A 
C*             RECURSION RELATION TO CALCULATE THE 
C*             ASSOCIATED LEGENDRE FUNCTIONS.
C*
C*    ARGUMENT(DIMENSIONS)          DESCRIPTION
C*
C*    EPS(MNWV1)           OUTPUT : ARRAY USED IN VARIOUS
C*                                  CALCULATIONS INVOLVING
C*                                  SPHERICAL HARMONICS.
C*                                  VALUES AT THE END OF EACH
C*                                  COLUMN ARE STORED FROM MNWV0+1
C*     MEND                 INPUT : TRUNCATION OF THE TRIANGULAR
C*                                  SPECTRAL DOMAIN.
C*     MNWV1                INPUT : NUMBER OF WAVE COEFFICIENTS
C*
       include "fftpln.h"
C*
      INTEGER MEND1,MEND2,MEND3,MNWV2,MNWV3,MNWV1
      PARAMETER (MEND1=MEND+1,MEND2=MEND+2,MEND3=MEND+3,
     *           MNWV2=MEND1*MEND2,MNWV3=MNWV2+2*MEND1,
     *           MNWV1=MNWV3/2)
C*
      REAL EPS(MNWV1)
C*
      INTEGER L,NN,MMAX,MM
      REAL AM,AN
C*
      DO L=1,MEND1
      EPS(L)=0.0
      ENDDO
C*
      L=MEND1
      DO NN=2,MEND2
      MMAX=MEND3-NN
      DO MM=1,MMAX
      L=L+1
      AM=MM-1
      AN=MM+NN-2
      EPS(L)=SQRT((AN*AN-AM*AM)/(4.0*AN*AN-1.0))
      ENDDO
      ENDDO
C*
      RETURN
      END
