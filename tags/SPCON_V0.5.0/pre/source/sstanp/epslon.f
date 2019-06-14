      SUBROUTINE EPSLON(EPS,MEND1,NEND2,JEND2,MNWV1)
C @(#)epslon.v1	1.7
C***********************************************************************
C
C     EPSLON : CALCULATES AN ARRAY WHICH IS USED IN A RECURSION RELATION
C              TO CALCULATE THE ASSOCIATED LEGENDRE FUNCTIONS.
C              THIS ARRAY IS ALSO USED TO CALCULATE SINE WEIGHTED
C              ASSOCIATED LEGENDRE FUNCTIONS AND COSINE WEIGHTED
C              DERIVATIVES OF ASSOCIATED LEGENDRE FUNCTIONS.
C
C***********************************************************************
C
C     EPSLON IS CALLED BY THE MAIN ROUTINE SMF.
C
C     EPSLON CALLS NO SUBROUTINES.
C
C***********************************************************************
C
C    ARGUMENT(DIMENSIONS)                       DESCRIPTION
C
C         EPS(MNWV1)             OUTPUT : ARRAY USED IN VARIOUS
C                                         CALCULATIONS INVOLVING
C                                         SPHERICAL HARMONICS.
C                                         VALUES AT THE END OF EACH
C                                         COLUMN ARE STORED FROM MNWV0+1
C            MNWV1                INPUT : NUMBER OF WAVE COEFFICIENTS
C            MEND1                INPUT : TRUNCATION OF THE RHOMBOIDAL
C            NEND2                        SPECTRAL DOMAIN. SET IN MAIN
C            JEND2                        ROUTINE "SMF".
C
C***********************************************************************
C
C     EPSLON DOES NOT REFER TO ANY COMMONS.
C
C***********************************************************************
C

      DIMENSION EPS(MNWV1)
C
      DO 10 L=1, MEND1
      EPS(L) = 0.0E0
10    CONTINUE
      L=MEND1
      DO 20 NN=2,NEND2
      IF(NN.LE.JEND2-MEND1+1) THEN
      MMAX=MEND1
      ELSE
      MMAX=JEND2-NN+1
      END IF
      DO 20 MM=1,MMAX
      L=L+1
      AM=MM-1
      AN=MM+NN-2
      EPS(L)= SQRT((AN*AN-AM*AM)/(4.0E0*AN*AN-1.0E0))
20    CONTINUE

C
      RETURN
      END
