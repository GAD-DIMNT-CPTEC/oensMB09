      SUBROUTINE SYMASY (A,B,T,IMAX,KMAX)
C*
C*    CONVERTS THE FOURIER REPRESENTATIONS OF A FIELD AT TWO
C*    PARALLELS AT THE SAME LATITUDE IN THE NORTHERN AND
C*    SOUTHERN HEMISPHERES INTO THE FOURIER REPRESENTATIONS
C*    OF THE SYMMETRIC AND ANTI-SYMMETRIC PORTIONS OF THAT
C*    FIELD AT THE SAME DISTANCE FROM THE EQUATOR AS THE
C*    INPUT LATITUDE CIRCLES.
C*
C*    ARGUMENT(DIMENSIONS)                       DESCRIPTION
C
C          A(IMAX,KMAX)           INPUT : FOURIER REPRESENTATION OF ONE
C                                         LATITUDE CIRCLE OF A FIELD
C                                         FROM THE NORTHERN HEMISPHERE
C                                         AT "N" LEVELS IN THE VERTICAL.
C                                OUTPUT : FOURIER REPRESENTATION OF THE
C                                         SYMMETRIC PORTION OF A FIELD
C                                         AT THE SAME LATITUDE AS THE
C                                         INPUT, AT "N" LEVELS IN THE
C                                         VERTICAL.
C          B(IMAX,KMAX)           INPUT : FOURIER REPRESENTATION OF ONE
C                                         LATITUDE CIRCLE OF A FIELD
C                                         FROM THE SOUTHERN HEMISPHERE
C                                         AT "N" LEVELS IN THE VERTICAL.
C                                OUTPUT : FOURIER REPRESENTATION OF THE
C                                         ANTI-SYMMETRIC PORTION OF A
C                                         FIELD AT THE SAME LATITUDE AS
C                                         THE INPUT, AT "N" LEVELS IN
C                                         THE VERTICAL.
C          T(IMAX,KMAX)                   TEMPORARY STORAGE
C
C             IMAX                INPUT : NUMBER OF GRID POINTS ON ONE
C                                         GAUSSIAN LATITUDE LINE
C             KMAX                INPUT : NUMBER OF LEVELS.
C*
C*    SYMASY IS CALLED BY THE SUBROUTINES GRDTOS.
C*    SYMASY CALLS NO SUBROUTINES.
C*
      INTEGER IMAX,KMAX
      REAL A(IMAX,KMAX),B(IMAX,KMAX),T(IMAX,KMAX)
C*
      INTEGER I,K
C*
C     ON INPUT A CORRESP. TO N. HEMIS. FOURIERS
C     ON INPUT B CORRESP. TO S. HEMIS. FOURIERS
C     ON OUTPUT A CORRESP. TO MULTIPLIERS OF SYM. PLN
C     ON OUTPUT B CORRESP. TO MULTIPLIERS OF ANTISYM. PLN
C*
      DO 10 K=1,KMAX
      DO 10 I=1,IMAX
      T(I,K)=A(I,K)
      A(I,K)=T(I,K)+B(I,K)
      B(I,K)=T(I,K)-B(I,K)
   10 CONTINUE
C*
      RETURN
      END
