      SUBROUTINE POLY(N,RAD,P)
C*
C*    CALCULATES THE VALUE OF THE ORDINARY LEGENDRE FUNCTION
C*    OF GIVEN ORDER AT A SPECIFIED LATITUDE.  USED TO
C*    DETERMINE GAUSSIAN LATITUDES.
C*
C*    ARGUMENT(DIMENSIONS)                       DESCRIPTION
C
C             N                   INPUT : ORDER OF THE ORDINARY LEGENDRE
C                                         FUNCTION WHOSE VALUE IS TO BE
C                                         CALCULATED. SET IN ROUTINE
C                                         "GLATS".
C            RAD                  INPUT : COLATITUDE (IN RADIANS) AT
C                                         WHICH THE VALUE OF THE ORDINAR
C                                         LEGENDRE FUNCTION IS TO BE
C                                         CALCULATED. SET IN ROUTINE
C                                         "GLATS".
C             P                  OUTPUT : VALUE OF THE ORDINARY LEGENDRE
C                                         FUNCTION OF ORDER "N" AT
C                                         COLATITUDE "RAD".
C*
C*    POLY IS CALLED BY THE SUBROUTINE GLATS.
C*    POLY CALLS NO SUBROUTINES.
C*    POLY DOES NOT REFER TO ANY COMMONS.
C*
      INTEGER N
      DOUBLE PRECISION RAD,P
C*
      INTEGER I
      DOUBLE PRECISION X,Y1,Y2,G,Y3,ONE
      DATA ONE /1.0D0/
C*
      X=COS(RAD)
      Y1=ONE
      Y2=X
      DO 10 I=2,N
      G=X*Y2
      Y3=G-Y1+G-(G-Y1)/DBLE(I)
      Y1=Y2
      Y2=Y3
   10 CONTINUE
      P=Y3
C*
      RETURN
      END
