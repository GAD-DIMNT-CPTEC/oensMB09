      SUBROUTINE POLY(N,RAD,P)
C @(#)poly.v1	1.7
C
C***********************************************************************
C
C     POLY : CALCULATES THE VALUE OF THE ORDINARY LEGENDRE FUNCTION
C            OF GIVEN ORDER AT A SPECIFIED LATITUDE.  USED TO
C            DETERMINE GAUSSIAN LATITUDES.
C
C***********************************************************************
C
C     POLY IS CALLED BY THE SUBROUTINE GLATS.
C
C     POLY CALLS NO SUBROUTINES.
C
C***********************************************************************
C
C    ARGUMENT(DIMENSIONS)                       DESCRIPTION
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
C
C***********************************************************************
C
C     POLY DOES NOT REFER TO ANY COMMONS.
C
C***********************************************************************
C

      X = COS(RAD)
      Y1 = 1.0E0
      Y2=X
      DO 1 I=2,N
      G=X*Y2
      Y3=G-Y1+G-(G-Y1)/ FLOAT(I)
      Y1=Y2
      Y2=Y3
1     CONTINUE
      P=Y3

      RETURN
      END
