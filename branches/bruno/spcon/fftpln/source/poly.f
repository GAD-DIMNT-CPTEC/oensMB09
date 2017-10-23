      SUBROUTINE POLY(N,RAD,P)
C*
C*    POLY : CALCULATES THE VALUE OF THE ORDINARY 
C*           LEGENDRE FUNCTION OF GIVEN ORDER 
C*           AT A SPECIFIED LATITUDE.  USED TO
C*           DETERMINE GAUSSIAN LATITUDES.
C*
C*    POLY IS CALLED BY THE SUBROUTINE GLATS.
C*
C*    ARGUMENT(DIMENSIONS)        DESCRIPTION
C*
C*    P                   OUTPUT: VALUE OF THE ORDINARY LEGENDRE
C*                                FUNCTION OF ORDER "N" AT
C*                                COLATITUDE "RAD".
C*    N                    INPUT: ORDER OF THE ORDINARY LEGENDRE
C*                                FUNCTION WHOSE VALUE IS TO BE
C*                                CALCULATED. SET IN ROUTINE GLATS.
C*    RAD                  INPUT: COLATITUDE (IN RADIANS) AT
C*                                WHICH THE VALUE OF THE ORDINARY
C*                                LEGENDRE FUNCTION IS TO BE
C*                                CALCULATED. SET IN ROUTINE GLATS.
C*
      INTEGER N
      REAL RAD,P
C*
      INTEGER I
      REAL X,Y1,Y2,G,Y3
C*
      X=COS(RAD)
      Y1=1.0E0
      Y2=X
C*
      DO I=2,N
      G=X*Y2
      Y3=G-Y1+G-(G-Y1)/FLOAT(I)
      Y1=Y2
      Y2=Y3
      ENDDO
C*
      P=Y3
C*
      RETURN
      END
