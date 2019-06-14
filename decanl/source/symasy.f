      SUBROUTINE SYMASY(A,B,T,KDIM)
C*
C*    CONVERTS THE FOURIER REPRESENTATIONS OF A FIELD AT TWO
C*    PARALLELS AT THE SAME LATITUDE IN THE NORTHERN AND
C*    SOUTHERN HEMISPHERES INTO THE FOURIER REPRESENTATIONS
C*    OF THE SYMMETRIC AND ANTI-SYMMETRIC PORTIONS OF THAT
C*    FIELD AT THE SAME DISTANCE FROM THE EQUATOR AS THE
C*    INPUT LATITUDE CIRCLES.
C*
C*    ARGUMENT(DIMENSIONS)        DESCRIPTION
C*
C*    A(IMX,KDIM)          INPUT: FOURIER REPRESENTATION OF ONE
C*                                LATITUDE CIRCLE OF A FIELD
C*                                FROM THE NORTHERN HEMISPHERE
C*                                AT "N" LEVELS IN THE VERTICAL.
C*                        OUTPUT: FOURIER REPRESENTATION OF THE
C*                                SYMMETRIC PORTION OF A FIELD
C*                                AT THE SAME LATITUDE AS THE
C*                                INPUT, AT "N" LEVELS IN 
C*                                THE VERTICAL.
C*    B(IMX,KDIM)          INPUT: FOURIER REPRESENTATION OF ONE
C*                                LATITUDE CIRCLE OF A FIELD
C*                                FROM THE SOUTHERN HEMISPHERE
C*                                AT "N" LEVELS IN THE VERTICAL.
C*                        OUTPUT: FOURIER REPRESENTATION OF THE
C*                                ANTI-SYMMETRIC PORTION OF A
C*                                FIELD AT THE SAME LATITUDE AS
C*                                THE INPUT, AT "N" LEVELS IN
C*                                THE VERTICAL.
C*    T(IMX,KDIM)                 TEMPORARY STORAGE
C*
C*    KDIM                 INPUT: NUMBER OF LAYERS.
C*
#include <decanl.h>
C*
      INTEGER IMX
      PARAMETER (IMX=IMAX+2)
C*
      INTEGER KDIM
      REAL A(IMX,KDIM),B(IMX,KDIM),T(IMX,KDIM)
C*
      INTEGER I,K
C*
      DO K=1,KDIM
      DO I=1,IMAX
      T(I,K)=A(I,K)
      A(I,K)=T(I,K)+B(I,K)
      B(I,K)=T(I,K)-B(I,K)
      ENDDO
      ENDDO
C*
      RETURN
      END
