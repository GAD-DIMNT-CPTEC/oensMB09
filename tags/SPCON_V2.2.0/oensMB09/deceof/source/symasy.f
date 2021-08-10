      SUBROUTINE SYMASY(A,B,T,KDIM)
!*
!*    CONVERTS THE FOURIER REPRESENTATIONS OF A FIELD AT TWO
!*    PARALLELS AT THE SAME LATITUDE IN THE NORTHERN AND
!*    SOUTHERN HEMISPHERES INTO THE FOURIER REPRESENTATIONS
!*    OF THE SYMMETRIC AND ANTI-SYMMETRIC PORTIONS OF THAT
!*    FIELD AT THE SAME DISTANCE FROM THE EQUATOR AS THE
!*    INPUT LATITUDE CIRCLES.
!*
!*    ARGUMENT(DIMENSIONS)        DESCRIPTION
!*
!*    A(IMX,KDIM)          INPUT: FOURIER REPRESENTATION OF ONE
!*                                LATITUDE CIRCLE OF A FIELD
!*                                FROM THE NORTHERN HEMISPHERE
!*                                AT "N" LEVELS IN THE VERTICAL.
!*                        OUTPUT: FOURIER REPRESENTATION OF THE
!*                                SYMMETRIC PORTION OF A FIELD
!*                                AT THE SAME LATITUDE AS THE
!*                                INPUT, AT "N" LEVELS IN 
!*                                THE VERTICAL.
!*    B(IMX,KDIM)          INPUT: FOURIER REPRESENTATION OF ONE
!*                                LATITUDE CIRCLE OF A FIELD
!*                                FROM THE SOUTHERN HEMISPHERE
!*                                AT "N" LEVELS IN THE VERTICAL.
!*                        OUTPUT: FOURIER REPRESENTATION OF THE
!*                                ANTI-SYMMETRIC PORTION OF A
!*                                FIELD AT THE SAME LATITUDE AS
!*                                THE INPUT, AT "N" LEVELS IN
!*                                THE VERTICAL.
!*    T(IMX,KDIM)                 TEMPORARY STORAGE
!*
!*    KDIM                 INPUT: NUMBER OF LAYERS.
!*
      include "decanl.h"
!*
      INTEGER IMX
      PARAMETER (IMX=IMAX+2)
!*
      INTEGER KDIM
      REAL A(IMX,KDIM),B(IMX,KDIM),T(IMX,KDIM)
!*
      INTEGER I,K
!*
      DO K=1,KDIM
      DO I=1,IMAX
      T(I,K)=A(I,K)
      A(I,K)=T(I,K)+B(I,K)
      B(I,K)=T(I,K)-B(I,K)
      ENDDO
      ENDDO
!*
      RETURN
      END
