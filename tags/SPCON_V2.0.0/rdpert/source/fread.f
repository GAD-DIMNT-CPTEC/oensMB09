      SUBROUTINE FREAD(N,A)
C*
      include "rdpert.h"
C*
      INTEGER N,K,J,I
      REAL A(IMAX,JMAX,KMAX)
      REAL*4 BUF(IMAX,JMAX)
C*
      DO K=1,KMAX
      READ(N)BUF
      DO J=1,JMAX
      DO I=1,IMAX
      A(I,J,K)=BUF(I,J)
      ENDDO
      ENDDO
      ENDDO
C*
      RETURN
      END
