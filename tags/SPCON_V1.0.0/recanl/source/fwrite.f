      SUBROUTINE FWRITE(N,A)
C*
      include "recanl.h"
C*
      INTEGER N,K,J,I
      REAL A(IMAX,JMAX,KMAX)
      REAL*4 BUF(IMAX,JMAX)
C*
      DO K=1,KMAX
      DO J=1,JMAX
      DO I=1,IMAX
      BUF(I,J)=A(I,J,K)
      ENDDO
      ENDDO
      WRITE(N)BUF
      ENDDO
C*
      RETURN
      END
