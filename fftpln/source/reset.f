      SUBROUTINE RESET(DATA,N)
C*
      INTEGER N
      REAL DATA(N)
C*
      INTEGER I
C*
      DO I=1,N
      DATA(I)=0.0E0
      ENDDO
C*
      RETURN
      END
