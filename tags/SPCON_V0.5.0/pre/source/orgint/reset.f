      SUBROUTINE RESET(DATA,N)
C*
C*    SETS DATA(N) TO ZERO
C*
      INTEGER N
      REAL DATA(*)
C*
      INTEGER I
      REAL ZERO
      DATA ZERO /0.0/
C*
      DO I=1,N
      DATA(I)=ZERO
      ENDDO
C*
      RETURN
      END
