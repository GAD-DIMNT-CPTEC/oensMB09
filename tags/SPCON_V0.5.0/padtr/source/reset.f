      SUBROUTINE RESET(A,N)
C*
      INTEGER N,I
      REAL*4 A(N)
C*
      DO 10 I=1,N
      A(I)=0.0E0
   10 CONTINUE
C*
      RETURN
      END
