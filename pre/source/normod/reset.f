      SUBROUTINE RESET(N,A,B)
C*
      INTEGER N,I
      REAL B
      REAL A(*)
C*
      DO 10 I=1,N
      A(I)=B
   10 CONTINUE
C*
      RETURN
      END
