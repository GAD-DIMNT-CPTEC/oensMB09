      SUBROUTINE FILTER(NM,NI,NJ,A,B,C)
C*
      INTEGER NM,NI,NJ,N,J
      REAL B,C
      REAL A(NM,*)
C*
      DO 10 J=1,NJ
      DO 10 N=1,NI
      IF (ABS(A(N,J)) .LE. C) A(N,J)=B
   10 CONTINUE
C*
   20 CONTINUE
C*
      RETURN
      END
