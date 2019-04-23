      SUBROUTINE ABCX(A,B,C,N,M,L)
C*
      INTEGER N,M,L
      REAL A(N,M),B(M,L),C(N,L)
C*
      INTEGER J,I,K
C*
      DO J=1,L
      DO I=1,N
      C(I,J)=0.0
      DO K=1,M
      C(I,J)=C(I,J)+A(I,K)*B(K,J)
      ENDDO
      ENDDO
      ENDDO
C*
      RETURN
      END
