      SUBROUTINE TRED2(NM,N,A,D,E,Z)
C*
C*    REDUCE A REAL SYMMETRIC MATRIX TO A SYMMETRIC
C*    TRIDIAGONAL MATRIX ACCUMUTALATING THE
C*    ORTHOGONAL TRANSFORMATIONS
C*
C*    FORMAL ARGUMENTS:
C*
      INTEGER NM,N
      REAL A(NM,N),D(N),E(N),Z(NM,N)
C*
C*    LOCAL PARAMETERS:
C*
      INTEGER I,J,K,L,II,JP1
      REAL F,G,H,HH,SCALE
C*
      DO I=1,N
      DO J=1,I
      Z(I,J)=A(I,J)
      ENDDO
      ENDDO
C*
      IF (N .GT. 1) THEN
C*
C*    MAIN LOOP: FOR I=N STEP -1 UNTIL 2 DO
C*
      DO II=2,N
      I=N+2-II
      L=I-1
      H=0.0
      SCALE=0.0
C*
C*    SCALE ROW
C*
      IF (L .GE. 2) THEN
      DO K=1,L
      SCALE=SCALE+ABS(Z(I,K))
      ENDDO
      ENDIF
C*
      IF (SCALE .NE. 0.0) THEN
C*
      DO K=1,L
      Z(I,K)=Z(I,K)/SCALE
      H=H+Z(I,K)*Z(I,K)
      ENDDO
C*
      F=Z(I,L)
      G=-SIGN(SQRT(H),F)
      E(I)=SCALE*G
      H=H-F*G
      Z(I,L)=F-G
      F=0.0
C*
      DO J=1,L
C*
      Z(J,I)=Z(I,J)/H
      G=0.0
C*
C*    FORM ELEMENT OF A*U
C*
      DO K=1,J
      G=G+Z(J,K)*Z(I,K)
      ENDDO
C*
      JP1=J+1
      IF (L .GE. JP1) THEN
      DO K=JP1,L
      G=G+Z(K,J)*Z(I,K)
      ENDDO
      ENDIF
C*
C*    FORM ELEMENT OF P
C*
      E(J)=G/H
      F=F+E(J)*Z(I,J)
C*
      ENDDO
C*
      HH=F/(H+H)
C*
C*    FORM REDUCED A
C*
      DO J=1,L
      F=Z(I,J)
      G=E(J)-HH*F
      E(J)=G
      DO K=1,J
      Z(J,K)=Z(J,K)-F*E(K)-G*Z(I,K)
      ENDDO
      ENDDO
C*
      ELSE
      E(I)=Z(I,L)
      ENDIF
C*
      D(I)=H
C*
C*    END MAIN LOOP
C*
      ENDDO
      ENDIF
C*
      D(1)=0
      E(1)=0
C*
C*    ACCUMULATION OF TRANSFORMATION MATRICES
C*
      DO I=1,N
      L=I-1
C*
      IF (D(I) .NE. 0.0) THEN
      DO J=1,L
      G=0
      DO K=1,L
      G=G+Z(I,K)*Z(K,J)
      ENDDO
      DO K=1,L
      Z(K,J)=Z(K,J)-G*Z(K,I)
      ENDDO
      ENDDO
      ENDIF
C*
      D(I)=Z(I,I)
      Z(I,I)=1.0
C*
      IF (L .GE. 1) THEN
      DO J=1,L
      Z(I,J)=0.0
      Z(J,I)=0.0
      ENDDO
      ENDIF
C*
      ENDDO
C*
      RETURN
      END
