      SUBROUTINE TQL2(NM,N,D,E,Z,IERR)
C*
C*    COMPUTES THE EIGENVALUES AND EIGENVECTORS OF A REAL
C*    SYMMETRIC TRIDIAGONAL MATRIX USING THE QL METHOD
C*
C*    FORMAL ARGUMENTS:
C*
      INTEGER NM,N,IERR
      REAL D(N),E(N),Z(NM,N)
C*
C*    LOCAL PARAMETERS:
C*
      INTEGER NEXP,I,J,K,L,M,II,L1,MML
      REAL EPS,EPSMAC,B,C,F,G,H,P,R,S
C*
      IF (N .EQ. 1) RETURN
C*
C*    EPS IS A MACHINE DEPENDENT PARAMETER SPECIFYING THE
C*    RELATIVE PRECISION OF THE FLOATING POINT ARITHMETIC
C*
      EPS=EPSMAC(NEXP)
      IERR=0
C*
      DO I=2,N
      E(I-1)=E(I)
      ENDDO
C*
      F=0.0
      B=0.0
      E(N)=0.0
C*
C*    MAIN LOOP
C*
      DO L=1,N
C*
      H=EPS*(ABS(D(L))+ABS(E(L)))
      IF (B .LT. H) B=H
C*
C*    LOOK FOR SMALL SUB-DIAGONAL ELEMENT
C*
      DO M=L,N
      IF (ABS(E(M)) .LE. B) GOTO 120
C*
C*    E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C*         THROUGH THE BOTTOM OF THE LOOP
C*
      ENDDO
C*
  120 IF (M .NE. L) THEN
C*
C*    ITERATIONS LOOP
C*
      DO J=1,51
C*
C*    NO CONVERGENCE TO AN EIGENVALUE AFTER 50 ITERATIONS
C*
      IF (J .EQ. 51) THEN
      IERR=L
      WRITE(6,400) L
  400 FORMAT(/,' *** THE',I4,'-TH EIGENVALUE DID NOT CONVERGE ***',/)
      RETURN
      ENDIF
C*
C*    FORM SHIFT
C*
      L1=L+1
      G=D(L)
      P=(D(L1)-G)/(2.0*E(L))
      R=SQRT(P*P+1.0)
      D(L)=E(L)/(P+SIGN(R,P))
      H=G-D(L)
C*
      DO I=L1,N
      D(I)=D(I)-H
      ENDDO
C*
      F=F+H
C*
C*    QL TRANSFORMATION
C*
      P=D(M)
      C=1.0
      S=0.0
      MML=M-L
C*
C*    FOR I=M-1 STEP -1 UNTIL L DO -- .
C*
      DO II=1,MML
C*
      I=M-II
      G=C*E(I)
      H=C*P
C*
      IF (ABS(P) .LT. ABS(E(I))) THEN
      C=P/E(I)
      R=SQRT(C*C+1.0)
      E(I+1)=S*E(I)*R
      S=1.0/R
      C=C*S
      ELSE
      C=E(I)/P
      R=SQRT(C*C+1.0)
      E(I+1)=S*P*R
      S=C/R
      C=1.0/R
      ENDIF
C*
      P=C*D(I)-S*G
      D(I+1)=H+S*(C*G+S*D(I))
C*
C*    FORM VECTOR
C*
      DO K=1,N
      H=Z(K,I+1)
      Z(K,I+1)=S*Z(K,I)+C*H
      Z(K,I)=C*Z(K,I)-S*H
      ENDDO
C*
      ENDDO
C*
      E(L)=S*P
      D(L)=C*P
      IF (ABS(E(L)) .LE. B) GOTO 130
C*
C*    END ITERATIONS LOOP
C*
      ENDDO
C*
      ENDIF
  130 D(L)=D(L)+F
C*
C*    END MAIN LOOP
C*
      ENDDO
C*
C*    ORDER EIGENVALUES AND EIGENVECTORS
C*
      DO II=2,N
C*
      I=II-1
      K=I
      P=D(I)
C*
      DO J=II,N
      IF (D(J) .GE. P) THEN
      K=J
      P=D(J)
      ENDIF
      ENDDO
C*
      IF (K .NE. I) THEN
      D(K)=D(I)
      D(I)=P
C*
      DO J=1,N
      P=Z(J,I)
      Z(J,I)=Z(J,K)
      Z(J,K)=P
      ENDDO
      ENDIF
C*
C*    END ORDER EIGENVALUES AND EIGENVECTORS
C*
      ENDDO
C*
      RETURN
      END
