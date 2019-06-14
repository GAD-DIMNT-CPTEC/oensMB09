      SUBROUTINE TQL2(NM,N,D,E,Z,EPS,IERR)
C*
C*    Abstract: Computes the Eigenvalues and Eigenvectors of a real
C*              Symmetric Tridiagonal Matrix Using the QL Method.
C*
      INTEGER NM,N,ITM,I,J,K,L,M,II,L1,MML,IERR
C*
      REAL EPS,B,C,F,G,H,P,R,S,ZERO,ONE,TWO
C*
      REAL D(N),E(N),Z(NM,N)
C*
C*    EPS is a Machine Dependent Parameter Specifying the
C*        Relative Precision of the Floating Point Arithmetic.
C*
C*    EPS = 2.0 ** -50 - 64 BITS
C*    EPS = 2.0 ** -20 - 32 BITS
C*
      DATA ITM /50/, ZERO /0.0E0/, ONE /1.0E0/, TWO /2.0E0/
C*
      IERR=0
C*
      DO 100 I=2,N
      E(I-1)=E(I)
  100 CONTINUE
C*
      F=ZERO
      B=ZERO
      E(N)=ZERO
C*
      DO 240 L=1,N
      J=0
      H=EPS*(ABS(D(L))+ABS(E(L)))
      IF (B .LT. H) B=H
C*
C*    Look for Small Sub-diagonal Element.
C*
      DO 110 M=L,N
      IF (ABS(E(M)) .LE. B) GOTO 120
C*
C*    E(N) is Always Zero, so there is No Exit
C*         Through the Bottom of the Loop.
C*
  110 CONTINUE
C*
  120 IF (M .EQ. L) GOTO 220
  130 IF (J .EQ. ITM) THEN
C*
C*    No Convergence to an Eigenvalue after 50 Iterations.
C*
      IERR=L
      WRITE(6,400) L
  400 FORMAT(/,' *** The',I4,'-th Eigenvalue Did Not Converge ***',/)
      RETURN
      ENDIF
C*
      J=J+1
C*
C*    Form Shift.
C*
      L1=L+1
      G=D(L)
      P=(D(L1)-G)/(TWO*E(L))
      R=SQRT(P*P+ONE)
      D(L)=E(L)/(P+SIGN(R,P))
      H=G-D(L)
C*
      DO 140 I=L1,N
      D(I)=D(I)-H
  140 CONTINUE
C*
      F=F+H
C*
C*    QL Transformation.
C*
      P=D(M)
      C=ONE
      S=ZERO
      MML=M-L
C*
C*    For I=M-1 Step -1 Until L DO -- .
C*
      DO 200 II=1,MML
      I=M-II
      G=C*E(I)
      H=C*P
C*
      IF (ABS(P) .LT. ABS(E(I))) THEN
      C=P/E(I)
      R=SQRT(C*C+ONE)
      E(I+1)=S*E(I)*R
      S=ONE/R
      C=C*S
      ELSE
      C=E(I)/P
      R=SQRT(C*C+ONE)
      E(I+1)=S*P*R
      S=C/R
      C=ONE/R
      ENDIF
C*
      P=C*D(I)-S*G
      D(I+1)=H+S*(C*G+S*D(I))
C*
C*    Form Vector.
C*
      DO 180 K=1,N
      H=Z(K,I+1)
      Z(K,I+1)=S*Z(K,I)+C*H
      Z(K,I)=C*Z(K,I)-S*H
  180 CONTINUE
C*
  200 CONTINUE
C*
      E(L)=S*P
      D(L)=C*P
      IF (ABS(E(L)) .GT. B) GOTO 130
  220 D(L)=D(L)+F
  240 CONTINUE
C*
C*    Order Eigenvalues and Eigenvectors.
C*
      DO 300 II=2,N
      I=II-1
      K=I
      P=D(I)
C*
      DO 260 J=II,N
      IF (D(J) .LT. P) THEN
      K=J
      P=D(J)
      ENDIF
  260 CONTINUE
C*
      IF (K .NE. I) THEN
      D(K)=D(I)
      D(I)=P
C*
      DO 280 J=1,N
      P=Z(J,I)
      Z(J,I)=Z(J,K)
      Z(J,K)=P
  280 CONTINUE
      ENDIF
C*
  300 CONTINUE
C*
      RETURN
      END
