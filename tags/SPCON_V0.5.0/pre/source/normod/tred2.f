      SUBROUTINE TRED2(NM,N,A,D,E,Z)
C*
      INTEGER I,J,K,L,N,II,NM,JP1
      REAL A(NM,N),D(N),E(N),Z(NM,N)
      REAL F,G,H,HH,SCALE
      REAL ZERO,ONE
      DATA ZERO /0.0E0/, ONE/1.0E0/
C*
      DO 100 I=1,N
      DO 100 J=1,I
      Z(I,J)=A(I,J)
  100 CONTINUE                         	
C*
      IF (N .EQ. 1) GOTO 320
C*    for I=N step -1 until 2 do
      DO 300 II=2,N
      I=N+2-II
      L=I-1
      H=ZERO
      SCALE=ZERO
      IF (L .LT. 2) GOTO 130
C*    scale row (algol tol then not needed)
      DO 120 K=1,L
      SCALE=SCALE+ABS(Z(I,K))
  120 CONTINUE
C*
      IF (SCALE .NE. ZERO) GOTO 140
  130 E(I)=Z(I,L)
      GOTO 290
C*
  140 DO 150 K=1,L
      Z(I,K)=Z(I,K)/SCALE
      H=H+Z(I,K)*Z(I,K)
  150 CONTINUE
C*
      F=Z(I,L)
      G=-SIGN(SQRT(H),F)
      E(I)=SCALE*G
      H=H-F*G
      Z(I,L)=F-G
      F=ZERO
C*
      DO 240 J=1,L
      Z(J,I)=Z(I,J)/H
      G=ZERO
C*    form element of A*U
      DO 180 K=1,J
      G=G+Z(J,K)*Z(I,K)
  180 CONTINUE
C*
      JP1=J+1
      IF (L .LT. JP1) GO TO 220
C*
      DO 200 K=JP1,L
      G=G+Z(K,J)*Z(I,K)
  200 CONTINUE
C*    form element of P
  220 E(J)=G/H
      F=F+E(J)*Z(I,J)
  240 CONTINUE
C*
      HH=F/(H+H)
C*    form reduced A
      DO 260 J=1,L
      F=Z(I,J)
      G=E(J)-HH*F
      E(J)=G
C*
      DO 260 K=1,J
      Z(J,K)=Z(J,K)-F*E(K)-G*Z(I,K)
  260 CONTINUE
C*
  290 D(I)=H
  300 CONTINUE
C*
  320 D(1)=ZERO
      E(1)=ZERO
C*    accumulation of transformation matrices
      DO 500 I=1,N
      L=I-1
      IF (D(I) .EQ. ZERO) GOTO 380
C*
      DO 360 J=1,L
      G=ZERO
C*
      DO 340 K=1,L
      G=G+Z(I,K)*Z(K,J)
  340 CONTINUE
C*
      DO 360 K=1,L
      Z(K,J)=Z(K,J)-G*Z(K,I)
  360 CONTINUE
C*
  380 D(I)=Z(I,I)
      Z(I,I)=ONE
      IF (L .LT. 1) GOTO 500
C*
      DO 400 J=1,L
      Z(I,J)=ZERO
      Z(J,I)=ZERO
  400 CONTINUE
C*
  500 CONTINUE
C*
      RETURN
      END
