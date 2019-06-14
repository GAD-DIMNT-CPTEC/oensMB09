      SUBROUTINE BALANC(NM,N,A,LOW,IGH,SCALE)
C*
C*    ** BALANC balances a real general matrix, and isolates
C*    **        eigenvalues whenever possible.
C*
      LOGICAL NOCONV
C*
      INTEGER NM,N,LOW,IGH,I,J,K,L,M,JJ,IEXC
C*
      REAL C,F,G,R,S,B2,RADIX
      REAL A(NM,*),SCALE(*)
C*
      REAL ZERO,P95,ONE,TWO
      DATA ZERO /0.0E0/, P95/0.95E0/, ONE /1.0E0/, TWO /2.0E0/
C*
C*    ** RADIX is a machine dependent parameter specifying
C*             the base of the machine floating pont representation.
C*
      RADIX=TWO
C*
      B2=RADIX*RADIX
      K=1
      L=N
      GOTO 100
C*
C*    ** In-line procedure for row and column exchange.
C*
   20 SCALE(M)=FLOAT(J)
      IF (J .EQ. M) GOTO 50
C*
      DO 30 I=1,L
      F=A(I,J)
      A(I,J)=A(I,M)
      A(I,M)=F
   30 CONTINUE
C*
      DO 40 I=K,N
      F=A(J,I)
      A(J,I)=A(M,I)
      A(M,I)=F
   40 CONTINUE
C*
   50 GOTO (80,130) IEXC
C*
C*    ** Search for rows isolating an eigenvalue and push them down.
C*
   80 IF (L .EQ. 1) GOTO 280
      L=L-1
C*
C*    ** For J = L  step -1 until 1 DO -- .
C*
  100 DO 120 JJ=1,L
      J=L+1-JJ
C*
      DO 110 I=1,L
      IF (I .EQ. J) GOTO 110
      IF (A(J,I) .NE. ZERO) GOTO 120
  110 CONTINUE
C*
      M=L
      IEXC=1
      GOTO 20
  120 CONTINUE
C*
      GOTO 140
C*
C*    ** Search for columns isolating an eigenvalue and push them left.
C*
  130 K=K+1
C*
  140 DO 170 J=K,L
C*
      DO 150 I=K,L
      IF (I .EQ. J) GOTO 150
      IF (A(I,J) .NE. ZERO) GOTO 170
  150 CONTINUE
C*
      M=K
      IEXC=2
      GOTO 20
  170 CONTINUE
C*
C*    ** Now balance the submatrix in rows K to L.
C*
      DO 180 I=K,L
      SCALE(I)=ONE
  180 CONTINUE
C*
C*    ** Interative loop for norm reduction.
C*
  190 NOCONV=.FALSE.
C*
      DO 270 I=K,L
      C=ZERO
      R=ZERO
C*
      DO 200 J=K,L
      IF (J .EQ. I) GOTO 200
      C=C+ABS(A(J,I))
      R=R+ABS(A(I,J))
  200 CONTINUE
C*
C*    ** Guard against zero C or R due to underflow.
C*
      IF (C.EQ.ZERO .OR. R.EQ.ZERO) GOTO 270
      G=R/RADIX
      F=ONE
      S=C+R
  210 IF (C .GE. G) GOTO 220
      F=F*RADIX
      C=C*B2
      GOTO 210
  220 G=R*RADIX
  230 IF (C .LT.G) GOTO 240
      F=F/RADIX
      C=C/B2
      GOTO 230
C*
C*    ** Now balance
C*
  240 IF ((C+R)/F .GE. P95*S) GOTO 270
      G=ONE/F
      SCALE(I)=SCALE(I)*F
      NOCONV=.TRUE.
C*
      DO 250 J=K,N
      A(I,J)=A(I,J)*G
  250 CONTINUE
C*
      DO 260 J=1,L
      A(J,I)=A(J,I)*F
  260 CONTINUE
C*
  270 CONTINUE
C*
      IF(NOCONV) GOTO 190
C*
  280 LOW=K
      IGH=L
C*
      RETURN
      END
