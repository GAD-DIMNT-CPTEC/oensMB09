      SUBROUTINE ORTHES(NM,N,LOW,IGH,A,ORT,TOLH)
C*
C*    ** ORTHES reduces a real general matrix to upper
C*              Hessemberg form using orthogonal similarity.
C*
      INTEGER NM,N,LOW,IGH,I,J,M,II,JJ,LA,MP,KP1
C*
      REAL F,G,H,SCALE,TOLH
      REAL A(NM,N),ORT(IGH)
      REAL ZERO
      DATA ZERO /0.0E0/
C*
      LA=IGH-1
      KP1=LOW+1
      IF (LA .LT. KP1) GOTO 200
C*
      DO 180 M=KP1,LA
      H=ZERO
      ORT(M)=ZERO
      SCALE=ZERO
C*
C*    ** Scale column.
C*
      DO 90 I=M,IGH
      SCALE=SCALE+ABS(A(I,M-1))
   90 CONTINUE
C*
      IF (SCALE .EQ. ZERO) GOTO 180
      MP=M+IGH
C*
C*    ** For I = IGH step -1 until M DO -- .
C*
      DO 100 II=M,IGH
      I=MP-II
      ORT(I)=A(I,M-1)/SCALE
      H=H+ORT(I)*ORT(I)
  100 CONTINUE
C*
      G=-SIGN(SQRT(H),ORT(M))
      H=H-ORT(M)*G
      ORT(M)=ORT(M)-G
C*
C*    ** Form (I-(U*UT)/H) * A .
C*
      DO 130 J=M,N
      F=ZERO
C*
C*    ** For I = IGH step -1 until M DO -- .
C*
      DO 110 II=M,IGH
      I=MP-II
      F=F+ORT(I)*A(I,J)
  110 CONTINUE
C*
      F=F/H
C*
      DO 120 I=M,IGH
      A(I,J)=A(I,J)-F*ORT(I)
      IF (ABS(A(I,J)) .LT. TOLH) A(I,J)=TOLH
  120 CONTINUE
C*
  130 CONTINUE
C*
C*    ** Form (I-(U*UT)/H) * A * (I-(U*UT)/H) .
C*
      DO 160 I=1,IGH
      F=ZERO
C*
C*    ** For J = IGH step -1 until M DO -- .
C*
      DO 140 JJ=M,IGH
      J=MP-JJ
      F=F+ORT(J)*A(I,J)
  140 CONTINUE
C*
      F=F/H
C*
      DO 150 J=M,IGH
      A(I,J)=A(I,J)-F*ORT(J)
      IF (ABS(A(I,J)) .LT. TOLH) A(I,J)=TOLH
  150 CONTINUE
C*
  160 CONTINUE
C*
      ORT(M)=SCALE*ORT(M)
      A(M,M-1)=SCALE*G
      IF (ABS(A(M,M-1)) .LT. TOLH) A(M,M-1)=TOLH
  180 CONTINUE
C*
  200 RETURN
      END
