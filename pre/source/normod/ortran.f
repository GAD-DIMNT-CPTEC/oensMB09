      SUBROUTINE ORTRAN(NM,N,LOW,IGH,A,ORT,Z,TOLH)
C*
C*    ** ORTRAN accumulates the orthogonal similarity
C*              tranformations used in the reduction of a real
C*              general matrix to upper Hessemberg form
C*              by ORTHES.
C*
      INTEGER NM,N,LOW,IGH,I,J,KL,MM,MP,MP1

      REAL G,TOLH
      REAL A(NM,N),Z(NM,N),ORT(N)
      REAL ZERO,ONE
      DATA ZERO /0.0E0/, ONE /1.0E0/
C*
C*    ** Initialize Z to identity matrix
C*
      DO 80 I=1,N
      DO 60 J=1, N
      Z(I,J)=ZERO
   60 CONTINUE
      Z(I,I)=ONE
   80 CONTINUE
C*
      KL=IGH-LOW-1
      IF (KL .LT. 1) GOTO 200
C*
C*    ** For MP = IGH-1 step -1 until LOW+1 DO --.
C*
      DO 140 MM=1,KL
      MP=IGH-MM
      IF (A(MP,MP-1) .EQ. ZERO) GOTO 140
      MP1=MP+1
C*
      DO 100 I=MP1,IGH
      ORT(I)=A(I,MP-1)
  100 CONTINUE
C*
      DO 130 J=MP,IGH
      G=ZERO
C*
      DO 110 I=MP,IGH
      G=G+ORT(I)*Z(I,J)
  110 CONTINUE
C*
C*    ** Divisor below is negative of H formed in ORTHES
C*       double division avoids possible underflow.
C*
      G=(G/ORT(MP))/A(MP,MP-1)
C*
      DO 120 I=MP,IGH
      Z(I,J)=Z(I,J)+G*ORT(I)
      IF (ABS(Z(I,J)) .LT. TOLH) Z(I,J)=TOLH
  120 CONTINUE
C*
  130 CONTINUE
C*
  140 CONTINUE
C*
  200 RETURN
      END
