      SUBROUTINE BALBAK(NM,N,LOW,IGH,SCALE,M,Z)
C*
C*    ** BALBAK forms the eigenvectors of a real general matrix
C*              from the eigenvectors of that matrix
C*              transformed by BALANC.
C*
      INTEGER NM,N,LOW,IGH,M,I,J,K,II
C*
      REAL S
      REAL Z(NM,N),SCALE(N)
C*
      IF (M .EQ. 0) GOTO 200
      IF (IGH .EQ. LOW) GOTO 120
C*
      DO 110 I=LOW,IGH
      S=SCALE(I)
C*
C*    ** Left hand eigenvectors are back transformed
C*       if the foregoing statment is replaced by S = 1.0 / SCALE(I) .
C*
      DO 100 J=1,M
      Z(I,J)=Z(I,J)*S
  100 CONTINUE
C*
  110 CONTINUE
C*
C*    ** For I=LOW-1 step -1 until 1,
C*           IGH+1 step 1 until N DO -- .
C*
  120 DO 140 II=1,N
      I=II
      IF (I.GE.LOW .AND. I.LE.IGH) GOTO 140
      IF (I .LT. LOW) I=LOW-II
      K=INT(SCALE(I))
      IF (K .EQ. I) GOTO 140
C*
      DO 130 J=1,M
      S=Z(I,J)
      Z(I,J)=Z(K,J)
      Z(K,J)=S
  130 CONTINUE
C*
  140 CONTINUE
C*
  200 RETURN
      END
