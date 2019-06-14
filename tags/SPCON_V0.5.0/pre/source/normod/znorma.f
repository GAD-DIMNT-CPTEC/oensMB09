      SUBROUTINE ZNORMA(NM,N,WR,WI,Z,MATZ,H,TOLW,TOLZ)
C*
C*    ** ZNORMA normalizes and filters the eigenvectors and filters the
C*              eigenvalues.
C*
C*       It sets ZZ = a + b * i, corresponding to the maximum absolute
C*       value of the eigenvector, to:
C*
C*       a) 1.0 + 0.0   * i - if B .EQ. 0.0
C*       b) 1.0 + (b/a) * i - if  ABS(a) .GE. ABS(b)
C*       c) 1.0 + (a/b) * i - if  ABS(a) .LT. ABS(b)
C*
      INTEGER NM,N,MATZ,I,J,IC,J1
C*
      REAL ZZ,TOLZ,TOLW,DIV
      REAL Z(NM,N),H(NM,N),WR(N),WI(N)
      REAL ZERO,ONE
      DATA ZERO /0.0E0/, ONE /1.0E0/
C*
      IF (MATZ .EQ. 2) RETURN
C*
      DO 900 J=1,N
      DO 900 I=1,N
      H(I,J)=Z(I,J)
  900 CONTINUE
C*
      DO 910 J=1,N
C*
      IF (WI(J) .EQ. ZERO) THEN
C*
      ZZ=ZERO
      DO 940 I=1,N
      ZZ=MAX(ZZ,ABS(H(I,J)))
      IF (ZZ .EQ. ABS(H(I,J))) IC=I
  940 CONTINUE
C*
      DO 950 I=1,N
      Z(I,J)=H(I,J)/H(IC,J)
  950 CONTINUE
C*
      ELSEIF (WI(J) .GT. ZERO) THEN
C*
      ZZ=ZERO
      J1=J+1
      DO 960 I=1,N
      DIV=H(I,J)*H(I,J)+H(I,J1)*H(I,J1)
      ZZ=MAX(ZZ,DIV)
      IF (ZZ .EQ. DIV) IC=I
  960 CONTINUE
      IF (ABS(H(IC,J)) .LT. ABS(H(IC,J1))) THEN
      DIV=ONE/H(IC,J1)
      ELSE
      DIV=ONE/H(IC,J)
      ENDIF
      IF (DIV .NE. ZERO) THEN
      DO 970 I=1,N
      Z(I,J)=H(I,J)*DIV
      Z(I,J1)=H(I,J1)*DIV
  970 CONTINUE
      ENDIF
C*
      ENDIF
C*
  910 CONTINUE
C*
      IF (MATZ .EQ. 3) RETURN
C*
      DIV=ZERO
      DO 980 J=1,N
      ZZ=SQRT(WR(J)*WR(J)+WI(J)*WI(J))
      DIV=MAX(DIV,ZZ)
  980 CONTINUE
      IF (DIV .LE. ZERO) DIV=ONE
C*
      DO 990 J=1,N
      IF (ABS(WR(J)/DIV) .LT. TOLW) WR(J)=ZERO
      IF (ABS(WI(J)/DIV) .LT. TOLW) WI(J)=ZERO
      DO 990 I=1,N
      IF (ABS(Z(I,J)) .LT. TOLZ) Z(I,J)=ZERO
  990 CONTINUE
C*
      RETURN
      END
