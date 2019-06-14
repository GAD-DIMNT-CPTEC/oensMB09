      SUBROUTINE HQR2(NM,N,LOW,IGH,H,WR,WI,Z,IERR,MATZ,MACHEP,TOL,*)
C*
C*     ** HQR2 computes the eigenvalues and/or eigenvectors
C*             of a real upper Hessemberg matrix using the
C*             QR method.
C*
      INTEGER NM,N,LOW,IGH,IERR,MATZ,I,J,K,L,M,EN,LL,MM,NA,
     *        ITS,MP2,ENM2
C*
      REAL P,Q,R,S,T,X,W,Y,ZZ,NORM,MACHEP,TOL
      REAL H(NM,N),Z(NM,N),WR(N),WI(N)
C*
      LOGICAL NOTLAS
C*
      REAL ZERO,P4375,HALF,P75
      DATA ZERO /0.0E0/, P4375 /0.4375E0/,
     *     HALF /0.5E0/, P75 /0.75E0/
C*
C*     ** MACHEP is a machine dependent parameter specifying the
C*               relative precision of the floating point arithmetic.
C*               It must be recomputed and replaced for the specific
C*               machine in use.
C*
      IERR=0
      NORM=ZERO
      K = 1
C*
C*     ** Store roots isolated by BALANC and compute matrix norm.
C*
      DO 50 I=1,N
C*
      DO 40 J=K,N
      IF (ABS(H(I,J)) .LT. TOL) H(I,J)=TOL
      NORM=NORM+ABS(H(I,J))
   40 CONTINUE
C*
      K=I
      IF (I.GE.LOW .AND. I.LE.IGH) GOTO 50
      WR(I)=H(I,I)
      WI(I)=ZERO
   50 CONTINUE
C*
      EN=IGH
      T=ZERO
C*
C*     ** Search for next eigenvalues.
C*
   60 IF (EN .LT. LOW) GOTO 340
      ITS=0
      NA=EN-1
      ENM2=NA-1
C*
C*     ** Look for single small sub-diagonal element.
C*        For L=EN step -1 until LOW DO -- .
C*
   70 DO 80 LL=LOW,EN
      L=EN+LOW-LL
      IF (L .EQ. LOW) GOTO 100
      S=ABS(H(L-1,L-1))+ABS(H(L,L))
      IF (S .EQ. ZERO) S=NORM
      IF (ABS(H(L,L-1)) .LE. MACHEP*S) GOTO 100
   80 CONTINUE
C*
C*     ** Form shift.
C*
  100 X=H(EN,EN)
      IF (L .EQ. EN) GOTO 270
      Y=H(NA,NA)
      W=H(EN,NA)*H(NA,EN)
      IF (L .EQ. NA) GOTO 280
      IF (ITS .EQ. 30) GOTO 1000
      IF (ITS.NE.10 .AND. ITS.NE.20) GOTO 130
C*
C*     Form exceptional shift.
C*
      T=T+X
C*
      DO 120 I=LOW,EN
      H(I,I)=H(I,I)-X
  120 CONTINUE
C*
      S=ABS(H(EN,NA))+ABS(H(NA,ENM2))
      X=P75*S
      Y=X
      W=-P4375*S*S
  130 ITS=ITS+1
C*
C*     ** Look for two consecutive small sub-diagonal elements.
C*        For M=EN-2 step -1 until L DO -- .
C*
      DO 140 MM=L,ENM2
      M=ENM2+L-MM
      ZZ=H(M,M)
      R=X-ZZ
      S=Y-ZZ
      P=(R*S-W)/H(M+1,M)+H(M,M+1)
      Q=H(M+1,M+1)-ZZ-R-S
      R=H(M+2,M+1)
      S=ABS(P)+ABS(Q)+ABS(R)
      P=P/S
      Q=Q/S
      R=R/S
      IF (M .EQ. L) GOTO 150
      IF (ABS(H(M,M-1))*(ABS(Q)+ABS(R)) .LE. MACHEP*ABS(P)*
     *   (ABS(H(M-1,M-1))+ABS(ZZ)+ABS(H(M+1,M+1)))) GOTO 150
  140 CONTINUE
C*
  150 MP2=M+2
C*
      DO 160 I=MP2,EN
      H(I,I-2)=ZERO
      IF (I .EQ. MP2) GOTO 160
      H(I,I-3)=ZERO
  160 CONTINUE
C*
C*     ** Double QR step involving rows L to END and columns M to EN
C*
      DO 260 K=M,NA
      NOTLAS=K .NE. NA
      IF (K .EQ. M) GOTO 170
      P=H(K,K-1)
      Q=H(K+1,K-1)
      R=ZERO
      IF(NOTLAS) R=H(K+2,K-1)
      X=ABS(P)+ABS(Q)+ABS(R)
      IF (X .EQ. ZERO) GOTO 260
      P=P/X
      Q=Q/X
      R=R/X
  170 S=SIGN(SQRT(P*P+Q*Q+R*R),P)
      IF (K .EQ. M) GOTO 180
      H(K,K-1)=-S*X
      GOTO 190
  180 IF (L .NE. M) H(K,K-1)=-H(K,K-1)
  190 P=P+S
      X=P/S
      Y=Q/S
      ZZ=R/S
      Q=Q/P
      R=R/P
C*
C*     ** Row modification
C*
      DO 210 J=K,N
      P=H(K,J)+Q*H(K+1,J)
      IF (.NOT.NOTLAS) GOTO 200
      P=P+R*H(K+2,J)
      H(K+2,J)=H(K+2,J)-P*ZZ
  200 H(K+1,J)=H(K+1,J)-P*Y
      H(K,J)=H(K,J)-P*X
  210 CONTINUE
C*
      J=MIN(EN,K+3)
C*
C*     ** Column modification
C*
      DO 230 I=1,J
      P=X*H(I,K)+Y*H(I,K+1)
      IF (.NOT.NOTLAS) GOTO 220
      P=P+ZZ*H(I,K+2)
      H(I,K+2)=H(I,K+2)-P*R
  220 H(I,K+1)=H(I,K+1)-P*Q
      H(I,K)=H(I,K)-P
  230 CONTINUE
C*
      IF (MATZ .EQ. 0) GOTO 260
C*
C*     ** Accumulate transformations
C*
      DO 250 I=LOW,IGH
      P=X*Z(I,K)+Y*Z(I,K+1)
      IF (.NOT.NOTLAS) GOTO 240
      P=P+ZZ*Z(I,K+2)
      Z(I,K+2)=Z(I,K+2)-P*R
  240 IF (ABS(P) .LT. TOL) P=TOL
      Z(I,K+1)=Z(I,K+1)-P*Q
      Z(I,K)=Z(I,K)-P
  250 CONTINUE
C*
  260 CONTINUE
C*
      GOTO 70
C*
C*     ** One root found.
C*
  270 H(EN,EN)=X+T
      WR(EN)=H(EN,EN)
      WI(EN)=ZERO
      EN=NA
      GOTO 60
C*
C*     ** Two roots found.
C*
  280 P=(Y-X)*HALF
      Q=P*P+W
      ZZ=SQRT(ABS(Q))
      H(EN,EN)=X+T
      X=H(EN,EN)
      H(NA,NA)=Y+T
      IF (Q .LT. ZERO) GOTO 320
C*
C*     ** Real pair.
C*
      ZZ=P+SIGN(ZZ,P)
      WR(NA)=X+ZZ
      WR(EN)=WR(NA)
      IF (ZZ .NE. ZERO) WR(EN)=X-W/ZZ
      WI(NA)=ZERO
      WI(EN)=ZERO
C*
      IF (MATZ .EQ. 0) GOTO 330
C*
      X=H(EN,NA)
      S=ABS(X)+ABS(ZZ)
      P=X/S
      Q=ZZ/S
      R=SQRT(P*P+Q*Q)
      P=P/R
      Q=Q/R
C*
C*     ** Row  modification.
C*
      DO 290 J=NA,N
      ZZ = H(NA,J)
      H(NA,J)=Q*ZZ+P*H(EN,J)
      H(EN,J)=Q*H(EN,J)-P*ZZ
  290 CONTINUE
C*
C*     ** Column modification.
C*
      DO 300 I=1,EN
      ZZ=H(I,NA)
      H(I,NA)=Q*ZZ+P*H(I,EN)
      H(I,EN)=Q*H(I,EN)-P*ZZ
  300 CONTINUE
C*
C*     ** Accumulate transformations.
C*
      DO 310 I=LOW,IGH
      ZZ=Z(I,NA)
      Z(I,NA)=Q*ZZ+P*Z(I,EN)
      Z(I,EN)=Q*Z(I,EN)-P*ZZ
  310 CONTINUE
C*
      GOTO 330
C*
C*     ** Complex pair
C*
  320 WR(NA)=X+P
      WR(EN)=X+P
      WI(NA)=ZZ
      WI(EN)=-ZZ
C*
  330 EN=ENM2
C*
      GOTO 60
C*
  340 IF (MATZ .EQ. 0) RETURN 1
C*
C*     ** All roots found.
C*        Backsubstitute to find vectors of upper triangular form.
C*
      IF (NORM .EQ. ZERO) GOTO 1001
C*
      CALL HQR3(NM,N,LOW,IGH,H,WR,WI,Z,MACHEP,NORM)
C*
      GOTO 1001
C*
C*     ** Set error - no convergence to an
C*        eigenvalue after 30 iterations
C*
 1000 IERR = EN
 1001 RETURN
      END
