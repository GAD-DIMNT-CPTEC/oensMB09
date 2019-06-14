      SUBROUTINE HQR3(NM,N,LOW,IGH,H,WR,WI,Z,MACHEP,NORM)
C*
C*     ** HQR3 backsubstitutes to find
C*             vectors of upper triangular form.
C*
      INTEGER NM,N,LOW,IGH,I,J,K,M,EN,II,JJ,NA,NN,ENM2
C*
      REAL P,Q,R,S,T,X,W,Y,AR,AI,BR,BI,RA,SA,ZZ,NORM,MACHEP
      REAL H(NM,N),Z(NM,N),WR(N),WI(N)
      REAL ZD,A1,A2,A3,A4
      REAL ZERO,ONE,TWO
      DATA ZERO /0.0E0/, ONE /1.0E0/, TWO /2.0E0/
C*
      ZD(A1,A2,A3,A4)=(A1*A3+A2*A4)/(A3*A3+A4*A4)
C*
C*     ** For EN=N step -1 until 1 DO -- .
C*
      DO 800 NN=1,N
      EN=N+1-NN
      P=WR(EN)
      Q=WI(EN)
      NA=EN-1
      IF (Q) 710,600,800
C*
C*     ** Real vector.
C*
  600 M=EN
      H(EN,EN)=ONE
      IF (NA .EQ. 0) GOTO 800
C*
C*     ** For I=EN-1 step -1 until 1 DO -- .
C*
      DO 700 II=1,NA
      I=EN-II
      W=H(I,I)-P
      R=H(I,EN)
      IF (M .GT. NA) GOTO 620
C*
      DO 610 J=M,NA
      R=R+H(I,J)*H(J,EN)
  610 CONTINUE
C*
  620 IF (WI(I) .GE. ZERO) GOTO 630
      ZZ=W
      S=R
      GOTO 700
  630 M=I
      IF (WI(I) .NE. ZERO) GOTO 640
      T=W
      IF (W .EQ. ZERO) T=MACHEP*NORM
      H(I,EN)=-R/T
      GOTO 700
C*
C*     ** Solve real equations.
C*
  640 X=H(I,I+1)
      Y=H(I+1,I)
      Q=(WR(I)-P)*(WR(I)-P)+WI(I)*WI(I)
      T=(X*S-ZZ*R)/Q
      H(I,EN)=T
      IF (ABS(X) .LE. ABS(ZZ)) GOTO 650
      H(I+1,EN)=(-R-W*T)/X
      GOTO 700
  650 H(I+1,EN)=(-S-Y*T)/ZZ
C*
  700 CONTINUE
C*
C*     ** End real vector.
C*
      GOTO 800
C*
C*     ** Complex vector.
C*
  710 M=NA
C*
C*     ** Last vector component chosen imaginary so that
C*        eigenvector matrix is triangular.
C*
      IF (ABS(H(EN,NA)) . LE. ABS(H(NA,EN))) GOTO 720
      H(NA,NA)=Q/H(EN,NA)
      H(NA,EN)=-(H(EN,EN)-P)/H(EN,NA)
      GOTO 730
  720 H(NA,NA)=ZD(ZERO,-H(NA,EN),H(NA,NA)-P,Q)
      H(NA,EN)=ZD(-H(NA,EN),ZERO,H(NA,NA)-P,Q)
  730 H(EN,NA)=ZERO
      H(EN,EN)=ONE
      ENM2=NA-1
      IF (ENM2 .EQ. 0) GOTO 800
C*
C*     ** For I=EN-2 step -1 until 1 DO -- .
C*
      DO 790 II=1,ENM2
      I=NA-II
      W=H(I,I)-P
      RA=ZERO
      SA=H(I,EN)
C*
      DO 760 J=M,NA
      RA=RA+H(I,J)*H(J,NA)
      SA=SA+H(I,J)*H(J,EN)
  760 CONTINUE
C*
      IF (WI(I) .GE. ZERO) GOTO 770
      ZZ=W
      R=RA
      S=SA
      GOTO 790
  770 M=I
      IF (WI(I) .NE. ZERO) GOTO 780
      H(I,NA)=ZD(-RA,-SA,W,Q)
      H(I,EN)=ZD(-SA,RA,W,Q)
      GOTO 790
C*
C*     ** Solve complex equations.
C*
  780 X=H(I,I+1)
      Y=H(I+1,I)
      AR=X*R-ZZ*RA+Q*SA
      AI=X*S-ZZ*SA-Q*RA
      BR=(WR(I)-P)*(WR(I)-P)+WI(I)*WI(I)-Q*Q
      BI=(WR(I)-P)*TWO*Q
      IF (BR.EQ.ZERO .AND. BI.EQ.ZERO) BR=MACHEP*NORM*
     *   (ABS(W)+ABS(Q)+ABS(X)+ABS(Y)+ABS(ZZ))
      H(I,NA)=ZD(AR,AI,BR,BI)
      H(I,EN)=ZD(AI,-AR,BR,BI)
      IF (ABS(X) .LE. (ABS(ZZ)+ABS(Q))) GOTO 785
      H(I+1,NA)=(-RA-W*H(I,NA)+Q*H(I,EN))/X
      H(I+1,EN)=(-SA-W*H(I,EN)-Q*H(I,NA))/X
      GOTO 790
  785 H(I+1,NA)=ZD(-R-Y*H(I,NA),-S-Y*H(I,EN),ZZ,Q)
      H(I+1,EN)=ZD(-S-Y*H(I,EN),R+Y*H(I,NA),ZZ,Q)
  790 CONTINUE
C*
C*     ** End complex vector.
C*
  800 CONTINUE
C*
C*     ** End back substitution.
C*        vectors of isolated roots.
C*
      DO 840 I=1,N
      IF (I.GE.LOW .AND. I.LE.IGH) GOTO 840
      DO 820 J=I,N
      Z(I,J)=H(I,J)
  820 CONTINUE
C*
  840 CONTINUE
C*
C*     ** Multiply by transformations matrix to give
C*        vectors of original full matrix.
C*        For J=N step -1 until LOW DO -- .
C*
      DO 880 JJ=LOW,N
      J=N+LOW-JJ
      M=MIN(J,IGH)
C*
      DO 880 I=LOW,IGH
      ZZ=ZERO
C*
      DO 860 K=LOW,M
      ZZ=ZZ+Z(I,K)*H(K,J)
  860 CONTINUE
C*
      Z(I,J)=ZZ
  880 CONTINUE
C*
      RETURN
      END
