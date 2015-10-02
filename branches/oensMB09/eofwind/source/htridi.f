      SUBROUTINE HTRIDI(NM,N,AR,AI,D,E,E2,TAU)
C*
C*    REDUCE A COMPLEX HERMITIAN MATRIX TO A SYMMETRIC
C*    TRIDIAGONAL MATRIX USING THE UNITARY TRANSFORMATIONS
C*
C*    FORMAL ARGUMENTS:
C*
      INTEGER NM,N
      REAL AR(NM,N),AI(NM,N),D(N),E(N),E2(N),TAU(2,N)
C*
C*    LOCAL PARAMETERS:
C*
      INTEGER I,J,K,L,II,JP1
      REAL F,G,H,FI,GI,HH,SI,SCALE
C*
      TAU(1,N)=1.0
      TAU(2,N)=0.0
      DO I=1,N
      D(I)=AR(I,I)
      ENDDO
C*
C*    MAIN LOOP: FOR I=N STEP -1 UNTIL 2 DO
C*
      DO II=1,N
      I=N+1-II
      L=I-1
      H=0.0
      SCALE=0.0
C*
C*    SCALE ROW
C*
      IF (L .GE. 1) THEN
      DO K=1,L
      SCALE=SCALE+ABS(AR(I,K))+ABS(AI(I,K))
      ENDDO
      ENDIF
C*
      IF (SCALE .NE. 0.0) THEN
C*
      DO K=1,L
      AR(I,K)=AR(I,K)/SCALE
      AI(I,K)=AI(I,K)/SCALE
      H=H+AR(I,K)*AR(I,K)+AI(I,K)*AI(I,K)
      ENDDO
C*
      E2(I)=SCALE*SCALE*H
      G=SQRT(H)
      E(I)=SCALE*G
      F=SQRT(AR(I,L)*AR(I,L)+AI(I,L)*AI(I,L))
      IF (F .NE. 0.0) THEN
      TAU(1,L)=(AI(I,L)*TAU(2,I)-AR(I,L)*TAU(1,I))/F
      SI=(AR(I,L)*TAU(2,I)+AI(I,L)*TAU(1,I))/F
      H=H+F*G
      G=1.0+G/F
      AR(I,L)=G*AR(I,L)
      AI(I,L)=G*AI(I,L)
      ELSE
      TAU(1,L)=-TAU(1,I)
      SI=TAU(2,I)
      AR(I,L)=G
      ENDIF
C*
      IF (.NOT.(F.NE.0.0 .AND. L.EQ.1)) THEN
      F=0.0
C*
      DO J=1,L
C*
      G=0.0
      GI=0.0
C*
C*    FORM ELEMENT OF A*U
C*
      DO K=1,J
      G=G+AR(J,K)*AR(I,K)+AI(J,K)*AI(I,K)
      GI=GI-AR(J,K)*AI(I,K)+AI(J,K)*AR(I,K)
      ENDDO
C*
      JP1=J+1
      IF (L .GE. JP1) THEN
      DO K=JP1,L
      G=G+AR(K,J)*AR(I,K)-AI(K,J)*AI(I,K)
      GI=GI-AR(K,J)*AI(I,K)-AI(K,J)*AR(I,K)
      ENDDO
      ENDIF
C*
C*    FORM ELEMENT OF P
C*
      E(J)=G/H
      TAU(2,J)=GI/H
      F=F+E(J)*AR(I,J)-TAU(2,J)*AI(I,J)
C*
      ENDDO
C*
      HH=F/(H+H)
C*
C*    FORM REDUCED A
C*
      DO J=1,L
      F=AR(I,J)
      G=E(J)-HH*F
      E(J)=G
      FI=-AI(I,J)
      GI=TAU(2,J)-HH*FI
      TAU(2,J)=-GI
      DO K=1,J
      AR(J,K)=AR(J,K)-F*E(K)-G*AR(I,K)
     *               +FI*TAU(2,K)+GI*AI(I,K)
      AI(J,K)=AI(J,K)-F*TAU(2,K)-G*AI(I,K)
     *               -FI*E(K)-GI*AR(I,K)
      ENDDO
      ENDDO
C*
      ENDIF
C*
      DO K=1,L
      AR(I,K)=SCALE*AR(I,K)
      AI(I,K)=SCALE*AI(I,K)
      ENDDO
      TAU(2,L)=-SI
C*
      ELSE
      IF (L .GE. 1) THEN
      TAU(1,L)=1.0
      TAU(2,L)=0.0
      ENDIF
      E(I)=0.0
      E2(I)=0.0
      ENDIF
C*
      HH=D(I)
      D(I)=AR(I,I)
      AR(I,I)=HH
      AI(I,I)=SCALE*SQRT(H)
C*
C*    END MAIN LOOP
C*
      ENDDO
C*
      RETURN
      END
