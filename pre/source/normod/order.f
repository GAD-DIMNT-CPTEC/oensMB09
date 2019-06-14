      SUBROUTINE ORDER(NM,N,FR,FW,Z,ZW,PERCUT,NF)
C*
      INTEGER NM,N,NF
      REAL PERCUT
      REAL FR(NM),FW(NM),Z(NM,*),ZW(NM,*)
C*
      INTEGER NM1,K,J,J1,I,JC,NC
      REAL CHG,ZERO,ONE
      DATA ZERO /0.0E0/, ONE /1.0E0/
C*
      NM1=N-1
   10 K=0
      DO 20 J=1,NM1
      J1=J+1
      IF (ABS(FR(J)) .GT. ABS(FR(J1))) THEN
      CHG=FR(J)
      DO 30 I=1,N
      FW(I)=Z(I,J)
   30 CONTINUE
      FR(J)=FR(J1)
      DO 40 I=1,N
      Z(I,J)=Z(I,J1)
   40 CONTINUE
      FR(J1)=CHG
      DO 50 I=1,N
      Z(I,J1)=FW(I)
   50 CONTINUE
      K=1
      ENDIF
   20 CONTINUE
      IF (K .NE. 0) GOTO 10
C*
      IF (PERCUT .LE. ZERO) THEN
      NF=N
      RETURN
      ENDIF
C*
      NC=0
      DO 60 J=1,N
      IF (ABS(ONE/FR(J)) .GT. PERCUT) NC=J
   60 CONTINUE
      NF=N-NC
      NC=NC+1
C*
      DO 70 J=1,N
      FW(J)=FR(J)
      DO 70 I=1,N
      ZW(I,J)=Z(I,J)
   70 CONTINUE
C*
      CALL RESET(NM,FR,ZERO)
      CALL RESET(NM*N,Z,ZERO)
C*
      DO 80 JC=NC,N
      J=JC+1-NC
      FR(J)=FW(JC)
      DO 80 I=1,N
      Z(I,J)=ZW(I,JC)
   80 CONTINUE
C*
      NC=NC-1
      DO 90 JC=1,NC
      J=N+JC-NC
      FR(J)=FW(JC)
      DO 90 I=1,N
      Z(I,J)=ZW(I,JC)
   90 CONTINUE
C*
      RETURN
      END
