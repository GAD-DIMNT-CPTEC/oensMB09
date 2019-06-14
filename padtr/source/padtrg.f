      SUBROUTINE PADTRG(MEND1A,MEND1B,NWNA,NWNB,KM,QA,QB,SAME)
C*
      INTEGER MEND1A,MEND1B,NWNA,NWNB,KM
      LOGICAL SAME
      REAL*4 QA(NWNA,KM),QB(NWNB,KM)
C*
      INTEGER K,NA,NB,MA,MB,N,M
C*
      CALL RESET(QB,NWNB*KM)
C*
      IF (SAME) THEN
      DO K=1,KM
      DO NB=1,NWNB
      QB(NB,K)=QA(NB,K)
      ENDDO
      ENDDO
      ELSE
C*
      IF (MEND1B .LE. MEND1A) THEN
C*
      DO K=1,KM
      NA=0
      NB=0
      DO N=1,MEND1B
      MA=2*(MEND1A+1-N)
      MB=2*(MEND1B+1-N)
      DO M=1,MA
      NA=NA+1
      IF (M .LE. MB) THEN
      NB=NB+1
      QB(NB,K)=QA(NA,K)
      ENDIF
      ENDDO
      ENDDO
      ENDDO
C*
      ELSE
C*
      DO K=1,KM
      NA=0
      NB=0
      DO N=1,MEND1A
      MA=2*(MEND1A+1-N)
      MB=2*(MEND1B+1-N)
      DO M=1,MB
      NB=NB+1
      IF (M .LE. MA) THEN
      NA=NA+1
      QB(NB,K)=QA(NA,K)
      ENDIF
      ENDDO
      ENDDO
      ENDDO
C*
      ENDIF
C*
      ENDIF
C*
      RETURN
      END
