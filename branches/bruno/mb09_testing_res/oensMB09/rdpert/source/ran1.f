      FUNCTION RAN1(IDUM)
C*
      INTEGER IDUM
      REAL RAN1
C*
      INTEGER IA,IM,IQ,IR,NTAB,NDIV
      REAL AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,IQ=127773,
     *           IR=2836,NTAB=32,NDIV=1+(IM-1)/NTAB,
     *           AM=1.0/IM,EPS=1.2E-7,RNMX=1.0-EPS)
C*
      INTEGER J,K,IV(NTAB),IY
C*
      SAVE IV,IY
      DATA IV /NTAB*0/, IY /0/
C*
      IF (IDUM.LE.0 .OR. IY.EQ.0) THEN
      IDUM=MAX(-IDUM,1)
      DO J=NTAB+8,1,-1
      K=IDUM/IQ
      IDUM=IA*(IDUM-K*IQ)-IR*K
      IF (IDUM .LT. 0) IDUM=IDUM+IM
      IF (J .LE. NTAB) IV(J)=IDUM
      ENDDO
      IY=IV(1)
      ENDIF
C*
      K=IDUM/IQ
      IDUM=IA*(IDUM-K*IQ)-IR*K
      IF (IDUM .LT. 0) IDUM=IDUM+IM
      J=1+IY/NDIV
      IY=IV(J)
      IV(J)=IDUM
      RAN1=MIN(AM*IY,RNMX)
C*
      RETURN
      END
