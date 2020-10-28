      SUBROUTINE GETROW(ZM,SL,LAT,LON)
C*
C*    READS MODIFIED NAVY DATA
C*    INPUT DATA CONTAINS ONLY TERRAIN HEIGHT (INDEX 1)
C*    AND PERCENTAGE OF WATER (INDEX 2)
C*
C*    OUTPUT:
C*    ZM(30,30) - TERRAIN HEIGHT
C*    SL(30,30) - PERCENTAGE OF WATER
C*    LAT       - NORTHERN MOST LATITUDE OF GRID BOX (5 DG X 5 DG)
C*    LON       - WESTERN MOST LONGITUDE
C*
      INTEGER LAT,LON
      REAL ZM(30,30),SL(30,30)
C*
      INTEGER LAT2,LON2,J,I
      REAL ZFCT
      LOGICAL IWANT,IWANT2
      INTEGER ITOP(2,30,30)
      DATA IWANT /.TRUE./, IWANT2 /.TRUE./
C*
      ZFCT=100.0*(1200.0/3937.0)
C*
      READ(18,'(2I3)')LAT2,LON2
      READ(18,'(30I3)')ITOP
C*
      LAT=LAT2
      LON=LON2
      DO J=1,30
      DO I=1,30
C*
C*    CORRECTIONS (BY J.P.BONATTI, 18 NOV 1999)
C*
C*    ITOP(1,I,J) - IS THE NORMALIZED TOPOGRAPHY:
C*                  MUST BE BETWEEN 100 AND 320
C*    ITOP(2,I,J) - IS THE PERCENTAGE OF WATER:
C*                  MUST BE BETWEEN 0 AND 100
C*
C*    CORRECTION WERE DONE ANLYSING THE SURROUNDING VALUES
C*
      IF (IWANT) THEN
C*
C*    WRONG VALUE
      IF (ITOP(1,I,J) .EQ. 330) THEN
      ITOP(1,I,J)=300
      ENDIF
C*
C*    WRONG VALUE
      IF (ITOP(1,I,J) .EQ. 340) THEN
      ITOP(1,I,J)=300
      ENDIF
C*
C*    WRONG VALUE
      IF (ITOP(1,I,J) .EQ. 357) THEN
      ITOP(1,I,J)=257
      ENDIF
C*
C*    WRONG VALUE
      IF (ITOP(1,I,J) .EQ. 433) THEN
      ITOP(1,I,J)=113
      ENDIF
C*
C*    MAY BE MISSING VALUE
      IF (ITOP(1,I,J) .EQ. 511) THEN
      ITOP(1,I,J)=100
      ENDIF
C*
C*    MAY BE MISSING VALUE
      IF (ITOP(2,I,J) .EQ. 127) THEN
      IF (ITOP(1,I,J) .EQ. 100) THEN
      ITOP(2,I,J)=100
      ELSE
      ITOP(2,I,J)=1
      ENDIF
      ENDIF
C*
      IF (IWANT2) THEN
C*
C*    TO AVOID NEGATIVE TOPOGRAPHY
      IF (ITOP(1,I,J) .LT. 100) THEN
      ITOP(1,I,J)=100
      ENDIF
C*
C*    TO AVOID NON-DESIRABLE VALUES OF
C*    SURFACE TEMPERATURE OVER WATER
C*    INSIDE CONTINENTS
      IF (ITOP(1,I,J).GT.100 .AND. ITOP(2,I,J).GT.50) THEN
      ITOP(2,I,J)=1
      ENDIF
C*
      ENDIF
C*
      ENDIF
C*
C*    ZM(I,J)=FLOAT((ITOP(1,I,J)-100)*100)*1200.0/3937.0
      ZM(I,J)=ZFCT*FLOAT(ITOP(1,I,J)-100)
      SL(I,J)=FLOAT(ITOP(2,I,J))
C*
      ENDDO
      ENDDO
C*
      RETURN
      END