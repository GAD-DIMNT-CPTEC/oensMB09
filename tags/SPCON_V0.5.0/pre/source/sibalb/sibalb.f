      PROGRAM ALBSIB
C
C***  CONVERT SIBALBX DATA FROM FORMATED DATA TO UNFORMATED
C
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(ITYP=13,IMON=12,ICG=2,NJJ=6,ILD=2,IBD=2,NJ=9,NK=3)
C--------------------------------------------------------
C--------------------------------------------------------
C        ALBEDO PARAMETERS
C--------------------------------------------------------
      COMMON /ALBCEF/CEDFU(ITYP,IMON,NJ), CEDIR(ITYP,IMON,NJ,3),
     *   CEDFU1(2,ITYP,IMON,NJJ,3), CEDIR1(2,ITYP,IMON,NJJ,NK,3),
     *   CEDFU2(2,ITYP,IMON,NJJ,3), CEDIR2(2,ITYP,IMON,NJJ,NK,3),
     *   CLEDFU(ITYP,IMON,NJ), CLEDIR(ITYP,IMON,NJ,3),
     *   XMIU(IMON,NK), CETHER(ITYP,IMON,2),XMIW(IMON,NK)
C--------------------------------------------------------
C                                                                       
      OPEN(10,FORM='FORMATTED',STATUS='UNKNOWN')
      OPEN(11,FORM='UNFORMATTED',STATUS='UNKNOWN')
C
      DO 1005 ITP = 1, 12
      DO 1100 L = 1, 2
 1100 READ(10,6500) (CETHER(ITP,MON,L), MON =1,12)
      DO 1200 J = 1, NJ
      READ(10,6500) (CLEDFU(ITP,MON,J),MON = 1,12)
      DO 1250 L = 1,3
 1250 READ(10,6500) (CLEDIR(ITP,MON,J,L), MON = 1,12)
 1200 CONTINUE
      DO 1300 I = 1,NK
 1300 READ (10, 6500) ( XMIU(MON,I), MON = 1, 12)
      DO 1500 J = 1, NJ
      READ(10,6500) (CEDFU(ITP,MON,J), MON = 1,12)
      DO 1600 L = 1,3
      READ(10,6500) (CEDIR(ITP,MON,J,L), MON = 1, 12)
 1600 CONTINUE
 1500 CONTINUE
      DO 1400 ML = 1, 2
      DO 1700 J = 1, NJJ
      DO 1700 L = 1, 3
      READ(10,6500) (CEDFU1(ML,ITP,MON,J,L), MON = 1,12)
      READ(10,6500) (CEDFU2(ML,ITP,MON,J,L), MON = 1,12)
 1700 CONTINUE
      DO 1800 J = 1,NJJ
      DO 1800 K = 1,NK
      DO 1800 L = 1,3
      READ(10,6500) (CEDIR1(ML,ITP,MON,J,K,L), MON= 1,12)
      READ(10,6500) (CEDIR2(ML,ITP,MON,J,K,L), MON= 1,12)
 1800 CONTINUE
 1400 CONTINUE
 1005 CONTINUE
 6500 FORMAT(6E12.5)
C
      DO 3100 L = 1, 2
 3100 READ(10,6600) (CETHER(13,MON,L), MON =1, 7)
      DO 3200 J = 1, NJ
      READ(10,6600) (CLEDFU(13,MON,J),MON = 1, 7)
      DO 3250 L = 1,3
 3250 READ(10,6600) (CLEDIR(13,MON,J,L), MON = 1, 7)
 3200 CONTINUE
      DO 3300 I = 1,NK
 3300 READ (10, 6600) ( XMIW(MON,I), MON = 1, 7)
      DO 3301 I = 1,NK
 3301 WRITE(6, 6600) ( XMIW(MON,I), MON = 1, 7)
      DO 3500 J = 1, NJ
      READ(10,6600) (CEDFU(13,MON,J), MON = 1, 7)
      DO 3600 L = 1,3
      READ(10,6600) (CEDIR(13,MON,J,L), MON = 1, 7)
 3600 CONTINUE
 3500 CONTINUE
      DO 3400 ML = 1, 2
      DO 3700 J = 1, NJJ
      DO 3700 L = 1, 3
      READ(10,6600) (CEDFU1(ML,13,MON,J,L), MON = 1, 7)
      READ(10,6600) (CEDFU2(ML,13,MON,J,L), MON = 1, 7)
 3700 CONTINUE
      DO 3800 J = 1,NJJ
      DO 3800 K = 1,NK
      DO 3800 L = 1,3
      READ(10,6600) (CEDIR1(ML,13,MON,J,K,L), MON= 1, 7)
      READ(10,6600) (CEDIR2(ML,13,MON,J,K,L), MON= 1, 7)
 3800 CONTINUE
 3400 CONTINUE
 6600 FORMAT(7E12.5)
      REWIND 11
      WRITE (11) CEDFU, CEDIR, CEDFU1, CEDIR1, CEDFU2, CEDIR2,
     *   CLEDFU, CLEDIR, XMIU, CETHER, XMIW
      REWIND 11
      READ (11) CEDFU, CEDIR, CEDFU1, CEDIR1, CEDFU2, CEDIR2,
     *   CLEDFU, CLEDIR, XMIU, CETHER, XMIW
      REWIND 11
      STOP
      END