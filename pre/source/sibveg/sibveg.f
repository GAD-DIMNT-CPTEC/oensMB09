      PROGRAM VEGSIB
C
C**      THIS IS FOR SIMPLIFIED VERSION ONLY for ix
C***  READ VEGETATION MORPHOLOGICAL AND PHYSIOLOGICAL DATA
C
C     IMPLICIT HALF PRECISION (A-H,O-Z)
      PARAMETER(ITYP=13,IMON=12,ICG=2,IWV=3,ILD=2,IDP=3,IBD=2)
C----------------------------------------------------------------------
C        VEGETATION AND SOIL PARAMETERS
C----------------------------------------------------------------------
      COMMON/COMVEG/ TRAN  (ITYP,ICG,IWV,ILD),REF   (ITYP,ICG,IWV,ILD),
     1               RSTPAR(ITYP,ICG,IWV)    ,SOREF (ITYP,IWV),
     2               CHIL  (ITYP,ICG),TOPT  (ITYP,ICG),TLL  (ITYP,ICG),
     3               TU    (ITYP,ICG),DEFAC (ITYP,ICG),PH1  (ITYP,ICG),
     4               PH2   (ITYP,ICG),ROOTD (ITYP,ICG),RLMAX(ITYP,ICG),
     5               ROOTCA(ITYP,ICG),RPLANT(ITYP,ICG),RDRES(ITYP,ICG),
     7               BEE   (ITYP),PHSAT (ITYP),SATCO (ITYP),POROS(ITYP),
     8               SLOPE (ITYP),ZDEPTH(ITYP,IDP),
     9               GREEN (ITYP,IMON,ICG),XCOVER(ITYP,IMON,ICG),
     A               ZLT   (ITYP,IMON,ICG),X0X   (ITYP,IMON),
     B               XD    (ITYP,IMON)    ,Z2    (ITYP,IMON),
     C               Z1    (ITYP,IMON)    ,XDC   (ITYP,IMON),
     D               XBC   (ITYP,IMON)    ,ROOTL (ITYP,IMON,ICG)
C----------------------------------------------------------------------
      DATA INFILE/19/,IOUTFL/10/
C
C
      OPEN(19,FORM='FORMATTED',STATUS='UNKNOWN')
      OPEN(10,FORM='UNFORMATTED',STATUS='UNKNOWN')
C
      READ(INFILE,11,END=1000)
   11 FORMAT(1X)
    2 CONTINUE
C
      READ(INFILE,50,END=1000) NTYP
      WRITE(6,999) NTYP
  999 FORMAT(1H ,'TYPE=',I2,'  READ IN')
C
C
C     READ(INFILE,200)
      READ(INFILE,*)
     &((RSTPAR(NTYP,IV,IM),IM=1,3),IV=1,2),
     & (CHIL  (NTYP,IV), IV=1,2), (TOPT  (NTYP,IV), IV=1,2),
     & (TLL   (NTYP,IV), IV=1,2), (TU    (NTYP,IV), IV=1,2),
     & (DEFAC (NTYP,IV), IV=1,2), (PH1   (NTYP,IV), IV=1,2),
     & (PH2   (NTYP,IV), IV=1,2), (ROOTD (NTYP,IV), IV=1,2),
     &  BEE   (NTYP),PHSAT (NTYP), SATCO (NTYP),
     &  POROS (NTYP),(ZDEPTH(NTYP,K),K=1,3)
   50 FORMAT(I3)
  200 FORMAT(6F12.4/2F8.2/6F8.1/2F10.4,4F8.2/2F6.1/
     & 2F10.4/E12.5,F10.4/3E12.5)
C
      DO 500 I = 1,12
      READ(INFILE,*)  MMM,(ZLT   (NTYP,I,IV),IV=1,2),
C     READ(INFILE,800)  MMM,(ZLT   (NTYP,I,IV),IV=1,2),
     &                      (GREEN (NTYP,I,IV),IV=1,2),
     &                       Z2 (NTYP,I),Z1 (NTYP,I),
     &                      (XCOVER(NTYP,I,IV),IV=1,2),
     &                       X0X(NTYP,I),XD (NTYP,I),
     &                       XBC(NTYP,I),XDC(NTYP,I)
  500 CONTINUE
C 800 FORMAT(I3/2F10.5,2F10.5,2F8.4/2F10.5,2F10.5/F12.4,F12.4,2F12.1)
  800 FORMAT(I3/2F10.5,2F10.5,2F8.4/2F10.5,2F10.5/2F10.2)
C
      GO TO 2
 1000 CONTINUE
C
      REWIND IOUTFL
      WRITE (IOUTFL) RSTPAR,CHIL ,TOPT ,TLL   ,
     1               TU    ,DEFAC,PH1   ,PH2  ,ROOTD,
     2               BEE   ,PHSAT,SATCO,POROS,
     3               ZDEPTH
      WRITE (IOUTFL) GREEN ,XCOVER,ZLT  ,X0X  ,XD   ,Z2   ,
     1               Z1    ,XDC   ,XBC
*     WRITE(6,888)
* 888 FORMAT(1H ,'DATA WRIITEN ON FILE 9')
      REWIND IOUTFL
      READ  (IOUTFL) RSTPAR,CHIL ,TOPT ,TLL   ,
     1               TU    ,DEFAC,PH1   ,PH2  ,ROOTD,
     2               BEE   ,PHSAT,SATCO,POROS,
     3               ZDEPTH
      READ  (IOUTFL) GREEN ,XCOVER,ZLT  ,X0X  ,XD   ,Z2    ,
     1               Z1    ,XDC   ,XBC
      REWIND IOUTFL
      DO 444 NTYP=1,ITYP
      WRITE(6,50) NTYP
      WRITE(6,200)
     &((RSTPAR(NTYP,IV,IM),IM=1,3),IV=1,2),
     & (CHIL  (NTYP,IV), IV=1,2), (TOPT  (NTYP,IV), IV=1,2),
     & (TLL   (NTYP,IV), IV=1,2), (TU    (NTYP,IV), IV=1,2),
     & (DEFAC (NTYP,IV), IV=1,2), (PH1   (NTYP,IV), IV=1,2),
     & (PH2   (NTYP,IV), IV=1,2), (ROOTD (NTYP,IV), IV=1,2),
     &  BEE   (NTYP),PHSAT (NTYP), SATCO (NTYP),
     &  POROS (NTYP),(ZDEPTH(NTYP,K),K=1,3)
      DO 550 I = 1,12
      WRITE(6,800)  I,(ZLT   (NTYP,I,IV),IV=1,2),
     &                      (GREEN (NTYP,I,IV),IV=1,2),
     &                       Z2 (NTYP,I),Z1 (NTYP,I),
     &                      (XCOVER(NTYP,I,IV),IV=1,2),
     &                       X0X(NTYP,I),XD (NTYP,I),
     &                       XBC(NTYP,I),XDC(NTYP,I)
  550 CONTINUE
  444 CONTINUE
C
      STOP
      END
