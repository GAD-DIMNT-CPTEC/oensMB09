      PROGRAM RDPERT
C*
C*    GENERATION OF RANDOM PERTURBATIONS
C*    FOR TEMPERATURE, WINDS AND HUMIDITY (IF HUM=YES)
C*
      INTEGER IMAX,JMAX,MEND,KMAX,IDM
      PARAMETER (IMAX=384,JMAX=192,MEND=126,KMAX=28,IDM=7)
C*
      INTEGER ID,IDUM,IW,IE,JN,JS,I,J,K,
     *        IDO,IDP,IGO,IGP(IDM)
      REAL DX,FLONW,FLONE,GLATS,GLATN
      REAL FLON(IMAX),GLAT(JMAX)
      REAL T(IMAX,JMAX,KMAX),Q(IMAX,JMAX,KMAX),
     *     U(IMAX,JMAX,KMAX),V(IMAX,JMAX,KMAX)
      REAL RPT(IMAX,JMAX,KMAX),RPU(IMAX,JMAX,KMAX),
     *     RPQ(IMAX,JMAX,KMAX),RPV(IMAX,JMAX,KMAX),
     *     STDT(KMAX),STDU(KMAX),STDV(KMAX),STDQ(KMAX)
      REAL*4 BUF(IMAX,JMAX)
      LOGICAL SECTOR
      CHARACTER GNAMEO*64,GNAMEP(IDM)*64,DIRO*64,DIRP*64
      CHARACTER HUM*3
C*
      NAMELIST /DATAIN/ FLONW,FLONE,GLATN,GLATS,IW,IE,JN,JS,SECTOR
      NAMELIST /STTEMP/ STDT
      NAMELIST /STZWIN/ STDU
      NAMELIST /STMWIN/ STDV
      NAMELIST /STHUMI/ STDQ
      NAMELIST /HUMIDI/ HUM
      NAMELIST /DATNAM/ GNAMEO,GNAMEP,DIRO,DIRP
C*
C*
      IW=1
      IE=IMAX
      JN=1
      JS=JMAX
      DX=360.0/FLOAT(IMAX)
      DO I=1,IMAX
      FLON(I)=FLOAT(I-1)*DX
      ENDDO
      CALL GLATD(JMAX,GLAT)
      FLONW=FLON(1)
      FLONE=FLON(IMAX)
      GLATN=GLAT(1)
      GLATS=GLAT(JMAX)
      SECTOR=.FALSE.
      READ(*,DATAIN)
      CALL GSCTIJ(IMAX,JMAX,FLON,GLAT,
     *            FLONW,FLONE,GLATN,GLATS,
     *            IW,IE,JN,JS)
      IF (IW .NE.    1) SECTOR=.TRUE.
      IF (IE .NE. IMAX) SECTOR=.TRUE.
      IF (JN .NE.    1) SECTOR=.TRUE.
      IF (JS .NE. JMAX) SECTOR=.TRUE.
      WRITE(*,DATAIN)
C*
      READ(*,STTEMP)
      READ(*,STZWIN)
      READ(*,STMWIN)
      READ(*,STHUMI)
      READ(*,HUMIDI)
      write(*,*)'stdt'
      write(*,*)stdt
      write(*,*)'stdu'
      write(*,*)stdu
      write(*,*)'stdv'
      write(*,*)stdv
      write(*,*)'stdq'
      write(*,*)stdq
      write(*,*)'hum=',hum
C*
      READ(*,DATNAM)
      IDO=INDEX(DIRO//' ',' ')-1
      IDP=INDEX(DIRP//' ',' ')-1
      IGO=INDEX(GNAMEO//' ',' ')-1
      DO ID=1,IDM
      IGP(ID)=INDEX(GNAMEP(ID)//' ',' ')-1
      ENDDO
      WRITE(*,DATNAM)
C*
      WRITE(*,'(/,A)')' OPEN: '//DIRO(1:IDO)//GNAMEO(1:IGO)
      OPEN (10,FILE=DIRO(1:IDO)//GNAMEO(1:IGO),STATUS='UNKNOWN',
     *         FORM='UNFORMATTED')
      CALL FREAD(10,T)
      CALL FREAD(10,Q)
      CALL FREAD(10,U)
      CALL FREAD(10,V)
      CLOSE(10)
C*
      DO ID=1,IDM
      IDUM=-ID
C*
      WRITE(*,'(/,I5,1X,A)')IDUM,
     *             DIRP(1:IDP)//GNAMEP(ID)(1:IGP(ID))//'.out'
      OPEN(66,FILE=DIRP(1:IDP)//GNAMEP(ID)(1:IGP(ID))//'.out',
     *        STATUS='UNKNOWN')
      WRITE(66,DATAIN)
C*
C*    GENERATE RANDOM PERTURBATIONS
C*
      CALL PERTUR(RPT,RPU,RPV,RPQ,IMAX,JMAX,KMAX,IDUM,
     *STDT,STDU,STDV,STDQ)

      IF (HUM .NE. 'YES') THEN
      write(*,*)'HUM eh diferente de YES, anula rpq'
      DO K=1,KMAX
      DO J=1,JMAX
      DO I=1,IMAX
      RPQ(I,J,K)=0.0
      ENDDO
      ENDDO
      ENDDO
      ENDIF
C*
C*    PUT ZEROES OUTSIDE SELECTED REGION
C*
      IF (SECTOR) THEN
      DO K=1,KMAX
      DO J=1,JMAX
      DO I=1,IW-1
      RPT(I,J,K)=0.0
      RPQ(I,J,K)=0.0
      RPU(I,J,K)=0.0
      RPV(I,J,K)=0.0
      ENDDO
      DO I=IE+1,IMAX
      RPT(I,J,K)=0.0
      RPQ(I,J,K)=0.0
      RPU(I,J,K)=0.0
      RPV(I,J,K)=0.0
      ENDDO
      ENDDO
      DO I=IW,IE
      DO J=1,JN-1
      RPT(I,J,K)=0.0
      RPQ(I,J,K)=0.0
      RPU(I,J,K)=0.0
      RPV(I,J,K)=0.0
      ENDDO
      DO J=JS+1,JMAX
      RPT(I,J,K)=0.0
      RPQ(I,J,K)=0.0
      RPU(I,J,K)=0.0
      RPV(I,J,K)=0.0
      ENDDO
      ENDDO
      ENDDO
      ENDIF
C*
C*    ADD PERTURBATIONS AND OUTPUT RESULTS
C*
      WRITE(*,'(/,A)')' OPEN: '//DIRP(1:IDP)//GNAMEP(ID)(1:IGP(ID))
      OPEN (20,FILE=DIRP(1:IDP)//GNAMEP(ID)(1:IGP(ID)),
     *         STATUS='UNKNOWN',
     *         FORM='UNFORMATTED')
      CALL FWRITE(20,T,RPT)
      CALL FWRITE(20,Q,RPQ)
      CALL FWRITE(20,U,RPU)
      CALL FWRITE(20,V,RPV)
      CLOSE(20)
C*
      ENDDO
C*
      STOP
      END
