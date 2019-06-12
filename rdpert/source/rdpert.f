      PROGRAM RDPERT
C*
C*    GENERATION OF RANDOM PERTURBATIONS
C*    FOR TEMPERATURE, WINDS AND HUMIDITY (IF HUM=YES)
C*
      include "rdpert.h"
C*
      INTEGER ID,IDUM,IW,IE,JN,JS,I,J,K,
     *        IDO,IDP,IGO,IGP(IDM)
      REAL DX,FLONW,FLONE,GLATS,GLATN
      REAL FLON(IMAX),GLAT(JMAX)
      REAL P(IMAX,JMAX),T(IMAX,JMAX,KMAX),Q(IMAX,JMAX,KMAX),
     *     U(IMAX,JMAX,KMAX),V(IMAX,JMAX,KMAX)
      REAL RPP(IMAX,JMAX),RPT(IMAX,JMAX,KMAX),RPU(IMAX,JMAX,KMAX),
     *     RPQ(IMAX,JMAX,KMAX),RPV(IMAX,JMAX,KMAX),
     *     STDP,STDT(KMAX),STDU(KMAX),STDV(KMAX),STDQ(KMAX)
      REAL*4 BUF(IMAX,JMAX)
      LOGICAL SECTOR
      CHARACTER GNAMEO*256,GNAMEP(IDM)*256,DIRO*256,DIRP*256
      CHARACTER HUM*3
C*
      NAMELIST /DATAIN/ FLONW,FLONE,GLATN,GLATS,IW,IE,JN,JS,SECTOR
      NAMELIST /STPRES/ STDP
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
      READ(*,STPRES)
      READ(*,STTEMP)
      READ(*,STZWIN)
      READ(*,STMWIN)
      READ(*,STHUMI)
      READ(*,HUMIDI)
      write(*,*)'stdp'
      write(*,*)stdp
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
      READ(10)BUF
      DO J=1,JMAX
      DO I=1,IMAX
      P(I,J)=BUF(I,J)
      ENDDO
      ENDDO
      PRINT*,'PK','MAXVAL=',MAXVAL(P),';MINVAL=',MINVAL(P)
      CALL FREAD(10,T)
      PRINT*,'PK','MAXVAL=',MAXVAL(T),';MINVAL=',MINVAL(t)

      CALL FREAD(10,Q)
      PRINT*,'PK','MAXVAL=',MAXVAL(q),';MINVAL=',MINVAL(q)

      CALL FREAD(10,U)
      PRINT*,'PK','MAXVAL=',MAXVAL(u),';MINVAL=',MINVAL(u)

      CALL FREAD(10,V)
      PRINT*,'PK','MAXVAL=',MAXVAL(v),';MINVAL=',MINVAL(v)

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
      CALL PERTUR(RPP,RPT,RPU,RPV,RPQ,IMAX,JMAX,KMAX,IDUM,
     *STDP,STDT,STDU,STDV,STDQ)

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
      IF (K .EQ. 1) THEN
      RPP(I,J)=0.0
      ENDIF
      RPT(I,J,K)=0.0
      RPQ(I,J,K)=0.0
      RPU(I,J,K)=0.0
      RPV(I,J,K)=0.0
      ENDDO
      DO I=IE+1,IMAX
      IF (K .EQ. 1) THEN
      RPP(I,J)=0.0
      ENDIF
      RPT(I,J,K)=0.0
      RPQ(I,J,K)=0.0
      RPU(I,J,K)=0.0
      RPV(I,J,K)=0.0
      ENDDO
      ENDDO
      DO I=IW,IE
      DO J=1,JN-1
      IF (K .EQ. 1) THEN
      RPP(I,J)=0.0
      ENDIF
      RPT(I,J,K)=0.0
      RPQ(I,J,K)=0.0
      RPU(I,J,K)=0.0
      RPV(I,J,K)=0.0
      ENDDO
      DO J=JS+1,JMAX
      IF (K .EQ. 1) THEN
      RPP(I,J)=0.0
      ENDIF
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
      DO J=1,JMAX
      DO I=1,IMAX
      BUF(I,J)=P(I,J)+RPP(I,J)
      ENDDO
      ENDDO
      WRITE(20)BUF
      CALL FWRITE(20,T,RPT)
      CALL FWRITE(20,Q,RPQ)
      CALL FWRITE(20,U,RPU)
      CALL FWRITE(20,V,RPV)
      CLOSE(20)
      CLOSE(66)
C*
      ENDDO
C*
      STOP
      END
