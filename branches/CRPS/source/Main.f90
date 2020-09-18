Program Main
!
!  Main program to calculate the 
!  Continuous Ranked Probability Score
!  for the EPSCPTEC
!
   USE ClimateInterp,    ONLY : climint

   IMPLICIT NONE

   INTEGER :: IMax,JMax,NMembers,T,I,J,TimeSeriesLength,K,Lenght
   INTEGER :: NM, NXY, NLAT,NGP,NX,NY
   INTEGER :: N, NVariables, NLevels, JB, IB, NB
   INTEGER :: NYears=21,DaysPerMonth(12),LON1,LON2,LAT1,LAT2
   INTEGER, PARAMETER :: NumberOfBins=10

   PARAMETER (IMax=240,JMax=121,NMembers=15,TimeSeriesLength=23)
   !
   ! LON1=1,LON2=240,LAT1=74,LAT2=121
   ! Limits that define de Northern Hemisphere.
   ! Considers SOUTH -> NORTH orientation
   ! Considers a 1.5 x 1.5 grid resolution
   ! Latitude range: 19.5 90
   !
   !PARAMETER (LON1=1,LON2=240,LAT1=74,LAT2=121)
   !PARAMETER (LON1=1,LON2=240,LAT1=1,LAT2=48)
   !PARAMETER (LON1=1,LON2=240,LAT1=48,LAT2=74)

   PARAMETER (NVariables=1,NLevels=1)

   INTEGER :: alloc_error ! storage the allocation status
   INTEGER, PARAMETER :: fourdecplaces = selected_real_kind(4,10)
   INTEGER, PARAMETER :: long = selected_real_kind(9,99)
   REAL, ALLOCATABLE, DIMENSION(:,:)      :: geop500clm,temp850clm,psnmclm
   REAL, DIMENSION(IMax,JMax) :: Analise,Rank2ArrayIn
   !REAL, DIMENSION ((LON2-LON1+1)*(LAT2-LAT1+1)) :: SDAnalise,WGHT,Rank1ArrayOut
   REAL, DIMENSION (:), ALLOCATABLE :: SDAnalise,WGHT,Rank1ArrayOut
   REAL, DIMENSION(IMax,JMax,NMembers) :: EPSForecasts
   !REAL, DIMENSION((LON2-LON1+1)*(LAT2-LAT1+1),NMembers) :: SDEPSForecasts
   REAL, DIMENSION(:,:), ALLOCATABLE :: SDEPSForecasts
   REAL, ALLOCATABLE,DIMENSION(:,:,:) :: TimeInterpClim
   REAL, ALLOCATABLE, DIMENSION(:,:) :: SDClim
   REAL :: weight(IMax,JMax),aloc(IMax,JMax),sumwgt(IMax)
   REAL :: SWGT, SCRF, LATBEG
   REAL, DIMENSION(IMax,JMax,TimeSeriesLength) :: ClimateRecord
   REAL :: DLat
   REAL :: latweight(JMax)

   CHARACTER(LEN=10)  :: ANLDATE
   CHARACTER(LEN=200) :: dirclm

   REAL(KIND=4) :: Xint, RoundTemp, F, One

   REAL(KIND=4),DIMENSION(:),ALLOCATABLE :: Y       ! output array for JSORT subroutine
   REAL(KIND=4),DIMENSION(:),ALLOCATABLE :: AuxTS   ! auxiliary time series array
   REAL(KIND=4)                          :: Aux     ! auxiliary real (single) variable

   CHARACTER(LEN=500) :: ObsFile,EPSFctFiles,ClimateRecordFile 
   CHARACTER(LEN=4) :: Month,FCTLAG
   CHARACTER(LEN=3) :: ObsType
   CHARACTER(LEN=9) :: RefFct

   DATA DaysPerMonth/31,28,31,30,31,30,31,31,30,31,30,31/

   NAMELIST /PARAM/ AnlDate,Month,dirclm,FCTLAG,ObsType,RefFct,LAT1,LAT2,LON1,LON2
   NAMELIST /FILES/ ObsFile,EPSFctFiles,ClimateRecordFile

   READ(5,NML=PARAM)
   READ(5,NML=FILES)
   WRITE(6,"(4I10)") LON1,LON2,LAT1,LAT2

   !================================================================================================================
   !
   !					ALLOCATING MEMORY FOR ALL ARRAYS
   !
   ALLOCATE(geop500clm(IMax,JMax), STAT=alloc_error)
   IF (alloc_error .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: geop500clm'
   ALLOCATE(temp850clm(IMax,JMax), STAT=alloc_error)
   IF (alloc_error .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: temp850clm'
   ALLOCATE(psnmclm(IMax,JMax), STAT=alloc_error)
   IF (alloc_error .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: psnmclm'
   ALLOCATE(SDAnalise((LON2-LON1+1)*(LAT2-LAT1+1)), STAT=alloc_error)
   IF (alloc_error .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: SDAnalise'
   ALLOCATE(WGHT((LON2-LON1+1)*(LAT2-LAT1+1)), STAT=alloc_error)
   IF (alloc_error .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: WGHT'
   ALLOCATE(Rank1ArrayOut((LON2-LON1+1)*(LAT2-LAT1+1)), STAT=alloc_error)
   IF (alloc_error .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: Rank1ArrayOut'
   ALLOCATE(SDEPSForecasts((LON2-LON1+1)*(LAT2-LAT1+1),NMembers), STAT=alloc_error)
   IF (alloc_error .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: SDEPSForecasts'
   !
   !					END ALLOCATING MEMORY FOR ALL ARRAYS
   !
   !================================================================================================================

   ! ----
   !     CALCULATE LATITUDINAL WEIGHTS
   ! ----
   DLat=1.5
   CALL lweights(JMax,LAT1,DLat,latweight,LATBEG)
   DO I = 1, IMax
   DO J = 1, JMax
      aloc(I,J) = latweight(J)
   ENDDO
   ENDDO
   CALL SUBDOMAIN(IMAX,JMAX,aloc,LON1,LON2,LAT1,LAT2,wght,Lenght)
   OPEN(115,FILE='../dataout/wght.grads',FORM='UNFORMATTED',ACTION='WRITE',ACCESS='SEQUENTIAL',STATUS='UNKNOWN')
       WRITE(115) wght
   CLOSE(115)

   !
   ! READS IN THE ANALYSIS
   !
   OPEN(Unit=100, File=Trim(ObsFile), Form="Unformatted", Access="Direct", RecL=IMax*JMax*4)
   READ(Unit=100, Rec=1) Analise
   Close(100)
   CALL SUBDOMAIN(IMAX,JMAX,Analise,LON1,LON2,LAT1,LAT2,SDAnalise,Lenght)
   OPEN(105,FILE='../dataout/SDAnalise.grads',FORM='UNFORMATTED',ACTION='WRITE',ACCESS='SEQUENTIAL',STATUS='UNKNOWN')
       WRITE(105) SDAnalise
   CLOSE(105)
   OPEN(106,FILE='../dataout/SDAnalise.ctl',FORM='FORMATTED',STATUS='UNKNOWN')
      WRITE(106,100) 'SDAnalise.grads'
      WRITE(106,105)
      WRITE(106,110)
      WRITE(106,115) LON2-LON1+1
      WRITE(106,120) LAT2-LAT1+1,LATBEG
      WRITE(106,125)
      WRITE(106,130)
      WRITE(106,140)
      WRITE(106,145)
      WRITE(106,150)
   CLOSE(106)
 100  FORMAT ('DSET ', A)
 105  FORMAT ('UNDEF -9999.')
 110  FORMAT ('OPTIONS SEQUENTIAL')
 115  FORMAT ('XDEF ',I6,' LINEAR 0.0 1.5')
 120  FORMAT ('YDEF ',I6,' LINEAR ',F6.2,' 1.5')
 125  FORMAT ('ZDEF 1 LINEAR 1.0 1.0')
 130  FORMAT ('TDEF 1 LINEAR 1JAN1950 1YR')
 140  FORMAT ('VARS 1')
 145  FORMAT ('VAR 0 99 SUBDOMAIN')
 150  FORMAT ('ENDVARS')


   !
   ! READS IN THE ENSEMBLE FORECASTS
   !
   OPEN(Unit=110,File=TRIM(EPSFctFiles), FORM="UNFORMATTED", Access="Direct",RecL=IMax*JMax*4)
   DO NM=1,NMembers
      Print*,"READING MEMBER NUMBER ",NM
      Read(110,Rec=NM) EPSForecasts(:,:,NM)
      DO I = 1, IMax
      DO J = 1, JMax
         Rank2ArrayIn(I,J)=EPSForecasts(I,J,NM)
      ENDDO
      ENDDO
      CALL SUBDOMAIN(IMAX,JMAX,Rank2ArrayIn,LON1,LON2,LAT1,LAT2,Rank1ArrayOut,Lenght)
      WRITE(6,"('ENSEMBLE FORECAST MEMBERS ',I6)") Lenght
      DO I = 1, Lenght
         SDEPSForecasts(I,NM)=Rank1ArrayOut(I)
  !      WRITE(6,*) SDEPSForecasts(I,NM)
      ENDDO
   ENDDO
   CLOSE(110)

   NXY=(LON2-LON1+1)*(LAT2-LAT1+1)
   IF (RefFct .EQ. "EQPROBBIN") THEN
      IB=10
      SCRF = 1.0/float(IB)
      ALLOCATE(SDClim((LON2-LON1+1)*(LAT2-LAT1+1),IB+1))
      ALLOCATE(TimeInterpClim(IMax,JMax,IB+1))
      !
      ! THE SUBROUTINE CLIMINT READS THE MONTHLY CLIMATOLOGY FILES 
      ! AND INTERPOLATE THEM (IN TIME) TO THE SAME DATE OF THE ANALYSIS
      !
      CALL CLIMINT(anldate,dirclm,psnmclm,geop500clm,temp850clm,TimeInterpClim)
      DO NB = 1,NumberOfBins+1
         Rank2ArrayIn=TimeInterpClim(:,:,NB)
         CALL SUBDOMAIN(IMAX,JMAX,Rank2ArrayIn,LON1,LON2,LAT1,LAT2,Rank1ArrayOut,Lenght)
         SDClim(:,NB)=Rank1ArrayOut
      ENDDO
   ELSE IF (RefFct .EQ. "CLIMRECOR") THEN
      IB=TimeSeriesLength-1
      SCRF = 1.0/float(IB)
      ALLOCATE(SDClim((LON2-LON1+1)*(LAT2-LAT1+1),IB+1))
      !
      ! READS THE CLIMATE RECORD
      !
      OPEN(Unit=110,File=TRIM(ClimateRecordFile),Access="Direct",RecL=IMax*JMax*4)
      WRITE (6,*) "Reading the long-term time series of observations ..."
      WRITE (6,*) TRIM(ClimateRecordFile)
      OPEN(52,FILE='../dataout/SDClim.grads',FORM='UNFORMATTED', &
           ACTION='WRITE',ACCESS='SEQUENTIAL',STATUS='UNKNOWN')
      DO T=1,TimeSeriesLength
         Read(Unit=110,Rec=T) ((ClimateRecord(I,J,T),I=1,IMax),J=1,JMax)
         DO I = 1, IMax
         DO J = 1, JMax
            Rank2ArrayIn(I,J)=ClimateRecord(I,J,T)
         ENDDO
         ENDDO
         CALL SUBDOMAIN(IMAX,JMAX,Rank2ArrayIn,LON1,LON2,LAT1,LAT2,Rank1ArrayOut,Lenght)
         DO NXY = 1,Lenght
            SDClim(NXY,T)=Rank1ArrayOut(NXY)
         ENDDO
         WRITE(52) (SDClim(NXY,T),NXY=1,Lenght)
      ENDDO
   ELSE
      STOP "BAD OPTION FOR REFERENCE FORECAST (RefFct)"
   ENDIF

!CFB - Nesta versao do codigo, apenas uma ou outra chamada devera ser utilizada:
!CRPS
   CALL PROB(SDEPSForecasts,SDAnalise,SDClim,wght,Lenght,IB,NMembers,SCRF,ANLDATE,FCTLAG)
!RANK HISTOGRAM
!   CALL DIST(SDEPSForecasts,SDAnalise,SDClim,wght,Lenght,IB,NMembers,ANLDATE,FCTLAG)
!CFB

End Program Main

   SUBROUTINE lweights(jmax,LAT1,DLat,latweight,LATBEG)
   IMPLICIT NONE
   INTEGER,               INTENT(IN)  :: jmax
   INTEGER,               INTENT(IN)  :: LAT1
   REAL,                  INTENT(IN)  :: DLat
   REAL, DIMENSION(jmax), INTENT(OUT) :: latweight
   REAL,                  INTENT(OUT) :: LATBEG
   INTEGER  :: j
   REAL     :: pi,lat,latr

   pi=4.0*ATAN(1.0)
   lat=-90.0
   DO j=1,jmax
     IF (LAT1 == J) LATBEG=lat
     latr=(pi*lat)/180.0
     latweight(j)=cos(latr)
     IF (latweight(j) .LT. 0.0) latweight(j)=0.0
     WRITE(*,*) 'lat(',j,'):',lat,latweight(j)
     Lat=Lat+DLat
   END DO
   WRITE(*,*)' '
   !WRITE(*,*) 'Latitudinal weights:'
   !WRITE(*,'(8E13.5)') (latweight(j),j=1,jmax)
   RETURN
   END SUBROUTINE lweights

