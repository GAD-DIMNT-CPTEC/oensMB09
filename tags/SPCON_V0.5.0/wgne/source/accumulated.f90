  PROGRAM accumulated

  USE GaussRep,          ONLY : CreateGaussRep, glat
  USE ReadFields_accum,  ONLY : ReadGribFct,ReadGribIcn
  USE HorizontalInterp,  ONLY : horintfld
  
  IMPLICIT NONE

  !
  ! Program to evaluate the accumulated from the global ensemble 
  ! weather forecasting
  ! Author: Antonio Marcos Mendonca 
  ! Any comentary, send a e-mail to: marcos.mendonca@cptec.inpe.br
  ! Last Atualization: Oct/2007
  ! Modified : Cristiano Prestrelo (cristiano.prestrelo@cptec.inpe.br)
  ! 22/04/08
  !

  !
  ! Variables
  !
  !noutpday= (4) nº de saidas por dia (06, 12, 18, 00)
  !nfctdy= (1) nº de previsoes Obs. pq so usamos 1 dia, normal sao 15 (15 membros)
  !

  INTEGER, PARAMETER :: nbanllag=2,anllaghr=24

  INTEGER :: i,j,nbs,nmb,nout,nolr,nanl
  INTEGER :: imax,jmax,jmaxhf
  INTEGER :: imaxint,jmaxint
  INTEGER :: iwesttr,ieasttr,jsouttr,jnorttr
  
  INTEGER :: nfctdy,nmembers,noutpday,ngrbs,freqcalc
  INTEGER :: ierr,statfctrd,rest,nhwrt
  INTEGER, ALLOCATABLE, DIMENSION(:)     :: fcthours

  REAL               :: undef       ! Undef value set up according to original grib files
  REAL               :: lonwtr=0.0, lonetr=360.0, latstr=-15.4283,latntr=15.4283  !latstr=-90,latntr=90    

  REAL, ALLOCATABLE, DIMENSION(:)        :: latweight
  REAL, ALLOCATABLE, DIMENSION(:)        :: sumw
  REAL, ALLOCATABLE, DIMENSION(:)        :: medrole
  REAL, ALLOCATABLE, DIMENSION(:)        :: medu200
  REAL, ALLOCATABLE, DIMENSION(:)        :: medu850
  
  REAL, ALLOCATABLE, DIMENSION(:,:)      :: field                         ! field of forecasted Radiation Outgoing Longwave
  REAL, ALLOCATABLE, DIMENSION(:,:)      :: fieldb                        ! field of forecasted u200 
  REAL, ALLOCATABLE, DIMENSION(:,:)      :: fieldc                        ! field of forecasted u850
  REAL, ALLOCATABLE, DIMENSION(:,:)      :: roleint                       ! field of forecasted ROLE	      new scale  
  REAL, ALLOCATABLE, DIMENSION(:,:)      :: u200int                       ! field of forecasted vento u200    new scale
  REAL, ALLOCATABLE, DIMENSION(:,:)      :: u850int                       ! field of forecasted vento u850    new scale 
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: ROLE                          ! accumulated Radiation Outgoing Longwave
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: U200                          ! accumulated U200
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: U850                          ! accumulated U850
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: roleinc                       ! forecasted ROLE	     for each member and lead time
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: U200inc                       ! forecasted vento u200    for each member and lead time  
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: U850inc                       ! forecasted vento u850    for each member and lead time  
  
  CHARACTER(LEN=10)  :: datei                                             ! initial date
  CHARACTER(LEN=200) :: dirinp                                            ! input directory where is the files of the ensemble members        
  CHARACTER(LEN=200) :: dirout                                            ! output directory for accumulated
  CHARACTER(LEN=200) :: accumsetup                                        ! namelist file
  CHARACTER(LEN=200) :: resol                                             ! and vertical model resolution
  CHARACTER(LEN=200) :: prefx                                             ! preffix for input and output files
  CHARACTER(LEN=200) :: lstoutgrb_role
  CHARACTER(LEN=200) :: lstoutgrb_U200
  CHARACTER(LEN=200) :: lstoutgrb_U850       
  CHARACTER(len=200), ALLOCATABLE, DIMENSION(:)     :: diranldate
  CHARACTER(LEN=10),  ALLOCATABLE, DIMENSION(:)     :: anldate
  CHARACTER(LEN=10),  ALLOCATABLE, DIMENSION(:,:)   :: fctdate
  CHARACTER(LEN=200), ALLOCATABLE, DIMENSION(:,:,:) :: lstingrb             ! name of input ctl's and bin's (roleaccum)

  LOGICAL :: exarq

  !
  ! Attributions
  !

  CALL GETARG(1,datei)
  WRITE(accumsetup,'(3A)')'accumsetup.',datei,'.nml'

  !
  ! Call attribution function
  !FILE_100 = accumsetup.2008051900.nml

  OPEN(100,FILE=TRIM(accumsetup),STATUS='OLD',FORM='FORMATTED')

  CALL attribute(undef,imax,jmax,imaxint,jmaxint,nmembers, &
                 nfctdy,freqcalc,dirinp,dirout,resol,prefx) 
  jmaxhf=jmax/2

  CLOSE(100)

  WRITE(*,*)'undef   : ',undef
  WRITE(*,*)'imax    : ',imax
  WRITE(*,*)'jmax    : ',jmax
  WRITE(*,*)'imaxint : ',imaxint
  WRITE(*,*)'jmaxint : ',jmaxint
  WRITE(*,*)'nmembers: ',nmembers
  WRITE(*,*)'nfctdy  : ',nfctdy
  WRITE(*,*)'freqcalc: ',freqcalc
  WRITE(*,*)'dirinp  : ',TRIM(dirinp)
  WRITE(*,*)'dirout  : ',TRIM(dirout)
  WRITE(*,*)'resol   : ',TRIM(resol)
  WRITE(*,*)'prefx   : ',TRIM(prefx)

  IF (freqcalc .GT. 24) STOP 'Main Program: freqcalc must be .LE. 24'
  rest=MOD(24,freqcalc)
  IF (rest .NE. 0) THEN
    STOP 'Main Program: freqcalc must be a divisor of 24. Ex.: 6, 12 and 24'
  ELSE
    noutpday=24/freqcalc
  print*,'passou'
  END IF

  ngrbs=nfctdy*noutpday+2 !number of gribs icn + inz + fct's

  !Obs:  ngrbs = (1 * 4) + 2 = 6
  
  !
  !  Make dynamic allocation of memory 
  !

  ALLOCATE(latweight(jmaxint), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: latweight'
  ALLOCATE(fcthours(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fcthours'
  ALLOCATE(anldate(nbanllag), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: anldate'
  ALLOCATE(fctdate(ngrbs,nbanllag), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fctdate'
  ALLOCATE(lstingrb(nmembers,ngrbs,nbanllag), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstingrb'

  ALLOCATE(sumw(imaxint), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: sumw'
  ALLOCATE(medrole(imaxint), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: medrole'
  ALLOCATE(medu200(imaxint), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: medu200'
  ALLOCATE(medu850(imaxint), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: medu850'

  ALLOCATE(field(imax,jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: field' 
  ALLOCATE(fieldb(imax,jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fieldb'
  ALLOCATE(fieldc(imax,jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fieldc'

  ALLOCATE(roleint(imaxint,jmaxint), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: roleint' 
  ALLOCATE(roleinc(imaxint,jmaxint,nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: roleinc'
  ALLOCATE(ROLE(imaxint,jmaxint,nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: ROLE'

  ALLOCATE(u200int(imaxint,jmaxint), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: u200int'
  ALLOCATE(U200inc(imaxint,jmaxint,nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: U200inc'
  ALLOCATE(U200(imaxint,jmaxint,nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: U200'

  ALLOCATE(u850int(imaxint,jmaxint), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: u850int'
  ALLOCATE(U850inc(imaxint,jmaxint,nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: U850inc'
  ALLOCATE(U850(imaxint,jmaxint,nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: U850'

  ALLOCATE(diranldate(nbanllag))
  
  CALL CreateGaussRep(jmax,jmaxhf) ! Obtain gaussian latitudes
  
  !
  ! Generate the list of forecast output dates and hours
  !

  !CALL caldate(datei,ngrbs,freqcalc,fctdate,fcthours)
  CALL Caldate2(datei,nbanllag,anllaghr,ngrbs,freqcalc,anldate,fctdate,fcthours)

  !
  ! Generate the list of forecast files
  ! Generate the name of the output files
  !
  


  CALL getlstgrb(anldate,nbanllag,fctdate,ngrbs,nmembers,prefx,resol,dirinp, &
                 lstingrb,lstoutgrb_role,lstoutgrb_U200, &
		 lstoutgrb_U850,diranldate)		 

  !
  !Compute Latitudinal weights
  !

  CALL lweights(jmaxint,latweight)

  !
  !  Read forecast data and accumulate
  !  and verify the existence of files
  !

  WRITE(*,*)' '
  roleinc(:,:,:,:)=0.0
  U200inc(:,:,:,:)=0.0  
  U850inc(:,:,:,:)=0.0  

  DO nbs=1,ngrbs

     IF (nbs .EQ. 1) THEN

           !for lagged 24h fct OLR valid to analysis date

           DO nolr=3,6
             nanl=2
             INQUIRE(FILE=TRIM(diranldate(nanl))//TRIM(lstingrb(nmembers,nolr,nanl)),EXIST=exarq)
             IF (exarq) THEN
             	 WRITE(*,*)'unit70: ',TRIM(diranldate(nanl))//TRIM(lstingrb(nmembers,nolr,nanl))
             	 statfctrd=0
    	     	 CALL ReadGribFct(TRIM(diranldate(nanl))//TRIM(lstingrb(nmembers,nolr,nanl)),imax,jmax,field,fieldb,fieldc,statfctrd)
             	 IF (statfctrd .NE. 0) STOP 'Main Program: It does not read grib correctly'
             	 ! obs.: Output from horintfld is oriented: North to South, East to Weast (from prime meridian)
	     	 CALL horintfld(field,imax,jmax,roleint,imaxint,jmaxint)

	         DO j=1,jmaxint
	            DO i=1,imaxint
                       roleinc(i,j,nmembers,nbs)=roleinc(i,j,nmembers,nbs)+roleint(i,j)
                    END DO
                 END DO

             ELSE
             	 WRITE(*,*) 'THE FORECAST FILE DOES NOT EXIST: ',TRIM(diranldate(nanl))//TRIM(lstingrb(nmembers,nolr,nanl))
	     	 STOP
             END IF
           END DO
	   DO j=1,jmaxint
	      DO i=1,imaxint
                 roleinc(i,j,nmembers,nbs)=roleinc(i,j,nmembers,nbs)/FLOAT(noutpday)
              END DO
           END DO


           !for instantaneous analysis wind
           nanl=1
           INQUIRE(FILE=TRIM(diranldate(nanl))//TRIM(lstingrb(nmembers,nbs,nanl)),EXIST=exarq)
           IF (exarq) THEN
               WRITE(*,*)'unit70: ',TRIM(diranldate(nanl))//TRIM(lstingrb(nmembers,nbs,nanl))
               statfctrd=0
    	       CALL ReadGribIcn(TRIM(diranldate(nanl))//TRIM(lstingrb(nmembers,nbs,nanl)),imax,jmax,fieldb,fieldc,statfctrd)
               IF (statfctrd .NE. 0) STOP 'Main Program: It does not read grib correctly'
               ! obs.: Output from horintfld is oriented: North to South, East to Weast (from prime meridian)
	       CALL horintfld(fieldb,imax,jmax,u200int,imaxint,jmaxint)	
	       CALL horintfld(fieldc,imax,jmax,u850int,imaxint,jmaxint)
           ELSE
               WRITE(*,*) 'THE FORECAST FILE DOES NOT EXIST: ',TRIM(diranldate(nanl))//TRIM(lstingrb(nmembers,nbs,nanl))
	       STOP
           END IF
           DO nmb=1,nmembers 
             DO i=1,imaxint
             	DO j=1,jmaxint
             	   roleinc(i,j,nmb,nbs)=roleinc(i,j,nmembers,nbs)	       
             	END DO
             END DO 
	     DO i=1,imaxint
             	DO j=1,jmaxint
             	   U200inc(i,j,nmb,nbs)=u200int(i,j)	       
             	END DO
             END DO
	     DO i=1,imaxint
             	DO j=1,jmaxint
             	   U850inc(i,j,nmb,nbs)=u850int(i,j)	       
             	END DO
             END DO
           END DO
     END IF


     IF (fcthours(nbs) .GT. 0) THEN
        nanl=1
        DO nmb=1,nmembers 
           INQUIRE(FILE=TRIM(diranldate(nanl))//TRIM(lstingrb(nmb,nbs,nanl)),EXIST=exarq)
           IF (exarq) THEN
               WRITE(*,*)'unit70: ',TRIM(diranldate(nanl))//TRIM(lstingrb(nmb,nbs,nanl))
               statfctrd=0
    	       CALL ReadGribFct(TRIM(diranldate(nanl))//TRIM(lstingrb(nmb,nbs,nanl)),imax,jmax,field,fieldb,fieldc,statfctrd)
               IF (statfctrd .NE. 0) STOP 'Main Program: It does not read grib correctly'
               ! obs.: Output from horintfld is oriented: North to South, East to Weast (from prime meridian)
	       CALL horintfld(field,imax,jmax,roleint,imaxint,jmaxint)
	       CALL horintfld(fieldb,imax,jmax,u200int,imaxint,jmaxint)	
	       CALL horintfld(fieldc,imax,jmax,u850int,imaxint,jmaxint)
           ELSE
               WRITE(*,*) 'THE FORECAST FILE DOES NOT EXIST: ',TRIM(diranldate(nanl))//TRIM(lstingrb(nmembers,nbs,nanl))	 
	       STOP
           END IF
           DO i=1,imaxint
              DO j=1,jmaxint
                 roleinc(i,j,nmb,nbs)=roleint(i,j)		 
              END DO
           END DO 
	   DO i=1,imaxint
              DO j=1,jmaxint
                 U200inc(i,j,nmb,nbs)=u200int(i,j)		 
              END DO
           END DO
	   DO i=1,imaxint
              DO j=1,jmaxint
                 U850inc(i,j,nmb,nbs)=u850int(i,j)		 
              END DO
           END DO
        END DO
     END IF
  END DO

  WRITE(*,*) ' '	
  WRITE(*,*) 'Tropical Region:'
  CALL getpoints(lonwtr,lonetr,latstr,latntr,imaxint,jmaxint,iwesttr,ieasttr,jsouttr,jnorttr)
  
  !
  ! Obtain the points (x=1..n,y=1..n) for each region
  !
  
  !**********************************************************************************
  !!! Salvando roleint
  !**********************************************************************************
 
  ROLE(:,:,:,:)=0.0
  DO nbs=1,ngrbs
     IF(fcthours(nbs).GE.24)THEN
        DO nmb=1,nmembers
           DO i=1,imaxint
	      DO j=jnorttr,jsouttr
	         DO nout=nbs-noutpday+1,nbs
                    IF (( roleinc(i,j,nmb,nbs) .NE. undef ) .AND. ( ROLE(i,j,nmb,nbs) .NE. undef ) ) THEN
                          ROLE(i,j,nmb,nbs)=ROLE(i,j,nmb,nbs)+roleinc(i,j,nmb,nbs)
                    ELSE
                          ROLE(i,j,nmb,nbs)=undef
	            END IF
	         END DO
              END DO
           END DO             
	   DO j=jnorttr,jsouttr
	      DO i=1,imaxint
                 IF ( ROLE(i,j,nmb,nbs) .NE. undef ) THEN	      
                      ROLE(i,j,nmb,nbs)=ROLE(i,j,nmb,nbs)/FLOAT(noutpday)
		 END IF
              END DO
	   END DO
        END DO
     ELSE
        IF(nbs .EQ. 1) THEN
           DO nmb=1,nmembers
              DO i=1,imaxint
	   	 DO j=jnorttr,jsouttr
           	     ROLE(i,j,nmb,nbs)=roleinc(i,j,nmb,nbs)
           	 END DO
              END DO		 
           END DO
	ELSE
           DO nmb=1,nmembers
              DO i=1,imaxint
	   	 DO j=jnorttr,jsouttr
           	     ROLE(i,j,nmb,nbs)=undef
           	 END DO
              END DO		 
           END DO
        END IF
     END IF
  END DO

  WRITE(*,*)'unit90: ',TRIM(dirout)//TRIM(lstoutgrb_role)
  OPEN(90,FILE=TRIM(dirout)//TRIM(lstoutgrb_role),FORM='FORMATTED',STATUS='UNKNOWN')

  DO nmb=1,nmembers
     nhwrt=24
     DO nbs=1,ngrbs
        IF(nbs .EQ. 1)THEN
           medrole(:)=0.0
           sumw(:)=0.0
	   DO i=1,imaxint
	      DO j=jnorttr,jsouttr
                 medrole(i)=medrole(i)+(ROLE(i,j,nmb,nbs)*latweight(j))
	         sumw(i)=sumw(i)+latweight(j)
              END DO
   	         medrole(i)=medrole(i)/sumw(i)
           END DO
           WRITE(90,'(144(2X,F10.4))')(medrole(i),i=1,imaxint)
	   WRITE(90,'(A)') ' '
        END IF
        IF(fcthours(nbs) .EQ. nhwrt)THEN
          medrole(:)=0.0
          sumw(:)=0.0
	  DO i=1,imaxint
	    DO j=jnorttr,jsouttr
              medrole(i)=medrole(i)+(ROLE(i,j,nmb,nbs)*latweight(j))
	      sumw(i)=sumw(i)+latweight(j)
            END DO
   	    medrole(i)=medrole(i)/sumw(i)
          END DO
            WRITE(90,'(144(2X,F10.4))')(medrole(i),i=1,imaxint)
	    WRITE(90,'(A)') ' '
            nhwrt=nhwrt+24
        END IF
     END DO
     WRITE(*,*) ' '
  END DO
  CLOSE(90)

  !**********************************************************************************
  !**********************************************************************************

  U200(:,:,:,:)=0.0
  DO nbs=1,ngrbs
     IF(fcthours(nbs).GE.24)THEN
        DO nmb=1,nmembers
           DO i=1,imaxint
	      DO j=jnorttr,jsouttr
	         DO nout=nbs-noutpday+1,nbs
                    IF (( U200inc(i,j,nmb,nbs) .NE. undef ) .AND. ( U200(i,j,nmb,nbs) .NE. undef ) ) THEN
                          U200(i,j,nmb,nbs)=U200(i,j,nmb,nbs)+U200inc(i,j,nmb,nbs)
                    ELSE
                          U200(i,j,nmb,nbs)=undef
	            END IF
	         END DO
              END DO
           END DO             
	   DO j=jnorttr,jsouttr
	      DO i=1,imaxint
                 IF ( U200(i,j,nmb,nbs) .NE. undef ) THEN	      
                      U200(i,j,nmb,nbs)=U200(i,j,nmb,nbs)/FLOAT(noutpday)
		 END IF
              END DO
	   END DO
        END DO
     ELSE
        IF(nbs .EQ. 1) THEN
           DO nmb=1,nmembers
              DO i=1,imaxint
	   	 DO j=jnorttr,jsouttr
           	     U200(i,j,nmb,nbs)=U200inc(i,j,nmb,nbs)
           	 END DO
              END DO		 
           END DO
	ELSE
           DO nmb=1,nmembers
              DO i=1,imaxint
	   	 DO j=jnorttr,jsouttr
           	     U200(i,j,nmb,nbs)=undef
           	 END DO
              END DO		 
           END DO
        END IF
     END IF
  END DO
  
  WRITE(*,*)'unit91: ',TRIM(dirout)//TRIM(lstoutgrb_U200)
  OPEN(91,FILE=TRIM(dirout)//TRIM(lstoutgrb_U200),FORM='FORMATTED',STATUS='UNKNOWN')

  DO nmb=1,nmembers
     nhwrt=24
     DO nbs=1,ngrbs
        IF(nbs .EQ. 1)THEN
           medu200(:)=0.0
           sumw(:)=0.0
	   DO i=1,imaxint
	      DO j=jnorttr,jsouttr
                 medu200(i)=medu200(i)+(U200(i,j,nmb,nbs)*latweight(j))
	         sumw(i)=sumw(i)+latweight(j)
              END DO
   	         medu200(i)=medu200(i)/sumw(i)
           END DO
           WRITE(91,'(144(2X,F10.4))')(medu200(i),i=1,imaxint)
	   WRITE(91,'(A)') ' '
        END IF
        IF(fcthours(nbs) .EQ. nhwrt)THEN
           medu200(:)=0.0
           sumw(:)=0.0
	   DO i=1,imaxint
	      DO j=jnorttr,jsouttr
                 medu200(i)=medu200(i)+(U200(i,j,nmb,nbs)*latweight(j))
	         sumw(i)=sumw(i)+latweight(j)
              END DO
   	         medu200(i)=medu200(i)/sumw(i)
           END DO
           WRITE(91,'(144(2X,F10.4))')(medu200(i),i=1,imaxint)
	   WRITE(91,'(A)') ' '
           nhwrt=nhwrt+24
        END IF
     END DO
     WRITE(*,*) ' '
  END DO
  CLOSE(91)
  
  !**********************************************************************************
  !**********************************************************************************
  
  U850(:,:,:,:)=0.0
  DO nbs=1,ngrbs
     IF(fcthours(nbs).GE.24)THEN
        DO nmb=1,nmembers
           DO i=1,imaxint
	      DO j=jnorttr,jsouttr
	         DO nout=nbs-noutpday+1,nbs
                    IF (( U850inc(i,j,nmb,nbs) .NE. undef ) .AND. ( U850(i,j,nmb,nbs) .NE. undef ) ) THEN
                          U850(i,j,nmb,nbs)=U850(i,j,nmb,nbs)+U850inc(i,j,nmb,nbs)
                    ELSE
                          U850(i,j,nmb,nbs)=undef
	            END IF
	         END DO
              END DO
           END DO             
	   DO j=jnorttr,jsouttr
	      DO i=1,imaxint
                 IF ( U850(i,j,nmb,nbs) .NE. undef ) THEN	      
                      U850(i,j,nmb,nbs)=U850(i,j,nmb,nbs)/FLOAT(noutpday)
		 END IF
              END DO
	   END DO
        END DO
     ELSE
        IF(nbs .EQ. 1) THEN
           DO nmb=1,nmembers
              DO i=1,imaxint
	   	 DO j=jnorttr,jsouttr
           	     U850(i,j,nmb,nbs)=U850inc(i,j,nmb,nbs)
           	 END DO
              END DO		 
           END DO
	ELSE
           DO nmb=1,nmembers
              DO i=1,imaxint
	         DO j=jnorttr,jsouttr
                    U850(i,j,nmb,nbs)=undef
                 END DO
              END DO             
           END DO
	END IF
     END IF
  END DO
  
 WRITE(*,*)'unit92: ',TRIM(dirout)//TRIM(lstoutgrb_U850)
 OPEN(92,FILE=TRIM(dirout)//TRIM(lstoutgrb_U850),FORM='FORMATTED',STATUS='UNKNOWN')

  DO nmb=1,nmembers
     nhwrt=24
     DO nbs=1,ngrbs
        IF(nbs .EQ. 1)THEN
           medu850(:)=0.0
           sumw(:)=0.0
	   DO i=1,imaxint
	      DO j=jnorttr,jsouttr
                 medu850(i)=medu850(i)+(U850(i,j,nmb,nbs)*latweight(j))
	         sumw(i)=sumw(i)+latweight(j)
              END DO
   	         medu850(i)=medu850(i)/sumw(i)
           END DO
	   WRITE(92,'(144(2X,F10.4))')(medu850(i),i=1,imaxint)
	   WRITE(92,'(A)') ' '
        END IF
        IF(fcthours(nbs) .EQ. nhwrt)THEN
           medu850(:)=0.0
           sumw(:)=0.0
	   DO i=1,imaxint
	      DO j=jnorttr,jsouttr
                 medu850(i)=medu850(i)+(U850(i,j,nmb,nbs)*latweight(j))
	         sumw(i)=sumw(i)+latweight(j)
              END DO
   	         medu850(i)=medu850(i)/sumw(i)
           END DO
           WRITE(92,'(144(2X,F10.4))')(medu850(i),i=1,imaxint)
	   WRITE(92,'(A)') ' '
           nhwrt=nhwrt+24
        END IF
     END DO
     WRITE(*,*) ' '
  END DO
  CLOSE(92)
  
  END PROGRAM accumulated
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to obtain the respective points of the selected region
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE getpoints(lonw,lone,lats,latn,imaxint,jmaxint,iwest,ieast,jsout,jnort)

  IMPLICIT NONE
  
  INTEGER,               INTENT(IN)  :: imaxint, jmaxint
  INTEGER,               INTENT(OUT) :: iwest, ieast, jsout, jnort
  REAL,                  INTENT(IN)  :: lonw, lone, lats, latn
  
  INTEGER  :: i, j
  REAL     :: lonaux, lataux, difaux, dlon, dlat, difminn, difmins, difmine, difminw
  REAL     :: lonw2, lone2

     IF (lonw .GE. lone) THEN
        WRITE(*,*) 'lonw=', lonw 
        WRITE(*,*) 'lone=', lone
        STOP 'lonw greater or equal to lone'
     ENDIF
     IF (lats .GE. latn) THEN
        WRITE(*,*) 'lats=', lats 
        WRITE(*,*) 'latn=', latn
        STOP 'lats greater or equal to latn'
     ENDIF

     lone2=lone
     lonw2=lonw
     IF ( (lonw2 .EQ. -180.0) .AND. (lone2 .EQ. 180.0) ) THEN
        lonw2=  0.0
        lone2=360.0
     END IF
     IF (lonw2 .LT. 0.0) THEN
        lonw2=360.0+lonw2
     END IF
     IF (lone2 .LT. 0.0) THEN
        lone2=360.0+lone2
     END IF

     dlon=2.5
     iwest=0
     ieast=0
     lonaux=0.0
     difminw=1e37
     difmine=1e37
     DO i=1,imaxint
       difaux=lonaux-lonw2
       IF (ABS(difaux) .LT. difminw)  THEN
          difminw=ABS(difaux)
          iwest=i
       END IF
       difaux=lonaux-lone2
       IF (ABS(difaux) .LT. difmine) THEN
          difmine=ABS(difaux)
          ieast=i
       END IF
       lonaux=lonaux+dlon
     END DO
     IF (ieast .EQ. iwest) THEN     !amm
        ieast=ieast+1
     END IF

     WRITE(*,*)'difminw=',difminw,' iwest=',iwest
     WRITE(*,*)'difmine=',difmine,' ieast=',ieast

     dlat=2.5
     jnort=0
     jsout=0
     lataux=90.0
     difminn=1e37
     difmins=1e37
     DO j=1,jmaxint
       difaux=latn-lataux
       IF (ABS(difaux) .LT. difminn)  THEN
          difminn=ABS(difaux)
          jnort=j
       END IF
       difaux=lats-lataux
       IF (ABS(difaux) .LT. difmins) THEN
          difmins=ABS(difaux)
          jsout=j
       END IF
       lataux=lataux-dlat
     END DO

     WRITE(*,*)'difminn=',difminn,' jnort=',jnort
     WRITE(*,*)'difmins=',difmins,' jsout=',jsout

  RETURN

  END SUBROUTINE getpoints

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to set up basic variables
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE attribute(undef,imax,jmax,imaxint,jmaxint,nmembers, &
                       nfctdy,freqcalc,dirinp,dirout,resol,prefx) 

  IMPLICIT NONE

  INTEGER,           INTENT(OUT)  :: imax,jmax,imaxint,jmaxint
  INTEGER,           INTENT(OUT)  :: nmembers,nfctdy,freqcalc
  REAL,              INTENT(OUT)  :: undef
  CHARACTER (LEN=*), INTENT(OUT)  :: dirinp,dirout,resol,prefx

  CHARACTER (LEN=200) :: line,line2
  INTEGER             :: ierr

  ierr=0      ! To start the loop

  DO WHILE (ierr .EQ. 0)

         READ(100,'(A14,A)', IOSTAT=ierr) line,line2
              IF( line(1:10).EQ.'UNDEF     ' ) THEN
               READ(line2(1:10),'(F5.0)')undef
         ELSE IF( line(1:10).EQ.'IMAX      ' ) THEN
               READ(line2(1:4),'(I4)')  imax
         ELSE IF( line(1:10).EQ.'JMAX      ' ) THEN
               READ(line2(1:4),'(I4)')  jmax	       
	 ELSE IF (line(1:10).EQ.'IMAXINT   ' ) THEN     
               READ(line2(1:4),'(I4)')  imaxint
         ELSE IF( line(1:10).EQ.'JMAXINT   ' ) THEN
               READ(line2(1:4),'(I4)')  jmaxint	       
         ELSE IF( line(1:10).EQ.'NMEMBERS  ' ) THEN 
               READ(line2(1:4),'(I4)')  nmembers
         ELSE IF( line(1:10).EQ.'NFCTDY    ' ) THEN 
               READ(line2(1:4),'(I4)')  nfctdy
         ELSE IF( line(1:10).EQ.'FREQCALC  ' ) THEN 
               READ(line2(1:4),'(I4)')  freqcalc
         ELSE IF( line(1:10).EQ.'DIRINP    ' ) THEN
               dirinp=TRIM(line2)
         ELSE IF( line(1:10).EQ.'DIROUT    ' ) THEN 
               dirout=TRIM(line2)
         ELSE IF( line(1:10).EQ.'RESOL     ' ) THEN 
               resol=TRIM(line2)
         ELSE IF( line(1:10).EQ.'PREFX     ' ) THEN 
               prefx=TRIM(line2)
         END IF
  ENDDO
  RETURN
  END SUBROUTINE attribute
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to evaluate the latitudinal weights
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE lweights(jmaxint,latweight)

  IMPLICIT NONE

  INTEGER,                   INTENT(IN)  :: jmaxint
  REAL, DIMENSION(jmaxint),  INTENT(OUT) :: latweight

  INTEGER  :: j
  REAL     :: pi,dlon,lat,latr

  pi=4.0*ATAN(1.0)

  lat=90.0
  dlon=2.5
  DO j=1,jmaxint
     latr=(pi*lat)/180.0
     latweight(j)=cos(latr)
     IF (latweight(j) .LT. 0.0) latweight(j)=0.0
     WRITE(*,*) 'lat(',j,'):',lat,latweight(j)
     lat=lat-dlon
  END DO

  WRITE(*,*)' '
  WRITE(*,*) 'Latitudinal weights:'
  WRITE(*,'(8E13.5)') (latweight(j),j=1,jmaxint)

  RETURN
  END SUBROUTINE lweights
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to calculate the dates of forecast files
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  
  SUBROUTINE caldate(anldate,ngrbs,freqcalc,fctdate,fcthours)
  
  IMPLICIT NONE
  
  INTEGER,                             INTENT(IN)  :: ngrbs,freqcalc
  CHARACTER(len=10),                   INTENT(IN)  :: anldate

  INTEGER,           DIMENSION(ngrbs), INTENT(OUT) :: fcthours
  CHARACTER(len=10), DIMENSION(ngrbs), INTENT(OUT) :: fctdate

!+++++Local variables

  INTEGER                :: i,iyy,imm,idd,ihh,rest,lastd,inthour
  INTEGER, DIMENSION(12) :: monl,monlb
  
  CHARACTER(len=10)      :: datep 

!+++++
  
  DATA monl  /31,28,31,30,31,30,31,31,30,31,30,31/
  DATA monlb /31,29,31,30,31,30,31,31,30,31,30,31/

  READ(anldate(1: 4),'(I4)') iyy
  READ(anldate(5: 6),'(I2)') imm
  READ(anldate(7: 8),'(I2)') idd
  READ(anldate(9:10),'(I2)') ihh

  inthour=freqcalc

  IF (inthour .GT. 24) STOP 'Caldate: variable inthour must be less or equal 24 hours'
  WRITE(*,'(/,A)') 'FCT DATES:'

  fcthours(:)=0
  rest=MOD(iyy,4)	! Decide if is a leap year

  IF (rest .EQ. 0) THEN
     lastd=monlb(imm)
  ELSE
     lastd=monl(imm)
  END IF

  !i=1
  WRITE(datep,'(I4.4,3I2.2)') iyy,imm,idd,ihh
  fctdate(1)(1:10)=datep(1:10)
  WRITE(*,'(2A)') 'fctdate(1):',fctdate(1)(1:10)

  !i=2
  WRITE(datep,'(I4.4,3I2.2)') iyy,imm,idd,ihh
  fctdate(2)(1:10)=datep(1:10)
  WRITE(*,'(2A)') 'fctdate(2):',fctdate(2)(1:10)

  DO i=3,ngrbs
     fcthours(i)=(i-2)*inthour
     ihh=ihh+inthour
     IF (ihh .GE. 24) THEN
     	ihh=ihh-24
        idd=idd+1
        IF (idd .GT. lastd) THEN
           idd=01
           imm=imm+1
           IF (imm .GT. 12) THEN
	      imm=01
	      iyy=iyy+1
	   END IF
           rest=MOD(iyy,4)	! Decide if is a leap year
           IF (rest .EQ. 0) THEN
     	      lastd=monlb(imm)
           ELSE
             lastd=monl(imm)
           END IF
        END IF
     END IF
     WRITE(datep,'(I4.4,3I2.2)') iyy,imm,idd,ihh
     fctdate(i)(1:10)=datep(1:10)
  END DO
  RETURN
  END SUBROUTINE caldate



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  !
  ! Subroutine to calculate the dates of forecast files
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
    
  SUBROUTINE Caldate2(labeli,nbanllag,anllaghr,ngrbs,freqcalc,anldate,fctdate,fcthours)
  
  IMPLICIT NONE
  
  !External variables
  INTEGER,           INTENT(IN)  :: ngrbs,freqcalc,anllaghr,nbanllag
  CHARACTER(len=10), INTENT(IN)  :: labeli

  INTEGER,           DIMENSION(ngrbs),          INTENT(OUT) :: fcthours
  CHARACTER(len=10), DIMENSION(nbanllag),       INTENT(OUT) :: anldate
  CHARACTER(len=10), DIMENSION(ngrbs,nbanllag), INTENT(OUT) :: fctdate

  
  !Local variables
  INTEGER   :: i,iyy,imm,idd,ihh,rest,lastd,inthour
  INTEGER   :: lyy,lmm,ldd,lhh,nanl

  INTEGER, DIMENSION(12) :: monl,monlb
  
  CHARACTER(len=10)      :: datep 

  DATA monl  /31,28,31,30,31,30,31,31,30,31,30,31/
  DATA monlb /31,29,31,30,31,30,31,31,30,31,30,31/


  !Dynamic allocation
  !ALLOCATE (anldate(nbanllag),fcthours(ngrbs),fctdate(ngrbs))


  !Begin of computation
  READ(labeli(1: 4),'(I4)') iyy
  READ(labeli(5: 6),'(I2)') imm
  READ(labeli(7: 8),'(I2)') idd
  READ(labeli(9:10),'(I2)') ihh
  

  inthour=freqcalc

  IF (inthour .GT. 24) STOP 'Caldate: variable inthour must be less or equal 24 hours'

  IF (anllaghr .GT. 24) STOP 'Caldate: variable anllaghr must be less or equal 24 hours'

  
  !Compute lagged analyses
  WRITE(*,'(/,A)') 'ANL LAGGED DATES:'

  lyy=iyy
  lmm=imm
  ldd=idd
  lhh=ihh

  DO i=1,nbanllag

    IF (i .EQ. 1) THEN

      WRITE(datep,'(I4.4,3I2.2)') lyy,lmm,ldd,lhh	       
      anldate(i)(1:10)=datep(1:10)
      WRITE(*,'(A,I2,2A)') 'anldate(',i,'):',anldate(i)(1:10)	
 
    ELSE

      lhh=lhh-anllaghr  				       
      IF (lhh .LT. 00) THEN				       
    	 lhh=lhh+24					       
    	 ldd=ldd-1					       

    	 IF (ldd .LE. 0) THEN				       
    	    lmm=lmm-1					       
    	    IF (lmm .LE. 0) THEN			       
    	       lmm=12
    	       lyy=lyy-1
    	    END IF

    	    rest=MOD(lyy,4)	  ! Decide if is a leap year
    	    IF (rest .EQ. 0) THEN
    	       lastd=monlb(lmm)
    	    ELSE
    	       lastd=monl(lmm)
    	    END IF

    	    ldd=lastd					       

    	 END IF 					       
      END IF						       

      WRITE(datep,'(I4.4,3I2.2)') lyy,lmm,ldd,lhh	       
      anldate(i)(1:10)=datep(1:10)
      WRITE(*,'(A,I2,2A)') 'anldate(',i,'):',anldate(i)(1:10)	

    END IF						       

  END DO


  !Compute valid forecasts
  WRITE(*,'(/,A)') 'FCT DATES:'

  fcthours(:)=0

  DO nanl=1,nbanllag


  READ(anldate(nanl)(1: 4),'(I4)') iyy
  READ(anldate(nanl)(5: 6),'(I2)') imm
  READ(anldate(nanl)(7: 8),'(I2)') idd
  READ(anldate(nanl)(9:10),'(I2)') ihh


  rest=MOD(iyy,4)	! Decide if is a leap year
  IF (rest .EQ. 0) THEN
     lastd=monlb(imm)
  ELSE
     lastd=monl(imm)
  END IF

  i=1
  WRITE(datep,'(I4.4,3I2.2)') iyy,imm,idd,ihh
  fctdate(i,nanl)(1:10)=datep(1:10)
  WRITE(*,'(A,I2,A,I2,2A)') 'fctdate(',i,',',nanl,'):',fctdate(i,nanl)(1:10)

  i=2
  WRITE(datep,'(I4.4,3I2.2)') iyy,imm,idd,ihh
  fctdate(i,nanl)(1:10)=datep(1:10)
  WRITE(*,'(A,I2,A,I2,2A)') 'fctdate(',i,',',nanl,'):',fctdate(i,nanl)(1:10)

  DO i=3,ngrbs


     fcthours(i)=(i-2)*inthour
     
     ihh=ihh+inthour
     IF (ihh .GE. 24) THEN
     	ihh=ihh-24
        idd=idd+1
        IF (idd .GT. lastd) THEN
           idd=01
           imm=imm+1
           IF (imm .GT. 12) THEN
	      imm=01
	      iyy=iyy+1
	   END IF
           rest=MOD(iyy,4)	! Decide if is a leap year
           IF (rest .EQ. 0) THEN
     	      lastd=monlb(imm)
           ELSE
             lastd=monl(imm)
           END IF
        END IF
     END IF

     WRITE(datep,'(I4.4,3I2.2)') iyy,imm,idd,ihh
     fctdate(i,nanl)(1:10)=datep(1:10)

     WRITE(*,'(A,I2,A,I2,2A)') 'fctdate(',i,',',nanl,'):',fctdate(i,nanl)(1:10)
     
  END DO


  END DO

  RETURN
  END SUBROUTINE caldate2
 
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! teste de uma Subroutine para generar lista de saida
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  
  SUBROUTINE getlstgrb (anldate,nbanllag,fctdate,ngrbs,nmembers,prefx,resol,dirinp, &
                        lstingrb,lstoutgrb_role,lstoutgrb_U200, &
                        lstoutgrb_U850,diranldate)
  

  IMPLICIT NONE
  
  INTEGER,                                      INTENT(IN)  :: nmembers,ngrbs,nbanllag
  CHARACTER(len=10), DIMENSION(nbanllag),       INTENT(IN)  :: anldate
  CHARACTER(len=10), DIMENSION(ngrbs,nbanllag), INTENT(IN)  :: fctdate
  CHARACTER(LEN=*),                             INTENT(IN)  :: prefx,resol,dirinp                

  CHARACTER(len=*),  DIMENSION(nbanllag),                INTENT(OUT)  :: diranldate
  CHARACTER(len=*),  DIMENSION(nmembers,ngrbs,nbanllag), INTENT(OUT)  :: lstingrb
  CHARACTER(len=*),                   	                 INTENT(OUT)  :: lstoutgrb_role
  CHARACTER(len=*),  	                                 INTENT(OUT)  :: lstoutgrb_U200
  CHARACTER(len=*),  	                                 INTENT(OUT)  :: lstoutgrb_U850

!+++++Local variables

  INTEGER                :: k,nf,membm,nb

  CHARACTER(len=6), DIMENSION(2) :: extp
  
  DATA extp /'P.icn.','P.fct.'/

  ! fct files

  membm=(nmembers-1)/2
  !membm=(15-1)/2  !membm=7

  DO nf=1,ngrbs
   DO nb=1,nbanllag
    WRITE (*,*) "OIII2", nf, nbanllag,nmembers
     DO k=1,membm
        IF (nf .EQ. 1) THEN
           WRITE(lstingrb(k,nf,nb),'(A,I2.2,6A)')'GPOS',k,'P',anldate(nb)(1:10),fctdate(nf,nb)(1:10),'P.icn.',TRIM(resol),'.grb'
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstingrb(k,nf,nb),'(A,I2.2,6A)')'GPOS',k,'P',anldate(nb)(1:10),fctdate(nf,nb)(1:10),'P.inz.',TRIM(resol),'.grb'
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstingrb(k,nf,nb),'(A,I2.2,6A)')'GPOS',k,'P',anldate(nb)(1:10),fctdate(nf,nb)(1:10),'P.fct.',TRIM(resol),'.grb'
        END IF
     END DO
     DO k=1,membm
        IF (nf .EQ. 1) THEN
           WRITE(lstingrb(k+membm,nf,nb),'(A,I2.2,6A)')'GPOS',k,'N',anldate(nb)(1:10),fctdate(nf,nb)(1:10),'P.icn.',TRIM(resol),'.grb'
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstingrb(k+membm,nf,nb),'(A,I2.2,6A)')'GPOS',k,'N',anldate(nb)(1:10),fctdate(nf,nb)(1:10),'P.inz.',TRIM(resol),'.grb'
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstingrb(k+membm,nf,nb),'(A,I2.2,6A)')'GPOS',k,'N',anldate(nb)(1:10),fctdate(nf,nb)(1:10),'P.fct.',TRIM(resol),'.grb'
        END IF
     END DO
        IF (nf .EQ. 1) THEN
           WRITE(lstingrb(nmembers,nf,nb),'(7A)')'GPOS',TRIM(prefx),anldate(nb)(1:10),fctdate(nf,nb)(1:10),'P.icn.',TRIM(resol),'.grb'
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstingrb(nmembers,nf,nb),'(7A)')'GPOS',TRIM(prefx),anldate(nb)(1:10),fctdate(nf,nb)(1:10),'P.inz.',TRIM(resol),'.grb'
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstingrb(nmembers,nf,nb),'(7A)')'GPOS',TRIM(prefx),anldate(nb)(1:10),fctdate(nf,nb)(1:10),'P.fct.',TRIM(resol),'.grb'
        END IF	
   END DO
  END DO

 nb=1							   	  
 WRITE(lstoutgrb_role,'(2A)')anldate(nb)(1:10),'_CPTC_OLRA'	  
 WRITE(lstoutgrb_U200,'(2A)')anldate(nb)(1:10),'_CPTC_U200'	  
 WRITE(lstoutgrb_U850,'(2A)')anldate(nb)(1:10),'_CPTC_U850'	  

  DO nb=1,nbanllag
!AAF     WRITE(diranldate(nb),'(3A)')TRIM(dirinp),anldate(nb)(1:8),'/'
     WRITE(diranldate(nb),'(3A)')TRIM(dirinp),'/'
     WRITE(*,*)'diranldate(',nb,')=',TRIM(diranldate(nb))
  END DO

  RETURN
  END SUBROUTINE getlstgrb  
