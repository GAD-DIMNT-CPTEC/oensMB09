  PROGRAM spread
  USE GaussRep, ONLY : CreateGaussRep, glat

  IMPLICIT NONE

  !
  ! Program to evaluate the spread of the global ensemble 
  ! weather forecasting. The spread is defined as the standard deviation 
  ! of the ensemble members in respect to ensemble mean. 
  ! Authors: Antonio Marcos Mendonca and Fabio Augusto Fernandes
  ! Any comentary, send a e-mail to: mendonca@cptec.inpe.br
  ! Last Atualization: Oct/2007
  !

  ! nrecs: especify the number of records that are evaluated the spread
  ! nrecs=(3 rec. of uvel + 3 of vvel + 3 of fcor + 3 of potv + 3 of zgeo 
  !        + 1 of psnm + 3 of temp) = 19 (Currently: Oct/2007)
  ! The list of jpds informations is set up below and obtained from the grib 
  ! global ensemble output file  

  INTEGER, PARAMETER :: nrecs=19 

  INTEGER :: i,j,k,nmb,nbs,nr,ni,nj,nxny,ierr
  INTEGER :: imax,jmax,jmaxhf,nwords,knum
  INTEGER :: nmembers,nfctdy,freqcalc,noutpday,ngrbs,rest
  INTEGER :: iwest,ieast,jsout,jnort,nxpts,nypts
  INTEGER,   DIMENSION(200)    :: jpds,jgds,kpds,kgds
  INTEGER,   DIMENSION(nrecs)  :: jpds5,jpds6,jpds7
  INTEGER, ALLOCATABLE, DIMENSION(:) :: fcthours
       
  REAL                                :: lonw,lone,lats,latn
  REAL                                :: undef       ! Undef value set up according to original grib files
  REAL,ALLOCATABLE,DIMENSION(:)       :: tmp
  REAL,ALLOCATABLE,DIMENSION(:,:)     :: var,cm,sp
  REAL,ALLOCATABLE,DIMENSION(:,:,:,:) :: field
 
  CHARACTER(LEN=10)  :: datei                 ! initial and final dates
  CHARACTER(LEN=200) :: spreadsetup           ! namelist file
  CHARACTER(LEN=200) :: prefx                 ! preffix for input and output files
  CHARACTER(LEN=200) :: dirinp,dirout
  CHARACTER(LEN=200) :: resol, lstoutlst
  CHARACTER(LEN=10), ALLOCATABLE,DIMENSION(:)   :: fctdate
  CHARACTER(LEN=200),ALLOCATABLE,DIMENSION(:)   :: lstoutgrb,lstoutctl  ! output spread files: ctls and bins
  CHARACTER(LEN=200),ALLOCATABLE,DIMENSION(:,:) :: lstingrb             ! name of input ctl's and bin's

  LOGICAL                            :: exarq
  LOGICAL*1,ALLOCATABLE,DIMENSION(:) :: bitmap

  !
  ! Attributions   
  !
  
  DATA jpds5 / 33, 33, 33, 34, 34, 34, 35, 35, 35, 36, 36, 36, &
                7,  7,  7,  2, 11, 11, 11/
  DATA jpds6 /100,100,100,100,100,100,100,100,100,100,100,100, &
              100,100,100,102,100,100,100/
  DATA jpds7 /850,500,250,850,500,250,850,500,250,850,500,250, &
              850,500,250,  0,850,500,250/

  CALL GETARG(1,datei)
  WRITE(spreadsetup,'(3A)')'spreadsetup.',datei,'.nml'


  !
  ! Call attribution function
  !

  OPEN(100,FILE=TRIM(spreadsetup),STATUS='OLD',FORM='FORMATTED')

  CALL attribute(100,undef,imax,jmax,lonw,lone,lats,latn,  &
                 nmembers,nfctdy,freqcalc,                 &
		 dirinp,dirout,resol,prefx) 

  CLOSE(100)

  WRITE(*,*)'undef   : ',undef
  WRITE(*,*)'imax    : ',imax
  WRITE(*,*)'jmax    : ',jmax
  WRITE(*,*)'lonw    : ',lonw
  WRITE(*,*)'lone    : ',lone
  WRITE(*,*)'lats    : ',lats
  WRITE(*,*)'latn    : ',latn
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
  END IF

  ngrbs=nfctdy*noutpday+2 !number of gribs icn + inz + fct's
  nxny=imax*jmax
  
  !
  ! Obtain gaussian latitudes
  !

  jmaxhf=jmax/2
  CALL CreateGaussRep(jmax,jmaxhf)
  
  WRITE(*,*) ' '
  WRITE(*,*) 'Gaussian Latitudes:'
  WRITE(*,'(8F10.5)') (glat(j),j=jmax,1,-1)

  !
  ! Obtain the points (x=1..n,y=1..n) of the considered region
  !
  
  CALL getpoints(glat,lonw,lone,lats,latn,imax,jmax,iwest,ieast,jsout,jnort)
 
  IF (iwest .LE. ieast) THEN 
    nxpts=ieast-iwest+1
  ELSE                               ! This condition if lonw < 0 and lone > 0
    nxpts=(imax-iwest+1)+ieast
  END IF
  nypts=jsout-jnort+1  ! It is supposed that data are organized from northern to southern.
                       ! Currently default of global model.

  WRITE(*,*) ' '
  WRITE(*,*) 'nxpts= ',nxpts
  WRITE(*,*) 'nypts= ',nypts

  !
  ! Make dynamic allocation of memory 
  !

  ALLOCATE(fcthours(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fcthours'

  ALLOCATE(fctdate(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fctdate'

  ALLOCATE(lstoutgrb(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstoutgrb'
  
  ALLOCATE(lstoutctl(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstoutctl'

  ALLOCATE(tmp(nxny), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: tmp'
  
  ALLOCATE(bitmap(nxny), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: bitmap'
  
  ALLOCATE(lstingrb(nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstingrb'

  ALLOCATE(var(imax,jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: var'
  
  ALLOCATE(cm(nxpts,nypts), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: cm'
  
  ALLOCATE(sp(nxpts,nypts), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: sp'
  
  ALLOCATE(field(nxpts,nypts,nmembers,nrecs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: field'
  
  
  !
  ! Generate the list of forecast output dates and hours
  !

  CALL caldate(datei,ngrbs,freqcalc,fctdate,fcthours)


  !
  ! Generate the list of forecast files
  ! Generate the name of the output files
  !

  CALL getlstgrb(datei,fctdate,ngrbs,nmembers,prefx,resol, &
                 lstingrb,lstoutctl,lstoutgrb,lstoutlst)


  !
  ! Loop to evaluate the spread for all range of outputs
  !


  WRITE(*,*)'unit79: ',TRIM(dirout)//TRIM(lstoutlst)
  OPEN(79,FILE=TRIM(dirout)//TRIM(lstoutlst),FORM='FORMATTED',STATUS='UNKNOWN')

  jpds(:)=-1
  jgds(:)=-1

  DO nbs=1,ngrbs !++++++++++Begin Main Loop

     WRITE(*,'(/,A,/)') '*****************************'
     WRITE(*,'(A,I3,A)')      'evaluating spread for forecast hour :',fcthours(nbs),' hs'
     WRITE(*,'(/,A,/)') '*****************************'

     ! Create the ctl 

     WRITE(*,*)'unit95: ',TRIM(dirout)//TRIM(lstoutctl(nbs))
     OPEN(95,FILE=TRIM(dirout)//TRIM(lstoutctl(nbs)),STATUS='UNKNOWN',FORM='FORMATTED') 

     CALL createctl(imax,jmax,glat,freqcalc,fctdate(nbs),lstoutgrb(nbs),resol,undef) 

     CLOSE(95)

     ! Open the binary file for writing out the spread informations
     
     WRITE(*,*)'unit50: ',TRIM(dirout)//TRIM(lstoutgrb(nbs))
     OPEN(50,FILE=TRIM(dirout)//TRIM(lstoutgrb(nbs)),STATUS='UNKNOWN', &
             FORM='UNFORMATTED',ACCESS='SEQUENTIAL')

     ! Read forecast data to evaluate the spread

     field(:,:,:,:)=0.0
     DO nmb=1,nmembers 
        INQUIRE(FILE=TRIM(dirinp)//TRIM(lstingrb(nmb,nbs)),EXIST=exarq)

        IF (exarq) THEN
           WRITE(*,*)'unit70: ',TRIM(dirinp)//TRIM(lstingrb(nmb,nbs))
           CALL BAOPEN(70,TRIM(dirinp)//TRIM(lstingrb(nmb,nbs)),ierr)
           IF (ierr .NE. 0) THEN
             WRITE(*,*) 'STOP BAOPEN TROUBLE  File: 70',TRIM(dirinp)//TRIM(lstingrb(nmb,nbs)),'  ierr:',ierr
             STOP
           END IF 


	   DO nr=1,nrecs
              jpds(5)=jpds5(nr)
              jpds(6)=jpds6(nr)
              jpds(7)=jpds7(nr)

              CALL GETGB  (70    ,0    ,nxny ,0   ,jpds,jgds,nwords,knum  ,kpds,kgds,bitmap,tmp,ierr)
              IF (ierr .NE. 0 .AND. ierr .NE. 96  .AND. ierr .NE. 97  .AND. ierr .NE. 98 .AND. ierr .NE. 99) THEN
                WRITE(*,*) 'STOP GETGB TROUBLE  File: 70',TRIM(dirinp)//TRIM(lstingrb(nmb,nbs)),'  ierr:',ierr
                STOP
              ELSE
                DO j=1,jmax
                DO i=1,imax
                   k=(j-1)*imax+i
                   var(i,j)=tmp(k)	  
                END DO
                END DO 
              END IF 

              IF (iwest .LE. ieast) THEN
                field(1:nxpts,1:nypts,nmb,nr)=var(iwest:ieast,jnort:jsout)
              ELSE
                 ni=0
                 DO i=iwest,imax		  ! "field" receive the west part of data
                    ni=ni+1
                    nj=0
                    DO j=jnort,jsout
                       nj=nj+1
                       field(ni,nj,nmb,nr)=var(i,j)
                    END DO
                 END DO
                 DO i=1,ieast			  ! "field" receive the east part of data
                     ni=ni+1
                     nj=0
                     DO j=jnort,jsout
                	nj=nj+1
                	field(ni,nj,nmb,nr)=var(i,j)
                     END DO
                 END DO
                 WRITE(*,*) 'ni= ',ni
                 WRITE(*,*) 'nj= ',nj
              END IF
           END DO
           CALL BACLOSE(70,ierr)
           IF (ierr .NE. 0) THEN
             WRITE(*,*) 'STOP BACLOSE TROUBLE  File: 70',TRIM(dirinp)//TRIM(lstingrb(nmb,nbs)),'  ierr:',ierr
             STOP
           END IF 
        ELSE
           WRITE(*,*) 'THE FORECAST FILE DOES NOT EXIST: ',TRIM(lstingrb(nmb,nbs))
           STOP
        END IF
     END DO

     ! Average of the members
  
     DO nr=1,nrecs 
   
        cm(1:nxpts,1:nypts)=0.0
        DO nmb=1, nmembers
    	   cm(1:nxpts,1:nypts)=cm(1:nxpts,1:nypts)+field(1:nxpts,1:nypts,nmb,nr)
        END DO
        cm(1:nxpts,1:nypts)=cm(1:nxpts,1:nypts)/FLOAT(nmembers)
    
        ! Obtain the spread "sp"

        sp(1:nxpts,1:nypts)=0.0
        DO nmb=1, nmembers
           DO i=1, nxpts
              DO j=1, nypts
                 sp(i,j)=sp(i,j)+(field(i,j,nmb,nr) - cm(i,j))**2
              END DO
           END DO
        END DO
        sp(1:nxpts,1:nypts)=SQRT(sp(1:nxpts,1:nypts)/FLOAT(nmembers))

        ! Write on the binary spread output file 

        WRITE(50) sp
     END DO

     CLOSE(50)
 
     WRITE(79,'(A)')TRIM(dirout)//TRIM(lstoutctl(nbs))
     WRITE(79,'(A)')TRIM(dirout)//TRIM(lstoutgrb(nbs))
  
  END DO  !--------End Main Loop 
  
  CLOSE(79)
   
  END PROGRAM spread


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to obtain the respective points of the selected region
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

SUBROUTINE getpoints(glat,lonw,lone,lats,latn,imax,jmax,iwest,ieast,jsout,jnort)

  IMPLICIT NONE

  INTEGER,               INTENT(IN)  :: imax, jmax
  REAL,                  INTENT(IN)  :: lonw, lone, lats, latn
  REAL, DIMENSION(jmax), INTENT(IN)  :: glat

  INTEGER,               INTENT(OUT) :: iwest, ieast, jsout, jnort
  
  INTEGER  :: i, j
  REAL     :: lonaux, difaux, dlon, difminn, difmins, difmine, difminw
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

     dlon=360.0/FLOAT(imax)
  
     iwest=0
     ieast=0
     lonaux=0.0
     difminw=1e37
     difmine=1e37
     DO i=1,imax
       lonaux=lonaux+dlon
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
     END DO

     WRITE(*,*) ' '   
     WRITE(*,*)'difminw=',difminw,' iwest=',iwest
     WRITE(*,*)'difmine=',difmine,' ieast=',ieast
     
  
     jnort=0
     jsout=0
     difminn=1e37
     difmins=1e37
     DO j=jmax,1,-1
       difaux=glat(j)-latn
       IF (ABS(difaux) .LT. difminn ) THEN
          difminn=ABS(difaux)
          jnort=j
       END IF
       difaux=glat(j)-lats
       IF (ABS(difaux) .LT. difmins ) THEN
          difmins=ABS(difaux)
          jsout=j
       END IF
     END DO

     WRITE(*,*)'difmins=',difmins,' jsout=',jsout
     WRITE(*,*)'difminn=',difminn,' jnort=',jnort

  RETURN

END SUBROUTINE getpoints


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to set up basic variables
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE attribute(unit,undef,imax,jmax,lonw,lone,lats,latn,  &
                       nmembers,nfctdy,freqcalc,                  &
		       dirinp,dirout,resol,prefx) 

  IMPLICIT NONE

  INTEGER,           INTENT(IN)   :: unit
  INTEGER,           INTENT(OUT)  :: imax,jmax,nmembers,nfctdy,freqcalc
  REAL,              INTENT(OUT)  :: lonw,lone,lats,latn,undef
  CHARACTER (LEN=*), INTENT(OUT)  :: dirinp,dirout,resol,prefx

  CHARACTER (LEN=200) :: line,line2
  INTEGER             :: ierro


  ierro=0      ! To start the loop

  DO 
      READ(unit,'(A14,A)', IOSTAT=ierro) line,line2
      IF( ierro .NE. 0 ) THEN
   	   EXIT
      ELSE
                IF( line(1:10).EQ.'UNDEF     ' ) THEN
               READ(line2(1:10),'(1PE10.3)')undef
           ELSE IF( line(1:10).EQ.'IMAX      ' ) THEN
   		 READ(line2(1:4),'(I4)')  imax
   	   ELSE IF( line(1:10).EQ.'JMAX      ' ) THEN
   		 READ(line2(1:4),'(I4)')  jmax
   	   ELSE IF( line(1:10).EQ.'LONW      ' ) THEN 
   		 READ(line2(1:10),'(F10.2)')  lonw
   	   ELSE IF( line(1:10).EQ.'LONE      ' ) THEN 
   		 READ(line2(1:10),'(F10.2)')  lone
   	   ELSE IF( line(1:10).EQ.'LATS      ' ) THEN 
   		 READ(line2(1:10),'(F10.2)')  lats
   	   ELSE IF( line(1:10).EQ.'LATN      ' ) THEN 
   		 READ(line2(1:10),'(F10.2)')  latn
   	   ELSE IF( line(1:10).EQ.'NMEMBERS  ' ) THEN 
   		 READ(line2(1:4),'(I4)')  nmembers
   	   ELSE IF( line(1:10).EQ.'NFCTDY    ' ) THEN 
   		 READ(line2(1:4),'(I4)')  nfctdy
   	   ELSE IF( line(1:10).EQ.'FREQCALC  ' ) THEN 
   		 READ(line2(1:4),'(I4)')  freqcalc
   	   ELSE IF( line(1:10).EQ.'DATALSTDIR' ) THEN
   		 dirinp=line2
   	   ELSE IF( line(1:10).EQ.'DATAOUTDIR' ) THEN 
   		 dirout=line2
   	   ELSE IF( line(1:10).EQ.'RESOL     ' ) THEN 
   		 resol=line2
           ELSE IF( line(1:10).EQ.'PREFX     ' ) THEN 
                 prefx=line2
   	   END IF
      END IF
  ENDDO

  RETURN
  END SUBROUTINE attribute


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to create the ctl's for spread files
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE createctl(imax,jmax,glat,freqcalc,fctdate,lstoutgrb,resol,undef) 

  IMPLICIT NONE

  INTEGER,               INTENT(IN) :: imax,jmax,freqcalc
  REAL,                  INTENT(IN) :: undef
  REAL, DIMENSION(jmax), INTENT(IN) :: glat
  CHARACTER(LEN=*),      INTENT(IN) :: fctdate,lstoutgrb,resol


  REAL                             :: delx
  INTEGER                          :: j,iday,imonth,iyear,iutc
  CHARACTER(LEN= 3), DIMENSION(12) :: mon


  DATA mon/'JAN','FEB','MAR','APR','MAY','JUN',  &
           'JUL','AUG','SEP','OCT','NOV','DEC'/

  !informations to tdef
  READ(fctdate(1: 4),'(I4)') iyear    
  READ(fctdate(5: 6),'(I2)') imonth
  READ(fctdate(7: 8),'(I2)') iday
  READ(fctdate(9:10),'(I2)') iutc
  
  delx=360.0/float(imax) ! Increment in zonal direction 
  
  WRITE(95,'(A)')'DSET ^'//TRIM(lstoutgrb)
  WRITE(95,'(A)')'*'
  WRITE(95,'(A)')'OPTIONS SEQUENTIAL BIG_ENDIAN YREV'
  WRITE(95,'(A)')'*'
  WRITE(95,'(A,1PE10.3)')'UNDEF ',undef 
  WRITE(95,'(A)')'*'
  WRITE(95,'(3A)')'TITLE CPTEC ENS SPREAD FROM AGCM v3.0 1999 ',TRIM(resol),'  COLD'     
  WRITE(95,'(A)')'*'
  WRITE(95,'(A,I5,A,F8.6,3X,F8.6)')'XDEF  ',imax,'  LINEAR    ',0.0,delx
  WRITE(95,'(A,I5,A)')'YDEF  ',jmax,'  LEVELS' 
  WRITE(95,'(8F10.5)') (glat(j),j=jmax,1,-1)
  WRITE(95,'(A)')'ZDEF   3 LEVELS 850 500 250'
  WRITE(95,'(A,I2.2,A,I2.2,A,I4.4,2X,I2.2,A)') 'TDEF   1 LINEAR ',iutc,'Z',iday,  &
                                                mon(imonth),iyear,freqcalc,'HR'
  WRITE(95,'(A)')'*'
  WRITE(95,'(A)')'VARS  7'
  WRITE(95,'(A)')'SUVEL   3  99 SPREAD DO CAMPO VENTO ZONAL             (m/s  )'
  
  WRITE(95,'(A)')'SVVEL   3  99 SPREAD DO CAMPO VENTO MERIDIONAL        (m/s  )' 
  
  WRITE(95,'(A)')'SFCOR   3  99 SPREAD DO CAMPO FUNCAO DE CORRENTE      (m^2/s)' 
  
  WRITE(95,'(A)')'SPOTV   3  99 SPREAD DO CAMPO POTENCIAL DE VELOCIDADE (m^2/s)' 
  
  WRITE(95,'(A)')'SZGEO   3  99 SPREAD DO CAMPO ALTURA GEOPOTENCIAL     (m    )' 
  WRITE(95,'(A)')'SPSNM   0  99 SPREAD DO CAMPO PRESSAO N. M. DO MAR    (hPa  )' 
  WRITE(95,'(A)')'STEMP   3  99 SPREAD DO CAMPO TEMPERATURA ABSOLUTA    (K    )'
  WRITE(95,'(A)')'ENDVARS'
  
  END SUBROUTINE createctl
  
  
 
   
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
  
  CHARACTER(len=10)   :: datep 

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

     WRITE(*,'(A,I,2A)') 'fctdate(',i,'):',fctdate(i)(1:10)
     
  END DO
  
  RETURN
  END SUBROUTINE caldate
 
  
    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to generate the list of forecast files (gribs)
! Generate the name of the output clusters list
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  
  SUBROUTINE getlstgrb(anldate,fctdate,ngrbs,nmembers,prefx,resol, &
                       lstingrb,lstoutctl,lstoutgrb,lstoutlst)
  
  IMPLICIT NONE
  
  INTEGER,                             INTENT(IN)  :: nmembers,ngrbs
  CHARACTER(len=10),                   INTENT(IN)  :: anldate
  CHARACTER(len=10), DIMENSION(ngrbs), INTENT(IN)  :: fctdate
  CHARACTER(LEN=*),                    INTENT(IN)  :: prefx,resol                

  CHARACTER(len=*),                             INTENT(OUT)  :: lstoutlst
  CHARACTER(len=*),  DIMENSION(ngrbs),          INTENT(OUT)  :: lstoutctl,lstoutgrb
  CHARACTER(len=*),  DIMENSION(nmembers,ngrbs), INTENT(OUT)  :: lstingrb

!+++++Local variables

  INTEGER                :: k,nf,membm


  ! fct files

  membm=(nmembers-1)/2

  DO nf=1,ngrbs

!     WRITE(*,*)' '

     DO k=1,membm
        IF (nf .EQ. 1) THEN
           WRITE(lstingrb(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',k,',',nf,')=',TRIM(lstfct(k,nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstingrb(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',k,',',nf,')=',TRIM(lstfct(k,nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstingrb(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',k,',',nf,')=',TRIM(lstfct(k,nf))
        END IF
     END DO

     DO k=1,membm
        IF (nf .EQ. 1) THEN
           WRITE(lstingrb(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',k+membm,',',nf,')=',TRIM(lstfct(k+membm,nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstingrb(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',k+membm,',',nf,')=',TRIM(lstfct(k+membm,nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstingrb(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',k+membm,',',nf,')=',TRIM(lstfct(k+membm,nf))
        END IF
     END DO

        IF (nf .EQ. 1) THEN
           WRITE(lstingrb(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',nmembers,',',nf,')=',TRIM(lstfct(nmembers,nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstingrb(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',nmembers,',',nf,')=',TRIM(lstfct(nmembers,nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstingrb(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',nmembers,',',nf,')=',TRIM(lstfct(nmembers,nf))
        END IF

!     WRITE(loutctl(n),'(5A)')'prob',lstctl(nmembers,n)(8:27),'.',TRIM(resol),'.ctl'
        IF (nf .EQ. 1) THEN
           WRITE(lstoutctl(nf),'(6A)')'spread',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol),'.ctl'
!           WRITE(*,*)'lstoutctl(',nf,')=',TRIM(lstoutctl(nf))
           WRITE(lstoutgrb(nf),'(6A)')'spread',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol),'.bin'
!           WRITE(*,*)'lstoutgrb(',nf,')=',TRIM(lstoutgrb(nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstoutctl(nf),'(6A)')'spread',anldate(1:10),fctdate(nf)(1:10),'.inz.',TRIM(resol),'.ctl'
!           WRITE(*,*)'lstoutctl(',nf,')=',TRIM(lstoutctl(nf))
           WRITE(lstoutgrb(nf),'(6A)')'spread',anldate(1:10),fctdate(nf)(1:10),'.inz.',TRIM(resol),'.bin'
!           WRITE(*,*)'lstoutgrb(',nf,')=',TRIM(lstoutgrb(nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstoutctl(nf),'(6A)')'spread',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol),'.ctl'
!           WRITE(*,*)'lstoutctl(',nf,')=',TRIM(lstoutctl(nf))
           WRITE(lstoutgrb(nf),'(6A)')'spread',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol),'.bin'
!           WRITE(*,*)'lstoutgrb(',nf,')=',TRIM(lstoutgrb(nf))
        END IF

  END DO
           WRITE(lstoutlst,'(6A)')'spread',anldate(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol),'.lst'
!           WRITE(*,*)'lstoutlst=',TRIM(lstoutlst)

  RETURN
  END SUBROUTINE getlstgrb 
  

