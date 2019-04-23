  PROGRAM prcmed

  USE GaussRep,  ONLY : CreateGaussRep, glat
  USE ReadFields,ONLY : ReadGrib

  IMPLICIT NONE

  ! Evaluate the week mean precipitation from the ensemble mean
  ! Author: Antonio Marcos Mendonca 
  ! Any comentary, send a e-mail to: marcos.mendonca@cptec.inpe.br
  ! Last Atualization: Dec/2008
    
  !
  ! Variables
  !

  INTEGER :: imax,jmax,jmaxhf
  INTEGER :: ierr,nbs,nwk,nhrpwk,ngrbs,rest,noutpday,statfctrd 
  INTEGER :: nfctdy,freqcalc,nweek

  REAL :: undef

  CHARACTER(LEN=10)  :: datei	    ! initial date
  CHARACTER(LEN=200) :: dirin	    ! dir of ensemble mean input data
  CHARACTER(LEN=200) :: dirout      ! dir of precipitation mean output data
  CHARACTER(LEN=200) :: prcmedi     ! namelist file
  CHARACTER(LEN=200) :: resol	    ! horizontal and vertical model resolution
  CHARACTER(LEN=200) :: prefx	    ! preffix for input and output files
  CHARACTER(LEN=200) :: lstoutlst

  LOGICAL :: exarq

  INTEGER,ALLOCATABLE,DIMENSION(:)   :: fcthours

  REAL,   ALLOCATABLE,DIMENSION(:,:) :: prec,temp,datapaccum,datataccum

  CHARACTER(LEN=10), ALLOCATABLE,DIMENSION(:) :: fctdate
  CHARACTER(LEN=200),ALLOCATABLE,DIMENSION(:) :: lstingrb
  CHARACTER(LEN=200),ALLOCATABLE,DIMENSION(:) :: lstoutctl,lstoutgrb

  !
  ! Atributions
  !

  CALL GETARG(1,datei)
  WRITE(*,*)'datei: ',datei

  WRITE(prcmedi,'(A7,A10,A4)')'prcmed.',datei,'.nml'
  WRITE(*,*)'prcmedi: ',TRIM(prcmedi)

  !
  ! Call attribution function
  !

  OPEN(100,FILE=TRIM(prcmedi),STATUS='OLD',FORM='FORMATTED')

  CALL attribute(100,undef,imax,jmax,nfctdy,freqcalc,nweek,dirin,dirout,prefx,resol) 
  jmaxhf=jmax/2

  CLOSE(100)

  WRITE(*,*)'undef   : ',undef
  WRITE(*,*)'imax    : ',imax
  WRITE(*,*)'jmax    : ',jmax
  WRITE(*,*)'nfctdy  : ',nfctdy
  WRITE(*,*)'freqcalc: ',freqcalc
  WRITE(*,*)'nweek   : ',nweek
  WRITE(*,*)'dirin   : ',TRIM(dirin)
  WRITE(*,*)'dirout  : ',TRIM(dirout)
  WRITE(*,*)'prefx   : ',TRIM(prefx)
  WRITE(*,*)'resol   : ',TRIM(resol)


  IF (freqcalc .GT. 24) STOP 'Main Program: freqcalc must be .LE. 24'
  rest=MOD(24,freqcalc)
  IF (rest .NE. 0) THEN
    STOP 'Main Program: freqcalc must be a divisor of 24. Ex.: 6, 12 and 24'
  ELSE
    noutpday=24/freqcalc
  END IF

  ngrbs=nfctdy*noutpday+2 !number of gribs icn + inz + fct's
 
  !
  ! Dynamic allocations
  !

  ALLOCATE(fctdate(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fctdate'
  ALLOCATE(fcthours(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fcthours'
  ALLOCATE(lstingrb(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstingrb'
  ALLOCATE(lstoutgrb(nweek), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstoutgrb'
  ALLOCATE(lstoutctl(nweek), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstoutctl'
  ALLOCATE(prec(imax,jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: prec'
  ALLOCATE(temp(imax,jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: temp'
  ALLOCATE(datapaccum(imax,jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: datapaccum'
  ALLOCATE(datataccum(imax,jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: datataccum'


  CALL CreateGaussRep(jmax,jmaxhf) ! Obtain gaussian latitudes


  !
  ! Generate the list of forecast output dates and hours
  !

  CALL caldate(datei,ngrbs,freqcalc,fctdate,fcthours)


  !
  ! Generate the list of forecast files
  ! Generate the name of the output files
  !

  CALL getlstgrb(datei,fctdate,fcthours,ngrbs,nweek,prefx,resol, &
	  	 lstingrb,lstoutctl,lstoutgrb,lstoutlst)

  !
  ! Evaluate the week mean of forecast precipitation and generate the output files
  !

  !
  ! Generate the lst output files
  !
   
  WRITE(*,*)' '
  WRITE(*,*)'lstoutlst: ',TRIM(dirout)//TRIM(lstoutlst)
  OPEN(50,FILE=TRIM(dirout)//TRIM(lstoutlst),STATUS='UNKNOWN',&
          FORM='FORMATTED')

  WRITE(*,*)' '

  nwk=1
  nhrpwk=nwk*7*24      !nwk=week, 7=nb of days on the week, 24=nb of hours in a day
  datapaccum(:,:)=0.0
  datataccum(:,:)=0.0

  DO nbs=1,ngrbs
     IF (fcthours(nbs) .GT. 0) THEN
	 INQUIRE(FILE=TRIM(dirin)//TRIM(lstingrb(nbs)),EXIST=exarq)
	 IF (exarq) THEN
	    WRITE(*,*)'unit70: ',TRIM(lstingrb(nbs))
	    statfctrd=0
	    CALL ReadGrib(TRIM(dirin)//TRIM(lstingrb(nbs)),imax,jmax,prec,temp,statfctrd)
	    IF (statfctrd .NE. 0) STOP 'Main Program: It does not read grib correctly'
	 ELSE
	    WRITE(*,*) 'THE FORECAST FILE DOES NOT EXIST: ',TRIM(lstingrb(nbs))
	    STOP
	 END IF

	 datapaccum(1:imax,1:jmax)=datapaccum(1:imax,1:jmax)+prec(1:imax,1:jmax)
	 datataccum(1:imax,1:jmax)=datataccum(1:imax,1:jmax)+temp(1:imax,1:jmax)
	
	 IF (fcthours(nbs) .EQ. nhrpwk) THEN

	    WRITE(*,*)'unit95: ',TRIM(dirout)//TRIM(lstoutctl(nwk))
	    OPEN(95,FILE=TRIM(dirout)//TRIM(lstoutctl(nwk)),STATUS='UNKNOWN',FORM='FORMATTED') 
	    CALL CreateCtl(95,fctdate(nbs),imax,jmax,glat,lstoutgrb(nwk),resol,undef)
            CLOSE(95) 

	    WRITE(*,*)'FILE80: ',TRIM(dirout)//TRIM(lstoutgrb(nwk))
	    OPEN(80,FILE=TRIM(dirout)//TRIM(lstoutgrb(nwk)),STATUS='UNKNOWN', &
        	    FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
	    datapaccum(1:imax,1:jmax)=datapaccum(1:imax,1:jmax)/(7*noutpday) !7=nb of days on the week, noutpday=nb of outputs per day
	    datataccum(1:imax,1:jmax)=datataccum(1:imax,1:jmax)/(7*noutpday) !7=nb of days on the week, noutpday=nb of outputs per day
	    WRITE(80) datapaccum
	    WRITE(80) datataccum
	    CLOSE(80)

	    WRITE(50,'(A)')TRIM(dirout)//TRIM(lstoutctl(nwk))
	    WRITE(50,'(A)')TRIM(dirout)//TRIM(lstoutgrb(nwk))
	    
	    nwk=nwk+1
	    nhrpwk=nwk*7*24   !7=nb of days on the week, 24=nb of hours per day
	    datapaccum(:,:)=0.0
	    datataccum(:,:)=0.0

	    IF (nwk .GT. nweek) EXIT 

	 END IF
     END IF
  END DO

  CLOSE(50)

  END PROGRAM prcmed


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  !
  ! Subroutine to set up basic variables
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE attribute(unit,undef,imax,jmax,nfctdy,freqcalc,nweek,dirin,dirout,prefx,resol) 

  IMPLICIT NONE

  INTEGER,	     INTENT(IN)   :: unit

  INTEGER,	     INTENT(OUT)  :: imax,jmax,nfctdy,freqcalc,nweek
  REAL, 	     INTENT(OUT)  :: undef
  CHARACTER (LEN=*), INTENT(OUT)  :: dirin,dirout,prefx,resol

  CHARACTER (LEN=200) :: line,line2
  INTEGER	      :: ierr

  ierr=0       ! To start the loop

  DO WHILE ( ierr .EQ. 0 )

	 READ(unit,'(A14,A)', IOSTAT=ierr) line,line2
	      IF( line(1:10).EQ.'UNDEF     ' ) THEN
	       READ(line2(1:10),'(1PE10.3)')undef
	 ELSE IF( line(1:10).EQ.'IMAX      ' ) THEN
	       READ(line2(1:5),'(I5)')  imax
	 ELSE IF( line(1:10).EQ.'JMAX      ' ) THEN
	       READ(line2(1:5),'(I5)')  jmax
	 ELSE IF( line(1:10).EQ.'NFCTDY    ' ) THEN 
	       READ(line2(1:4),'(I4)')  nfctdy
	 ELSE IF( line(1:10).EQ.'FREQCALC  ' ) THEN 
	       READ(line2(1:4),'(I4)')  freqcalc
	 ELSE IF( line(1:10).EQ.'NWEEK     ' ) THEN 
	       READ(line2(1:4),'(I4)')  nweek
	 ELSE IF( line(1:10).EQ.'DATAINDIR ' ) THEN
	       dirin=line2
	 ELSE IF( line(1:10).EQ.'DATAOUTDIR' ) THEN 
	       dirout=line2
	 ELSE IF( line(1:10).EQ.'PREFX     ' ) THEN 
	       prefx=line2
	 ELSE IF( line(1:10).EQ.'RESOL     ' ) THEN 
	       resol=line2
	 END IF
  ENDDO

  RETURN
  END SUBROUTINE attribute



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  !
  ! Subroutine to create the ctl's for prec mean
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE CreateCtl(unit,fctdate,imax,jmax,glat,loutbin,resol,undef) 

  IMPLICIT NONE

  INTEGER,		 INTENT(IN) :: unit,imax,jmax
  REAL, 		 INTENT(IN) :: undef
  REAL, DIMENSION(jmax), INTENT(IN) :: glat
  CHARACTER(LEN=*),	 INTENT(IN) :: fctdate,loutbin,resol


  REAL :: delx
  INTEGER :: j,iday,imonth,iyear,iutc
  CHARACTER(LEN= 3), DIMENSION(12) :: mon


  DATA mon/'JAN','FEB','MAR','APR','MAY','JUN',  &
	   'JUL','AUG','SEP','OCT','NOV','DEC'/

  ! informations to tdef
  READ(fctdate(1: 4),'(I4)') iyear    
  READ(fctdate(5: 6),'(I2)') imonth
  READ(fctdate(7: 8),'(I2)') iday
  READ(fctdate(9:10),'(I2)') iutc

  delx=360.0/float(imax) ! Increment in zonal direction 

  WRITE(unit,'(A)')'DSET ^'//TRIM(loutbin)
  WRITE(unit,'(A)')'*'
  WRITE(unit,'(A)')'OPTIONS SEQUENTIAL BIG_ENDIAN YREV'
  WRITE(unit,'(A)')'*'
  WRITE(unit,'(A,1PE10.3)')'UNDEF ',undef 
  WRITE(unit,'(A)')'*'
  WRITE(unit,'(3A)')'TITLE WEEK PREC MEAN FROM ENS CPTEC AGCM v3.0 1999 ',TRIM(resol),'  COLD'         
  WRITE(unit,'(A)')'*'
  WRITE(unit,'(A,I5,A,F8.6,3X,F8.6)')'XDEF  ',imax,'  LINEAR	',0.0,delx
  WRITE(unit,'(A,I5,A)')'YDEF  ',jmax,'  LEVELS' 
  WRITE(unit,'(8F10.5)') (glat(j),j=jmax,1,-1)
  WRITE(unit,'(A)')'ZDEF   1 LEVELS 1000'
  WRITE(unit,'(A,I2.2,A,I2.2,A,I4.4,A)') 'TDEF       1 LINEAR ',iutc,'Z',iday,  &
					  mon(imonth),iyear,' 06HR'
  WRITE(unit,'(A)')'*'
  WRITE(unit,'(A)')'VARS  2'
  WRITE(unit,'(A)')'PREC  0 99 WEEK MEAN OF TOTAL PRECIPITATION    (Kg M**-2 Day**-1)'
  WRITE(unit,'(A)')'TSMT  0 99 WEEK MEAN OF TIME MEAN SURFACE ABSOLUTE TEMPERATURE  (K)'
  WRITE(unit,'(A)')'ENDVARS'

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
  
  SUBROUTINE getlstgrb(anldate,fctdate,fcthours,ngrbs,nweek,prefx,resol, &
                       lstingrb,lstoutctl,lstoutgrb,lstoutlst)
  
  IMPLICIT NONE
  
  INTEGER,                   INTENT(IN)  :: nweek,ngrbs
  INTEGER, DIMENSION(ngrbs), INTENT(IN)  :: fcthours

  CHARACTER(len=10),                   INTENT(IN)  :: anldate
  CHARACTER(len=10), DIMENSION(ngrbs), INTENT(IN)  :: fctdate
  CHARACTER(LEN=*),                    INTENT(IN)  :: prefx,resol                

  CHARACTER(len=*),                    INTENT(OUT)  :: lstoutlst
  CHARACTER(len=*),  DIMENSION(nweek), INTENT(OUT)  :: lstoutctl,lstoutgrb
  CHARACTER(len=*),  DIMENSION(ngrbs), INTENT(OUT)  :: lstingrb

!+++++Local variables

  INTEGER                :: nf,nwk,nhrpwk


  ! fct files

  nwk=1
  nhrpwk=nwk*7*24 !7=nb of days on the week, 24=nb of hours per day
  DO nf=1,ngrbs

!     WRITE(*,*)' '

        IF (nf .EQ. 1) THEN
           WRITE(lstingrb(nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstingrb(',nf,')=',TRIM(lstingrb(nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstingrb(nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstingrb(',nf,')=',TRIM(lstingrb(nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstingrb(nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstingrb(',nf,')=',TRIM(lstingrb(nf))
        END IF

        IF (fcthours(nf) .EQ. nhrpwk) THEN
           WRITE(lstoutctl(nwk),'(7A)')'GPRC',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.ctl'
           WRITE(*,*)'lstoutctl(',nwk,')=',TRIM(lstoutctl(nwk))
           WRITE(lstoutgrb(nwk),'(6A)')'GPRC',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol)
           WRITE(*,*)'lstoutgrb(',nwk,')=',TRIM(lstoutgrb(nwk))
           nwk=nwk+1
           nhrpwk=nwk*7*24
        END IF

  END DO
           WRITE(lstoutlst,'(7A)')'GPRC',TRIM(prefx),anldate(1:10),fctdate(ngrbs)(1:10),'P.fct.',TRIM(resol),'.lst'
           WRITE(*,*)'lstoutlst=',TRIM(lstoutlst)

  RETURN
  END SUBROUTINE getlstgrb 

