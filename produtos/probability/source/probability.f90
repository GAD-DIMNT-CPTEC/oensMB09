  PROGRAM probability

  USE GaussRep,    ONLY : CreateGaussRep, glat
  USE ReadFields,  ONLY : ReadGrib

  IMPLICIT NONE

  !
  ! Program to evaluate the probabilities from the global ensemble 
  ! weather forecasting
  ! Author: Antonio Marcos Mendonca 
  ! Any comentary, send a e-mail to: mendonca@cptec.inpe.br
  ! Last Atualization: Oct/2007
  !

  !
  ! Variables
  !

  INTEGER :: i,j,l,nbs,nmb,nout
  INTEGER :: imax,jmax,jmaxhf
  INTEGER :: nfctdy,nmembers,noutpday,ngrbs,freqcalc
  INTEGER :: ierr,statfctrd,rest
  INTEGER, ALLOCATABLE, DIMENSION(:)     :: fcthours

  REAL               :: undef       ! Undef value set up according to original grib files
  REAL, DIMENSION(4) :: tshdprc

  REAL, ALLOCATABLE, DIMENSION(:,:)      :: field          ! field of forecasted precipitation 
  REAL, ALLOCATABLE, DIMENSION(:,:)      :: prob           ! probability evaluated 
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: datap          ! forecasted precipitation for each member and lead time
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: datapaccum     ! 24h accumulated precipitation for each member and lead time

  CHARACTER(LEN=10)  :: datei                 ! initial date
  CHARACTER(LEN=200) :: dirinp                ! input directory where is the files of the ensemble members        
  CHARACTER(LEN=200) :: dirout                ! output directory for probabilities
  CHARACTER(LEN=200) :: probsetup             ! namelist file
  CHARACTER(LEN=200) :: resol                 ! horizontal and vertical model resolution
  CHARACTER(LEN=200) :: lstoutlst             ! list of outputs probabilities forecasting
  CHARACTER(LEN=200) :: prefx                 ! preffix for input and output files
  CHARACTER(LEN=10),  ALLOCATABLE, DIMENSION(:)   :: fctdate
  CHARACTER(LEN=200), ALLOCATABLE, DIMENSION(:)   :: lstoutctl,lstoutgrb
  CHARACTER(LEN=200), ALLOCATABLE, DIMENSION(:,:) :: lstingrb        ! name of input ctl's and bin's

  LOGICAL :: exarq

  !
  ! Block Data
  !

  DATA tshdprc /1.0,5.0,10.0,20.0/   ! Thresholds for precipitation probabilities

  !
  ! Attributions
  !

  CALL GETARG(1,datei)
  WRITE(probsetup,'(3A)')'probsetup.',datei,'.nml'

  !
  ! Call attribution function
  !

  OPEN(100,FILE=TRIM(probsetup),STATUS='OLD',FORM='FORMATTED')

  CALL attribute(undef,imax,jmax,nmembers,nfctdy,freqcalc,dirinp,dirout,resol,prefx) 
  jmaxhf=jmax/2

  CLOSE(100)

  WRITE(*,*)'imax    : ',imax
  WRITE(*,*)'jmax    : ',jmax
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
  
  !
  !  Make dynamic allocation of memory 
  !

  ALLOCATE(fcthours(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fcthours'
  ALLOCATE(fctdate(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fctdate'
  ALLOCATE(lstingrb(nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstoutgrb'
  ALLOCATE(lstoutctl(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstoutctl'
  ALLOCATE(lstoutgrb(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstoutgrb'
  ALLOCATE(prob(imax,jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: prob'
  ALLOCATE(field(imax,jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: field'
  ALLOCATE(datap(imax,jmax,nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: datap'
  ALLOCATE(datapaccum(imax,jmax,nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: datapaccum'
 

  CALL CreateGaussRep(jmax,jmaxhf) ! Obtain gaussian latitudes
  
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
  !  Read precipitation forecast data and accumulate the precipitation
  !

  WRITE(*,*)' '
  datap(:,:,:,:)=0.0
  DO nbs=1,ngrbs
     IF (fcthours(nbs) .GT. 0) THEN
        DO nmb=1,nmembers 
           INQUIRE(FILE=TRIM(dirinp)//TRIM(lstingrb(nmb,nbs)),EXIST=exarq)
           IF (exarq) THEN
              WRITE(*,*)'unit70: ',TRIM(lstingrb(nmb,nbs))
              statfctrd=0
    	      CALL ReadGrib(TRIM(dirinp)//TRIM(lstingrb(nmb,nbs)),imax,jmax,field,statfctrd)
!CFB
!              IF (statfctrd .NE. 0) STOP 'Main Program: It does not read grib correctly'
!CFB
           ELSE
	      WRITE(*,*) 'THE FORECAST FILE DOES NOT EXIST: ',TRIM(lstingrb(nmb,nbs))
	      STOP
           END IF
           datap(1:imax,1:jmax,nmb,nbs)=field(1:imax,1:jmax)
        END DO
     END IF
  END DO


  !
  ! Evaluate the 24h-accumulated precipitation
  !

  datapaccum(:,:,:,:)=0.0
  DO nbs=1,ngrbs
     IF (fcthours(nbs) .GE. 24) THEN
        DO nmb=1,nmembers 
           DO nout=nbs-noutpday+1,nbs 
              datapaccum(:,:,nmb,nbs)=datapaccum(:,:,nmb,nbs)+datap(:,:,nmb,nout)
           END DO
           datapaccum(:,:,nmb,nbs)=datapaccum(:,:,nmb,nbs)/FLOAT(noutpday)
        END DO
     END IF
  END DO

  !
  !  Evaluate the probabilities and write in the output binary files
  !

  WRITE(*,*)' '
  WRITE(*,*)'size tshdprc: ',SIZE(tshdprc)
  OPEN(79,FILE=TRIM(dirout)//TRIM(lstoutlst),FORM='FORMATTED',STATUS='UNKNOWN')

  DO nbs=1,ngrbs

     IF (fcthours(nbs) .GE. 24) THEN

        WRITE(*,*)'unit95: ',TRIM(dirout)//TRIM(lstoutctl(nbs))
        OPEN(95,FILE=TRIM(dirout)//TRIM(lstoutctl(nbs)),STATUS='UNKNOWN',FORM='FORMATTED') 
     	CALL CreateCtl(imax,jmax,glat,freqcalc,fctdate(nbs),lstoutgrb(nbs),resol,undef) 
        CLOSE(95)

     	WRITE(*,*)'unit80: ',TRIM(dirout)//TRIM(lstoutgrb(nbs))
     	OPEN(80,FILE=TRIM(dirout)//TRIM(lstoutgrb(nbs)),FORM='UNFORMATTED',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')

     	DO l=1,SIZE(tshdprc)

     	   WRITE(*,*)'tshdprc(',l,'): ',tshdprc(l)
     	   prob(:,:)=0.0
     	   DO nmb=1,nmembers
     	      DO i=1,imax
     	         DO j=1,jmax
     	   	    IF ( (datapaccum(i,j,nmb,nbs) .NE. undef) .AND. (prob(i,j) .NE. undef) ) THEN
     	   	       IF ( datapaccum(i,j,nmb,nbs) .GE. tshdprc(l) ) THEN
     	   	    	  prob(i,j)=prob(i,j)+1.0
     	   	       END IF
     	   	    ELSE
     	   	       prob(i,j)=undef
     	   	    END IF
     	         END DO
     	      END DO
     	   END DO

     	   DO i=1,imax
     	      DO j=1,jmax
     	   	 IF ( prob(i,j) .NE. undef ) THEN
     	   	    prob(i,j)=prob(i,j)/FLOAT(nmembers)
     	   	 END IF
     	      END DO
     	   END DO
     	   WRITE(80) prob

     	END DO
     	CLOSE(80)

     	WRITE(79,'(A)')TRIM(dirout)//TRIM(lstoutctl(nbs))
     	WRITE(79,'(A)')TRIM(dirout)//TRIM(lstoutgrb(nbs))
 
     END IF
 
  END DO
  CLOSE(79)
    

  END PROGRAM probability



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to set up basic variables
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE attribute(undef,imax,jmax,nmembers,nfctdy,freqcalc,dirinp,dirout,resol,prefx) 

  IMPLICIT NONE

  INTEGER,           INTENT(OUT)  :: imax,jmax,nmembers,nfctdy,freqcalc
  REAL,              INTENT(OUT)  :: undef
  CHARACTER (LEN=*), INTENT(OUT)  :: dirinp,dirout,resol,prefx

  CHARACTER (LEN=200) :: line,line2
  INTEGER             :: ierr


  ierr=0      ! To start the loop

  DO WHILE (ierr .EQ. 0)

         READ(100,'(A14,A)', IOSTAT=ierr) line,line2
              IF( line(1:10).EQ.'UNDEF     ' ) THEN
               READ(line2(1:10),'(1PE10.3)')undef
         ELSE IF( line(1:10).EQ.'IMAX      ' ) THEN
               READ(line2(1:4),'(I4)')  imax
         ELSE IF( line(1:10).EQ.'JMAX      ' ) THEN
               READ(line2(1:4),'(I4)')  jmax
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
! Subroutine to create the ctl's for probability files
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE CreateCtl(imax,jmax,glat,freqcalc,fctdate,loutbin,resol,undef) 

  IMPLICIT NONE

  INTEGER,               INTENT(IN) :: imax,jmax,freqcalc
  REAL,                  INTENT(IN) :: undef
  REAL, DIMENSION(jmax), INTENT(IN) :: glat
  CHARACTER(LEN=*),      INTENT(IN) :: fctdate,loutbin,resol


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
  
  
  WRITE(95,'(A)')'DSET ^'//TRIM(loutbin)
  WRITE(95,'(A)')'*'
  WRITE(95,'(A)')'OPTIONS SEQUENTIAL BIG_ENDIAN YREV'
  WRITE(95,'(A)')'*'
  WRITE(95,'(A,1PE10.3)')'UNDEF ',undef 
  WRITE(95,'(A)')'*'
  WRITE(95,'(3A)')'TITLE PROBABILITIES FROM ENS CPTEC AGCM v3.0 1999 ',TRIM(resol),'  COLD'     
  WRITE(95,'(A)')'*'
  WRITE(95,'(A,I5,A,F8.6,3X,F8.6)')'XDEF  ',imax,'  LINEAR    ',0.0,delx
  WRITE(95,'(A,I5,A)')'YDEF  ',jmax,'  LEVELS' 
  WRITE(95,'(8F10.5)') (glat(j),j=jmax,1,-1)
  WRITE(95,'(A)')'ZDEF   1 LEVELS 1000'
  WRITE(95,'(A,I2.2,A,I2.2,A,I4.4,2X,I2.2,A)') 'TDEF   1 LINEAR ',iutc,'Z',iday,  &
                                                mon(imonth),iyear,freqcalc,'HR'
  WRITE(95,'(A)')'*'
  WRITE(95,'(A)')'VARS  4'
  WRITE(95,'(A)')'PROB1   0 99 PROB. OF 24HR ACCUMULATED PRECIPITATION >  1.0 mm'
  WRITE(95,'(A)')'PROB5   0 99 PROB. OF 24HR ACCUMULATED PRECIPITATION >  5.0 mm' 
  WRITE(95,'(A)')'PROB10  0 99 PROB. OF 24HR ACCUMULATED PRECIPITATION > 10.0 mm' 
  WRITE(95,'(A)')'PROB20  0 99 PROB. OF 24HR ACCUMULATED PRECIPITATION > 20.0 mm' 
  WRITE(95,'(A)')'ENDVARS'
  
  CLOSE(95)
  
  END SUBROUTINE CreateCtl
  
  
 
   
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
           WRITE(lstoutctl(nf),'(6A)')'prob',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol),'.ctl'
!           WRITE(*,*)'lstoutctl(',nf,')=',TRIM(lstoutctl(nf))
           WRITE(lstoutgrb(nf),'(6A)')'prob',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol),'.bin'
!           WRITE(*,*)'lstoutgrb(',nf,')=',TRIM(lstoutgrb(nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstoutctl(nf),'(6A)')'prob',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol),'.ctl'
!           WRITE(*,*)'lstoutctl(',nf,')=',TRIM(lstoutctl(nf))
           WRITE(lstoutgrb(nf),'(6A)')'prob',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol),'.bin'
!           WRITE(*,*)'lstoutgrb(',nf,')=',TRIM(lstoutgrb(nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstoutctl(nf),'(6A)')'prob',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol),'.ctl'
!           WRITE(*,*)'lstoutctl(',nf,')=',TRIM(lstoutctl(nf))
           WRITE(lstoutgrb(nf),'(6A)')'prob',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol),'.bin'
!           WRITE(*,*)'lstoutgrb(',nf,')=',TRIM(lstoutgrb(nf))
        END IF

  END DO
           WRITE(lstoutlst,'(6A)')'prob',anldate(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol),'.lst'
!           WRITE(*,*)'lstoutlst=',TRIM(lstoutlst)

  RETURN
  END SUBROUTINE getlstgrb 
  
  

