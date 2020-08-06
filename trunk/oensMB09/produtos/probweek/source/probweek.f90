  PROGRAM probweek
  
  USE GaussRep,  ONLY : CreateGaussRep, glat
  USE ReadFields,ONLY : ReadGrib

  IMPLICIT NONE
  
  ! Evaluate the probabilities of weekly precipitation from ensemble members
  ! Author: Antonio Marcos Mendonca 
  ! Any comentary, send a e-mail to: marcos.mendonca@cptec.inpe.br
  ! Last Atualization: Sep/2008
    
  !
  ! Variables
  !
  
  INTEGER :: imax,jmax,jmaxhf
  INTEGER :: i,j,l,nbs,nmb,ierr,nwk,nhrpwk,rest,statfctrd
  INTEGER :: nmembers,nfctdy,ngrbs
  INTEGER :: freqcalc,nweek,ndacc,noutpday 

  REAL               :: undef
  REAL, DIMENSION(6) :: tshdprc

  CHARACTER(LEN=10)  :: anldate     ! initial date
  CHARACTER(LEN=200) :: dirin	    ! dir of ensemble input data
  CHARACTER(LEN=200) :: dirout      ! dir of precipitation mean output data
  CHARACTER(LEN=200) :: probweekf   ! namelist file
  CHARACTER(LEN=200) :: resol	    ! horizontal and vertical model resolution
  CHARACTER(LEN=200) :: prefx       ! preffix for input and output files
  CHARACTER(LEN=200) :: lstoutlst

  INTEGER,            DIMENSION(:),     ALLOCATABLE :: fcthours
  REAL,               DIMENSION(:,:),   ALLOCATABLE :: prec,prob
  REAL,               DIMENSION(:,:,:), ALLOCATABLE :: prcwkac
  CHARACTER(len=10),  DIMENSION(:),     ALLOCATABLE :: fctdate
  CHARACTER(LEN=200), DIMENSION(:),     ALLOCATABLE :: lstoutctl,lstoutgrb
  CHARACTER(LEN=200), DIMENSION(:,:),   ALLOCATABLE :: lstingrb
  
  !
  ! Block Data
  !

  DATA tshdprc /15.0,20.0,25.0,30.0,40.0,50.0/   ! Thresholds for precipitation probabilities

  !
  ! Atributions
  !
  
  CALL GETARG(1,anldate)
  WRITE(probweekf,'(3A)')'probweek.',anldate,'.nml'

  !
  ! Call attribution function
  !

  OPEN(100,FILE=TRIM(probweekf),STATUS='OLD',FORM='FORMATTED')

  CALL attribute(100,undef,imax,jmax,nmembers,nfctdy,freqcalc,nweek,ndacc, &
                 dirin,dirout,prefx,resol) 

  CLOSE(100)

  WRITE(*,*)'undef   : ',undef
  WRITE(*,*)'imax    : ',imax
  WRITE(*,*)'jmax    : ',jmax
  WRITE(*,*)'nmembers: ',nmembers
  WRITE(*,*)'nfctdy  : ',nfctdy
  WRITE(*,*)'freqcalc: ',freqcalc
  WRITE(*,*)'nweek   : ',nweek 
  WRITE(*,*)'ndacc   : ',ndacc 
  WRITE(*,*)'dirin   : ',TRIM(dirin)
  WRITE(*,*)'dirout  : ',TRIM(dirout)
  WRITE(*,*)'prefx   : ',TRIM(prefx)
  WRITE(*,*)'resol   : ',TRIM(resol)

  
  IF (freqcalc .GT. 24) STOP 'Main Program: freqcalc must be .LE. 24'
  rest=MOD(24,freqcalc)
  IF (rest .NE. 0) THEN
    STOP 'Main Program: freqcalc must be a divisor of 24. Ex.: 6, 12 or 24'
  ELSE
    noutpday=24/freqcalc
  END IF

  ngrbs=nfctdy*noutpday+2 !number of gribs icn + inz + fct's
 
  !
  ! Dynamic allocations
  !
  
  ALLOCATE(fcthours(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fcthours'
  ALLOCATE(fctdate(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fctdate'
  ALLOCATE(lstoutgrb(nweek), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstoutgrb'
  ALLOCATE(lstoutctl(nweek), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstoutctl'
  ALLOCATE(lstingrb(nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstingrb'
  ALLOCATE(prec(imax,jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: prec'
  ALLOCATE(prob(imax,jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: prob'
  ALLOCATE(prcwkac(imax,jmax,nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: prcwkac'

  !
  !  Obtain gaussian latitudes
  !
  
  jmaxhf=jmax/2
  CALL CreateGaussRep(jmax,jmaxhf) 

  !
  ! Generate the list of forecast output dates and hours
  !
  
  CALL caldate(anldate,ngrbs,freqcalc,fctdate,fcthours)

  !
  ! Generate the list of forecast files
  ! Generate the name of the output files
  !

  CALL getlstgrb(anldate,fctdate,fcthours,ngrbs,nmembers,nweek,ndacc,prefx,resol, &
                 lstingrb,lstoutctl,lstoutgrb,lstoutlst)

  !
  !  Week probability precipitation
  !

  WRITE(*,*)' '
  WRITE(*,*) 'unit79: ',TRIM(dirout)//TRIM(lstoutlst)
  OPEN(79,FILE=TRIM(dirout)//TRIM(lstoutlst),FORM='FORMATTED',STATUS='UNKNOWN')
  WRITE(*,*)' '

  IF (ndacc .LT. 1) STOP 'Main Program: ndacc must be equal or greater than 1'

  nwk=1
  nhrpwk=nwk*ndacc*24   !nwk=number of period (week,pentad,etc), 
  prcwkac(:,:,:)=0.0	!ndacc=number of days to accumulate prec, 24=nb of hours in a day 			        

  DO nbs=1,ngrbs
     IF (fcthours(nbs) .GT. 0) THEN

        !
        ! Evaluate the week accumulatted forecast precipitation for each ensemble member
        !

        DO nmb=1,nmembers
	   statfctrd=0
	   CALL ReadGrib(TRIM(dirin)//TRIM(lstingrb(nmb,nbs)),imax,jmax,prec,statfctrd)
	   IF (statfctrd .NE. 0) STOP 'Main Program: It does not read grib correctly'
           prcwkac(1:imax,1:jmax,nmb) = prcwkac(1:imax,1:jmax,nmb) + (prec(1:imax,1:jmax)/FLOAT(noutpday)) 
        ENDDO

     	IF (fcthours(nbs) .EQ. nhrpwk) THEN

     	   !
     	   !  Evaluate the probabilities and write in the output binary files
     	   !
    
     	   WRITE(*,*)' '

    	   WRITE(*,*)'unit95: ',TRIM(dirout)//TRIM(lstoutctl(nwk))
    	   OPEN(95,FILE=TRIM(dirout)//TRIM(lstoutctl(nwk)),STATUS='UNKNOWN',FORM='FORMATTED') 
     	   CALL CreateCtl(95,fctdate(nbs),imax,jmax,ndacc,glat,lstoutgrb(nwk),resol,undef) 
           CLOSE(95) 

     	   WRITE(*,*)'unit80: ',TRIM(dirout)//TRIM(lstoutgrb(nwk))
     	   OPEN(80,FILE=TRIM(dirout)//TRIM(lstoutgrb(nwk)),FORM='UNFORMATTED',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')

     	   DO l=1,SIZE(tshdprc)

     	      WRITE(*,*)'tshdprc(',l,'): ',tshdprc(l)
     	      prob(:,:)=0.0
     	      DO nmb=1,nmembers
     	   	 DO j=1,jmax
     	   	    DO i=1,imax
     	   	       IF ( (prcwkac(i,j,nmb) .NE. undef) .AND. (prob(i,j) .NE. undef) ) THEN
     	   		  IF ( prcwkac(i,j,nmb) .GE. tshdprc(l) ) THEN
     	   		     prob(i,j)=prob(i,j)+1.0
     	   		  END IF
	   	       ELSE
     	   		  prob(i,j)=undef
     	   	       END IF
     	   	    END DO
     	   	 END DO
     	      END DO
     	      DO j=1,jmax
     	   	 DO i=1,imax
     	   	    IF ( prob(i,j) .NE. undef ) THEN
     	   	       prob(i,j)=prob(i,j)/FLOAT(nmembers)
     	   	    END IF
     	   	 END DO
     	      END DO
     	      WRITE(80) prob

     	   END DO 
     	   CLOSE(80)
 
     	   WRITE(79,'(A)')TRIM(dirout)//TRIM(lstoutctl(nwk))
     	   WRITE(79,'(A)')TRIM(dirout)//TRIM(lstoutgrb(nwk))

     	   nwk=nwk+1
     	   nhrpwk=nwk*ndacc*24      !nwk=number of period (week,pentad,etc), 
     	   prcwkac(:,:,:)=0.0	    !ndacc=number of days to accumulate prec, 24=nb of hours in a day		      

	   IF (nwk .GT. nweek) EXIT 

     	END IF

     END IF
  ENDDO

  CLOSE(79)

END PROGRAM probweek



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to set up basic variables
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE attribute(unit,undef,imax,jmax,nmembers,nfctdy,freqcalc,nweek,ndacc, &
  		   dirin,dirout,prefx,resol) 

  IMPLICIT NONE

  INTEGER,	     INTENT(IN )  :: unit

  INTEGER,	     INTENT(OUT)  :: imax,jmax,nmembers,nfctdy,freqcalc,nweek,ndacc
  REAL, 	     INTENT(OUT)  :: undef
  CHARACTER (LEN=*), INTENT(OUT)  :: dirin,dirout,prefx,resol

  CHARACTER (LEN=200) :: line,line2
  INTEGER	      :: ierr

  ierr=0      ! To start the loop

  DO WHILE ( ierr.EQ.0 )

  	 READ(unit,'(A14,A)', IOSTAT=ierr) line,line2
	      IF( line(1:10).EQ.'UNDEF     ' ) THEN
	       READ(line2(1:10),'(1PE10.3)')undef
  	 ELSE IF( line(1:10).EQ.'IMAX      ' ) THEN
  	       READ(line2(1:5),'(I5)')  imax
  	 ELSE IF( line(1:10).EQ.'JMAX      ' ) THEN
  	       READ(line2(1:5),'(I5)')  jmax
  	 ELSE IF( line(1:10).EQ.'NMEMBERS  ' ) THEN
  	       READ(line2(1:4),'(I4)')  nmembers
  	 ELSE IF( line(1:10).EQ.'NFCTDY    ' ) THEN
  	       READ(line2(1:4),'(I4)')  nfctdy
  	 ELSE IF( line(1:10).EQ.'FREQCALC  ' ) THEN
  	       READ(line2(1:4),'(I4)')  freqcalc
  	 ELSE IF( line(1:10).EQ.'NWEEK     ' ) THEN 
  	       READ(line2(1:4),'(I4)')  nweek
  	 ELSE IF( line(1:10).EQ.'NDACC     ' ) THEN 
  	       READ(line2(1:4),'(I4)')  ndacc
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
! Subroutine to create the ctl's for probability files
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE CreateCtl(unit,date,imax,jmax,ndacc,glat,fileout,resol,undef) 

  IMPLICIT NONE

  INTEGER,               INTENT(IN) :: unit,imax,jmax,ndacc
  REAL, 		 INTENT(IN) :: undef
  REAL, DIMENSION(jmax), INTENT(IN) :: glat
  CHARACTER(LEN=*),      INTENT(IN) :: date,fileout,resol


  REAL :: delx
  INTEGER :: j,iday,imonth,iyear,iutc
  CHARACTER(LEN= 3), DIMENSION(12) :: mon


  DATA mon/'JAN','FEB','MAR','APR','MAY','JUN',  &
           'JUL','AUG','SEP','OCT','NOV','DEC'/

  !informations to tdef
  READ(date(1: 4),'(I4)') iyear    
  READ(date(5: 6),'(I2)') imonth
  READ(date(7: 8),'(I2)') iday
  READ(date(9:10),'(I2)') iutc
  
  delx=360.0/float(imax) ! Increment in zonal direction 
  
  WRITE(unit,'(A)')'DSET ^'//TRIM(fileout)
  WRITE(unit,'(A)')'*'
  WRITE(unit,'(A)')'OPTIONS SEQUENTIAL BIG_ENDIAN YREV'
  WRITE(unit,'(A)')'*'
  WRITE(unit,'(A,1PE10.3)')'UNDEF ',undef 
  WRITE(unit,'(A)')'*'
  WRITE(unit,'(3A)')'TITLE PROBABILITIES FROM ENS CPTEC AGCM v3.0 1999 ',TRIM(resol),'  COLD'     
  WRITE(unit,'(A)')'*'
  WRITE(unit,'(A,I5,A,F8.6,3X,F8.6)')'XDEF  ',imax,'  LINEAR    ',0.0,delx
  WRITE(unit,'(A,I5,A)')'YDEF  ',jmax,'  LEVELS' 
  WRITE(unit,'(8F10.5)') (glat(j),j=jmax,1,-1)
  WRITE(unit,'(A)')'ZDEF   1 LEVELS 1000'
  WRITE(unit,'(A,I2.2,A,I2.2,A,I4.4,A)') 'TDEF   1 LINEAR ',iutc,'Z',iday,  &
                                          mon(imonth),iyear,' 06HR'
  WRITE(unit,'(A)')'*'
  WRITE(unit,'(A)')'VARS  6'
  WRITE(unit,'(A,I2,A)')'PROB15  0 99 PROB. OF ',ndacc,' DAYS ACCUMULATED PRECIPITATION > 15.0 mm' 
  WRITE(unit,'(A,I2,A)')'PROB20  0 99 PROB. OF ',ndacc,' DAYS ACCUMULATED PRECIPITATION > 20.0 mm' 
  WRITE(unit,'(A,I2,A)')'PROB25  0 99 PROB. OF ',ndacc,' DAYS ACCUMULATED PRECIPITATION > 25.0 mm' 
  WRITE(unit,'(A,I2,A)')'PROB30  0 99 PROB. OF ',ndacc,' DAYS ACCUMULATED PRECIPITATION > 30.0 mm' 
  WRITE(unit,'(A,I2,A)')'PROB40  0 99 PROB. OF ',ndacc,' DAYS ACCUMULATED PRECIPITATION > 40.0 mm' 
  WRITE(unit,'(A,I2,A)')'PROB50  0 99 PROB. OF ',ndacc,' DAYS ACCUMULATED PRECIPITATION > 50.0 mm' 
  WRITE(unit,'(A)')'ENDVARS'
 
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

  INTEGER                :: i,iyy,imm,idd,ihh,lastd,inthour,rest
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
! Subroutine to generate the list of forecast files 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  
  SUBROUTINE getlstgrb(anldate,fctdate,fcthours,ngrbs,nmembers,nweek,ndacc,prefx,resol, &
                       lstingrb,lstoutctl,lstoutgrb,lstoutlst)

  IMPLICIT NONE
  
  INTEGER,                   INTENT(IN)  :: ngrbs,nmembers,nweek,ndacc
  INTEGER, DIMENSION(ngrbs), INTENT(IN)  :: fcthours

  CHARACTER(len=10),                   INTENT(IN)  :: anldate
  CHARACTER(len=10), DIMENSION(ngrbs), INTENT(IN)  :: fctdate
  CHARACTER(LEN=*),                    INTENT(IN)  :: prefx,resol                

  CHARACTER(len=*),                             INTENT(OUT)  :: lstoutlst
  CHARACTER(len=*),  DIMENSION(nweek),          INTENT(OUT)  :: lstoutctl,lstoutgrb
  CHARACTER(len=*),  DIMENSION(nmembers,ngrbs), INTENT(OUT)  :: lstingrb

!+++++Local variables

  INTEGER           :: k,nf,nwk,membm,nhrpwk
 
 
  ! fct files

  nwk=1                   !nwk=number of period (week,pentad,etc), ndacc=number of days to accumulate prec, 
  nhrpwk=nwk*ndacc*24	  !24=nb of hours in a day

  membm=(nmembers-1)/2

  DO nf=1,ngrbs

     WRITE(*,*)' '
     DO k=1,membm
        IF (nf .EQ. 1) THEN
          WRITE(lstingrb(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
          WRITE(*,*)'lstingrb(',k,',',nf,')=',TRIM(lstingrb(k,nf))
        END IF
        IF (nf .EQ. 2) THEN
          WRITE(lstingrb(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
          WRITE(*,*)'lstingrb(',k,',',nf,')=',TRIM(lstingrb(k,nf))
        END IF
        IF (nf .GT. 2) THEN
          WRITE(lstingrb(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
          WRITE(*,*)'lstingrb(',k,',',nf,')=',TRIM(lstingrb(k,nf))
        END IF
     END DO
     DO k=1,membm
        IF (nf .EQ. 1) THEN
          WRITE(lstingrb(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
          WRITE(*,*)'lstingrb(',k+membm,',',nf,')=',TRIM(lstingrb(k+membm,nf))
        END IF
        IF (nf .EQ. 2) THEN
          WRITE(lstingrb(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
          WRITE(*,*)'lstingrb(',k+membm,',',nf,')=',TRIM(lstingrb(k+membm,nf))
        END IF
        IF (nf .GT. 2) THEN
          WRITE(lstingrb(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
          WRITE(*,*)'lstingrb(',k+membm,',',nf,')=',TRIM(lstingrb(k+membm,nf))
        END IF
     END DO
        IF (nf .EQ. 1) THEN
          WRITE(lstingrb(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
          WRITE(*,*)'lstingrb(',nmembers,',',nf,')=',TRIM(lstingrb(nmembers,nf))
        END IF
        IF (nf .EQ. 2) THEN
          WRITE(lstingrb(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
          WRITE(*,*)'lstingrb(',nmembers,',',nf,')=',TRIM(lstingrb(nmembers,nf))
        END IF
        IF (nf .GT. 2) THEN
          WRITE(lstingrb(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
          WRITE(*,*)'lstingrb(',nmembers,',',nf,')=',TRIM(lstingrb(nmembers,nf))
        END IF

        IF (fcthours(nf) .EQ. nhrpwk) THEN
          WRITE(lstoutctl(nwk),'(6A)')'weekprecprob',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol),'.ctl'
          WRITE(*,*)'lstoutctl(',nwk,')=',TRIM(lstoutctl(nwk))
          WRITE(lstoutgrb(nwk),'(5A)')'weekprecprob',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol)
          WRITE(*,*)'lstoutgrb(',nwk,')=',TRIM(lstoutgrb(nwk))
          nwk=nwk+1
          nhrpwk=nwk*ndacc*24	  !nwk=number of period (week,pentad,etc), 
        END IF                    !ndacc=number of days to accumulate prec, 24=nb of hours in a day

  END DO
          WRITE(lstoutlst,'(6A)')'weekprecprob',anldate(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol),'.lst'
          WRITE(*,*)'lstoutlst=',TRIM(lstoutlst)

  RETURN
  END SUBROUTINE getlstgrb
  
  
