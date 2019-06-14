  PROGRAM ensmed

  !
  ! Read mbembers of ensemble and evaluate the ensemble mean
  !

  IMPLICIT NONE

  !
  ! Variables
  !

  CHARACTER(LEN=100) :: dirin
  CHARACTER(LEN=100) :: dirout
  CHARACTER(LEN=100) :: dirctl
  CHARACTER(LEN=100) :: ensmedi

  INTEGER :: ierr,imax,jmax,nbs,nmb,irec
  INTEGER :: ngrbs
  INTEGER :: nfctdy,nmembers,freqcalc,noutpday,rest
  INTEGER :: nbitss,lgrib,lskip,nbyte,nbits
  INTEGER*4                 :: nxny
  INTEGER*4, DIMENSION(200) :: kpds,kgds
  INTEGER*4, DIMENSION(200) :: kens

  REAL, DIMENSION(:), ALLOCATABLE :: tmp,varsmed

  CHARACTER(LEN=10)  :: anldate
  CHARACTER(LEN=200) :: prefx,resol
  CHARACTER(LEN=200) :: lstoutlst
  CHARACTER(LEN=10),  DIMENSION(:),   ALLOCATABLE :: fctdate
  CHARACTER(LEN=200), DIMENSION(:),   ALLOCATABLE :: lstoutctl,lstoutgrb
  CHARACTER(LEN=200), DIMENSION(:,:), ALLOCATABLE :: lstinctl,lstingrb

  LOGICAL*1, DIMENSION(:), ALLOCATABLE :: bitmap

  INCLUDE "mpif.h"
  INTEGER :: numproc,thisproc,firstcount,lastcount,ngrbslocal,restlocal

  !
  ! Attributions
  !

  CALL GETARG(1,anldate)
  WRITE(ensmedi,'(A7,A10,A4)')'ensmed.',anldate,'.nml'

  !
  ! Call attribution function
  !

  WRITE(*,*) 'File 100: '//TRIM(ensmedi)
  OPEN(100,FILE=TRIM(ensmedi),STATUS='OLD',FORM='FORMATTED')

  CALL attribute(100,nbyte,nfctdy,freqcalc,nmembers,imax,jmax,dirin,dirout,resol,prefx) 

  CLOSE(100)

  WRITE(*,*) 'nbyte   : ',nbyte
  WRITE(*,*) 'nfctdy  : ',nfctdy
  WRITE(*,*) 'freqcalc: ',freqcalc
  WRITE(*,*) 'nmembers: ',nmembers
  WRITE(*,*) 'imax    : ',imax
  WRITE(*,*) 'jmax    : ',jmax
  WRITE(*,*) 'dirin   : ',dirin
  WRITE(*,*) 'dirout  : ',dirout
  WRITE(*,*) 'resol   : ',resol
  WRITE(*,*) 'prefx   : ',prefx


  IF (freqcalc .GT. 24) STOP 'Main Program: freqcalc must be .LE. 24'
  rest=MOD(24,freqcalc)
  IF (rest .NE. 0) THEN
    STOP 'Main Program: freqcalc must be a divisor of 24. Ex.: 6, 12 and 24'
  ELSE
    noutpday=24/freqcalc
  END IF

  ngrbs=nfctdy*noutpday+2 !number of gribs icn + inz + fct's
  nxny=imax*jmax
  nbits=8*nbyte
  
  !
  ! Dynamic allocation
  !

  ALLOCATE(fctdate(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'DOES NOT ALLOCATE CORRECTLY: fctdate'
  ALLOCATE(lstinctl(nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'DOES NOT ALLOCATE CORRECTLY: lstinctl '
  ALLOCATE(lstingrb(nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'DOES NOT ALLOCATE CORRECTLY: lstingrb '
  ALLOCATE(lstoutctl(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'DOES NOT ALLOCATE CORRECTLY: lstoutctl '
  ALLOCATE(lstoutgrb(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'DOES NOT ALLOCATE CORRECTLY: lstoutgrb '
  ALLOCATE(tmp(nxny), STAT=ierr)
  IF (ierr .NE. 0) STOP 'DOES NOT ALLOCATE CORRECTLY: tmp'
  ALLOCATE(bitmap(nxny), STAT=ierr)
  IF (ierr .NE. 0) STOP 'DOES NOT ALLOCATE CORRECTLY: bitmap'
  ALLOCATE(varsmed(nxny), STAT=ierr)
  IF (ierr .NE. 0) STOP 'DOES NOT ALLOCATE CORRECTLY: varsmed'

  !
  ! Generate the list of forecast output dates and hours
  !

  CALL caldate(anldate,ngrbs,freqcalc,fctdate)

  !
  ! Generate the list of forecast files: bin's, CTL's and .lst
  !

  CALL getlstgrb(anldate,fctdate,ngrbs,nmembers,prefx,resol, &
                 lstinctl,lstingrb,lstoutctl,lstoutgrb,lstoutlst)

  !
  ! Evaluate the mean of N members
  !

  WRITE(*,*)'FILE10: ',TRIM(dirout)//TRIM(lstoutlst)
  OPEN(10,FILE=TRIM(dirout)//TRIM(lstoutlst),STATUS='UNKNOWN',FORM='FORMATTED')

  DO nbs=1,ngrbs

    WRITE(10,'(A)')TRIM(lstoutctl(nbs))
    WRITE(10,'(A)')TRIM(lstoutgrb(nbs))
    
  END DO

  CLOSE(10)


  lgrib=101+nxny*2   ! number of bytes for each variable: nxny*2 bytes for each grib record plus
                     !                                    101 additional bytes for each record

  CALL MPI_INIT(ierr)                                   !Begin parallel computation
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,numproc,ierr)       !Number of parallel process (n)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD,thisproc,ierr)      !Number of this process (0,1,...,n-1)

  IF (numproc .LE. ngrbs) THEN
     ngrbslocal=ngrbs/numproc
     restlocal=ngrbs-ngrbslocal*numproc
  ELSE
     CALL MPI_FINALIZE(ierr)
     STOP 'The number of CPU or Process can not be greater than the number of ngrbs' 
  ENDIF

  IF (thisproc .LT. restlocal) THEN
     ngrbslocal=ngrbslocal+1
     restlocal=0
  ENDIF
  firstcount=thisproc*ngrbslocal+restlocal+1
  lastcount =(thisproc+1)*ngrbslocal+restlocal


  DO nbs=firstcount,lastcount

    WRITE(*,*)'FILE11: ',TRIM(dirin)//TRIM(lstinctl(nmembers,nbs))
    OPEN(11,FILE=TRIM(dirin)//TRIM(lstinctl(nmembers,nbs)),STATUS='UNKNOWN',FORM='FORMATTED')

    WRITE(*,*)'FILE19: ',TRIM(dirout)//TRIM(lstoutctl(nbs))
    OPEN(19,FILE=TRIM(dirout)//TRIM(lstoutctl(nbs)),STATUS='UNKNOWN',FORM='FORMATTED')

    CALL CreateCtlGrib(11,19,lstoutgrb(nbs))
    
    CLOSE(11)
    CLOSE(19)

!    WRITE(*,*)'FILE20: ',TRIM(dirout)//TRIM(lstoutgrb(nbs))
    CALL BAOPEN(20,TRIM(dirout)//TRIM(lstoutgrb(nbs)),ierr)
    IF (ierr .NE. 0) THEN
      WRITE(*,*) 'STOP BAOPEN TROUBLE  File: ',20,TRIM(dirout)//TRIM(lstoutgrb(nbs)),'  ierr:',ierr
      STOP
    ELSE 
      WRITE(*,*) 'BAOPEN OK  File: ',20,TRIM(dirout)//TRIM(lstoutgrb(nbs)),'  ierr:',ierr
    END IF 

    DO nmb=1,nmembers
      CALL BAOPEN(20+nmb,TRIM(dirin)//TRIM(lstingrb(nmb,nbs)),ierr)
      IF (ierr .NE. 0) THEN
        WRITE(*,*) 'STOP BAOPEN TROUBLE  File:',20+nmb,TRIM(dirin)//TRIM(lstingrb(nmb,nbs)),'  ierr:',ierr
        STOP
      ELSE 
        WRITE(*,*) 'BAOPEN OK  File:',20+nmb,TRIM(dirin)//TRIM(lstingrb(nmb,nbs)),'  ierr:',ierr
      END IF 
    ENDDO


    irec=1
    DO 
 
      varsmed(:)=0.0
      DO nmb=1,nmembers
  
         lskip=(irec-1)*lgrib  ! number of bytes to be skiped

         CALL GETGB1R(20+nmb,lskip,lgrib,nxny,kpds,kgds,kens,bitmap,tmp,nbitss,ierr)

         IF (ierr .ne. 0) THEN
           WRITE(*,*) 'irec:',irec
           EXIT
         END IF

         varsmed(:)=varsmed(:)+tmp(:) 

      ENDDO

      IF (ierr .ne. 0) EXIT

      varsmed(:)=varsmed(:)/FLOAT(nmembers)

!!      CALL putgbn(20,nxny,kpds,kgds,0,nbits,bitmap,varsmed,ierr) ! write ensemble mean grib file
      CALL putgben(20,nxny,kpds,kgds,kens,0,nbits,bitmap,varsmed,ierr) ! write ensemble mean grib file
    
      IF (ierr .NE. 0) THEN
        write(*,*) 'PUTGBEN failed!  ierr=', ierr
        stop 'WRITE_GRIB'
      END IF

      irec=irec+1
    ENDDO

    CALL BACLOSE(20,ierr)
    IF (ierr .NE. 0) THEN
      WRITE(*,*) 'STOP BACLOSE TROUBLE  File: 20',TRIM(dirout)//TRIM(lstoutgrb(nbs)),'  ierr:',ierr
      STOP
    END IF 

    DO nmb=1,nmembers
      CALL BACLOSE(20+nmb,ierr)
      IF (ierr .NE. 0) THEN
        WRITE(*,*) 'STOP BACLOSE TROUBLE  File:',20+nmb,TRIM(dirin)//TRIM(lstingrb(nmb,nbs)),'  ierr:',ierr
        STOP
      END IF 
    ENDDO

  ENDDO

  CALL MPI_FINALIZE(ierr)

  END PROGRAM ensmed



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to make the attribution data
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    

  SUBROUTINE attribute(unit,nbyte,nfctdy,freqcalc,nmembers,imax,jmax,dirin,dirout,resol,prefx) 

  IMPLICIT NONE

  INTEGER,          INTENT(IN)   :: unit
  INTEGER,          INTENT(OUT)  :: nbyte,nfctdy,freqcalc,nmembers,imax,jmax
  CHARACTER(LEN=*), INTENT(OUT)  :: dirin,dirout,resol,prefx

  CHARACTER(LEN=100) :: line, line2
  INTEGER	     :: ierr
  
  
  ierr=0 
  
  DO WHILE (ierr .EQ. 0)
  
  	 READ(unit,'(A14,A)',IOSTAT=ierr) line,line2
  	 IF	(line(1:10) .EQ. 'NBYTE     ') THEN 
  	     READ(line2(1:4),'(I5)')  nbyte
  	 ELSE IF(line(1:10) .EQ. 'NFCTDY    ') THEN 
  	     READ(line2(1:4),'(I5)')  nfctdy
  	 ELSE IF(line(1:10) .EQ. 'FREQCALC  ') THEN 
  	     READ(line2(1:4),'(I5)')  freqcalc
  	 ELSE IF(line(1:10) .EQ. 'MEMB      ') THEN 
  	     READ(line2(1:4),'(I5)')  nmembers
         ELSE IF(line(1:10) .EQ. 'IMAX      ') THEN
             READ(line2(1:4),'(I5)')  imax
         ELSE IF(line(1:10) .EQ. 'JMAX      ') THEN
             READ(line2(1:4),'(I5)')  jmax
         ELSE IF(line(1:10) .EQ. 'DATAINDIR ') THEN
               dirin=line2
         ELSE IF(line(1:10) .EQ. 'DATAOUTDIR') THEN 
               dirout=line2
         ELSE IF(line(1:10) .EQ. 'RESOL     ') THEN
               resol=line2
         ELSE IF(line(1:10) .EQ. 'PREFX     ') THEN
               prefx=line2
         END IF

  END DO

  RETURN
  END SUBROUTINE attribute

 
  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to calculate the dates of forecast files
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  
  SUBROUTINE caldate(anldate,ngrbs,freqcalc,fctdate)
  
  IMPLICIT NONE
  
  INTEGER,                             INTENT(IN)  :: ngrbs,freqcalc
  CHARACTER(len=10),                   INTENT(IN)  :: anldate
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
                       lstinctl,lstingrb,lstoutctl,lstoutgrb,lstoutlst)
  
  IMPLICIT NONE
  
  INTEGER,                             INTENT(IN)  :: nmembers,ngrbs
  CHARACTER(len=10),                   INTENT(IN)  :: anldate
  CHARACTER(len=10), DIMENSION(ngrbs), INTENT(IN)  :: fctdate
  CHARACTER(LEN=*),                    INTENT(IN)  :: prefx,resol                

  CHARACTER(len=*),                             INTENT(OUT)  :: lstoutlst
  CHARACTER(len=*),  DIMENSION(ngrbs),          INTENT(OUT)  :: lstoutctl,lstoutgrb
  CHARACTER(len=*),  DIMENSION(nmembers,ngrbs), INTENT(OUT)  :: lstinctl,lstingrb

!+++++Local variables

  INTEGER                :: i,k,nf,membm,intnh


  ! fct files

  membm=(nmembers-1)/2

  DO nf=1,ngrbs

!     WRITE(*,*)' '

     DO k=1,membm
        IF (nf .EQ. 1) THEN
           WRITE(lstinctl(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.ctl'
           WRITE(lstingrb(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',k,',',nf,')=',TRIM(lstfct(k,nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstinctl(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.ctl'
           WRITE(lstingrb(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',k,',',nf,')=',TRIM(lstfct(k,nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstinctl(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.ctl'
           WRITE(lstingrb(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',k,',',nf,')=',TRIM(lstfct(k,nf))
        END IF
     END DO

     DO k=1,membm
        IF (nf .EQ. 1) THEN
           WRITE(lstinctl(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.ctl'
           WRITE(lstingrb(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',k+membm,',',nf,')=',TRIM(lstfct(k+membm,nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstinctl(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.ctl'
           WRITE(lstingrb(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',k+membm,',',nf,')=',TRIM(lstfct(k+membm,nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstinctl(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.ctl'
           WRITE(lstingrb(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',k+membm,',',nf,')=',TRIM(lstfct(k+membm,nf))
        END IF
     END DO

        IF (nf .EQ. 1) THEN
           WRITE(lstinctl(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.ctl'
           WRITE(lstingrb(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',nmembers,',',nf,')=',TRIM(lstfct(nmembers,nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstinctl(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.ctl'
           WRITE(lstingrb(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',nmembers,',',nf,')=',TRIM(lstfct(nmembers,nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstinctl(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.ctl'
           WRITE(lstingrb(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstfct(',nmembers,',',nf,')=',TRIM(lstfct(nmembers,nf))
        END IF

        IF (nf .EQ. 1) THEN
           WRITE(lstoutctl(nf),'(6A)')'GPOSENM',anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.ctl'
!           WRITE(*,*)'lstoutctl(',nf,')=',TRIM(lstoutctl(nf))
           WRITE(lstoutgrb(nf),'(6A)')'GPOSENM',anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstoutgrb(',nf,')=',TRIM(lstoutgrb(nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstoutctl(nf),'(6A)')'GPOSENM',anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.ctl'
!           WRITE(*,*)'lstoutctl(',nf,')=',TRIM(lstoutctl(nf))
           WRITE(lstoutgrb(nf),'(6A)')'GPOSENM',anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstoutgrb(',nf,')=',TRIM(lstoutgrb(nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstoutctl(nf),'(6A)')'GPOSENM',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.ctl'
!           WRITE(*,*)'lstoutctl(',nf,')=',TRIM(lstoutctl(nf))
           WRITE(lstoutgrb(nf),'(6A)')'GPOSENM',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
!           WRITE(*,*)'lstoutgrb(',nf,')=',TRIM(lstoutgrb(nf))
        END IF

  END DO
           WRITE(lstoutlst,'(6A)')'GPOSENM',anldate(1:10),fctdate(ngrbs)(1:10),'P.fct.',TRIM(resol),'.lst'
!           WRITE(*,*)'lstoutlst=',TRIM(lstoutlst)

  RETURN
  END SUBROUTINE getlstgrb 



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Generate the CTL's for ensemble mean grib files
! It is based on the CTL for each lead time
! Some characteristics is assumed: 
! i)   There is CTL's for grib ensemble members
! ii)  The DSET and INDEX is on the first lines of the original CTL's
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE CreateCtlGrib(unitin,unitout,lstoutgrb)
  
  IMPLICIT NONE
  
  INTEGER,         INTENT(IN)  :: unitin,unitout
  CHARACTER(LEN=*),INTENT(IN)  :: lstoutgrb		   


  INTEGER            :: lname,ierr
  CHARACTER(LEN=256) :: line

!++++++++++++

  lname=LEN_TRIM(lstoutgrb)

  WRITE(unitout,'(A)') 'dset ^'//TRIM(lstoutgrb)
  WRITE(unitout,'(A)') '*'
  WRITE(unitout,'(A)') 'index ^'//lstoutgrb(1:lname-4)//'.idx'    

  READ(unitin,'(A)') line
  READ(unitin,'(A)') line
  READ(unitin,'(A)') line

  DO
     
     READ(unitin,'(A)',IOSTAT=ierr) line
     IF ( ierr .NE. 0 ) EXIT
     WRITE(unitout,'(A)')TRIM(line)

  ENDDO

  END SUBROUTINE CreateCtlGrib
