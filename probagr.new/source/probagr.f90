  PROGRAM probagr
  
  USE GaussRep, ONLY : CreateGaussRep, glat

  IMPLICIT NONE
  
  ! Evaluate the week mean precipitation from the ensemble mean
  ! Author: Antonio Marcos Mendonca 
  ! Any comentary, send a e-mail to: mendonca@cptec.inpe.br
  ! Last Atualization: Aug/2008
    
  !
  ! Parameter
  !
  
  REAL, PARAMETER :: undef=-2.56E+33 

  !
  ! Variables
  !
  
  INTEGER :: imax,jmax,jmaxhf
  INTEGER :: i,j,k,nf,nw,ifct,irec,ierro
  INTEGER :: nmembers,nfctdy,nfiles
  INTEGER :: iprec,nweek,ndacc,noutpd 

  CHARACTER(LEN=10)  :: anldate     ! initial date
  CHARACTER(LEN=200) :: dirin	    ! dir of ensemble input data
  CHARACTER(LEN=200) :: dirout      ! dir of precipitation mean output data
  CHARACTER(LEN=200) :: probagrf    ! namelist file
  CHARACTER(LEN=200) :: resol	    ! horizontal and vertical model resolution
  CHARACTER(LEN=200) :: prefx       ! preffix for input and output files
  CHARACTER(LEN=200) :: fileout,fileoutlst

  REAL  	    , DIMENSION(:,:),   ALLOCATABLE :: vars,prob
  REAL  	    , DIMENSION(:,:,:), ALLOCATABLE :: prcwkac
  CHARACTER(len=10)                                 :: fctdatei
  CHARACTER(len=10),  DIMENSION(:),     ALLOCATABLE :: fctdate
  CHARACTER(LEN=200), DIMENSION(:,:),   ALLOCATABLE :: lstfct
  
  !
  ! Atributions
  !
  
  CALL GETARG(1,anldate)
  WRITE(probagrf,'(3A)')'probagr.',anldate,'.nml'

  CALL attribute(probagrf,imax,jmax,nmembers,nfctdy,dirin,dirout,resol,prefx,iprec, &
                 nweek,ndacc,noutpd) 

  WRITE(*,*)'imax    : ',imax
  WRITE(*,*)'jmax    : ',jmax
  WRITE(*,*)'nmembers: ',nmembers
  WRITE(*,*)'nfctdy  : ',nfctdy
  WRITE(*,*)'dirin   : ',TRIM(dirin)
  WRITE(*,*)'dirout  : ',TRIM(dirout)
  WRITE(*,*)'resol   : ',TRIM(resol)
  WRITE(*,*)'prefx   : ',TRIM(prefx)
  WRITE(*,*)'iprec   : ',iprec 
  WRITE(*,*)'nweek   : ',nweek 
  WRITE(*,*)'ndacc   : ',ndacc 
  WRITE(*,*)'noutpd  : ',noutpd
  
  
  !
  ! Dynamic allocations
  !
  
  ALLOCATE(fctdate(nfctdy*noutpd), STAT=ierro)
  IF (ierro .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fctdate'
  ALLOCATE(lstfct(nmembers,nfctdy*noutpd), STAT=ierro)
  IF (ierro .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstfct'
  ALLOCATE(vars(imax,jmax), STAT=ierro)
  IF (ierro .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: vars'
  ALLOCATE(prob(imax,jmax), STAT=ierro)
  IF (ierro .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: prob'
  ALLOCATE(prcwkac(imax,jmax,nmembers), STAT=ierro)
  IF (ierro .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: prcwkac'

  !
  !  Obtain gaussian latitudes
  !
  
  jmaxhf=jmax/2
  CALL CreateGaussRep(jmax,jmaxhf) 

  !
  !  Generate the list of forecast files
  !
  
  CALL caldate(anldate,nfctdy,noutpd,fctdate)

  CALL getlstgrb(anldate,nfctdy,noutpd,fctdate,nmembers,prefx,resol,lstfct)

  !
  !  Week probability precipitation
  !

  WRITE(*,*)' '
  WRITE(fileoutlst,'(6A)')'wmaprecprob',anldate(1:10),fctdate(nfctdy*noutpd)(1:10),'.',TRIM(resol),'.lst'
  WRITE(*,*) 'unit79: ',TRIM(dirout)//TRIM(fileoutlst)
  OPEN(79,FILE=TRIM(dirout)//TRIM(fileoutlst),FORM='FORMATTED',STATUS='UNKNOWN')
  WRITE(*,*)' '

  ifct=1
  DO nw=1,nweek
  
    !
    ! Evaluate the week accumulatted forecast precipitation for each ensemble member
    !
  
     nfiles=ndacc*noutpd*nw	 !nfiles -> last binary file to be opened: ndacc(number of days to accumulate prec)*noutpd(number of outputs per day)*nw(week)

     DO k=1,nmembers

        prcwkac(:,:,k)=0.0	 			        

        DO nf=ifct,nfiles

          WRITE(*,*)'unit90: ',TRIM(dirin)//TRIM(lstfct(k,nf))
          OPEN(90,FILE=TRIM(dirin)//TRIM(lstfct(k,nf)),STATUS='OLD',FORM='UNFORMATTED',&
	          ACTION='READ',ACCESS='SEQUENTIAL')
   
           DO irec=1,iprec
         	READ(90) vars
           ENDDO
           CLOSE(90)
     
           vars(1:imax,1:jmax) = vars(1:imax,1:jmax)/FLOAT(noutpd) 
           prcwkac(1:imax,1:jmax,k) = prcwkac(1:imax,1:jmax,k) + vars(1:imax,1:jmax)
        ENDDO

     ENDDO

     !
     !  Evaluate the probabilities and write in the output binary files
     !
    
     WRITE(*,*)' '
     IF (ifct .EQ. 1) THEN
        WRITE(fctdatei,'(A)')anldate(1:10)
     ELSE
        WRITE(fctdatei,'(A)')fctdate(ifct-1)(1:10)
     ENDIF
     WRITE(fileout,'(5A)')'wmaprecprob',fctdatei,fctdate(nfiles)(1:10),'.',TRIM(resol)
     WRITE(*,*)'unit80: ',TRIM(dirout)//TRIM(fileout)
     OPEN(80,FILE=TRIM(dirout)//TRIM(fileout),FORM='UNFORMATTED',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
     CALL CreateCtl(fctdatei,imax,jmax,glat,dirout,fileout,resol) 

     prob(:,:)=0.0
     DO k=1,nmembers
     	DO j=1,jmax
     	   DO i=1,imax
     	      IF ( (prcwkac(i,j,k) .NE. undef) .AND. (prob(i,j) .NE. undef) ) THEN
     		 IF ( prcwkac(i,j,k) .GE. 10.0 ) THEN
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
     CLOSE(80)
 
     WRITE(79,'(A)')TRIM(dirout)//TRIM(fileout)//'.ctl'
     WRITE(79,'(A)')TRIM(dirout)//TRIM(fileout)

     ifct=nfiles+1
  ENDDO

  CLOSE(79)

END PROGRAM probagr


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to set up basic variables
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

SUBROUTINE attribute(probagrf,imax,jmax,nmembers,nfctdy,dirin,dirout,resol,prefx,iprec, &
                     nweek,ndacc,noutpd) 

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN)	:: probagrf
CHARACTER (LEN=*), INTENT(OUT)  :: dirin,dirout,resol,prefx
INTEGER,	   INTENT(OUT)  :: imax,jmax,nfctdy,nmembers
INTEGER,	   INTENT(OUT)  :: iprec,nweek,ndacc,noutpd

CHARACTER (LEN=200) :: line,line2
INTEGER 	    :: ierro

WRITE(*,*)'FILE100: ',TRIM(probagrf)
OPEN(100,FILE=TRIM(probagrf),STATUS='OLD',FORM='FORMATTED')

ierro=0      ! To start the loop

DO WHILE ( ierro.EQ.0 )

       READ(100,'(A14,A)', IOSTAT=ierro) line,line2
            IF( line(1:10).EQ.'IMAX      ' ) THEN
	     READ(line2(1:4),'(I4)')  imax
       ELSE IF( line(1:10).EQ.'JMAX      ' ) THEN
	     READ(line2(1:4),'(I4)')  jmax
       ELSE IF( line(1:10).EQ.'NMEMBERS  ' ) THEN
	     READ(line2(1:4),'(I4)')  nmembers
       ELSE IF( line(1:10).EQ.'NFCTDY    ' ) THEN
	     READ(line2(1:4),'(I4)')  nfctdy
       ELSE IF( line(1:10).EQ.'DATAINDIR ' ) THEN
	     dirin=line2
       ELSE IF( line(1:10).EQ.'DATAOUTDIR' ) THEN 
	     dirout=line2
       ELSE IF( line(1:10).EQ.'RESOL     ' ) THEN 
	     resol=line2
       ELSE IF( line(1:10).EQ.'PREFX     ' ) THEN 
	     prefx=line2
       ELSE IF( line(1:10).EQ.'IPREC     ' ) THEN 
	     READ(line2(1:4),'(I4)')  iprec
       ELSE IF( line(1:10).EQ.'NWEEK     ' ) THEN 
	     READ(line2(1:4),'(I4)')  nweek
       ELSE IF( line(1:10).EQ.'NDACC     ' ) THEN 
	     READ(line2(1:4),'(I4)')  ndacc
       ELSE IF( line(1:10).EQ.'NOUTPD    ' ) THEN 
	     READ(line2(1:4),'(I4)')  noutpd
       END IF
ENDDO

CLOSE(100)

RETURN
END SUBROUTINE attribute


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subrotine to shift a character string to uppercase
! From Book: Fortran 90/95 for Scientists and Engineers 
!    Author: Stephen J. Chapman   
!       Ed.: McGraw-Hill, First Edition, 1998.               
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

SUBROUTINE ucase(string)

IMPLICIT NONE

CHARACTER(len=*), INTENT(INOUT) :: string

INTEGER :: i
INTEGER :: length

length = LEN(string)

DO i=1,length
  IF ( LGE(string(i:i),'a') .AND. LLE(string(i:i),'z') ) THEN
     string(i:i) = ACHAR( IACHAR( string(i:i) ) - 32 ) 
  ENDIF
ENDDO

RETURN
END SUBROUTINE ucase



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to create the ctl's for probability files
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE createctl(date,imax,jmax,glat,dirout,fileout,resol) 

  IMPLICIT NONE

  INTEGER,               INTENT(IN) :: imax,jmax
  REAL, DIMENSION(jmax), INTENT(IN) :: glat
  CHARACTER(LEN=*),      INTENT(IN) :: date,dirout,fileout,resol


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
  
  
  WRITE(*,*)'unit95: ',TRIM(dirout)//TRIM(fileout)//'.ctl'
  OPEN(95,FILE=TRIM(dirout)//TRIM(fileout)//'.ctl',STATUS='UNKNOWN',FORM='FORMATTED') 
  WRITE(*,*)'  '

  WRITE(95,'(A)')'DSET ^'//TRIM(fileout)
  WRITE(95,'(A)')'*'
  WRITE(95,'(A)')'OPTIONS SEQUENTIAL BIG_ENDIAN YREV'
  WRITE(95,'(A)')'*'
  WRITE(95,'(A)')'UNDEF -2.56E+33' 
  WRITE(95,'(A)')'*'
  WRITE(95,'(3A)')'TITLE PROBABILITIES FROM ENS CPTEC AGCM v3.0 1999 ',TRIM(resol),'  COLD'     
  WRITE(95,'(A)')'*'
  WRITE(95,'(A,I5,A,F8.6,3X,F8.6)')'XDEF  ',imax,'  LINEAR    ',0.0,delx
  WRITE(95,'(A,I5,A)')'YDEF  ',jmax,'  LEVELS' 
  WRITE(95,'(8F10.5)') (glat(j),j=jmax,1,-1)
  WRITE(95,'(A)')'ZDEF   1 LEVELS 1000'
  WRITE(95,'(A,I2.2,A,I2.2,A,I4.4,A)') 'TDEF   1 LINEAR ',iutc,'Z',iday,  &
                                             mon(imonth),iyear,' 12HR'
  WRITE(95,'(A)')'*'
  WRITE(95,'(A)')'VARS  1'
  WRITE(95,'(A)')'PROB10  0 99 PROB. OF 7 DAYS ACCUMULATED PRECIPITATION > 10.0 mm' 
  WRITE(95,'(A)')'ENDVARS'
  
  CLOSE(95)
  
  END SUBROUTINE createctl
 
  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to calculate the dates of forecast files
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  
  SUBROUTINE caldate(anldate,nfctdy,noutpd,fctdate)
  
  IMPLICIT NONE
  
  INTEGER,                                     INTENT(IN)  :: nfctdy,noutpd
  CHARACTER(len=10),                           INTENT(IN)  :: anldate
  CHARACTER(len=10), DIMENSION(nfctdy*noutpd), INTENT(OUT) :: fctdate

!+++++Local variables

  INTEGER                :: i,iyy,imm,idd,ihh,lstd,intnh,rest
  INTEGER, DIMENSION(12) :: monl,monlb
  
  CHARACTER(len=10)   :: datep 

!+++++
  
  DATA monl  /31,28,31,30,31,30,31,31,30,31,30,31/
  DATA monlb /31,29,31,30,31,30,31,31,30,31,30,31/

  
  READ(anldate(1: 4),'(I4)') iyy
  READ(anldate(5: 6),'(I2)') imm
  READ(anldate(7: 8),'(I2)') idd
  READ(anldate(9:10),'(I2)') ihh

  intnh=24/noutpd
  
  WRITE(*,'(/,A)') 'FCT DATES:'

  rest=MOD(iyy,4)	! Decide if is a leap year
  IF (rest .EQ. 0) THEN
     lstd=monlb(imm)
  ELSE
     lstd=monl(imm)
  END IF

  DO i=1,nfctdy*noutpd
     
     ihh=ihh+intnh
     IF (ihh .EQ. 24) THEN
 
     	ihh=0
        idd=idd+1

        IF (idd .GT. lstd) THEN

           idd=1
           imm=imm+1
           IF (imm .GT. 12) THEN
	      iyy=iyy+1
	      imm=1
	   END IF

           rest=MOD(iyy,4)       ! Decide if is a leap year
           IF (rest .EQ. 0) THEN
	      lstd=monlb(imm)
           ELSE
	      lstd=monl(imm)
           END IF

        END IF

     END IF
     
  
     WRITE(datep,'(I4.4,3I2.2)') iyy,imm,idd,ihh
     fctdate(i)(1:10)=datep(1:10)
     
     WRITE(*,'(A,I2.2,2A)') 'fctdate(',i,'): ',fctdate(i)
  
  END DO
  
  RETURN
  END SUBROUTINE caldate
  
  
  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to generate the list of forecast files 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  
  SUBROUTINE getlstgrb(anldate,nfctdy,noutpd,fctdate,nmembers,prefx,resol,lstfct)

  
  IMPLICIT NONE
  
  INTEGER,                              INTENT(IN)  :: nfctdy,nmembers,noutpd
  CHARACTER(len=10),                    INTENT(IN)  :: anldate
  CHARACTER(len=10), DIMENSION(nfctdy), INTENT(IN)  :: fctdate
  CHARACTER(LEN=*)                    , INTENT(IN)  :: prefx,resol           

  CHARACTER(len=*),  DIMENSION(nmembers,nfctdy), INTENT(OUT)  :: lstfct

!+++++Local variables

  INTEGER                :: k,nf,membm
 
 

   ! anl file
 
!AMM    WRITE(*,*)' '
!AMM    WRITE(lstanl,'(7A)') 'GPOS',TRIM(prefx),anldate(1:10),anldate(1:10),'P.icn.',TRIM(resol)
!AMM    WRITE(diranl,'(7A)') TRIM(dirlst),anldate(1:4),'/',anldate(5:6),'/',anldate(7:8),'/'
!AMM    
!AMM    WRITE(*,*)'lstanl= ',TRIM(diranl)//TRIM(lstanl)

   ! fct files

  membm=(nmembers-1)/2

  DO nf=1,nfctdy*noutpd
     WRITE(*,*)' '
     DO k=1,membm
!AMM         WRITE(dirfct(nf),'(7A)') TRIM(dirlst),fctdate(nf)(1:4),'/',fctdate(nf)(5:6),'/',fctdate(nf)(7:8),'/'
        WRITE(lstfct(k,nf),'(A,I2.2,5A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol)
!AMM         WRITE(*,*)'lstfct(',k,',',nf,')=',TRIM(dirfct(nf))//TRIM(lstfct(k,nf))
        WRITE(*,*)'lstfct(',k,',',nf,')=',TRIM(lstfct(k,nf))
     END DO
     DO k=1,membm
        WRITE(lstfct(k+membm,nf),'(A,I2.2,5A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol)
!AMM         WRITE(*,*)'lstfct(',k+membm,',',nf,')=',TRIM(dirfct(nf))//TRIM(lstfct(k+membm,nf))
        WRITE(*,*)'lstfct(',k+membm,',',nf,')=',TRIM(lstfct(k+membm,nf))
     END DO
        WRITE(lstfct(nmembers,nf),'(6A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol)
!AMM         WRITE(*,*)'lstfct(',nmembers,',',nf,')=',TRIM(dirfct(nf))//TRIM(lstfct(nmembers,nf))
        WRITE(*,*)'lstfct(',nmembers,',',nf,')=',TRIM(lstfct(nmembers,nf))

!AMM         WRITE(direnm(nf),'(7A)') TRIM(dirmed),fctdate(nf)(1:4),'/',fctdate(nf)(5:6),'/',fctdate(nf)(7:8),'/'
!AMM         WRITE(lstenm(nf),'(6A)')'GPOSENM',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol)
!AMM         WRITE(*,*)'lstenm(',nf,')=',TRIM(direnm(nf))//TRIM(lstenm(nf))
  END DO



  RETURN
  END SUBROUTINE getlstgrb
  
  
