  PROGRAM plumes

!  USE GaussRep, ONLY : CreateGaussRep, glat
!  USE Readvars, ONLY : ReadGrib

  IMPLICIT NONE

  !
  ! Program to evaluate the plumes from the grid-history of global ensemble 
  ! weather forecasting
  ! Author: Antonio Marcos Mendonca 
  ! Any comentary, send a e-mail to: mendonca@cptec.inpe.br
  ! Last Atualization: Oct/2007
  !

  !
  ! Variables
  !

  INTEGER :: nmb,nfd,nfd2,ngh,ngh2,ntm,idv,int,nintprob
  INTEGER :: imax,jmax,jmaxhf
  INTEGER :: nfctdy,nfcthours,nmembers,noutpday,ngrbs,freqcalc,nhours
  INTEGER :: intprobvar,nvarsp1,nvarsp2
  INTEGER :: ierr,rest,count
  INTEGER, ALLOCATABLE, DIMENSION(:)       :: fcthours,nvars,nbgh
  INTEGER, ALLOCATABLE, DIMENSION(:,:)     :: rec_vars,pos_ident

  REAL               :: undef                 ! Undef value set up according to original grib files
  REAL               :: field_val        
  REAL               :: tmax,tmin,hmax,hmin,pmax,pmin,wmax,wmin        
  REAL, DIMENSION(4) :: tshdprc

  REAL, ALLOCATABLE, DIMENSION(:)        :: fldmin_var,fldmax_var,aux_var,aux_uves,aux_vves
  REAL, ALLOCATABLE, DIMENSION(:,:)      :: prob_var,determ_var
  REAL, ALLOCATABLE, DIMENSION(:,:)      :: fldmin,fldmax
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: field

  CHARACTER(LEN=10)  :: datei                 ! initial date
  CHARACTER(LEN=200) :: dirinp                ! input directory where is the files of the ensemble members        
  CHARACTER(LEN=200) :: dirout                ! output directory for probabilities
  CHARACTER(LEN=200) :: plmsetup              ! namelist file
  CHARACTER(LEN=200) :: resol                 ! horizontal and vertical model resolution
  CHARACTER(LEN=200) :: lstoutlst             ! list of outputs probabilities forecasting
  CHARACTER(LEN=200) :: prefx                 ! preffix for input and output files
  CHARACTER(LEN=200) :: line,lstoutloc,auxstr,auxstr2               
  CHARACTER(LEN=10),  ALLOCATABLE, DIMENSION(:)   :: fctdate
  CHARACTER(LEN=200), ALLOCATABLE, DIMENSION(:)   :: lstoutctl,lstoutgrb
  CHARACTER(LEN=200), ALLOCATABLE, DIMENSION(:)   :: lstinctl,lstingrb       ! name of input ctl's and bin's
  CHARACTER(LEN=200), ALLOCATABLE, DIMENSION(:)   :: identify,lstlocaliz,localiz
  CHARACTER(LEN=200), ALLOCATABLE, DIMENSION(:,:) :: vars,identif

  !
  ! Attributions
  !

  CALL GETARG(1,datei)
  WRITE(plmsetup,'(3A)')'plmsetup.',datei,'.nml'

  !
  ! Call attribution function
  !

  OPEN(10,FILE=TRIM(plmsetup),STATUS='OLD',FORM='FORMATTED')

  CALL attribute(undef,imax,jmax,nmembers,nfctdy,nfcthours,freqcalc,dirinp,dirout,resol,prefx)
  jmaxhf=jmax/2

  CLOSE(10)

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
  nhours=nfctdy*24 !number of hours in all integration period
  
  !
  !  Make dynamic allocation of memory 
  !

  ALLOCATE(fcthours(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fcthours'
  ALLOCATE(fctdate(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fctdate'
  ALLOCATE(lstinctl(nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstoutctl'
  ALLOCATE(lstingrb(nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstoutgrb'
  ALLOCATE(identify(nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: identify'
  ALLOCATE(nvars(nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: nvars'
  ALLOCATE(nbgh(nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: nbgh'
  ALLOCATE(lstlocaliz(nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstlocaliz'

  ! CALL CreateGaussRep(jmax,jmaxhf) ! Obtain gaussian latitudes
  
  !
  ! Generate the list of forecast output dates and hours
  !

  CALL caldate(datei,ngrbs,freqcalc,fctdate,fcthours)

  !
  ! Generate the list of forecast files
  ! Generate the name of the output files
  !

  CALL getlstgrb(datei,fctdate,ngrbs,nmembers,prefx,resol, &
                 lstinctl,lstingrb,identify,lstlocaliz)

  ! Obtain the number of vars and the number of 
  ! of geographical points at each post grh file member
  DO nmb=1,nmembers 

     WRITE(*,*)'UNIT95=',TRIM(dirinp)//TRIM(lstinctl(nmb))
     OPEN(95,FILE=TRIM(dirinp)//TRIM(lstinctl(nmb)),STATUS='UNKNOWN',FORM='FORMATTED') 

     WRITE(*,*)'UNIT96=',TRIM(dirinp)//TRIM(identify(nmb))
     OPEN(96,FILE=TRIM(dirinp)//TRIM(identify(nmb)),STATUS='UNKNOWN',FORM='FORMATTED') 

     ierr=0
     DO WHILE (ierr .EQ. 0)   !Find the number of vars at each post grh file member
       READ(95,'(A)', IOSTAT=ierr) line
         CALL Ucase(line(1:4))
         IF( line(1:4) .EQ. 'VARS' ) THEN
       READ(line(5:200),'(I196)')  nvars(nmb)
       WRITE(*,*)'nvars(',nmb,')=',nvars(nmb)
         ierr=1
       END IF
     END DO

     READ(96,'(I7)') nbgh(nmb) !Read the number of geographical points of each post grh file member
     WRITE(*,*)'nbgh(',nmb,')=',nbgh(nmb)

     CLOSE(95)
     CLOSE(96)

  END DO

  DO nmb=2,nmembers  

    !Verify if the number of vars are equal for all members 
    IF (nvars(nmb) .NE. nvars(1)) THEN
      WRITE(*,*)'nvars(',nmb,')=',nvars(nmb),'nvars(1)=',nvars(1)
      WRITE(*,*) 'MAIN PROGRAM: The number of nvars for all members are inconsistent'
      STOP 
    END IF

    !Verify if the number of geographical points are equal for all members 
    IF (nbgh(nmb) .NE. nbgh(1)) THEN
      WRITE(*,*)'nbgh(',nmb,')=',nbgh(nmb),'nbgh(1)=',nbgh(1)
      WRITE(*,*) 'MAIN PROGRAM: The number of grid-history points for all members are inconsistent'
      STOP 
    END IF
  END DO

  nvarsp1=nvars(1)+1
  nvarsp2=nvars(1)+2

  !Allocate additional variables 
  ALLOCATE(lstoutctl(nvarsp2), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstoutctl'
  ALLOCATE(lstoutgrb(nvarsp2), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstoutgrb'
  ALLOCATE(fldmin_var(nvarsp1), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fldmin_var'
  ALLOCATE(fldmax_var(nvarsp1), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fldmax_var'
  ALLOCATE(localiz(nbgh(1)), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: localiz'
  ALLOCATE(aux_uves(nbgh(1)), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: aux_uves'
  ALLOCATE(aux_vves(nbgh(1)), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: aux_vves'
  ALLOCATE(vars(nmembers,nvarsp1), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: vars'
  ALLOCATE(rec_vars(nmembers,nvarsp1), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: rec_vars'
  ALLOCATE(identif(nmembers,nbgh(1)), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: identif'
  ALLOCATE(pos_ident(nmembers,nbgh(1)), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: pos_ident'
  ALLOCATE(fldmin(nvarsp1,nbgh(1)), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fldmin'
  ALLOCATE(fldmax(nvarsp1,nbgh(1)), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fldmax'
  ALLOCATE(field(nmembers,nvarsp1,nfcthours,nbgh(1)), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: field'

  !Obtain the vars and the geographical points of each post grh file member
  DO nmb=1,nmembers   

    WRITE(*,*)'UNIT95=',TRIM(dirinp)//TRIM(lstinctl(nmb))
    OPEN(95,FILE=TRIM(dirinp)//TRIM(lstinctl(nmb)),STATUS='UNKNOWN',FORM='FORMATTED') 

    WRITE(*,*)'UNIT96=',TRIM(dirinp)//TRIM(identify(nmb))
    OPEN(96,FILE=TRIM(dirinp)//TRIM(identify(nmb)),STATUS='UNKNOWN',FORM='FORMATTED') 

    ierr=0
    DO WHILE (ierr .EQ. 0)   
      READ(95,'(A)', IOSTAT=ierr) line
        CALL Ucase(line(1:4))
        IF( line(1:4) .EQ. 'VARS' ) THEN
            ierr=1
      END IF
    END DO

    DO nfd=1,nvars(nmb)   !Find the vars of each post grh file member
      READ(95,'(A)') line
      idv=INDEX(line,' ')-1
      CALL Ucase(line(1:idv))
      vars(nmb,nfd)=line(1:idv)
      WRITE(*,*)'vars(',nmb,',',nfd,')=',vars(nmb,nfd)(1:idv)
    END DO

    nfd=nvarsp1
    vars(nmb,nfd)='VSUT'

    WRITE(*,*)'vars(',nmb,',',nfd,')=',vars(nmb,nfd)(1:idv)

    READ(96,'(A40)') line
    DO ngh=1,nbgh(nmb)   !Read the name of geographical points of each post grh file member
      READ(96,'(A40)') identif(nmb,ngh)
    END DO

    CLOSE(95)
    CLOSE(96)

  END DO

  WRITE(*,*)'UNIT97=',TRIM(dirinp)//TRIM(lstlocaliz(1))  !Read the localization (lat/lon) of geographical points 
  OPEN(97,FILE=TRIM(dirinp)//TRIM(lstlocaliz(1)),STATUS='UNKNOWN',FORM='FORMATTED') 
  READ(97,'(A40)') line

  DO ngh=1,nbgh(1)   
    READ(97,'(A)') localiz(ngh)
  END DO

  CLOSE(97)

  !Verify if all vars are available at all members and the correspondent record 
  WRITE(*,'(/,A)')'Records of each field at each ensemble member:'
  WRITE(*,'(A6,100(x,A5))')'Member',vars(1,1:nvarsp1)
  rec_vars(:,:)=0   
  DO nfd=1,nvarsp1   ! Find the record of each field at each ensemble member
     rec_vars(1,nfd)=nfd
  END DO
  WRITE(*,'(I6,100(I4,2x))') 1,rec_vars(1,1:nvarsp1)
  DO nmb=2,nmembers
     count=0
     DO nfd=1,nvarsp1
       DO nfd2=nfd,nvarsp1
         idv=INDEX(vars(nmb,nfd2),' ')-1
         IF (vars(nmb,nfd2)(1:idv) .EQ. vars(1,nfd)(1:idv)) THEN
           rec_vars(nmb,nfd)=nfd2
           count=count+1
           EXIT
         END IF
       END DO
     END DO
     WRITE(*,'(I6,100(I4,2x))') nmb,rec_vars(nmb,1:nvarsp1)
     IF (count .NE. nvarsp1) THEN
       WRITE(*,*)'GETLSTGRB: The number of corresponding vars of this member is inconsitent'
       WRITE(*,*)'Some Var of Member: 1 was not found at Member:',nmb
       STOP 
     END IF
  END DO

  !Verify if all geographical points are available at all members and the correspondent record 
  pos_ident(:,:)=0   

  DO ngh=1,nbgh(1)   ! Find the position of each geog. point at each ensemble member
    pos_ident(1,ngh)=ngh
  END DO

  DO nmb=2,nmembers
    count=0
    DO ngh=1,nbgh(1)
      DO ngh2=ngh,nbgh(nmb)
        IF (identif(nmb,ngh2)(1:40) .EQ. identif(1,ngh)(1:40)) THEN
          pos_ident(nmb,ngh)=ngh2
          count=count+1
          EXIT
       END IF
      END DO
    END DO
    IF (count .NE. nbgh(1)) THEN
      WRITE(*,*)'GETLSTGRB: The number of corresponding vars of this member is inconsitent'
      WRITE(*,*)'Some geographical point of Member: 1 was not found at Member:',nmb
      STOP 
    END IF
  END DO

  !
  ! Create the list of output files
  !

  WRITE(*,*)' '
  DO nfd=1,nvars(1)
    WRITE(lstoutctl(nfd),'(6A)')TRIM(vars(1,nfd)),datei(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol),'.ctl'
    WRITE(lstoutgrb(nfd),'(6A)')TRIM(vars(1,nfd)),datei(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol),'.bin'
    WRITE(*,*)'lstoutctl(',nfd,')=',TRIM(lstoutctl(nfd))
    WRITE(*,*)'lstoutgrb(',nfd,')=',TRIM(lstoutgrb(nfd))
  END DO

  nfd=nvarsp1
  WRITE(lstoutctl(nfd),'(6A)')TRIM(vars(1,nfd)),datei(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol),'.ctl'
  WRITE(lstoutgrb(nfd),'(6A)')TRIM(vars(1,nfd)),datei(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol),'.bin'
  WRITE(*,*)'lstoutctl(',nfd,')=',TRIM(lstoutctl(nfd))
  WRITE(*,*)'lstoutgrb(',nfd,')=',TRIM(lstoutgrb(nfd))
  nfd=nvarsp2
  WRITE(lstoutctl(nfd),'(6A)')'CONT',datei(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol),'.ctl'
  WRITE(lstoutgrb(nfd),'(6A)')'CONT',datei(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol),'.bin'
  WRITE(*,*)'lstoutctl(',nfd,')=',TRIM(lstoutctl(nfd))
  WRITE(*,*)'lstoutgrb(',nfd,')=',TRIM(lstoutgrb(nfd))

  WRITE(lstoutloc,'(5A)')'LOCMM',datei(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol)
  WRITE(*,*)'lstoutloc=',TRIM(lstoutloc)

  WRITE(lstoutlst,'(6A)')'PLUMES',datei(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol),'.lst'
  WRITE(*,*)'lstoutlst=',TRIM(lstoutlst)

  !
  !  Read data, identify the position that each city is written in the binary file,   
  !  calculate the probabilities and generate the output files
  !

  DO nmb=1,nmembers  !Read the fields 

    WRITE(*,*)'FILE90: ',TRIM(dirinp)//TRIM(lstingrb(nmb))
    OPEN(90,FILE=TRIM(dirinp)//TRIM(lstingrb(nmb)),STATUS='OLD',FORM='UNFORMATTED',&
            ACTION='READ',ACCESS='SEQUENTIAL')
  
    DO ntm=1,nfcthours

      aux_uves(1:nbgh(nmb))=0.0
      aux_vves(1:nbgh(nmb))=0.0

      DO nfd=1,nvars(1)

        READ(90) field(nmb,nfd,ntm,1:nbgh(nmb))
          
        IF (TRIM(vars(nmb,nfd)) .EQ. 'UVES') THEN
          aux_uves(1:nbgh(nmb))=field(nmb,nfd,ntm,1:nbgh(nmb))
        END IF 

        IF (TRIM(vars(nmb,nfd)) .EQ. 'VVES') THEN
          aux_vves(1:nbgh(nmb))=field(nmb,nfd,ntm,1:nbgh(nmb))
        END IF 

      END DO

      nfd=nvarsp1

      DO ngh=1,nbgh(nmb)
        field(nmb,nfd,ntm,ngh)=SQRT( aux_uves(ngh)**2 + aux_vves(ngh)**2 )
      END DO

    END DO

    CLOSE(90)

  END DO

  DEALLOCATE(aux_uves,aux_vves, STAT=ierr)   ! deallocate aux_uves,vves
  IF (ierr .NE. 0) STOP 'NAO DEALOCOU CORRETAMENTE: aux_uves,vves'
 
  ! The list of output files
  WRITE(*,*)'unit105: ',TRIM(dirout)//TRIM(lstoutlst) 
  OPEN(105,FILE=TRIM(dirout)//TRIM(lstoutlst),STATUS='UNKNOWN',FORM='FORMATTED') 

  DO nfd=1,nvarsp1 !Find the min and max value of the field over all members and time
     DO ngh=1,nbgh(1) 
       fldmin(nfd,ngh)=MINVAL(field(1:nmembers,nfd,1:nfcthours,ngh))   
       fldmax(nfd,ngh)=MAXVAL(field(1:nmembers,nfd,1:nfcthours,ngh))   
     END DO        
  END DO

  DO nfd=1,nvarsp1 !Find the min and max value of the field over all geographical points
    fldmin_var(nfd)=MINVAL(fldmin(nfd,1:nbgh(1)))   
    fldmax_var(nfd)=MAXVAL(fldmax(nfd,1:nbgh(1)))   
  END DO

  DO nfd=1,nvarsp1 !Compute the probabilities or write the members without compute probabilites

    CALL SelectIntprobvar(TRIM(vars(1,nfd)),intprobvar) !Function to select intprobvar: intprobvar represents the interval to compute probabilities
                                                        !                              (If intprobvar != 0 compute probabilities)
                                                        !                              (If intprobvar == 0 does not compute probabilities)
                                                        !                              (If intprobvar  < 0 the var was not found in the prior-established list)

     IF (intprobvar .LT. 0) THEN
       WRITE(*,*)'MAIN PROGRAM: The var: ',TRIM(vars(1,nfd)),' was not found in the prior-established list'
       STOP
     END IF

     IF (intprobvar .NE. 0) THEN

       nintprob=CEILING((FLOAT(CEILING(fldmax_var(nfd)))-FLOAT(FLOOR(fldmin_var(nfd))))/FLOAT(intprobvar))               
       WRITE(*,*)'fldmax_var(',nfd,')=',fldmax_var(nfd),'fldmin_var(',nfd,')=',fldmin_var(nfd), & 
                 'intprobvar=',intprobvar,'nintprob=',nintprob                                           

       ALLOCATE(prob_var(nbgh(1),nintprob), STAT=ierr)   !allocate prob_var
       IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: prob_var'

       WRITE(105,'(A)') TRIM(dirout)//TRIM(lstoutctl(nfd))  
       WRITE(105,'(A)') TRIM(dirout)//TRIM(lstoutgrb(nfd))  

       WRITE(*,*)'unit90: ',TRIM(dirout)//TRIM(lstoutgrb(nfd))  !open files to write out the results
       OPEN(90,FILE=TRIM(dirout)//TRIM(lstoutgrb(nfd)),FORM='UNFORMATTED',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')

       WRITE(*,*)'unit95: ',TRIM(dirout)//TRIM(lstoutctl(nfd))
       OPEN(95,FILE=TRIM(dirout)//TRIM(lstoutctl(nfd)),STATUS='UNKNOWN',FORM='FORMATTED') 
       CALL CreateCtl(TRIM(vars(1,nfd)),'PROB',datei,nfcthours,nbgh(1),nintprob,fldmin_var(nfd), &
                     intprobvar,TRIM(lstoutgrb(nfd)),resol,undef,'PROBABILITIES (0..1)') 
       CLOSE(95)

       DO ntm=1,nfcthours     

         prob_var(:,:)=0.0   

         DO ngh=1,nbgh(1)
          
           DO nmb=1,nmembers   

             nfd2=rec_vars(nmb,nfd)
             ngh2=pos_ident(nmb,ngh)
        
             field_val=FLOOR(fldmin_var(nfd))

             DO int=1,nintprob   
               IF ((field(nmb,nfd2,ntm,ngh2) .NE. undef) .AND. (prob_var(ngh,int) .NE. undef)) THEN
                 IF ((field(nmb,nfd2,ntm,ngh2) .GE. field_val) .AND. (field(nmb,nfd2,ntm,ngh2) .LT. field_val+intprobvar)) THEN
                   prob_var(ngh,int)=prob_var(ngh,int)+1  
                 END IF
               ELSE
                 prob_var(ngh,int)=undef
             END IF
             field_val=field_val+intprobvar
           END DO 
         END DO
       END DO
           
       WHERE (prob_var(:,:) .NE. undef)
         prob_var(:,:)=prob_var(:,:)/FLOAT(nmembers) 
       END WHERE
         WRITE(90) prob_var   
     ENDDO 

     DEALLOCATE(prob_var, STAT=ierr)   ! deallocate prob_var
     IF (ierr .NE. 0) STOP 'NAO DEALOCOU CORRETAMENTE: prob_var'
        
     CLOSE(90)

   ELSE

     ALLOCATE(determ_var(nbgh(1),nmembers), STAT=ierr)   !allocate determ_var
     IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: aux_var'

       WRITE(105,'(A)') TRIM(dirout)//TRIM(lstoutctl(nfd))  
       WRITE(105,'(A)') TRIM(dirout)//TRIM(lstoutgrb(nfd))  

       WRITE(*,*)'unit90: ',TRIM(dirout)//TRIM(lstoutgrb(nfd))  !open files to write out the results
       OPEN(90,FILE=TRIM(dirout)//TRIM(lstoutgrb(nfd)),FORM='UNFORMATTED',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')

       WRITE(*,*)'unit95: ',TRIM(dirout)//TRIM(lstoutctl(nfd))
       OPEN(95,FILE=TRIM(dirout)//TRIM(lstoutctl(nfd)),STATUS='UNKNOWN',FORM='FORMATTED') 
       CALL CreateCtl(TRIM(vars(1,nfd)),TRIM(vars(1,nfd)),datei,nfcthours,nbgh(1),nmembers,1.0, &
                     1,TRIM(lstoutgrb(nfd)),resol,undef,'MEMBERS') 
       CLOSE(95)

       DO ntm=1,nfcthours

         determ_var(:,:)=undef   

         DO nmb=1,nmembers   

           DO ngh=1,nbgh(1)
          
             nfd2=rec_vars(nmb,nfd)
             ngh2=pos_ident(nmb,ngh)
             determ_var(ngh,nmb)=field(nmb,nfd2,ntm,ngh2)  

           END DO    
         END DO
          
         WRITE(90) determ_var
 
       END DO

       DEALLOCATE(determ_var, STAT=ierr)   ! deallocate determ_var
       IF (ierr .NE. 0) STOP 'NAO DEALOCOU CORRETAMENTE: determ_var'
       
       CLOSE(90)

     END IF

    ENDDO

  ! Create the control file
  ALLOCATE(aux_var(nbgh(1)), STAT=ierr)   !allocate aux_var
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: aux_var'

  nfd=nvarsp2
  WRITE(105,'(A)') TRIM(dirout)//TRIM(lstoutctl(nfd))  
  WRITE(105,'(A)') TRIM(dirout)//TRIM(lstoutgrb(nfd))  

  WRITE(*,*)'unit90: ',TRIM(dirout)//TRIM(lstoutgrb(nfd))  !open files to write out the results 	        
  OPEN(90,FILE=TRIM(dirout)//TRIM(lstoutgrb(nfd)),FORM='UNFORMATTED',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')      

  WRITE(*,*)'unit95: ',TRIM(dirout)//TRIM(lstoutctl(nfd))
  OPEN(95,FILE=TRIM(dirout)//TRIM(lstoutctl(nfd)),STATUS='UNKNOWN',FORM='FORMATTED')
  CALL CreateCtlCont(vars(nmembers,1:nvars(nmembers)),nvars(nmembers),datei,nfcthours,nbgh(nmembers), &
                     TRIM(lstoutgrb(nfd)),undef)
  CLOSE(95)

  DO ntm=1,nfcthours
     DO nfd=1,nvars(nmembers)
        DO ngh=1,nbgh(1)
           ngh2=pos_ident(nmembers,ngh)
           aux_var(ngh)=field(nmembers,nfd,ntm,ngh2)        
        END DO     
        WRITE(90) aux_var
     END DO        
  END DO        

  DEALLOCATE(aux_var, STAT=ierr)   ! deallocate aux_var
  IF (ierr .NE. 0) STOP 'NAO DEALOCOU CORRETAMENTE: aux_var'
  CLOSE(90)

  ! Create the file with informations about fields at each geographical point
  WRITE(105,'(A)') TRIM(dirout)//TRIM(lstoutloc)  

  WRITE(*,*)'unit95: ',TRIM(dirout)//TRIM(lstoutloc)
  OPEN(95,FILE=TRIM(dirout)//TRIM(lstoutloc),STATUS='UNKNOWN',FORM='FORMATTED')

  auxstr =' NUM                               GRID POINT    POSITION'
  auxstr2='    T_max    T_min    H_max    H_min    P_max    P_min    W_max    W_min'

  WRITE(95,'(I7)') nbgh(1)
  WRITE(95,'(A)') TRIM(auxstr)//TRIM(auxstr2)

  DO ngh=1,nbgh(1)
    
    DO nfd=1,nvarsp1
      IF (TRIM(vars(1,nfd)) .EQ. 'TEMS') THEN
        tmin=fldmin(nfd,ngh)
        tmax=fldmax(nfd,ngh)
      END IF
      IF (TRIM(vars(1,nfd)) .EQ. 'UMRS') THEN
        hmin=fldmin(nfd,ngh)
        hmax=fldmax(nfd,ngh)
      END IF     
      IF (TRIM(vars(1,nfd)) .EQ. 'PSLC') THEN
        pmin=fldmin(nfd,ngh)
        pmax=fldmax(nfd,ngh)
      END IF
      IF (TRIM(vars(1,nfd)) .EQ. 'VSUT') THEN
        wmin=fldmin(nfd,ngh)
        wmax=fldmax(nfd,ngh)
      END IF
    END DO

    WRITE(95,'(I4,X,A40,X,A11,8(X,F8.2))') ngh,identif(1,ngh)(1:40),localiz(ngh)(1:11), &
                                           tmax,tmin,hmax,hmin,pmax,pmin,wmax,wmin
  END DO

  CLOSE(95)
  CLOSE(105)

  END PROGRAM plumes

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to set up basic variables
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE attribute(undef,imax,jmax,nmembers,nfctdy,nfcthours,freqcalc,dirinp,dirout,resol,prefx) 

  IMPLICIT NONE

  INTEGER,           INTENT(OUT)  :: imax,jmax,nmembers,nfctdy,freqcalc,nfcthours
  REAL,              INTENT(OUT)  :: undef
  CHARACTER (LEN=*), INTENT(OUT)  :: dirinp,dirout,resol,prefx

  CHARACTER (LEN=200) :: line,line2
  INTEGER             :: ierr

  ierr=0      ! To start the loop

  DO WHILE (ierr .EQ. 0)

    READ(10,'(A14,A)', IOSTAT=ierr) line,line2
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
    ELSE IF( line(1:10).EQ.'NFCTHOURS ' ) THEN 
      READ(line2(1:7),'(I7)')  nfcthours
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

  SUBROUTINE CreateCtl(vars,vars2,fctdate,nfcthours,nbgh,nypts,fldmin_var,intprobvar,loutbin,resol,undef,description) 

  IMPLICIT NONE

  INTEGER,               INTENT(IN) :: nfcthours,nbgh,nypts,intprobvar
  REAL,                  INTENT(IN) :: undef,fldmin_var
  CHARACTER(LEN=*),      INTENT(IN) :: vars,vars2,fctdate,loutbin,resol,description

  INTEGER                          :: iday,imonth,iyear,iutc
  CHARACTER(LEN= 3), DIMENSION(12) :: mon

  DATA mon/'JAN','FEB','MAR','APR','MAY','JUN',  &
           'JUL','AUG','SEP','OCT','NOV','DEC'/

  !informations to tdef
  READ(fctdate(1: 4),'(I4)') iyear    
  READ(fctdate(5: 6),'(I2)') imonth
  READ(fctdate(7: 8),'(I2)') iday
  READ(fctdate(9:10),'(I2)') iutc
  
  WRITE(95,'(A)')'DSET ^'//TRIM(loutbin)
  WRITE(95,'(A)')'*'
  WRITE(95,'(A)')'OPTIONS SEQUENTIAL BIG_ENDIAN'
  WRITE(95,'(A)')'*'
  WRITE(95,'(A,1PE10.3)')'UNDEF ',undef 
  WRITE(95,'(A)')'*'
  WRITE(95,'(A)')'TITLE PROBABILITIES FROM ENS CPTEC FOR '//TRIM(vars)//' '//TRIM(resol)//'  COLD'     
  WRITE(95,'(A)')'*'
  WRITE(95,'(A,I7,A)')'XDEF  ',nbgh,'  LINEAR  1  1'
  WRITE(95,'(A,I7,A,F10.3,X,I4)')'YDEF  ',nypts,'  LINEAR ',fldmin_var,intprobvar
  WRITE(95,'(A)')'ZDEF        1  LEVELS 1000'
  WRITE(95,'(A,I5,A,I2.2,A,I2.2,A,I4.4,2X,A)') 'TDEF   ',nfcthours,' LINEAR ',iutc+1,'Z',iday,  &
                                                mon(imonth),iyear,'1HR'
  WRITE(95,'(A)')'*'
  WRITE(95,'(A)')'VARS  1'
  WRITE(95,'(A)')TRIM(vars2)//'   0 99 '//TRIM(description)
  WRITE(95,'(A)')'ENDVARS'
  
  END SUBROUTINE CreateCtl
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to create the ctl of the control file
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE CreateCtlCont(vars,nvars,fctdate,nfcthours,nbgh,loutbin,undef) 

  IMPLICIT NONE

  INTEGER,               INTENT(IN) :: nfcthours,nbgh,nvars
  REAL,                  INTENT(IN) :: undef
  CHARACTER(LEN=*),      INTENT(IN) :: fctdate,loutbin
  CHARACTER(LEN=*), DIMENSION(nvars), INTENT(IN) :: vars


  INTEGER                          :: nfd,iday,imonth,iyear,iutc
  CHARACTER(LEN= 3), DIMENSION(12) :: mon

  DATA mon/'JAN','FEB','MAR','APR','MAY','JUN',  &
           'JUL','AUG','SEP','OCT','NOV','DEC'/

  !informations to tdef
  READ(fctdate(1: 4),'(I4)') iyear    
  READ(fctdate(5: 6),'(I2)') imonth
  READ(fctdate(7: 8),'(I2)') iday
  READ(fctdate(9:10),'(I2)') iutc
  
  WRITE(95,'(A)')'DSET ^'//TRIM(loutbin)
  WRITE(95,'(A)')'*'
  WRITE(95,'(A)')'OPTIONS SEQUENTIAL BIG_ENDIAN'
  WRITE(95,'(A)')'*'
  WRITE(95,'(A,1PE10.3)')'UNDEF ',undef 
  WRITE(95,'(A)')'*'
  WRITE(95,'(A)')'TITLE GRID POINT HISTORY '     
  WRITE(95,'(A)')'*'
  WRITE(95,'(A,I7,A)')'XDEF  ',nbgh,'  LINEAR  1  1'
  WRITE(95,'(A)')     'YDEF        1  LINEAR  1  1'
  WRITE(95,'(A)')     'ZDEF        1  LEVELS 1000'
  WRITE(95,'(A,I5,A,I2.2,A,I2.2,A,I4.4,2X,A)') 'TDEF    ',nfcthours,' LINEAR ',iutc+1,'Z',iday,  &
                                                mon(imonth),iyear,'1HR'
  WRITE(95,'(A)')'*'
  WRITE(95,'(A,I4)')'VARS  ',nvars

  DO nfd=1,nvars
     IF( TRIM(vars(nfd)) .EQ. 'TOPO' ) THEN 
             WRITE(95,'(A)') 'TOPO  0 99 TOPOGRAPHY			   (M  	            )'
     ELSE IF( TRIM(vars(nfd)) .EQ. 'PSLC' ) THEN  
             WRITE(95,'(A)') 'PSLC  0 99 SURFACE PRESSURE		   (Mb 	            )'
     ELSE IF( TRIM(vars(nfd)) .EQ. 'PSNM' ) THEN 
             WRITE(95,'(A)') 'PSNM  0 99 SEA LEVEL PRESSURE		   (Mb 	    	    )'
     ELSE IF( TRIM(vars(nfd)) .EQ. 'PREC' ) THEN 
             WRITE(95,'(A)') 'PREC  0 99 TOTAL PRECIPITATION		   (Kg M**-2 Day**-1)'
     ELSE IF( TRIM(vars(nfd)) .EQ. 'CBNV' ) THEN 
             WRITE(95,'(A)') 'CBNV  0 99 CLOUD COVER 			   (%		    )'
     ELSE IF( TRIM(vars(nfd)) .EQ. 'UVES' ) THEN 
             WRITE(95,'(A)') 'UVES  0 99 SURFACE ZONAL WIND (U)		   (M/Sec	    )'
     ELSE IF( TRIM(vars(nfd)) .EQ. 'VVES' ) THEN 
             WRITE(95,'(A)') 'VVES  0 99 SURFACE MERIDIONAL WIND (V)  	   (M/Sec	    )'
     ELSE IF( TRIM(vars(nfd)) .EQ. 'TEMS' ) THEN 
             WRITE(95,'(A)') 'TEMS  0 99 SURFACE ABSOLUTE TEMPERATURE 	   (C		    )'
     ELSE IF( TRIM(vars(nfd)) .EQ. 'UMRS' ) THEN 
             WRITE(95,'(A)') 'UMRS  0 99 SURFACE RELATIVE HUMIDITY	   (%  	            )'
     ELSE IF( TRIM(vars(nfd)) .EQ. 'PNEV' ) THEN 
             WRITE(95,'(A)') 'PNEV  0 99 SNOW DEPTH			   (Kg M**-2	    )'
     ELSE IF( TRIM(vars(nfd)) .EQ. 'NEVE' ) THEN 
             WRITE(95,'(A)') 'NEVE  0 99 SNOWFALL			   (Kg M**-2 Sec**-1)'
     ELSE IF( TRIM(vars(nfd)) .EQ. 'TADL' ) THEN 
             WRITE(95,'(A)') 'TADL  0 99 TEMPERATURE OF CANOPY AIR SPACE   (C  	            )'
     ELSE IF( TRIM(vars(nfd)) .EQ. 'TP2M' ) THEN 
             WRITE(95,'(A)') 'TP2M  0 99 TEMP AT 2-M FROM SFC LAYER	   (K  	            )'
     ELSE IF(TRIM(VARS(NFD)) .EQ. 'CLLW') THEN
             WRITE(95,'(A)') 'CLLW  0 99 FRACTION OF CLOUDS FOR LOW              (NO DIM          )'
     ELSE IF(TRIM(VARS(NFD)) .EQ. 'CLMD') THEN
             WRITE(95,'(A)') 'CLMD  0 99 FRACTION OF CLOUDS FOR MEDIUM           (NO DIM          )'
     ELSE IF(TRIM(VARS(NFD)) .EQ. 'CLHI') THEN
             WRITE(95,'(A)') 'CLHI  0 99 FRACTION OF CLOUDS FOR HIGH             (NO DIM          )'
     ELSE IF(TRIM(VARS(NFD)) .EQ. 'PRCV') THEN
             WRITE(95,'(A)') 'PRCV  1 99 CONVECTIVE PRECIPITATION                (KG M**-2 SEC**-1)'
     ELSE 
             WRITE(*,'(A)') 'CreateCtlCont: Var: ',TRIM(vars(nfd)),'was not found in the prior-stablished list of vars'
     STOP
     END IF        
  END DO

  WRITE(95,'(A)')'ENDVARS'
  
  END SUBROUTINE CreateCtlCont
  
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

  rest=MOD(iyy,4) ! Decide if is a leap year
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
        rest=MOD(iyy,4) ! Decide if is a leap year
        IF (rest .EQ. 0) THEN
          lastd=monlb(imm)
        ELSE
          lastd=monl(imm)
        END IF
      END IF
    END IF

    WRITE(datep,'(I4.4,3I2.2)') iyy,imm,idd,ihh

    fctdate(i)(1:10)=datep(1:10)

    WRITE(*,'(A,I3,2A)') 'fctdate(',i,'):',fctdate(i)(1:10)
     
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
                       lstinctl,lstingrb,identify,lstlocaliz)
  
  IMPLICIT NONE
  
  INTEGER,                             INTENT(IN)  :: nmembers,ngrbs
  CHARACTER(len=10),                   INTENT(IN)  :: anldate
  CHARACTER(len=10), DIMENSION(ngrbs), INTENT(IN)  :: fctdate
  CHARACTER(LEN=*),                    INTENT(IN)  :: prefx,resol                

  CHARACTER(len=*),  DIMENSION(nmembers), INTENT(OUT)  :: lstinctl,lstingrb,identify,lstlocaliz

  !+++++Local variables

  INTEGER                :: nmb,membm

  ! fct files

  membm=(nmembers-1)/2

  WRITE(*,*) ''

  DO nmb=1,membm
    WRITE(lstinctl(nmb),'(A,I2.2,6A)')'GFGN',nmb,'P',anldate(1:10),fctdate(ngrbs)(1:10),'M.grh.',TRIM(resol),'.ctl'
    WRITE(lstingrb(nmb),'(A,I2.2,5A)')'GFGN',nmb,'P',anldate(1:10),fctdate(ngrbs)(1:10),'M.grh.',TRIM(resol)
!    WRITE(*,*)'lstingrb(',nmb,')=',TRIM(lstingrb(nmb))
    WRITE(identify(nmb),'(A,I2.2,5A)')'Identif',nmb,'P',anldate(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol)
    WRITE(*,*)'identify(',nmb,')=',TRIM(identify(nmb))
    WRITE(lstlocaliz(nmb),'(A,I2.2,5A)')'Localiz',nmb,'P',anldate(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol)
    WRITE(*,*)'lstlocaliz(',nmb,')=',TRIM(lstlocaliz(nmb))
  END DO

  DO nmb=1,membm
    WRITE(lstinctl(nmb+membm),'(A,I2.2,6A)')'GFGN',nmb,'N',anldate(1:10),fctdate(ngrbs)(1:10),'M.grh.',TRIM(resol),'.ctl'
    WRITE(lstingrb(nmb+membm),'(A,I2.2,5A)')'GFGN',nmb,'N',anldate(1:10),fctdate(ngrbs)(1:10),'M.grh.',TRIM(resol)
!    WRITE(*,*)'lstingrb(',nmb+membm,')=',TRIM(lstingrb(nmb+membm))
    WRITE(identify(nmb+membm),'(A,I2.2,5A)')'Identif',nmb,'P',anldate(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol)
    WRITE(*,*)'identify(',nmb+membm,')=',TRIM(identify(nmb+membm))
    WRITE(lstlocaliz(nmb+membm),'(A,I2.2,5A)')'Localiz',nmb,'P',anldate(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol)
    WRITE(*,*)'lstlocaliz(',nmb+membm,')=',TRIM(lstlocaliz(nmb+membm))
  END DO

  WRITE(lstinctl(nmembers),'(7A)')'GFGN',TRIM(prefx),anldate(1:10),fctdate(ngrbs)(1:10),'M.grh.',TRIM(resol),'.ctl'
  WRITE(lstingrb(nmembers),'(6A)')'GFGN',TRIM(prefx),anldate(1:10),fctdate(ngrbs)(1:10),'M.grh.',TRIM(resol)
!  WRITE(*,*)'lstingrb(',nmembers,')=',TRIM(lstingrb(nmembers))
  WRITE(identify(nmembers),'(6A)')'Identif',TRIM(prefx),anldate(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol)
  WRITE(*,*)'identify(',nmembers,')=',TRIM(identify(nmembers))
  WRITE(lstlocaliz(nmembers),'(6A)')'Localiz',TRIM(prefx),anldate(1:10),fctdate(ngrbs)(1:10),'.',TRIM(resol)
  WRITE(*,*)'lstlocaliz(',nmembers,')=',TRIM(lstlocaliz(nmembers))

  RETURN

  END SUBROUTINE getlstgrb 
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subrotine to shift a character string to uppercase
! From Book: Fortran 90/95 for Scientists and Engineers 
!    Author: Stephen J. Chapman   
!       Ed.: McGraw-Hill, First Edition, 1998.               
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE Ucase(string)

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

  END SUBROUTINE Ucase

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!
! Subroutine to select the interval for computing the probabilities
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  SUBROUTINE SelectIntprobvar(vars,intprobvar) 

  IMPLICIT NONE

  CHARACTER (LEN=*), INTENT(IN )  :: vars
  INTEGER,           INTENT(OUT)  :: intprobvar ! represent the interval for computing probabilities

  intprobvar=-1 !<0 Indicate that the var was not found

  IF( TRIM(vars) .EQ. 'TOPO' ) intprobvar=0 !=0 It does not compute probabilities
  IF( TRIM(vars) .EQ. 'PSNM' ) intprobvar=3
  IF( TRIM(vars) .EQ. 'PSLC' ) intprobvar=3
  IF( TRIM(vars) .EQ. 'PNEV' ) intprobvar=0 !=0 It does not compute probabilities
  IF( TRIM(vars) .EQ. 'NEVE' ) intprobvar=0 !=0 It does not compute probabilities
  IF( TRIM(vars) .EQ. 'PREC' ) intprobvar=0 !=0 It does not compute probabilities
  IF( TRIM(vars) .EQ. 'PRCV' ) intprobvar=0 ! CFB
  IF( TRIM(vars) .EQ. 'TADL' ) intprobvar=1
  IF( TRIM(vars) .EQ. 'CBNV' ) intprobvar=3
  IF( TRIM(vars) .EQ. 'UVES' ) intprobvar=2
  IF( TRIM(vars) .EQ. 'VVES' ) intprobvar=2
  IF( TRIM(vars) .EQ. 'TEMS' ) intprobvar=1
  IF( TRIM(vars) .EQ. 'UMRS' ) intprobvar=3
  IF( TRIM(vars) .EQ. 'TP2M' ) intprobvar=0 !=0 It does not compute probabilities
  IF( TRIM(vars) .EQ. 'VSUT' ) intprobvar=2 
  IF( TRIM(vars) .EQ. 'CLLW' ) intprobvar=0 ! CFB
  IF( TRIM(vars) .EQ. 'CLMD' ) intprobvar=0 ! CFB
  IF( TRIM(vars) .EQ. 'CLHI' ) intprobvar=0 ! CFB

  RETURN

  END SUBROUTINE SelectIntprobvar
