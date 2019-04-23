  PROGRAM cluster

  USE GaussRep,    ONLY : CreateGaussRep, glat
  USE ReadFields,  ONLY : ReadGrib

  IMPLICIT NONE

  !
  ! Program to evaluate the clusters from the global ensemble 
  ! weather forecasting. It is based on the Ward's minimum variance algorithm 
  ! Authors: Antonio Marcos Mendonca and Fabio Augusto Fernandes
  ! Any comentary, send a e-mail to: mendonca@cptec.inpe.br
  ! Last Atualization: 27/Sep/2007
  !

  !*******ETAi1: clusters for Eta model
  ! The parameter "grpeta" is related to amount
  ! of groups selected to Eta model. Currently grpeta=4

  INTEGER            :: grpeta,nclteta,nmembeta,labeleta,labelcltarq
  CHARACTER(LEN=10)  :: dateeta 
  !*******ETAf1
  
  INTEGER :: i,j,k,m,n,ni,nj,m1,nbs,ierr
  INTEGER :: ngroup,stp,stpb,nmb,lm,p1,p2,rest
  INTEGER :: imax,jmax,jmaxhf,nmembers,mon,nfctdy,freqcalc,noutpday
  INTEGER :: ncltant,ngrbs
  INTEGER :: tmemb,memb,psolim,nclt,pm,rmshour
  INTEGER :: iwest,ieast,jsout,jnort,nxpts,nypts,statfctrd
  INTEGER, ALLOCATABLE, DIMENSION(:)     :: nmemb,fcthours
  INTEGER, ALLOCATABLE, DIMENSION(:,:)   :: nmgrp,lmax
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: mbgrp

  REAL :: sumweight,waux
  REAL :: lonw,lone,lats,latn
  REAL,              DIMENSION(12)      :: rmsp
  REAL, ALLOCATABLE, DIMENSION(:)       :: rmslim,dmax,wmin 
  REAL, ALLOCATABLE, DIMENSION(:)       :: merge1,merge2,latweight,latwght
  REAL, ALLOCATABLE, DIMENSION(:,:)     :: var1,w
  REAL, ALLOCATABLE, DIMENSION(:,:,:)   :: z500,xmedm,dist
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: xmed
 
  CHARACTER(LEN=2)   :: hh                     ! hour UTC of the initial condition
  CHARACTER(LEN=10)  :: datei,datef            ! initial and final dates
  CHARACTER(LEN=200) :: clustersetup           ! namelist file
  CHARACTER(LEN=200) :: prefx                  ! preffix for input and output files
  CHARACTER(LEN=200) :: dirinp,dirout,dirrms,dirclt
  CHARACTER(LEN=200) :: resol,rmsclim,straux
  CHARACTER(LEN=200) :: cltarqeta
  CHARACTER(LEN=10) , ALLOCATABLE, DIMENSION(:)   :: fctdate
  CHARACTER(LEN=200), ALLOCATABLE, DIMENSION(:)   :: cltarq          ! output cluster file: nb. of clusters and list of members for each cluster
  CHARACTER(LEN=200), ALLOCATABLE, DIMENSION(:)   :: lstfiles        ! lists of ensemble members: bin's and ctl's
  CHARACTER(LEN=200), ALLOCATABLE, DIMENSION(:,:) :: lstctl,lstbin   ! name of input ctl's and bin's

  LOGICAL :: exarq1

  !
  ! Attributions
  !

  CALL GETARG(1,datei)
  CALL GETARG(2,dateeta)
  WRITE(clustersetup,'(3A)')'clustersetup.',datei,'.nml'

  CALL attribute(clustersetup,imax,jmax,nmembers,nfctdy,grpeta,freqcalc, &
                 lonw,lone,lats,latn,dirinp,dirrms,dirout,dirclt,resol,prefx) 

  WRITE(*,*)'imax    : ',imax
  WRITE(*,*)'jmax    : ',jmax
  WRITE(*,*)'nmembers: ',nmembers
  WRITE(*,*)'nfctdy  : ',nfctdy
  WRITE(*,*)'grpeta  : ',grpeta
  WRITE(*,*)'freqcalc: ',freqcalc
  WRITE(*,*)'lonw    : ',lonw
  WRITE(*,*)'lone    : ',lone
  WRITE(*,*)'lats    : ',lats
  WRITE(*,*)'latn    : ',latn
  WRITE(*,*)'dirinp  : ',TRIM(dirinp)
  WRITE(*,*)'dirrms  : ',TRIM(dirrms)
  WRITE(*,*)'dirout  : ',TRIM(dirout)
  WRITE(*,*)'dirclt  : ',TRIM(dirclt)
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

  ALLOCATE(cltarq(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: cltarq'

  ALLOCATE(rmslim(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: rmslim'
  
  ALLOCATE(lstfiles(nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstfiles'

  ALLOCATE(fctdate(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fctdate'

  ALLOCATE(fcthours(ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: fcthours'

  ALLOCATE(merge1(nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: merge1'
  
  ALLOCATE(merge2(nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: merge2'
  
  ALLOCATE(dmax(nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: dmax'
  
  ALLOCATE(wmin(nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: wmin'

  ALLOCATE(nmemb(nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: nmemb'
  
  ALLOCATE(latweight(jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: latweight'
  
  ALLOCATE(latwght(nypts), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: latwght'
  
  ALLOCATE(var1(imax,jmax), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: var1'
  
  ALLOCATE(lstbin(nmembers,ngrbs), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lstbin'

  ALLOCATE(lmax(nmembers,nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: lmax'
  
  ALLOCATE(w(nmembers,nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: w'
  
  ALLOCATE(nmgrp(nmembers,nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: nmgrp'

  ALLOCATE(dist(nmembers,nmembers,nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: dist'
  
  ALLOCATE(mbgrp(nmembers,nmembers,nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: mbgrp'

  ALLOCATE(z500(nxpts,nypts,nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: z500'
  
  ALLOCATE(xmedm(nxpts,nypts,nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: xmedm'
  
  ALLOCATE(xmed(nxpts,nypts,nmembers,nmembers), STAT=ierr)
  IF (ierr .NE. 0) STOP 'NAO ALOCOU CORRETAMENTE: xmed'
  

  !
  ! Generate the list of forecast output dates and hours
  !

  CALL caldate(datei,ngrbs,freqcalc,fctdate,fcthours)


  !
  ! Open the climatological model rms and obtain the tresholds values
  !

  WRITE(*,*) ' '
  hh(1:2)=datei(9:10)
  WRITE(rmsclim,'(5A)')'MonthlyMeanRmsGCM',TRIM(resol),'_',TRIM(hh),'Z'

  WRITE(*,*) 'unit50 :',TRIM(dirrms)//TRIM(rmsclim)
  OPEN(50,FILE=TRIM(dirrms)//TRIM(rmsclim),STATUS='OLD', &
          FORM='FORMATTED',ACTION='READ')
  
  READ(50,'(A)') straux
!  READ(50,'(I8,12F8.2)',IOSTAT=ierr) rmshour, rmsp

  READ(datei(5:6),'(I2)') mon
  
  ierr=0
  m=1
  DO 
     READ(50,'(I8,12F8.2)',IOSTAT=ierr) rmshour, rmsp
     IF (ierr .NE. 0) EXIT

!     IF (rmshour .EQ. fcthours(m)) THEN
      rmslim(m)=rmsp(mon)
      WRITE(*,*) 'rmshour:',rmshour,' fcthours(',m,'):',fcthours(m),'rmslim(',m,'):',rmslim(m)
      m=m+1
!     END IF
  END DO
  
  !
  ! Obtain the latitudinal weights and the sum of weights on the area
  !

  CALL lweights(imax,jmax,glat,latweight)

  latwght(1:nypts)=latweight(jnort:jsout)

  sumweight=0.0
  IF (iwest .LE. ieast) THEN
     DO i=iwest,ieast
        DO j=1,nypts
           sumweight=sumweight+latwght(j)
        END DO  	
      END DO  	
  ELSE
     DO i=iwest,imax		    !Weight of the west part 
        DO j=1,nypts
           sumweight=sumweight+latwght(j)
        END DO
     END DO
     DO i=1,ieast 		    !Weight of the east part
        DO j=1,nypts
           sumweight=sumweight+latwght(j)
        END DO
     END DO
  END IF

  WRITE(*,*)' '
  WRITE(*,*)'sumweight= ',sumweight


  !
  ! Generate the list of forecast files
  ! Generate the name of the output clusters list
  !


  CALL getlstgrb(datei,dateeta,fctdate,ngrbs,nmembers,prefx,resol, &
                 lstbin,cltarq,cltarqeta)

  !
  ! Loop to evaluate the clusters for all range of outputs
  !

  ncltant=0
  DO nbs=1,ngrbs !++++++++++Begin Main Loop

     IF ( nbs.EQ.1 .OR. nbs.EQ.2 ) THEN
     

     	! Write on the cluster output file the number of clusters and the members of each cluster 
     	! For nbs=1,2 there are 1 cluster containing all ensemble members 

     	OPEN(50,FILE=TRIM(dirclt)//TRIM(cltarq(nbs)),STATUS='UNKNOWN', FORM='FORMATTED')

        nclt=1
     	WRITE(50,'(A)')  'GLOBAL ENSEMBLE CLUSTERING'
     	WRITE(50,'(4A)') 'FORECAST FROM: ',datei,'     VALID FOR: ',fctdate(nbs)
     	WRITE(50,'(A,I3)') 'no. of clusters: ',nclt

     	DO m=1, nclt
     	   nmemb(m)=nmembers
     	   WRITE(50,'(A,I3.3,A,I3)') 'n.o of members clt(',m,'): ',nmemb(m)
     	   DO n=1, nmemb(m)
     	      WRITE(50,'(A,I3)')'member: ',n
     	   END DO
     	END DO
     	CLOSE(50)
     
     ELSE

     	!
     	! Read 500 hPa geopotential height forecast to evaluate the clusters
     	!

     	WRITE(*,'(/,A,/)') '*****************************'
     	WRITE(*,'(A,I3,A)')	 'evaluating clusters for forecast hour :',fcthours(nbs),' hs'
     	WRITE(*,'(/,A,/)') '*****************************'

     	Z500(:,:,:)=0.0
     	DO m=1,nmembers 
     	   INQUIRE(FILE=TRIM(dirinp)//TRIM(lstbin(m,nbs)),EXIST=exarq1)
     	   IF (exarq1) THEN

     	      statfctrd=0
 	      CALL ReadGrib(TRIM(dirinp)//TRIM(lstbin(m,nbs)),imax,jmax,var1,statfctrd)
     	      IF (statfctrd .NE. 0) STOP 'Main Program: It does not read grib correctly'

     	      IF (iwest .LE. ieast) THEN
     		z500(1:nxpts,1:nypts,m)=var1(iwest:ieast,jnort:jsout)
     	      ELSE
     		ni=0
     		DO i=iwest,imax 		 !z500 receive the west part of field
     		   ni=ni+1
     		   nj=0
     		   DO j=jnort,jsout
     		      nj=nj+1
     		      z500(ni,nj,m)=var1(i,j)
     		   END DO
     		END DO
     		DO i=1,ieast			 !z500 receive the east part of field
     		   ni=ni+1
     		   nj=0
     		   DO j=jnort,jsout
     		      nj=nj+1
     		      z500(ni,nj,m)=var1(i,j)
     		   END DO
     		END DO
     		WRITE(*,*) 'ni= ',ni
     		WRITE(*,*) 'nj= ',nj
     	      END IF
     	   ELSE
     	      IF (.NOT.(exarq1)) THEN
     		 WRITE(*,*) 'THE FORECAST FILE DOES NOT EXIST: ',TRIM(dirinp)//TRIM(lstbin(m,nbs))
     		 STOP
     	      END IF 
     	   END IF
     	END DO

     	!
     	! Obain the clusters
     	!

     	! Step 1: initialization
  
     	dmax(:)=0.0
     	ngroup=nmembers
     	stp=1
     	nmb=1
     	nmgrp(i,stp)=0
     	mbgrp(i,stp,nmb)=0
     	DO i=1,ngroup
     	    nmgrp(i,stp)=1
     	    mbgrp(i,stp,nmb)=i
     	END DO
  
     	!amm WRITE(*,*) ' '
     	!amm WRITE(*,*)'clusters of the step 1:'
     	!amm DO i=1,ngroup
     	!amm	 nmb=1
     	!amm	 DO WHILE (nmb.LE.nmgrp(i,stp))
     	!amm	      WRITE(*,*) 'mbgrp(',i,',',nmb,',',stp,')=',mbgrp(i,nmb,stp)
     	!amm	     nmb=nmb+1
     	!amm	 END DO
     	!amm END DO 
  
     	! Loop to group all members up to rest only one group

     	DO WHILE (ngroup > 1)

     	   stpb=stp
     	   stp =stp+1

     	   ! Mean of groups before grouping

     	   DO m=1, ngroup
     	      tmemb=nmgrp(m,stpb)
     	      xmedm(1:nxpts,1:nypts,m)=0.0
     	      DO nmb=1, nmgrp(m,stpb)
     		 memb=mbgrp(m,nmb,stpb)
     		 xmedm(1:nxpts,1:nypts,m)=xmedm(1:nxpts,1:nypts,m)+z500(1:nxpts,1:nypts,memb)
     	      END DO
     	      xmedm(1:nxpts,1:nypts,m)=xmedm(1:nxpts,1:nypts,m)/FLOAT(tmemb)
     	   END DO
  
     	   ! Mean after merging two groups
     	 
     	   DO m=1, ngroup-1
     	      DO n=m+1, ngroup
     		 tmemb=nmgrp(m,stpb)+nmgrp(n,stpb)
     		 xmed(1:nxpts,1:nypts,m,n)=0.0
     		 DO nmb=1, nmgrp(m,stpb)
     		    memb=mbgrp(m,nmb,stpb)
     		    xmed(1:nxpts,1:nypts,m,n)=xmed(1:nxpts,1:nypts,m,n)+z500(1:nxpts,1:nypts,memb)
     		 END DO
     		 DO nmb=1, nmgrp(n,stpb)
     		    memb=mbgrp(n,nmb,stpb)
     		    xmed(1:nxpts,1:nypts,m,n)=xmed(1:nxpts,1:nypts,m,n)+z500(1:nxpts,1:nypts,memb)
     		 END DO
     		 xmed(1:nxpts,1:nypts,m,n)=xmed(1:nxpts,1:nypts,m,n)/FLOAT(tmemb)
     	      END DO
     	   END DO

     	   ! Evaluate w to all possibilities of merging

     	   lmax(:,:)=0
     	   dist(:,:,:)=0.0
     	   wmin(stpb)=1.0e37
     	   dmax(stp)=-1.0e37
     	   DO m=1, ngroup-1
     	      DO n=m+1, ngroup

     		 w(m,n)=0.0
     		 DO nmb=1, nmgrp(m,stpb)
		    memb=mbgrp(m,nmb,stpb)
		    lmax(m,n)=lmax(m,n)+1
		    waux=0.0
		    DO i=1, nxpts
		       DO j=1, nypts
     			  waux= waux + ((z500(i,j,memb) - xmed(i,j,m,n))**2)*latwght(j)
			  dist(m,n,lmax(m,n))= dist(m,n,lmax(m,n)) + ((z500(i,j,memb) - xmed(i,j,m,n))**2)*latwght(j)
     		       END DO
     		    END DO
     		    dist(m,n,lmax(m,n))= SQRT(dist(m,n,lmax(m,n))/sumweight)
     		    waux= waux / sumweight
     		    w(m,n)= w(m,n) + waux
     		 END DO
     		 DO nmb=1, nmgrp(n,stpb)
     		    memb=mbgrp(n,nmb,stpb)
		    lmax(m,n)=lmax(m,n)+1
		    waux=0.0
		    DO i=1, nxpts
		       DO j=1, nypts
     			  waux= waux + ((z500(i,j,memb) - xmed(i,j,m,n))**2)*latwght(j)
			  dist(m,n,lmax(m,n))= dist(m,n,lmax(m,n)) + ((z500(i,j,memb) - xmed(i,j,m,n))**2)*latwght(j)
		       END DO
     		    END DO
     		    dist(m,n,lmax(m,n))= SQRT(dist(m,n,lmax(m,n))/sumweight)
     		    waux= waux / sumweight
     		    w(m,n)= w(m,n) + waux
     		 END DO

     		 DO k=1, ngroup
		    IF ( (k .NE. m) .AND. (k .NE. n) ) THEN
     		       DO nmb=1, nmgrp(k,stpb)
     			  memb=mbgrp(k,nmb,stpb)
			  waux=0.0
			  DO i=1, nxpts
			     DO j=1, nypts
     				waux= waux + ((z500(i,j,memb) - xmedm(i,j,k))**2)*latwght(j)
     			     END DO
     			  END DO
     			  waux= waux / sumweight
     			  w(m,n)= w(m,n) + waux
     		       END DO
		    END IF
     		 END DO
     	 
     		 ! Obtain the minimum w and the groups to be merged: merge1(stp) e merge2(stp)
     	 
     		 !amm WRITE(*,*)'w(',m,',',n,')=',w(m,n)
     		 IF (w(m,n) .LT. wmin(stpb)) THEN
     		    wmin(stpb)=w(m,n)
		    merge1(stpb)=m
		    merge2(stpb)=n
     		 END IF
     	 
     	      END DO
     	   END DO
  
     	   !amm WRITE(*,*)'W minimum =',wmin(stpb)
     	   !amm WRITE(*,*)'The groups to be merged are: (',merge1(stpb),',',merge2(stpb),')'

     	   ! Reorganize the groups separating the members of each group 

     	   p1=merge1(stpb)
     	   p2=merge2(stpb)
  
     	   ! First group on the step: "stp"

     	   nmgrp(1,stp)=nmgrp(p1,stpb)+nmgrp(p2,stpb)
     	   k=0
     	   DO m=1, nmgrp(p1,stpb)
     	      k=k+1
     	      mbgrp(1,k,stp)=mbgrp(p1,m,stpb)
     	   END DO
     	   DO m=1, nmgrp(p2,stpb)
     	      k=k+1
     	      mbgrp(1,k,stp)=mbgrp(p2,m,stpb)
     	   END DO
  
     	   ! Another groups on the step: "stp"

     	   k=1
     	   DO m=1, ngroup
     	      IF ((m .NE. p1) .AND. (m .NE. p2)) THEN
     		 k=k+1
     		 nmgrp(k,stp)=nmgrp(m,stpb)
     		 !amm WRITE(*,*) 'nmgrp(',k,',',stp,')=',nmgrp(k,stp)
     		 DO nmb=1, nmgrp(m,stpb)
     		    mbgrp(k,nmb,stp)=mbgrp(m,nmb,stpb)
     		    !amm WRITE(*,*) 'mbgrp(',k,',',nmb,',',stp,'=',mbgrp(k,nmb,stp)
     		 END DO
     	      END IF
     	   END DO

     	   ! Write on the output control file the members of each group

     	   !amm WRITE(*,*) 'Clusters of the step: ',stp
     	   !amm DO m=1, ngroup-1
     	   !amm    WRITE(*,*) 'nmgrp(',m,',',stp,')=',nmgrp(m,stp)
     	   !amm    DO nmb=1, nmgrp(m,stp)
     	   !amm       WRITE(*,*) 'mbgrp(',m,',',nmb,',',stp,')=',mbgrp(m,nmb,stp)
     	   !amm    END DO
     	   !amm END DO
 
     	   ! Obtain the member that is situated more distant of the average of the group: "p1+p2"

     	   DO lm=1, lmax(p1,p2)
     	      !amm WRITE(*,*)'dist(',p1,',',p2,',',lm,')=',dist(p1,p2,lm)
     	      IF(dist(p1,p2,lm) .GT. dmax(stp)) THEN
     		 dmax(stp)=dist(p1,p2,lm)
     	      END IF	
     	   END DO

     	   !amm WRITE(*,*)'The major distance from the average of the group on the step ',stp, 'is: ',dmax(stp)

     	   ngroup=ngroup-1

     	END DO

     	! It is applied the criterion to select the number of clusters
  
     	WRITE(*,*)'  '
     	WRITE(*,*) 'rmslim(',nbs,')= ',rmslim(nbs)

     	! Identify which step the major distance between the member and the average of the group 
     	! is greater than the distance "ERC/2 -> model climatology" 
     	! The maximum number of clusters is set to 5

     	m=1
     	DO WHILE (m .LE. nmembers)
     	   WRITE(*,*) 'dmax(',m,'):',dmax(m)
     	   m=m+1
     	END DO

     	psolim=nmembers
     	IF (dmax(1) .GT. rmslim(nbs)) THEN   ! The lesser distance is greater than the rms treshold 
     	   psolim=nmembers-4		     ! The maximum number of clusters is 5
     	ELSE
     	   m=2
     	   DO WHILE (m .LE. nmembers)
     	      m1=m-1
     	      IF ((dmax(m1) .LE. rmslim(nbs)) .AND. (dmax(m) .GT. rmslim(nbs))) THEN
     		 psolim=m1
     		 EXIT
     	      END IF
     	      m=m+1
     	   END DO    
     	END IF

     	IF (psolim .LE. (nmembers-5)) THEN    ! The maximum number of clusters is 5
     	   psolim=nmembers-4
     	END IF

     	nclt=nmembers-psolim+1   ! Number of clusters in the range: "nbs"
  
     	IF ( nclt .LT. ncltant ) THEN	! The number of clusters in the range "nbs" is not 
     	   nclt=ncltant 		! lesser than the number of clusters in the range "nbs-1"
     	   psolim=nmembers-nclt+1
     	END IF
  
     	WRITE(*,*) 'treshold step:',psolim
     	WRITE(*,*) 'nclt=',nclt

     	! Write on the cluster output file the number of clusters and the members of each cluster 

     	OPEN(50,FILE=TRIM(dirclt)//TRIM(cltarq(nbs)),STATUS='UNKNOWN', FORM='FORMATTED')

     	WRITE(50,'(A)')  'GLOBAL ENSEMBLE CLUSTERING'
     	WRITE(50,'(4A)') 'FORECAST FROM: ',datei,'     VALID FOR: ',fctdate(nbs)
     	WRITE(50,'(A,I3)') 'no. of clusters: ',nclt

     	pm=psolim
     	DO m=1, nclt
     	   nmemb(m)=nmgrp(m,pm)
     	   WRITE(50,'(A,I3.3,A,I3)') 'n.o of members clt(',m,'): ',nmemb(m)
     	   DO n=1, nmemb(m)
     	      WRITE(50,'(A,I3)')'member: ',mbgrp(m,n,pm)
     	   END DO
     	END DO
     	CLOSE(50)

     	!*******ETAi2
     	READ(dateeta,*) labeleta
     	READ(fctdate(nbs),*) labelcltarq
     	IF ( labeleta .EQ. labelcltarq ) THEN
     	   nclteta=grpeta
     	   WRITE(*,*) 'nclteta= ',nclteta
     	   OPEN(51,FILE=TRIM(dirclt)//TRIM(cltarqeta),STATUS='UNKNOWN', FORM='FORMATTED')

     	   WRITE(51,'(A)')  'GLOBAL ENSEMBLE CLUSTERING'
     	   WRITE(51,'(4A)') 'FORECAST FROM: ',datei,'	  VALID FOR: ',fctdate(nbs)
     	   WRITE(51,'(A,I3)') 'no. of clusters: ',nclteta
     
     	   pm=nmembers-nclteta+1
     	   DO m=1, nclteta
     	      nmembeta=nmgrp(m,pm)
     	      WRITE(51,'(A,I3.3,A,I3)') 'n.o of members clt(',m,'): ',nmembeta
     	      DO n=1, nmembeta
     		 WRITE(51,'(A,I3)')'member: ',mbgrp(m,n,pm)
     	      END DO
     	   END DO
     	   CLOSE(51)
     	END IF
     	!*******ETAf2

     	! Bring up the number of clusters in the range "nbs"

     	ncltant=nclt  

     END IF  

  END DO  !--------End Main Loop 

END PROGRAM cluster



!************************************************************
!
! Subroutine to set up basic variables
!
!************************************************************

  SUBROUTINE attribute(clustersetup,imax,jmax,nmembers,nfctdy,grpeta,freqcalc, &
                       lonw,lone,lats,latn,dirinp,dirrms,dirout,dirclt,resol,prefx) 

  IMPLICIT NONE

  CHARACTER (LEN=*), INTENT(IN)   :: clustersetup
  INTEGER,           INTENT(OUT)  :: imax,jmax,nmembers,nfctdy,grpeta,freqcalc
  REAL,              INTENT(OUT)  :: lonw,lone,lats,latn
  CHARACTER (LEN=*), INTENT(OUT)  :: dirinp,dirrms,dirout,dirclt,resol,prefx

  CHARACTER (LEN=200) :: line,line2
  INTEGER             :: ierro

  WRITE(*,*)' '
  WRITE(*,*)'FILE100: ',TRIM(clustersetup)
  OPEN(10,FILE=TRIM(clustersetup),STATUS='OLD',FORM='FORMATTED')

  ierro=0      ! To start the loop
  DO 
      READ(10,'(A14,A)', IOSTAT=ierro) line,line2
      IF( ierro .NE. 0 ) THEN
   	   EXIT
      ELSE
   		IF( line(1:10).EQ.'IMAX      ' ) THEN
   		 READ(line2(1:4),'(I4)')  imax
   	   ELSE IF( line(1:10).EQ.'JMAX      ' ) THEN
   		 READ(line2(1:4),'(I4)')  jmax
   	   ELSE IF( line(1:10).EQ.'NMEMBERS  ' ) THEN 
   		 READ(line2(1:4),'(I4)')  nmembers
   	   ELSE IF( line(1:10).EQ.'NFCTDY    ' ) THEN 
   		 READ(line2(1:4),'(I4)')  nfctdy
   	   ELSE IF( line(1:10).EQ.'GRPETA    ' ) THEN 
   		 READ(line2(1:4),'(I4)')  grpeta
   	   ELSE IF( line(1:10).EQ.'FREQCALC  ' ) THEN 
   		 READ(line2(1:4),'(I4)')  freqcalc
   	   ELSE IF( line(1:10).EQ.'LONW      ' ) THEN 
   		 READ(line2(1:10),'(F10.2)')  lonw
   	   ELSE IF( line(1:10).EQ.'LONE      ' ) THEN 
   		 READ(line2(1:10),'(F10.2)')  lone
   	   ELSE IF( line(1:10).EQ.'LATS      ' ) THEN 
   		 READ(line2(1:10),'(F10.2)')  lats
   	   ELSE IF( line(1:10).EQ.'LATN      ' ) THEN 
   		 READ(line2(1:10),'(F10.2)')  latn
   	   ELSE IF( line(1:10).EQ.'DATALSTDIR' ) THEN
   		 dirinp=line2
   	   ELSE IF( line(1:10).EQ.'DATARMSDIR' ) THEN
   		 dirrms=line2
   	   ELSE IF( line(1:10).EQ.'DATAOUTDIR' ) THEN 
   		 dirout=line2
   	   ELSE IF( line(1:10).EQ.'DATACLTDIR' ) THEN
   		 dirclt=line2
   	   ELSE IF( line(1:10).EQ.'RESOL     ' ) THEN 
   		 resol=line2
           ELSE IF( line(1:10).EQ.'PREFX     ' ) THEN 
                 prefx=line2
   	   END IF
      END IF
  ENDDO

  CLOSE(10)

  RETURN
  END SUBROUTINE attribute
  
  

!************************************************************
!
! Subroutine to obtain the respective points of the selected region
!
!************************************************************

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



!************************************************************
!
! Subroutine to evaluate the latitudinal weights
!
!************************************************************

  SUBROUTINE lweights(imax,jmax,glat,latweight)

  IMPLICIT NONE

  INTEGER,               INTENT(IN)  :: imax,jmax
  REAL, DIMENSION(jmax), INTENT(IN)  :: glat
  REAL, DIMENSION(jmax), INTENT(OUT) :: latweight

  INTEGER  :: j
  REAL     :: rt,pi,dlon,dlonr,area
  REAL     :: eps1,eps2,latg1,latg2,latr1,latr2
  REAL, DIMENSION(jmax/2) :: glatr,latwgt

  pi=4.0*ATAN(1.0)
  rt=6.37E6
  dlon=360.0/FLOAT(imax)
  dlonr=(pi*dlon)/180.0

  area=0.0
  glatr(1:jmax/2)=glat(jmax/2:1:-1)
  
  DO j=1,jmax/2

    IF (j .EQ. 1) THEN
       eps1=glatr(j)
       eps2=(glatr(j+1)-glatr(j))/2.0
    ELSE IF (j .EQ. jmax/2) THEN
       eps1=(glatr(j)-glatr(j-1))/2.0
       eps2=90.0-glatr(j)
    ELSE 
       eps1=(glatr(j)-glatr(j-1))/2.0
       eps2=(glatr(j+1)-glatr(j))/2.0
    END IF

    latg1=glatr(j)-eps1
    latg2=glatr(j)+eps2
    
    latr1=(pi*latg1)/180.0
    latr2=(pi*latg2)/180.0
    
    latwgt(j)=(rt*rt)*(sin(latr2)-sin(latr1))*dlonr

    area=area+(2.0*latwgt(j))*FLOAT(imax)

  END DO

  latweight(1:jmax/2)=latwgt(jmax/2:1:-1)
  latweight(1+jmax/2:jmax)=latwgt(1:jmax/2)

  WRITE(*,*)' '
  WRITE(*,'(A,E20.10)')'Exactly   area: ',4.0*pi*rt**2
  WRITE(*,'(A,E20.10)')'Evaluated area: ',area
  WRITE(*,*)' '
  WRITE(*,*) 'Latitudinal weights:'
  WRITE(*,'(8E13.5)') (latweight(j),j=1,jmax)

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
  
  SUBROUTINE getlstgrb(anldate,dateeta,fctdate,ngrbs,nmembers,prefx,resol, &
                       lstfct,cltarq,cltarqeta)
  
  IMPLICIT NONE
  
  INTEGER,                             INTENT(IN)  :: nmembers,ngrbs
  CHARACTER(len=10),                   INTENT(IN)  :: anldate,dateeta
  CHARACTER(len=10), DIMENSION(ngrbs), INTENT(IN)  :: fctdate
  CHARACTER(LEN=*),                    INTENT(IN)  :: prefx,resol                

  CHARACTER(len=*),                             INTENT(OUT)  :: cltarqeta
  CHARACTER(len=*),  DIMENSION(ngrbs),          INTENT(OUT)  :: cltarq
  CHARACTER(len=*),  DIMENSION(nmembers,ngrbs), INTENT(OUT)  :: lstfct

!+++++Local variables

  INTEGER                :: k,nf,membm


   ! fct files

  membm=(nmembers-1)/2

  DO nf=1,ngrbs
  
     WRITE(*,*)' '

     DO k=1,membm
        IF (nf .EQ. 1) THEN
           WRITE(lstfct(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
           WRITE(*,*)'lstfct(',k,',',nf,')=',TRIM(lstfct(k,nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstfct(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
           WRITE(*,*)'lstfct(',k,',',nf,')=',TRIM(lstfct(k,nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstfct(k,nf),'(A,I2.2,6A)')'GPOS',k,'P',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
           WRITE(*,*)'lstfct(',k,',',nf,')=',TRIM(lstfct(k,nf))
        END IF
     END DO
     DO k=1,membm
        IF (nf .EQ. 1) THEN
           WRITE(lstfct(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
           WRITE(*,*)'lstfct(',k+membm,',',nf,')=',TRIM(lstfct(k+membm,nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstfct(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
           WRITE(*,*)'lstfct(',k+membm,',',nf,')=',TRIM(lstfct(k+membm,nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstfct(k+membm,nf),'(A,I2.2,6A)')'GPOS',k,'N',anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
           WRITE(*,*)'lstfct(',k+membm,',',nf,')=',TRIM(lstfct(k+membm,nf))
        END IF
     END DO
        IF (nf .EQ. 1) THEN
           WRITE(lstfct(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.icn.',TRIM(resol),'.grb'
           WRITE(*,*)'lstfct(',nmembers,',',nf,')=',TRIM(lstfct(nmembers,nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(lstfct(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.inz.',TRIM(resol),'.grb'
           WRITE(*,*)'lstfct(',nmembers,',',nf,')=',TRIM(lstfct(nmembers,nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(lstfct(nmembers,nf),'(7A)')'GPOS',TRIM(prefx),anldate(1:10),fctdate(nf)(1:10),'P.fct.',TRIM(resol),'.grb'
           WRITE(*,*)'lstfct(',nmembers,',',nf,')=',TRIM(lstfct(nmembers,nf))
        END IF

        IF (nf .EQ. 1) THEN
           WRITE(cltarq(nf),'(5A)')'clusters',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol)
           WRITE(*,'(A,I2,2A)')'cltarq(',nf,'): ',TRIM(cltarq(nf))
        END IF
        IF (nf .EQ. 2) THEN
           WRITE(cltarq(nf),'(5A)')'clusters',anldate(1:10),fctdate(nf)(1:10),'.inz.',TRIM(resol)
           WRITE(*,'(A,I2,2A)')'cltarq(',nf,'): ',TRIM(cltarq(nf))
        END IF
        IF (nf .GT. 2) THEN
           WRITE(cltarq(nf),'(5A)')'clusters',anldate(1:10),fctdate(nf)(1:10),'.',TRIM(resol)
           WRITE(*,'(A,I2,2A)')'cltarq(',nf,'): ',TRIM(cltarq(nf))
        END IF
  END DO
        WRITE(cltarqeta,'(6A)')'clusters',anldate(1:10),dateeta(1:10),'.',TRIM(resol),'.eta'
        WRITE(*,'(2A)')'cltarqeta: ',TRIM(cltarqeta)

  RETURN
  END SUBROUTINE getlstgrb 
  



