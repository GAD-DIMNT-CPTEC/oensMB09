
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:37 $
!  $Revision: 1.1.1.1 $
!
PROGRAM model

  USE Radiation, ONLY:        &
       InitRadiation         , &
       InitSpmrad            , &
       InitRadtim            , &
       InitGetoz

  USE Surface, ONLY:          &
       InitSurface

  USE FieldsPhysics, ONLY:     &
       InitFieldsPhyscs, InitVariancia, InitBoundCond, &
       InitCheckfile, InitSurfTemp, InitGetsbc, restartphyscs, &
       zorl, imask, gtsea, td0, w0, capac0, prct, prcc, geshem

  USE Convection, ONLY:       &
       InitConvection

  USE Sizes, ONLY:            &
       imaxperj              , &
       ibMax                 , &
       jbMax                 , &
       mnMax                 , &
       mMax                  , &
       nMax                  , &
       mnMap                 , &
       kMax                  , &
       imax                  , &
       jmax                  , &
       ijMax                 , &
       ijMaxGauQua           , &
       ibPerIJ               , &
       jbPerIJ               , &
       ibMaxPerJB            , &
       si                    , & 
       sl                    , &
       cl


  USE GridHistory, ONLY:      &
       InitGridHistory       , &
       WriteGridHistory      , &
       TurnOnGridHistory     , &
       IsGridHistoryOn

  USE InputOutput, ONLY:       &
       InitInputOutput       , &
       gread4                , &
       gwrite                , &
       fsbc

  USE Diagnostics, ONLY:      &
       InitDiagnostics       , &
       rmsgt                 , &
       rsdiag                , &
       Prec_Diag             , &
       accpf                 , &
       wridia                , &
       weprog                , &
       wrprog                , &
       reqdg                 , &
       combf                 , &
       dodia                 , &
       itavl                 , &
       iavrq                 , &
       nucf                  , &
       lvrq                  , &
       nurq                  , &
       lvcf                  , &
       itcf                  , &
       mxavl                 , &
       icf  

  USE PlanBoundLayer, ONLY:   &
       InitPlanBoundLayer

  USE FieldsDynamics, ONLY: &
       fgqm, fgqmm, fgum, fgvm, fgtmpm, fgdivm, &
       fgtlamm, fgtphim, fglnpm, fgplamm, fgpphim, &
       qgzs, qlnpp, qtmpp, qdivp, qrotp, qqp

  USE Utils, ONLY:     &
       tmstmp2,        &
       colrad,         &
       colrad2D,       &
       InitTimeStamp,  &
       TimeStamp

  USE Init, ONLY :            &
       InitAll

  USE NonLinearNMI, ONLY:  &
       Nlnmi                     , &
       Diaten, &
       Getmod

  USE SpecDynamics, ONLY:      &
       bmcm

  USE ModTimeStep, ONLY:      &
       SfcGeoTrans,           &
       TimeStep

  USE PhysicsDriver , ONLY: &
       InitSimpPhys

  USE SemiLagrangian, ONLY: &
       InitSL

  USE IOLowLevel, ONLY:    &
       InitReadWriteSpec         , &
       ReadHead                  , &
       GReadHead                 , &
       ReadLandSeaMask2          , &
       LandSeaMask

  USE  Options, ONLY: &
       InitResSet, InitNamModel,InitBlkdat, &
       trunc, vert, dt, maxtim, idate, idatec, idatef, delt, initlz, &
       nstep, ndord, ifilt, maxtid, dk, tk, istrt, filta, filtb, &
       ifsst, sstlag, intsst, yrl, kt, ktm, ktp, jdt, ddelt, dct, deltOut, &
       maxtfm, mdxtfm, ctdh0, dctd, mextfm, cteh0, dcte, monl, cdhl, ifprt, &
       dogwd, doprec, isimp, enhdif, grhflg, igwd, allghf, dodyn, &
       reststep, start, slagr, reducedGrid, linearGrid, nlnminit, &
       diabatic, eigeninit, rsettov, GenRestFiles, rmRestFiles, &
       nfprt, nfin0, nfin1, nfout0, nfout1, nfsibo, nfsibi, nfdrct, &
       nfdiag, nfcnv0, nf3d, nfsst, nfcnv1, neprog, nedrct, nefcst, &
       nfvar, nfprc, nfdyn, nfdhn, nhdhn, igrfu, iptu, ighdr, ighou, &
       fNameInput0, fNameInput1, fNameNmi, fNameSSTAOI, fNameSnow, fNameSoilms, &
       fNameAlbedo, fNameUnitTb, fNameCnfTbl, fNameCnf2Tb, fNameLookTb, fNameSibVeg, &
       fNameSibAlb, fNameOrgvar, fNameSibmsk, fNameTg3zrl, fNameDTable, &
       path_in, dirfNameOutput, PREFX, EXTF, EXDH, EXTW, EXDW, EXTS

  IMPLICIT NONE

  CHARACTER(LEN=200)   :: roperm
  CHARACTER(LEN=200)   :: fileout11
  CHARACTER(LEN=200)   :: fileout02
  CHARACTER(LEN=200)   :: fileout01
  CHARACTER(LEN=200)   :: fileoutGH
  CHARACTER(LEN=200)   :: fconvcl0
  CHARACTER(LEN=200)   :: fconvcl1
  CHARACTER(LEN=200)   :: fsibprg0
  CHARACTER(LEN=200)   :: fsibprg1
  CHARACTER(LEN=  7)   :: namee
  CHARACTER(LEN=  7)   :: namef
  CHARACTER(LEN= 10)   :: labeli
  CHARACTER(LEN= 10)   :: labelc
  CHARACTER(LEN= 10)   :: labelf
  CHARACTER(LEN=  4)   :: TRC
  CHARACTER(LEN=  4)   :: LV
  LOGICAL              :: lreststep
  LOGICAL              :: restart
  LOGICAL              :: inirestart
  REAL   , ALLOCATABLE :: qgzs_orig(:)
  REAL   , ALLOCATABLE :: lsmk(:)
  REAL   , ALLOCATABLE :: si_in(:)
  REAL   , ALLOCATABLE :: sl_in(:)
  REAL   , ALLOCATABLE :: del_in(:)
  REAL   , ALLOCATABLE :: ct_in(:)
  REAL   , ALLOCATABLE :: cq_in(:)
  REAL                 :: SumDel
  INTEGER              :: ifday
  REAL                 :: tod
  REAL                 :: fa 
  REAL                 :: fb
  REAL                 :: fb1
  INTEGER              :: i
  INTEGER              :: ij
  INTEGER              :: j
  INTEGER              :: l
  INTEGER              :: PlotStep 
  CHARACTER (LEN=10)   :: DateInit_s 
  CHARACTER (LEN=10)   :: DateNow_s
  CHARACTER (LEN=10)   :: cc1
  CHARACTER (LEN=10)   :: ModelName
  LOGICAL              :: enhdifl
  INTEGER              :: jhr
  INTEGER              :: jmon
  INTEGER              :: jday
  INTEGER              :: jyr
  REAL                 :: ahour
  INTEGER              :: ifdy
  REAL                 :: todcld
  INTEGER              :: ids(4)
  INTEGER              :: idc(4)
  REAL                 :: totm 
  REAL                 :: todsib
  INTEGER              :: limlow
  INTEGER              :: ierr
  INTEGER              :: maxstp
  REAL                 :: cthw  
  LOGICAL, ALLOCATABLE :: cehl(:)
  REAL   , ALLOCATABLE :: cehr(:)  
  REAL   , ALLOCATABLE :: cdhr(:) 
  REAL                 :: fdh
  REAL                 :: dth
  REAL                 :: delth
  REAL                 :: fdayh
  REAL                 :: zero=0.0
  CHARACTER(LEN=4 )    :: nexp
  CHARACTER(LEN=40)    :: jttl
  INTEGER              :: maxt0
  REAL   , ALLOCATABLE :: rlsm(:,:)

  INTEGER :: ljdt

  CALL InitBlkdat
  NEXP    ='0003'
  fsbc    =.TRUE.
  !
  ! input case
  !
  CALL InitResSet() 
  CALL InitNamModel()

  IF(trunc < 100)WRITE(TRC,'(a1,i3.3)')'T',trunc
  IF(trunc >= 100 .AND. trunc < 1000)WRITE(TRC,'(a1,i3)')'T',trunc
  IF(trunc >= 1000)WRITE(TRC,'(a1,i4.4)')'T',trunc   
  IF(vert < 100)WRITE(LV,'(a1,i2.2)')'L',vert
  IF(vert >= 100 .AND. vert < 1000)WRITE(LV,'(a1,i3.3)')'L',vert

  ALLOCATE(cehl(0:maxtid))
  ALLOCATE(cehr(1:maxtid))  
  ALLOCATE(cdhr(1:maxtid))

  PlotStep = NINT(deltOut/delt)
  !
  ! echo problem size
  !
  WRITE(ModelName,"('T',i3.3,'L',i2.2)") trunc, vert
  JTTL    =  'CPTEC AGCM R1.2 2001  '//TRIM(ModelName)//'  '//TRIM(start)
  WRITE(*,"('**')")
  WRITE(cc1,"(i10)") maxtim
  WRITE(*,"('** model ',a,' runs for ',a,' timesteps')") ModelName, &
       TRIM(ADJUSTL(cc1))
  WRITE(cc1,"(i10)") INT(delt)
  WRITE(*,"('** timestep is ',a,' (s)')") TRIM(ADJUSTL(cc1))
  WRITE(cc1,"(i10)") PlotStep
  WRITE(*,"('** outputs are spaced by ',a,' timesteps')") TRIM(ADJUSTL(cc1))
  WRITE(*,"('** input file name ',a)") TRIM(fNameInput0)
  WRITE(*,"('** input file name ',a)") TRIM(fNameInput1)
  WRITE(*,"('** output file directory ',a)") TRIM(dirFNameOutput)
  WRITE(*,"('** model configuration: ')",ADVANCE='NO') 
  IF (slagr) THEN
     WRITE(*,"('Semi-Lagrangean, ')",ADVANCE='NO')
  ELSE
     WRITE(*,"('Eulerian, ')",ADVANCE='NO')
  END IF
  IF (reducedGrid) THEN
     WRITE(*,"('Reduced and ')",ADVANCE='NO')
  ELSE
     WRITE(*,"('Gaussian and ')",ADVANCE='NO')
  END IF
  IF (linearGrid) THEN
     WRITE(*,"('Linear Grid')")
  ELSE
     WRITE(*,"('Quadratic Grid')")
  END IF
  WRITE(*,"('**')")
  !
  !     Get Initial Labels and Vertical Discretization
  !
  OPEN(unit=nfin1, file=fNameInput1, form='unformatted', &
       ACTION='read', IOSTAT=ierr)

  ALLOCATE (si_in(vert+1))
  ALLOCATE (sl_in(vert))
  ALLOCATE (del_in(vert))
  ALLOCATE (ct_in(vert))
  ALLOCATE (cq_in(vert))

  IF( TRIM(start) == "warm" )THEN
     CALL GReadHead (nfin1, ifday, tod, idate, idatec, si_in, sl_in, vert)
     CALL InitTimeStamp (DateInit_s, idate)
     labeli  = DateInit_s
     CALL InitTimeStamp (DateInit_s, idatec)
     labelc  = DateInit_s
     WRITE(labelf,'(I4.4, 3I2.2)' ) (idatef(i),i=4,1,-1)
     nfcnv0 = nfcnv1
     nfsibi = nfsibo
  ELSE
     CALL ReadHead (nfin1, ifday, tod, idate, idatec, si_in, sl_in, vert)
     CALL InitTimeStamp (DateInit_s, idate)
     labeli  = DateInit_s
     CALL TimeStamp     (DateNow_s, idatec, maxtim, delt)
     labelf  = DateNow_s
     labelc  = labelf
     idatec  = idate
  END IF
  REWIND (nfin1)
  !
  ! initialize modules
  !
  CALL InitAll(trunc, vert, reducedGrid, linearGrid, & 
       ct_in, cq_in, si_in, sl_in, del_in, dk, tk)
  SumDel=SUM(del_in)
  IF (ABS(1.0-SumDel) > 1.0E-5) THEN
     WRITE (nfprt, '(A,1PG12.5,A)') ' 1-SUM(del_in) = ', 1.0-SumDel, ' > 1.0E-5 '
     STOP ' Main : (ABS(1.0-SumDel) > 1.0E-5)'
  END IF

  CALL InitInputOutput (mMax, nMax , mnMax, mnMap, kmax, &
       path_in, fNameSnow, fNameSSTAOI, fNameSSTAOI, &
       fNameSoilms, fNameAlbedo, fNameCnfTbl, &
       fNameCnf2Tb, fNameLookTb, fNameUnitTb)

  CALL InitDiagnostics (doprec, dodyn  ,    colrad , &
       mMax   ,nMax   ,mnMax  , mnMap  ,    iMax   ,   jMax, &
       kMax   ,ibMax  ,jbMax  , ibMaxPerJB, fNameDTable)

  CALL InitReadWriteSpec(&
       mxavl   ,icf     ,mMax    , &
       mnMax   ,kMax    ,ijMax   ,iMax    ,jMax    ,ibMax   , &
       jbMax   , &
       reqdg   ,combf   ,dodia   ,itavl   ,iavrq   ,&
       nucf    ,lvrq    ,nurq    ,lvcf    ,itcf    )  

  CALL InitFieldsPhyscs(ibMax, kMax, jbMax,iMax,jMax)

  DO jdt=0,maxtim
     cehl(jdt)=.FALSE.
     cdhl(jdt)=.FALSE.
  END DO
  !     
  dogwd=1
  CALL InitVariancia(igwd  ,nfvar ,fNameOrgvar)
  delt     = ddelt
  IF (nstep.EQ.1) nstep=7

  IF(enhdif.EQ.'YES ') THEN
     enhdifl = .TRUE.
  ELSE
     enhdifl = .FALSE.
  ENDIF

  IF(isimp.EQ.'YES ') THEN
     WRITE(nfprt,7)
     initlz=0 
     nfcnv0=0
     igwd  ='NO  '
  END IF
  !
  !     initialize module  SL 
  !
  IF (slagr) THEN
     CALL InitSL
  END IF
  !
  ! Initialize modules 
  !
  CALL InitPlanBoundLayer(kmax, sl, del_in, si(1:kmax))
  CALL InitConvection(si, del_in, sl, cl, kmax)
  CALL InitSurface(del_in,path_in,fNameSibVeg,fNameSibAlb)
  CALL InitRadiation()
  CALL InitSpmrad
  CALL InitRadtim(monl)
  CALL InitGetoz(yrl,kmax,sl)
  IF (isimp == 'YES') THEN
     CALL InitSimpPhys(colrad2D, sl)
  END IF
  !
  ! prepare output files
  !
  roperm  = dirfNameOutput
  namee   = "GPRG"//TRIM(PREFX)
  namef   = "GFCT"//TRIM(PREFX) 

  fileout02=TRIM(dirFnameOutput)//"/"//TRIM(namef)//labeli//labelc//&
       &EXTW//'.'//TRIM(ModelName)//'.outmdt'

  fileout11=TRIM(dirFnameOutput)//"/"//TRIM(namef)//labeli//labelc//&
       &EXTW//'.'//TRIM(ModelName)//'.outatt'

  IF (nhdhn /= 0) THEN
     OPEN (nfdhn, FILE=TRIM(dirFnameOutput)//"/"//&
          &"GDHN"//TRIM(PREFX)//labeli//labelc//&
          &EXTW//'.'//TRIM(ModelName), ACTION='write', STATUS='replace')
  END IF

  IF (doprec) THEN
     OPEN (nfprc, FILE=TRIM(dirFnameOutput)//"/"//&
          &"GPRC"//TRIM(PREFX)//labeli//labelc//&
          &EXTW//'.'//TRIM(ModelName), ACTION='write', STATUS='replace')
  END IF

  IF (grhflg) THEN
     fileoutGH=TRIM(dirFnameOutput)//"/"//&
          &"GFGH"//TRIM(PREFX)//labeli//labelc//&
          &EXTW//'.'//TRIM(ModelName)
     OPEN (ighdr, FILE=TRIM(dirFnameOutput)//"/"//&
          &"GFGH"//TRIM(PREFX)//labeli//labelc//&
          &EXDH//'.'//TRIM(ModelName), ACTION='write')

  END IF

  fconvcl0=TRIM(dirFnameOutput)//"/"//TRIM(namef)//labeli//labelc//&
       &EXTW//'.'//TRIM(ModelName)//'.convcl'

  fsibprg0=TRIM(dirFnameOutput)//"/"//TRIM(namef)//labeli//labelc//&
       &EXTW//'.'//TRIM(ModelName)//'.sibprg'

  WRITE(*,*)fconvcl0
  WRITE(*,*)fsibprg0
  IF( TRIM(start) == "warm" )THEN
     nfcnv0 = nfcnv1
     nfsibi = nfsibo
     OPEN (nfsibi, FILE=TRIM(fsibprg0))
     OPEN (nfcnv0, FILE=TRIM(fconvcl0))       
  ENDIF
  !
  !     read cloud dataset - logic assumes that initialization not
  !     performed for warm start
  !
  jdt=0
  CALL InitGridHistory (del_in ,nexp  ,jttl  ,idate ,allghf,grhflg,igrfu  , &
       iptu   ,ighdr ,iMax  ,jMax  ,ibMax ,jbMax ,ibPerIJ, &
       jbPerIJ,kMax)  
  CALL InitBoundCond(ibMax,jbMax,ifdy,todcld,ids,idc,ifday, &
       tod ,totm ,todsib,idate,idatec,si,sl, &
       fNameSibmsk,fNameTg3zrl,ibMaxPerJB)
  !
  tod=0.0
  IF(iabs(initlz).GE.1.AND.initlz.GT.-3) THEN
     CALL rsdiag
     CALL Diaten(slagr,fNameInput0,fNameInput1,ifday, tod, idate, idatec)
     ktm=0
     ktp=0
     kt =0
     !
     !     snow reinitialization after surface temperature initialization
     !     
     IF(initlz.GE.1)THEN
        CALL InitSurfTemp (jbMax,ibMaxPerJB )
     END IF
     !
     !   reset old time step values on the grid to zero
     !   ----------------------------------------------
     fgqm = 0.
     fgqmm = 0.
     fgum = 0.
     fgvm = 0.
     fgtmpm = 0.
     fgdivm = 0.
     fgtlamm = 0.
     fgtphim = 0.
     fglnpm = 0.
     fgplamm = 0.
     fgpphim = 0.
  END IF
  ! array allocation
  !**(JP)** lsmk de entrada "hardwired" para a grade gausseana

  ALLOCATE(qgzs_orig(2*mnMax))
  ALLOCATE(lsmk(ijMaxGauQua))
  ALLOCATE(rlsm(imax,jmax))

  OPEN(unit=nfin0, file=fNameInput0, form='unformatted', &
       ACTION='read', IOSTAT=ierr)
  !
  !     start reading initial values (t=t   )
  !
  IF (nfin0 .EQ. nfin1) THEN      
     !
     ! cold inicialization
     !
     restart = .FALSE.
     inirestart = .FALSE.
     CALL gread4 (nfin0, ifday, tod  , idate, idatec,qgzs  ,qlnpp , &
          qtmpp, qdivp, qrotp, qqp  , sl    , si   ,dodyn , &
          nfdyn)      
     REWIND nfin0
  ELSE
     !
     ! warm inicialization
     !
     restart = .TRUE.
     READ(nfin0) fgqm, fgum, fgvm, fgtmpm, fgdivm, fglnpm, fgtlamm, &
          fgtphim, fgplamm, fgpphim     
     inirestart = .FALSE.      
     READ(nfin1) ifday,tod,idate,idatec,si,sl
     READ(nfin1) qgzs,qlnpp,qtmpp,qdivp,qrotp,qqp
     REWIND nfin0
     REWIND nfin1
  END IF
  !
  !
  IF(ifsst.GT.3.AND.sstlag.LT.zero)THEN
     WRITE(nfprt,336)ifsst,sstlag
     STOP 336
  END IF
  !    
  IF (ifsst .GE. 4) THEN
     CALL LandSeaMask(ifsst,nfsst,labeli,intsst,sstlag,fNameSSTAOI,rlsm) 
     ij=0
     DO j=1,jmax
        DO i=1,imax
           ij=ij+1
           lsmk(ij)=rlsm(i,j)
        END DO
     END DO
  ELSE
     CALL ReadLandSeaMask2 (TRIM(fNameSSTAOI), lsmk)
  END IF
  !
  IF(isimp.EQ.'YES ') qqp = 0.0 
  IF(isimp.EQ.'YES ') fgqm = 0.0
  !
  qgzs_orig = qgzs
  !
  !     write diagnostics/prognostics directory
  !
  !
  !     write uninitialized initial condition prognostics on tape
  !     the use of swrk in wrprog destroys wsib
  !     
  IF(ifday.EQ.0.AND.tod.EQ.zero) THEN
     CALL wrprog (nfdrct,nfdiag ,ifday ,tod   ,idate ,idatec ,qrotp , &
          qdivp ,qqp    ,qlnpp ,qtmpp ,zorl  ,gtsea  ,td0   , &
          capac0,w0     ,imask ,nf3d   ,del_in, &
          qgzs_orig,lsmk,ijMaxGauQua  ,kmax  ,imax   ,jmax  , &
          roperm,namef  ,labeli,labelf,extw  ,exdw   ,TRIM(TRC), &
          TRIM(LV))
     IF (mextfm .GT. 0) &
          CALL weprog (nedrct,neprog,nefcst,ifday ,tod    ,idate ,idatec , &
          del_in,qgzs_orig,lsmk,qlnpp ,qdivp  , &
          qrotp ,qqp   ,qtmpp ,zorl  ,gtsea  ,td0   ,capac0 , &
          w0    ,imask ,roperm,namee ,labeli ,labelf,extw   , &
          exdw  ,TRIM(TRC),TRIM(LV),ijMaxGauQua,kmax ,imax  ,jmax  )
  END IF
  !
  ! compute the spectral coefficients of Laplacian of topography
  !
  CALL SfcGeoTrans()
  !
  ! compute normal modes if necessary
  !
  IF (eigeninit) THEN
     CALL Getmod(slagr,fNameNmi,rsettov)
  END IF
  !
  ! Non-linear normal mode initialization
  !
  IF (nlnminit) THEN
     !
     !     do machenhauer's non-linear normal mode initialization
     !
     IF(iabs(initlz).EQ.2) THEN
        CALL rsdiag
        CALL Nlnmi(nlnminit,diabatic,slagr,fNameNmi, ifday, tod, idatec, ktm)

        CALL wrprog (nfdrct,nfdiag,ifday ,tod   ,idate ,idatec,qrotp , &
             qdivp ,qqp   ,qlnpp ,qtmpp ,zorl  ,gtsea ,td0   , &
             capac0,w0    ,imask ,nf3d  ,del_in, &
             qgzs_orig,lsmk,ijMaxGauQua ,kmax  ,imax  ,jmax  , &
             roperm,namef ,labeli,labelf,extw  ,exdw  ,TRIM(TRC), &
             TRIM(LV))

        IF (mextfm .GT. 0) &
             CALL weprog (nedrct,neprog,nefcst,ifday ,tod   ,idate ,idatec, &
             del_in,qgzs_orig,lsmk,qlnpp,qdivp , &
             qrotp ,qqp   ,qtmpp ,zorl  ,gtsea ,td0   ,capac0, &
             w0    ,imask ,roperm,namee ,labeli,labelf,extw  , &
             exdw  ,TRIM(TRC),TRIM(LV),ijMaxGauQua,kmax,imax  ,jmax  )

        ! Writes the Initialized Fields as an Initial Condition for Further Run
        IF (maxtim <= 0) THEN
           OPEN (nfout1, FILE=TRIM(fileout11), ACTION='write', STATUS='replace')
           CALL gwrite(nfout1,ifday ,tod   ,idate ,idatec,qlnpp ,qtmpp , &
                qdivp ,qrotp ,qqp   ,sl    ,si    ,qgzs_orig)
           CLOSE(nfout1)
        END IF

        IF (ifprt(3) .GE. 2) CALL rmsgt(qlnpp,qdivp,qtmpp,qrotp,del_in,qqp)

     END IF
     nlnminit = .FALSE.
  END IF

  ! Force stop when labeli=labec=labelf
  IF (maxtim <= 0) STOP ' Model Ended Without Time Integration'

  fdh=24.0
  dth=86400.0/fdh
  delth=delt/dth
  fdayh=float(ifday)*fdh+tod/dth
  cthw=fdayh

  IF (dctd .GT. 0.0) THEN
     cdhr(1)=ctdh0
     DO l=2,mdxtfm
        cdhr(l)=cdhr(l-1)+dctd
     END DO
     cdhr(mdxtfm:maxtid) = -100.0
  ELSE
     cdhr = -100.0
  END IF

  IF (dcte .GT. 0.0) THEN
     cehr(1)=cteh0
     DO l=2,mextfm
        cehr(l)=cehr(l-1)+dcte
     END DO
  END IF

  WRITE(*,*)' '
  WRITE(*,*)' Time Step List Output, cehl:'
  cthw=fdayh
  DO jdt=1,maxtim
     cthw=cthw+delth
     DO l=1,mextfm
        IF (ABS(cehr(l)-cthw) .LE. 0.00001) THEN
           cehl(jdt)=.TRUE.
           WRITE(*,'(A8,L1,A8,F10.2)') &
                ' cehl = ',cehl(jdt),' cthw = ',cthw
        END IF
     END DO
  END DO

  WRITE(*,*)' '
  WRITE(*,*)' Time Step List Output, cdhl:'
  cthw=fdayh    
  DO jdt=1,maxtim
     cthw=fdayh + (jdt-1)*delth
     DO l=1,maxtfm
        IF (ABS(cdhr(l)-cthw) .LE. 0.00001) THEN
           cdhl(jdt)=.TRUE.
           WRITE(*,'(A8,L1,A8,F10.2)') &
                ' cdhl = ',  cdhl(jdt),' cthw = ',cthw
        END IF
     END DO
  END DO
  !
  maxt0=NINT((float(ifday)*fdh+tod/dth)/delth)
  !     
  !     this is to remove accumulations from nlnmi
  !
  geshem=0.0
  !
  !     clear all diagnostic accumulators
  !
  CALL rsdiag 
  !
  !     check files
  !     if nfin0=nfin1   then  cold start
  !
  limlow=1
  IF(nfin0.EQ.nfin1)THEN
     !
     !     read cloud dataset for cold start
     !     
     CALL InitCheckfile(&
          jbMax  ,ifdy  ,todcld,ids   ,idc   ,ifday , &
          tod   ,idate ,idatec,todsib  ,ibMaxPerJB)
     !
     !     cold start (at first delt/4 ,then delt/2 )
     !
     limlow =2
     dt= delt /4.0
     !
     ! filter arguments for first time step
     !
     fa  = 0.0
     fb  = 1.0
     fb1 = 1.0

     DO jdt=1,2
        !
        !     snow reinitialization after surface temperature initialization
        !
        IF(initlz.GE.1.AND.jdt.EQ.2) THEN
           CALL InitSurfTemp(jbMax ,ibMaxPerJB)
        END IF

        IF (jdt.EQ.2) THEN
           CALL rsdiag()
           CALL TurnOnGridHistory()
        END IF

        istrt=jdt      
        IF(ifprt(7).GE.1)WRITE(nfprt,104) jdt

        WRITE(*,"(' cold start step ',i2)") jdt
        !     
        !     calculate matrices for semi-implicit integration
        !     
        CALL bmcm(dt, slagr)
        !
        ! perform time step 
        !
        ifilt=0
!$OMP PARALLEL
        CALL TimeStep(fb1,fa,fb,slagr,nlnminit,inirestart,.FALSE.,.FALSE.,dt,jdt, & 
             ifday,tod, idatec)
!$OMP END PARALLEL

        IF(jdt.EQ.2) THEN
           !     
           !     accumulate time mean prognostic fields if requested
           !     
           CALL accpf (ifday, tod, qtmpp, qrotp, qdivp, qqp, qlnpp, nfdyn) 
           !     
           !     diagnostic of preciptation if requested
           !     
           CALL Prec_Diag (ifday, tod, prct, prcc, nfprc) 

        END IF

        ! prepare next time step, including filter arguments

        dt=dt*2.0
        IF(IsGridHistoryOn())THEN
	   OPEN (ighou, FILE=TRIM(fileoutGH),STATUS='UNKNOWN',FORM='UNFORMATTED',&
                ACTION='WRITE')
           CALL WriteGridHistory (ighou, ifday, tod, idate)
        END IF
        ktm=kt    
        fsbc=.FALSE.
        fb1 = 0.0
     END DO
     tod=dt !PYK
  END IF
  !
  !     smooth start
  !
  IF(igwd.EQ.'YES ')dogwd=0
  ahour=ifday*24.0+tod/3600.0
  istrt=0
  !
  ! semi-implicit matrices
  !
  CALL bmcm(dt, slagr)
  !
  ! filter arguments for all remaining time steps
  !
  fa     = filta
  fb     = filtb
  fb1    = 0.0
  IF( TRIM(start) == "warm" )THEN
     CLOSE(nfsibi)
     CLOSE(nfcnv0)    
     fb1    = fb
  ENDIF
  !
  ! time step loop
  !
  ifilt=1
!$OMP PARALLEL PRIVATE(ljdt)
  DO ljdt=limlow,maxtim
     !
     !     step loop starts
     !     
     IF(isimp.NE.'YES ') THEN
!$OMP SINGLE
        CALL InitGetsbc(ifday, tod, idate, idatec)
!$OMP END SINGLE
     END IF
     jdt = ljdt
!$OMP SINGLE
     tod=tod+dt
     IF(ABS( MOD(tod+0.03125,86400.0)-0.03125).LT.0.0625)THEN
        tod=0.0
        ifday=ifday+1
     END IF
     CALL tmstmp2(idate,ifday,tod,jhr,jday,jmon,jyr)
     idatec(1)=jhr
     idatec(2)=jmon
     idatec(3)=jday
     idatec(4)=jyr
     ahour=(ifday*24.0e0)+(tod/3.6e3)      
!$OMP END SINGLE

     kt   =INT(ahour-(1.0e-2))
     ktp  =INT(ahour+(dt/3.6e3)-(1.0e-2))
     IF(jdt.EQ.maxtim) THEN
        ktm=kt
     END IF
     !      
     ! perform time step 
     !
     CALL TimeStep(fb1,fa,fb,slagr,nlnminit,inirestart,.FALSE.,enhdifl,dt,jdt, &
          ifday,tod,idatec)

!$OMP SINGLE

     !     
     !     accumulate time mean prognostic fields if requested
     !     
     CALL accpf (ifday, tod, qtmpp, qrotp, qdivp, qqp, qlnpp, nfdyn) 

     !     
     !     diagnostic of preciptation if requested
     !     
     CALL Prec_Diag (ifday, tod, prct, prcc, nfprc) 

     IF(IsGridHistoryOn())THEN
        CALL WriteGridHistory (ighou, ifday, tod, idate)
     END IF
     fsbc=.FALSE.
     ktm=kt

!$OMP END SINGLE

     !
     ! output, if desired
     !      
     !*JPB IF(MOD(jdt,reststep)==0.OR.jdt.EQ.maxtim) THEN
     IF (reststep == 0) THEN
        lreststep=(jdt == maxtim .AND. GenRestFiles)
     ELSE
        lreststep=((MOD(jdt,reststep) == 0 .OR. jdt == maxtim) .AND. GenRestFiles)
     END IF
     IF (lreststep) THEN
!$OMP SINGLE
        !
        !     write history wave-data
        !     
        WRITE(labeli,'(I4.4, 3I2.2)' )idate(4),idate(2),idate(3),idate(1)
        WRITE(labelc,'(I4.4, 3I2.2)' )idatec(4),idatec(2),idatec(3),idatec(1)

        IF (rmRestFiles) THEN

           OPEN (nfsibo, FILE=TRIM(fsibprg0 ), ACTION='write', STATUS='replace')
           OPEN (nfcnv1, FILE=TRIM(fconvcl0 ), ACTION='write', STATUS='replace')
           OPEN (nfout1, FILE=TRIM(fileout11), ACTION='write', STATUS='replace')
           OPEN (nfout0, FILE=TRIM(fileout02), ACTION='write', STATUS='replace')

           CLOSE(nfout0,STATUS='DELETE')
           CLOSE(nfsibo,STATUS='DELETE')
           CLOSE(nfcnv1,STATUS='DELETE')
           CLOSE(nfout1,STATUS='DELETE')

        END IF

        fsibprg1=TRIM(dirFnameOutput)//"/"//TRIM(namef)//labeli//labelc//&
             &EXTW//'.'//TRIM(ModelName)//'.sibprg'

        fconvcl1=TRIM(dirFnameOutput)//"/"//TRIM(namef)//labeli//labelc//&
             &EXTW//'.'//TRIM(ModelName)//'.convcl'

        fileout11=TRIM(dirFnameOutput)//"/"//TRIM(namef)//labeli//labelc//&
             &EXTW//'.'//TRIM(ModelName)//'.outatt'

        fileout01=TRIM(dirFnameOutput)//"/"//TRIM(namef)//labeli//labelc//&
             &EXTW//'.'//TRIM(ModelName)//'.outmdt'

        OPEN (nfsibo, FILE=TRIM(fsibprg1), ACTION='write', STATUS='replace')
        OPEN (nfcnv1, FILE=TRIM(fconvcl1), ACTION='write', STATUS='replace')
        OPEN (nfout1, FILE=TRIM(fileout11), ACTION='write', STATUS='replace')
        OPEN (nfout0, FILE=TRIM(fileout01), ACTION='write', STATUS='replace')

        WRITE(nfout1) ifday,tod,idate,idatec,si,sl
        WRITE(nfout1) qgzs_orig,qlnpp,qtmpp,qdivp,qrotp,qqp
        IF (ifprt(3) .GE. 3) &
             CALL rmsgt (qlnpp,qdivp,qtmpp,qrotp,del_in,qqp)
        WRITE(nfout0) fgqm, fgum, fgvm, fgtmpm, fgdivm, fglnpm, fgtlamm, &
             fgtphim, fgplamm, fgpphim

!$OMP END SINGLE

        CALL restartphyscs (jbMax,ifday,tod ,idate ,idatec, &
             nfsibo,nfcnv1,totm,ibMaxPerJB)

!$OMP SINGLE
        fileout02=fileout01
        fconvcl0=fconvcl1
        fsibprg0=fsibprg1
        CLOSE(nfsibo) 
        CLOSE(nfcnv1)
        CLOSE(nfout1)
        CLOSE(nfout0)           
!$OMP END SINGLE

     ENDIF

!$OMP SINGLE
     IF(MOD(jdt,plotstep)==0) THEN
        maxstp=NINT((float(ifday)*fdh+tod/dth)/delth)-maxt0

        WRITE(*,"('Write file at timestep ',i5)") jdt

        !
        !     reset precip. every maxstp time steps
        !     
        CALL wrprog (nfdrct,nfdiag,ifday ,tod   ,idate ,idatec,qrotp , &
             qdivp ,qqp   ,qlnpp ,qtmpp ,zorl  ,gtsea ,td0   , &
             capac0,w0    ,imask ,nf3d  ,del_in, &
             qgzs_orig,lsmk,ijMaxGauQua ,kmax  ,imax  ,jmax  , &
             roperm,namef ,labeli,labelf,extw  ,exdw  ,TRIM(TRC), &
             TRIM(LV))   
        CALL wridia(nfdiag, maxstp)
        !
        !     zero reset diagnostic fields
        !     
        CALL rsdiag
        geshem=0.0
        limlow=1     

        IF(jdt.NE.maxtim)THEN
           CALL bmcm(dt, slagr)
           maxt0=NINT((float(ifday)*fdh+tod/dth)/delth)
        END IF
     END IF

     IF (cehl(jdt)) THEN
        maxstp=NINT((float(ifday)*fdh+tod/dth)/delth)-maxt0

        IF(ifprt(8).GE.1)WRITE(nfprt,102)dt,ifday,tod, &
             float(ifday)*fdh+tod/dth,jdt,maxstp

        IF (mextfm .GT. 0) &
             CALL weprog (nedrct,neprog,nefcst,ifday ,tod   ,idate ,idatec, &
             del_in,qgzs_orig,lsmk,qlnpp,qdivp , &
             qrotp ,qqp   ,qtmpp,zorl   ,gtsea ,td0   ,capac0, &
             w0    ,imask ,roperm,namee ,labeli,labelf,extw  , &
             exdw  ,TRIM(TRC),TRIM(LV),ijMaxGauQua,kmax,imax  ,jmax  )      
     END IF

!$OMP END SINGLE

     fb1 = fb
  ENDDO

!$OMP END PARALLEL

  IF(ifprt(3).GE.2)CALL rmsgt(qlnpp,qdivp,qtmpp,qrotp,del_in,qqp)

  REWIND nfout0
  PRINT *, "***MODEL EXECUTION ENDS NORMALY***"
  STOP 

7 FORMAT( ' SIMPLIFIED PHYSICS OPTION IN EFFECT'/ &
       '  ALL OTHER OPTIONS OVERRIDDEN'/ &
       '  INITIAL CONDITIONS ISOTHERMAL RESTING ATMOSPHERE'/ &
       '  FLAT LOWER BOUNDARY'/ &
       ' PHYSICS:'/ &
       '  NEWTONIAN COOLING'/ &
       '  RALEIGH DAMPING'/ &
       '  NO WATER VAPOR OR CONDENSATION EFFECTS')
102 FORMAT(' DT=',G13.6,' IFDAY=',I10,' TOD=',G13.6, &
       ' SEC: WEPROG AT=',G13.6,'  STEP=',I10,' MAXSTP=',I5)
104 FORMAT(' ITERATION COUNT FOR THE COLD START=',I2)
336 FORMAT(' FOR IFSST=',I5,' SSTLAG MUST BE SET NONNEGATIVE.  NOT ',G12.5)         


END PROGRAM model
