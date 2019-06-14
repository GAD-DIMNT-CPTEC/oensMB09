
!  $Author: pkubota $
!  $Date: 2010/02/24 20:20:12 $
!  $Revision: 1.31 $
!
PROGRAM model

  USE Parallelism, ONLY:   &
       CreateParallelism,  &
       DestroyParallelism, &
       MsgOne,             &
       FatalError,         &
       unitDump,           &
       myId

  USE Watches, ONLY:  &
       Watch,         &
       CreateWatch,   &
       NameWatch,     &
       ChangeWatch,   &
       DumpWatch,     &
       DestroyWatch


  USE ModRadiationDriver, ONLY:        &
       InitRadiationDriver         
       
  USE Surface, ONLY:          &
       InitSurface

  USE FieldsPhysics, ONLY:     &
       InitFieldsPhyscs, InitVariancia, InitBoundCond, &
       InitCheckfile, InitSurfTemp, InitGetsbc, restartphyscs, &
       imask, gtsea, td0, w0, capac0, prct, prcc, geshem,xland,&
       seamask,z0,ustar,lowlyr,temp2m,umes2m,uve10m,vve10m

  USE Convection, ONLY:       &
       InitConvection

  USE Sizes, ONLY:            &
       myMNMax               , &
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
       IsGridHistoryOn       , &
       WriteGridHistoryTopo

  USE InputOutput, ONLY:       &
       InitInputOutput       , &
       gread4                , &
       gwrite                , &
       fsbc

  USE Diagnostics, ONLY:      &
       InitDiagnostics       , &
       StartStorDiag         , &
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

  USE PblDriver, ONLY:   &
       InitPBLDriver

  USE GwddDriver, ONLY:   &
       InitGWDDDriver



  USE FieldsDynamics, ONLY: &
       fgqm, fgqmm, fgum, fgvm, fgtmpm, fgdivm, &
       fgtlamm, fgtphim, fglnpm, fgplamm, fgpphim, &
       qgzs, qlnpp, qtmpp, qdivp, qrotp, qqp,fgtmp,fgzs, &
       fgpass_scalars

  USE Utils, ONLY:     &
       tmstmp2,        &
       colrad,         &
       colrad2D,       &
       InitTimeStamp,  &
       TimeStamp,      &
       total_mass

  USE Init, ONLY :            &
       InitAll, nls

  USE NonLinearNMI, ONLY:  &
       Nlnmi                     , &
       Diaten, &
       Getmod

  USE SpecDynamics, ONLY:      &
       bmcm

  USE ModTimeStep, ONLY:      &
       SfcGeoTrans,           &
       TimeStep   ,           &
       InitBoundSimpPhys,     &
       iold,                  &
       iact,                  &
       inew

  USE PhysicsDriver , ONLY: &
       InitSimpPhys

  USE SemiLagrangian, ONLY: &
       InitSL , &
       ulonm  , &
       ulatm  , &
       usigm  , &
       ulonm2D, &
       ulatm2D

  USE IOLowLevel, ONLY:    &
       InitReadWriteSpec         , &
       ReadHead                  , &
       GReadHead                 , &
       ReadLandSeaMask2          , &
       LandSeaMask

  USE  Options, ONLY: &
       ReadNameList, DumpOptions,CreateFileName,FNameRestInput2,FNameRestOutput2,&
       FNameRestInput1,FNameRestOutput1,FNameConvClInp0,FNameConvClOut1,FNameSibPrgInp0,&
       FNameSibPrgOut1,FNameGDHN,FNameGDYN,FNameGPRC,FNameOutGH,FNameTopGH,FNamenDrGH,  &
       trunc, vert, dt, maxtim, idate, idatec, idatef, delt, initlz, &
       nstep, ndord, ifilt, maxtid, dk, tk, istrt, filta, filtb, &
       ifsst, sstlag, intsst, yrl, kt, ktm, ktp, jdt, ddelt, dct,  &
       maxtfm, mdxtfm, ctdh0, dctd, mextfm, cteh0, dcte, monl, cdhl, nfctrl, &
       dogwd, doprec, isimp, enhdif, grhflg, igwd, allghf, dodyn, &
       reststep, start, slagr, reducedGrid, linearGrid, nlnminit, SL_twotime_scheme, &
       diabatic, eigeninit, rsettov, GenRestFiles, rmRestFiles, &
       nfprt, nfin0, nfin1, nfout0, nfout1, nfsibo, nfsibi, nfdrct, &
       nfdiag, nfcnv0, nffcst, nfcnv1, neprog, nedrct, nefcst, &
       nfvar, nfprc, nfdyn, nfdhn, nhdhn, nfghds, nfghloc, nfghdr, nfghou,nfghtop, &
       fNameInput0, fNameInput1, fNameNmi, mgiven, gaussgiven, &
       fNameSSTAOI, & !fNameSnow, fNameSoilms, fNameAlbedo, &
       fNameUnitTb, fNameCnfTbl, fNameCnf2Tb, fNameLookTb, fNameSibVeg, &
       fNameSibAlb, fNameOrgvar, fNameSibmsk, fNameTg3zrl, fNameDTable, &
       path_in, dirfNameOutput, PREFX, EXTF, EXDH, EXTW, EXDW, EXTS,iqdif, &
       record_type,fNameSoilType,fNameVegType,fNameSoilMoist,SetOutPut,cthl, &
       asolc,asolm,crdcld,iswrad,ilwrad,nfsibt,nscalars,  LV, TruncLev,TRCG

  USE Constants, ONLY:    r8

  IMPLICIT NONE

  INCLUDE 'mpif.h'

  CHARACTER(LEN=200)   :: roperm
  CHARACTER(LEN=  9)   :: namee
  CHARACTER(LEN=  9)   :: namef
  CHARACTER(LEN= 10)   :: labeli
  CHARACTER(LEN= 10)   :: labelc
  CHARACTER(LEN= 10)   :: labelf
  CHARACTER(LEN=  4)   :: PRC
  LOGICAL              :: lreststep
  LOGICAL              :: restart
  REAL(KIND=r8)   , ALLOCATABLE :: qgzs_orig(:)
  REAL(KIND=r8)   , ALLOCATABLE :: lsmk(:)
  REAL(KIND=r8)   , ALLOCATABLE :: si_in(:)
  REAL(KIND=r8)   , ALLOCATABLE :: sl_in(:)
  REAL(KIND=r8)   , ALLOCATABLE :: del_in(:)
  REAL(KIND=r8)   , ALLOCATABLE :: ct_in(:)
  REAL(KIND=r8)   , ALLOCATABLE :: cq_in(:)
  REAL(KIND=r8)   , ALLOCATABLE :: tequi(:,:,:)
  REAL(KIND=r8)                 :: SumDel
  INTEGER              :: ifday
  REAL(KIND=r8)                 :: tod
  REAL(KIND=r8)                 :: fa
  REAL(KIND=r8)                 :: fb
  REAL(KIND=r8)                 :: fb1
  REAL(KIND=r8)                 :: delta2
  INTEGER              :: i
  INTEGER              :: ij
  INTEGER              :: j
  INTEGER              :: l,l1,l2
  CHARACTER (LEN=10)   :: DateInit_s
  LOGICAL              :: enhdifl
  INTEGER              :: jhr
  INTEGER              :: jmon
  INTEGER              :: jday
  INTEGER              :: jyr
  REAL(KIND=r8)                 :: ahour
  INTEGER              :: ifdy
  REAL(KIND=r8)                 :: todcld
  INTEGER              :: ids(4)
  INTEGER              :: idc(4)
  REAL(KIND=r8)                 :: todsib
  INTEGER              :: iovmax
  INTEGER              :: limlow
  INTEGER              :: ierr
  INTEGER              :: maxstp
  INTEGER              :: nsca_save
  REAL(KIND=r8)                 :: cthw
  LOGICAL, ALLOCATABLE :: cehl(:)
  REAL(KIND=r8)   , ALLOCATABLE :: cehr(:)
  REAL(KIND=r8)   , ALLOCATABLE :: cdhr(:)
  REAL(KIND=r8)                 :: fdh
  REAL(KIND=r8)                 :: dth
  REAL(KIND=r8)                 :: delth
  REAL(KIND=r8)                 :: fdayh
  REAL(KIND=r8)                 :: zero=0.0_r8
  INTEGER              :: maxt0
  REAL(KIND=r8)   , ALLOCATABLE :: rlsm(:,:)
  !$ INTEGER, EXTERNAL :: OMP_GET_NUM_THREADS
  !$ INTEGER, EXTERNAL :: OMP_GET_THREAD_NUM
  !$ INTEGER, EXTERNAL :: OMP_GET_MAX_THREADS
  INTEGER :: nThreads=0
  INTEGER :: iThread
  INTEGER :: ljdt
  LOGICAL, PARAMETER :: instrument=.TRUE.
  TYPE(Watch), ALLOCATABLE :: wt(:)
  CHARACTER(LEN=*), PARAMETER :: h="**(Model)**" 

  ! engage MPI 

  CALL CreateParallelism()

  ! execution time instrumentation

  IF (instrument) THEN
     !$ nThreads= OMP_GET_MAX_THREADS()-1
     ALLOCATE(wt(0:nThreads))
     DO iThread = 0, nThreads
        wt(iThread) = CreateWatch(2, 1)
        CALL NameWatch(wt(iThread), 1,"Initialize     ")
        CALL NameWatch(wt(iThread), 2,"Integrate      ")
     END DO
  END IF

  ! read name list and fill all options

  CALL ReadNameList()

  fsbc    =.TRUE.

  ALLOCATE(cehl(0:maxtid))
  ALLOCATE(cehr(1:maxtid))
  ALLOCATE(cdhr(1:maxtid))
  !
  !     Get Initial Labels and Vertical Discretization
  !

  ALLOCATE (si_in (vert+1))
  ALLOCATE (sl_in (vert  ))
  ALLOCATE (del_in(vert  ))
  ALLOCATE (ct_in (vert  ))
  ALLOCATE (cq_in (vert  ))

  IF (myid < 100) WRITE(PRC,'(a1,i3.3)')'P',myid
  IF (myid >= 100 .AND. myid < 1000) WRITE(PRC,'(a1,i3)')'P',myid

  CALL CreateFileName()

  IF (TRIM(start) == "warm" ) THEN
     OPEN(UNIT=nfin1, FILE=TRIM(fNameInput1)//TRIM(PRC), FORM='unformatted', ACCESS='sequential', &
          ACTION='read', STATUS='old',IOSTAT=ierr)
     IF (ierr /= 0) THEN
        WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
             TRIM(fNameInput1), ierr
        STOP "**(ERROR)**"
     END IF
     CALL GReadHead (nfin1, ifday, tod, idate, idatec, si_in, sl_in, vert)
     CALL InitTimeStamp (DateInit_s, idate)
     labeli  = DateInit_s
     CALL InitTimeStamp (DateInit_s, idatec)
     labelc  = DateInit_s
     WRITE(labelf,'(I4.4, 3I2.2)' ) (idatef(i),i=4,1,-1)
     nfcnv0 = nfcnv1
     nfsibi = nfsibo
     CLOSE(UNIT=nfin1)
  ELSE
     OPEN(UNIT=nfin1, FILE=TRIM(fNameInput1), FORM='unformatted',  ACCESS='sequential',&
          ACTION='read', STATUS='old', IOSTAT=ierr)
     IF (ierr /= 0) THEN
        WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
             TRIM(fNameInput1), ierr
        STOP "**(ERROR)**"
     END IF

     CALL ReadHead (nfin1, ifday, tod, idate, idatec, si_in, sl_in, vert)
     CALL InitTimeStamp (DateInit_s, idate)
     labeli  = DateInit_s
     WRITE(labelf,'(I4.4, 3I2.2)' ) (idatef(i),i=4,1,-1)
     labelc  = labelf
     idatec  = idate
     REWIND (nfin1)
  END IF
  !
  ! initialize modules
  !
  IF(TRIM(isimp).EQ.'YES')iqdif='NO'
  CALL InitAll(trunc, vert, reducedGrid, linearGrid, mgiven, gaussgiven, &
       ct_in, cq_in, si_in, sl_in, del_in, dk, tk)

  SumDel=SUM(del_in)
  IF( TRIM(start) == "warm" )THEN
     fNameInput0=TRIM(fNameInput0)//TRIM(PRC)
     fNameInput1=TRIM(fNameInput1)//TRIM(PRC)
  END IF
  IF (ABS(1.0_r8-SumDel) > 1.0E-5_r8) THEN
     WRITE (UNIT=nfprt, FMT='(A,1PG12.5,A)') ' 1-SUM(del_in) = ', 1.0_r8-SumDel, ' > 1.0E-5_r8 '
     STOP ' Main : (ABS(1.0_r8-SumDel) > 1.0E-5_r8)'
  END IF
  CALL InitInputOutput (mMax, nMax , mnMax, kmax, &
       path_in, fNameCnfTbl, &
       fNameCnf2Tb, fNameLookTb, fNameUnitTb)

  CALL InitDiagnostics (doprec, dodyn  ,    colrad , &
       mMax   ,nMax   ,mnMax  , iMax   ,   jMax, &
       kMax   ,ibMax  ,jbMax  , ibMaxPerJB, fNameDTable)

  CALL InitReadWriteSpec(&
       mxavl   ,icf     ,mMax        , &
       mnMax   ,kMax    ,ijMaxGauQua ,iMax    ,jMax    ,ibMax   , &
       jbMax   , &
       reqdg   ,combf   ,dodia   ,itavl   ,iavrq   ,&
       nucf    ,lvrq    ,nurq    ,lvcf    ,itcf    )

  CALL InitFieldsPhyscs(ibMax, kMax, jbMax,iMax,jMax)
  DO jdt=0,maxtim
     cehl(jdt)=.FALSE.
     cdhl(jdt)=.FALSE.
     cthl(jdt)=.FALSE.
  END DO
  !
  dogwd=1
  CALL InitVariancia(igwd  ,nfvar ,fNameOrgvar)
  delt     = ddelt
  IF (nstep.EQ.1) nstep=7

  IF(TRIM(enhdif).EQ.'YES') THEN
     enhdifl = .TRUE.
  ELSE
     enhdifl = .FALSE.
  ENDIF

  IF(TRIM(isimp).EQ.'YES') THEN
     WRITE(UNIT=nfprt,FMT=7)
     initlz=0
     nfcnv0=0
     igwd  ='NO'
  END IF
  !
  ! Initialize modules
  !
  IF (slagr) THEN
     CALL InitSL
     iovmax = 2
   ELSE
     iovmax = 0
  END IF
  CALL InitPBLDriver(kmax, sl, del_in, si)
  CALL InitGWDDDriver(kmax,  si)
  CALL InitConvection(si, del_in, sl, cl, kmax)
  CALL InitSurface(ibMax ,jbMax,del_in,path_in,fNameSibVeg,fNameSibAlb)
  CALL InitRadiationDriver(monl,yrl,kmax,sl,nls)
  !
  ! Write problem options to stdio
  !
  CALL DumpOptions()
  !
  ! prepare output files
  !
  roperm  = dirfNameOutput
  namee   = "GPRG"//TRIM(PREFX)
  namef   = "GFCT"//TRIM(PREFX)
  !
  ! Create restart file name   
  !  
  CALL CreateFileName(TRIM(PRC),1)
  IF (nhdhn /= 0) THEN
      !
      ! Create DHN file name   
      !  
     CALL CreateFileName(TRIM(PRC),"GDHN")
     OPEN (UNIT=nfdhn, FILE=TRIM(FNameGDHN), FORM='unformatted', ACCESS='sequential',&
          ACTION='write', STATUS='replace',IOSTAT=ierr)
     IF (ierr /= 0) THEN
        WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
             TRIM(FNameGDHN), ierr
        STOP "**(ERROR)**"
     END IF
  END IF

  IF (dodyn) THEN
      !
      ! Create Dynamics file name   
      !  
     CALL CreateFileName(TRIM(PRC),"GDYN")
     OPEN (UNIT=nfdyn, FILE=TRIM(FNameGDYN),FORM='unformatted', ACCESS='sequential',&
          ACTION='write', STATUS='replace',IOSTAT=ierr)
     IF (ierr /= 0) THEN
        WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
             TRIM(FNameGDYN), ierr
        STOP "**(ERROR)**"
     END IF
  END IF

  IF (doprec) THEN
     CALL CreateFileName(TRIM(PRC),"GPRC")
     OPEN (UNIT=nfprc, FILE=TRIM(FNameGPRC), FORM='unformatted', ACCESS='sequential',&
          ACTION='write', STATUS='replace',IOSTAT=ierr)
     IF (ierr /= 0) THEN
        WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
             TRIM(FNameGPRC), ierr
        STOP "**(ERROR)**"
     END IF
  END IF

  IF (grhflg) THEN
     CALL CreateFileName(TRIM(PRC),"GFGH")
     IF(myid ==0 ) THEN
        OPEN (UNIT=nfghdr, FILE=TRIM(FNamenDrGH), FORM='formatted', ACCESS='sequential',&
             ACTION='write',STATUS='replace',IOSTAT=ierr)
        IF (ierr /= 0) THEN
           WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                TRIM(FNamenDrGH), ierr
           STOP "**(ERROR)**"
        END IF
     END IF
  END IF


  IF( TRIM(start) == "warm" )THEN
     nfcnv0 = nfcnv1
     nfsibi = nfsibo
     OPEN (UNIT=nfsibi, FILE=TRIM(FNameSibPrgInp0),FORM='unformatted',ACCESS='sequential',&
          ACTION='read',STATUS='old',IOSTAT=ierr)
     IF (ierr /= 0) THEN
        WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
             TRIM(FNameSibPrgInp0), ierr
        STOP "**(ERROR)**"
     END IF
     OPEN (UNIT=nfcnv0, FILE=TRIM(FNameConvClInp0),FORM='unformatted',ACCESS='sequential',&
          ACTION='read',STATUS='old',IOSTAT=ierr)
     IF (ierr /= 0) THEN
        WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
             TRIM(FNameConvClInp0), ierr
        STOP "**(ERROR)**"
     END IF
  ENDIF
  !
  !     read cloud dataset - logic assumes that initialization not
  !     performed for warm start
  !
  jdt=0
  CALL InitGridHistory (del_in ,idate ,iovmax, allghf,grhflg,nfghds  , &
       nfghloc   ,nfghdr ,iMax  ,jMax  ,ibMax ,jbMax ,ibPerIJ, &
       jbPerIJ,kMax,ibMaxPerJB,iMaxPerJ)


  ALLOCATE(lsmk(ijMaxGauQua))
  ALLOCATE(rlsm(imax,jmax))
  IF (ifsst .GE. 4) THEN
     CALL LandSeaMask(ifsst,labeli,intsst,sstlag,fNameSSTAOI,rlsm)
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
  CALL InitBoundCond(ibMax,jbMax,kMax,ifdy,todcld,ids,idc,ifday, &
       tod ,todsib,idate,idatec,si,sl, record_type,&
       fNameSoilType,fNameVegType,fNameSoilMoist,&
       fNameSibmsk,fNameTg3zrl,ibMaxPerJB)

  !XLAND (1:ibMax,1:jbMax) !-- XLAND land mask (1 for land, 2 for water)
  DO j=1,jbmax
     DO i=1,ibmax
        IF(imask(i,j)>0 ) THEN
           XLAND(i,j)=1
        ELSE
           XLAND(i,j)=2
        END IF
        IF(imask(i,j)<=0)XLAND(i,j)=2

        !***  CONVERT LAND MASK (1 FOR SEA; 0 FOR LAND)
        SEAMASK (i,j)=XLAND(i,j)-1.0
     END DO
  END DO
  LOWLYR=1
  !
  RESTART=nfin0 .NE. nfin1
  !
  tod=0.0_r8
  ! passive scalars are not used in normal mode initialization
  !
  nsca_save = nscalars
  nscalars = 0
  IF(iabs(initlz).GE.1.AND.initlz.GT.-3) THEN
     CALL MsgOne(h," Init: Diabatic Tendencies")
     CALL rsdiag
     CALL Diaten(slagr,fNameInput0,ifday, tod, idate, idatec)
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
     fgqm = 0.0_r8
     fgqmm = 0.0_r8
     fgum = 0.0_r8
     fgvm = 0.0_r8
     fgtmpm = 0.0_r8
     fgdivm = 0.0_r8
     fgtlamm = 0.0_r8
     fgtphim = 0.0_r8
     fglnpm = 0.0_r8
     fgplamm = 0.0_r8
     fgpphim = 0.0_r8
  END IF

  ALLOCATE(qgzs_orig(2*myMNMax))
  ALLOCATE(tequi(ibMax, kMax, jbMax))
  CLOSE(UNIT=nfin1)
  OPEN(UNIT=nfin0, FILE=TRIM(fNameInput0), FORM='unformatted',ACCESS='sequential',&
       ACTION='read', STATUS='old', IOSTAT=ierr)
  IF (ierr /= 0) THEN
     WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
          TRIM(fNameInput0), ierr
     STOP "**(ERROR)**"
  END IF
  OPEN(UNIT=nfin1, FILE=TRIM(fNameInput1), FORM='unformatted',ACCESS='sequential', &
       ACTION='read', STATUS='old',  IOSTAT=ierr)
  IF (ierr /= 0) THEN
     WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
          TRIM(fNameInput1), ierr
     STOP "**(ERROR)**"
  END IF
  !
  !     start reading initial values (t=t   )
  !
  IF (nfin0 .EQ. nfin1) THEN
     !
     ! cold inicialization
     !
     restart = .FALSE.
     CALL gread4 (nfin0, ifday, tod  , idate, idatec,qgzs  ,qlnpp , &
          qtmpp, qdivp, qrotp, qqp  , sl    , si   ,dodyn , &
          nfdyn)
     qgzs_orig = qgzs
     REWIND nfin0
  ELSE
     !
     ! warm inicialization
     !
     restart = .TRUE.
     READ(UNIT=nfin0) fgqm, fgum, fgvm, fgtmpm, fgdivm, fglnpm, fgtlamm, &
          fgtphim, fgplamm, fgpphim
     IF (slagr) THEN
        READ(UNIT=nfin0) ulonm ,ulatm ,usigm,ulonm2D,ulatm2D
     END IF
     if (nscalars>0) then
        read(UNIT=nfin0) iold,iact,inew,fgpass_scalars
     endif
     IF (TRIM(isimp) == 'YES') THEN
        READ(UNIT=nfin0)tequi
     END IF

     READ(UNIT=nfin1) ifday,tod,idate,idatec,si,sl
     READ(UNIT=nfin1) qgzs,qlnpp,qtmpp,qdivp,qrotp,qqp,total_mass
     qgzs_orig = qgzs
     REWIND nfin0
     REWIND nfin1
  END IF

  !
  IF (TRIM(isimp) == 'YES') THEN
     IF (TRIM(start) == "cold") THEN
        !$OMP PARALLEL
        CALL InitBoundSimpPhys ()
        !$OMP END PARALLEL
     END IF
     CALL InitSimpPhys(fgtmp,tequi,sl,dt)
  END IF
  !
  IF(ifsst.GT.3.AND.sstlag.LT.zero)THEN
     WRITE(UNIT=nfprt,FMT=336)ifsst,sstlag
     STOP 336
  END IF
  !
  !
  !     write diagnostics/prognostics directory
  !
  !
  !     write uninitialized initial condition prognostics on tape
  !     the use of swrk in wrprog destroys wsib
  !
  IF(ifday.EQ.0.AND.tod.EQ.zero) THEN
     CALL wrprog (nfdrct,nfdiag ,ifday ,tod   ,idate ,idatec ,qrotp , &
          qdivp ,qqp    ,qlnpp ,qtmpp ,z0  ,gtsea  ,td0   , &
          capac0,w0     ,imask ,temp2m,umes2m,uve10m,vve10m,nffcst   ,del_in, &
          qgzs_orig,lsmk, &
          roperm,namef  ,labeli,labelf,extw  ,exdw   ,TRIM(TRCG), &
          TRIM(LV))
     IF (mextfm .GT. 0) &
          CALL weprog (nedrct,neprog,nefcst,ifday ,tod    ,idate ,idatec , &
          del_in,qgzs_orig,lsmk,qlnpp ,qdivp  , &
          qrotp ,qqp   ,qtmpp ,z0  ,gtsea  ,td0   ,capac0 , &
          w0    ,imask ,temp2m,umes2m,uve10m,vve10m,roperm,namee ,labeli ,labelf,extw   , &
          exdw  ,TRIM(TRCG),TRIM(LV) )
  END IF
  !
  ! compute the spectral coefficients of Laplacian of topography
  !
  CALL SfcGeoTrans(slagr)

  CALL WriteGridHistoryTopo (fgzs,FNameTopGH,nfghtop)

  !
  ! compute normal modes if necessary
  !
  IF (eigeninit) THEN
     IF (myid.eq.0) CALL Getmod(.FALSE.,fNameNmi,rsettov)
     CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
  END IF
  !
  ! Non-linear normal mode initialization
  !
  IF (nlnminit) THEN
     !
     !     do machenhauer's non-linear normal mode initialization
     !
     IF(iabs(initlz).EQ.2) THEN
        CALL MsgOne(h," Init: Non-linear Normal Modes")
        CALL rsdiag

        CALL Nlnmi(nlnminit,diabatic,.FALSE.,fNameNmi, ifday, tod, idatec, ktm)

        CALL wrprog (nfdrct,nfdiag,ifday ,tod   ,idate ,idatec,qrotp , &
             qdivp ,qqp   ,qlnpp ,qtmpp ,z0  ,gtsea ,td0   , &
             capac0,w0    ,imask ,temp2m,umes2m,uve10m,vve10m,nffcst  ,del_in, &
             qgzs_orig,lsmk, &
             roperm,namef ,labeli,labelf,extw  ,exdw  ,TRIM(TRCG), &
             TRIM(LV))

        IF (mextfm .GT. 0) &
             CALL weprog (nedrct,neprog,nefcst,ifday ,tod   ,idate ,idatec, &
             del_in,qgzs_orig,lsmk,qlnpp,qdivp , &
             qrotp ,qqp   ,qtmpp ,z0  ,gtsea ,td0   ,capac0, &
             w0    ,imask ,temp2m,umes2m,uve10m,vve10m,roperm,namee ,labeli,labelf,extw  , &
             exdw  ,TRIM(TRCG),TRIM(LV) )

        ! Writes the Initialized Fields as an Initial Condition for Further Run

        IF (maxtim <= 0) THEN
           IF(myid.eq.0) THEN
              OPEN (UNIT=nfout1, FILE=TRIM(FNameRestInput2),FORM='unformatted', &
                   ACCESS='sequential',ACTION='write', STATUS='replace', IOSTAT=ierr)
              IF (ierr /= 0) THEN
                 WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                      TRIM(FNameRestInput2), ierr
                 STOP "**(ERROR)**"
              END IF
           END IF
           CALL gwrite(nfout1,ifday ,tod   ,idate ,idatec,qlnpp ,qtmpp , &
                qdivp ,qrotp ,qqp   ,sl    ,si    ,qgzs_orig)
           IF(myid.eq.0) CLOSE(UNIT=nfout1)
        END IF
     END IF
     nlnminit = .FALSE.
  END IF

  ! Force stop when labeli=labec=labelf
  IF (maxtim <= 0) STOP ' Model Ended Without Time Integration'

  fdh=24.0_r8
  dth=86400.0_r8/fdh
  delth=delt/dth
  fdayh=REAL(ifday,r8)*fdh+tod/dth
  cthw=fdayh

  CALL SetOutPut (tod,idatec)

  IF (dctd .GT. 0.0_r8) THEN
     cdhr(1)=ctdh0
     DO l=2,mdxtfm
        cdhr(l)=cdhr(l-1)+dctd
     END DO
     cdhr(mdxtfm:maxtid) = -100.0_r8
  ELSE
     cdhr = -100.0_r8
  END IF

  IF (dcte .GT. 0.0_r8) THEN
     !hmjb: This is the same as doing:
     !    cehr(l)=cteh0+dcte*(l-1)
     ! but avoids the multiplications
     cehr(1)=cteh0
     DO l=2,mextfm
        cehr(l)=cehr(l-1)+dcte
     END DO
  END IF

!!$  WRITE(UNIT=nfprt,FMT=*)' '
!!$  WRITE(UNIT=nfprt,FMT=*)' Time Step List Output, cehl:'

!hmjb
! Searching cehr() between 1:mextfm may take too long if 
! the user set NHEXT>50years. This might be the case, for
! instance, when running a long climatic run and asking 
! the model to save PRG's every 6hr until the end of the
! run.
! 
! The alternative code below uses the fact that 
!    cehr(l)=cteh0+dcte*(l-1)
! and 
!    -0.00001_r8 <= cehr(l)-cthw <= 0.00001_r8
! hence
!    (chtw-cteh0-0.00001_r8)/dcte <= l <= (chtw-cteh0+0.00001_r8)/dcte
!
! Therefore, cehl(jdt)=T only when there is an integer
! value between these two real limits.
!

  cthw=fdayh
  DO jdt=1,maxtim
     cthw=cthw+delth
     l1 = 1+CEILING((cthw-cteh0-0.00001_r8)/dcte)
     l2 = 1+FLOOR  ((cthw-cteh0+0.00001_r8)/dcte)
     IF (l1.eq.l2) THEN
        cehl(jdt)=.TRUE.
     ENDIF
  END DO

  cthw=fdayh
  DO jdt=1,maxtim
     cthw=fdayh + (jdt-1)*delth
     DO l=1,maxtfm
        IF (ABS(cdhr(l)-cthw) .LE. 0.00001_r8) THEN
           cdhl(jdt)=.TRUE.
        END IF
     END DO
  END DO
  !
  maxt0=NINT((REAL(ifday,r8)*fdh+tod/dth)/delth)
  !
  !     this is to remove accumulations from nlnmi
  !
  geshem=0.0_r8
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
     CALL MsgOne(h," Init: Cold Start")
     !
     !     read cloud dataset for cold start
     !
     CALL InitCheckfile(ibMax,&
          jbMax  ,kMax, ifdy  ,todcld,ids   ,idc   ,ifday , &
          tod   ,idate ,idatec,todsib  ,ibMaxPerJB)
     !
     !     cold start (at first delt/4 ,then delt/2 )
     !
     limlow =2
     dt= delt /4.0_r8
     !
     ! filter arguments for first time step
     !
     fa  = 0.0_r8
     fb  = 1.0_r8
     fb1 = 1.0_r8
     nscalars = nsca_save
     iold   = 1
     iact   = 2
     inew   = 3
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
        IF(nfctrl(7).GE.1)WRITE(UNIT=nfprt,FMT=104) jdt
        !
        !     calculate matrices for semi-implicit integration
        !
        delta2 = dt
        IF(slagr.and.SL_twotime_scheme) delta2 = dt/2._r8
        CALL bmcm(delta2, slagr)
        !
        ! perform time step
        !
        ifilt=0
        !$OMP PARALLEL
        CALL TimeStep(fb1,fa,fb,slagr,nlnminit,.FALSE.,.FALSE.,dt,jdt, &
             ifday,tod, idatec,jdt)
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

        dt=dt*2.0_r8
        IF(IsGridHistoryOn())THEN
           IF(myid.eq.0)THEN
              OPEN (UNIT=nfghou, FILE=TRIM(FNameOutGH),FORM='unformatted',&
                   ACCESS='sequential',ACTION='write',STATUS='replace',IOSTAT=ierr)
              IF (ierr /= 0) THEN
                 WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                      TRIM(FNameOutGH), ierr
                 STOP "**(ERROR)**"
              END IF
           END IF
           CALL WriteGridHistory (nfghou, ifday, tod, idate)
        END IF
        ktm=kt
        fsbc=.FALSE.
        CALL MsgOne(h,"Time integration starts")
        StartStorDiag=.TRUE.
        fb1 = 0.0_r8
     END DO
     tod=dt !PYK
  ELSE
     CALL MsgOne(h,"Time integration starts")
     StartStorDiag=.TRUE.
  END IF
  !
  !     smooth start
  !
  IF(TRIM(igwd).EQ.'YES')dogwd=0
  ahour=ifday*24.0_r8+tod/3600.0_r8
  istrt=0
  !
  ! semi-implicit matrices
  !
  delta2 = dt
  IF(slagr.and.SL_twotime_scheme) delta2 = dt/2._r8
  CALL bmcm(delta2, slagr)
  !
  ! filter arguments for all remaining time steps
  !
  fa     = filta
  fb     = filtb
  fb1    = 0.0_r8
  IF(slagr.and.SL_twotime_scheme) THEN
     fa     = 1.0_r8
     fb     = 0.0_r8
  END IF
  IF( TRIM(start) == "warm" )THEN
     CLOSE(UNIT=nfsibi)
     CLOSE(UNIT=nfcnv0)
     fb1    = fb
     IF(slagr.and.SL_twotime_scheme)   fb1 = 0.0_r8
  ENDIF
  !
  ! time step loop
  !
  ifilt=1
  !$OMP PARALLEL PRIVATE(ljdt)
  IF (instrument) THEN
     iThread = 0
     !$ iThread = OMP_GET_THREAD_NUM()
     CALL ChangeWatch(wt(iThread), 2)
  END IF
  DO ljdt=limlow,maxtim
     !
     !     step loop starts
     !
     IF(TRIM(isimp).NE.'YES') THEN
        !$OMP SINGLE
        CALL InitGetsbc(ifday, tod, idate, idatec,ibMax,jbMax,kMax,ibMaxPerJB)
        !$OMP END SINGLE
     END IF
     jdt = ljdt
     !$OMP SINGLE
     tod=tod+dt
     IF(ABS( MOD(tod+0.03125_r8,86400.0_r8)-0.03125_r8).LT.0.0625_r8)THEN
        tod=0.0_r8
        ifday=ifday+1
     END IF
     CALL tmstmp2(idate,ifday,tod,jhr,jday,jmon,jyr)
     idatec(1)=jhr
     idatec(2)=jmon
     idatec(3)=jday
     idatec(4)=jyr
     ahour=(ifday*24.0e0_r8)+(tod/3.6e3_r8)
     !$OMP END SINGLE

     kt   =INT(ahour-(1.0e-2_r8))
     ktp  =INT(ahour+(dt/3.6e3_r8)-(1.0e-2_r8))
     IF(jdt.EQ.maxtim) THEN
        ktm=kt
     END IF
     !
     ! perform time step
     !
     CALL TimeStep(fb1,fa,fb,slagr,nlnminit,.FALSE.,enhdifl,dt,jdt, &
          ifday,tod,idatec,jdt)
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
        CALL WriteGridHistory (nfghou, ifday, tod, idate)
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
        WRITE(labeli,'(I4.4,3I2.2)')idate (4),idate (2),idate (3),idate (1)
        WRITE(labelc,'(I4.4,3I2.2)')idatec(4),idatec(2),idatec(3),idatec(1)

        IF (rmRestFiles) THEN

           OPEN(UNIT=nfsibo,FILE=TRIM(FNameSibPrgInp0),FORM='unformatted',ACCESS='sequential',&
                ACTION='write',STATUS='replace',IOSTAT=ierr)
           IF (ierr /= 0) THEN
              WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                   TRIM(FNameSibPrgInp0), ierr
              STOP "**(ERROR)**"
           END IF

           OPEN(UNIT=nfcnv1, FILE=TRIM(FNameConvClInp0 ),FORM='unformatted',ACCESS='sequential',&
                ACTION='write',STATUS='replace',IOSTAT=ierr)
           IF (ierr /= 0) THEN
              WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                   TRIM(FNameConvClInp0 ), ierr
              STOP "**(ERROR)**"
           END IF

           OPEN(UNIT=nfout1, FILE=TRIM(FNameRestInput2),FORM='unformatted',ACCESS='sequential',&
                ACTION='write',STATUS='replace',IOSTAT=ierr)
           IF (ierr /= 0) THEN
              WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                   TRIM(FNameRestInput2), ierr
              STOP "**(ERROR)**"
           END IF

           OPEN(UNIT=nfout0, FILE=TRIM(FNameRestInput1),FORM='unformatted',ACCESS='sequential',&
                ACTION='write', STATUS='replace',IOSTAT=ierr)
           IF (ierr /= 0) THEN
              WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                   TRIM(FNameRestInput1), ierr
              STOP "**(ERROR)**"
           END IF

           CLOSE(UNIT=nfout0,STATUS='DELETE')
           CLOSE(UNIT=nfsibo,STATUS='DELETE')
           CLOSE(UNIT=nfcnv1,STATUS='DELETE')
           CLOSE(UNIT=nfout1,STATUS='DELETE')

        END IF

        CALL CreateFileName(TRIM(PRC),2)

        OPEN(UNIT=nfsibo, FILE=TRIM(FNameSibPrgOut1), FORM='unformatted',ACCESS='sequential',&
             ACTION='write',STATUS='replace',IOSTAT=ierr)
        IF (ierr /= 0) THEN
           WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                TRIM(FNameSibPrgOut1), ierr
           STOP "**(ERROR)**"
        END IF
        OPEN(UNIT=nfcnv1, FILE=TRIM(FNameConvClOut1), FORM='unformatted',ACCESS='sequential',&
             ACTION='write',STATUS='replace',IOSTAT=ierr)
        IF (ierr /= 0) THEN
           WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                TRIM(FNameConvClOut1), ierr
           STOP "**(ERROR)**"
        END IF
        OPEN(UNIT=nfout1, FILE=TRIM(FNameRestOutput2), FORM='unformatted',ACCESS='sequential',&
             ACTION='write',STATUS='replace',IOSTAT=ierr)
        IF (ierr /= 0) THEN
           WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                TRIM(FNameRestOutput2), ierr
           STOP "**(ERROR)**"
        END IF
        OPEN(UNIT=nfout0, FILE=TRIM(FNameRestOutput1), FORM='unformatted',ACCESS='sequential',&
             ACTION='write',STATUS='replace',IOSTAT=ierr)
        IF (ierr /= 0) THEN
           WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                TRIM(FNameRestOutput1), ierr
           STOP "**(ERROR)**"
        END IF

        WRITE(UNIT=nfout1) ifday,tod,idate,idatec,si,sl
        WRITE(UNIT=nfout1) qgzs_orig,qlnpp,qtmpp,qdivp,qrotp,qqp,total_mass
        WRITE(UNIT=nfout0) fgqm, fgum, fgvm, fgtmpm, fgdivm, fglnpm, fgtlamm, &
             fgtphim, fgplamm, fgpphim

        IF (slagr) THEN
           WRITE(UNIT=nfout0)ulonm ,ulatm ,usigm,ulonm2D,ulatm2D
        END IF
        IF (nscalars>0) THEN
           WRITE(UNIT=nfout0) iold,iact,inew,fgpass_scalars
        END IF
        IF (TRIM(isimp) == 'YES') THEN
           WRITE(UNIT=nfout0)tequi
        END IF

        !$OMP END SINGLE

        CALL restartphyscs (jbMax,ifday,tod ,idate ,idatec, &
             nfsibo,nfcnv1,ibMaxPerJB)

        !$OMP SINGLE
        FNameRestInput2 = FNameRestOutput2
        FNameRestInput1 = FNameRestOutput1
        FNameConvClInp0 = FNameConvClOut1
        FNameSibPrgInp0 = FNameSibPrgOut1
        CLOSE(UNIT=nfsibo)
        CLOSE(UNIT=nfcnv1)
        CLOSE(UNIT=nfout1)
        CLOSE(UNIT=nfout0)

        !$OMP END SINGLE
     ENDIF

     !$OMP SINGLE
     IF(cthl(jdt)) THEN
        maxstp=NINT((REAL(ifday,r8)*fdh+tod/dth)/delth)-maxt0

!!$        WRITE(UNIT=nfprt,FMT="('Write file at timestep ',i5)") jdt

        !
        !     reset precip. every maxstp time steps
        !
        CALL wrprog (nfdrct,nfdiag,ifday ,tod   ,idate ,idatec,qrotp , &
             qdivp ,qqp   ,qlnpp ,qtmpp ,z0  ,gtsea ,td0   , &
             capac0,w0    ,imask ,temp2m,umes2m,uve10m,vve10m,nffcst  ,del_in, &
             qgzs_orig,lsmk, &
             roperm,namef ,labeli,labelf,extw  ,exdw  ,TRIM(TRCG), &
             TRIM(LV))
        CALL wridia(nfdiag, maxstp, idatec)
        !
        !     zero reset diagnostic fields
        !
        CALL rsdiag
        geshem=0.0_r8
        limlow=1

        IF(jdt.NE.maxtim)THEN
           maxt0=NINT((REAL(ifday,r8)*fdh+tod/dth)/delth)
        END IF
     END IF

     IF (cehl(jdt)) THEN
        maxstp=NINT((REAL(ifday,r8)*fdh+tod/dth)/delth)-maxt0

        IF(nfctrl(8).GE.1) THEN
!!$           WRITE(UNIT=nfprt,FMT=102)dt,ifday,tod, &
!!$                REAL(ifday,r8)*fdh+tod/dth,jdt,maxstp
        END IF

        IF (mextfm .GT. 0) &
             CALL weprog (nedrct,neprog,nefcst,ifday ,tod   ,idate ,idatec, &
             del_in,qgzs_orig,lsmk,qlnpp,qdivp , &
             qrotp ,qqp   ,qtmpp,z0   ,gtsea ,td0   ,capac0, &
             w0    ,imask ,temp2m,umes2m,uve10m,vve10m,roperm,namee ,labeli,labelf,extw  , &
             exdw  ,TRIM(TRCG),TRIM(LV) )
     END IF


     !$OMP END SINGLE
     fb1 = fb
     IF(slagr.and.SL_twotime_scheme)   fb1 = 0.0_r8
  ENDDO
  !$OMP END PARALLEL
  REWIND nfout0
  !
  !  finish MPI
  !
  IF (instrument) THEN
     DO iThread = 0, nThreads
        CALL DumpWatch(wt(iThread), unitDump)
     END DO
     DO iThread = 0, nThreads
        CALL DestroyWatch(wt(iThread))
     END DO
  END IF
  CALL DestroyParallelism("***MODEL EXECUTION ENDS NORMALY***")
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
