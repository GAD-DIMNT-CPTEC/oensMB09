!
!  $Author: pkubota $
!  $Date: 2011/04/20 14:34:46 $
!  $Revision: 1.25 $
!
MODULE FieldsPhysics
  USE SFC_SSiB, ONLY: &
       vegin,        &
       sibwet,       &
       sibwet_GLSM,       &
       re_assign_sib_soil_prop

  USE IOLowLevel, ONLY: &
       ReadVar      , &
       ReadGetNFTGZ

  USE InputOutput, ONLY: &
       WillGetSbc, &
       getsbc

  USE Parallelism, ONLY: &
       MsgOne, FatalError

  USE Options, ONLY: &
       reducedGrid,  &
       isimp , &
       nfcnv0, &
       nfsibt, &
       nfsoiltp, &
       nfvegtp, &
       nfslmtp, &
       initlz, &
       ifalb , &
       ifsst , &
       ifslm , &
       ifsnw , &
       ifozone, &
       sstlag, &
       intsst, &
       fint  , &
       yrl   , &
       monl  , &
       nftgz0, &
       mxiter, &
       nfsibi, &
       nfprt , &
       nfctrl , &
       iglsm_w,&
       nfzol,&
       fNameRouLen

  USE Utils, ONLY: &
       IJtoIBJB, &
       LinearIBJBtoIJ, &
       LinearIJtoIBJB, &
       NearestIBJBtoIJ, &
       NearestIJtoIBJB, &
       SeaMaskIJtoIBJB, &
       SeaMaskIBJBtoIJ, &
       SplineIBJBtoIJ, &
       SplineIJtoIBJB, &
       AveBoxIBJBtoIJ, &
       AveBoxIJtoIBJB, &
       FreqBoxIJtoIBJB, &
       IBJBtoIJ, &
       vfirec


  USE Sizes, ONLY: &
       sl

  USE Constants, ONLY: &
       rk,r8,r4,i8,i4

  IMPLICIT NONE

  ! Gaussian fields: 28 3D , 12 2D and 12 1D

  PRIVATE

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:)   :: sigki ! Fator de conversao de temperatura potencial

  !---------------------------------------------------------------------------------------------------------------
  ! SHORT WAVE RADIATION
  !---------------------------------------------------------------------------------------------------------------

  ! Coeficiente de transporte vertical

  ! Viscosity (turbulencia)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: PBL_CoefKm ! m2/s momentum
  ! Scalar diffusivity (water)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: PBL_CoefKh ! m2/s water and heat

  ! Radiation fields at next integer hour
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: yVisBeam ! Down Sfc SW flux visible beam    (all-sky)  (W/m^2)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: yVisDiff ! Down Sfc SW flux visible diffuse (all-sky)  (W/m^2)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: yNirBeam ! Down Sfc SW flux Near-IR beam    (all-sky)  (W/m^2)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: yNirDiff ! Down Sfc SW flux Near-IR diffuse (all-sky)  (W/m^2)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: yVisBeamC! Down Sfc SW flux visible beam    (clear)  (W/m^2) 
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: yVisDiffC! Down Sfc SW flux visible diffuse (clear)  (W/m^2) 
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: yNirBeamC! Down Sfc SW flux Near-IR beam    (clear)  (W/m^2) 
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: yNirDiffC! Down Sfc SW flux Near-IR diffuse (clear)  (W/m^2) 
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: ySwHeatRate ! Heating rate due to shortwave         (K/s)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: ySwHeatRatec! Heating rate due to shortwave (clear) (K/s)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ySwToaDown! Incident SW at top (W/m^2)                        
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ySwSfcNet ! Abs Sfc SW 
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ySwSfcNetC! Abs Sfc SW (clear) 

  ! Radiation fields at last integer hour
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rVisBeam ! Down Sfc SW flux visible beam    (all-sky)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rVisDiff ! Down Sfc SW flux visible diffuse (all-sky)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rNirBeam ! Down Sfc SW flux Near-IR beam    (all-sky)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rNirDiff ! Down Sfc SW flux Near-IR diffuse (all-sky)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rVisBeamC! Down Sfc SW flux visible beam    (clear) 
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rVisDiffC! Down Sfc SW flux visible diffuse (clear) 
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rNirBeamC! Down Sfc SW flux Near-IR beam    (clear) 
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rNirDiffC! Down Sfc SW flux Near-IR diffuse (clear) 
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rSwToaDown! Incident SW at top (W/m^2)               
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rSwSfcNet ! Abs Sfc SW 
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rSwSfcNetC! Abs Sfc SW (clear) 

  ! Surface albedo for SW 
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: AlbVisBeam ! Visible beam surface albedo
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: AlbVisDiff ! Visible diffuse surface albedo
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: AlbNirBeam ! Near-ir beam surface albedo
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: AlbNirDiff ! Near-ir diffuse surface albedo

  !---------------------------------------------------------------------------------------------------------------
  ! LONG WAVE RADIATION
  !---------------------------------------------------------------------------------------------------------------

  ! LW Radiation fields at last integer hour
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: LwCoolRate ! Cooling rate due to longwave          (K/s)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: LwCoolRatec! Cooling rate due to longwave  (clear) (K/s)

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: LwSfcDown! Down Sfc LW flux         (W/m2)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: LwSfcDownC! Down Sfc LW flux (clear) (W/m2)

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: LwSfcNet    ! Net Sfc LW         (W/m2)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: LwSfcNetC ! Net Sfc LW (clear) (W/m2)

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: LwToaUp! Longwave upward at top           (W/m2)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: LwToaUpC! Longwave upward at top (clear)   (W/m2)

  !---------------------------------------------------------------------------------------------------------------
  ! CLOUDS FOR RADIATION
  !---------------------------------------------------------------------------------------------------------------

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: cldsav  ! Total Cloud cover

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: cldtot! Total cloud cover (at each layer)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: cldinv! Inversion clouds
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: cldsat! Saturation clouds
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: cldcon! Convection clouds
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: cldson! Shallow convective clouds

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: clwd  ! Cloud liquid water path. 
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: emisd ! emissivity
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: taud  ! Shortwave cloud optical depth

  !---------------------------------------------------------------------------------------------------------------
  ! CONVECTION
  !---------------------------------------------------------------------------------------------------------------

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   ppli ! Precipitation rate ( large scale ) ( mm/s)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   ppci ! Precipitation rate ( cumulus ) ( mm/s )
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prct ! cumulus and large scale scheme precipitation  (mm)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcc ! cumulus scheme precipitation (mm)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcp1! precipitation (cumulus) at each time step.(rrr) ( mm/s )
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcp2! precipitation (cumulus) at each time step.(rrr) ( mm/s )
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcp3! precipitation (cumulus) at each time step.(rrr) ( mm/s )
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcpt! precipitation(cumulus and large scale) at each time step(rrr)(mm/s)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   toplv! level of convective cloud top
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   botlv! level of convective cloud base
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   geshem! cumulus scheme precipitation (mm)

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: convc ! Convective cloud cover in 3 hr. avrage
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: convt ! Convective cloud top  (sigma layer)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: convb ! Convective cloud base (sigma layer)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: convts! Level sigma of the convective cloud cover in 3 hr. avrage
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: convcs! Level sigma of the convective cloud top  (sigma layer)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: convbs! Level sigma of the convective cloud base (sigma layer)

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: o3mix ! ozone mass mixing ratio (g/g)

  !---------------------------------------------------------------------------------------------------------------
  ! PBL
  !---------------------------------------------------------------------------------------------------------------

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ustr  ! Surface zonal stress
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: vstr  ! Surface meridional stress


  !---------------------------------------------------------------------------------------------------------------
  ! SURFACE/SSIB
  !---------------------------------------------------------------------------------------------------------------

  INTEGER(KIND=i8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: imask ! vegetation mask
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ssib  ! Fracao de umidade do solo

  !---------------------------------------------------------------------------------------------------------------
  ! OUTROS...
  !---------------------------------------------------------------------------------------------------------------

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: gl0   ! Maximum mixing length l0 in blackerdar's formula (m)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: Mmlen ! Maximum mixing length l0 in blackerdar's formula (m)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: zorl  ! Aero. Roughness length  (m)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: sheleg! Snow amount in (mm) (equivalent water depth)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: tseam !  IF (tseam < 0)  sea surface temp. (K)
                                                              !  IF (tseam > 0)  ground temp. (K)

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: gtsea ! IF (tseam < 0)  sea surface temp. (K)
                                                              ! IF (tseam > 0)  ground temp. (K)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tm0! prognostic surface of temparature (K) t+1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   qm0! prognostic surface of specific humid (kg/kg) t+1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tc0! "dossel" canopy of temparature (K) t+1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tg0! surface soil temperature (K)    t+1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   td0! dep soil temperature (K) t+1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: w0 !w0(id)Grau de saturacao de umid do solo id=1 na camada superficial t+1
                                                             !w0(id)Grau de saturacao de umid do solo id=2 na camada de raizes t+1
                                                             !w0(id)Grau de saturacao de umid do solo id=3 na camada de drenagem t+1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: capac0! capac0(iv). Agua interceptada iv=1 no dossel "water store capacity
                                                                !             of leaves"(m)  modificada t+1
                                                                ! capac0(iv). Agua interceptada iv=2 na cobertura do solo(m)modificada
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tmm! prognostic surface of temparature (K)  t-1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   qmm! prognostic surface of specific humid (kg/kg)  t-1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tcm! Temperatura da copa "dossel"(K)   modificada   t-1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tgm! Temperatura da superficie do solo  (K)   modificada  t-1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tdm! Temperatura do solo profundo (K)   modificada   t-1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: wm !w0(id)Grau de saturacao de umid do solo id=1 na camada superficial t+1
                                                             !w0(id)Grau de saturacao de umid do solo id=2 na camada de raizes t+1
                                                             !w0(id)Grau de saturacao de umid do solo id=3 na camada de drenagem t+1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: capacm! capac0(iv). Agua interceptada iv=1 no dossel"water store
                                                                !             capacity t+1 of leaves"(m)  modificada t-1
                                                                ! capac0(iv). Agua interceptada iv=2 na cobertura do solo(m)modific.

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   qsfc0! prognostic surface ocean temparature (K) t+1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tsfc0! prognostic surface ocean temparature (K) t+1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   qsfcm! prognostic surface ocean temparature (K) t-1
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tsfcm! prognostic surface ocean temparature (K) t-1

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   var  !Surface height variance (m**2)

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tg1 ! deep soil temp (K)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tg2 ! ground temperature (K)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tg3 ! canopy temperature (K)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   soilm ! total soil water in (mm)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   sens  ! sensible heat flux (w/m^2)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   evap  ! latent heat flux(W/m^2)

  INTEGER, PUBLIC, PARAMETER :: nzg     =8                !- total number of soil layers
  INTEGER, PUBLIC, PARAMETER :: npatches=5                !- total number of veg patches
  INTEGER, PUBLIC, PARAMETER :: npatches_actual=2         !- actual number of veg patches
  ! (must be =< npatches and >= 2)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: glsm_w   ! initial soil wetness data
  ! at soil model
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: wsib3d   ! initial soil wetness data
  ! at sib soil layers
  !
  !--common gl_sm - copie as mesmas linhas na gloop_slgm.f90 ----
  !
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: veg_type ! SIB veg type
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: frac_occ ! fractional area
  ! coverage
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:  ) :: soil_type! FAO/USDA soil texture

  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: xland! land mask (1 for land, 2 for water)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: seamask
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: z0
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ustar
  INTEGER      , PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: lowlyr
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: tkemyj
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: snow
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: THZ0
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: QZ0
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: UZ0
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: VZ0
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ZNT
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: PBLH
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: AKHS
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: AKMS
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: RMOL
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: CT
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: htdisp
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: temp2m
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: umes2m
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: uve10m
  REAL(KIND=r8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: vve10m
  INTEGER(KIND=i8), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: MskAnt

  INTEGER, PRIVATE :: iMax
  INTEGER, PRIVATE :: jMax
  INTEGER, PRIVATE :: ierr

  PUBLIC ::  InitFieldsPhyscs
  PUBLIC ::  InitVariancia
  PUBLIC ::  InitBoundCond
  PUBLIC ::  InitCheckfile
  PUBLIC ::  InitSurfTemp
  PUBLIC ::  InitGetsbc
  PUBLIC ::  restartphyscs
CONTAINS






  SUBROUTINE InitFieldsPhyscs(ibMax, kMax, jbMax,iMax_in,jMax_in)
    INTEGER, INTENT(IN) :: ibMax
    INTEGER, INTENT(IN) :: kMax
    INTEGER, INTENT(IN) :: jbMax
    INTEGER, INTENT(IN) :: iMax_in
    INTEGER, INTENT(IN) :: jMax_in
    INTEGER :: k

    iMax=iMax_in
    jMax=jMax_in
    !
    !   map(ib,jb)
    !
    ALLOCATE(sigki (kMax))
    DO k=1,kmax
       sigki (k)=1.0e0_r8/EXP(rk*LOG(sl(k)))
    END DO


    ! Coeficiente de transporte vertical para a turbulencia
    ALLOCATE(PBL_CoefKm (ibMax,kMax,jbMax))
    PBL_CoefKm = 0.0_r8
    ALLOCATE(PBL_CoefKh (ibMax,kMax,jbMax))
    PBL_CoefKh = 0.0_r8

    ! Radiation fields at next integer hour
    ALLOCATE(ySwToaDown(ibMax,jbMax))
    ySwToaDown=0.0_r8
    ALLOCATE(yVisBeam (ibMax,jbMax))
    yVisBeam =0.0_r8
    ALLOCATE(yVisDiff (ibMax,jbMax))
    yVisDiff =0.0_r8
    ALLOCATE(yNirBeam (ibMax,jbMax))
    yNirBeam =0.0_r8
    ALLOCATE(yNirDiff (ibMax,jbMax))
    yNirDiff =0.0_r8
    ALLOCATE(yVisBeamC(ibMax,jbMax))
    yVisBeamC=0.0_r8
    ALLOCATE(yVisDiffC(ibMax,jbMax))
    yVisDiffC=0.0_r8
    ALLOCATE(yNirBeamC(ibMax,jbMax))
    yNirBeamC=0.0_r8
    ALLOCATE(yNirDiffC(ibMax,jbMax))
    yNirDiffC=0.0_r8
    ALLOCATE(ySwHeatRate   (ibMax,kMax,jbMax))
    ySwHeatRate   =0.0_r8
    ALLOCATE(ySwHeatRatec  (ibMax,kMax,jbMax))
    ySwHeatRatec  =0.0_r8
    ALLOCATE(ySwSfcNet (ibMax,jbMax))
    ySwSfcNet =0.0_r8
    ALLOCATE(ySwSfcNetC(ibMax,jbMax))
    ySwSfcNetC=0.0_r8

    ! Radiation fields at last integer hour
    ALLOCATE(rSwToaDown(ibMax,jbMax))
    rSwToaDown=0.0_r8
    ALLOCATE(rVisBeam (ibMax,jbMax))
    rVisBeam =0.0_r8
    ALLOCATE(rVisDiff (ibMax,jbMax))
    rVisDiff =0.0_r8
    ALLOCATE(rNirBeam (ibMax,jbMax))
    rNirBeam =0.0_r8
    ALLOCATE(rNirDiff (ibMax,jbMax))
    rNirDiff =0.0_r8
    ALLOCATE(rVisBeamC(ibMax,jbMax))
    rVisBeamC=0.0_r8
    ALLOCATE(rVisDiffC(ibMax,jbMax))
    rVisDiffC=0.0_r8
    ALLOCATE(rNirBeamC(ibMax,jbMax))
    rNirBeamC=0.0_r8
    ALLOCATE(rNirDiffC(ibMax,jbMax))
    rNirDiffC=0.0_r8
    ALLOCATE(rSwSfcNet (ibMax,jbMax))
    rSwSfcNet =0.0_r8
    ALLOCATE(rSwSfcNetC(ibMax,jbMax))
    rSwSfcNetC=0.0_r8

    ! Surface albedo for SW 
    ALLOCATE(AlbVisBeam (ibMax,jbMax))
    AlbVisBeam =0.0_r8
    ALLOCATE(AlbVisDiff (ibMax,jbMax))
    AlbVisDiff =0.0_r8
    ALLOCATE(AlbNirBeam (ibMax,jbMax))
    AlbNirBeam =0.0_r8
    ALLOCATE(AlbNirDiff (ibMax,jbMax))
    AlbNirDiff =0.0_r8

    ! LW Radiation fields at last integer hour
    ALLOCATE(LwCoolRate   (ibMax,kMax,jbMax))
    LwCoolRate   =0.0_r8
    ALLOCATE(LwCoolRatec  (ibMax,kMax,jbMax))
    LwCoolRatec  =0.0_r8
    ALLOCATE(LwSfcDown(ibMax,jbMax))
    LwSfcDown=0.0_r8
    ALLOCATE(LwSfcDownC(ibMax,jbMax))
    LwSfcDownC=0.0_r8
    ALLOCATE(LwSfcNet(ibMax,jbMax))
    LwSfcNet   =0.0_r8
    ALLOCATE(LwSfcNetC (ibMax,jbMax))
    LwSfcNetC =0.0_r8
    ALLOCATE(LwToaUp(ibMax,jbMax))
    LwToaUp=0.0_r8
    ALLOCATE(LwToaUpC(ibMax,jbMax))
    LwToaUpC=0.0_r8

    ! Clouds for Radiation
    ALLOCATE(cldsav(ibMax,jbMax))
    cldsav=0.0_r8
    ALLOCATE(cldtot(ibMax,kMax,jbMax))
    cldtot=0.0_r8
    ALLOCATE(cldinv(ibMax,kMax,jbMax))
    cldinv=0.0_r8
    ALLOCATE(cldsat(ibMax,kMax,jbMax))
    cldsat=0.0_r8
    ALLOCATE(cldcon(ibMax,kMax,jbMax))
    cldcon=0.0_r8
    ALLOCATE(cldson(ibMax,kMax,jbMax))
    cldson=0.0_r8
    ALLOCATE(clwd  (ibMax,kMax,jbMax))
    clwd  =0.0_r8
    ALLOCATE(emisd (ibMax,kMax,jbMax))
    emisd =0.0_r8
    ALLOCATE(taud  (ibMax,kMax,jbMax))
    taud =0.0_r8 

    ! Convection
    ALLOCATE(ppli  (ibMax,jbMax))
    ppli  =0.0_r8
    ALLOCATE(ppci  (ibMax,jbMax))
    ppci  =0.0_r8
    ALLOCATE(prct  (ibMax,jbMax))
    prct   =0.0_r8
    ALLOCATE(prcc  (ibMax,jbMax))
    prcc    =0.0_r8
    ALLOCATE(prcp1 (ibMax,jbMax))
    prcp1  =0.0_r8
    ALLOCATE(prcp2 (ibMax,jbMax))
    prcp2  =0.0_r8
    ALLOCATE(prcp3 (ibMax,jbMax))
    prcp3  =0.0_r8
    ALLOCATE(prcpt (ibMax,jbMax))
    prcpt   =0.0_r8
    ALLOCATE(toplv (ibMax,jbMax))
    toplv   =0.0_r8
    ALLOCATE(botlv (ibMax,jbMax))
    botlv  =0.0_r8
    ALLOCATE(geshem(ibMax,jbMax))
    geshem =0.0_r8
    ALLOCATE(convc (ibMax,jbMax))
    convc=0.0_r8
    ALLOCATE(convt (ibMax,jbMax))
    convt=0.0_r8
    ALLOCATE(convb (ibMax,jbMax))
    convb=0.0_r8
    ALLOCATE(convts(ibMax,jbMax))
    convts=0.0_r8
    ALLOCATE(convcs(ibMax,jbMax))
    convcs=0.0_r8
    ALLOCATE(convbs(ibMax,jbMax))
    convbs=0.0_r8

    ! PBL
    ALLOCATE(ustr  (ibMax,jbMax))
    ustr=0.0_r8
    ALLOCATE(vstr  (ibMax,jbMax))
    vstr=0.0_r8

    ! Surface/SSIB
    ALLOCATE(imask (ibMax,jbMax))
    imask =0
    ALLOCATE(ssib  (ibMax,jbMax))
    ssib=0.0_r8

    ! Outros (falta organizar)
    ALLOCATE(gl0   (ibMax,jbMax))
    gl0   =0.0_r8
    ALLOCATE(Mmlen   (ibMax,jbMax))
    Mmlen   =0.0_r8
    ALLOCATE(zorl  (ibMax,jbMax))
    zorl  =0.0_r8
    ALLOCATE(sheleg(ibMax,jbMax))
    sheleg=0.0_r8
    ALLOCATE(tseam (ibMax,jbMax))
    tseam=0.0_r8
    ALLOCATE(gtsea (ibMax,jbMax))
    gtsea =0.0_r8
    ALLOCATE(tm0   (ibMax,jbMax))
    tm0   =0.0_r8
    ALLOCATE(qm0   (ibMax,jbMax))
    qm0   =0.0_r8
    ALLOCATE(tc0   (ibMax,jbMax))
    tc0   =0.0_r8
    ALLOCATE(tg0   (ibMax,jbMax))
    tg0   =0.0_r8
    ALLOCATE(td0   (ibMax,jbMax))
    td0   =0.0_r8
    ALLOCATE(w0    (ibMax,3,jbMax))
    w0    =0.0_r8
    ALLOCATE(capac0(ibMax,2,jbMax))
    capac0=0.0_r8
    ALLOCATE(tmm   (ibMax,jbMax))
    tmm   =0.0_r8
    ALLOCATE(qmm   (ibMax,jbMax))
    qmm   =0.0_r8
    ALLOCATE(tcm   (ibMax,jbMax))
    tcm   =0.0_r8
    ALLOCATE(tgm   (ibMax,jbMax))
    tgm   =0.0_r8
    ALLOCATE(tdm   (ibMax,jbMax))
    tdm   =0.0_r8
    ALLOCATE(wm    (ibMax,3,jbMax))
    wm    =0.0_r8
    ALLOCATE(capacm(ibMax,2,jbMax))
    capacm=0.0_r8
    ALLOCATE(var   (ibMax,jbMax))
    var   =0.0_r8
    ALLOCATE(tg1   (ibMax,jbMax))
    tg1    =0.0_r8
    ALLOCATE(tg2   (ibMax,jbMax))
    tg2    =0.0_r8
    ALLOCATE(tg3   (ibMax,jbMax))
    tg3    =0.0_r8
    ALLOCATE(soilm (ibMax,jbMax))
    soilm  =0.0_r8
    ALLOCATE(sens  (ibMax,jbMax))
    sens  =0.0_r8
    ALLOCATE(evap  (ibMax,jbMax))
    evap  =0.0_r8

    ALLOCATE(o3mix (ibMax,kMax,jbMax))
    o3mix =0.0_r8
    ALLOCATE(glsm_w   (ibMax,jbMax,nzg     ))
    glsm_w=0.0_r8
    ALLOCATE(wsib3d   (ibMax,jbMax,3       ))
    wsib3d=0.0_r8
    ALLOCATE(veg_type (ibMax,jbMax,npatches))
    veg_type=0.0_r8
    ALLOCATE(frac_occ (ibMax,jbMax,npatches))
    frac_occ=0.0_r8
    ALLOCATE(soil_type(ibMax,jbMax         ))
    soil_type=0.0_r8
    ALLOCATE( XLAND(ibMax,jbMax))
    xland=0.0_r8
    ALLOCATE( SEAMASK(ibMax,jbMax))
    seamask=0.0_r8
    ALLOCATE( Z0(ibMax,jbMax))
    z0=0.0_r8
    ALLOCATE( USTAR(ibMax,jbMax))
    ustar=0.0_r8
    ALLOCATE( LOWLYR(ibMax,jbMax))
    lowlyr=0
    ALLOCATE(tkemyj(ibMax,kMax,jbMax))
    tkemyj=0.02_r8
    ALLOCATE( qsfc0 (ibMax,jbMax))
    qsfc0=0.0_r8
    ALLOCATE( tsfc0 (ibMax,jbMax))
    tsfc0=0.0_r8
    ALLOCATE( qsfcm (ibMax,jbMax))
    qsfcm=0.0_r8
    ALLOCATE( tsfcm (ibMax,jbMax))
    tsfcm=0.0_r8
    ALLOCATE( snow (ibMax,jbMax))
    snow=0.0_r8
    ALLOCATE( THZ0 (ibMax,jbMax))
    THZ0=0.0_r8
    ALLOCATE( QZ0  (ibMax,jbMax))
    QZ0=0.0_r8
    ALLOCATE( UZ0  (ibMax,jbMax))
    UZ0=0.0_r8
    ALLOCATE( VZ0  (ibMax,jbMax))
    VZ0=0.0_r8
    ALLOCATE( ZNT  (ibMax,jbMax))
    ZNT=0.0_r8
    ALLOCATE( PBLH (ibMax,jbMax))
    PBLH=0.0_r8
    ALLOCATE(AKHS (ibMax,jbMax))
    AKHS=0.0_r8
    ALLOCATE(AKMS (ibMax,jbMax))
    AKMS=0.0_r8
    ALLOCATE(RMOL(ibMax,jbMax))
    RMOL=0.0_r8
    ALLOCATE(CT(ibMax,jbMax))
    CT=0.0_r8
    ALLOCATE(htdisp(ibMax,jbMax))
    htdisp=0.0_r8
    ALLOCATE(temp2m(ibMax,jbMax))
    temp2m=0.0_r8
    ALLOCATE(umes2m(ibMax,jbMax))
    umes2m=0.0_r8
    ALLOCATE(uve10m(ibMax,jbMax))
    uve10m=0.0_r8
    ALLOCATE(vve10m(ibMax,jbMax))
    vve10m=0.0_r8
    ALLOCATE(MskAnt(ibMax,jbMax))
    MskAnt=0_i8
  END SUBROUTINE InitFieldsPhyscs







  SUBROUTINE InitVariancia(igwd,nfvar,fNameOrgvar)
    CHARACTER(LEN=*) , INTENT(in   ) :: igwd
    INTEGER          , INTENT(in   ) :: nfvar
    CHARACTER(LEN=*) , INTENT(in   ) :: fNameOrgvar
    INTEGER       :: LRecIn,irec
    REAL(KIND=r8) ::   var_in (iMax,jMax)
    REAL(KIND=r4) ::   buffer (iMax,jMax)
    var_in  =0.0_r8
    buffer  =0.0_r4
    INQUIRE (IOLENGTH=LRecIn) buffer
    OPEN (UNIT=nfvar,FILE=TRIM(fNameOrgvar),FORM='UNFORMATTED', ACCESS='DIRECT', &
         ACTION='READ',RECL=LRecIn,STATUS='OLD', IOSTAT=ierr)

    IF (ierr /= 0) THEN
       WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
            TRIM(fNameOrgvar), ierr
       STOP "**(ERROR)**"
    END IF


    IF(TRIM(igwd).EQ.'YES') THEN
       irec=1
       CALL ReadVar(nfvar,irec,var_in)
       IF (reducedGrid) THEN
          CALL LinearIJtoIBJB(var_in,var)
       ELSE
          CALL IJtoIBJB(var_in,var)
       END IF
    END IF
  END SUBROUTINE InitVariancia







  SUBROUTINE InitBoundCond(&
       ibMax,jbMax,kMax,ifdy,todcld,ids,idc,ifday, &
       tod,todsib,idate,idatec,si,sl,record_type,&
       fNameSoilType,fNameVegType,fNameSoilMoist, &
       fNameSibmsk,fNameTg3zrl  ,ibMaxPerJB)

    INTEGER         , INTENT(IN   ) :: ibMax
    INTEGER         , INTENT(IN   ) :: jbMax
    INTEGER         , INTENT(IN   ) :: kMax
    INTEGER         , INTENT(OUT  ) :: ifdy
    REAL(KIND=r8)   , INTENT(OUT  ) :: todcld
    INTEGER         , INTENT(OUT  ) :: ids(:)
    INTEGER         , INTENT(OUT  ) :: idc(:)
    INTEGER         , INTENT(IN   ) :: ifday
    REAL(KIND=r8)   , INTENT(IN   ) :: tod

    REAL(KIND=r8)   , INTENT(OUT  ) :: todsib
    INTEGER         , INTENT(IN   ) :: idate(:)
    INTEGER         , INTENT(IN   ) :: idatec(:)

    REAL(KIND=r8)   , INTENT(IN   ) :: si(:)
    REAL(KIND=r8)   , INTENT(IN   ) :: sl(:)
    CHARACTER(LEN=*), INTENT(IN   ) :: record_type
    CHARACTER(LEN=*), INTENT(IN   ) :: fNameSoilType
    CHARACTER(LEN=*), INTENT(IN   ) :: fNameVegType
    CHARACTER(LEN=*), INTENT(IN   ) :: fNameSoilMoist
    INTEGER         , INTENT(IN   ) :: ibMaxPerJB(:)
    CHARACTER(LEN=*), INTENT(IN   ) :: fNameSibmsk
    CHARACTER(LEN=*), INTENT(IN   ) :: fNameTg3zrl

    REAL(KIND=r8)                            :: tice  =271.16e0_r8
    REAL(KIND=r8)                            :: t0
    REAL(KIND=r8)                            :: sinmax
    INTEGER                         :: j
    INTEGER                         :: i
    INTEGER                         :: ncount,LRecIN,irec
    REAL(KIND=r8)                            :: wsib  (ibMax,jbMax)
    REAL(KIND=r8)                            :: zero  =0.0e3_r8
    REAL(KIND=r8)                            :: thousd=1.0e3_r8
    REAL(KIND=r8)            , PARAMETER     :: xl0   =10.0_r8
    REAL(KIND=r8) ::   buf (iMax,jMax,4)
    REAL(KIND=r4) ::   brf (iMax,jMax)
    INTEGER ::  ibuf (iMax,jMax)
    INTEGER(KIND=i8) :: imask_in(iMax,jMax)
    INTEGER :: ier(iMax,jMax)
    INTEGER(KIND=i8) :: mskant_in(iMax,jMax)

    CHARACTER(LEN=*), PARAMETER :: h='**(InitBoundCond)**'

    imask_in =0_i8
    mskant_in=0_i8
    buf=0.0_r8
    ier=0
    IF(TRIM(isimp).NE.'YES') THEN
       IF(nfcnv0.NE.0) THEN
          CALL MsgOne(h,'Reading previous physics state for restart')

          READ(UNIT=nfcnv0) ifdy,todcld,ids,idc
          READ(UNIT=nfcnv0) convc,convt,convb,prcp1,prcp2,prcp3, &
               prcpt,toplv,botlv
          IF(ifday.GT.0.OR.tod.GT.0.0_r8)READ(UNIT=nfcnv0)rVisDiff,rVisBeam,rNirDiff, &
               rNirBeam,rVisDiffC,rVisBeamC,rNirDiffC,rNirBeamC,rSwToaDown

          REWIND nfcnv0

          IF(nfctrl(4) .GE. 1)WRITE(UNIT=nfprt,FMT=555)ifdy,todcld,ids,idc
       ELSE
          CALL MsgOne(h,'Initializing prec/cloud variables')
          convc=0.0_r8
          convt=0.0_r8
          convb=0.0_r8
          prcp1=0.0_r8
          prcp2=0.0_r8
          prcp3=0.0_r8
          prcpt=0.0_r8
          toplv=0.0_r8
          botlv=0.0_r8
       END IF

       sheleg=0.0_r8

       CALL vegin (si(1) ,sl(1))
       ibuf=0
       INQUIRE (IOLENGTH=LRecIN) ibuf
       OPEN (UNIT=nfsibt, FILE=TRIM(fNameSibmsk),FORM='UNFORMATTED', ACCESS='DIRECT', RECL=LRecIN,&
         ACTION='READ',STATUS='OLD', IOSTAT=ierr)
       IF (ierr /= 0) THEN
          WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
               TRIM(fNameSibmsk), ierr
          STOP "**(ERROR)**"
       END IF
       brf=0.0_r4
       INQUIRE (IOLENGTH=LRecIN) brf
       OPEN (UNIT=nftgz0,FILE=TRIM(fNameTg3zrl), FORM='UNFORMATTED', ACCESS='DIRECT', RECL=LRecIN, &
         ACTION='read', STATUS='OLD', IOSTAT=ierr) 
       IF (ierr /= 0) THEN
          WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
               TRIM(fNameTg3zrl), ierr
          STOP "**(ERROR)**"
       END IF
 
       INQUIRE (IOLENGTH=LRecIN) brf
       OPEN (UNIT=nfzol,FILE=TRIM(fNameRouLen),FORM='UNFORMATTED', ACCESS='DIRECT', RECL=LRecIN, &
            ACTION='READ', STATUS='OLD', IOSTAT=ierr)
       IF (ierr /= 0) THEN
          WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
               TRIM(fNameRouLen), ierr
          STOP "**(ERROR)**"
       END IF

       !
       ! READ SSib mask from disk as a global field
       !
       READ(UNIT=nfsibt, REC=1) ibuf
       imask_in=ibuf

       !
       ! In Case of Guevaerd&Freitas soil moisture, we also need to read
       ! soil type and vegetation type. For the moment we modify only the
       ! vegetation mask to match the predominant biome of the multiple
       ! patches scheme. The soil and vegetation parameters are yet not
       ! changed
       !
       IF(iglsm_w == 1) THEN
          CALL read_gl_sm_bc(&
               imax           , & !   IN
               jmax           , & !   IN
               jbMax          , & !   IN
               ibMaxPerJB     , & !   IN
               record_type    , & !   IN
               fNameSoilType  , & !   IN
               fNameVegType   , & !   IN
               fNameSoilMoist , & !   IN
               imask_in         ) !   INOUT

!hmjb      CALL re_assign_sib_soil_prop()
       ENDIF

       ! Now that the input mask has been modified, we can translate it
       ! to the matrix representation used on each CPU
       IF (reducedGrid) THEN
          CALL FreqBoxIJtoIBJB(imask_in,imask)
       ELSE
          CALL IJtoIBJB( imask_in,imask)
       END IF

       DO j=1,jMax
          DO i=1,iMax
             IF (imask_in(i,j) >= 1_i8) THEN
                ier(i,j) = 0
             ELSE
                ier(i,j) = 1
             END IF
          END DO
          IF (ANY( ier(1:iMax,j) /= 0)) THEN
             DO i=1,iMax
                mskant_in(i,j) = 1_i8
             END DO
          ELSE
             DO i=1,iMax
                mskant_in(i,j) = 0_i8
             END DO
          END IF
       END DO
       IF (reducedGrid) THEN
          CALL FreqBoxIJtoIBJB(mskant_in,mskant)
       ELSE
          CALL IJtoIBJB( mskant_in,mskant)
       END IF

       !REWIND nfsibt
       !
       !
       !     initialize sib variables
       !
       IF(ifday.EQ.0.AND.tod.EQ.zero.AND.initlz.GE.0) THEN

          call MsgOne(h,'Cold start SSib variables')

          CALL getsbc (iMax ,jMax  ,kMax, AlbVisDiff,gtsea,soilm,sheleg,o3mix,&
               ifday , tod  ,idate ,idatec, &
               ifalb,ifsst,ifslm ,ifsnw,ifozone, &
               sstlag,intsst,fint ,tice  , &
               yrl  ,monl,ibMax,jbMax,ibMaxPerJB)
          irec=1
          CALL ReadGetNFTGZ(nftgz0,irec,buf(:,:,1),buf(:,:,2),buf(:,:,3))
          READ (UNIT=nfzol, REC=1) brf

          buf(1:iMax,1:jMax,4)=brf(1:iMax,1:jMax)
          IF (reducedGrid) THEN
             CALL AveBoxIJtoIBJB(buf(:,:,1),tg1)
          ELSE
             CALL IJtoIBJB(buf(:,:,1) ,tg1 )
          END IF

          IF (reducedGrid) THEN
             CALL AveBoxIJtoIBJB(buf(:,:,2) ,tg2)
          ELSE
             CALL IJtoIBJB(buf(:,:,2) ,tg2 )
          END IF

          IF (reducedGrid) THEN
             CALL AveBoxIJtoIBJB(buf(:,:,3) ,tg3)
          ELSE
             CALL IJtoIBJB(buf(:,:,3) ,tg3 )
          END IF

          IF (reducedGrid) THEN
             CALL AveBoxIJtoIBJB(buf(:,:,4),zorl)
          ELSE
             CALL IJtoIBJB(buf(:,:,4),zorl )
          END IF
          z0=zorl
          t0    =271.17_r8
          sinmax=150.0_r8
          !
          !     use rVisDiff as temporary for abs(soilm)
          !
          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)
                rVisDiff(i,j)=ABS(soilm(i,j))
             END DO
          END DO
          !-srf--------------------------------
          IF(iglsm_w == 0) THEN
             CALL sibwet(ibMax,jbMax,rVisDiff,sinmax,imask,wsib,ssib, &
                  mxiter,ibMaxPerJB)
          ELSE
             !
             !hmjb A umide do solo ja foi lida acima, agora so eh preciso
             !     interpolar as varias camadas de Guevaerd&Freitas para 
             !     as tres camadas do SSib. Isto so precisa ser feito no
             !     primeiro time-step de um cold start. No caso de um 
             !     warm start, o campo de umidade eh lido dos arquivos de
             !     restart.
             !
             CALL sibwet_GLSM (&
                  ibMax          , & ! IN
                  jbMax          , & ! IN
                  imask          , & ! IN
                  wsib           , & ! OUT
                  ssib           , & ! IN
                  mxiter         , & ! IN
                  ibMaxPerJB     , & ! IN
                  soilm          , & ! OUT
                  nzg            , & ! IN
                  wsib3d         , & ! OUT
                  glsm_w)            ! IN

          END IF
          !-srf--------------------------------
          ppli=0.0_r8
          ppci=0.0_r8
          capac0=0.0_r8
          capacm=0.0_r8
          !
          !     td0 (deep soil temp) is temporarily defined as tg3
          !
          !$OMP PARALLEL DO PRIVATE(ncount,i)
          DO j=1,jbMax
             ncount=0
             DO i=1,ibMaxPerJB(j)
                gl0(i,j)=xl0
                Mmlen(i,j)=xl0
                IF(imask(i,j) .ne. 0)gtsea(i,j)=290.0_r8
                tseam(i,j)=gtsea(i,j)
                IF(imask(i,j).EQ.0) THEN
                   IF(-gtsea(i,j).LT.t0) THEN
                      imask(i,j)=-1
                   END IF
                ELSE
                   ncount=ncount+1
                   IF(iglsm_w == 0) THEN
                      w0    (ncount,1,j)=wsib(i,j)
                      w0    (ncount,2,j)=wsib(i,j)
                      w0    (ncount,3,j)=wsib(i,j)
                      wm    (ncount,1,j)=wsib(i,j)
                      wm    (ncount,2,j)=wsib(i,j)
                      wm    (ncount,3,j)=wsib(i,j)
                   ELSE
                      !-srf--------------------------------
                      w0    (ncount,1,j)=wsib3d(i,j,1)
                      w0    (ncount,2,j)=wsib3d(i,j,2)
                      w0    (ncount,3,j)=wsib3d(i,j,3)
                      wm    (ncount,1,j)=wsib3d(i,j,1)
                      wm    (ncount,2,j)=wsib3d(i,j,2)
                      wm    (ncount,3,j)=wsib3d(i,j,3)
                      !-srf--------------------------------
                   END IF

                   td0   (ncount,j)=tg3 (i,j)
                   tdm   (ncount,j)=tg3 (i,j)
                   tgm   (ncount,j)=tg3 (i,j)
                   tcm   (ncount,j)=tg3 (i,j)
                   ssib  (ncount,j)=0.0_r8
                   IF(soilm(i,j).LT.0.0_r8)ssib(ncount,j)=wsib(i,j)

                   IF(sheleg(i,j).GT.zero) THEN
                      capac0(ncount,2,j)=sheleg(i,j)/thousd
                      capacm(ncount,2,j)=sheleg(i,j)/thousd
                   END IF

                END IF
             END DO
          END DO
          !$OMP END PARALLEL DO

       ELSE

          call MsgOne(h,'Warm start SSib variables')

          READ(UNIT=nfsibi)ifdy,todsib,ids,idc
          READ(UNIT=nfsibi) tm0   ,tmm
          READ(UNIT=nfsibi) qm0   ,qmm
          READ(UNIT=nfsibi) td0   ,tdm
          READ(UNIT=nfsibi) tg0   ,tgm
          READ(UNIT=nfsibi) tc0   ,tcm
          READ(UNIT=nfsibi) w0    ,wm
          READ(UNIT=nfsibi) capac0,capacm
          READ(UNIT=nfsibi) ppci  ,ppli
          READ(UNIT=nfsibi) gl0   ,zorl  ,gtsea ,tseam,qsfc0,tsfc0,qsfcm,tsfcm
          READ(UNIT=nfsibi) imask
          Mmlen=gl0
          REWIND nfsibi



          IF(initlz.LT.0.AND.initlz.GT.-3)THEN

             CALL getsbc (iMax ,jMax  ,kMax, AlbVisDiff,gtsea,soilm,sheleg,o3mix,&
                  ifday , tod  ,idate ,idatec, &
                  ifalb,ifsst,ifslm ,ifsnw,ifozone, &
                  sstlag,intsst,fint ,tice  , &
                  yrl  ,monl,ibMax,jbMax,ibMaxPerJB)

          END IF

          !$OMP PARALLEL DO PRIVATE(ncount,i)
          DO j=1,jbMax
             ncount=0
             DO i=1,ibMaxPerJB(j)
                IF(imask(i,j).GT.0)THEN
                   ncount=ncount+1
                   ssib(ncount,j)=0.0_r8
                   IF(w0(ncount,1,j).LT.0.0_r8)THEN
                      ssib(ncount,j)=ABS(w0(ncount,1,j))
                      w0(ncount,1,j)=ABS(w0(ncount,1,j))
                      w0(ncount,2,j)=ABS(w0(ncount,2,j))
                      w0(ncount,3,j)=ABS(w0(ncount,3,j))
                      wm(ncount,1,j)=ABS(wm(ncount,1,j))
                      wm(ncount,2,j)=ABS(wm(ncount,2,j))
                      wm(ncount,3,j)=ABS(wm(ncount,3,j))
                   END IF
                END IF
             END DO
          END DO
          !$OMP END PARALLEL DO

          IF(nfctrl(5).GE.1)WRITE(UNIT=nfprt,FMT=444) ifdy,todsib,ids,idc

       END IF
    END IF

444 FORMAT(' SIB PROGNOSTIC VARIABLES READ IN. AT FORECAST DAY', &
         I8,' TOD ',F8.1/' STARTING',3I3,I5,' CURRENT',3I3,I5)
555 FORMAT(' CLOUD PROGNOSTIC DATA READ IN. AT FORECAST DAY', &
         I8,' TOD ',F8.1/' STARTING',3I3,I5,' CURRENT',3I3,I5)
  END SUBROUTINE InitBoundCond



  SUBROUTINE InitCheckfile(ibMax,&
       jbMax  ,kMax, ifdy  ,todcld,ids   ,idc   ,ifday , &
       tod   ,idate ,idatec   ,todsib,ibMaxPerJB )
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: jbMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(OUT  ) :: ifdy
    REAL(KIND=r8)   , INTENT(OUT  ) :: todcld
    INTEGER, INTENT(OUT  ) :: ids   (:)
    INTEGER, INTENT(OUT  ) :: idc   (:)
    INTEGER, INTENT(IN   ) :: ifday
    REAL(KIND=r8)   , INTENT(IN   ) :: tod
    INTEGER, INTENT(IN   ) :: idate (:)
    INTEGER, INTENT(IN   ) :: idatec(:)
    REAL(KIND=r8)   , INTENT(OUT  ) :: todsib
    INTEGER, INTENT(IN   ) :: ibMaxPerJB(:)

    INTEGER                :: j
    INTEGER                :: ncount
    INTEGER                :: i
    REAL(KIND=r8)                   :: tice  =271.16e0_r8

    CHARACTER(LEN=*), PARAMETER :: h="**(InitCheckfile)**"

    !
    !     read cloud dataset for cold start
    !
    IF(nfcnv0.NE.0) THEN
       CALL MsgOne(h,'Read prec/cloud variables')
       READ(UNIT=nfcnv0) ifdy,todcld,ids,idc
       READ(UNIT=nfcnv0) convc,convt,convb,prcp1,prcp2,prcp3, &
            prcpt,toplv,botlv


       REWIND nfcnv0

       IF(nfctrl(4) .GE. 1) WRITE(UNIT=nfprt,FMT=555)ifdy,todcld,ids,idc

    ELSE
       CALL MsgOne(h,'Initializing prec/cloud variables')
       convc=0.0_r8
       convt=0.0_r8
       convb=0.0_r8
       prcp1=0.0_r8
       prcp2=0.0_r8
       prcp3=0.0_r8
       prcpt=0.0_r8
       toplv=0.0_r8
       botlv=0.0_r8
    END IF


    IF(initlz.LT.0)THEN

       CALL MsgOne(h,'Read SSib variables from warm-start file')

       READ(UNIT=nfsibi) ifdy,todsib,ids,idc
       READ(UNIT=nfsibi) tm0   ,tmm
       READ(UNIT=nfsibi) qm0   ,qmm
       READ(UNIT=nfsibi) td0   ,tdm
       READ(UNIT=nfsibi) tg0   ,tgm
       READ(UNIT=nfsibi) tc0   ,tcm
       READ(UNIT=nfsibi) w0    ,wm
       READ(UNIT=nfsibi) capac0,capacm
       READ(UNIT=nfsibi) ppci  ,ppli
       READ(UNIT=nfsibi) gl0   ,zorl  ,gtsea,tseam,qsfc0,tsfc0,qsfcm,tsfcm
       READ(UNIT=nfsibi) imask

       Mmlen=gl0
       REWIND nfsibi

       CALL getsbc (iMax ,jMax  ,kMax, AlbVisDiff,gtsea,soilm,sheleg,o3mix,&
            ifday , tod  ,idate ,idatec, &
            ifalb,ifsst,ifslm ,ifsnw,ifozone, &
            sstlag,intsst,fint ,tice  , &
            yrl  ,monl,ibMax,jbMax,ibMaxPerJB)

       DO j=1,jbMax
          ncount=0
          DO i=1,ibMaxPerJB(j)
             IF(imask(i,j).GT.0)THEN
                ncount=ncount+1
                ssib(ncount,j)=0.0_r8
                IF(w0(ncount,1,j).LT.0.0_r8)THEN
                   ssib(ncount,  j)=ABS(w0(ncount,1,j))
                   w0  (ncount,1,j)=ABS(w0(ncount,1,j))
                   w0  (ncount,2,j)=ABS(w0(ncount,2,j))
                   w0  (ncount,3,j)=ABS(w0(ncount,3,j))
                   wm  (ncount,1,j)=ABS(wm(ncount,1,j))
                   wm  (ncount,2,j)=ABS(wm(ncount,2,j))
                   wm  (ncount,3,j)=ABS(wm(ncount,3,j))
                END IF
             END IF
          END DO
       END DO
       IF(nfctrl(5).GE.1)WRITE(UNIT=nfprt,FMT=444) ifdy,todsib,ids,idc
    END IF

444 FORMAT(' SIB PROGNOSTIC VARIABLES READ IN. AT FORECAST DAY', &
         I8,' TOD ',F8.1/' STARTING',3I3,I5,' CURRENT',3I3,I5)
555 FORMAT(' CLOUD PROGNOSTIC DATA READ IN. AT FORECAST DAY', &
         I8,' TOD ',F8.1/' STARTING',3I3,I5,' CURRENT',3I3,I5)

  END SUBROUTINE InitCheckfile



  SUBROUTINE InitSurfTemp (jbMax ,ibMaxPerJB)

    INTEGER, INTENT(IN   ) :: jbMax
    INTEGER, INTENT(IN   ) :: ibMaxPerJB(:)
    INTEGER                :: i
    INTEGER                :: j
    INTEGER                :: ncount
    REAL(KIND=r8)                   :: zero  =0.0e3_r8
    REAL(KIND=r8)                   :: thousd=1.0e3_r8
    REAL(KIND=r8)                   :: tf    =273.16e0_r8
    capacm=0.0_r8
    capac0=0.0_r8
    DO j=1,jbMax
       ncount=0
       DO i=1,ibMaxPerJB(j)
          IF(imask(i,j).GT.0) THEN
             ncount=ncount+1
             IF(sheleg(i,j).GT.zero) THEN
                capac0(ncount,2,j) = sheleg(i,j)/thousd
                capacm(ncount,2,j) = sheleg(i,j)/thousd
                tg0   (ncount,  j) = MIN(tg0(ncount,j),tf-0.01e0_r8)
                tgm   (ncount,  j) = MIN(tgm(ncount,j),tf-0.01e0_r8)
             END IF
          END IF
       END DO
    END DO
  END SUBROUTINE InitSurfTemp



  SUBROUTINE InitGetsbc(ifday ,tod   ,idate ,idatec,ibMax,jbMax,kMax,ibMaxPerJB)
    !
    ! getsbc :read surface boundary conditions.
    !
    INTEGER, INTENT(in   ) :: ibMax
    INTEGER, INTENT(in   ) :: jbMax
    INTEGER, INTENT(in   ) :: kMax
    INTEGER, INTENT(in   ) :: ibMaxPerJB(:)
    INTEGER, INTENT(in   ) :: ifday
    REAL(KIND=r8)   , INTENT(in   ) :: tod
    INTEGER, INTENT(in   ) :: idate (4)
    INTEGER, INTENT(in   ) :: idatec(4)

    REAL(KIND=r8)                   :: tice  =271.16e0_r8

    IF (WillGetSbc(idate, tod, fint)) THEN

       CALL getsbc (iMax ,jMax  ,kMax, AlbVisDiff,gtsea,soilm,sheleg,o3mix,&
            ifday , tod  ,idate ,idatec, &
            ifalb,ifsst,ifslm ,ifsnw,ifozone, &
            sstlag,intsst,fint ,tice  , &
            yrl  ,monl,ibMax,jbMax,ibMaxPerJB)

    END IF

  END SUBROUTINE InitGetsbc


  !------------------------------------------------------------
  SUBROUTINE read_gl_sm_bc(&
       iMax           , &!   IN
       jMax           , &!   IN
       jbMax          , &!   IN
       ibMaxPerJB     , &!   IN
       record_type    , &! IN
       fNameSoilType  , &! IN
       fNameVegType   , &! IN
       fNameSoilMoist , &
       imask_in        ) ! INOUT

    INTEGER, INTENT(IN   )          :: iMax
    INTEGER, INTENT(IN   )          :: jMax
    INTEGER, INTENT(IN   )          :: jbMax
    INTEGER, INTENT(IN   )          :: ibMaxPerJB(:)
    CHARACTER(LEN=*), INTENT(IN   ) :: record_type
    CHARACTER(LEN=*), INTENT(IN   ) :: fNameSoilType
    CHARACTER(LEN=*), INTENT(IN   ) :: fNameVegType
    CHARACTER(LEN=*), INTENT(IN   ) :: fNameSoilMoist
    INTEGER(KIND=i8), INTENT(INOUT) :: imask_in(iMax,jMax) ! SSIB veg type

    ! Local matrixes for reading global field
    ! After IJtoIBJB transformations, these are stored on each cpu
    ! as global variables of this module (only the parts needed for
    ! each processor)
    REAL(KIND=r8) :: VegType (iMax,jMax,npatches) ! SIB veg type
    REAL(KIND=r8) :: FracOcc (iMax,jMax,npatches) ! fractional area
    REAL(KIND=r8) :: glsm    (iMax,jMax,nzg     ) ! initial soil wetness data
    REAL(KIND=r8) :: SoilType(iMax,jMax         ) ! FAO/USDA soil texture
    !
    ! Counters
    !
    INTEGER       :: i
    INTEGER       :: j
    INTEGER       :: k
    INTEGER       :: ipatch
    REAL(KIND=r8) :: fractx
    !
    !------------------------- soil type initialization ------------
    !
    call MsgOne('**(read_gl_sm_bc)**','Opening GL soil file='//TRIM(fNameSoilType))
    FracOcc=0.0_r8
    glsm=0.0_r8
    SoilType=0.0_r8
    IF (record_type == 'seq') THEN      !sequential mode

       OPEN(UNIT=nfsoiltp,FILE=TRIM(fNameSoilType),FORM='unformatted',ACCESS='sequential',&
            ACTION='read',STATUS='old',IOSTAT=ierr)
       IF (ierr /= 0) THEN
          WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
               TRIM(fNameSoilType), ierr
          STOP "**(ERROR)**"
       END IF
       READ(UNIT=nfsoiltp) ((SoilType(i,j),i=1,iMax),j=1,jMax)

       IF (reducedGrid) THEN
          CALL NearestIJtoIBJB(SoilType,soil_type)
       ELSE
          CALL IJtoIBJB( SoilType,soil_type)
       END IF

       CLOSE(UNIT=nfsoiltp)

    ELSE IF (record_type == 'vfm') THEN !vformat model

       OPEN(UNIT=nfsoiltp,FILE=TRIM(fNameSoilType),FORM='formatted',ACCESS='sequential',&
            ACTION='read',STATUS='old',IOSTAT=ierr)
       IF (ierr /= 0) THEN
          WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
               TRIM(fNameSoilType), ierr
          STOP "**(ERROR)**"
       END IF

       CALL  vfirec(nfsoiltp,SoilType,imax*jmax,'LIN')

       DO i=1,iMax
          DO j=1,jMax
             SoilType(i,j)=REAL(INT(SoilType(i,j)+0.1_r8),r8)
          END DO
       END DO

       IF (reducedGrid) THEN
          CALL NearestIJtoIBJB(SoilType,soil_type)
       ELSE
          CALL IJtoIBJB( SoilType,soil_type)
       END IF

       CLOSE(UNIT=nfsoiltp)

    END IF

    !
    !-------------------veg type and fractional area initialization ------------
    !
    call MsgOne('**(read_gl_sm_bc)**','Opening GL veg file='//TRIM(fNameVegType))

    IF (record_type == 'seq') THEN !sequential mode

       OPEN(UNIT=nfvegtp,FILE=TRIM(fNameVegType),FORM='unformatted',ACCESS='sequential',&
            ACTION='read',STATUS='old',IOSTAT=ierr)
       IF (ierr /= 0) THEN
          WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
               TRIM(fNameVegType), ierr
          STOP "**(ERROR)**"
       END IF

       DO ipatch=1,npatches_actual

          READ(UNIT=nfvegtp) ((VegType(i,j,ipatch),i=1,iMax),j=1,jMax) !veg dominante no patch
          READ(UNIT=nfvegtp) ((FracOcc(i,j,ipatch),i=1,iMax),j=1,jMax) !fracao ocupada pelo patch

          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(VegType(:,:,ipatch) ,veg_type(:,:,ipatch) )
          ELSE
             CALL IJtoIBJB( VegType(:,:,ipatch) ,veg_type(:,:,ipatch) )
          END IF

          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(FracOcc(:,:,ipatch) ,frac_occ(:,:,ipatch) )
          ELSE
             CALL IJtoIBJB(FracOcc(:,:,ipatch) ,frac_occ(:,:,ipatch)  )
          END IF

       END DO

       CLOSE(UNIT=nfvegtp)

    ELSE IF (record_type == 'vfm') THEN !vformat model

       OPEN(UNIT=nfvegtp,FILE=TRIM(fNameVegType),FORM='formatted',ACCESS='sequential',&
            ACTION='read',STATUS='old',IOSTAT=ierr)
       IF (ierr /= 0) THEN
          WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
               TRIM(fNameVegType), ierr
          STOP "**(ERROR)**"
       END IF

       DO ipatch=1,npatches_actual
          !
          !print*,'=======================VEGET =======================',ipatch
          !
          CALL vfirec(nfvegtp,VegType(1,1,ipatch),iMax*jMax,'LIN') !veg dominante no patch

          DO j=1,jMax
             DO i=1,iMax
                VegType(i,j,ipatch)=REAL(INT(VegType(i,j,ipatch)+0.1_r8),r8)
             END DO
          END DO

          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(VegType(:,:,ipatch) ,veg_type(:,:,ipatch))
          ELSE
             CALL IJtoIBJB(VegType(:,:,ipatch) ,veg_type(:,:,ipatch) )
          END IF


          !
          !print*,'=======================FRACA =======================',ipatch
          !
          CALL vfirec(nfvegtp,FracOcc(1,1,ipatch),iMax*jMax,'LIN') !fracao ocupada pelo patch

          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(FracOcc(:,:,ipatch) ,frac_occ(:,:,ipatch) )
          ELSE
             CALL IJtoIBJB(FracOcc(:,:,ipatch) ,frac_occ(:,:,ipatch)  )
          END IF

       END DO

       CLOSE(UNIT=nfvegtp)

    END IF
    !
    ! Correction to original SSib mask
    !
    DO j=1,jMax
       DO i= 1,iMax
          !imask(i,j) = 0 => ocean  / imask(i,j) = 13 => ice
          IF (imask_in(i,j) > 0 .and. imask_in(i,j) < 13) THEN
             imask_in(i,j) = int(VegType(i,j,2))
          END IF
       END DO
    END DO
    !
    ! fractional area normalization
    !
    DO j=1,jbMax
       DO i=1,ibMaxPerJB(j)
          IF(frac_occ(i,j,1) < 0.99999_r8) THEN
             fractx=0.0_r8

             DO ipatch=1,npatches_actual-1
                fractx=fractx+frac_occ(i,j,ipatch)
             END DO

             frac_occ(i,j,npatches_actual)= 1.0_r8 - fractx
          END IF

       END DO
    END DO

    !
    !------------------------- soil moisture initialization ------------
    !
    call MsgOne('**(read_gl_sm_bc)**','Opening GL_SM file='//TRIM(fNameSoilMoist))

    IF (record_type == 'seq') THEN !sequential mode

       OPEN(UNIT=nfslmtp,FILE=TRIM(fNameSoilMoist),FORM='unformatted',ACCESS='sequential',&
            ACTION='read',STATUS='old',IOSTAT=ierr)
       IF (ierr /= 0) THEN
          WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
               TRIM(fNameSoilMoist), ierr
          STOP "**(ERROR)**"
       END IF

       ! do k=1,nzg   ! direct order

       DO k=nzg,1,-1 ! revert reading order
          READ(UNIT=nfslmtp) ((glsm(i,j,k),i=1,iMax),j=1,jMax) ! wetness

          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(glsm(:,:,k) ,glsm_w(:,:,k) )
          ELSE
             CALL IJtoIBJB(glsm(:,:,k) ,glsm_w(:,:,k) )
          END IF

       END DO

       CLOSE(UNIT=nfslmtp)

    ELSE IF (record_type == 'vfm') THEN !vformat model

       OPEN(UNIT=nfslmtp,FILE=TRIM(fNameSoilMoist),FORM='formatted',ACCESS='sequential',&
            ACTION='read',STATUS='old',IOSTAT=ierr)
       IF (ierr /= 0) THEN
          WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
               TRIM(fNameSoilMoist), ierr
          STOP "**(ERROR)**"
       END IF


       ! do k=1,nzg   ! direct order

       DO k=nzg,1,-1 ! revert reading order
          !
          !print*,'================== GLSM for k====================',k
          !
          CALL vfirec(nfslmtp,glsm(1,1,k),iMax*jMax,'LIN')

          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(glsm(:,:,k) ,glsm_w(:,:,k) )
          ELSE
             CALL IJtoIBJB(glsm(:,:,k) ,glsm_w(:,:,k) )
          END IF

       END DO

       CLOSE(UNIT=nfslmtp)

    ELSE

       call FatalError('**(read_gl_sm_bc)** unknown record type')

    END IF

    call MsgOne('**(read_gl_sm_bc)**','DONE')

    RETURN
  END SUBROUTINE read_gl_sm_bc


  SUBROUTINE restartphyscs (jbMax,ifday,tod,idate ,idatec, &
       nfsibo,nfcnv1,ibMaxPerJB)

    INTEGER           ,INTENT(IN   ) :: jbMax
    INTEGER           ,INTENT(IN   ) :: ifday
    REAL(KIND=r8)              ,INTENT(IN   ) :: tod
    INTEGER           ,INTENT(IN   ) :: idate(:)
    INTEGER           ,INTENT(IN   ) :: idatec(:)
    INTEGER           ,INTENT(IN   ) :: nfsibo
    INTEGER           ,INTENT(IN   ) :: nfcnv1
    INTEGER           ,INTENT(IN   ) :: ibMaxPerJB(:)
    INTEGER                         :: i
    INTEGER                         :: j
    INTEGER                         :: ncount

    IF(TRIM(isimp).NE.'YES') THEN

       CALL MsgOne('**(restartphyscs)**','Saving physics state for restart')

       !$OMP DO PRIVATE(ncount, i)
       DO j=1,jbMax
          ncount=0
          DO i=1,ibMaxPerJB(j)
             IF(imask(i,j).GT.0)THEN
                ncount=ncount+1
                IF(ssib(ncount,j).GT.0.0_r8)THEN
                   w0  (ncount,1,j)=-ssib(ncount,j)
                   w0  (ncount,2,j)=-ssib(ncount,j)
                   w0  (ncount,3,j)=-ssib(ncount,j)
                   wm  (ncount,1,j)=-ssib(ncount,j)
                   wm  (ncount,2,j)=-ssib(ncount,j)
                   wm  (ncount,3,j)=-ssib(ncount,j)
                END IF
             END IF
          END DO
       END DO


       !$OMP SINGLE
       WRITE(UNIT=nfsibo) ifday,tod,idate,idatec
       WRITE(UNIT=nfsibo) tm0,tmm
       WRITE(UNIT=nfsibo) qm0,qmm
       WRITE(UNIT=nfsibo) td0,tdm
       WRITE(UNIT=nfsibo) tg0,tgm
       WRITE(UNIT=nfsibo) tc0,tcm
       WRITE(UNIT=nfsibo) w0 ,wm
       WRITE(UNIT=nfsibo) capac0,capacm
       WRITE(UNIT=nfsibo) ppci,ppli
       WRITE(UNIT=nfsibo) gl0 ,zorl,gtsea,tseam,qsfc0,tsfc0,qsfcm,tsfcm
       WRITE(UNIT=nfsibo) imask

       WRITE(UNIT=nfcnv1) ifday,tod,idate,idatec
       WRITE(UNIT=nfcnv1) convc,convt,convb,prcp1,prcp2,prcp3, &
            prcpt,toplv,botlv
       WRITE(UNIT=nfcnv1) rVisDiff,rVisBeam,rNirDiff,rNirBeam, &
            rVisDiffC,rVisBeamC,rNirDiffC,rNirBeamC,rSwToaDown
       !$OMP END SINGLE
    END IF

  END SUBROUTINE restartphyscs
END MODULE FieldsPhysics
