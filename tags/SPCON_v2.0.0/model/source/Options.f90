!
!  $Author: pkubota $
!  $Date: 2010/10/21 10:46:22 $
!  $Revision: 1.30 $
!
MODULE Options
  USE Constants, Only: &
       r8,             &
       i8

  USE Parallelism, ONLY: &
       MsgOne,           &
       FatalError

  IMPLICIT NONE
  PRIVATE
  INTEGER           , PUBLIC           :: trunc
  INTEGER           , PUBLIC           :: vert
  REAL(KIND=r8)     , PUBLIC           :: dt
  CHARACTER(LEN=200), PUBLIC           :: start
  INTEGER           , PUBLIC           :: IDATEI(4)
  INTEGER           , PUBLIC           :: IDATEW(4)
  INTEGER           , PUBLIC           :: IDATEF(4)
  CHARACTER(LEN=6)  , PUBLIC           :: NMSST
  INTEGER           , PUBLIC           :: DHFCT
  INTEGER           , PUBLIC           :: DHRES
  INTEGER           , PUBLIC           :: DHDHN
  INTEGER           , PUBLIC           :: NHDHN
  INTEGER           , PUBLIC           :: DHEXT
  INTEGER           , PUBLIC           :: NHEXT
  LOGICAL           , PUBLIC           :: DOGRH
  LOGICAL           , PUBLIC           :: DOPRC
  LOGICAL           , PUBLIC           :: DOSMC
  CHARACTER(LEN=5)  , PUBLIC           :: PREFX
  CHARACTER(LEN=5)  , PUBLIC           :: PREFY
  CHARACTER(LEN=1)  , PUBLIC           :: TABLE
  CHARACTER(LEN=199), PUBLIC           :: path_in
  CHARACTER(LEN=200), PUBLIC           :: path_in1
  CHARACTER(LEN=200), PUBLIC           :: dirfNameOutput

  INTEGER           , PUBLIC           :: maxtim
  REAL(KIND=r8)     , PUBLIC           :: cth0
  REAL(KIND=r8)     , PUBLIC           :: dct
  INTEGER           , PUBLIC           :: maxtfm
  REAL(KIND=r8)     , PUBLIC           :: ctdh0
  REAL(KIND=r8)     , PUBLIC           :: dctd
  INTEGER           , PUBLIC           :: mdxtfm
  REAL(KIND=r8)     , PUBLIC           :: cteh0
  REAL(KIND=r8)     , PUBLIC           :: dcte
  INTEGER           , PUBLIC           :: mextfm
  INTEGER           , PUBLIC           :: ddelt

  CHARACTER(LEN=6 ) , PUBLIC           :: TRC
  CHARACTER(LEN=6 ) , PUBLIC           :: TRCG
  CHARACTER(LEN=4 ) , PUBLIC           :: LV
  CHARACTER(LEN=10) , PUBLIC           :: TruncLev
  CHARACTER(LEN=5 ) , PUBLIC           :: EXTF
  CHARACTER(LEN=5 ) , PUBLIC           :: EXDF
  CHARACTER(LEN=5 ) , PUBLIC           :: EXTH
  CHARACTER(LEN=5 ) , PUBLIC           :: EXDH
  CHARACTER(LEN=5 ) , PUBLIC           :: EXTW
  CHARACTER(LEN=5 ) , PUBLIC           :: EXDW
  CHARACTER(LEN=5 ) , PUBLIC           :: EXTS

  INTEGER           , PUBLIC           :: reststep
  CHARACTER(LEN=253), PUBLIC           :: fNameList
  CHARACTER(LEN=253), PUBLIC           :: FNameGDHN
  CHARACTER(LEN=253), PUBLIC           :: FNameGDYN
  CHARACTER(LEN=253), PUBLIC           :: FNameGPRC
  CHARACTER(LEN=253), PUBLIC           :: FNameOutGH
  CHARACTER(LEN=253), PUBLIC           :: FNameTopGH
  CHARACTER(LEN=253), PUBLIC           :: FNamenDrGH
  CHARACTER(LEN=253), PUBLIC           :: FNameRestInput2
  CHARACTER(LEN=253), PUBLIC           :: FNameRestOutput2
  CHARACTER(LEN=253), PUBLIC           :: FNameRestInput1
  CHARACTER(LEN=253), PUBLIC           :: FNameRestOutput1    
  CHARACTER(LEN=253), PUBLIC           :: FNameConvClInp0
  CHARACTER(LEN=253), PUBLIC           :: FNameConvClOut1
  CHARACTER(LEN=253), PUBLIC           :: FNameSibPrgInp0
  CHARACTER(LEN=253), PUBLIC           :: FNameSibPrgOut1
  CHARACTER(LEN=251), PUBLIC           :: fNameInput0
  CHARACTER(LEN=251), PUBLIC           :: fNameInput1
  CHARACTER(LEN=212), PUBLIC           :: fNameNmi
  CHARACTER(LEN=200), PUBLIC           :: fNameSSTAOI
  CHARACTER(LEN=226), PUBLIC           :: fNameSnow
  CHARACTER(LEN=211), PUBLIC           :: fNameSoilms
  CHARACTER(LEN=225), PUBLIC           :: fNameSoilType
  CHARACTER(LEN=229), PUBLIC           :: fNameVegType
  CHARACTER(LEN=225), PUBLIC           :: fNameSoilMoist
  CHARACTER(LEN=206), PUBLIC           :: fNameAlbedo
  CHARACTER(LEN=211), PUBLIC           :: fNameGauss
  CHARACTER(LEN=211), PUBLIC           :: fNameWaves
  CHARACTER(LEN=206), PUBLIC           :: fNameCO2   !hmjb
  CHARACTER(LEN=206), PUBLIC           :: fNameOzone !hmjb
  CHARACTER(LEN=206), PUBLIC           :: fNameSpecSW !hmjb
  CHARACTER(LEN=206), PUBLIC           :: fNameSpecLW !hmjb
  CHARACTER(LEN=206), PUBLIC           :: fNameCnfTbl
  CHARACTER(LEN=206), PUBLIC           :: fNameCnf2Tb
  CHARACTER(LEN=206), PUBLIC           :: fNameLookTb
  CHARACTER(LEN=206), PUBLIC           :: fNameUnitTb
  CHARACTER(LEN=206), PUBLIC           :: fNameSibVeg
  CHARACTER(LEN=206), PUBLIC           :: fNameSibAlb
  CHARACTER(LEN=214), PUBLIC           :: fNameDTable
  CHARACTER(LEN=212), PUBLIC           :: fNameGHLoc
  CHARACTER(LEN=209), PUBLIC           :: fNameGHTable
  CHARACTER(LEN=211), PUBLIC           :: fNameOrgvar
  CHARACTER(LEN=211), PUBLIC           :: fNameSibmsk
  CHARACTER(LEN=211), PUBLIC           :: fNameTg3zrl
  CHARACTER(LEN=255), PUBLIC           :: fNameRouLen
  LOGICAL           , PUBLIC           :: slagr=.FALSE.
  LOGICAL           , PUBLIC           :: SL_twotime_scheme=.FALSE.
  LOGICAL           , PUBLIC           :: mgiven=.FALSE.
  LOGICAL           , PUBLIC           :: gaussgiven=.FALSE.
  LOGICAL           , PUBLIC           :: reducedGrid=.FALSE.
  LOGICAL           , PUBLIC           :: linearGrid=.FALSE.
  LOGICAL           , PUBLIC           :: nlnminit=.TRUE.
  LOGICAL           , PUBLIC           :: diabatic=.TRUE.
  LOGICAL           , PUBLIC           :: eigeninit=.FALSE.
  LOGICAL           , PUBLIC           :: rsettov=.TRUE.
  LOGICAL           , PUBLIC           :: intcosz=.TRUE.
  LOGICAL           , PUBLIC           :: Model1D=.FALSE.
  LOGICAL           , PUBLIC           :: GenRestFiles=.FALSE.
  LOGICAL           , PUBLIC           :: rmRestFiles=.FALSE.
  LOGICAL           , PUBLIC           :: MasCon=.FALSE.
  LOGICAL           , PUBLIC           :: MasCon_ps=.FALSE.
  LOGICAL           , PUBLIC           :: givenfouriergroups=.FALSE.
  INTEGER           , PUBLIC           :: nproc_vert=1
  INTEGER           , PUBLIC           :: nscalars=0
  CHARACTER(LEN=3  ), PUBLIC           :: record_type="vfm"
  INTEGER           , PUBLIC           :: iglsm_w=0
  INTEGER           , PUBLIC           :: tamBlock=512
  INTEGER           , PUBLIC           :: ibdim_size=192
  REAL(KIND=r8)     , PUBLIC           :: swint=1.000000_r8
  REAL(KIND=r8)     , PUBLIC           :: trint=3.000000_r8
  REAL(KIND=r8)     , PUBLIC           :: yrl=365.2500_r8
  INTEGER           , PUBLIC           :: kt=0
  INTEGER           , PUBLIC           :: ktm=-1
  INTEGER           , PUBLIC           :: ktp=0
  INTEGER           , PUBLIC           :: jdt=0
  INTEGER           , PUBLIC           :: monl(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  CHARACTER(len=3)  , PUBLIC           :: iswrad="LCH"
  CHARACTER(len=3)  , PUBLIC           :: ilwrad="HRS"
  CHARACTER(len=3)  , PUBLIC           :: iccon ="KUO"
  CHARACTER(len=3)  , PUBLIC           :: ilcon ="YES"
  CHARACTER(len=4)  , PUBLIC           :: iscon ="TIED"
  CHARACTER(len=4)  , PUBLIC           :: idcon ="NO  "
  CHARACTER(len=4)  , PUBLIC           :: iqadj ="NO  "
  CHARACTER(len=4)  , PUBLIC           :: ipbl  ="YES "
  CHARACTER(len=4)  , PUBLIC           :: ievap ="YES "
  CHARACTER(len=4)  , PUBLIC           :: isens ="YES "
  CHARACTER(len=4)  , PUBLIC           :: idrag ="YES "
  CHARACTER(len=4)  , PUBLIC           :: iqdif ="YES "
  CHARACTER(len=4)  , PUBLIC           :: ifft  ="JMA "
  CHARACTER(len=4)  , PUBLIC           :: igwd  ="YES "
  CHARACTER(len=4)  , PUBLIC           :: isimp ="NO  "
  CHARACTER(len=4)  , PUBLIC           :: ickcfl="NO  "
  CHARACTER(len=4)  , PUBLIC           :: enhdif="YES "
  CHARACTER(LEN=4)  , PUBLIC           :: impdif="YES "
  REAL(KIND=r8)     , PUBLIC           :: asolc=0.22_r8
  REAL(KIND=r8)     , PUBLIC           :: asolm=0.14_r8
  REAL(KIND=r8)     , PUBLIC           :: crdcld=1.0_r8
  INTEGER           , PUBLIC           :: grepar1=1
  INTEGER           , PUBLIC           :: grepar2=3
  REAL(KIND=r8)     , PUBLIC           :: grepar3=85.0_r8
  REAL(KIND=r8)     , PUBLIC           :: grepar4=30.0_r8
  INTEGER           , PUBLIC           :: initlz=2
  INTEGER           , PUBLIC           :: nstep=1
  REAL(KIND=r8)     , PUBLIC           :: fint=6.0_r8
  INTEGER           , PUBLIC           :: intsst=-1
  REAL(KIND=r8)     , PUBLIC           :: sstlag=3.5_r8
  INTEGER           , PUBLIC           :: ndord=4
  INTEGER           , PUBLIC           :: nfiles=1
  INTEGER           , PUBLIC           :: ifin=0
  REAL(KIND=r8)     , PUBLIC           :: filta=0.92e0_r8
!  REAL(KIND=r8)     , PUBLIC           :: filtb=(1.0_r8-filta)*0.5_r8
  REAL(KIND=r8)     , PUBLIC           :: filtb=0.04_r8
  REAL(KIND=r8)     , PUBLIC           :: percut=27502.0_r8
  REAL(KIND=r8)     , PUBLIC           :: varcut=1.6e5_r8
  REAL(KIND=r8)     , PUBLIC           :: vmax_est=120.0_r8
  INTEGER           , PUBLIC           :: ifalb=0
  INTEGER           , PUBLIC           :: ifsst=-1
  INTEGER           , PUBLIC           :: ifslm=3
  INTEGER           , PUBLIC           :: ifsnw=3
  REAL(KIND=r8)     , PUBLIC           :: co2val=345.0_r8
  INTEGER           , PUBLIC           :: ifco2=0
  CHARACTER(LEN=4)  , PUBLIC           :: co2ipcc="    "
  INTEGER           , PUBLIC           :: ifozone=0
  REAL(KIND=r8)     , PUBLIC           :: ucrit=100.0_r8
  REAL(KIND=r8)     , PUBLIC           :: taucfl=86400.0_r8
  LOGICAL           , PUBLIC           :: ptime=.TRUE.
  LOGICAL           , PUBLIC           :: allghf=.FALSE.
  REAL(KIND=r8)     , PUBLIC           :: dfilta=0.92_r8
  REAL(KIND=r8)     , PUBLIC           :: dpercu=27502.0_r8
  REAL(KIND=r8)     , PUBLIC           :: vcrit=85.00_r8
  REAL(KIND=r8)     , PUBLIC           :: alpha=2.50_r8
  REAL(KIND=r8)     , PUBLIC           :: ucstr=85.0_r8
  REAL(KIND=r8)     , PUBLIC           :: tcflst=21600.0_r8
  REAL(KIND=r8)     , PUBLIC           :: ucupp=70.0_r8
  REAL(KIND=r8)     , PUBLIC           :: tcflup=2160.0_r8
  REAL(KIND=r8)     , PUBLIC           :: slupp=0.020_r8
  INTEGER           , PUBLIC           :: ifddp=10
  LOGICAL           , PUBLIC           :: doprec=.FALSE.
  LOGICAL           , PUBLIC           :: dodyn=.FALSE.
  LOGICAL           , PUBLIC           :: grhflg=.FALSE.
  REAL(KIND=r8)     , PUBLIC           :: sthick=0.65e0_r8
  REAL(KIND=r8)     , PUBLIC           :: sacum=0.46e0_r8
  REAL(KIND=r8)     , PUBLIC           :: acum0=-2.0e-8_r8
  REAL(KIND=r8)     , PUBLIC           :: tbase=273.15e00_r8
  REAL(KIND=r8)     , PUBLIC           :: ubase=0.0e00_r8
  REAL(KIND=r8)     , PUBLIC           :: vbase=1.0e03_r8
  REAL(KIND=r8)     , PUBLIC           :: rbase=30.0e00_r8
  REAL(KIND=r8)     , PUBLIC           :: dbase=2.0e07_r8
  REAL(KIND=r8)     , PUBLIC           :: pbase=10.0e00_r8
  REAL(KIND=r8)     , PUBLIC           :: tfact=0.000000000000000E+00_r8
  REAL(KIND=r8)     , PUBLIC           :: ufact=0.000000000000000E+00_r8
  REAL(KIND=r8)     , PUBLIC           :: vfact=0.000000000000000E+00_r8
  REAL(KIND=r8)     , PUBLIC           :: rfact=0.000000000000000E+00_r8
  REAL(KIND=r8)     , PUBLIC           :: dfact=0.000000000000000E+00_r8
  REAL(KIND=r8)     , PUBLIC           :: pfact=0.000000000000000E+00_r8
  INTEGER           , PUBLIC           :: mkuo=0
  INTEGER           , PUBLIC           :: mlrg=0
  INTEGER           , PUBLIC           :: is=1
  INTEGER           , PUBLIC           :: ki=1
  LOGICAL           , PUBLIC           :: mxrdcc=.TRUE.
  INTEGER           , PUBLIC           :: lcnvl=2
  INTEGER           , PUBLIC           :: lthncl=80
  REAL(KIND=r8)     , PUBLIC           :: rccmbl=3.0_r8
  INTEGER           , PUBLIC           :: icld=1
  INTEGER           , PUBLIC           :: inalb=2
  INTEGER           , PUBLIC           :: mxiter=200
  REAL(KIND=r8)     , PUBLIC           :: dk=8.0E+15_r8
  REAL(KIND=r8)     , PUBLIC           :: tk=6.0E+15_r8


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !     files
  !     ifxxx=0    xxx is not processed
  !     ifxxx=1    xxx is set to month=idatec(2) in the first call,
  !                but not processed from the subsequent calls.
  !                ifxxx is set to zero after interpolation
  !     ifxxx=2    xxx is interpolated to current day and time every fint
  !                hours synchronized to 00z regardless of initial time.
  !                interpolation is continuous (every time step) if fint<0.
  !     ifxxx=3    xxx is interpolated to current day and time when ifday=0
  !                and tod=0.0 but not processed otherwise
  !                ( appropriate only when xxx is predicted )
  !
  !                the following are for sst only (fint applies as in
  !                ifxxx=2):
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !|********************************* INPUT FILES **************************|
  !| Labels:                                                                |
  !| EXTW = F.unf              EXDN = F.dir                                 |
  !| EXT = 'icn' ou            EXDN(1:2)=EXDW(1:2)= F.                      |
  !|       'inz' ou 'fct' EXDN(3:5)= "dic" if EXT='icn'                     |
  !|   EXDN(3:5)= "din" if EXT='inz'                                        |
  !| EXTS = S.unf              EXDN(3:5)= "dir" if EXT='   '                |
  !| EXDH = F.dir              EXDN(6:6)= "."                               |
  !|                           PREFY= <def em Namelist>                     |
  !| EXTN(1:2)=EXTW(1:2)= F.   PREFX= <def em Namelist>                     |
  !| EXTN(3:5)=EXT(1:3)        Namee= GPRG + PREFX                          |
  !| EXTN(6:6)= "."            Namef= GFCT + PREFX                          |
  !|                                                                        |
  !|------------------------------------------------------------------------|
  !|********************************|****************|**********************|
  !|********** File  Names *********|  Unit  numbers |***    Procedure  ****|
  !|**********             *********|                |* accessing the file *|
  !|********************************|****************|**********************|
  !|--------------------------------|----------------|----------------------|
  !|GFCT<PREFX><labeli><labelc>     | nfcnv0=0=31=32 |  Model (open)        |
  !|<EXTW><trunc><lev>.convcl       | =0->isimp==yes |  FieldsPhysics(      |
  !|                                | =31="warm"     |  InitBoundCond;      |
  !|                                | =32=nfcnv1->   |  InitCheckfile:read) |
  !|                                | "warm"         |                      |
  !|--------------------------------|----------------|----------------------|
  !|GANL<PREFY><labeli><EXTS>.      |  nfin0=18      |  Model (open;read)   |
  !|<trunc><lev> (cold)             |                |  IOLowLevel(ReadHead |
  !|                                |                |  ReadField:read)     |
  !|GFCT<PREFY><labeli><labelc><EXTW|                |                      |
  !|<trunc><lev>.outmdt (warm)      |                |                      |
  !|--------------------------------|----------------|----------------------|
  !|GANL<PREFY><labeli><EXTS>.      |  nfin1=18(cold)|  Model (open;read)   |
  !|<trunc><lev>                    |                |                      |
  !|GFCT<PREFY><labeli><labelc>     |  nfin1=19(warm)|  IOLowLevel(GReadHead|
  !|<EXTW>.<trunc><lev>.outatt      |                |  ;ReadHead: read)    |
  !|--------------------------------|----------------|----------------------|
  !|GL_FAO_01patches.vfm.G<jMax>    |  nfsoiltp=22   |  FieldsPhysics       |
  !|                                |                |  (read_gl_sm_bc:open |
  !|                                |                |  and read)           |
  !|--------------------------------|----------------|----------------------|
  !|GL_VEG_SIB_05patches.vfm.G<jMax>|  nfvegtp=23    |  FieldsPhysics       |
  !|                                |                |  (read_gl_sm_bc:open;|
  !|                                |                |  and read)           |
  !|--------------------------------|----------------|----------------------|
  !|GL_SM.vfm.<labeli>.G<jMax>      |  nfslmtp=24    |  FieldsPhysics       |
  !|                                |                |  (read_gl_sm_bc:open;|
  !|                                |                |  and read)           |
  !|--------------------------------|----------------|----------------------|
  !|TopoVariance.G<jMax>            |  nfvar=33      |  FieldsPhysics       |
  !|<jMax> ---> 5 Digits            |                |  (InitVariancia:open)|
  !|                                |                |  IOLowLevel(ReadVar: |
  !|                                |                |  read)               |
  !|--------------------------------|----------------|----------------------|
  !|Units                           |  nfauntbl=36   |  InputOutput (InitIn-|
  !|                                |                |  putOutput:open;read)|
  !|--------------------------------|----------------|----------------------|
  !|UnitsConvFactor1Table           |  nfcnftbl=37   |  InputOutput (InitIn-|
  !|                                |                |  putOutput:open;read)|
  !|--------------------------------|----------------|----------------------|
  !|UnitsConvFactor2Table           |  nfcnf2tb=38   |  InputOutput (InitIn-|
  !|                                |                |  putOutput:open;read)|
  !|--------------------------------|----------------|----------------------|
  !|UnitsLookUpTable                |  nflooktb=39   |  InputOutput (InitIn-|
  !|                                |                |  putOutput:open;read)|
  !|--------------------------------|----------------|----------------------|
  !|GridHistLocations.G<jMax>       |  nfghloc=42    |  GridHistory (InitGr-|
  !|<jMax> ---> 5 Digits            |                |  idHistory:open;read)|
  !|--------------------------------|----------------|----------------------|
  !|GridHistDesiredTable            |  nfghds=45     |  GridHistory (InitGr-|
  !|                                |                |  idHistory:open;read)|
  !|--------------------------------|----------------|----------------------|
  !|DiagDesiredTable.<pnt>          |  nfdestbl=49   |  Diagnostics(InitDia-|
  !|DiagDesiredTable.<clm>          |                |  gnostics:open;read) |
  !|--------------------------------|----------------|----------------------|
  !|SSTWeekly<labels>.G<jMax>       |  nfsst=50      |  InputOutput (getsbc:|
  !|                                |                |  open); IOLowLevel(  |
  !|<jMax> ---> 5 Digits            |                |  ReadGetSST:read)    |
  !|--------------------------------|----------------|----------------------|
  !|Snow<labeli>.<EXTS>.G<jMax>     |  nfsnw=51      |  InputOutput (getsbc:|
  !|  <jMax> ---> 5 Digits          |                |  open)               |
  !|--------------------------------|----------------|----------------------|
  !|AlbedoSSiB                      |  nfalb=52      |  InputOutput (getsbc:|
  !|                                |                |  open); IOLowLevel(  |
  !|                                |                |  ReadGetALB:read)    |
  !|--------------------------------|----------------|----------------------|
  !|SoilMoisture.G<jMax>            |  nfslm=53      |  InputOutput (getsbc:|
  !|                                |                |  open); IOLowLevel(  |
  !|                                |                |  ReadGetSLM:read)    |
  !|--------------------------------|----------------|----------------------|
  !|co2<labeli>.<trunc><lev>        |  nfco2=54      |  Not used            |
  !|co2clim.<trunc><lev>            |                |                      |
  !|co2mtd.<trunc><lev>             |                |                      |
  !|--------------------------------|----------------|----------------------|
  !|OZONNMC<label>S.unf             |                |                      |
  !|.G<jMax>L<lev>                  |  nfozone=55    |  InputOutput (getsbc:|
  !|ozoneclim.G<jMax>L<lev>         |                |  open)		    |
  !|ozonemtd.G<jMax>L<lev>          |                |                      |
  !|--------------------------------|----------------|----------------------|
  !|spec3a_sw_hadgem1_3             |  nfspecsw=56   | namelist             |
  !|--------------------------------|----------------|----------------------|
  !|spec3a_lw_hadgem1_3             |  nfspeclw=57   | namelist             |
  !|--------------------------------|----------------|----------------------|
  !|DeepSoilTemperature.G<jMax>     |  nftgz0=61     |  FieldsPhysics (Init-|
  !|                                |                |  BoundCond:open)     |
  !|                                |                |  IOLowLevel(ReadGet- |
  !|                                |                |  NFTGZ: read)        |
  !|--------------------------------|----------------|----------------------| 
  !|                                |                |  Roughness Length    |  
  !|RoughnessLength.G<jMax>         |  nfzol=71      |  FieldsPhysics (Init-|
  !|                                |                |  BoundCond:open)     |
  !|                                |                |  IOLowLevel(ReadGet- |
  !|                                |                |  NFTGZ: read)        |
  !|--------------------------------|----------------|----------------------|
  !|                                |                |  Not used            |
  !|diagclouds.dat                  |  nfcldr=74     |  PhyscsDriver(physcs:|
  !|(read/write temporary)          |                |  open,write,read)    |
  !|--------------------------------|----------------|----------------------|
  !|GFCT<PREFX><labeli><labelc>     |  nfsibi=77     |  Model (open)        |
  !|<EXTW>.<trunc><lev>.sibprg      | ou nfsibo(warm)|  FieldsPhysics(Init -|
  !|                                |                |  BoundCond;          |
  !|                                |                |  InitCheckfile:read) |
  !|--------------------------------|----------------|----------------------|
  !|gaussp.G<jMax>                  |  nfgauss=84    |  Diagnostics (opnprg:|
  !|                                |                |  open;write,read)    |
  !|                                |                |                      |
  !|--------------------------------|----------------|----------------------|
  !|mwaves.<trunc>G<jMax>           |  nfwaves=85    |  Diagnostics (opnprg:|
  !|                                |                |  open;write,read)    |
  !|                                |                |                      |
  !|--------------------------------|----------------|----------------------|
  !|NMI.<trunc><lev>                |  nfnmi=80      |  NonLinearNMI(Nlnmi: |
  !|                                |                |  open,read; horiz1:  |
  !|                                |                |  read; horiz2:read;  |
  !|                                |                |  Getmod:open,read    |
  !|                                |                |  Vermod:write;       |
  !|                                |                |  record:write)       |
  !|--------------------------------|----------------|----------------------|
  !|VegetationSSiB                  |  nfsibd =88    |  Surface(vegin:open, |
  !|                                |                |  read)               |
  !|--------------------------------|----------------|----------------------|
  !|VegetationMask.G<jMax>          |  nfsibt=99     |  FieldsPhysics (Init-|
  !|                                |                |  BoundCond:open;read)|
  !|--------------------------------|----------------|----------------------|
  !|------------------------------------------------------------------------|
  !|******************************* OUTPUT FILES ***************************|
  !|------------------------------------------------------------------------|
  !|GFCT<PREFX><labeli><labelc>     |  nfout0=20     |  Model (open;write)  |
  !|<EXTW>.<trunc><lev>.outmdt      |                |                      |
  !|--------------------------------|----------------|----------------------|
  !|GFCT<PREFX><labeli><labelc>     |  nfout1=21     |  Model (open;write)  |
  !|<EXTW>.<trunc><lev>.outatt      |                |  IOLowLevel(GWrite-  |
  !|                                |                |  Head:write)         |
  !|--------------------------------|----------------|----------------------|
  !|GFCT<PREFX><labeli><labelc>     |  nfdrct=25     |  Diagnostics (opnfct:|
  !|<EXDN>.<trunc><lev>             |                |  open); IOLowLevel(  |
  !|                                |                |  WriteDir:write)     |
  !|--------------------------------|----------------|----------------------|
  !|GFCT<PREFX><labeli><labelc>     |  nfdiag=26     |  Diagnostics (opnfct:|
  !|<EXTN>.<trunc><lev>             |                |  open); InputOutput( |
  !|                                |                |  sclout->WriteField: |
  !|                                |                |  write);IOLowLevel(  |
  !|                                |                |  WriteProgHead;      |
  !|                                |                |  WriteField: write)  |
  !|--------------------------------|----------------|----------------------|
  !|GFCT<PREFX><labeli><labelf>F.   |  nffcst=27     |  Diagnostics         |
  !|dir.<trunc><lev>.files          |                |  (opnfct:open;write) |
  !|(F.dir=<EXTW(1:2)> + "dir")     |                |                      |
  !|--------------------------------|----------------|----------------------|
  !|GFCT<PREFX><labeli><labelc>     |  nfcnv1=32     |  Model (open)        |
  !|<EXTW>.<trunc><lev>.convcl      |                |  FieldsPhysics(      |
  !|                                |                |  restartphyscs:write)|
  !|--------------------------------|----------------|----------------------|
  !|GFGH<PREFX><labeli><labelc>     |  nfghdr=43     |  Model (open)        |
  !|<EXDH>.<trunc><lev>             |                |  GridHistory(Init-   |
  !|                                |                |  GridHistory:write)  |
  !|--------------------------------|----------------|----------------------|
  !|GFGH<PREFX><labeli><labelc>     |  nfghtop=44    | GridHistory (Write-  |
  !|F.top.<trunc><lev>              |                | GridHistoryTopo:open)|
  !|                                |                | IOLowLevel(          |
  !|                                |                | WrTopoGrdHist:write) |
  !|--------------------------------|----------------|----------------------|
  !|GFCT<PREFX><labeli><labelc>     |  nfsibo=66     |  Model (open)        |
  !|<EXTW>.<trunc><lev>.sibprg      |                |  FieldsPhysics       |
  !|                                |                | (restartphyscs:write)|
  !|--------------------------------|----------------|----------------------|
  !|diagclouds.dat                  |  nfcldr=74     |  PhyscsDriver(physcs:|
  !|(read/write temporary)          |                |  open,write,read)    |
  !|--------------------------------|----------------|----------------------|
  !|NMI.<trunc><lev>                |  nfnmi=80      |  NonLinearNMI(Nlnmi: |
  !|                                |                |  open,read; horiz1:  |
  !|                                |                |  read; horiz2:read;  |
  !|                                |                |  Getmod:open,read    |
  !|                                |                |  Vermod:write;       |
  !|                                |                |  record:write)       |
  !|--------------------------------|----------------|----------------------|
  !|GPRG<PREFX><labeli><labelc>     |  neprog=81     |  Diagnostics (opnprg:|
  !|                                |                |  open);IOLowLevel(   |
  !|<EXTN>.<trunc><lev>             |                |  WriteProgHead;      |
  !|                                |                |  WriteField: write)  |
  !|--------------------------------|----------------|----------------------|
  !|GPRG<PREFX><labeli><labelc>     |  nedrct =82    |  Diagnostics (opnprg:|
  !|<EXDN>.<trunc><lev>             |                |  open); IOLowLevel(  |
  !|                                |                |  WriteDire:write)    |
  !|--------------------------------|----------------|----------------------|
  !|GPRG<PREFX><labeli><labelf>F.   |  nefcst=83     |  Diagnostics (opnprg:|
  !|<trunc><lev>.files              |                |  open;write)         |
  !|(F.dir=<EXTW(1:2)> + "dir")     |                |                      |
  !|--------------------------------|----------------|----------------------|
  !|gaussp.<trunc>                  |  nfgauss=84    |  Diagnostics (opnprg:|
  !|                                |                |  open;write)         |
  !|                                |                |                      |
  !|--------------------------------|----------------|----------------------|
  !|mwaves.<trunc>                  |  nfwaves=85    |  Diagnostics (opnprg:|
  !|                                |                |  open;write)         |
  !|                                |                |                      |
  !|--------------------------------|----------------|----------------------|
  !|GFCT<PREFX><labeli><labelc>     |  nfghou=91     |  Model (open)        |
  !|<EXTW>.<trunc><lev>             |                |  IOLowLevel(         |
  !|                                |                |  WriteGrdHist:write) |
  !|--------------------------------|----------------|----------------------|
  !|GDHN<PREFX><labeli><labelc>     |  nfdhn=92      |  Model (open)        |
  !|<EXTW>.<trunc><lev>             |                |  IOLowLevel(         |
  !|                                |                |  WriteDiagHead;      |
  !|                                |                |  WriteField:write)   |
  !|--------------------------------|----------------|----------------------|
  !|GPRC<PREFX><labeli><labelc>     |  nfprc=93      |  Model (open)        |
  !|<EXTW>.<trunc><lev>             |                |  IOLowLevel(         |
  !|                                |                |  WriteDiagHead;      |
  !|                                |                |  WriteField:write)   |
  !|--------------------------------|----------------|----------------------|
  !|GDYN<PREFX><labeli><labelf>     |  nfdyn=94      |  Model (open)        |
  !|<EXTW>.<trunc><lev>             |                |  Diagnostics(accpf:  |
  !|                                |                |  write; InputOutput( |
  !|                                |                |  gread4: write       |
  !|------------------------------------------------------------------------|
  ! ************************************************************************
  ! ************* Define I/O file units  ***********************************
  ! ************************************************************************
  INTEGER , PUBLIC                     :: nferr=0      !error print out unit
  !0 no print, 1 less detail, 2 more detail, 3 most detail
  INTEGER , PUBLIC                     :: nfcnv0=0     ! initial information on convective clouds for int. radiation
  INTEGER , PUBLIC                     :: nfprt=6 !standard print out unit
  !0 no print, 1 less detail, 2 more detail, 3 most detail
  INTEGER                              :: nNameList=17     ! namelist read
  INTEGER , PUBLIC                     :: nfin0=18      ! GANLNM input  file at time level t-dt
  INTEGER , PUBLIC                     :: nfin1=18      ! input  file at time level t
  INTEGER , PUBLIC                     :: nfout0=20     ! output file at time level t-dt
  INTEGER , PUBLIC                     :: nfout1=21     ! output file at time level t
  INTEGER , PUBLIC                     :: nfsoiltp=22   ! soil type GL_FAO_01patches file
  INTEGER , PUBLIC                     :: nfvegtp=23    ! vegetation type GL_VEG_SIB_05patches file
  INTEGER , PUBLIC                     :: nfslmtp=24    ! soil moisture GL_SM file
  INTEGER , PUBLIC                     :: nfdrct=25     ! directory for diagnostics
  INTEGER , PUBLIC                     :: nfdiag=26     ! diagnostics
  INTEGER , PUBLIC                     :: nffcst=27     ! intermediate 3-d diagnostics
  INTEGER , PUBLIC                     :: nf2d=28       ! intermediate 2-d diagnostics
  INTEGER , PUBLIC                     :: nfcnv1=32     ! output information on convective clouds for int. radiation
  INTEGER , PUBLIC                     :: nfvar=33      ! surface height variance
  INTEGER , PUBLIC                     :: nfauntbl=36   ! Units file
  INTEGER , PUBLIC                     :: nfcnftbl=37   ! UnitsConvFactor1Table file
  INTEGER , PUBLIC                     :: nfcnf2tb=38   ! UnitsConvFactor2Table file
  INTEGER , PUBLIC                     :: nflooktb=39   ! UnitsLookUpTable file
  INTEGER , PUBLIC                     :: nfghloc=42    ! GridHistLocations.G<jMax> file
  INTEGER , PUBLIC                     :: nfghdr=43     ! GridHistDesiredTable file
  INTEGER , PUBLIC                     :: nfghtop=44    ! GFGH<PREFX><labeli><labelc>F.top.<trunc><lev> topography file
  INTEGER , PUBLIC                     :: nfghds=45     ! GridHistDesiredTable file
  INTEGER , PUBLIC                     :: nfdestbl=49   ! DiagDesiredTable file
  INTEGER , PUBLIC                     :: nfsst=50      ! sst   file
  INTEGER , PUBLIC                     :: nfsnw=51      ! snow   file
  INTEGER , PUBLIC                     :: nfalb=52      ! albedo file
  INTEGER , PUBLIC                     :: nfslm=53      ! soil moisture file
  INTEGER , PUBLIC                     :: nfco2=54      ! co2 file
  INTEGER , PUBLIC                     :: nfzol=71      ! Roughness Length file
  INTEGER , PUBLIC                     :: nfozone=55    ! ozone file
  INTEGER , PUBLIC                     :: nfspecsw=56   ! sw spectral file for ukmet radiation
  INTEGER , PUBLIC                     :: nfspeclw=57   ! lw spectral file for ukmet radiation
  INTEGER , PUBLIC                     :: nftgz0=61     ! ground temperature file
  INTEGER , PUBLIC                     :: nfsibo=66     ! sib prognostic variable output file
  INTEGER , PUBLIC                     :: nfcldr=74     ! temporary diagclouds.dat
  INTEGER , PUBLIC                     :: nfsibi=77     ! sib prognostic variable input  file
  INTEGER , PUBLIC                     :: nfnmi=80      ! normal modes
  INTEGER , PUBLIC                     :: neprog=81     ! neprog : unit file number to output extra prognostics
  INTEGER , PUBLIC                     :: nedrct=82     ! nedrct : unit file number to output description of extra prognostics
  INTEGER , PUBLIC                     :: nefcst=83     ! nefcst : unit file number to output files list of extra prognostics
  INTEGER , PUBLIC                     :: nfgauss=84    ! gaussian points and weights
  INTEGER , PUBLIC                     :: nfwaves=85    ! wave numbers (m) per latitude
  INTEGER , PUBLIC                     :: nfsibd=88     ! sib vegetation parameter
  INTEGER , PUBLIC                     :: nfghou=91     ! gridhistory file
  INTEGER , PUBLIC                     :: nfdhn=92      ! ustress and vstress at surface
  INTEGER , PUBLIC                     :: nfprc=93      ! instantaneous total and convective precipitation
  INTEGER , PUBLIC                     :: nfdyn=94      ! first level of divergence, vorticity, virtual temperature, 
  !                                                     ! specific humidity and log of surface pressure at every time step
  INTEGER , PUBLIC                     :: nfsibt=99     ! sib surface vegetation type
  INTEGER , PUBLIC                     :: nfctrl(100)=(/& ! print control: from 0 (noprint) to 3 (most detail)
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
  !
  ! ************* END Define I/O file units ***********************************


  INTEGER         , PUBLIC             :: maxtid
  INTEGER         , PUBLIC             :: ifilt

  INTEGER , PUBLIC                     :: idate (4)
  INTEGER , PUBLIC                     :: idatec(4)

  REAL(KIND=r8)    , PUBLIC            :: delt

  REAL(KIND=r8)    , PUBLIC            :: cflric

  INTEGER , PUBLIC                     :: istrt
  LOGICAL , PUBLIC                     :: first
  REAL(KIND=r8)    , PUBLIC            :: dtc3x
  REAL(KIND=r8)    , PUBLIC            :: epsflt
  INTEGER , PUBLIC                     :: intg
  INTEGER , PUBLIC                     :: dogwd


  CHARACTER(LEN=10 ), PUBLIC           :: labelsi
  CHARACTER(LEN=10 ), PUBLIC           :: labelsj
  LOGICAL , PUBLIC, ALLOCATABLE        :: cdhl  (:)
  LOGICAL , PUBLIC, ALLOCATABLE        :: cthl  (:)
  INTEGER, PUBLIC :: schemes=1 ! schemes=1 SSiB_Driver

  PUBLIC :: ReadNameList
  PUBLIC :: SetTimeOutput
  PUBLIC :: SetOutPut
  PUBLIC :: DumpOptions
  PUBLIC :: SetFileNameGaussPoints
  PUBLIC :: SetFileNameWavesPerLat

  PUBLIC :: CreateFileName
  INTERFACE CreateFileName
     MODULE PROCEDURE FileNameAnalis, FileNameRest,FileNameGeral
  END INTERFACE

CONTAINS


  !*** Read Namelist File and complete options ***


  SUBROUTINE ReadNameList()
    INTEGER :: ierr
    CHARACTER(LEN=8) :: c0
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadNameList)**"

    NAMELIST /MODEL_RES/trunc,vert,dt,idatei,idatew,idatef,nmsst,&
         dhfct,dhres,dhdhn,nhdhn,dhext,nhext,dogrh,&
         doprc,prefx,prefy,table,path_in,dirfNameOutput

    NAMELIST /MODEL_IN/slagr,SL_twotime_scheme,nlnminit,diabatic,eigeninit, &
         rsettov,intcosz,Model1D,mgiven,gaussgiven,reducedGrid,linearGrid, &
         GenRestFiles,rmRestFiles,MasCon,MasCon_ps,nscalars, &
         record_type,iglsm_w,tamBlock,ibdim_size,givenfouriergroups, &
         nproc_vert

    NAMELIST /PHYSPROC/iswrad,ilwrad,iccon,ilcon,iqdif, &
         iscon,igwd ,isimp,enhdif,asolc,asolm,crdcld, &
         grepar1,grepar2,grepar3,grepar4,iglsm_w

    NAMELIST /PHYSCS/mxrdcc,lcnvl ,lthncl ,rccmbl ,swint  , &
         trint ,icld  ,inalb  ,mxiter ,co2val , &
         sthick,sacum ,acum0  ,tbase  ,mlrg   , &
         is    ,ki, cflric

    NAMELIST /COMCON/initlz , nstep  , fint   , intsst , ndord  ,&
         filta  , percut , varcut , ifsst  , &
         ifsnw  , ifalb  , ifslm  , allghf , &
         ifco2  , ifozone  , &
         dpercu , vcrit  , alpha  , dodyn  , dk, tk

    ! Reads namelist file
    !INTEGER :: IARGC
    !EXTERNAL IARGC

    !if (iargc().eq.0) then
       fnamelist="MODELIN"
    !else
    !   call getarg(1,fnamelist)
    !endif

    OPEN(unit=nNameList, file=fNameList, action="read", status="old", iostat=ierr)
    IF (ierr /= 0) THEN
       WRITE(c0,"(i8)") ierr
       CALL FatalError(h//" open namelist file "//TRIM(ADJUSTL(fNameList))//&
            " returned iostat="//TRIM(ADJUSTL(c0)))
    END IF
    READ (nNameList,MODEL_RES, iostat=ierr)
    IF (ierr /= 0) THEN
       WRITE(c0,"(i8)") ierr
       CALL FatalError(h//" read namelist MODEL_RES from file "//&
            TRIM(ADJUSTL(fNameList))//" returned iostat="//&
            TRIM(ADJUSTL(c0)))
    END IF
    READ (nNameList, MODEL_IN, iostat=ierr)
    IF (ierr /= 0) THEN
       WRITE(c0,"(i8)") ierr
       CALL FatalError(h//" read namelist MODEL_IN from file "//&
            TRIM(ADJUSTL(fNameList))//" returned iostat="//&
            TRIM(ADJUSTL(c0)))
    END IF
    READ (nNameList, PHYSPROC, iostat=ierr)
    IF (ierr /= 0) THEN
       WRITE(c0,"(i8)") ierr
       CALL FatalError(h//" read namelist PHYSPROC from file "//&
            TRIM(ADJUSTL(fNameList))//" returned iostat="//&
            TRIM(ADJUSTL(c0)))
    END IF
    READ (nNameList, PHYSCS, iostat=ierr)
    IF (ierr /= 0) THEN
       WRITE(c0,"(i8)") ierr
       CALL FatalError(h//" read namelist PHYSCS from file "//&
            TRIM(ADJUSTL(fNameList))//" returned iostat="//&
            TRIM(ADJUSTL(c0)))
    END IF
    READ (nNameList, COMCON, iostat=ierr)
    IF (ierr /= 0) THEN
       WRITE(c0,"(i8)") ierr
       CALL FatalError(h//" read namelist COMCON from file "//&
            TRIM(ADJUSTL(fNameList))//" returned iostat="//&
            TRIM(ADJUSTL(c0)))
    END IF

    ! model truncation and levels

    TRCG=" "
    TRC =" "
    IF (.not. Lineargrid) THEN
       IF (trunc < 1000) THEN
          WRITE(TRCG,'(a2,i4.4)')'TQ',trunc
          WRITE(TRC ,'(a1,i4.4)')'T',trunc
       ELSE
          WRITE(TRCG,'(a2,i4.4)')'TQ',trunc
          WRITE(TRC ,'(a1,i4.4)')'T',trunc
       END IF
    ELSE
       IF (trunc < 1000) THEN
          WRITE(TRCG,'(a2,i4.4)')'TL',trunc
          WRITE(TRC ,'(a1,i4.4)')'T',trunc
       ELSE 
          WRITE(TRCG,'(a2,i4.4)')'TL',trunc
          WRITE(TRC ,'(a1,i4.4)')'T',trunc
       END IF
    ENDIF

    LV=" "
    IF (vert < 100) THEN
       WRITE(LV,'(a1,i3.3)')'L',vert
    ELSE
       WRITE(LV,'(a1,i3.3)')'L',vert
    END IF

    TruncLev=TRIM(TRCG)//TRIM(LV)

    ! Complete Options variables

    grhflg = dogrh
    doprec = doprc
    path_in1=TRIM(path_in)//'/'
    ddelt=dt
    delt=dt
    IF (ANY(idatew /= idatef)) THEN
       start='warm'
    ELSE
       start='cold'
    END IF
    IF( TRIM(start) == "warm" )THEN
       CALL SetTimeOutput(idatew ,idatef, dhfct ,nhdhn ,dhdhn ,nhext ,dhext )
    ELSE
       CALL SetTimeOutput(idatei ,idatef, dhfct ,nhdhn ,dhdhn ,nhext ,dhext )
    END IF
    reststep=NINT((dhres*3600)/dt)

    CALL CheckOptions()

    record_type='vfm'
    IF(iglsm_w == 1)ifslm=0

    CALL ColdWarm()

    dtc3x  = 0.0_r8
    maxtid=(51*366*86400)/dt
    filtb =(1.0_r8-filta)*0.5_r8
    !
    !     intg=2  time integration of surface physical variable is done
    !     by leap-frog implicit scheme. this conseves enegy and h2o.
    !     intg=1  time integration of surface physical variable is done
    !     by backward implicit scheme.
    !
    intg =2
    IF(intg == 1) THEN
       epsflt=0.0e0_r8
    ELSE
       epsflt=0.5e0_r8 *(1.0e0_r8 -filta)
    END IF
    ALLOCATE(cdhl(0:maxtid), stat=ierr)
    IF (ierr /= 0) THEN
       WRITE(c0,"(i8)") ierr
       CALL FatalError(h//" allocate cdhl fails with stat="//TRIM(ADJUSTL(c0)))
    END IF
    ALLOCATE(cthl(0:maxtid), stat=ierr)
    IF (ierr /= 0) THEN
       WRITE(c0,"(i8)") ierr
       CALL FatalError(h//" allocate cthl fails with stat="//TRIM(ADJUSTL(c0)))
    END IF
  END SUBROUTINE ReadNameList



  !hmjb - 10/3/2006
  ! Todas as opcoes possiveis de se modificar no modelo deveriam ser testadas depois de lido
  ! o modelin, pois o usuario pode passar algum parametro incorreto! Este eh um esforco nesta
  ! direcao, mas desconheco todas as opcoes do modelo. Por enquanto, apenas as opcoes para
  ! radiacao e conveccao estao sendo testadas


  SUBROUTINE CheckOptions()
    CHARACTER(LEN=8) :: c0
    CHARACTER(LEN=*), PARAMETER :: h="**(CheckOptions)**"

    !------------- Short Wave Radiation

    IF  (TRIM(iswrad) /= 'NON'.AND. &
         TRIM(iswrad) /= 'LCH'.AND. &
         TRIM(iswrad) /= 'CRD'.AND. &
         TRIM(iswrad) /= 'UKM'       ) THEN
       CALL FatalError(h//" Unknown option iswrad="//TRIM(iswrad)//&
            "; Known options are: NON, LCH, CRD, UKM")
    END IF

    !  Check options for Clirad and UKMet Short Wave Radiation

    IF (TRIM(iswrad) == 'CRD') THEN
       IF (.NOT.(crdcld == 1_i8 .OR. crdcld == 4_i8)) THEN
          WRITE(c0,"(f8.2)") crdcld
          CALL FatalError(h//" Wrong cloud scheme option crdcld="//&
               TRIM(ADJUSTL(c0))//"; Valid options are: "//&
               "1 (stable) or 4 (experimental)")
       ELSE IF (crdcld == 4_i8) then
          CALL MsgOne(h, " WARN: CCM3 clouds + Clirad used for research only !!!")
       END IF
       IF (.NOT.(0.0_r8 <= asolc .AND. asolc <= 50.0_r8)) THEN
          WRITE(c0,"(f8.2)") asolc
          CALL FatalError(h//" Invalid option asolc="//&
               TRIM(ADJUSTL(c0))//"; Valid range: [0.0, 50.0]")
       END IF
       IF (.NOT.(0.0_r8 <= asolm .AND. asolm <= 50.0_r8)) THEN
          WRITE(c0,"(f8.2)") asolm
          CALL FatalError(h//" Invalid option asolm="//&
               TRIM(ADJUSTL(c0))//"; Valid range: [0.0, 50.0]")
       END IF
    ELSE IF (TRIM(iswrad) == 'UKM') THEN

    END IF

    !------------- Long Wave Radiation

    IF  (TRIM(ilwrad) /= 'NON' .AND. &
         TRIM(ilwrad) /= 'HRS' .AND. &
         TRIM(ilwrad) /= 'CRD' .AND. &
         TRIM(ilwrad) /= 'UKM'       ) THEN
       CALL FatalError(h//" Unknown option iswrad= "//TRIM(iswrad)//&
            "; Known options are: NON, HRS, CRD, UKM")
    END IF

    ! Check options for Clirad and UKMET Long Wave Radiation

    IF (TRIM(ilwrad) == 'CRD') THEN
       CALL FatalError(h//" Clirad-LW (ilwrad=CRD) not yet available")
    END IF

    !------------- Gases

    IF (ifco2 < -1 .OR. ifco2 > 4) THEN
       WRITE(c0,"(i8)") ifco2
       CALL FatalError(h//" Invalid option ifco2="//TRIM(ADJUSTL(c0))//&
            "; valid values are: -1, 0, 1, 2, 3 or 4")
    ELSE IF (ifco2 >= 1 .AND. ifco2 <= 4) THEN
       WRITE(c0,"(i8)") ifco2
       CALL FatalError(h//" Invalid option ifco2="//TRIM(ADJUSTL(c0))//&
            "; reading co2 field not implemented yet!")
    END IF
    IF (ifozone < 0 .OR. ifozone > 4) THEN
       WRITE(c0,"(i8)") ifozone
       CALL FatalError(h//" Invalid option ifozone="//TRIM(ADJUSTL(c0))//&
            "; valid values are: -0, 1, 2, 3 or 4")
    END IF

    !------------- Convection

    IF  (TRIM(iccon) /= 'NON' .AND. &
         TRIM(iccon) /= 'ARA' .AND. &
         TRIM(iccon) /= 'KUO' .AND. &
         TRIM(iccon) /= 'GRE'         ) THEN
       CALL FatalError(h//" Unknown option iccon= "//TRIM(iccon)//&
            "; Known options are: ARA, KUO or GRE")
    END IF

    ! Check options for grell

    IF (TRIM(iccon) == 'GRE') THEN
       IF  (grepar1 /= 0  .AND. &
            grepar1 /= 1  .AND. &
            grepar1 /= 4  .AND. &
            grepar1 /= 7  .AND. &
            grepar1 /= 10 .AND. &
            grepar1 /= 13 .AND. &
            grepar1 /= 24        ) THEN
          WRITE(c0,"(i8)") grepar1
          CALL FatalError(h//" Unknown option grepar1="//TRIM(ADJUSTL(c0))//&
               "; known options are: 0, 1, 4, 7, 10, 13 or 24")
       END IF
       IF  (grepar2 /=  1 .AND. &
            grepar2 /=  2 .AND. &
            grepar2 /=  3        ) THEN
          WRITE(c0,"(i8)") grepar2
          CALL FatalError(h//" Unknown option grepar2="//TRIM(ADJUSTL(c0))//&
               "; known options are: 1, 2 or 3")
       END IF
       IF (.NOT. (25.0_r8 <= grepar3 .AND. grepar3 <= 125.0_r8)) THEN
          WRITE(c0,"(f8.2)") grepar3
          CALL FatalError(h//" Invalid option grepar3="//TRIM(ADJUSTL(c0))//&
               "; valid values range: [25.0, 125.0]")
       END IF
       IF (.NOT. (15.0_r8 <= grepar4 .AND. grepar4 <= 75.0_r8)) THEN
          WRITE(c0,"(f8.2)") grepar4
          CALL FatalError(h//" Invalid option grepar4="//TRIM(ADJUSTL(c0))//&
               "; valid values range: [15.0, 75.0]")
       END IF
    END IF

    IF (.NOT.(&
         TRIM(iscon) == 'NON' .OR. &
         TRIM(iscon) == 'TIED'.OR. &
         TRIM(iscon) == 'SOUZ'       )) THEN
       CALL FatalError(h//" Unknown option iscon="//TRIM(iscon)//&
            "; known options are: TIED or SOUZ")
    END IF
  END SUBROUTINE CheckOptions






  SUBROUTINE DumpOptions()
    CHARACTER(LEN=14) :: runsFrom, runsTo, runsInitial
    CHARACTER(LEN=16) :: c0
    CHARACTER(LEN=16) :: c1
    CHARACTER(LEN=128) :: line
    CHARACTER(LEN=*), PARAMETER :: tab="    "
    CHARACTER(LEN=*), PARAMETER :: h="**(DumpOptions)**"

    ! first line

    CALL MsgOne(h, tab)

    ! model resolution and running period
    
    runsInitial="  Z   /  /    "
    WRITE(runsInitial( 1:2 ), "(i2.2)") idatei(1)
    WRITE(runsInitial( 5:6 ), "(i2.2)") idatei(2)
    WRITE(runsInitial( 8:9 ), "(i2.2)") idatei(3)
    WRITE(runsInitial(11:14), "(i4.4)") idatei(4)
    runsTo="  Z   /  /    "
    WRITE(runsTo( 1:2 ), "(i2.2)") idatef(1)
    WRITE(runsTo( 5:6 ), "(i2.2)") idatef(2)
    WRITE(runsTo( 8:9 ), "(i2.2)") idatef(3)
    WRITE(runsTo(11:14), "(i4.4)") idatef(4)
    runsFrom="  Z   /  /    "
    IF (TRIM(start) == "cold") THEN
       WRITE(runsFrom( 1:2 ), "(i2.2)") idatei(1)
       WRITE(runsFrom( 5:6 ), "(i2.2)") idatei(2)
       WRITE(runsFrom( 8:9 ), "(i2.2)") idatei(3)
       WRITE(runsFrom(11:14), "(i4.4)") idatei(4)
    ELSE
       WRITE(runsFrom( 1:2 ), "(i2.2)") idatew(1)
       WRITE(runsFrom( 5:6 ), "(i2.2)") idatew(2)
       WRITE(runsFrom( 8:9 ), "(i2.2)") idatew(3)
       WRITE(runsFrom(11:14), "(i4.4)") idatew(4)
    END IF
    CALL MsgOne(h, " model "//TRIM(TruncLev)//&
         &" runs from "//runsFrom//" to "//runsTo//&
         &" with initial state from "//runsInitial)
    
    ! timestep info
    
    WRITE(c0,"(i16)") maxtim
    WRITE(c1,"(i16)") ddelt
    CALL MsgOne(h, " model executes "//TRIM(ADJUSTL(c0))//&
         &" timesteps of length "//TRIM(ADJUSTL(c1))//" seconds ")

    ! model configuration

    IF (slagr) THEN
       line = "Semi-Lagrangean"
       IF (SL_twotime_scheme) THEN
          line = TRIM(line)//", two-time level scheme"
         ELSE
          line = TRIM(line)//", three-time level scheme"
       ENDIF
    ELSE
       line = "Eulerian"
    END IF
    IF (reducedGrid) THEN
       line = TRIM(line)//", Reduced Gaussian and"
    ELSE
       line = TRIM(line)//", Full Gaussian and"
    END IF
    IF (linearGrid) THEN
       line = TRIM(line)//" Linear Grid"
    ELSE
       line = TRIM(line)//" Quadratic Grid"
    END IF
    CALL MsgOne(h, " model dynamics configuration is  "//TRIM(line))

    ! files

    CALL MsgOne(h, " input file name is "//TRIM(fNameInput0))
    CALL MsgOne(h, " output file directory is "//TRIM(dirFNameOutput))

    ! physics

    CALL MsgOne(h, " model physics configuration:")

    ! Shortwave Radiation

    IF (TRIM(iswrad).eq.'NON') THEN
       CALL MsgOne(h, tab//"No Shortwave Radiation")
    ELSEIF (TRIM(iswrad).eq.'LCH') THEN
       CALL MsgOne(h, tab//"Shortwave Radiation is Lacis & Hansen")
    ELSEIF (TRIM(iswrad).eq.'CRD') THEN
       CALL MsgOne(h, tab//"Shortwave Radiation is CLIRAD-SW")
       WRITE(c0,"(e16.7)") asolm
       CALL MsgOne(h, tab//"Maritime aerosol is "//TRIM(ADJUSTL(c0)))
       WRITE(c0,"(e16.7)") asolc
       CALL MsgOne(h, tab//"Continental aerosol is "//TRIM(ADJUSTL(c0)))
       WRITE(c0,"(e16.7)") crdcld
       CALL MsgOne(h, tab//"Cloud Scheme is "//TRIM(ADJUSTL(c0)))
    ELSEIF (TRIM(iswrad).eq.'UKM') THEN
       CALL MsgOne(h, tab//"Shortwave Radiation is UKMO-SW")
       CALL MsgOne(h, tab//tab//"spectral-file: "//TRIM(fNameSpecSW))
    ENDIF

    ! Longwave Radiation

    IF (TRIM(ilwrad).eq.'NON') THEN
       CALL MsgOne(h, tab//"No Longwave Radiation")
    ELSEIF (TRIM(ilwrad).eq.'HRS') THEN
       CALL MsgOne(h, tab//"Longwave Radiation is Harshvardhan")
    ELSEIF (TRIM(ilwrad).eq.'CRD') THEN
       CALL MsgOne(h, tab//"Longwave Radiation is Clirad-LW")
    ELSEIF (TRIM(ilwrad).eq.'UKM') THEN
       CALL MsgOne(h, tab//"Longwave Radiation is UKMO-LW")
       CALL MsgOne(h, tab//tab//"spectral-file: "//TRIM(fNameSpecLW))
    ENDIF

    ! CO2

    IF (ifco2.LE.0) THEN
       CALL MsgOne(h, tab//"Using a single global value for co2 concentration,")
       IF (ifco2.EQ.0) THEN
          WRITE(c0,"(e16.7)") co2val
          CALL MsgOne(h, tab//tab//"with constant concentration of "//&
               TRIM(ADJUSTL(c0))//" ppmv")
       ELSEIF (ifco2.EQ.-1) THEN
          CALL MsgOne(h, tab//tab//"with variable co2val following 2nd order fit to Mauna Loa data (1958-04)")
       ENDIF
    ELSE
       CALL MsgOne(h, tab//"Reading global co2 field from external file")
       IF (ifco2.EQ.1) THEN
          CALL MsgOne(h, tab//tab//"CO2 field is constant")
       ELSEIF (ifco2.EQ.2) THEN
          CALL MsgOne(h, tab//tab//"CO2 field is climatology")
       ELSEIF (ifco2.EQ.3) THEN
          CALL MsgOne(h, tab//tab//"CO2 field is predicted")
       ELSEIF (ifco2.EQ.4) THEN
          CALL MsgOne(h, tab//tab//"CO2 field is direct access")
       ENDIF
    ENDIF

    ! OZONE

    IF (ifozone.EQ.0) THEN
       CALL MsgOne(h, tab//"Using old 4-month zonally averaged OZONE climatology")
    ELSE
       CALL MsgOne(h, tab//"Reading global OZONE field from external file")
       IF (ifozone.EQ.1) THEN
          CALL MsgOne(h, tab//"OZONE field is constant")
       ELSEIF (ifozone.EQ.2) THEN
          CALL MsgOne(h, tab//"OZONE field is climatology")
       ELSEIF (ifozone.EQ.3) THEN
          CALL MsgOne(h, tab//"OZONE field is predicted")
       ELSEIF (ifozone.EQ.4) THEN
          CALL MsgOne(h, tab//"OZONE field is continuous")
       ENDIF
    ENDIF

    ! Deep Convection

    IF (TRIM(iccon).eq.'NON') THEN
       CALL MsgOne(h, tab//"No Deep Convection selected")
    ELSEIF (TRIM(iccon).eq.'ARA') THEN
       CALL MsgOne(h, tab//"Deep Convection is Arakawa")
    ELSEIF (TRIM(iccon).eq.'KUO') THEN
       CALL MsgOne(h, tab//"Deep Convection is KUO")
    ELSEIF (TRIM(iccon).eq.'GRE') THEN
       CALL MsgOne(h, tab//"Deep Convection is GRELL with parameters:")
       WRITE(c0,"(i16)") grepar1
       WRITE(c1,"(i16)") grepar2
       CALL MsgOne(h, tab//tab//"grepar1="//TRIM(ADJUSTL(c0))//&
            &"; grepar2="//TRIM(ADJUSTL(c1)))
       WRITE(c0,"(e16.7)") grepar3
       WRITE(c1,"(e16.7)") grepar4
       CALL MsgOne(h, tab//tab//"grepar3="//TRIM(ADJUSTL(c0))//&
            &"; grepar4="//TRIM(ADJUSTL(c1)))
    ENDIF

    ! Shallow Convection

    IF (TRIM(iscon).eq.'NON') THEN
       CALL MsgOne(h, tab//"No Shallow Convection selected")
    ELSEIF (TRIM(iscon).eq.'TIED') THEN
       CALL MsgOne(h, tab//"Shallow Convection is Tiedke")
    ELSEIF (TRIM(iscon).eq.'SOUZ') THEN
       CALL MsgOne(h, tab//"Shallow Convection is Souza")
    ENDIF

    ! last line

    CALL MsgOne(h, tab)
  END SUBROUTINE DumpOptions








  SUBROUTINE SetTimeOutput(idate ,idatec,dhfct ,nhdhn ,dhdhn ,nhext ,dhext )

    INTEGER, INTENT(IN     ) :: idate (4)
    INTEGER, INTENT(IN     ) :: idatec(4)
    INTEGER, INTENT(IN     ) :: dhfct
    INTEGER, INTENT(INOUT  ) :: nhdhn
    INTEGER, INTENT(INOUT  ) :: dhdhn
    INTEGER, INTENT(INOUT  ) :: nhext
    INTEGER, INTENT(INOUT  ) :: dhext
    INTEGER                  :: yi
    INTEGER                  :: mi
    INTEGER                  :: di
    INTEGER                  :: hi
    INTEGER                  :: yf
    INTEGER                  :: mf
    INTEGER                  :: df
    INTEGER                  :: hf
    INTEGER                  :: ntstepmax
    REAL(KIND=r8)                     :: xday
    REAL(KIND=r8)                     :: datehr
    REAL(KIND=r8)                     :: datehf
    INTEGER                  :: nday
    REAL(KIND=r8)                     :: ybi
    INTEGER                  :: md(12)
    INTEGER                  :: ntstep
    INTEGER                  :: mhfct
    INTEGER                  :: mhdhn
    INTEGER                  :: mhext
    REAL(KIND=r8)                     :: dh
    REAL(KIND=r8)                     :: nts
    REAL(KIND=r8)                     :: mhf
    REAL(KIND=r8)                     :: chk

    hi = idate (1)
    di = idate (2)
    mi = idate (3)
    yi = idate (4)
    hf = idatec(1)
    df = idatec(2)
    mf = idatec(3)
    yf = idatec(4)

    CALL jull(yi,mi,di,hi,xday)
    datehr=yi+(xday/365.25e0_r8)
    CALL jull(yf,mf,df,hf,xday)
    datehf=yf+(xday/365.25e0_r8)
    nday=0
    IF(yi == yf .AND. mi==mf .AND. di==df) THEN
       nday=0
    ELSE
       DO WHILE (datehr < datehf)
          nday=nday+1
          ybi=MOD(yi,4)
          IF ( ybi == 0.0_r8 )THEN
             md =(/31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
          ELSE
             md =(/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
          END IF
          di=di+1
          IF( di > md(mi) )THEN
             di=1
             mi=mi+1
             IF ( mi > 12 ) THEN
                mi=1
                yi=yi+1
             END IF
          END IF
          CALL jull(yi,mi,di,hi,xday)
          datehr=yi+(xday/365.25e0_r8)
       END DO
    END IF
    ntstep=(nday)*86400/dt
    IF(dhfct /= 0 ) THEN
       mhfct=nday*24/dhfct
    ELSE
       mhfct=17
    END IF

    IF( nhdhn == 0 ) THEN
       dhdhn=0
       mhdhn=0
    ELSE IF ( dhdhn /= 0 ) THEN
       mhdhn=nhdhn/dhdhn
    ELSE
       mhdhn=0
       nhdhn=0
    END IF
    IF ( nhext == 0 ) THEN
       dhext=0
       !**(JP)** faltou atribuicao abaixo
       mhext=0
    ELSE IF ( dhext /= 0 ) THEN
       mhext=nhext/dhext
    ELSE
       mhext=0
       nhext=0
    END IF
    IF ( dhfct /= 0 ) THEN
       IF ( hi /= hf ) THEN
          dh =hf-hi
          nts=dh*3600/dt
          mhf=dh/dhfct
          chk=mhf*dhfct
          IF ( chk /= dh ) THEN
             WRITE(nfprt,*) 'Wrong Request for the Hour in datef =', yf,mf,df,hf
             WRITE(nfprt,*) 'Difference of Hours in datei = ',yi,mi,di,hi, 'and '
             WRITE(nfprt,*) 'datef is Not Compatible With dhfct =' ,dhfct
             STOP
          END IF
          ntstep=ntstep+nts
          mhfct=mhfct+mhf
       END IF
    END IF
    maxtim=ntstep
    maxtfm=mhfct
    cth0 =dhfct
    mdxtfm=mhdhn
    ctdh0=dhdhn
    mextfm=mhext
    cteh0=dhext
    ntstepmax=51*366*86400/dt
    IF( ntstep > ntstepmax ) THEN
       WRITE(nfprt,*) 'nstep = ',ntstep,' is greater than ntstepmax = ',ntstepmax
       STOP
    END IF
    dct=cth0
    dctd=ctdh0
    dcte=cteh0
  END SUBROUTINE SetTimeOutput






  SUBROUTINE jull(yi,mi,di,hi,xday)

    INTEGER, INTENT(IN   ) :: yi
    INTEGER, INTENT(IN   ) :: mi
    INTEGER, INTENT(IN   ) :: di
    INTEGER, INTENT(IN   ) :: hi
    REAL(KIND=r8)   , INTENT(OUT  ) :: xday
    REAL(KIND=r8)                   :: tod
    REAL(KIND=r8)                   :: yrl
    INTEGER                :: monl(12)
    INTEGER                :: monday(12)
    INTEGER                :: m
    REAL(KIND=r8)   , PARAMETER     :: f3600=3.6e3_r8
    tod=0.0_r8
    yrl=365.25e0_r8
    MONL    =   (/31,28,31,30,31,30,31,31,30,31,30,31/)
    !
    !     id is now assumed to be the current date and hour
    !
    monday(1)=0
    DO m=2,12
       monday(m)=monday(m-1)+monl(m-1)
    END DO
    xday=hi*f3600
    xday=xday+MOD(tod,f3600)
    xday=monday(mi)+di+xday/86400.0_r8
    xday=xday-MOD(yi+3,4)*0.25_r8
    IF(MOD(yi,4).EQ.0.AND.mi.GT.2)xday=xday+1.0e0_r8
    xday= MOD(xday-1.0_r8,yrl)
  END SUBROUTINE jull

 
  SUBROUTINE  ColdWarm()
    CHARACTER(LEN=*), PARAMETER :: h="**(ColdWarm)**"
    IF ( TRIM(START) == 'cold' ) THEN
       first = .TRUE.
    ELSEIF ( TRIM(START) == 'warm' ) THEN
       nlnminit = .FALSE.
       diabatic = .FALSE.
       eigeninit= .FALSE.
       rsettov  = .FALSE.
       first    = .FALSE.
       nfin1 =19
       nfcnv0=31
       initlz=0
       ifsnw=0
       ifalb=0
       ifslm=0
    END IF
  END SUBROUTINE ColdWarm




  SUBROUTINE setsst (c0)
    CHARACTER(LEN=*), INTENT(IN   ) :: c0

    INTEGER           :: LFSST
    INTEGER           :: LWSST
    CHARACTER(LEN=8)  :: LABELS
    LOGICAL           :: lexist
    CHARACTER(LEN=*), PARAMETER :: h="**(setsst)**"

    IF(LEN(TRIM(NMSST)) /= 6)THEN
       CALL FatalError(h//" NMSST file ("//TRIM(NMSST)//") not set")
    END IF

    WRITE(LABELS,'(i4.4,2i2.2)')idate(4),idate(2),idate(3)
    WRITE (labelsi(1: 4), '(I4.4)') idate(4)
    WRITE (labelsi(5: 6), '(I2.2)') 01
    WRITE (labelsi(7: 8), '(I2.2)') 16
    WRITE (labelsi(9:10), '(I2.2)') 12
    WRITE (labelsj(1: 4), '(I4.4)') idate(4)
    WRITE (labelsj(5: 6), '(I2.2)') 02
    WRITE (labelsj(7: 8), '(I2.2)') 14
    WRITE (labelsj(9:10), '(I2.2)') 00

    IF ( TRIM(NMSST) == 'sstwkl' ) THEN
       fNameSSTAOI=TRIM(path_in1)//'SSTWeekly'//LABELS//'.G'//TRIM(c0)
       INQUIRE (FILE=TRIM(fNameSSTAOI),exist=lexist)
       IF(lexist) THEN
          fNameSSTAOI=TRIM(path_in1)//'SSTWeekly'//LABELS//'.G'//TRIM(c0)
       ELSE
          NMSST='sstaoi'
          CALL MsgOne(h, '*******************************************************')
          CALL MsgOne(h, '* NMSST changed from weekly running mean (sstwkl) to    *')
          CALL MsgOne(h, '* climatology (sstaoi), since sstwkl are unavailable  *')
          CALL MsgOne(h, '* for the last 15 days                                *')
          CALL MsgOne(h, '*******************************************************')
          fNameSSTAOI=TRIM(path_in1)//'SSTClima'//LABELS//'.G'//TRIM(c0)
       END IF
    ELSE IF ( TRIM(NMSST) == 'sstwkd' ) THEN
       fNameSSTAOI=TRIM(path_in1)//TRIM(NMSST)//LABELS//'.'//TRIM(TRC)
    ELSE IF ( TRIM(NMSST) == 'sstmtd' ) THEN
       fNameSSTAOI=TRIM(path_in1)//TRIM(NMSST)//LABELS//'.'//TRIM(TRC)
    ELSE IF ( TRIM(NMSST) == 'sstanp' ) THEN
       fNameSSTAOI=TRIM(path_in1)//TRIM(NMSST)//LABELS//'.'//TRIM(TRC)
    ELSE
       NMSST='sstaoi'
       fNameSSTAOI=TRIM(path_in1)//'SSTClima'//LABELS//'.G'//TRIM(c0)
    END IF

    INQUIRE (FILE=TRIM(fNameSSTAOI),exist=lexist)
    IF (.NOT. lexist) THEN
       CALL FatalError(h//" file "//TRIM(fNameSSTAOI)//" does not exist")
       STOP
    END IF


    IF ( TRIM(NMSST) == 'sstwkl' ) THEN
       LFSST=-1
    ELSE
       LFSST=2
    END IF

    IF      ( TRIM(NMSST) == 'sstwkd' ) THEN
       LFSST=4
    ELSE IF ( TRIM(NMSST) == 'sstmtd' ) THEN
       LFSST=4
    END IF

    IF (TRIM(START)  == 'warm') THEN
       IF ( LFSST == -1 ) THEN
          LWSST=0
       ELSE
          LWSST=LFSST
       END IF
    END IF

    IF ( TRIM(START) /= 'warm')THEN
       ifsst=LFSST
    ELSE
       ifsst=LWSST
    END IF
  END SUBROUTINE setsst



  SUBROUTINE SetFileNameGaussPoints(jMax)
    INTEGER, INTENT(IN) :: jMax
    CHARACTER(LEN= 10)                 :: LABELI
    CHARACTER(LEN= 10)                 :: LABELC
    CHARACTER(LEN= 10)                 :: LABELF
    LOGICAL :: lexist
    CHARACTER(LEN=5) :: c0
    CHARACTER(LEN=*), PARAMETER :: h="**(SetFileNameGaussPoints)**"

    WRITE(LABELI,'(i4.4,3i2.2)')IDATEI(4),IDATEI(3),IDATEI(2),IDATEI(1)
    WRITE(LABELC,'(i4.4,3i2.2)')IDATEW(4),IDATEW(3),IDATEW(2),IDATEW(1)
    WRITE(LABELF,'(i4.4,3i2.2)')IDATEF(4),IDATEF(3),IDATEF(2),IDATEF(1)
    EXTS    ='S.unf'

    WRITE(c0,"(i5.5)") jMax
    IF (gaussGiven) THEN
       fNameGauss  = TRIM(path_in1)//'/gaussp'//'.G'//c0
       INQUIRE (FILE=TRIM(fNameGauss),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameGauss)//" does not exist")
       END IF
    ELSE
       fNameGauss  = TRIM(path_in1)//'/gaussp'//'.G'//c0
    END IF

    ! SET CLIMATOLOGICAL VEGETATION MASK FILE NAME

    fNameSibmsk = TRIM(path_in1)//'VegetationMask'//'.G'//c0

    ! SET CLIMATOLOGICAL SNOW FILE NAME

    fNameSnow   = TRIM(path_in1)//'Snow'//LABELI//EXTS//'.G'//c0

    ! SET CLIMATOLOGICAL SOIL MOISTURE FILE NAME

    fNameSoilms = TRIM(path_in1)//'SoilMoisture'//'.G'//c0

    ! SET TOPOGRAPHY VARIANCE "m" FILE NAME

    fNameOrgvar = TRIM(path_in1)//'TopoVariance'//'.G'//c0
 
    ! SET PARAMETER OF ALBEDO TO SSIB FILE NAME
 
    fNameSibAlb = TRIM(path_in1)//'AlbedoSSiB'
   
    ! SET CLIMATOLOGICAL ALBEDO FILE NAME  
    
    fNameAlbedo = TRIM(path_in1)//'AlbedoSSiB'!errado mas nao usa este arquivo

    ! SET TABLE UNITS FILE NAME      

    fNameUnitTb = TRIM(path_in1)//'Units'

    ! SET INDEX TO CONVERSION OF UNITS FILE NAME      

    fNameLookTb = TRIM(path_in1)//'UnitsLookUpTable'

    ! SET TABLE CONVERSION FILE NAME      

    fNameCnfTbl = TRIM(path_in1)//'UnitsConvFactor1Table'

    ! SET TABLE CONVERSION FILE NAME      

    fNameCnf2Tb = TRIM(path_in1)//'UnitsConvFactor2Table'

    ! SET GRID HISTORY FIELDS FILE NAME      

    fNameGHTable= TRIM(path_in1)//'GridHistDesiredTable'

    ! SET SSIB PARAMETER OF VEGETATION FILE NAME

    fNameSibVeg = TRIM(path_in1)//'VegetationSSiB'

    ! SET DEEP SOIL TEMPERATURE FIELD FILE NAME

    fNameTg3zrl = TRIM(path_in1)//'DeepSoilTemperature'//'.G'//c0

    ! SET ROUGHNESS LENGTH FIELD FILE NAME

    fNameRouLen = TRIM(path_in1)//'RoughnessLength'//'.G'//c0

    ! SET DIAGNOSTICS FIELDS FILE NAME

    IF(TRIM(TABLE) == 'p') THEN
       fNameDTable=TRIM(path_in1)//'DiagDesiredTable.pnt'
    ELSE IF(TRIM(TABLE) == 'c') THEN
       fNameDTable=TRIM(path_in1)//'DiagDesiredTable.clm'
    ELSE IF(TRIM(TABLE) == 'n') THEN
       fNameDTable=TRIM(path_in1)//'DiagDesiredTable'
    ELSE
       fNameDTable=TRIM(path_in1)//'DiagDesiredTable'
    END IF

    ! SET TABLE GRID HISTORY FILE NAME

    fNameGHLoc  = TRIM(path_in1)//'GridHistLocations'//'.G'//TRIM(c0)
    
    ! SET  SOIL TYPE FILE NAME
    
    fNameSoilType= TRIM(path_in1)//'GL_FAO_01patches'//'.'//TRIM(record_type)//'.G'//TRIM(c0)

    ! SET  VEGETATION TYPE FILE NAME

    fNameVegType =TRIM(path_in1)//'GL_VEG_SIB_05patches'//'.'//trim(record_type)//'.G'//TRIM(c0)

    ! SET  SOIL MOISTURE FILE NAME

    fNameSoilMoist =TRIM(path_in1)//'GL_SM'//'.'//TRIM(record_type)//'.'//TRIM(LABELI)//'.G'//TRIM(c0)

    ! SET CO2 FILE NAME (TEST)
    
    IF (ifco2 > 0) THEN
       IF (ifco2 >= 1 .and. ifco2 <=4 ) THEN
          IF (ifco2 == 1) THEN ! first call only
             fNameCO2  = TRIM(path_in1)//'co2'//LABELI//'.G'//TRIM(c0)//TRIM(LV)
          ELSEIF (ifco2 == 2) THEN ! clim interpol
             fNameCO2  = TRIM(path_in1)//'co2clm'//'.G'//TRIM(c0)//TRIM(LV)
          ELSEIF (ifco2 == 3) THEN ! pred interpol
             fNameCO2  = TRIM(path_in1)//'co2fct'//'.G'//TRIM(c0)//TRIM(LV)
          ELSEIF (ifco2 == 4) THEN ! direct access
             fNameCO2  = TRIM(path_in1)//'co2mtd'//'.G'//TRIM(c0)//TRIM(LV)
          ENDIF
       ENDIF
    ENDIF

    ! SET OZONE FILE NAME
    
    IF (ifozone >=1 .and. ifozone <=4 ) THEN
       IF (ifozone == 1) THEN ! first call only 
          fNameOzone  = TRIM(path_in1)//'OZON'//TRIM(PREFY)//LABELI//'S.grd.'//'G'//TRIM(c0)//TRIM(LV)
       ELSEIF (ifozone == 2) THEN ! clim interpol
          fNameOzone  = TRIM(path_in1)//'ozoneclm'//'.G'//TRIM(c0)//TRIM(LV)
       ELSEIF (ifozone == 3) THEN ! pred interpol
          fNameOzone  = TRIM(path_in1)//'ozonefct'//'.G'//TRIM(c0)//TRIM(LV)
       ELSEIF (ifozone == 4) THEN ! direct access
          fNameOzone  = TRIM(path_in1)//'ozonemtd'//'.G'//TRIM(c0)//TRIM(LV)
       ENDIF
    ENDIF

    ! SET UKMO SPECTRAL FILENAME

    fNameSpecSW = TRIM(path_in1)//'sp_sw_hadgem1_3r'
    fNameSpecLW = TRIM(path_in1)//'sp_lw_hadgem1_3'

    ! SET SST FILE NAME AND OPTIONS

    CALL SETSST (c0)

    IF (grhflg) THEN
       INQUIRE (FILE=TRIM(fNameGHLoc),exist=lexist)
       IF(.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameGHLoc)//" does not exist")
       END IF
       INQUIRE (FILE=TRIM(fNameGHTable),exist=lexist)
       IF(.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameGHTable)//" does not exist")
       END IF
    END IF 

    INQUIRE (FILE=TRIM(fNameDTable),exist=lexist)
    IF(.NOT. lexist) THEN
       CALL FatalError(h//" file "//TRIM(fNameDTable)//" does not exist")
    END IF

    INQUIRE (FILE=TRIM(fNameUnitTb),exist=lexist)
    IF(.NOT. lexist) THEN
       CALL FatalError(h//" file "//TRIM(fNameUnitTb)//" does not exist")
    END IF

    INQUIRE (FILE=TRIM(fNameLookTb),exist=lexist)
    IF(.NOT. lexist) THEN
       CALL FatalError(h//" file "//TRIM(fNameLookTb)//" does not exist")
    END IF

    INQUIRE (FILE=TRIM(fNameCnfTbl),exist=lexist)
    IF(.NOT. lexist) THEN
       CALL FatalError(h//" file "//TRIM(fNameCnfTbl)//" does not exist")
    END IF

    INQUIRE (FILE=TRIM(fNameCnf2Tb),exist=lexist)
    IF(.NOT. lexist) THEN
       CALL FatalError(h//" file "//TRIM(fNameCnf2Tb)//" does not exist")
    END IF

  
    IF (TRIM(isimp) == 'NO') THEN
       INQUIRE (FILE=TRIM(fNameSibAlb),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameSibAlb)//" does not exist")
       END IF

       INQUIRE (FILE=TRIM(fNameAlbedo),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameAlbedo)//" does not exist")
       END IF
      
       INQUIRE (FILE=TRIM(fNameSnow),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameSnow)//" does not exist")
       END IF
       
       INQUIRE (FILE=TRIM(fNameSoilms),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameSoilms)//" does not exist")
       END IF 
      
       INQUIRE (FILE=TRIM(fNameOrgvar),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameOrgvar)//" does not exist")
       END IF

       INQUIRE (FILE=TRIM(fNameSibmsk),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameSibmsk)//" does not exist")
       END IF

       INQUIRE (FILE=TRIM(fNameTg3zrl),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameTg3zrl)//" does not exist")
       END IF

       INQUIRE (FILE=TRIM(fNameRouLen),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameRouLen)//" does not exist")
       END IF

       INQUIRE (FILE=TRIM(fNameSibVeg),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameSibVeg)//" does not exist")
       END IF

       IF (iglsm_w .ne. 0) THEN
          INQUIRE (FILE=TRIM(fNameSoilType),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameSoilType)//" does not exist")
          END IF

          INQUIRE (FILE=TRIM(fNameVegType),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameVegType)//" does not exist")
          END IF

          INQUIRE (FILE=TRIM(fNameSoilMoist),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameSoilMoist)//" does not exist")
          END IF
       END IF
    END IF

    IF (ifco2>0) THEN
       INQUIRE (FILE=TRIM(fNameCO2),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameCO2)//" does not exist")
       END IF
    END IF

    IF (ifozone/=0) THEN
       INQUIRE (FILE=TRIM(fNameOzone),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameOzone)//" does not exist")
       END IF
    END IF

  END SUBROUTINE SetFileNameGaussPoints



  SUBROUTINE SetFileNameWavesPerLat(trunc, jMax)
    INTEGER, INTENT(IN) :: trunc
    INTEGER, INTENT(IN) :: jMax
    CHARACTER(LEN=5) :: c0
    CHARACTER(LEN=4) :: c1
    LOGICAL :: lexist
    CHARACTER(LEN=*), PARAMETER :: h="**(SetFileNameWavesPerLat)**"

    WRITE(c0,"(i5.5)") jMax
    WRITE(c1,"(i4.4)") trunc

    IF (mGiven) THEN
       fNameWaves  = TRIM(path_in1)//"/mwaves.T"//c1//"G"//c0
       INQUIRE (FILE=TRIM(fNameWaves),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameWaves)//" does not exist")
       END IF
    ELSE
       fNameWaves  = TRIM(path_in1)//"/mwaves.T"//c1//"G"//c0
    END IF

    fNameNmi    = TRIM(path_in1)//"NMI.T"//TRIM(c1)//TRIM(LV)
    INQUIRE (FILE=TRIM(fNameNmi),exist=lexist)
    IF (.NOT. lexist) THEN
       CALL MsgOne(h," file "//TRIM(fNameNmi)//" does not exist")
    END IF

  END SUBROUTINE SetFileNameWavesPerLat

  SUBROUTINE FileNameAnalis()
  IMPLICIT NONE
    CHARACTER(LEN=10)                 :: LABELI
    CHARACTER(LEN=10)                 :: LABELC
    CHARACTER(LEN=10)                 :: LABELF
    WRITE(LABELI,'(i4.4,3i2.2)')IDATEI(4),IDATEI(3),IDATEI(2),IDATEI(1)
    WRITE(LABELC,'(i4.4,3i2.2)')IDATEW(4),IDATEW(3),IDATEW(2),IDATEW(1)
    WRITE(LABELF,'(i4.4,3i2.2)')IDATEF(4),IDATEF(3),IDATEF(2),IDATEF(1) 
    IF ( TRIM(START) == 'cold' ) THEN
       EXTS='S.unf'
       EXTW='F.unf'
       EXDW='F.dir'
       EXTH='F.unf'
       EXDH='F.dir'
       LABELC=LABELF
       fNameInput0=TRIM(path_in1)//'GANL'//TRIM(PREFY)//LABELI//EXTS//'.'//TRIM(TRCG)//TRIM(LV)
       fNameInput1=TRIM(path_in1)//'GANL'//TRIM(PREFY)//LABELI//EXTS//'.'//TRIM(TRCG)//TRIM(LV)
    ELSEIF ( TRIM(START) == 'warm' ) THEN
       EXTS='S.unf'
       EXTW='F.unf'
       EXDW='F.dir'
       EXTH='F.unf'
       EXDH='F.dir'
       fNameInput0=TRIM(dirfNameOutput)//'/'//'GFCT'//TRIM(PREFX)//LABELI//LABELC//EXTW//'.'//TRIM(TRCG)//TRIM(LV)//'.outmdt'
       fNameInput1=TRIM(dirfNameOutput)//'/'//'GFCT'//TRIM(PREFX)//LABELI//LABELC//EXTW//'.'//TRIM(TRCG)//TRIM(LV)//'.outatt'
    END IF
  END SUBROUTINE FileNameAnalis


  SUBROUTINE FileNameRest(PRC,OPT)
   IMPLICIT NONE
    CHARACTER(LEN= * ), INTENT(IN   )  :: PRC
    INTEGER           , INTENT(IN   )  :: OPT
    CHARACTER(LEN= 10)                 :: LABELI
    CHARACTER(LEN= 10)                 :: LABELC
    CHARACTER(LEN= 10)                 :: LABELF
    WRITE(LABELI,'(i4.4,3i2.2)')idate(4),idate(2),idate(3),idate(1)
    WRITE(LABELC,'(i4.4,3i2.2)')idatec(4),idatec(2),idatec(3),idatec(1)
    WRITE(LABELF,'(i4.4,3i2.2)')idatec(4),idatec(2),idatec(3),idatec(1)
    IF (OPT==1) THEN
        FNameRestInput1=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labeli//labelc//&
            &EXTW//'.'//TRIM(TruncLev)//'.outmdt'//TRIM(PRC)

        FNameRestInput2=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labeli//labelc//&
            &EXTW//'.'//TRIM(TruncLev)//'.outatt'//TRIM(PRC)

        FNameConvClInp0=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labeli//labelc//&
            &EXTW//'.'//TRIM(TruncLev)//'.convcl'//TRIM(PRC)

        FNameSibPrgInp0=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labeli//labelc//&
            &EXTW//'.'//TRIM(TruncLev)//'.sibprg'//TRIM(PRC)
    ELSE
        FNameSibPrgOut1=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labeli//labelc//&
             &EXTW//'.'//TRIM(TruncLev)//'.sibprg'//TRIM(PRC)

        FNameConvClOut1=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labeli//labelc//&
             &EXTW//'.'//TRIM(TruncLev)//'.convcl'//TRIM(PRC)

        FNameRestOutput2=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labeli//labelc//&
             &EXTW//'.'//TRIM(TruncLev)//'.outatt'//TRIM(PRC)

        FNameRestOutput1=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labeli//labelc//&
             &EXTW//'.'//TRIM(TruncLev)//'.outmdt'//TRIM(PRC)
    END IF
  END SUBROUTINE FileNameRest


  SUBROUTINE FileNameGeral(PRC,OPT)
    IMPLICIT NONE
    CHARACTER(LEN= * ), INTENT(IN   )  :: PRC
    CHARACTER(LEN= * ), INTENT(IN   )  :: OPT
    CHARACTER(LEN= 10)                 :: LABELI
    CHARACTER(LEN= 10)                 :: LABELC
    CHARACTER(LEN= 10)                 :: LABELF
    WRITE(LABELI,'(i4.4,3i2.2)')IDATEI(4),IDATEI(3),IDATEI(2),IDATEI(1)
    WRITE(LABELC,'(i4.4,3i2.2)')IDATEW(4),IDATEW(3),IDATEW(2),IDATEW(1)
    WRITE(LABELF,'(i4.4,3i2.2)')IDATEF(4),IDATEF(3),IDATEF(2),IDATEF(1)
    IF(TRIM(OPT)=='GDHN')THEN
       FNameGDHN=TRIM(dirFnameOutput)//"/"//TRIM(OPT)//TRIM(PREFX)//labeli//labelc//&
                 &EXTW//'.'//TRIM(TruncLev)
    ELSE IF (TRIM(OPT)=='GDYN') THEN
       FNameGDYN=TRIM(dirFnameOutput)//"/"//TRIM(OPT)//TRIM(PREFX)//labeli//labelf//&
                 &EXTW//'.'//TRIM(TruncLev)
    ELSE IF (TRIM(OPT)=='GPRC') THEN
       FNameGPRC=TRIM(dirFnameOutput)//"/"//TRIM(OPT)//TRIM(PREFX)//labeli//labelc//&
          &EXTW//'.'//TRIM(TruncLev)    
    ELSE IF (TRIM(OPT)=='GFGH') THEN
       FNameOutGH=TRIM(dirFnameOutput)//"/"//TRIM(OPT)//TRIM(PREFX)//labeli//labelc//&
          &EXTW//'.'//TRIM(TruncLev)
       FNameTopGH=TRIM(dirFnameOutput)//"/"//TRIM(OPT)//TRIM(PREFX)//labeli//labelc//&
          &'F.top'//'.'//TRIM(TruncLev)
       FNamenDrGH=TRIM(dirFnameOutput)//"/"//TRIM(OPT)//TRIM(PREFX)//labeli//labelc//&
             &EXDH//'.'//TRIM(TruncLev)
    END IF 
  END SUBROUTINE FileNameGeral


  SUBROUTINE SetOutPut (tod1,idate)

    REAL(KIND=r8)   , INTENT(IN  )  :: tod1
    INTEGER, INTENT(IN  )  :: idate (4)

    REAL(KIND=r8)     :: tod
    INTEGER  :: l
    INTEGER  :: maxtfm2
    INTEGER  :: jdt
    INTEGER  :: ihour
    INTEGER  :: idays
    INTEGER  :: imont
    INTEGER  :: iyear
    REAL(KIND=r8)     :: cthr(maxtim)
    INTEGER, PARAMETER :: imonth=12
    INTEGER, PARAMETER :: nday1(imonth)=(/31,28,31,30,31,30,&
         31,31,30,31,30,31 /)
    INTEGER, PARAMETER :: nday2(imonth)=(/31,29,31,30,31,30,&
         31,31,30,31,30,31 /)
    tod=tod1
    IF(dhfct >= 0 ) THEN
       IF (dhfct > 0) THEN
          maxtfm2=maxtim
          cthr  =0.0_r8
          cthr(1)=dhfct
          DO l=2,maxtfm2
             cthr(l)=cthr(l-1)+dhfct
          END DO
       ELSE IF ( dhfct == 0 ) THEN
          READ(nNameList,*)maxtfm2
          IF (maxtfm2 > 0)   THEN
             READ(nNameList,*)cthr( 1:maxtfm2)
          ELSE
             maxtfm2      = 17 ! number of output forecast
             cthr        = 0.0_r8
             cthr( 1)= 6.0_r8;cthr( 2)=12.0_r8;cthr( 3)= 18.0_r8;cthr( 4)= 24.0_r8
             cthr( 5)=30.0_r8;cthr( 6)=36.0_r8;cthr( 7)= 42.0_r8;cthr( 8)= 48.0_r8
             cthr( 9)=54.0_r8;cthr(10)=60.0_r8;cthr(11)= 66.0_r8;cthr(12)= 72.0_r8
             cthr(13)=84.0_r8;cthr(14)=96.0_r8;cthr(15)=120.0_r8;cthr(16)=144.0_r8
             cthr(17)=168.0_r8
          END IF
       END IF
       CLOSE(nNameList)
       ihour=0
       DO jdt=1,maxtim
          tod=tod+delt
          IF(MOD(tod,3600.0_r8) == 0.0_r8) THEN
             ihour=ihour+1
             DO l=1,maxtfm2
                IF (abs(cthr(l)-ihour) .le. 0.00001_r8) THEN
                   cthl(jdt)=.true.
                   !PRINT*, cthl(jdt),cthr(l),jdt,tod,ihour
                END IF
             END DO
             tod  =0.0_r8
          END IF
       END DO

    ELSE
       ihour=idate(1)
       idays=idate(3)
       imont=idate(2)
       iyear=idate(4)

       DO jdt=1,maxtim
          tod=tod+delt

          IF(MOD(tod, 3600.0_r8) == 0.0_r8) THEN
             ihour=ihour+1
             IF (ihour == 24 )ihour  = 0
          END IF

          IF(MOD(tod,86400.0_r8) == 0.0_r8) THEN
             tod=0.0_r8
             idays=idays+1
          END IF

          IF( MOD(REAL(iyear,r8),4.0_r8) == 0.0_r8) THEN

             IF(idays > nday2(imont)) THEN
                idays=1
                imont=imont+1
                cthl(jdt)   =.true.
                IF(imont > 12) THEN
                   imont = 1
                   iyear = iyear + 1
                END IF
                PRINT*,cthl(jdt) ,iyear, imont, idays, ihour,tod
             END IF

          ELSE

             IF(idays > nday1(imont)) THEN
                idays=1
                imont=imont+1
                cthl(jdt)   =.true.
                IF(imont > 12) THEN
                   imont = 1
                   iyear = iyear + 1
                END IF
                PRINT*,cthl(jdt) ,iyear, imont, idays, ihour,tod
             END IF

          END IF
       END DO
    END IF
  END SUBROUTINE SetOutPut

END MODULE Options
