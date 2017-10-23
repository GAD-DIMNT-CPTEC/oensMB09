!
!  $Author: pkubota $
!  $Date: 2009/03/10 12:02:11 $
!  $Revision: 1.28 $
!
!  Modified by Tarassova 2015
!  Climate aerosol (Kinne, 2013) coarse mode included
!  Fine aerosol mode 2000 is included
!  Modifications (4) are marked by 
!  !tar begin and  !tar end
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
  LOGICAL           , PUBLIC           :: UNIFIED=.FALSE.
  CHARACTER(LEN=6)  , PUBLIC           :: NMSST
  CHARACTER(LEN=20) , PUBLIC           :: NMCO2FX='co2flxaoi'
  CHARACTER(LEN=20) , PUBLIC           :: NMSNOW='snowaoi'
  CHARACTER(LEN=20) , PUBLIC           :: NMSOILM='soilmaoi'
  CHARACTER(LEN=12) , PUBLIC           :: NMNDVI
  INTEGER           , PUBLIC           :: DHFCT
  INTEGER           , PUBLIC           :: DHRES
  INTEGER           , PUBLIC           :: DHDHN
  INTEGER           , PUBLIC           :: NHDHN
  INTEGER           , PUBLIC           :: DHEXT
  INTEGER           , PUBLIC           :: NHEXT
  LOGICAL           , PUBLIC           :: FIELDGAUS
  LOGICAL           , PUBLIC           :: FILELIST=.FALSE.
  LOGICAL           , PUBLIC           :: DOGRH
  LOGICAL           , PUBLIC           :: DOPRC
  LOGICAL           , PUBLIC           :: DOSMC
  CHARACTER(LEN=5)  , PUBLIC           :: PREFX
  CHARACTER(LEN=5)  , PUBLIC           :: PREFY
  CHARACTER(LEN=1)  , PUBLIC           :: TABLE
  CHARACTER(LEN=199), PUBLIC           :: path_in
  CHARACTER(LEN=200), PUBLIC           :: path_in1
  CHARACTER(LEN=200), PUBLIC           :: dirfNameOutput
  CHARACTER(LEN=200), PUBLIC           :: dirRstOutput

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
  CHARACTER(LEN=456), PUBLIC           :: fNameInput0
  CHARACTER(LEN=456), PUBLIC           :: fNameInput1
  CHARACTER(LEN=212), PUBLIC           :: fNameNmi
  CHARACTER(LEN=200), PUBLIC           :: fNameSSTAOI
  CHARACTER(LEN=200), PUBLIC           :: fNameCO2FLX
  CHARACTER(LEN=200), PUBLIC           :: fNameNDVIAOI
  CHARACTER(LEN=226), PUBLIC           :: fNameSnow
  CHARACTER(LEN=211), PUBLIC           :: fNameSoilms
  CHARACTER(LEN=211), PUBLIC           :: fNameSoilmsWkl
  CHARACTER(LEN=211), PUBLIC           :: fNameSoilMoistSib2

!tar begin
! names of input files of climate aerosol parameters coarse mode
!
!  CHARACTER(LEN=211), PUBLIC           :: fNameClimAodRRTM
!  CHARACTER(LEN=211), PUBLIC           :: fNameClimAsyRRTM
!  CHARACTER(LEN=211), PUBLIC           :: fNameClimSsaRRTM
!  CHARACTER(LEN=211), PUBLIC           :: fNameClimAodVrt
!tar end 
!
!tar begin
! names of input files of climate aerosol parameters fine mode 
!
!  CHARACTER(LEN=211), PUBLIC           :: fNameFineAodRRTM
!  CHARACTER(LEN=211), PUBLIC           :: fNameFineAsyRRTM
!  CHARACTER(LEN=211), PUBLIC           :: fNameFineSsaRRTM
!  CHARACTER(LEN=211), PUBLIC           :: fNameFineAodVrt
!tar end 

  CHARACTER(LEN=225), PUBLIC           :: fNameSoilType
  CHARACTER(LEN=229), PUBLIC           :: fNameVegType
  CHARACTER(LEN=225), PUBLIC           :: fNameSoilMoist
  CHARACTER(LEN=206), PUBLIC           :: fNameAlbedo
  CHARACTER(LEN=211), PUBLIC           :: fNameGauss
  CHARACTER(LEN=211), PUBLIC           :: fNameWaves
  CHARACTER(LEN=206), PUBLIC           :: fNameCO2   !hmjb
  CHARACTER(LEN=206), PUBLIC           :: fNameOzone !hmjb
  CHARACTER(LEN=206), PUBLIC           :: fNameSpecSW !hmjb
  CHARACTER(LEN=456), PUBLIC           :: fNametracer !hmjb
  CHARACTER(LEN=456), PUBLIC           :: fNameSpecLW !hmjb
  CHARACTER(LEN=456), PUBLIC           :: fNameCldOptSW 
  CHARACTER(LEN=456), PUBLIC           :: fNameCldOptLW 

  CHARACTER(LEN=456), PUBLIC           :: fNameMicro 
  CHARACTER(LEN=456), PUBLIC           :: fNameCnfTbl
  CHARACTER(LEN=456), PUBLIC           :: fNameCnf2Tb
  CHARACTER(LEN=456), PUBLIC           :: fNameLookTb
  CHARACTER(LEN=456), PUBLIC           :: fNameUnitTb
  CHARACTER(LEN=206), PUBLIC           :: fNameSibVeg
  CHARACTER(LEN=206), PUBLIC           :: fNameSibAlb
  CHARACTER(LEN=214), PUBLIC           :: fNameDTable
  CHARACTER(LEN=212), PUBLIC           :: fNameGHLoc
  CHARACTER(LEN=209), PUBLIC           :: fNameGHTable
  CHARACTER(LEN=209), PUBLIC           :: fNameTopo
  CHARACTER(LEN=211), PUBLIC           :: fNameOrgvar
  CHARACTER(LEN=211), PUBLIC           :: fNameHPRIME
  CHARACTER(LEN=456), PUBLIC           :: fNameGtopog
  CHARACTER(LEN=211), PUBLIC           :: fNameSibmsk
  CHARACTER(LEN=211), PUBLIC           :: fNameTg3zrl
  CHARACTER(LEN=255), PUBLIC           :: fNameRouLen
  CHARACTER(LEN=255), PUBLIC           :: fNameSoilTab
  CHARACTER(LEN=255), PUBLIC           :: fNameMorfTab
  CHARACTER(LEN=255), PUBLIC           :: fNameBioCTab
  CHARACTER(LEN=255), PUBLIC           :: fNameAeroTab 
  CHARACTER(LEN=255), PUBLIC           :: fNameSiB2Mask
  CHARACTER(LEN=255), PUBLIC           :: fNameIBISMask
  CHARACTER(LEN=255), PUBLIC           :: fNameIBISDeltaTemp
  CHARACTER(LEN=255), PUBLIC           :: fNameClimaTemp
  CHARACTER(LEN=255), PUBLIC           :: fNameSandMask
  CHARACTER(LEN=255), PUBLIC           :: fNameClayMask
  CHARACTER(LEN=255), PUBLIC           :: fNameTextMask
  CHARACTER(LEN=255), PUBLIC           :: fNameSlabOcen
  
  LOGICAL           , PUBLIC           :: slagr=.FALSE.
  LOGICAL           , PUBLIC           :: slhum=.FALSE.
  LOGICAL           , PUBLIC           :: microphys=.FALSE.
  INTEGER           , PUBLIC           :: nClass=0
  INTEGER           , PUBLIC           :: nAeros=0

  LOGICAL           , PUBLIC           :: SL_twotime_scheme=.FALSE. ! Two-time level SL scheme
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
  LOGICAL           , PUBLIC           :: GenAssFiles=.FALSE.   !.flag to generate  assimilation files
  LOGICAL           , PUBLIC           :: GenRestFiles=.FALSE.
  LOGICAL           , PUBLIC           :: rmRestFiles=.FALSE.
  LOGICAL           , PUBLIC           :: MasCon=.FALSE.
  LOGICAL           , PUBLIC           :: MasCon_ps=.FALSE.
  LOGICAL           , PUBLIC           :: FluxCon_co2=.FALSE.
  LOGICAL           , PUBLIC           :: givenfouriergroups=.FALSE.
  INTEGER           , PUBLIC           :: nproc_vert=1
  INTEGER           , PUBLIC           :: nscalars=0
  CHARACTER(LEN=3  ), PUBLIC           :: record_type="vfm"
  INTEGER           , PUBLIC           :: iglsm_w=0    !soil moisture options 
  INTEGER           , PUBLIC           :: tamBlock=512
  INTEGER           , PUBLIC           :: ibdim_size=192
  REAL(KIND=r8)     , PUBLIC           :: swint=1.000000_r8
  REAL(KIND=r8)     , PUBLIC           :: trint=3.000000_r8
  REAL(KIND=r8)     , PUBLIC           :: yrl=365.2500_r8
  INTEGER           , PUBLIC           :: iyear_AD=0 ! Year to calculate orbit for
  INTEGER           , PUBLIC           :: kt=0
  INTEGER           , PUBLIC           :: ktm=-1
  INTEGER           , PUBLIC           :: ktp=0
  INTEGER           , PUBLIC           :: jdt=0
  INTEGER           , PUBLIC           :: monl(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  INTEGER        ,ALLOCATABLE   , PUBLIC           :: indexchem(:)
  CHARACTER(LEN=10),ALLOCATABLE   , PUBLIC         :: typechem(:)
  CHARACTER(len=5)  , PUBLIC           :: iswrad="LCH"
  CHARACTER(len=5)  , PUBLIC           :: ilwrad="HRS"
  CHARACTER(len=3)  , PUBLIC           :: iccon ="KUO"
  CHARACTER(len=4)  , PUBLIC           :: ilcon ="LSC"
  CHARACTER(len=4)  , PUBLIC           :: iscon ="TIED" ! iscon=TIED:shallow convection this process follows cumulus convection (tiedke)
                                                        ! iscon=SOUZ:shallow convection this process follows cumulus convection (souza)
                                                        ! iscon=MFLX:shallow convection this process follows cumulus convection (souza)
                                                        ! iscon=JHK:shallow convection this process follows cumulus convection (Hack)
                                                        ! iscon=UW:shallow convection this process follows cumulus convection (U.Washington)
                                                        ! Not available still. iscon=SOUZ:cumulus heating and moistening tendencies
                                                        ! Enio Pereira de Souza 12/Jul/2001 
                                                        ! The option souza (SOUZ) for shallow convection  has problem in the 
                                                        ! mass conservation, generates wrong resulted for climate
  INTEGER           , PUBLIC           :: sfcpbl=1  ! 1 pbl Mellor Yamada 2.0
  INTEGER           , PUBLIC           :: atmpbl=3  ! 1 pbl Mellor Yamada 2.0
                                                    ! 2 pbl Mellor Yamada 2.5              
                                                    ! 3 pbl Hostlag Boville 1992 
  CHARACTER(LEN=4)  , PUBLIC           :: OCFLUX="COLA"
  CHARACTER(LEN=4)  , PUBLIC           :: SLABOCEAN="COLA"
  CHARACTER(LEN=4)  , PUBLIC           :: ICEMODEL="COLA"
  LOGICAL           , PUBLIC           :: PBLEntrain=.FALSE.
  CHARACTER(len=4)  , PUBLIC           :: idcon ="NO  "
  CHARACTER(len=4)  , PUBLIC           :: iqadj ="NO  "
  CHARACTER(len=4)  , PUBLIC           :: ipbl  ="YES "
  CHARACTER(len=4)  , PUBLIC           :: ievap ="YES "
  CHARACTER(len=4)  , PUBLIC           :: isens ="YES "
  CHARACTER(len=4)  , PUBLIC           :: idrag ="YES "
  CHARACTER(len=4)  , PUBLIC           :: iqdif ="YES " ! iqdif=yes:horizontal diffusion of moisture
  CHARACTER(len=4)  , PUBLIC           :: ifft  ="JMA "
  CHARACTER(len=4)  , PUBLIC           :: igwd  ="YES "
  CHARACTER(len=4)  , PUBLIC           :: isimp ="NO  "! isimp=yes:simplified physics version. 
  CHARACTER(len=4)  , PUBLIC           :: ickcfl="NO  "
  CHARACTER(len=4)  , PUBLIC           :: enhdif="YES "! enhdif=yes: enhance diffusion on higher levels )
  CHARACTER(LEN=4)  , PUBLIC           :: impdif="YES "
                                                         ! specific for clirad
  REAL(KIND=r8)     , PUBLIC           :: asolc=0.22_r8  ! continental: total column aerosol in the first 2km
  REAL(KIND=r8)     , PUBLIC           :: asolm=0.14_r8  ! maritime:	total column aerosol in the first 2km
  REAL(KIND=r8)     , PUBLIC           :: crdcld=1.0_r8  ! cloud scheme =1 (old) =4 (ccm3) =5 (cam5)  =6 (gfs)
                       ! specific for grell
  INTEGER           , PUBLIC           :: grepar1=1 ! integer: 0 ensemble 1 GRE   4 OMG   7 KUO  10 Chappel 13 ARA   24 ensemble2
  INTEGER           , PUBLIC           :: grepar2=3 ! integer: number eff-ensemble(1,2,3)
  REAL(KIND=r8)     , PUBLIC           :: grepar3=85.0_r8! cpmax
  REAL(KIND=r8)     , PUBLIC           :: grepar4=30.0_r8! cpmax-diff
  
  REAL(KIND=r8)     , PUBLIC           :: Wgh1=0.000_r8!1.0_r8/3.0_r8 ! pbl Hostlag Boville
  REAL(KIND=r8)     , PUBLIC           :: Wgh2=1.000_r8!1.0_r8/3.0_r8 ! pbl Mellor Yamada 2.0
  REAL(KIND=r8)     , PUBLIC           :: Wgh3=0.000_r8!1.0_r8/3.0_r8 ! pbl Mellor Yamada 2.5
  INTEGER           , PUBLIC           :: initlz=2
  INTEGER           , PUBLIC           :: nstep=1 ! number of steps in getting diabatic heating rate
                                                  ! in diaten if nstep=1,nstep is set equal to 7 in Model.  
  REAL(KIND=r8)     , PUBLIC           :: fint=6.0_r8
  INTEGER           , PUBLIC           :: intsst=-1  ! sst data set interval in days (if > 0)
                                                     ! sst data set is in calendar months if < 0.
  INTEGER           , PUBLIC           :: intsoilm=-1  ! soil moisture data set interval in days (if > 0)
                                                       ! sst data set is in calendar months if < 0.
  INTEGER           , PUBLIC           :: intndvi=-1
  REAL(KIND=r8)     , PUBLIC           :: sstlag=3.5_r8
  REAL(KIND=r8)     , PUBLIC           :: Soilmlag=3.5_r8
  INTEGER           , PUBLIC           :: ndord=4 ! order (even) of horizontal diffusion del operator
  INTEGER           , PUBLIC           :: nfiles=1
  INTEGER           , PUBLIC           :: ifin=0
  REAL(KIND=r8)     , PUBLIC           :: filta=0.92e0_r8! time filter constant 0.92 orig
!  REAL(KIND=r8)     , PUBLIC           :: filtb=(1.0_r8-filta)*0.5_r8
  REAL(KIND=r8)     , PUBLIC           :: filtb=0.04_r8
  REAL(KIND=r8)     , PUBLIC           :: percut=27502.0_r8! percut   cut off period in sec  in nlnmi
                                                           ! modes are to be read.
  REAL(KIND=r8)     , PUBLIC           :: varcut=1.6e5_r8 !! cut off height variance in m**2 for gravity wave drag
  REAL(KIND=r8)     , PUBLIC           :: vmax_est=1720.0_r8
  INTEGER           , PUBLIC           :: ifalb=0
  INTEGER           , PUBLIC           :: ifsst=-1
  INTEGER           , PUBLIC           :: ifndvi=-1
  INTEGER           , PUBLIC           :: ifslm=3
  INTEGER           , PUBLIC           :: ifslmSib2=3  
!!!!!!!!!!!!!tar begin
!!!!!!!!!!!  ifaeros=0  ....original aerosol: fixed continental and oceanic aerosol types
!!!!!!!!!!  ifaeros=2 .... Climate aerosol of Kinne, 2013  
!
!!!!!!!  INTEGER           , PUBLIC           :: ifaeros=2
!!!  INTEGER           , PUBLIC           :: aaaa=0
!!!!!!!!!!!!!!tar end    
  INTEGER           , PUBLIC           :: ifsnw=3
  REAL(KIND=r8)     , PUBLIC           :: co2val=345.0_r8
  INTEGER           , PUBLIC           :: ifco2=0
  INTEGER           , PUBLIC           :: ifco2flx=-1

  CHARACTER(LEN=4)  , PUBLIC           :: co2ipcc="    "
  INTEGER           , PUBLIC           :: ifozone=0
  INTEGER           , PUBLIC           :: iftracer=0
  REAL(KIND=r8)     , PUBLIC           :: ucrit=100.0_r8
  REAL(KIND=r8)     , PUBLIC           :: taucfl=86400.0_r8
  LOGICAL           , PUBLIC           :: ptime=.TRUE.
  LOGICAL           , PUBLIC           :: allghf=.FALSE.! it is possible to select all available grid history
                                                       ! fields:  
                                                       !
                                                       ! allghf=.TRUE.-  all available fields are required
  REAL(KIND=r8)     , PUBLIC           :: dfilta=0.92_r8
  REAL(KIND=r8)     , PUBLIC           :: vcrit=85.00_r8! critical velocity (m/s) at which damping kicks in
                                                        ! (for troposphere)
  REAL(KIND=r8)     , PUBLIC           :: alpha=2.50_r8
  REAL(KIND=r8)     , PUBLIC           :: ucstr=85.0_r8
  REAL(KIND=r8)     , PUBLIC           :: tcflst=21600.0_r8
  REAL(KIND=r8)     , PUBLIC           :: ucupp=70.0_r8
  REAL(KIND=r8)     , PUBLIC           :: tcflup=2160.0_r8
  REAL(KIND=r8)     , PUBLIC           :: slupp=0.020_r8
  INTEGER           , PUBLIC           :: ifddp=10
  LOGICAL           , PUBLIC           :: doprec=.FALSE.
  LOGICAL           , PUBLIC           :: dodyn=.FALSE.! logical flag to output
                                                       ! first level of divergence, vorticity,
                                                       ! virtual temperature, specific humidity
                                                       ! and log of surface pressure
                                                       ! at every time step
  LOGICAL           , PUBLIC           :: grhflg=.FALSE.
  REAL(KIND=r8)     , PUBLIC           :: sthick=0.65e0_r8 ! sthick; upper limit for originating air for lcl.
                                                           ! replaces kthick.
  REAL(KIND=r8)     , PUBLIC           :: sacum=0.46e0_r8 ! sacum; top level for integrated moisture 
                                                          ! convergence test. replaces
                                                          ! kacum
  REAL(KIND=r8)     , PUBLIC           :: acum0=-2.0e-8_r8! acum0; threshold moisture convergence such that 
                                                         ! integrated moisture
                                                         ! convergence > - acum0 for convection to occur.
  REAL(KIND=r8)     , PUBLIC           :: tbase=273.15e00_r8! tbase.....temperature of fusion of ice
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
  INTEGER           , PUBLIC           :: mlrg=0! mlrg=1 ;output of pre-adjusted & post adjusted 
                                                ! temp. & s.h. in lrgscl
  INTEGER           , PUBLIC           :: is=1! is  ;start i-point
  INTEGER           , PUBLIC           :: ki=1! ki  ; lowest level from which parcels can be 
                                               ! lifted to find lcl
  LOGICAL           , PUBLIC           :: mxrdcc=.TRUE.! use maximum random converage for radiative conv. clouds
  INTEGER           , PUBLIC           :: lcnvl=2      ! the lowest layer index where non-convective clouds can
                                                       ! occur (ben says this should be 2 or more)
  INTEGER           , PUBLIC           :: lthncl=80    ! minimum depth in mb of non-zero low level cloud
  REAL(KIND=r8)     , PUBLIC           :: rccmbl=3.0_r8 ! radiative convective cloud minimum base layer index
  INTEGER           , PUBLIC           :: icld=1        ! not used icld........>>> icld = 1    : old cloud emisivity (optical depth) setting
    !                   ccu :       0.05 *dp
    !                   css :       0.025*dp            for ice cloud t<253.0
    !                         0.05 *dp            for ice cloud t>253.0
    !            >>> icld = 2    : new cloud emisivity (optical depth) setting
    !                   ccu :       (0.16)*dp
    !                   css :        0.0                        t<-82.5c
    !                         (2.0e-6*(t-tcrit)**2)*dp    -82.5<t<-10.0c
    !                         (6.949e-3*(t-273)+.08)*dp   -10.0<t< 0.0c
    !                         (0.08)*dp                 -10.0<t< 0.0c
    !            >>> icld = 3    : ccm3 based cloud emisivity
  REAL(KIND=r8)    , PUBLIC            :: cflric=0.10_r8! parameter used by relaxed arakawa-schubert

  INTEGER           , PUBLIC           :: inalb=2! not used
  INTEGER           , PUBLIC           :: mxiter=200 ! soil moiture initialization . parameter of interaction
  REAL(KIND=r8)     , PUBLIC           :: dk=8.0E+15_r8 
  REAL(KIND=r8)     , PUBLIC           :: tk=6.0E+15_r8 
  LOGICAL           , PUBLIC           :: omlmodel=.FALSE.
  REAL(KIND=r8)     , PUBLIC           :: oml_hml0=60.0_r8
  REAL(KIND=r8)     , PUBLIC           :: rhdifd=1.43130_r8
  REAL(KIND=r8)     , PUBLIC           :: rhdift=1.90840_r8
  !REAL(KIND=r8)     , PUBLIC           :: dk=2.5e15
  !REAL(KIND=r8)     , PUBLIC           :: tk=2.0e15
  !REAL(KIND=r8)     , PUBLIC           :: dk=8.0E+15_r8*0.1_r8 
  !REAL(KIND=r8)     , PUBLIC           :: tk=6.0E+15_r8*0.1_r8 

  LOGICAL           , PUBLIC           :: iftsi=.FALSE. 
  INTEGER           , PUBLIC           :: sflyr=1
  INTEGER           , PUBLIC           :: cpldlev=1

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
  !|--------------------------------|----------------|----------------------|
  !|FLUXCO2Clima<labels>.G<jMax>    |  nfco2fx=58    |  InputOutput (getsbc:|
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
  !|GFCT<PREFX><labeli><labelf>F.   |  nftmp=87      |  Temporary Unit      |
  !|dir.<trunc><lev>.files          |                |  Diagnostics         |
  !|(F.dir=<EXTW(1:2)> + "dir")     |                | (opnfct:open;write)  |
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
  INTEGER , PUBLIC                     :: nftmp=87     ! temporary unit
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
  INTEGER , PUBLIC                     :: nfco2fx=58    ! co2 flux file
  INTEGER , PUBLIC                     :: nfndvi=47     ! ndvi   file 
  INTEGER , PUBLIC                     :: nfsib2mk=10   ! sib2 land mask file
  INTEGER , PUBLIC                     :: nfsnw=51      ! snow   file
  INTEGER , PUBLIC                     :: nfalb=52      ! albedo file
  INTEGER , PUBLIC                     :: nfsand=29     ! sand soil file   
  INTEGER , PUBLIC                     :: nfclay=9     ! clay soil file    
  INTEGER , PUBLIC                     :: nfSoilMostSib2=67     ! soil moisture file for sib2   
  INTEGER , PUBLIC                     :: nftext=41     ! text soil file        
  INTEGER , PUBLIC                     :: nfsoiltb=14     !   soil properties file
  INTEGER , PUBLIC                     :: nfbioctb=15 !   table aerodynamic variables file
  INTEGER , PUBLIC                     :: nfaerotb=11 !   biome properties file
  INTEGER , PUBLIC                     :: nfmorftb=7     !  morphological each vegtype soil properties file 
  INTEGER , PUBLIC                     :: nfslm=53      ! soil moisture file
  INTEGER , PUBLIC                     :: nfco2=54      ! co2 file
  INTEGER , PUBLIC                     :: nftrc=41      ! tracer file
  INTEGER , PUBLIC                     :: nfzol=71      ! Roughness Length file
  INTEGER , PUBLIC                     :: nfozone=55    ! ozone file
!tar begin
!  INTEGER , PUBLIC                     :: nfaod=72         ! clim aod 
!  INTEGER , PUBLIC                     :: nfasy=73         ! clim asy 
!  INTEGER , PUBLIC                     :: nfssa=46         ! clim ssa 
!  INTEGER , PUBLIC                     :: nfaodvrt=76      ! clim aodvrt 
!tar end 
!
!tar begin
!  INTEGER , PUBLIC                     :: nfaodF=34         ! Fine mode aodF 
!  INTEGER , PUBLIC                     :: nfasyF=35         ! Fine mode asyF 
!  INTEGER , PUBLIC                     :: nfssaF=75         ! Fine mode ssaF 
!  INTEGER , PUBLIC                     :: nfaodvrtF=95      ! Fine mode aodvrtF 
!tar end 
! 
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


  INTEGER , PUBLIC                     :: istrt
  LOGICAL , PUBLIC                     :: first
  REAL(KIND=r8)    , PUBLIC            :: dtc3x
  REAL(KIND=r8)    , PUBLIC            :: epsflt
  INTEGER , PUBLIC                     :: intg
  INTEGER , PUBLIC                     :: dogwd=0
  REAL (KIND=r8), PUBLIC :: nForecast(160) ! 

  CHARACTER(LEN=10 ), PUBLIC           :: labelsi_soilm
  CHARACTER(LEN=10 ), PUBLIC           :: labelsj_soilm

  CHARACTER(LEN=10 ), PUBLIC           :: labelsi
  CHARACTER(LEN=10 ), PUBLIC           :: labelsj
  LOGICAL , PUBLIC, ALLOCATABLE        :: cdhl  (:)
  LOGICAL , PUBLIC, ALLOCATABLE        :: cthl  (:)
  INTEGER, PUBLIC :: schemes=1 ! schemes=1 SSiB_Driver
  INTEGER, PUBLIC :: isimveg =0         ! 0 = static veg, 
                                                   ! 1 = dynamic veg, 
                                                   ! 2 = dynamic veg with cold start
  INTEGER, PUBLIC :: isimco2=0
  PUBLIC :: ReadNameList
  PUBLIC :: SetTimeOutput
  PUBLIC :: SetOutPut
  PUBLIC :: SetOutPutDHN
  PUBLIC :: DumpOptions
  PUBLIC :: SetFileNameGaussPoints
  PUBLIC :: SetFileNameWavesPerLat
  PUBLIC :: CalDate
  PUBLIC :: CreateFileName
  INTERFACE CreateFileName
     MODULE PROCEDURE FileNameAnalis, FileNameRest,FileNameRest2,FileNameGeral
  END INTERFACE

CONTAINS


  !*** Read Namelist File and complete options ***


  SUBROUTINE ReadNameList()
    INTEGER :: ierr,i,m
    CHARACTER(LEN=8) :: c0
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadNameList)**"

    NAMELIST /MODEL_RES/trunc,vert,dt,idatei,idatew,idatef,nmsst,NMCO2FX,NMSNOW,NMSOILM,&
         dhfct,dhres,dhdhn,nhdhn,dhext,nhext,dogrh,FIELDGAUS,FILELIST,&
         doprc,prefx,prefy,table,path_in,dirfNameOutput, dirRstOutput

    NAMELIST /MODEL_IN/slagr,slhum,microphys,SL_twotime_scheme,nlnminit,diabatic, &
         eigeninit,rsettov,intcosz,Model1D,mgiven,gaussgiven,reducedGrid, &
         linearGrid,GenAssFiles,GenRestFiles,rmRestFiles,MasCon,MasCon_ps,FluxCon_co2,nscalars, &
         record_type,iglsm_w,tamBlock,ibdim_size,givenfouriergroups, &
         nproc_vert

    NAMELIST /PHYSPROC/UNIFIED,iswrad,ilwrad,iccon,ilcon,iqdif, &
         iscon,igwd ,isimp,enhdif,asolc,asolm,crdcld, &
         grepar1,grepar2,grepar3,grepar4,iglsm_w,sfcpbl,atmpbl,&
         PBLEntrain,schemes,isimveg,isimco2,OCFLUX,SLABOCEAN,ICEMODEL,omlmodel,oml_hml0,&
         Wgh1,Wgh2,Wgh3,iyear_AD,nAeros

    NAMELIST /PHYSCS/mxrdcc,lcnvl ,lthncl ,rccmbl ,swint  , &
         trint ,icld  ,inalb  ,mxiter ,co2val , &
         sthick,sacum ,acum0  ,tbase  ,mlrg   , &
         is    ,ki, cflric

    NAMELIST /COMCON/initlz , nstep  , fint   , intsst ,intndvi,intsoilm, ndord  ,&
         filta  , percut , varcut , ifsst  ,ifndvi, &
         ifsnw  , ifalb  , ifslm  ,ifslmSib2, allghf , &
         ifco2  ,ifco2flx, ifozone  ,iftracer,&
         vcrit  , alpha  , dodyn  , dk, tk,&
         iftsi,sflyr,cpldlev

!    IF(microphys)ilcon='MIC'

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
    IF(FILELIST)THEN
       IF(Table /= 'p')FILELIST=.FALSE.
    END IF

    ! SET THE NUMBER OF TRACERS USED IN MICROPHYSICS.

    IF(TRIM(ILCON) == 'NON' .OR. TRIM(ILCON) == 'YES' .OR. TRIM(ILCON) == 'LSC')THEN
       microphys=.FALSE.
       nClass=0
       nAeros=0
    ELSE
       microphys=.TRUE.
       nAeros=0
    END IF
    IF (microphys) THEN
        IF(TRIM(ILCON) == 'NON' .OR. TRIM(ILCON) == 'YES' .OR. TRIM(ILCON) == 'LSC')nClass=0
        IF(TRIM(ILCON) == 'MIC')nClass=0
        IF(TRIM(ILCON) =='HWRF')nClass=8
        IF(TRIM(ILCON) =='HGFS')nClass=1
        IF(TRIM(ILCON) =='UKMO')nClass=1
        IF(TRIM(ILCON) =='MORR')nClass=7
        IF(TRIM(ILCON) =='HUMO')nClass=8
        IF(isimco2 > 0)nAeros=nAeros+1
        IF(ifozone > 0)nAeros=nAeros+1
        ALLOCATE(indexchem(nClass+nAeros));indexchem=-1
        ALLOCATE(typechem (500));typechem=''
    END IF
    m=0
    DO i=1,nClass
       m=m+1
       indexchem(m)=m
       typechem(m)='HIDROMETEO'
    END DO
    IF(isimco2 > 0)THEN
       m=m+1
       typechem(m)='CO2'
       indexchem(m)=m
    END IF 
    IF(ifozone > 0)THEN
       m=m+1
       typechem(m)='O3'
       indexchem(m)=m
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
    !IF(nscalars == 0)iftracer=0
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
    ddelt=INT(dt)
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
    IF(dhres<0)THEN
       reststep=dhres
    ELSE
       reststep=NINT((dhres*3600)/dt)
    END IF
    CALL CheckOptions()

    IF(TRIM(NMSOILM) == 'soilmdyd')THEN
       ifslm=5
    ELSE IF(TRIM(NMSOILM) =='soilmwkl')THEN
       ifslm=1
    ELSE IF(TRIM(NMSOILM) == 'soilmaoi') THEN
       ifslm=3
    ELSE
       ifslm=3
    END IF

    record_type='vfm'
    IF(iglsm_w == 1) THEN
       ifslm=0
       ifslmSib2=0
    END IF 
    CALL ColdWarm()

    dtc3x  = 0.0_r8
    maxtid=INT(REAL(51*366*86400)/dt)
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
         TRIM(iswrad) /= 'CRDTF'.AND. &
         TRIM(iswrad) /= 'RRTMG'.AND. &
         TRIM(iswrad) /= 'UKM'       ) THEN
       CALL FatalError(h//" Unknown option iswrad="//TRIM(iswrad)//&
            "; Known options are: NON, LCH, CRD,CRDTF,RRTMG, UKM")
    END IF

    !------------- Ocean Flux

    IF  (TRIM(OCFLUX) /= 'COLA'.AND. &
         TRIM(OCFLUX) /= 'UKME'.AND. &
         TRIM(OCFLUX) /= 'WGFS') THEN
       CALL FatalError(h//" Unknown option OCFLUX="//TRIM(OCFLUX)//&
            "; Known options are: COLA, UKME, WGFS")
       WRITE(c0,"(f8.2)") oml_hml0

       CALL FatalError(h//" Unknown option oml_hml0="//TRIM(c0)//&
            "; Known options are: oml_hml0, WGFS")

    END IF

    !------------- Ocean Albedo

    IF  (TRIM(SLABOCEAN) /= 'COLA'.AND. &
         TRIM(SLABOCEAN) /= 'SLAB') THEN
       CALL FatalError(h//" Unknown option SLABOCEAN="//TRIM(SLABOCEAN)//&
            "; Known options are: COLA, SLAB")

    END IF

    !------------- IceOcean MODEL

    IF  (TRIM(ICEMODEL) /= 'COLA'.AND. &
         TRIM(ICEMODEL) /= 'SSIB') THEN
       CALL FatalError(h//" Unknown option ICEMODEL="//TRIM(ICEMODEL)//&
            "; Known options are: COLA, SSIB")

    END IF

    !  Check options for Clirad and UKMet Short Wave Radiation

    IF (TRIM(iswrad) == 'CRD') THEN
       IF (.NOT.(crdcld == 1_i8 .OR. crdcld == 4_i8 .OR. crdcld == 5_i8 .OR. crdcld == 6_i8 .OR. crdcld == 7_i8) ) THEN
          WRITE(c0,"(f8.2)") crdcld
          CALL FatalError(h//" Wrong cloud scheme option crdcld="//&
               TRIM(ADJUSTL(c0))//"; Valid options are: "//&
               "1 (stable) or 4 (experimental)")
       ELSE IF (crdcld == 4_i8 .OR. crdcld == 5_i8.OR. crdcld == 6_i8 .OR. crdcld == 7_i8) then
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
    ELSE IF (TRIM(iswrad) == 'CRDTF') THEN
       IF (.NOT.(crdcld == 1_i8 .OR. crdcld == 4_i8 .OR. crdcld == 5_i8 .OR. crdcld == 6_i8 .OR. crdcld == 7_i8) ) THEN
          WRITE(c0,"(f8.2)") crdcld
          CALL FatalError(h//" Wrong cloud scheme option crdcld="//&
               TRIM(ADJUSTL(c0))//"; Valid options are: "//&
               "1 (stable) or 4 (experimental)")
       ELSE IF (crdcld == 4_i8 .OR. crdcld == 5_i8.OR. crdcld == 6_i8 .OR. crdcld == 7_i8) then
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

    ELSE IF (TRIM(iswrad) == 'RRTMG') THEN
       IF (.NOT.(crdcld == 6_i8 .OR. crdcld == 7_i8 ) ) THEN
          WRITE(c0,"(f8.2)") crdcld
          CALL FatalError(h//" Wrong cloud scheme option crdcld="//&
               TRIM(ADJUSTL(c0))//"; Valid options are: "//&
               "1 (stable) or 4 (experimental)")
       ELSE IF ( crdcld == 6_i8 .OR. crdcld == 7_i8) then
          CALL MsgOne(h, " WARN: NCEP clouds + RRTMG used for research only !!!")
       END IF

    ELSE IF (TRIM(iswrad) == 'UKM') THEN

    END IF

    !------------- Long Wave Radiation

    IF  (TRIM(ilwrad) /= 'NON' .AND. &
         TRIM(ilwrad) /= 'HRS' .AND. &
         TRIM(ilwrad) /= 'CRD' .AND. &
         TRIM(ilwrad) /= 'CRDTF' .AND. &
         TRIM(ilwrad) /= 'RRTMG' .AND. &
         TRIM(ilwrad) /= 'UKM'       ) THEN
       CALL FatalError(h//" Unknown option ilwrad= "//TRIM(ilwrad)//&
            "; Known options are: NON, HRS, CRD,RRTMG, UKM")
    END IF

    ! Check options for Clirad and UKMET Long Wave Radiation

    IF (TRIM(ilwrad) == 'CRDGG') THEN
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

    IF (iftracer < 0 .OR. iftracer > 4) THEN
       WRITE(c0,"(i8)") iftracer
       CALL FatalError(h//" Invalid option iftracer="//TRIM(ADJUSTL(c0))//&
            "; valid values are: -0, 1, 2, 3 or 4")
    END IF

    !------------- Gyavity wage drag

    IF  (TRIM(IGWD) /= 'NON' .AND. &
         TRIM(IGWD) /= 'YES' .AND. &
         TRIM(IGWD) /= 'GMB' .AND. & 
         TRIM(IGWD) /= 'CAM' .AND. &
         TRIM(IGWD) /= 'USS'         ) THEN
       CALL FatalError(h//" Unknown option IGWD= "//TRIM(IGWD)//&
            "; Known options are: YES, CAM, USS or NON")
    END IF

    !------------- Large Scale Convection

    IF  (TRIM(ILCON) /= 'NON' .AND. &
         TRIM(ILCON) /= 'YES' .AND. &
         TRIM(ILCON) /= 'LSC' .AND. &
         TRIM(ILCON) /='HWRF' .AND. &
         TRIM(ILCON) /='HGFS' .AND. &
         TRIM(ILCON) /='UKMO' .AND. & 
         TRIM(ILCON) /='MORR' .AND. & 
         TRIM(ILCON) /='HUMO' .AND. & 
         TRIM(ILCON) /= 'MIC'         ) THEN
       CALL FatalError(h//" Unknown option ILCON= "//TRIM(ILCON)//&
            "; Known options are: NON, YES, LSC,HWRF, HGFS,UKMO ,MORR,HUMO or MIC")
    END IF

    !------------- Convection

    IF  (TRIM(iccon) /= 'NON' .AND. &
         TRIM(iccon) /= 'ARA' .AND. &
         TRIM(iccon) /= 'RAS' .AND. &
         TRIM(iccon) /= 'ZMC' .AND. &
         TRIM(iccon) /= 'KUO' .AND. &
         TRIM(iccon) /= 'GEC' .AND. &
         TRIM(iccon) /= 'GDN' .AND. &
         TRIM(iccon) /= 'GRE'         ) THEN
       CALL FatalError(h//" Unknown option iccon= "//TRIM(iccon)//&
            "; Known options are: ARA, RAS, KUO, ZMC, GEC, GDN or GRE")
    END IF

    ! Check options for grell

    IF (TRIM(iccon) == 'GRE') THEN
       IF  (grepar1 /= 0  .AND. &
            grepar1 /= 1  .AND. &
            grepar1 /= 4  .AND. &
            grepar1 /= 7  .AND. &
            grepar1 /= 10 .AND. &
            grepar1 /= 17 .AND. &
            grepar1 /= 18 .AND. &
            grepar1 /= 19 .AND. &
            grepar1 /= 20 .AND. &
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

    ! Check options for grell cptec

    IF (TRIM(iccon) == 'GEC') THEN
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
         TRIM(iscon) == 'MFLX'.OR. &
         TRIM(iscon) == 'JHK'.OR. &
         TRIM(iscon) == 'UW'.OR. &
         TRIM(iscon) == 'SOUZ'       )) THEN
       CALL FatalError(h//" Unknown option iscon="//TRIM(iscon)//&
            "; known options are: TIED, MFLX or SOUZ")
    END IF

    ! Check options for Microphysics

    IF (microphys .and..not.slhum) THEN
       CALL FatalError(h//" Microphysics requires slhum to be true ")
    END IF
    IF (microphys .and. SL_twotime_scheme) THEN
       CALL FatalError(h//" Microphysics not yet implemented for 2 level scheme" )
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
       IF (SLhum) THEN
          line = TRIM(line)//", SL transport of humidity remaining in grid-point space "
         ELSE
          line = TRIM(line)//", humidity spectral, SL transport "
       ENDIF
    ELSE
       line = "Eulerian"
       IF (SLhum) THEN
          line = TRIM(line)//", SL transport of humidity remaining in grid-point space "
         ELSE
          line = TRIM(line)//", humidity spectral, Eulerian transport "
       ENDIF
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
    CALL MsgOne(h, " input SST file name is "//TRIM(fNameSSTAOI))
    CALL MsgOne(h, " input SST file name is "//TRIM(fNameCO2FLX))
    CALL MsgOne(h, " input SNOW file name is "//TRIM(fNameSnow))

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
    ELSEIF (TRIM(iswrad).eq.'CRDTF') THEN
       CALL MsgOne(h, tab//"Shortwave Radiation is CLIRADTARASOVA-SW")
       WRITE(c0,"(e16.7)") asolm
       CALL MsgOne(h, tab//"Maritime aerosol is "//TRIM(ADJUSTL(c0)))
       WRITE(c0,"(e16.7)") asolc
       CALL MsgOne(h, tab//"Continental aerosol is "//TRIM(ADJUSTL(c0)))
       WRITE(c0,"(e16.7)") crdcld
       CALL MsgOne(h, tab//"Cloud Scheme is "//TRIM(ADJUSTL(c0)))
    ELSEIF (TRIM(iswrad).eq.'RRTMG') THEN
       CALL MsgOne(h, tab//"Shortwave Radiation is RRTMG")
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
    ELSEIF (TRIM(ilwrad).eq.'CRDTF') THEN
       CALL MsgOne(h, tab//"Longwave Radiation is Clirad-LW")
    ELSEIF (TRIM(ilwrad).eq.'RRTMG') THEN
       CALL MsgOne(h, tab//"Longwave Radiation is RRTMG")
    ELSEIF (TRIM(ilwrad).eq.'UKM') THEN
       CALL MsgOne(h, tab//"Longwave Radiation is UKMO-LW")
       CALL MsgOne(h, tab//tab//"spectral-file: "//TRIM(fNameSpecLW))
    ENDIF

    ! CLOUD OPTICS

!    fNameCldOptSW
!    fNameCldOptLW

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

    ! TRACER

    IF (iftracer.EQ.0) THEN
       CALL MsgOne(h, tab//"Not Use TRACER")
    ELSE
       CALL MsgOne(h, tab//"Reading global TRACER field from external file")
       IF (iftracer.EQ.1) THEN
          CALL MsgOne(h, tab//"TRACER field is constant")
       ELSEIF (iftracer.EQ.2) THEN
          CALL MsgOne(h, tab//"TRACER field is climatology")
       ELSEIF (iftracer.EQ.3) THEN
          CALL MsgOne(h, tab//"TRACER field is predicted")
       ELSEIF (iftracer.EQ.4) THEN
          CALL MsgOne(h, tab//"TRACER field is continuous")
       ENDIF
    ENDIF

    ! Deep Convection

    IF (TRIM(iccon).eq.'NON') THEN
       CALL MsgOne(h, tab//"No Deep Convection selected")
    ELSEIF (TRIM(iccon).eq.'ARA') THEN
       CALL MsgOne(h, tab//"Deep Convection is Arakawa")
    ELSEIF (TRIM(iccon).eq.'RAS') THEN
       CALL MsgOne(h, tab//"Deep Convection is Arakawa")
    ELSEIF (TRIM(iccon).eq.'KUO') THEN
       CALL MsgOne(h, tab//"Deep Convection is KUO")
    ELSEIF (TRIM(iccon).eq.'ZMC') THEN
       CALL MsgOne(h, tab//"Deep Convection is Zhang-McFarlane")
    ELSEIF (TRIM(iccon).eq.'GDN') THEN
       CALL MsgOne(h, tab//"Deep Convection is Grell-Divani-Nilo")
    ELSEIF (TRIM(iccon).eq.'GEC') THEN
       CALL MsgOne(h, tab//"Deep Convection is GRELL CPTEC with parameters:")
       WRITE(c0,"(i16)") grepar1
       WRITE(c1,"(i16)") grepar2
       CALL MsgOne(h, tab//tab//"grepar1="//TRIM(ADJUSTL(c0))//&
            &"; grepar2="//TRIM(ADJUSTL(c1)))
       WRITE(c0,"(e16.7)") grepar3
       WRITE(c1,"(e16.7)") grepar4
       CALL MsgOne(h, tab//tab//"grepar3="//TRIM(ADJUSTL(c0))//&
            &"; grepar4="//TRIM(ADJUSTL(c1)))
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
    ELSEIF (TRIM(iscon).eq.'JHK') THEN
       CALL MsgOne(h, tab//"Shallow Convection is J. Hack")
    ELSEIF (TRIM(iscon).eq.'SOUZ') THEN
       CALL MsgOne(h, tab//"Shallow Convection is Souza")
    ELSEIF (TRIM(iscon).eq.'MFLX') THEN
       CALL MsgOne(h, tab//"Shallow Convection is Mass Flux")

    ENDIF

    ! MicroPhysics
 
    IF (TRIM(ILCON).eq.'NON') THEN
       CALL MsgOne(h, tab//"No MicroPhysics selected")
    ELSEIF (TRIM(ILCON).eq.'YES') THEN
       CALL MsgOne(h, tab//"Large Scale Precipitation")
    ELSEIF (TRIM(ILCON).eq.'LSC') THEN
       CALL MsgOne(h, tab//"Large Scale Precipitation")
    ELSEIF (TRIM(ILCON).eq.'MIC') THEN
       CALL MsgOne(h, tab//"MicroPhysics NCAR")
    ELSEIF (TRIM(ILCON).eq.'HGFS') THEN
       CALL MsgOne(h, tab//"MicroPhysics HGFS")
    ELSEIF (TRIM(ILCON).eq.'UKMO') THEN
       CALL MsgOne(h, tab//"MicroPhysics UKMO")
    ELSEIF (TRIM(ILCON).eq.'MORR') THEN
       CALL MsgOne(h, tab//"MicroPhysics MORR")
    ELSEIF (TRIM(ILCON).eq.'HUMO') THEN
       CALL MsgOne(h, tab//"MicroPhysics HUMO")
    ELSEIF (TRIM(ILCON).EQ.'HWRF') THEN
       CALL MsgOne(h, tab//"MicroPhysics THOMPSON")
       CALL MsgOne(h, tab//tab//"MicroPhysics-file: "//TRIM(fNameMicro))
    ENDIF

    ! PBL

    
    IF (atmpbl.EQ.1) THEN
       CALL MsgOne(h, tab//"atmpbl=1 pbl Mellor Yamada 2.0")
    ELSEIF (atmpbl.EQ.2) THEN
       CALL MsgOne(h, tab//"atmpbl=2 pbl Mellor Yamada 2.5")
    ELSEIF (atmpbl.EQ.3) THEN
       CALL MsgOne(h, tab//"atmpbl=3 pbl Hostlag Boville 1992")
    ELSEIF (atmpbl.EQ.4) THEN
       CALL MsgOne(h, tab//"atmpbl=4 pbl SUNGSU PARK")
    ENDIF

    ! Surface Scheme

    IF (schemes.EQ.1) THEN
       CALL MsgOne(h, tab//"schemes=1 ssib Surface Scheme")
    ELSEIF (schemes.EQ.2) THEN
       CALL MsgOne(h, tab//"schemes=2 sib2 Surface Scheme")
    ELSEIF (schemes.EQ.3) THEN
       CALL MsgOne(h, tab//"schemes=3 ibis Surface Scheme")
    ENDIF

    ! Surface Ocean Scheme

    IF (TRIM(OCFLUX).eq.'COLA') THEN
       CALL MsgOne(h, tab//"OCFLUX=COLA Surface Ocean Scheme")
    ELSEIF (TRIM(OCFLUX).eq.'UKME') THEN
       CALL MsgOne(h, tab//"OCFLUX=UKME Surface Ocean Scheme")
    ELSEIF (TRIM(OCFLUX).eq.'WGFS') THEN
       CALL MsgOne(h, tab//"OCFLUX=WGFS Surface Ocean Scheme")
    ENDIF

    ! Albedo Surface Ocean Scheme

    IF (TRIM(SLABOCEAN).eq.'COLA') THEN
       CALL MsgOne(h, tab//"SLABOCEAN=COLA Albedo Surface Ocean Scheme")
    ELSEIF (TRIM(SLABOCEAN).eq.'SLAB') THEN
       CALL MsgOne(h, tab//"SLABOCEAN=SLAB Albedo Surface Ocean Scheme")
    ENDIF

    ! Ice Ocean Model Scheme

    IF (TRIM(ICEMODEL).eq.'COLA') THEN
       CALL MsgOne(h, tab//"ICEMODEL=COLA Flux Surface IceOcean Scheme")
    ELSEIF (TRIM(ICEMODEL).eq.'SSIB') THEN
       CALL MsgOne(h, tab//"ICEMODEL=SSIB Flux Surface IceOcean Scheme")
    ENDIF

    ! Gravity Wave Scheme

    IF (TRIM(IGWD).eq.'NON') THEN
       CALL MsgOne(h, tab//"IGWD=NON  No Gravity Wave Scheme selected")
    ELSEIF (TRIM(IGWD).eq.'YES') THEN
       CALL MsgOne(h, tab//"IGWD=YES Alpert Gravity Wave Scheme")
    ELSEIF (TRIM(IGWD).eq.'GMB') THEN
       CALL MsgOne(h, tab//"IGWD=GMB ECMWF Gravity Wave Scheme")
    ELSEIF (TRIM(IGWD).eq.'CAM') THEN
       CALL MsgOne(h, tab//"IGWD=CAM NCAR Gravity Wave Scheme")
    ELSEIF (TRIM(IGWD).eq.'USS') THEN
       CALL MsgOne(h, tab//"IGWD=USS UKMET Gravity Wave Scheme")
    ENDIF

    ! Restart Options
                   ! initlz  =2 diabatic normal mode initialization
                   !         =1 diabatic with no normal mode initialization
                   !         =0 adiabatic with no normal mode initialization
                   !         <0 same as >0 with sib variables read in instead of 
                   !            initialized

    IF (initlz == 2) THEN
       CALL MsgOne(h, tab//"initlz=2 diabatic normal mode initialization (Cold Start)")
    ELSEIF (initlz == 1) THEN
       CALL MsgOne(h, tab//"initlz=1 diabatic with no normal mode initialization (Cold Start)")
    ELSEIF (initlz == 0) THEN
       CALL MsgOne(h, tab//"initlz=0 adiabatic with no normal mode initialization (Warm Start)")
    ELSEIF (initlz == -1) THEN
       CALL MsgOne(h, tab//"initlz=-1 diabatic with no normal mode initialization (Cold Start)")
       CALL MsgOne(h, tab//"initlz=-1  with Surface variables read in instead of initialized")
    ELSEIF (initlz == -2) THEN
       CALL MsgOne(h, tab//"initlz=-2 diabatic normal mode initialization")
       CALL MsgOne(h, tab//"initlz=-2 (Atmos.,Conv.,Rad,Cloud -> Cold Start) (Surface -> Warm Start)")
       CALL MsgOne(h, tab//"initlz=-2  with Surface variables read in instead of initialized")
    ELSEIF (initlz == -3) THEN
       CALL MsgOne(h, tab//"initlz=-3 diabatic normal mode initialization")
       CALL MsgOne(h, tab//"initlz=-3 (Atmos. -> Cold Start) (Conv.,Rad,Cloud,Surface -> Warm Start)")
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
    INTEGER                  :: ntstepex
    INTEGER                  :: mhfctex 
    INTEGER                  :: mhdhn
    REAL(KIND=r8)                     :: dhex 
    REAL(KIND=r8)                     :: ntsex
    REAL(KIND=r8)                     :: mhfex
    REAL(KIND=r8)                     :: chkex
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
    ntstep=INT(REAL((nday)*86400)/dt)
    ntstepex=INT(REAL((nday)*86400)/dt)
    !IF(dhext /= 0 ) THEN
    !   mhfctex=0
    !ELSE
       mhfctex=0
    !END IF
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

    IF ( nhext == 0 .and. dhext == 0) THEN
       dhext=0
       !**(JP)** faltou atribuicao abaixo
       ntstepex=0
    ELSE IF ( nhext /= 0 .and.  dhext /= 0 ) THEN
       ntstepex=nhext/dhext
    ELSE IF ( nhext == 0 .and. dhext /= 0) THEN
       IF ( dhext /= 0 ) THEN
          IF ( hi /= hf ) THEN
             dhex   = hf-hi
             ntsex  = dhex*3600/dt
             mhfex  = dhex/dhext
             chkex  = mhfex*dhext
             IF ( chkex /= dhex ) THEN
                WRITE(nfprt,*) 'Wrong Request for the Hour in datef =', yf,mf,df,hf
                WRITE(nfprt,*) 'Difference of Hours in datei = ',yi,mi,di,hi, 'and '
                WRITE(nfprt,*) 'datef is Not Compatible With dhext =' ,dhex
                STOP
             END IF
             ntstepex=ntstepex+INT(ntsex)
             mhfctex =mhfctex +INT(mhfex)
          END IF
       ELSE
          ntstepex=0
          mhfctex=0
       END IF
    ELSE
       ntstepex=0
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
          ntstep=ntstep+INT(nts)
          mhfct=mhfct+INT(mhf)
       END IF
    END IF
    maxtim=ntstep
    maxtfm=mhfct
    cth0 =dhfct
    mdxtfm=mhdhn
    ctdh0=dhdhn
    mextfm=ntstepex
    cteh0=dhext
    ntstepmax=INT(REAL(51*366*86400)/dt)
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
       nfcnv0=0     ! initial information on convective clouds for int. radiation
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
       ifslmSib2=0
    END IF
  END SUBROUTINE ColdWarm




  SUBROUTINE setsst (c0)
    CHARACTER(LEN=*), INTENT(IN   ) :: c0

    INTEGER           :: LFSST
    INTEGER           :: LFCO2FLX
    INTEGER           :: LFNDVI
    INTEGER           :: LWSST
    INTEGER           :: LWCO2FLX
    INTEGER           :: LWNDVI
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
          CALL FatalError(h//" file "//TRIM(fNameSSTAOI)//" does not exist")
          STOP
          fNameSSTAOI=TRIM(path_in1)//'SSTClima'//LABELS//'.G'//TRIM(c0)
       END IF
    ELSE IF ( TRIM(NMSST) == 'sstwkd' ) THEN
       fNameSSTAOI=TRIM(path_in1)//TRIM(NMSST)//LABELS//'.'//TRIM(TRC)
    ELSE IF ( TRIM(NMSST) == 'sstmtd' ) THEN
       fNameSSTAOI=TRIM(path_in1)//'SSTMonthlyDirec'//LABELS//'.G'//TRIM(c0)
    ELSE IF ( TRIM(NMSST) == 'sstdyd' ) THEN
       fNameSSTAOI=TRIM(path_in1)//'SSTDailyDirec'//LABELS//'.G'//TRIM(c0)
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
!
!_____________________________________________________________________    

    IF(LEN(TRIM(NMCO2FX)) == 0 )THEN
       CALL FatalError(h//" NMSST file ("//TRIM(NMCO2FX)//") not set")
    END IF
    IF ( TRIM(NMCO2FX) == 'co2flxwkl' ) THEN
       fNameCO2FLX=TRIM(path_in1)//'FLUXCO2Weekly'//LABELS//'.G'//TRIM(c0)
       INQUIRE (FILE=TRIM(fNameCO2FLX),exist=lexist)
       IF(lexist) THEN
          fNameCO2FLX=TRIM(path_in1)//'FLUXCO2Weekly'//LABELS//'.G'//TRIM(c0)
       ELSE
          NMCO2FX='co2flxaoi'
          CALL MsgOne(h, '**************************************************************')
          CALL MsgOne(h, '* NMCO2FX changed from weekly running mean (co2flxaoi) to    *')
          CALL MsgOne(h, '* climatology (sstaoi), since sstwkl are unavailable         *')
          CALL MsgOne(h, '* for the last 15 days                                       *')
          CALL MsgOne(h, '**************************************************************')
          CALL FatalError(h//" file "//TRIM(fNameCO2FLX)//" does not exist")
          STOP
          fNameCO2FLX=TRIM(path_in1)//'FLUXCO2Clima'//LABELS//'.G'//TRIM(c0)
       END IF
    ELSE IF ( TRIM(NMSST) == 'co2flxwkd' ) THEN
       fNameCO2FLX=TRIM(path_in1)//TRIM(NMSST)//LABELS//'.'//TRIM(TRC)
    ELSE IF ( TRIM(NMSST) == 'co2flxmtd' ) THEN
       fNameCO2FLX=TRIM(path_in1)//'FLUXCO2MonthlyDirec'//LABELS//'.G'//TRIM(c0)
    ELSE IF ( TRIM(NMSST) == 'co2flxdyd' ) THEN
       fNameCO2FLX=TRIM(path_in1)//'FLUXCO2DailyDirec'//LABELS//'.G'//TRIM(c0)
    ELSE IF ( TRIM(NMSST) == 'co2flxanp' ) THEN
       fNameCO2FLX=TRIM(path_in1)//'FLUXCO2Clima'//LABELS//'.G'//TRIM(c0)
    ELSE
       NMCO2FX='co2flxaoi'
       fNameCO2FLX=TRIM(path_in1)//'FLUXCO2Clima'//LABELS//'.G'//TRIM(c0)
    END IF
    INQUIRE (FILE=TRIM(fNameCO2FLX),exist=lexist)
    IF (isimco2 /=0 .and. .NOT. lexist) THEN
       CALL FatalError(h//" file "//TRIM(fNameCO2FLX)//" does not exist")
       STOP
    END IF
!_____________________________________________________________________    

!
!
    IF(schemes == 2) THEN
       IF ( TRIM(NMNDVI) == 'ndviwkl' ) THEN
          fNameNDVIAOI=TRIM(path_in1)//'NDVIWeekly'//LABELS//'.G'//TRIM(c0)
          INQUIRE (FILE=TRIM(fNameNDVIAOI),exist=lexist)
          IF(lexist) THEN
             fNameNDVIAOI=TRIM(path_in1)//'NDVIWeekly'//LABELS//'.G'//TRIM(c0)
          ELSE
             NMNDVI='ndviaoi'
             CALL MsgOne(h, '********************************************************')
             CALL MsgOne(h, '* NMNDVI changed from weekly running mean (ndviwkl) to *')
             CALL MsgOne(h, '* climatology (ndviaoi), since ndviwkl are unavailable *')
             CALL MsgOne(h, '* for the last 15 days                                 *')
             CALL MsgOne(h, '********************************************************')
             fNameNDVIAOI=TRIM(path_in1)//'NDVI'//'.G'//TRIM(c0)
          END IF
       ELSE IF ( TRIM(NMNDVI) == 'ndviwkd' ) THEN
          fNameNDVIAOI=TRIM(path_in1)//TRIM(NMNDVI)//LABELS//'.'//TRIM(TRC)
       ELSE IF ( TRIM(NMNDVI) == 'ndvimtd' ) THEN
          fNameNDVIAOI=TRIM(path_in1)//TRIM(NMNDVI)//LABELS//'.'//TRIM(TRC)
       ELSE IF ( TRIM(NMNDVI) == 'ndvianp' ) THEN
          fNameNDVIAOI=TRIM(path_in1)//TRIM(NMNDVI)//LABELS//'.'//TRIM(TRC)
       ELSE
          NMNDVI='ndviaoi'
          fNameNDVIAOI=TRIM(path_in1)//'NDVI'//'.G'//TRIM(c0)
       END IF
           
       INQUIRE (FILE=TRIM(fNameNDVIAOI),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameNDVIAOI)//" does not exist")
          STOP
       END IF
    END IF
!_____________________________________________________________________    

    IF ( TRIM(NMSST) == 'sstwkl' ) THEN
       LFSST=-1
    ELSE
       LFSST=2
    END IF

    IF ( TRIM(NMCO2FX) == 'co2flxwkl' ) THEN
       LFCO2FLX=-1
    ELSE
       LFCO2FLX=2
    END IF

    IF(schemes == 2) THEN
       IF ( TRIM(NMNDVI) == 'ndvitwkl' ) THEN
          LFNDVI=-1
       ELSE
          LFNDVI=2
       END IF
    END IF
!_____________________________________________________________________    

    IF      ( TRIM(NMSST) == 'sstwkd' ) THEN
       LFSST=4
    ELSE IF ( TRIM(NMSST) == 'sstmtd' ) THEN
       LFSST=4
    ELSE IF ( TRIM(NMSST) == 'sstdyd' ) THEN
       LFSST=5
    END IF
!_____________________________________________________________________    

    IF      ( TRIM(NMCO2FX) == 'co2flxwkd' ) THEN
       LFCO2FLX=4
    ELSE IF ( TRIM(NMCO2FX) == 'co2flxmtd' ) THEN
       LFCO2FLX=4
    ELSE IF ( TRIM(NMCO2FX) == 'co2flxdyd' ) THEN
       LFCO2FLX=5
    END IF
!_____________________________________________________________________    

    IF(schemes == 2) THEN
       IF      ( TRIM(NMNDVI) == 'ndviwkd' ) THEN
          LFNDVI=4
       ELSE IF ( TRIM(NMNDVI) == 'ndvimtd' ) THEN
          LFNDVI=4
       END IF
    END IF
!_____________________________________________________________________    

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
!_____________________________________________________________________    

    IF (TRIM(START)  == 'warm') THEN
       IF ( LFCO2FLX == -1 ) THEN
          LWCO2FLX=0
       ELSE
          LWCO2FLX=LFCO2FLX
       END IF
    END IF
    IF ( TRIM(START) /= 'warm')THEN
       ifco2flx=LFCO2FLX
    ELSE
       ifco2flx=LWCO2FLX
    END IF
!_____________________________________________________________________    

    IF(schemes == 2) THEN  
       IF (TRIM(START)  == 'warm') THEN
          IF ( LFNDVI == -1 ) THEN
             LWNDVI=0
          ELSE
             LWNDVI=LFNDVI
          END IF
       END IF

       IF ( TRIM(START) /= 'warm')THEN
          ifndvi=LFNDVI
       ELSE
          ifndvi=LWNDVI
       END IF
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
    
    IF ( TRIM(NMSNOW) == 'snowwkl' ) THEN
       fNameSnow   = TRIM(path_in1)//'SNOWWeekly'//'.'//LABELI(1:8)//'.G'//c0
    ELSE IF (TRIM(NMSNOW) ==   'snowaoi')THEN
       fNameSnow   = TRIM(path_in1)//'Snow'//LABELI//EXTS//'.G'//c0
    ELSE
       CALL FatalError(h//" file fNameSnow does not exist")
    END IF

    ! SET CLIMATOLOGICAL SOIL MOISTURE FILE NAME
    IF(TRIM(NMSOILM) == 'soilmdyd' ) THEN
       fNameSoilms = TRIM(path_in1)//'SoilMoistureDaily'//'.G'//c0
    ELSE 
       fNameSoilms = TRIM(path_in1)//'SoilMoisture'//'.G'//c0
    END IF
    ! SET OBSERVED SOIL MOISTURE FILE NAME
                                      
    fNameSoilmsWkl = TRIM(path_in1)//'SoilMoistureWeekly'//'.'//LABELI(1:8)//'.G'//c0
    
    ! SET CLIMATOLOGICAL SOIL MOISTURE FILE NAME
    
    IF(schemes == 2) THEN
       fNameSoilMoistSib2 = TRIM(path_in1)//'CLimaSoilMoisture'//'.G'//c0
    END IF

    ! SET TOPOGRAPHY REAL GRID  "m" FILE NAME

    fNameTopo = TRIM(path_in1)//'TopographyRec'//'.G'//c0

    ! SET TOPOGRAPHY VARIANCE "m" FILE NAME

    fNameOrgvar = TRIM(path_in1)//'TopoVariance'//'.G'//c0

    ! SET TOPOGRAPHY DATA FILE NAME

    fNameHPRIME = TRIM(path_in1)//'HPRIME'//'.G'//c0

    ! SET GRADIENT OROGRAPHY "m" FILE NAME

    fNameGtopog = TRIM(path_in1)//'TopographyGradient'//LABELI//'.G'//c0

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


    ! SET OCEAN ALBEDO LOOK-UP-TABLE FILE NAME

    fNameSlabOcen = TRIM(path_in1)//'ocnalbtab24bnd.bin'

    IF(schemes == 2) THEN

       ! SET TABLE OF SOIL PHYSICAL CHAR.

       fNameSoilTab = TRIM(path_in1)//'SoilChar.Tab' !Lookup table of soil physical char.

       ! SET TABLE VEG. MORPHILOGICAL CHAR.

       fNameMorfTab = TRIM(path_in1)//'Morph.Tab'     !Lookup table veg. morphilogical char.

       ! SET TABLE VEG. BIOME DEPENDANT VARIABLES CHAR.

       fNameBioCTab = TRIM(path_in1)//'BioChar.Tab' !Lookup table of biome dependant variables table veg.

       ! SET INTERPOLATION TABLES FOR AERO VARIABLES

       fNameAeroTab = TRIM(path_in1)//'AeroVar.Tab' !Interpolation tables for aero variables

       ! SET CLIMATOLOGICAL SIB2 VEGETATION MASK FILE NAME

       fNameSiB2Mask = TRIM(path_in1)//'VegetationMaskSiB2'//'.G'//c0
    
       ! SET LOOKUP MAP OF SOIL PERCENT SAND    FILE NAME

       fNameSandMask = TRIM(path_in1)//'PorceSandMaskSiB2'//'.G'//c0
    
       ! SET LOOKUP MAP OF SOIL PERCENT CLAY  FILE NAME

       fNameClayMask = TRIM(path_in1)//'PorceClayMaskSiB2'//'.G'//c0

       ! SET LOOKUP MAP OF SOIL TYPES FILE NAME	  

       fNameTextMask = TRIM(path_in1)//'SoilTextureMaskSiB2'//'.G'//c0

    END IF

    IF(schemes == 3) THEN

       ! SET LOOKUP MAP OF SOIL TYPES FILE NAME	  

       fNameIBISMask = TRIM(path_in1)//'VegetationMaskIBIS'//'.G'//c0

       ! SET FILE NAME OF THE ABSOLUTE MINIMUM TEMPERATURE - TEMP ON AVERAGE OF COLDEST MONTH (C)

       fNameIBISDeltaTemp = TRIM(path_in1)//'DeltaTempColdes'//'.G'//c0
    
       ! SET LOOKUP MAP OF SOIL PERCENT SAND    FILE NAME

       fNameSandMask = TRIM(path_in1)//'PorceSandMaskIBIS'//'.G'//c0
    
       ! SET LOOKUP MAP OF SOIL PERCENT CLAY  FILE NAME

       fNameClayMask = TRIM(path_in1)//'PorceClayMaskIBIS'//'.G'//c0

       ! SET LOOKUP MAP OF CLIMATE TEMPERATURE 2 METERS  FILE NAME

       fNameClimaTemp = TRIM(path_in1)//'Temperature'//'.G'//c0


    END IF

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

    ! SET TRACER FILE NAME
    
    IF (iftracer >=1 .and. iftracer <=4 ) THEN
       IF (iftracer == 1) THEN ! first call only 
          fNametracer  = TRIM(path_in1)//'TRAC'//TRIM(PREFY)//LABELI//'S.grd.'//'G'//TRIM(c0)//TRIM(LV)
       ELSEIF (iftracer == 2) THEN ! clim interpol
          fNametracer  = TRIM(path_in1)//'tracerclm'//'.G'//TRIM(c0)//TRIM(LV)
       ELSEIF (iftracer == 3) THEN ! pred interpol
          fNametracer  = TRIM(path_in1)//'tracerfct'//'.G'//TRIM(c0)//TRIM(LV)
       ELSEIF (iftracer == 4) THEN ! direct access
          fNametracer  = TRIM(path_in1)//'tracermtd'//'.G'//TRIM(c0)//TRIM(LV)
       ENDIF
    ENDIF

    ! SET UKMO SPECTRAL FILENAME

    fNameSpecSW = TRIM(path_in1)//'sp_sw_hadgem1_3r'
    fNameSpecLW = TRIM(path_in1)//'sp_lw_hadgem1_3'


    fNameCldOptSW =  TRIM(path_in1)//'F_nwvl200_mu20_lam50_res64_t298_c080428.bin'
    fNameCldOptLW =  TRIM(path_in1)//'iceoptics_c080917.bin'


    ! SET GFS MICROPHYSICS FILENAME

    fNameMicro = TRIM(path_in1)//'ETAMPNEW_DATA'

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
      
       IF(schemes == 2) THEN
          INQUIRE (FILE=TRIM(fNameSoilMoistSib2),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameSoilMoistSib2)//" does not exist")
          END IF        
       END IF   

       INQUIRE (FILE=TRIM(fNameTopo),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameTopo)//" does not exist")
       END IF

       INQUIRE (FILE=TRIM(fNameOrgvar),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameOrgvar)//" does not exist")
       END IF
       IF(TRIM(IGWD) == 'GMB')THEN
          INQUIRE (FILE=TRIM(fNameHPRIME),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameHPRIME)//" does not exist")
          END IF
       END IF

       IF(TRIM(IGWD) == 'USS')THEN
          INQUIRE (FILE=TRIM(fNameGtopog),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameGtopog)//" does not exist")
          END IF
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

       INQUIRE (FILE=TRIM(fNameSlabOcen),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNameSlabOcen)//" does not exist")
       END IF

       IF(schemes == 2) THEN

          INQUIRE (FILE=TRIM(fNameSoilTab),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameSoilTab)//" does not exist")
          END IF

          INQUIRE (FILE=TRIM(fNameMorfTab),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameMorfTab)//" does not exist")
          END IF

          INQUIRE (FILE=TRIM(fNameBioCTab),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameBioCTab)//" does not exist")
          END IF

          INQUIRE (FILE=TRIM(fNameAeroTab),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameAeroTab)//" does not exist")
          END IF

          INQUIRE (FILE=TRIM(fNameSiB2Mask),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameSiB2Mask)//" does not exist")
          END IF

          INQUIRE (FILE=TRIM(fNameSandMask),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameSandMask)//" does not exist")
          END IF

          INQUIRE (FILE=TRIM(fNameClayMask),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameClayMask)//" does not exist")
          END IF

          INQUIRE (FILE=TRIM(fNameTextMask),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameTextMask)//" does not exist")
          END IF
        
       END IF
       IF(schemes == 3) THEN

          INQUIRE (FILE=TRIM(fNameIBISMask),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameIBISMask)//" does not exist")
          END IF
          
          INQUIRE (FILE=TRIM(fNameIBISDeltaTemp),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameIBISDeltaTemp)//" does not exist")
          END IF
          
          INQUIRE (FILE=TRIM(fNameSandMask),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameSandMask)//" does not exist")
          END IF

          INQUIRE (FILE=TRIM(fNameClayMask),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameClayMask)//" does not exist")
          END IF
 
          INQUIRE (FILE=TRIM(fNameClimaTemp),exist=lexist)
          IF (.NOT. lexist) THEN
             CALL FatalError(h//" file "//TRIM(fNameClimaTemp)//" does not exist")
          END IF      
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

    IF (iftracer/=0) THEN
       INQUIRE (FILE=TRIM(fNametracer),exist=lexist)
       IF (.NOT. lexist) THEN
          CALL FatalError(h//" file "//TRIM(fNametracer)//" does not exist")
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
       fNameInput0=TRIM(dirfNameOutput)//'/'//'GFCT'//TRIM(PREFX)//LABELC//LABELC//EXTW//'.'//TRIM(TRCG)//TRIM(LV)//'.outmdt'
       fNameInput1=TRIM(dirfNameOutput)//'/'//'GFCT'//TRIM(PREFX)//LABELC//LABELC//EXTW//'.'//TRIM(TRCG)//TRIM(LV)//'.outatt'
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
        FNameRestInput1=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labelc//labelc//&
            &EXTW//'.'//TRIM(TruncLev)//'.outmdt'//TRIM(PRC)

        FNameRestInput2=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labelc//labelc//&
            &EXTW//'.'//TRIM(TruncLev)//'.outatt'//TRIM(PRC)

        FNameConvClInp0=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labelc//labelc//&
            &EXTW//'.'//TRIM(TruncLev)//'.convcl'//TRIM(PRC)

        FNameSibPrgInp0=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labelc//labelc//&
            &EXTW//'.'//TRIM(TruncLev)//'.sibprg'//TRIM(PRC)
    ELSE
        FNameSibPrgOut1=TRIM(dirRstOutput)//"/"//"GFCT"//TRIM(PREFX)//labelc//labelc//&
             &EXTW//'.'//TRIM(TruncLev)//'.sibprg'//TRIM(PRC)

        FNameConvClOut1=TRIM(dirRstOutput)//"/"//"GFCT"//TRIM(PREFX)//labelc//labelc//&
             &EXTW//'.'//TRIM(TruncLev)//'.convcl'//TRIM(PRC)

        FNameRestOutput2=TRIM(dirRstOutput)//"/"//"GFCT"//TRIM(PREFX)//labelc//labelc//&
             &EXTW//'.'//TRIM(TruncLev)//'.outatt'//TRIM(PRC)

        FNameRestOutput1=TRIM(dirRstOutput)//"/"//"GFCT"//TRIM(PREFX)//labelc//labelc//&
             &EXTW//'.'//TRIM(TruncLev)//'.outmdt'//TRIM(PRC)
    END IF
  END SUBROUTINE FileNameRest

  SUBROUTINE FileNameRest2(PRC,OPT,REALTIME)
   IMPLICIT NONE
    CHARACTER(LEN= * ), INTENT(IN   )  :: PRC
    INTEGER           , INTENT(IN   )  :: OPT 
    CHARACTER(LEN= * ), INTENT(IN   )  :: REALTIME
    CHARACTER(LEN= 10)                 :: LABELI
    CHARACTER(LEN= 10)                 :: LABELC
    CHARACTER(LEN= 10)                 :: LABELF
    CHARACTER(LEN=*), PARAMETER :: h="**(FileNameRest2)**"
    WRITE(LABELI,'(i4.4,3i2.2)')idate(4),idate(2),idate(3),idate(1)
    WRITE(LABELC,'(i4.4,3i2.2)')idatec(4),idatec(2),idatec(3),idatec(1)
    WRITE(LABELF,'(i4.4,3i2.2)')idatec(4),idatec(2),idatec(3),idatec(1)
    CALL MsgOne(h, "restar ssib files "//TRIM(REALTIME))

    IF (OPT==1) THEN
        FNameRestInput1=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labeli//labeli//&
            &EXTW//'.'//TRIM(TruncLev)//'.outmdt'//TRIM(PRC)

        FNameRestInput2=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labeli//labeli//&
            &EXTW//'.'//TRIM(TruncLev)//'.outatt'//TRIM(PRC)

        FNameConvClInp0=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labeli//labeli//&
            &EXTW//'.'//TRIM(TruncLev)//'.convcl'//TRIM(PRC)

        FNameSibPrgInp0=TRIM(dirFnameOutput)//"/"//"GFCT"//TRIM(PREFX)//labeli//labeli//&
            &EXTW//'.'//TRIM(TruncLev)//'.sibprg'//TRIM(PRC)
    ELSE
        FNameSibPrgOut1=TRIM(dirRstOutput)//"/"//"GFCT"//TRIM(PREFX)//labelc//labelc//&
             &EXTW//'.'//TRIM(TruncLev)//'.sibprg'//TRIM(PRC)

        FNameConvClOut1=TRIM(dirRstOutput)//"/"//"GFCT"//TRIM(PREFX)//labelc//labelc//&
             &EXTW//'.'//TRIM(TruncLev)//'.convcl'//TRIM(PRC)

        FNameRestOutput2=TRIM(dirRstOutput)//"/"//"GFCT"//TRIM(PREFX)//labelc//labelc//&
             &EXTW//'.'//TRIM(TruncLev)//'.outatt'//TRIM(PRC)

        FNameRestOutput1=TRIM(dirRstOutput)//"/"//"GFCT"//TRIM(PREFX)//labelc//labelc//&
             &EXTW//'.'//TRIM(TruncLev)//'.outmdt'//TRIM(PRC)
    END IF
  END SUBROUTINE FileNameRest2

  SUBROUTINE FileNameGeral(PRC,OPT)
    IMPLICIT NONE
    CHARACTER(LEN= * ), INTENT(IN   )  :: PRC
    CHARACTER(LEN= * ), INTENT(IN   )  :: OPT
    CHARACTER(LEN= 10)                 :: LABELI
    CHARACTER(LEN= 10)                 :: LABELC
    CHARACTER(LEN= 10)                 :: LABELF
    CHARACTER(LEN=*), PARAMETER :: h="**(PROCESSOR)**"

    WRITE(LABELI,'(i4.4,3i2.2)')IDATEI(4),IDATEI(3),IDATEI(2),IDATEI(1)
    WRITE(LABELC,'(i4.4,3i2.2)')IDATEW(4),IDATEW(3),IDATEW(2),IDATEW(1)
    WRITE(LABELF,'(i4.4,3i2.2)')IDATEF(4),IDATEF(3),IDATEF(2),IDATEF(1)

    ! first line

    CALL MsgOne(h, PRC)

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
       IF ( TRIM(START) == 'cold' ) THEN    
          FNameOutGH=TRIM(dirFnameOutput)//"/"//TRIM(OPT)//TRIM(PREFX)//labeli//labelc//&
             &EXTW//'.'//TRIM(TruncLev)
          FNameTopGH=TRIM(dirFnameOutput)//"/"//TRIM(OPT)//TRIM(PREFX)//labeli//labelc//&
             &'F.top'//'.'//TRIM(TruncLev)
          FNamenDrGH=TRIM(dirFnameOutput)//"/"//TRIM(OPT)//TRIM(PREFX)//labeli//labelc//&
             &EXDH//'.'//TRIM(TruncLev)
       ELSE
          FNameOutGH=TRIM(dirFnameOutput)//"/"//TRIM(OPT)//TRIM(PREFX)//labelc//labelf//&
             &EXTW//'.'//TRIM(TruncLev)
          FNameTopGH=TRIM(dirFnameOutput)//"/"//TRIM(OPT)//TRIM(PREFX)//labelc//labelf//&
             &'F.top'//'.'//TRIM(TruncLev)
          FNamenDrGH=TRIM(dirFnameOutput)//"/"//TRIM(OPT)//TRIM(PREFX)//labelc//labelf//&
             &EXDH//'.'//TRIM(TruncLev)
       END IF
    END IF 
  END SUBROUTINE FileNameGeral


  SUBROUTINE SetOutPut (tod1,idate)

    REAL(KIND=r8)   , INTENT(IN  )  :: tod1
    INTEGER, INTENT(IN  )  :: idate (4)

    REAL(KIND=r8)     :: tod
    INTEGER  :: l
    INTEGER  :: maxtfm2=17
    INTEGER  :: maxtfmfix=0
    INTEGER  :: jdt
    INTEGER  :: ihour
    INTEGER  :: idays
    INTEGER  :: imont
    INTEGER  :: iyear,ierr
    REAL(KIND=r8)     :: cthr(maxtim)
    REAL(KIND=r8)     :: cthrfx(200)

    INTEGER, PARAMETER :: imonth=12
    INTEGER, PARAMETER :: nday1(imonth)=(/31,28,31,30,31,30,&
         31,31,30,31,30,31 /)
    INTEGER, PARAMETER :: nday2(imonth)=(/31,29,31,30,31,30,&
         31,31,30,31,30,31 /)
    NAMELIST /NumberOutPutForecast/cthrfx,maxtfmfix

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
             maxtfm2      = 17 ! number of output forecast
             cthr        = 0.0_r8
             cthr( 1)= 6.0_r8;cthr( 2)=12.0_r8;cthr( 3)= 18.0_r8;cthr( 4)= 24.0_r8
             cthr( 5)=30.0_r8;cthr( 6)=36.0_r8;cthr( 7)= 42.0_r8;cthr( 8)= 48.0_r8
             cthr( 9)=54.0_r8;cthr(10)=60.0_r8;cthr(11)= 66.0_r8;cthr(12)= 72.0_r8
             cthr(13)=84.0_r8;cthr(14)=96.0_r8;cthr(15)=120.0_r8;cthr(16)=144.0_r8
             cthr(17)=168.0_r8
             READ (nNameList,NumberOutPutForecast, iostat=ierr)
             IF(maxtfmfix >0)THEN
                maxtfm2=maxtfmfix
                cthr(1:maxtfm2)=cthrfx(1:maxtfm2)
             END IF
       END IF

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


  SUBROUTINE SetOutPutDHN (tod1,idate)
    IMPLICIT NONE
    REAL(KIND=r8)   , INTENT(IN  )  :: tod1
    INTEGER         , INTENT(IN  )  :: idate (4)

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
    cthr=0.0_r8
    maxtfm2=maxtim
    IF(DHDHN >= 0 ) THEN
       IF (DHDHN > 0) THEN
          maxtfm2=maxtim
          cthr  =0.0_r8
          cthr(1)=DHDHN
          DO l=2,maxtfm2
             cthr(l)=cthr(l-1)+DHDHN
          END DO
       !ELSE IF ( DHDHN == 0 ) THEN
       !   READ(nNameList,*)maxtfm2
       !   IF (maxtfm2 > 0)   THEN
       !      READ(nNameList,*)cthr( 1:maxtfm2)
       !   ELSE
       !      maxtfm2      = 17 ! number of output forecast
       !      cthr        = 0.0_r8
       !      cthr( 1)= 6.0_r8;cthr( 2)=12.0_r8;cthr( 3)= 18.0_r8;cthr( 4)= 24.0_r8
       !      cthr( 5)=30.0_r8;cthr( 6)=36.0_r8;cthr( 7)= 42.0_r8;cthr( 8)= 48.0_r8
       !      cthr( 9)=54.0_r8;cthr(10)=60.0_r8;cthr(11)= 66.0_r8;cthr(12)= 72.0_r8
       !      cthr(13)=84.0_r8;cthr(14)=96.0_r8;cthr(15)=120.0_r8;cthr(16)=144.0_r8
       !      cthr(17)=168.0_r8
       !   END IF
       END IF
       !CLOSE(nNameList)
       ihour=0
       DO jdt=1,maxtim
          tod=tod+delt
          IF(MOD(tod,3600.0_r8) == 0.0_r8) THEN
             ihour=ihour+1
             DO l=1,maxtfm2
                IF (abs(cthr(l)-ihour) .le. 0.00001_r8) THEN
                   cdhl(jdt)=.true.
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
                cdhl(jdt)   =.true.
                IF(imont > 12) THEN
                   imont = 1
                   iyear = iyear + 1
                END IF
                PRINT*,cdhl(jdt) ,iyear, imont, idays, ihour,tod
             END IF

          ELSE

             IF(idays > nday1(imont)) THEN
                idays=1
                imont=imont+1
                cdhl(jdt)   =.true.
                IF(imont > 12) THEN
                   imont = 1
                   iyear = iyear + 1
                END IF
                PRINT*,cdhl(jdt) ,iyear, imont, idays, ihour,tod
             END IF

          END IF
       END DO
    END IF
  END SUBROUTINE SetOutPutDHN

  CHARACTER(LEN=10) FUNCTION CalDate(labelc,opt)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN   ) :: labelc
    INTEGER          :: nday
    CHARACTER(LEN=*), INTENT(IN   ) :: opt

    INTEGER :: iyear
    INTEGER :: imonth
    INTEGER :: iday
    INTEGER :: ihour
    INTEGER :: lenmon
   INTEGER, PARAMETER :: lenMonth2(0:13)=(/31,31,28,31,30,31,30,&
         31,31,30,31,30,31,31/)
    INTEGER, PARAMETER :: lenMonth(12)=(/31,28,31,30,31,30,&
         31,31,30,31,30,31/)
    IF(LEN_TRIM(labelc) < 10)THEN
       WRITE(0,*)'ERRO LEN_TRIM(labelc) <10', LEN_TRIM(labelc)
       STOP
    END IF
    READ(labelc(1: 4),'(I4.4)')iyear
    READ(labelc(5: 6),'(I2.2)')imonth
    READ(labelc(7: 8),'(I2.2)')iday
    READ(labelc(9:10),'(I2.2)')ihour
    IF(opt=='-')THEN
       nday=lenMonth2(imonth-1)
       IF(imonth-1 == 2 .AND. MOD(REAL(iyear),4.0) == 0.0)nday=29

       lenmon=lenMonth(imonth)
       IF(imonth == 2 .AND. MOD(REAL(iyear),4.0) == 0.0)lenmon=29
       iday=iday-nday
       IF (iday < 1) THEN
          imonth=imonth-1
          IF(imonth <1) THEN
             iyear=iyear-1
             imonth=12
          END IF
          lenmon=lenMonth(imonth)
          IF(imonth == 2 .AND. MOD(REAL(iyear),4.0) == 0.0)lenmon=29
          iday=lenmon+iday
       END IF
    ELSE IF(opt=='+')THEN
       nday=lenMonth2(imonth)
       IF(imonth == 2 .AND. MOD(REAL(iyear),4.0) == 0.0)nday=29

       lenmon=lenMonth(imonth)
       IF(imonth == 2 .AND. MOD(REAL(iyear),4.0) == 0.0)lenmon=29
       iday=iday+nday
       IF (iday > lenmon) THEN
          imonth=imonth+1
          iday=iday-lenmon
          IF(imonth > 12) THEN
             iyear=iyear+1
             imonth=1
          END IF
          lenmon=lenMonth(imonth)
          IF(imonth == 2 .AND. MOD(REAL(iyear),4.0) == 0.0)lenmon=29
       END IF

    END IF
    WRITE(CalDate(1: 4),'(I4.4)')iyear
    WRITE(CalDate(5: 6),'(I2.2)')imonth
    WRITE(CalDate(7: 8),'(I2.2)')iday
    WRITE(CalDate(9:10),'(I2.2)')ihour
  END FUNCTION CalDate

END MODULE Options
