!
!  $Author: pkubota $
!  $Date: 2011/04/07 16:00:31 $
!  $Revision: 1.23 $
!
MODULE Diagnostics

  USE Parallelism, ONLY: &
       myid,             &
       maxNodes,         &
       myid_four,        &
       maxNodes_four,    &
       mygroup_four,     &
       COMM_FOUR,        &
       MsgDump,          &
       MsgOne,           &
       FatalError

  USE Constants, ONLY :   &
       r8, i8, &
       ndavl, ndrq, ncdg, jxavl, jxcdg, numx, grav

  USE Utils, ONLY:     &
       tmstmp2,        &
       IBJBtoIJ,       &
       SplineIBJBtoIJ, &
       LinearIBJBtoIJ, &
       SeaMaskIBJBtoIJ,&
       NearestIBJBtoIJ

  USE Options, ONLY: &
       reducedGrid,  &
       yrl,   &
       monl,  &
       nfprt, &
       nferr, &
       nfdestbl, &
       nfctrl

  USE IOLowLevel, ONLY: &
       WriteField   , &
       WriteDiagHead, &
       WriteProgHead, &
       WriteDir     , &
       WriteDire

  USE InputOutput, ONLY: &
       scloutsp,         &
       scloutgr,         &
       cnvray,           &
       aunits


  USE Sizes, ONLY: &
       mymnMax,    &
       myjmax_d,   &
       myMMax,     &
       myMMap,     &
       ijmax,      &
       kmaxloc,    &
       kmax,       &
       kfirst_four,&
       klast_four, &
       nlatsinproc_d,&
       Msinproc,   &
       Msperproc,  &
       HaveM1,     &
       havesurf,   &
       nodeHasM,   &
       ngroups_four, &
       first_proc_four

  USE Communications, ONLY: &
       Collect_Grid_Red,    &
       Collect_Grid_Sur,    &
       Collect_Gauss,       &
       Collect_Grid_Sur_Print, &
       Collect_Grid_Full,      &
       p2d,                    &
       Collect_Spec



  IMPLICIT NONE

  PRIVATE
  PUBLIC :: InitDiagnostics
  PUBLIC :: pwater
  PUBLIC :: globme
  PUBLIC :: rsdiag
  PUBLIC :: upspec
  PUBLIC :: updia
  PUBLIC :: accpf
  PUBLIC :: Prec_Diag
  PUBLIC :: wridia
  PUBLIC :: weprog
  PUBLIC :: wrprog

  PUBLIC :: lgaus
  PUBLIC :: combf
  PUBLIC :: reqdg
  PUBLIC :: itcf
  PUBLIC :: nucf
  PUBLIC :: lvcf
  PUBLIC :: nurq
  PUBLIC :: iavrq
  PUBLIC :: itavl
  PUBLIC :: nuavl
  PUBLIC :: lvavl
  PUBLIC :: dodia
  PUBLIC :: inavl
  PUBLIC :: ixavl
  PUBLIC :: iclcd
  PUBLIC :: incf
  PUBLIC :: ixcf
  PUBLIC :: kravl
  PUBLIC :: jrcf
  PUBLIC :: krcf
  PUBLIC :: icf
  PUBLIC :: mxavl
  PUBLIC :: gaus
  PUBLIC :: gaus_in
  PUBLIC :: lvrq

  INTERFACE updia
     MODULE PROCEDURE updia2D, updia1D
  END INTERFACE

  INCLUDE 'mpif.h'

  REAL(KIND=r8),    PARAMETER :: undef =1.0e53_r8
  INTEGER, PARAMETER :: ngbme = 13
  INTEGER, PARAMETER :: igbme(ngbme)= &
       (/9,10,16,18,19,17,21,23,24,25,28,29,30/)
  INTEGER  :: ierr
  ! Available Diagnostics Indexes
  INTEGER, PUBLIC, PARAMETER :: nDiag_tmpsfc =  1 ! time mean surface pressure
  INTEGER, PUBLIC, PARAMETER :: nDiag_tmdivg =  2 ! time mean divergence (subroutine accpf)
  INTEGER, PUBLIC, PARAMETER :: nDiag_tmvort =  3 ! time mean vorticity (subroutine accpf)
  INTEGER, PUBLIC, PARAMETER :: nDiag_tmsphu =  4 ! time mean specific humidity (subroutine accpf)
  INTEGER, PUBLIC, PARAMETER :: nDiag_tmtvir =  5 ! time mean virtual temperature (subroutine accpf)
  INTEGER, PUBLIC, PARAMETER :: nDiag_tmtsfc =  6 ! time mean surface temperature
  INTEGER, PUBLIC, PARAMETER :: nDiag_omegav =  7 ! omega
  INTEGER, PUBLIC, PARAMETER :: nDiag_sigdot =  8 ! sigma dot
  INTEGER, PUBLIC, PARAMETER :: nDiag_toprec =  9 ! total precipiation
  INTEGER, PUBLIC, PARAMETER :: nDiag_cvprec = 10 ! convective precipitation
  INTEGER, PUBLIC, PARAMETER :: nDiag_lsprec = 11 ! large scale precipitation
  INTEGER, PUBLIC, PARAMETER :: nDiag_snowfl = 12 ! snowfall
  INTEGER, PUBLIC, PARAMETER :: nDiag_runoff = 13 ! runoff
  INTEGER, PUBLIC, PARAMETER :: nDiag_pwater = 14 ! precipitable water
  INTEGER, PUBLIC, PARAMETER :: nDiag_intlos = 15 ! interception loss
  INTEGER, PUBLIC, PARAMETER :: nDiag_sheatf = 16 ! sensible heat flux
  INTEGER, PUBLIC, PARAMETER :: nDiag_lheatf = 17 ! latent heat flux
  INTEGER, PUBLIC, PARAMETER :: nDiag_ustres = 18 ! surface zonal stress
  INTEGER, PUBLIC, PARAMETER :: nDiag_vstres = 19 ! surface meridional stress
  INTEGER, PUBLIC, PARAMETER :: nDiag_cloudc = 20 ! cloud cover
  INTEGER, PUBLIC, PARAMETER :: nDiag_lwdbot = 21 ! longwave downward at bottom
  INTEGER, PUBLIC, PARAMETER :: nDiag_lwubot = 22 ! longwave upward at bottom
  INTEGER, PUBLIC, PARAMETER :: nDiag_lwutop = 23 ! longwave upward at top
  INTEGER, PUBLIC, PARAMETER :: nDiag_swdtop = 24 ! shortwave downward at top
  INTEGER, PUBLIC, PARAMETER :: nDiag_swdbot = 25 ! shortwave downward at ground
  INTEGER, PUBLIC, PARAMETER :: nDiag_swubot = 26 ! shortwave upward at bottom
  INTEGER, PUBLIC, PARAMETER :: nDiag_swutop = 27 ! shortwave upward at top
  INTEGER, PUBLIC, PARAMETER :: nDiag_swabea = 28 ! shortwave absorbed by the earth/atmosphere
  INTEGER, PUBLIC, PARAMETER :: nDiag_swabgr = 29 ! shortwave absorbed by the ground
  INTEGER, PUBLIC, PARAMETER :: nDiag_lwnetb = 30 ! net longwave at bottom
  INTEGER, PUBLIC, PARAMETER :: nDiag_lwheat = 31 ! longwave heating
  INTEGER, PUBLIC, PARAMETER :: nDiag_swheat = 32 ! shortwave heating
  INTEGER, PUBLIC, PARAMETER :: nDiag_clheat = 33 ! convective latent heating
  INTEGER, PUBLIC, PARAMETER :: nDiag_cmchan = 34 ! convective moisture change
  INTEGER, PUBLIC, PARAMETER :: nDiag_lslhea = 35 ! large scale latent heating
  INTEGER, PUBLIC, PARAMETER :: nDiag_lsmcha = 36 ! large scale moisture change
  INTEGER, PUBLIC, PARAMETER :: nDiag_sclhea = 37 ! shallow convective latent heating
  INTEGER, PUBLIC, PARAMETER :: nDiag_scmcha = 38 ! shallow convective moisture change
  INTEGER, PUBLIC, PARAMETER :: nDiag_vdheat = 39 ! vertical diffusion heating
  INTEGER, PUBLIC, PARAMETER :: nDiag_vdmois = 40 ! vertical diffusion moistening
  INTEGER, PUBLIC, PARAMETER :: nDiag_vduzon = 41 ! vertical diffusion zonal momentum change
  INTEGER, PUBLIC, PARAMETER :: nDiag_vdvmer = 42 ! vertical diffusion meridional momentum change
  INTEGER, PUBLIC, PARAMETER :: nDiag_txgwds = 43 ! gravity wave drag surface zonal stress
  INTEGER, PUBLIC, PARAMETER :: nDiag_tygwds = 44 ! gravity wave drag surface meridional stress
  INTEGER, PUBLIC, PARAMETER :: nDiag_gwduzc = 45 ! gravity wave drag zonal momentum change
  INTEGER, PUBLIC, PARAMETER :: nDiag_gwdvmc = 46 ! gravity wave drag meridional momentum change
  INTEGER, PUBLIC, PARAMETER :: nDiag_hhedif = 47 ! horizontal heating diffusion
  INTEGER, PUBLIC, PARAMETER :: nDiag_hmodif = 48 ! horizontal moisture diffusion
  INTEGER, PUBLIC, PARAMETER :: nDiag_hdidif = 49 ! horizontal divergence diffusion
  INTEGER, PUBLIC, PARAMETER :: nDiag_hvodif = 50 ! horizontal vorticity diffusion
  INTEGER, PUBLIC, PARAMETER :: nDiag_divgxq = 51 ! divergence * specific humidity
  INTEGER, PUBLIC, PARAMETER :: nDiag_vmoadv = 52 ! vertical moisture advection
  INTEGER, PUBLIC, PARAMETER :: nDiag_hmofcv = 53 ! horizontal moisture flux convergence (???)
  INTEGER, PUBLIC, PARAMETER :: nDiag_vimfcv = 54 ! vertically integrated moisture flux convergence (???)
  INTEGER, PUBLIC, PARAMETER :: nDiag_tmlnps = 55 ! time mean log surface pressure (subroutine accpf)
  INTEGER, PUBLIC, PARAMETER :: nDiag_lwdbtc = 56 ! longwave downward at bottom (clear)
  INTEGER, PUBLIC, PARAMETER :: nDiag_lwutpc = 57 ! longwave upward at top (clear)
  INTEGER, PUBLIC, PARAMETER :: nDiag_swdbtc = 58 ! shortwave downward at ground (clear)
  INTEGER, PUBLIC, PARAMETER :: nDiag_swubtc = 59 ! shortwave upward at bottom (clear)
  INTEGER, PUBLIC, PARAMETER :: nDiag_swutpc = 60 ! shortwave upward at top (clear)
  INTEGER, PUBLIC, PARAMETER :: nDiag_swaeac = 61 ! shortwave absorbed by the earth/atmosphere (clear)
  INTEGER, PUBLIC, PARAMETER :: nDiag_swabgc = 62 ! shortwave absorbed by the ground (clear)
  INTEGER, PUBLIC, PARAMETER :: nDiag_lwnbtc = 63 ! net longwave at bottom (clear)
  INTEGER, PUBLIC, PARAMETER :: nDiag_tmtdps = 64 ! time mean deep soil temperature
  INTEGER, PUBLIC, PARAMETER :: nDiag_tgfccv = 65 ! ground/surface cover temperature
  INTEGER, PUBLIC, PARAMETER :: nDiag_tcanop = 66 ! canopy temperature
  INTEGER, PUBLIC, PARAMETER :: nDiag_tcairs = 67 ! temperature of canopy air space
  INTEGER, PUBLIC, PARAMETER :: nDiag_ecairs = 68 ! vapor pressure of canopy air space
  INTEGER, PUBLIC, PARAMETER :: nDiag_bsolht = 69 ! bare soil latent heat
  INTEGER, PUBLIC, PARAMETER :: nDiag_nshcrm = 70 ! negative specific humidity correction moisture source
  INTEGER, PUBLIC, PARAMETER :: nDiag_ozonmr = 71 ! ozone mass mixing ratio (g/g)
  INTEGER, PUBLIC, PARAMETER :: nDiag_vdtclc = 72 ! vertical dist total cloud cover
  INTEGER, PUBLIC, PARAMETER :: nDiag_invcld = 73 ! inversion cloud
  INTEGER, PUBLIC, PARAMETER :: nDiag_ssatcl = 74 ! supersaturation cloud
  INTEGER, PUBLIC, PARAMETER :: nDiag_cnvcld = 75 ! convective cloud
  INTEGER, PUBLIC, PARAMETER :: nDiag_shcvcl = 76 ! shallow convective cloud
  INTEGER, PUBLIC, PARAMETER :: nDiag_clliwp = 77 ! cloud liquid water path
  INTEGER, PUBLIC, PARAMETER :: nDiag_lwcemi = 78 ! longwave cloud emissivity
  INTEGER, PUBLIC, PARAMETER :: nDiag_sclopd = 79 ! shortwave cloud optical depth
  INTEGER, PUBLIC, PARAMETER :: nDiag_mofres = 80 ! momentum flux resistance
  INTEGER, PUBLIC, PARAMETER :: nDiag_casrrs = 81 ! canopy air spc to ref. lvl resistance
  INTEGER, PUBLIC, PARAMETER :: nDiag_cascrs = 82 ! canopy air spc to canopy resistance
  INTEGER, PUBLIC, PARAMETER :: nDiag_casgrs = 83 ! canopy air spc to ground resistance
  INTEGER, PUBLIC, PARAMETER :: nDiag_gcovrs = 84 ! ground cover resistance
  INTEGER, PUBLIC, PARAMETER :: nDiag_bssfrs = 85 ! bare soil surface resistance
  INTEGER, PUBLIC, PARAMETER :: nDiag_homtvu = 86 ! Horizontal Momentum Transport
  INTEGER, PUBLIC, PARAMETER :: nDiag_vzmtwu = 87 ! Vertical Zonal Momentum Transport
  INTEGER, PUBLIC, PARAMETER :: nDiag_vmmtwv = 88 ! Vertical Meridional Momentum Transport
  INTEGER, PUBLIC, PARAMETER :: nDiag_mshtvt = 89 ! Meridional Sensible Heat Transport
  INTEGER, PUBLIC, PARAMETER :: nDiag_zshtut = 90 ! Zonal Sensible Heat Transport
  INTEGER, PUBLIC, PARAMETER :: nDiag_vshtwt = 91 ! Vertical Sensible Heat Transport
  INTEGER, PUBLIC, PARAMETER :: nDiag_mshtuq = 92 ! Meridional Specific Humidity Transport
  INTEGER, PUBLIC, PARAMETER :: nDiag_zshtuq = 93 ! Zonal Specific Humidity Transport
  INTEGER, PUBLIC, PARAMETER :: nDiag_vshtwq = 94 ! Vertical Specific Humidity Transport
  INTEGER, PUBLIC, PARAMETER :: nDiag_lwhtcl = 95 ! longwave heating (clear sky)
  INTEGER, PUBLIC, PARAMETER :: nDiag_swhtcl = 96 ! shortwave heating (clear sky)
  INTEGER, PUBLIC, PARAMETER :: nDiag_tep02m = 97 ! time mean temp at 2-m from sfc layer
  INTEGER, PUBLIC, PARAMETER :: nDiag_mxr02m = 98 ! time mean es humid at 2-m from sfc layer
  INTEGER, PUBLIC, PARAMETER :: nDiag_spw02m = 99 ! Speed wind at 2-m from surface layer
  INTEGER, PUBLIC, PARAMETER :: nDiag_tep10m = 100 ! Temperature at 10-m from surface layer
  INTEGER, PUBLIC, PARAMETER :: nDiag_mxr10m = 101 ! specifc humidity at 10-m from surface layer
  INTEGER, PUBLIC, PARAMETER :: nDiag_spw10m = 102 ! Speed wind at 10-m from surface layer
  INTEGER, PUBLIC, PARAMETER :: nDiag_viozoc = 103 ! Vertically Integrated Ozone Content (Dobson units)
  INTEGER, PUBLIC, PARAMETER :: nDiag_dewptt = 104 ! Dew Point Temperature K
  INTEGER, PUBLIC, PARAMETER :: nDiag_zwn10m = 105 ! Zonal Wind at 10-m from surface layer
  INTEGER, PUBLIC, PARAMETER :: nDiag_mwn10m = 106 ! Meridional wind at 10-m from surface layer

  LOGICAL, PUBLIC :: StartStorDiag = .FALSE. ! Start Storage Diagnostic
  INTEGER              :: nMax
  INTEGER              :: mMax
  INTEGER              :: mnMax
  INTEGER, ALLOCATABLE :: ibMaxPerJB(:)
  INTEGER              :: iMaxNew
  INTEGER              :: jMaxNew
  INTEGER              :: kMaxNew
  INTEGER              :: ibMax
  INTEGER              :: jbMax
  INTEGER              :: mxgaus
  INTEGER              :: mxspec
  LOGICAL              :: pfbar
  LOGICAL              :: doprec
  LOGICAL              :: dodyn
  INTEGER              :: mxrq
  INTEGER              :: mgaus ! Number of kMax-Layer Gaussian Diagnostic Fields
  INTEGER              :: ngaus ! Number of    1-Layer Gaussian Diagnostic Fields
  INTEGER              :: mspec ! Number of kMax-Layer Spectral Diagnostic Fields
  INTEGER              :: nspec ! Number of    1-Layer Spectral Diagnostic Fields
  INTEGER              :: ispec
  INTEGER              :: igaus
  INTEGER              :: mxavl
  INTEGER              :: icf
  INTEGER              :: nof
  INTEGER              :: ihdim (2)
  INTEGER, ALLOCATABLE :: krcf  (:)
  INTEGER, ALLOCATABLE :: jrcf  (:)
  INTEGER, ALLOCATABLE :: kravl (:)
  INTEGER, ALLOCATABLE :: ixcf  (:)
  INTEGER, ALLOCATABLE :: incf  (:)
  INTEGER, ALLOCATABLE :: iclcd (:)
  INTEGER, ALLOCATABLE :: ixavl (:)
  INTEGER, ALLOCATABLE :: inavl (:)
  LOGICAL, ALLOCATABLE :: dodia (:)
  INTEGER, ALLOCATABLE :: lvavl (:)
  INTEGER, ALLOCATABLE :: nuavl (:)
  INTEGER, ALLOCATABLE :: itavl (:)
  INTEGER, ALLOCATABLE :: iavrq (:)
  INTEGER, ALLOCATABLE :: nurq  (:)
  INTEGER, ALLOCATABLE :: lvcf  (:)
  INTEGER, ALLOCATABLE :: nucf  (:)
  INTEGER, ALLOCATABLE :: itcf  (:)
  LOGICAL, ALLOCATABLE :: icfu  (:)
  INTEGER, ALLOCATABLE :: ixucf (:)
  INTEGER, ALLOCATABLE :: inucf (:)
  INTEGER, ALLOCATABLE :: jrucf (:)
  CHARACTER(len=40), ALLOCATABLE :: avail (:)
  CHARACTER(len=40), ALLOCATABLE :: reqdg (:)
  CHARACTER(len=40), ALLOCATABLE :: combf (:)
  REAL(KIND=r8)             , ALLOCATABLE :: gaus  (:,:,:)
  REAL(KIND=r8)             , ALLOCATABLE :: gaus_in(:,:,:)
  REAL(KIND=r8)             , ALLOCATABLE :: spec  (:,:)
  INTEGER          , ALLOCATABLE :: lspec (:)
  INTEGER          , ALLOCATABLE :: lgaus (:)
  REAL(KIND=r8) ,             ALLOCATABLE :: dcol  (:)
  REAL(KIND=r8) ,             ALLOCATABLE :: scol  (:)
  INTEGER,           ALLOCATABLE :: lvrq  (:)

CONTAINS


  SUBROUTINE InitDiagnostics (doprec_in, dodyn_in ,     colrad , &
       mMax_in   , nMax_in  , mnMax_in,  iMaxNew_in, jMaxNew_in , &
       kMaxNew_in,ibMax_in  , jbMax_in,  ibMaxPerJB_in, fNameDTable )
    !
    !
    ! indiag :initialize diagnostic database; extended diagnostics version 1;
    !         this routine reads in the available diagnostics table and
    !         the desired diagnostic table; the two are compared and
    !         diagnostic tables are determined; these tables along with the
    !         standard prognostic output table are used to form the output
    !         directory; the actual accumulators are set in subroutine setdia;
    !         available diagnostics table should only be changed by appending;
    !         positions in table are permanently set when determined.
    !
    ! development notes
    !
    !        version 1 of the extended diagnostics system removes data
    !        management of the diagnostic accumulators from the model code
    !        and allows for user selectable diagnostics.  this version will
    !        maintain the diagnostic accumulators in memory and permit the
    !        user to select individual diagnostics or to combine several into
    !        one diagnostic.  only available diagnostics can be selected or
    !        combined.
    !        later versions will allow for the use of solid state disk (ssd),
    !        regular disk, or other media to retain the accumulators.  this
    !        will allow for a large set of diagnostics with a reduced use of
    !        memory.  other changes will include use of dynamic memory and a
    !        user friendly interface.
    !

    LOGICAL, INTENT(IN)    :: doprec_in
    LOGICAL, INTENT(IN)    :: dodyn_in
    INTEGER, INTENT(IN)    :: mMax_in
    INTEGER, INTENT(IN)    :: nMax_in
    INTEGER, INTENT(IN)    :: mnMax_in
    INTEGER, INTENT(IN)    :: iMaxNew_in
    INTEGER, INTENT(IN)    :: jMaxNew_in
    INTEGER, INTENT(IN)    :: kMaxNew_in
    INTEGER, INTENT(IN)    :: ibMax_in
    INTEGER, INTENT(IN)    :: jbMax_in
    INTEGER, INTENT(IN)    :: ibMaxPerJB_in(:)
    REAL(KIND=r8),    INTENT(IN)    :: colrad(jMaxNew_in)
    CHARACTER(len=*), INTENT(IN) :: fNameDTable

    INTEGER, ALLOCATABLE   :: jpavl(:)
    INTEGER, ALLOCATABLE   :: irqav(:)
    INTEGER, ALLOCATABLE   :: irqcf(:)
    LOGICAL, ALLOCATABLE   :: irqu(:)
    INTEGER, ALLOCATABLE   :: kfrq(:)
    INTEGER, ALLOCATABLE   :: jpcf(:)
    CHARACTER(len=40)      :: ocf
    CHARACTER(len= 8)      :: typcd(2)
    CHARACTER(len=38)      :: poscd(0:3)
    INTEGER                :: m
    INTEGER                :: n
    INTEGER                :: j
    INTEGER                :: iac
    INTEGER                :: nn
    INTEGER                :: ix
    INTEGER                :: ja
    INTEGER                :: ia
    INTEGER                :: k1
    INTEGER                :: k2
    INTEGER                :: k3
    INTEGER                :: i1
    INTEGER                :: i2
    INTEGER                :: mm
    INTEGER                :: kk
    INTEGER                :: jx
    INTEGER                :: la
    INTEGER                :: ka
    INTEGER                :: kka
    INTEGER                :: irix
    INTEGER                :: kx
    INTEGER                :: nofp
    INTEGER                :: mx
    INTEGER                :: in
    REAL(KIND=r8)                   :: pie
    REAL(KIND=r8)                   :: colb(jMaxNew_in)
    LOGICAL                :: ExistDTable

    mMax    = mMax_in
    nMax    = nMax_in
    mnMax   = mnMax_in
    iMaxNew = iMaxNew_in
    jMaxNew = jMaxNew_in
    kMaxNew = kMaxNew_in
    ibMax   = ibMax_in
    jbMax   = jbMax_in
    ALLOCATE (ibMaxPerJB(jbMax))
    ibMaxPerJB=ibMaxPerJB_in

    !
    !     avail = name of available diagnostic
    !     lvavl = levels in available diagnostic (1 or kmax)
    !     nuavl = unit code of available diagnostic
    !     itavl = type of available diagnostic (1 gaussian, 2 spectral)
    !     jpavl = position in code of available diagnostic (1 gloop/gfidi,
    !             2 gwater, 3 both, 0 neither)

    doprec = doprec_in
    dodyn  = dodyn_in




    ihdim(1)=iMaxNew*myjmax_d
    ihdim(2)=2*mymnMax

    ALLOCATE(dcol(jMaxNew))
    ALLOCATE(scol(jMaxNew))
    pie = 4.0_r8*ATAN(1.0_r8)
    !
    !     define latitude grid for integration
    !
    colb(1) = 0.0_r8
    DO j = 2, jMaxNew
       colb(j) = 0.5_r8*(colrad(j)+colrad(j-1))
    END DO
    DO j = 1, jMaxNew-1
       dcol(j) = colb(j+1)-colb(j)
    END DO
    dcol(jMaxNew) = pie-colb(jMaxNew)
    DO j = 1, jMaxNew
       scol(j) = SIN(colrad(j))
    END DO

    ALLOCATE(dodia(ndavl))
    ALLOCATE(lvavl(ndavl))
    ALLOCATE(nuavl(ndavl))
    ALLOCATE(itavl(ndavl))
    ALLOCATE(iavrq(ndavl))
    ALLOCATE(ixavl(ndavl))
    ALLOCATE(inavl(ndavl))
    ALLOCATE(nurq (ndrq ))
    ALLOCATE(iclcd(ndrq ))
    ALLOCATE(lvcf (ncdg ))
    ALLOCATE(nucf (ncdg ))
    ALLOCATE(ixcf (ncdg ))
    ALLOCATE(incf (ncdg ))
    ALLOCATE(itcf (ncdg ))
    ALLOCATE(icfu (ncdg ))
    ALLOCATE(ixucf(ncdg ))
    ALLOCATE(inucf(ncdg ))
    ALLOCATE(kravl(jxavl))
    ALLOCATE(krcf (jxcdg))
    ALLOCATE(jrcf (jxcdg))
    ALLOCATE(jrucf(jxcdg))
    ALLOCATE(avail(ndavl))
    ALLOCATE(reqdg(ndrq) )
    ALLOCATE(combf(ncdg) )
    ALLOCATE(lspec(-ncdg:ndavl))
    ALLOCATE(lgaus(-ncdg:ndavl))


    ALLOCATE(jpavl(ndavl))
    ALLOCATE(irqav(ndrq ))
    ALLOCATE(irqcf(ndrq ))
    ALLOCATE(irqu (ndrq ))
    ALLOCATE(lvrq (ndrq ))
    ALLOCATE(kfrq (ncdg ))
    ALLOCATE(jpcf (ncdg ))

    combf=' '
    jpcf=0


    ! field name     !     avail = name of available diagnostic

    avail="                                        "
    avail(1:39)=(/  &
         'TIME MEAN SURFACE PRESSURE              ', &
         'TIME MEAN DIVERGENCE                    ', &
         'TIME MEAN VORTICITY                     ', &
         'TIME MEAN SPECIFIC HUMIDITY             ', &
         'TIME MEAN VIRTUAL TEMPERATURE           ', &
         'TIME MEAN SURFACE TEMPERATURE           ', &
         'TIME MEAN OMEGA                         ', &
         'TIME MEAN SIGMADOT                      ', &
         'TOTAL PRECIPITATION                     ', &
         'CONVECTIVE PRECIPITATION                ', &
         'LARGE SCALE PRECIPITATION               ', &
         'SNOWFALL                                ', &
         'RUNOFF                                  ', &
         'PRECIPITABLE WATER                      ', &
         'INTERCEPTION LOSS                       ', &
         'SENSIBLE HEAT FLUX FROM SURFACE         ', &
         'LATENT HEAT FLUX FROM SURFACE           ', &
         'SURFACE ZONAL WIND STRESS               ', &
         'SURFACE MERIDIONAL WIND STRESS          ', &
         'CLOUD COVER                             ', &
         'DOWNWARD LONG WAVE AT BOTTOM            ', &
         'UPWARD LONG WAVE AT BOTTOM              ', &
         'OUTGOING LONG WAVE AT TOP               ', &
         'INCIDENT SHORT WAVE FLUX                ', &
         'DOWNWARD SHORT WAVE AT GROUND           ', &
         'UPWARD SHORT WAVE AT GROUND             ', &
         'UPWARD SHORT WAVE AT TOP                ', &
         'SHORT WAVE ABSORBED BY EARTH/ATMOSPHERE ', &
         'SHORT WAVE ABSORBED AT GROUND           ', &
         'NET LONG WAVE AT BOTTOM                 ', &
         'LONG WAVE RADIATIVE HEATING             ', &
         'SHORT WAVE RADIATIVE HEATING            ', &
         'CONVECTIVE LATENT HEATING               ', &
         'CONVECTIVE MOISTURE SOURCE              ', &
         'LARGE SCALE LATENT HEATING              ', &
         'LARGE SCALE MOISTURE SOURCE             ', &
         'SHALLOW CONVECTIVE HEATING              ', &
         'SHALLOW CONV. MOISTURE SOURCE           ', &
         'VERTICAL DIFFUSION HEATING              '/)
    avail(40:78)=(/  &
         'VERTICAL DIFF. MOISTURE SOURCE          ', &
         'VERTICAL DIFFUSION DU/DT                ', &
         'VERTICAL DIFFUSION DV/DT                ', &
         'GRAVITY WAVE DRAG SFC ZONAL STRESS      ', &
         'GRAVITY WAVE DRAG SFC MERIDIONAL STRESS ', &
         'GRAVITY WAVE DRAG DU/DT                 ', &
         'GRAVITY WAVE DRAG DV/DT                 ', &
         'HORIZONTAL HEATING DIFFUSION            ', &
         'HORIZONTAL MOISTURE DIFFUSION           ', &
         'HORIZONTAL DIVERGENCE DIFFUSION         ', &
         'HORIZONTAL VORTICITY DIFFUSION          ', &
         'DIVERGENCE * SPECIFIC HUMIDITY          ', &
         'VERTICAL MOISTURE ADVECTION             ', &
         'HORIZ. MOISTURE FLUX CONV.              ', &
         'VERT. INTEGRATED MOISTURE FLUX CONV.    ', &
         'TIME MEAN LOG SURFACE PRESSURE          ', &
         'DOWNWARD LONG WAVE AT BOTTOM (CLEAR)    ', &
         'OUTGOING LONG WAVE AT TOP (CLEAR)       ', &
         'DOWNWARD SHORT WAVE AT GROUND (CLEAR)   ', &
         'UPWARD SHORT WAVE AT GROUND (CLEAR)     ', &
         'UPWARD SHORT WAVE AT TOP (CLEAR)        ', &
         'SHORT WV ABSRBD BY EARTH/ATMOS (CLEAR)  ', &
         'SHORT WAVE ABSORBED AT GROUND (CLEAR)   ', &
         'NET LONG WAVE AT BOTTOM (CLEAR)         ', &
         'TIME MEAN DEEP SOIL TEMPERATURE         ', &
         'GROUND/SURFACE COVER TEMPERATURE        ', &
         'CANOPY TEMPERATURE                      ', &
         'TEMPERATURE OF CANOPY AIR SPACE         ', &
         'VAPOR PRESSURE OF CANOPY AIR SPACE      ', &
         'BARE SOIL LATENT HEAT                   ', &
         'NEG. HUM. CORR. MOISTURE SOURCE         ', &
         'OZONE MIXING RATIO                      ', &
         'VERTICAL DIST TOTAL CLOUD COVER         ', &
         'INVERSION CLOUD                         ', &
         'SUPERSATURATION CLOUD                   ', &
         'CONVECTIVE CLOUD                        ', &
         'SHALLOW CONVECTIVE CLOUD                ', &
         'CLOUD LIQUID WATER PATH                 ', &
         'LONGWAVE CLOUD EMISSIVITY               '/)
    avail(79:ndavl)=(/  &
         'SHORTWAVE CLOUD OPTICAL DEPTH           ', &
         'CANOPY AIR SPC TO REF. LVL RESISTANCE   ', &
         'CANOPY AIR SPC TO CANOPY RESISTANCE     ', &
         'CANOPY AIR SPC TO GROUND RESISTANCE     ', &
         'CANOPY RESISTANCE                       ', &
         'GROUND COVER RESISTANCE                 ', &
         'BARE SOIL SURFACE RESISTANCE            ', &
         'HORIZONTAL MOMENTUM TRANSPORT           ', &
         'VERTICAL ZONAL MOMENTUM TRANSPORT       ', &
         'VERTICAL MERIDIONAL MOMENTUM TRANSPORT  ', &
         'MERIDIONAL SENSIBLE HEAT TRANSPORT      ', &
         'ZONAL SENSIBLE HEAT TRANSPORT           ', &
         'VERTICAL SENSIBLE HEAT TRANSPORT        ', &
         'MERIDIONAL SPECIFIC HUMIDITY TRANSPORT  ', &
         'ZONAL SPECIFIC HUMIDITY TRANSPORT       ', &
         'VERTICAL SPECIFIC HUMIDITY TRANSPORT    ', &
         'LONG WAVE RADIATIVE HEATING (CLEAR)     ', & !hmjb
         'SHORT WAVE RADIATIVE HEATING (CLEAR)    ', & !hmjb
         'TIME MEAN TEMP AT 2-M FROM SFC          ', &
         'TIME MEAN SPEC HUMIDITY AT 2-M FROM SFC ', &
         'SPEED WIND AT 2-M FROM SURFACE          ', &
         'TIME MEAN TEMP AT 10-M FROM SFC         ', &
         'TIME MEAN SPEC HUMIDITY AT 10-M FROM SFC', &
         'SPEED WIND AT 10-M FROM SURFACE         ', &
         'VERTICALLY INTEGRATED OZONE CONTENT     ', &!hmjb
         'DEW POINT TEMPERATURE                   ', &
         'TIME MEAN AT 10 METRE U-WIND COMPONENT  ', &
         'TIME MEAN AT 10 METRE V-WIND COMPONENT  '/)

    !     lvavl = levels in available diagnostic (1 or 2=kmax)

    lvavl(1:ndavl)=(/ &
         1,    2,    2,    2,    2,    1,    2,    2,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
         2,    2,    2,    2,    2,    2,    2,    2,    2,    2, &
         2,    2,    1,    1,    2,    2,    2,    2,    2,    2, &
         2,    2,    2,    1,    1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    2, &
         2,    2,    2,    2,    2,    2,    2,    2,    2,    1, &
         1,    1,    1,    1,    1,    2,    2,    2,    2,    2, &
         2,    2,    2,    2,    2,    2,    1,    1,    1,    1, &
         1,    1,    1,    2,    1,    1/)


    !     nuavl = unit code of available diagnostic

    nuavl(1:ndavl)=(/ &
         132,  50,  50,   0,  40,  40, 153,  50, 120, 120, &
         120, 120, 120, 110, 170, 170, 170, 130, 130,   0, &
         170, 170, 170, 170, 170, 170, 170, 170, 170, 170, &
         70,  70,  70,  50,  70,  50,  70,  50,  70,  50, &
         100, 100, 130, 130, 100, 100,  70,  50,  80,  80, &
         50,  50,  50, 120, 142, 170, 170, 170, 170, 170, &
         170, 170, 170,  40,  40,  40,  40, 131, 170,  50, &
         0,   0,   0,   0,   0,   0,   0,   0,   0, 190, &
         190, 190, 190, 190, 190, 180, 252, 252, 230, 230, &
         242,  60,  60, 153,  70,  70,  40,   0,  60,  40, &
         0,  60,   0,  40,  60,  60/)

    !     itavl = type of available diagnostic (1 gaussian, 2 spectral)

    itavl(1:ndavl)=(/ &
         1,    2,    2,    2,    2,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1,    2,    2,    2,    2, &
         1,    1,    2,    2,    2,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1/)

    !     jpavl = position in code of available diagnostic (1 gloop/gfidi,
    !             2 gwater, 3 both, 0 neither)

    jpavl(1:ndavl)=(/ &
         0,    0,    0,    0,   0,    1,    1,    1,    2,    2, &
         2,    2,    1,    1,   1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,   1,    1,    1,    1,    1,    1, &
         1,    1,    2,    2,   2,    2,    2,    2,    1,    1, &
         1,    1,    1,    1,   1,    1,    0,    0,    0,    0, &
         1,    1,    0,    0,   0,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,   1,    1,    1,    1,    1,    2, &
         1,    1,    1,    1,   1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,   1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,   1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,   1,    1/)

    IF(nfctrl(51).GE.1)WRITE(UNIT=nfprt,FMT=30)
    mxavl=0
    DO m=1,ndavl
       IF (avail(m)(1:5) .NE. "     ") THEN
          IF (lvavl(m) .NE. 1) lvavl(m)=kMaxNew
          IF(nfctrl(51).GE.1)WRITE(UNIT=nfprt,FMT=60)avail(m),lvavl(m),nuavl(m), &
               itavl(m),jpavl(m)
          dodia(m)=.FALSE.
          iavrq(m)=0
          mxavl=mxavl+1
       END IF
    END DO

    IF(mxavl.LE.0)THEN
       WRITE(UNIT=nfprt,FMT=6600)
       WRITE(UNIT=nferr,FMT=6600)
       STOP 6600
    END IF

    !     reqdg = name of requested diagnostic
    reqdg="                                        "
    reqdg(1:20)=(/ &
         "TOTAL PRECIPITATION                     ", &
         "CONVECTIVE PRECIPITATION                ", &
         "LARGE SCALE PRECIPITATION               ", &
         "SNOWFALL                                ", &
         "RUNOFF                                  ", &
         "INTERCEPTION LOSS                       ", &
         "SENSIBLE HEAT FLUX FROM SURFACE         ", &
         "LATENT HEAT FLUX FROM SURFACE           ", &
         "SURFACE ZONAL WIND STRESS               ", &
         "SURFACE MERIDIONAL WIND STRESS          ", &
         "CLOUD COVER                             ", &
         "DOWNWARD LONG WAVE AT BOTTOM            ", &
         "UPWARD LONG WAVE AT BOTTOM              ", &
         "OUTGOING LONG WAVE AT TOP               ", &
         "DOWNWARD SHORT WAVE AT GROUND           ", &
         "UPWARD SHORT WAVE AT GROUND             ", &
         "UPWARD SHORT WAVE AT TOP                ", &
         "SHORT WAVE ABSORBED AT GROUND           ", &
         "NET LONG WAVE AT BOTTOM                 ", &
         "GROUND/SURFACE COVER TEMPERATURE        "/)

    !     lvrq  = levels in requested diagnostic (1 or kmax)

    lvrq(1:20)=(/ &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    1 /)

    !     nurq  = unit code of requested diagnostic

    nurq(1:20)=(/ &
         121,  121,  121,  121,  121,  170,  170,  170,  130,  130, &
         0,  170,  170,  170,  170,  170,  170,  170,  170,   40/)

    !     iclcd = requested diagnostic calculation code (0 direct
    !             calculation, > 0 add to requested field number iclcd,
    !             < 0 subtract from requested field number -iclcd )

    iclcd(1:20)=(/ &
         0,    0,    0,    0,    0,    0,    0,    0,    0,    0, &
         0,    0,    0,    0,    0,    0,    0,    0,    0,    0/)

    IF(nfctrl(51).GE.1)WRITE(UNIT=nfprt,FMT=130)
    INQUIRE (file=TRIM(fNameDTable), EXIST=ExistDTable)
    IF (ExistDTable) THEN
       mgaus = 0
       ngaus = 0
       mspec = 0
       nspec = 0
       OPEN(UNIT=nfdestbl, FILE=TRIM(fNameDTable),FORM='formatted',ACCESS='sequential',&
            ACTION='read', STATUS='old', IOSTAT=ierr)
       IF (ierr /= 0) THEN
          WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
               TRIM(fNameDTable), ierr
          STOP "**(ERROR)**"
       END IF
!!$       WRITE (UNIT=nfprt,FMT='(/," Using User Required Fields",/)')
       mxrq=1
       DO n=1,ndrq
          READ(UNIT=nfdestbl,FMT='(A40,3I5)',END=225)reqdg(n),lvrq(n),nurq(n),iclcd(n)
          IF (reqdg(n)(1:5) .NE. "     ")THEN
             IF (lvrq(n) .NE. 1) lvrq(n)=kMaxNew
             IF(nfctrl(51).GE.1)WRITE(UNIT=nfprt,FMT=160)reqdg(n),lvrq(n),nurq(n), &
                  iclcd(n)
             irqcf(n)=0
             irqav(n)=0
             irqu(n)=.FALSE.
             mxrq=mxrq+1
             DO nn=1,ndavl
                IF (reqdg(n) == avail(nn)) THEN
                   IF (lvavl(nn) ==       1 .AND. itavl(nn) == 1) ngaus=ngaus+1
                   IF (lvavl(nn) == kMaxNew .AND. itavl(nn) == 1) mgaus=mgaus+1
                   IF (lvavl(nn) ==       1 .AND. itavl(nn) == 2) nspec=nspec+1
                   IF (lvavl(nn) == kMaxNew .AND. itavl(nn) == 2) mspec=mspec+1
                   EXIT
                END IF
             END DO
          END IF
       END DO
225    mxrq=n-1
       CLOSE(UNIT=nfdestbl,STATUS='KEEP')
    ELSE
!!$       WRITE(UNIT=nfprt,FMT='(/," Using Default Required Fields",/)')
       mgaus = 1
       ngaus = 21
       mspec = 4
       nspec = 0
       mxrq=20
!!$       PRINT*,'* The  ', TRIM(fNameDTable), ' file does not exist*'
       DO n=1,mxrq
          IF (lvrq(n) .NE. 1) lvrq(n)=kMaxNew
          IF(nfctrl(51).GE.1)WRITE(UNIT=nfprt,FMT=160)reqdg(n),lvrq(n),nurq(n),iclcd(n)
          irqcf(n)=0
          irqav(n)=0
          irqu(n)=.FALSE.
       END DO
    END IF
    IF(mxrq.LE.0)THEN
       WRITE(UNIT=nfprt,FMT=7100)
       WRITE(UNIT=nferr,FMT=7100)
       STOP 7100
    END IF
!!$    WRITE(UNIT=nfprt, FMT='(/," Nr. of Gaussian    1-Level  Fields:",I5)'  ) ngaus
!!$    WRITE(UNIT=nfprt, FMT='(  " Nr. of Gaussian Kmax-Levels Fields:",I5)'  ) mgaus
!!$    WRITE(UNIT=nfprt, FMT='(  " Nr. of Spectral    1-Level  Fields:",I5)'  ) nspec
!!$    WRITE(UNIT=nfprt, FMT='(  " Nr. of Spectral Kmax-Levels Fields:",I5,/)') mspec

    typcd(1)='GAUSSIAN'
    typcd(2)='SPECTRAL'
    poscd(0)='NOT COMPUTED IN EITHER GFIDI OR GWATER'
    poscd(1)='COMPUTED ONLY IN GFIDI                '
    poscd(2)='COMPUTED ONLY IN GWATER               '
    poscd(3)='COMPUTED IN BOTH GFIDI AND GWATER     '
    !     search for combined field components.  save as combined fields
    !     those fields which have at least one component.  mark desired
    !     field refered by component as a valid combined field
    !     (irqcf=-icf).  indicate combined field for component (irqcf=icf).
    nof=999999
    icf=0

    DO n=1,mxrq

       IF(nof.EQ.999999)THEN
          IF(iclcd(n).NE.0)nof=n-1
       END IF

       IF(nof.NE.999999)THEN

          IF(iclcd(n).EQ.0)THEN
             WRITE(UNIT=nfprt,FMT=2100)n,nof
             WRITE(UNIT=nferr,FMT=2100)n,nof
             STOP 2100
          END IF

          iac=iabs(iclcd(n))

          IF(iac.GE.n)THEN
             WRITE(UNIT=nfprt,FMT=2600)iac,n
             WRITE(UNIT=nferr,FMT=2600)iac,n
             STOP 2600
          END IF

          IF(iclcd(iac).NE.0)THEN
             WRITE(UNIT=nfprt,FMT=3100)iac,n,iclcd(iac)
             WRITE(UNIT=nferr,FMT=3100)iac,n,iclcd(iac)
             STOP 3100
          END IF

          IF(reqdg(iac).EQ.reqdg(n))THEN
             WRITE(UNIT=nfprt,FMT=3600)iac,n,reqdg(n)
             WRITE(UNIT=nferr,FMT=3600)iac,n,reqdg(n)
             STOP 3600
          END IF

          irqu(n)=.TRUE.

          IF(icf.NE.0)THEN
             DO ix=1,icf
                IF(reqdg(iac).EQ.combf(ix))go to 270
             END DO
          END IF

          icf=icf+1
          combf(icf)=reqdg(iac)
          lvcf(icf)=lvrq(iac)
          nucf(icf)=nurq(iac)
          kfrq(icf)=iac
          irqcf(iac)=-icf
          irqu(iac)=.TRUE.
270       irqcf(n)=icf
       END IF

    END DO

    IF(nof.EQ.999999) nof=mxrq
    !     find available diagnostics corresponding to desired diagnostic.
    !     first, find directly available desired diagnostic
    DO nn=1,nof

       IF(irqcf(nn).NE.0)CYCLE

       DO m=1,mxavl
          IF(reqdg(nn).EQ.avail(m))go to 360
       END DO

       WRITE(UNIT=nfprt,FMT=4100)nn,reqdg(nn)
       WRITE(UNIT=nferr,FMT=4100)nn,reqdg(nn)
       STOP 4100

360    irqu(nn)=.TRUE.
       dodia(m)=.TRUE.
       irqav(nn)=m
       iavrq(m)=nn

    END DO
    !     second, find available diagnostic components for combined fields
    !     or find other combined fields used as components.  save
    !     component index (+ for a.d., - for c.f.)
    IF(nof.LT.mxrq)THEN
       nofp=nof+1
       DO nn=nofp,mxrq

          DO m=1,mxavl
             IF(reqdg(nn).EQ.avail(m))go to 480
          END DO

          IF(icf.NE.0)THEN
             DO ix=1,icf
                IF(reqdg(nn).EQ.combf(ix))go to 490
             END DO
          END IF

          WRITE(UNIT=nfprt,FMT=4600)nn,reqdg(nn)
          WRITE(UNIT=nferr,FMT=4600)nn,reqdg(nn)
          STOP 4600

480       dodia(m)=.TRUE.
          irqav(nn)=m
          go to 495

490       irqav(nn)=-ix
495       irqu(nn)=.TRUE.

       END DO
    END IF

    !     check to make sure all desired diagnostics are used

    DO n=1,mxrq
       IF(.NOT.irqu(n))THEN

          WRITE(UNIT=nfprt,FMT=5100)n,reqdg(n)
          WRITE(UNIT=nferr,FMT=5100)n,reqdg(n)
          STOP 5100

       END IF
    END DO

    !     find all components for each combined field

    IF(icf.NE.0)THEN
       ja=1
       DO ix=1,icf
          ixcf(ix)=ja
          itcf(ix)=0
          ia=0
          k1=0
          k2=0
          k3=0
          i1=0
          i2=0
          DO n=1,mxrq

             IF(irqcf(n).NE.ix)CYCLE
             ia=ia+1
             jrcf(ja)=n

             IF(irqav(n).GT.0)THEN

                !     case for available diagnostic component

                mm=irqav(n)

                IF(nuavl(mm)/10.NE.nucf(ix)/10)THEN
                   WRITE(UNIT=nfprt,FMT=9100)nuavl(mm),mm,nucf(ix),ix
                   WRITE(UNIT=nferr,FMT=9100)nuavl(mm),mm,nucf(ix),ix
                   STOP 9100
                END IF

                krcf(ja)=mm
                IF(jpavl(mm).EQ.1)k1=k1+1
                IF(jpavl(mm).EQ.2)k2=k2+1
                IF(jpavl(mm).EQ.3)k3=k3+1
                IF(itavl(mm).EQ.1)i1=i1+1
                IF(itavl(mm).EQ.2)i2=i2+1

             ELSE IF(irqav(n).LT.0)THEN

                !     case for combined field component
                kk=-irqav(n)

                IF(nucf(ix).NE.nucf(kk))THEN
                   WRITE(UNIT=nfprt,FMT=9600)nucf(kk),kk,nucf(ix),ix
                   WRITE(UNIT=nferr,FMT=9600)nucf(kk),kk,nucf(ix),ix
                   STOP 9600
                END IF

                krcf(ja)=irqav(n)
                IF(jpcf(kk).EQ.1)k1=k1+1
                IF(jpcf(kk).EQ.2)k2=k2+1
                IF(jpcf(kk).EQ.3)k3=k3+1
                IF(itcf(kk).EQ.1)i1=i1+1
                IF(itcf(kk).EQ.2)i2=i2+1

             END IF

             ja=ja+1

             IF(ja.GT.jxcdg)THEN
                WRITE(UNIT=nfprt,FMT=5600)ix,n
                WRITE(UNIT=nferr,FMT=5600)ix,n
                STOP 5600
             END IF

          END DO

          incf(ix)=ia

          IF(k3.NE.0)THEN
             jpcf(ix)=3
          ELSE IF(k2.NE.0.AND.k1.NE.0)THEN
             jpcf(ix)=3
          ELSE IF(k2.NE.0)THEN
             jpcf(ix)=2
          ELSE IF(k1.NE.0)THEN
             jpcf(ix)=1
          ELSE
             jpcf(ix)=0
          END IF

          IF(i1.NE.0.AND.i2.NE.0)THEN
             WRITE(UNIT=nfprt,FMT=7600)ix,combf(ix),(krcf(mx),mx=ixcf(ix),ja-1)
             WRITE(UNIT=nferr,FMT=7600)ix,combf(ix),(krcf(mx),mx=ixcf(ix),ja-1)
             STOP 7600
          END IF

          IF(i1.NE.0)itcf(ix)=1
          IF(i2.NE.0)itcf(ix)=2

       END DO
    END IF

    !     determine all available diagnostic uses

    ja=1
    DO m=1,mxavl

       IF(.NOT.dodia(m))CYCLE

       ixavl(m)=ja
       ia=0

       IF(iavrq(m).GT.0)THEN
          ia=ia+1
          kravl(ja)=iavrq(m)
          ja=ja+1

          IF(ja.GT.jxavl)THEN
             WRITE(UNIT=nfprt,FMT=6100)m,ix
             WRITE(UNIT=nferr,FMT=6100)m,ix
             STOP 6100
          END IF

       END IF

       IF(icf.NE.0)THEN
          DO ix=1,icf
             ka=ixcf(ix)
             DO in=1,incf(ix)
                IF(krcf(ka).EQ.m)THEN
                   ia=ia+1
                   kravl(ja)=-ix
                   ja=ja+1
                   IF(ja.GT.jxavl)THEN
                      WRITE(UNIT=nfprt,FMT=6100)m,ix
                      WRITE(UNIT=nferr,FMT=6100)m,ix
                      STOP 6100
                   END IF
                END IF
                ka=ka+1
             END DO
          END DO
       END IF

       inavl(m)=ia
    END DO
    !     find combined fields requiring given combined field component
    la=1
    DO ix=1,icf
       icfu(ix)=.FALSE.
       ixucf(ix)=0
       ia=0
       DO jx=1,icf
          ka=ixcf(jx)
          DO in=1,incf(jx)
             IF(krcf(ka).EQ.-ix)go to 840
             ka=ka+1
          END DO
          CYCLE
840       IF(jx.LE.ix)THEN
             WRITE(UNIT=nfprt,FMT=8600)jx,ix,jrcf(jx),jrcf(ix)
             WRITE(UNIT=nferr,FMT=8600)jx,ix,jrcf(jx),jrcf(ix)
             STOP 8600
          END IF
          IF(.NOT.icfu(ix))THEN
             icfu(ix)=.TRUE.
             ixucf(ix)=la
          END IF
          IF(iclcd(jrcf(jx)).GT.0)THEN
             jrucf(la)=jx
          ELSE
             jrucf(la)=-jx
          END IF
          ia=ia+1
          la=la+1
          IF(la.GT.jxcdg)THEN
             WRITE(UNIT=nfprt,FMT=8100)jx,ix
             WRITE(UNIT=nferr,FMT=8100)jx,ix
             STOP 8100
          END IF
       END DO
       inucf(ix)=ia
    END DO

    IF(nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1410)
    mm=0
    DO m=1,mxavl
       IF(.NOT.dodia(m))CYCLE
       mm=mm+1
       IF(nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1420)mm,m,avail(m)
       IF(nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1430)lvavl(m),aunits(nuavl(m)), &
            typcd(itavl(m)),poscd(jpavl(m))
       IF(iavrq(m).NE.0.AND.nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1440)iavrq(m)
       IF(kravl(ixavl(m)).LT.0.OR.inavl(m).GT.1)THEN
          ka=ixavl(m)
          kka=ka+inavl(m)-1
          IF(kravl(ka).GT.0)ka=ka+1
          IF(nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1450)
          DO kk=ka,kka
             IF(nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1460)combf(iabs(kravl(kk)))
          END DO
       END IF
    END DO
    IF(nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1510)
    DO n=1,mxrq
       IF(nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1520)n,reqdg(n)
       IF(nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1530)lvrq(n),aunits(nurq(n)), &
            irqcf(n)
       IF(irqav(n).GT.0)THEN
          IF(iclcd(n) < 0 ) THEN
             IF(nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1545)irqav(n),reqdg(iabs(iclcd(n)))
          ELSE IF (iclcd(n) .EQ. 0 ) THEN
             IF(nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1555)irqav(n)
          ELSE
             IF(nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1565)irqav(n),reqdg(iclcd(n))
          END IF
       ELSE IF(irqav(n).LT.0)THEN
          irix=iabs(irqav(n))
          IF(iclcd(n).LT.0)THEN
             IF(nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1575)irix,reqdg(iabs(iclcd(n)))
          ELSE
             IF(nfctrl(51).GE.2)WRITE(UNIT=nfprt,FMT=1595)irix,reqdg(iclcd(n))
          END IF
       END IF
    END DO
    IF(icf.NE.0)THEN
       IF(nfctrl(51).GE.3)WRITE(UNIT=nfprt,FMT=1610)
       DO ix=1,icf
          IF(nfctrl(51).GE.3)WRITE(UNIT=nfprt,FMT=1620)ix,combf(ix)
          IF(nfctrl(51).GE.3)WRITE(UNIT=nfprt,FMT=1630)lvcf(ix),aunits(nucf(ix)), &
               kfrq(ix),poscd(jpcf(ix))
          IF(nfctrl(51).GE.3)WRITE(UNIT=nfprt,FMT=1640)combf(ix)
          ka=ixcf(ix)
          DO kk=ka,incf(ix)+ka-1
             kx=krcf(kk)
             IF(kx.GT.0)ocf=avail(kx)
             IF(kx.LT.0)ocf=combf(-kx)
             jx=jrcf(kk)
             IF(iclcd(jx).LT.0)THEN
                IF(nfctrl(51).GE.3)WRITE(UNIT=nfprt,FMT=1650)ocf,kx,jx
             ELSE IF(iclcd(jx).GT.0)THEN
                IF(nfctrl(51).GE.3)WRITE(UNIT=nfprt,FMT=1660)ocf,kx,jx
             END IF
          END DO
          IF(icfu(ix))THEN
             IF(nfctrl(51).GE.3)WRITE(UNIT=nfprt,FMT=1670)
             ka=ixucf(ix)
             DO kk=ka,inucf(ix)+ka-1
                kx=iabs(jrucf(kk))
                IF(nfctrl(51).GE.3)WRITE(UNIT=nfprt,FMT=1680)combf(kx)
             END DO
          END IF
       END DO
    END IF

    !     set diagnostic accumulators

    CALL setdia()

    pfbar=iavrq(1).NE.0.OR.iavrq(2).NE.0.OR.iavrq(3).NE.0.OR. &
         iavrq(4).NE.0.OR.iavrq(5).NE.0

30  FORMAT(//'0AVAILABLE DIAGNOSTIC INPUT DECK'/)
60  FORMAT(' ',A40,4I5)
130 FORMAT(//'0DESIRED DIAGNOSTIC INPUT DECK'/)
160 FORMAT(' ',A40,3I5)
1410 FORMAT(' A V A I L A B L E  D I A G N O S T I C S  U S E D  I N  T H I S  R U N')
1420 FORMAT(' DIAG. NO.',I3,' AVAILABLE DIAG. NO.',I3,' NAME = ',A40)
1430 FORMAT(' NUMBER OF LEVELS=',I2,' UNITS: ',A16/' TYPE=',A8,1X,A38)
1440 FORMAT(' REQUESTED DIRECTLY BY DESIRED DIAGNOSTIC NUMBER',I3)
1450 FORMAT(' USED IN THE FOLLOWING COMBINED FIELDS:')
1460 FORMAT(' ',A40)
1510 FORMAT(' D E S I R E D  D I A G N O S T I C S  T A B L E')
1520 FORMAT(' DESIRED DIAGNOSTIC NUMBER',I3,' NAME = ',A40)
1530 FORMAT(' NO. OF LVLS=',I2,' UNITS: ',A16, &
         ' COMBINED FIELD CODE=',I3)
1545 FORMAT(' IS AVAILABLE DIAGNOSTIC NUMBER',I3, &
         ' AND IS SUBTRACTED TO FORM COMBINED FIELD: ',A40)
1555 FORMAT(' IS AVAILABLE DIAGNOSTIC NUMBER',I3,' SAVED DIRECTLY')
1565 FORMAT(' IS AVAILABLE DIAGNOSTIC NUMBER',I3, &
         ' AND IS ADDED TO FORM COMBINED FIELD: ',A40)
1575 FORMAT(' IS COMBINED FIELD NUMBER',I3, &
         ' AND IS SUBTRACTED TO FORM COMBINED FIELD: ',A40)
1595 FORMAT(' IS COMBINED FIELD NUMBER',I3, &
         ' AND IS ADDED TO FORM COMBINED FIELD: ',A40)
1610 FORMAT('1C O M B I N E D  F I E L D  T A B L E')
1620 FORMAT('0COMBINED FIELD NUMBER',I3,' NAME = ',A40)
1630 FORMAT(' NUMBER OF LEVELS=',I2,' UNITS: ',A16/ &
         ' CORRESP. DESIRED DIAG. NO.',I3,1X,A38)
1640 FORMAT(' IS CONSTRUCTED AS FOLLOWS:'/'   ',A40,'=')
1650 FORMAT(' - ',A40,' ( A.D. OR C.F. NO. =',I3,' D.D. NO. =',I3,' )')
1660 FORMAT(' + ',A40,' ( A.D. OR C.F. NO. =',I3,' D.D. NO. =',I3,' )')
1670 FORMAT(' IS USED AS A COMPONENT IN THE FOLLOWING COMBINED FIELDS')
1680 FORMAT(' ',A40)
2100 FORMAT(' DESIRED DIAGNOSTIC DECK NOT WELL ORDERED.'/ &
         ' THE CALCULATION CODE IS ZERO FOR N=',I3, &
         ' WHICH EXCEEDS THE EXPECTED OUTPUT FIELD COUNT =',I3)
2600 FORMAT(' DESIRED DIAGNOSTIC DECK NOT WELL ORDERED.'/ &
         ' THE CALCULATION CODE IS ',I3, &
         ' WHICH EXCEEDS THE CURRENT FIELD COUNT =',I3)
3100 FORMAT(' DESIRED DIAGNOSTIC DECK NOT WELL ORDERED.'/ &
         ' THE CALCULATION CODE IS ',I3,' FOR N=',I3, &
         ' POINTS TO A FIELD WITH NONZERO CALCULATION CODE =',I3)
3600 FORMAT(' DESIRED DIAGNOSTIC DECK NOT WELL ORDERED.',/, &
         ' A FIELD CANNOT BE COMBINED WITH ITSELF.',/, &
         ' THE CALCULATION CODE ',I3,' AND N=',I3, ' POINT TO THE SAME FIELD =',/,'  ',A40)
4100 FORMAT(' DESIRED DIAGNOSTIC FIELD NUMBER',I3,' NAMED ',A40/ &
         ' CANNOT BE FOUND IN AVAILABLE DIAGNOSTICS AND IS NOT REFERENCED AS A COMBINED FIELD')
4600 FORMAT(' DESIRED DIAGNOSTIC FIELD NUMBER',I3,' NAMED ',A40/ &
         ' CANNOT BE FOUND IN AVAILABLE DIAGNOSTICS AND DOES NOT REFERENCE A VALID COMBINED FIELD')
5100 FORMAT(' DESIRED DIAGNOSTIC FIELD NUMBER',I3,' NAMED ',A40/ &
         ' IS NOT USED ANYWHERE')
5600 FORMAT(' COMBINED FIELD COMPONENT TABLE EXCEEDED FOR FIELD NO.', &
         I3,' AND DESIRED DIAGNOSTIC NUMBER',I3)
6100 FORMAT(' AVAILABLE DIAGNOSTIC USE TABLE EXCEEDED FOR NO.', &
         I3,' AND COMBINED FIELD NUMBER',I3)
6600 FORMAT(' AVAILABLE DIAGNOSTIC TABLE EMPTY OR NOT FOUND')
7100 FORMAT(' DESIRED DIAGNOSTIC TABLE EMPTY OR NOT FOUND')
7600 FORMAT(' TYPE CODES FOR COMBINED FIELD COMPONENTS ARE INCONSISTENT.',/,&
         ' COMBINED FIELD NO.=',I3,' NAME=',A40 / &
         ' AVAILABLE DIAGNOSTIC NO.=',(' ',10I5/))
8100 FORMAT(' COMBINED FIELD USE TABLE EXCEEDED FOR FIELD NO.', &
         I3,' AND COMBINED FIELD NUMBER',I3)
8600 FORMAT(' DESIRED DIAGNOSTIC DECK NOT WELL ORDERED.'/ &
         ' THE COMBINED FIELD',I3,' < COMBINED FIELD COMPONENT',I3/ &
         ' WHICH ARE DESIRED DIAGNOSTICS',I3,' AND',I3)
9100 FORMAT(' UNIT CODE GROUP FOR UNIT CODE',I4, &
         ' OF AVAILABLE DIAGNOSTIC COMPONENT',I3/ &
         ' IS NOT THE SAME CODE GROUP FOR UNIT CODE',I4, &
         ' OF COMBINED FIELD',I3)
9600 FORMAT(' THE UNIT CODE,',I4, &
         ', FOR COMBINED FIELD COMPONENT',I3/ &
         ' IS NOT THE SAME UNIT CODE,',I4, &
         ', FOR COMBINED FIELD',I3)

  END SUBROUTINE InitDiagnostics


  ! pwater :perfoms vertical integration of water vapour.


  SUBROUTINE pwater(q, pwtr, ps, del, nx,mx, nq)
    !
    !
    !  nx......imx=imax+1 or imax+2   :this dimension instead of imax
    !          is used in order to avoid bank conflict of memory
    !          access in fft computation and make it efficient. the
    !          choice of 1 or 2 depends on the number of banks and
    !          the declared type of grid variable (real*4,real*8)
    !          to be fourier transformed.
    !          cyber machine has the symptom.
    !          cray machine has no bank conflict, but the argument
    !          'imx' in subr. fft991 cannot be replaced by imax
    !  nq......Number of sigma levels
    !  q.......gq        specific humidity (fourier).
    !          gqu       u*q
    !          gqv       v*q
    !  pwtr
    !  ps......gpphi(imx)            input : latitudinal derivative of
    !                                        natural ig of surface
    !                                        pressure (fourier)
    !  del.....sigma spacing for each layer computed in routine "setsig".
    !  grav....grav   gravity constant        (m/s**2)
    !
    INTEGER, INTENT(IN   ) :: nx
    INTEGER, INTENT(IN   ) :: mx
    INTEGER, INTENT(IN   ) :: nq
    REAL(KIND=r8),    INTENT(IN   ) :: q(nx,nq)
    REAL(KIND=r8),    INTENT(INOUT) :: pwtr(nx)
    REAL(KIND=r8),    INTENT(IN   ) :: ps(nx)
    REAL(KIND=r8),    INTENT(IN   ) :: del(nq)

    REAL(KIND=r8)    :: fac
    INTEGER :: i
    INTEGER :: k

    fac = 1.0e3_r8/grav

    DO i = 1,mx
       pwtr(i) = 0.0_r8
    END DO

    DO k = 1,nq
       DO i = 1,mx
          pwtr(i) = pwtr(i) + q(i,k)*del(k)
       END DO
    END DO

    DO i=1,mx
       pwtr(i) = pwtr(i) * ps(i) * fac
    END DO
  END SUBROUTINE pwater



  !     hbartr : calculates the covariance of two
  !              global fields from their spectral representations.
  !              used to calculate standard deviations of global fields.
  !              note : global means are subtracted before
  !              covariance is calculated.



  SUBROUTINE hbartr (f, fgbar)
    !
    !
    !
    !***********************************************************************
    !
    !    argument(dimensions)                       description
    !
    !        f(2,mnwv0)               input : spectral representation of a
    !                                         global field.
    !
    !         fgbar                  output : zonally a / symmetric product
    !                                         of the two global fields.
    !
    !***********************************************************************
    !
    !
    !
    REAL(KIND=r8),    INTENT(IN ) :: f(2*mymnMax)
    REAL(KIND=r8),    INTENT(OUT) :: fgbar(myMMax)
    INTEGER :: l, mn
    fgbar = 0.0_r8
    mn = 1
    IF (HaveM1) THEN
       DO l = 2,mMax
          fgbar(1)=fgbar(1) + f(2*l-1)*f(2*l-1)*0.5_r8
       END DO
       mn = mMax+1
    ENDIF
    DO l = mn,mymnMax
       fgbar(myMMap(l))=fgbar(myMMap(l)) + f(2*l-1)*f(2*l-1)+f(2*l)*f(2*l)
    END DO
  END SUBROUTINE hbartr





  SUBROUTINE hbartr2D (f, fgbar)
    REAL(KIND=r8),    INTENT(IN ) :: f(2*mymnMax, kMaxloc)
    REAL(KIND=r8),    INTENT(OUT) :: fgbar(myMMax,kMaxloc)
    INTEGER :: k, l, mn
    fgbar = 0.0_r8
    mn = 1
    IF (HaveM1) THEN
       DO k = 1, kMaxloc
          DO l = 2,mMax
             fgbar(1,k)=fgbar(1,k) + f(2*l-1,k)*f(2*l-1,k)*0.5_r8
          END DO
       END DO
       mn = mMax+1
    ENDIF
    DO k = 1, kMaxloc
       DO l = mn,mymnMax
          fgbar(myMMap(l),k)=fgbar(myMMap(l),k) + f(2*l-1,k)*f(2*l-1,k)+f(2*l,k)*f(2*l,k)
       END DO
    END DO
  END SUBROUTINE hbartr2D



  SUBROUTINE setdia()
    !
    ! setdia :extended diagnostics version 1 used for
    !         initializing and partitioning the diagnostic accumulators;
    !         this version is the memory resident version;
    !         see subroutine indiag for further discussion.
    !
    INTEGER :: nf
    INTEGER :: ix
    INTEGER :: m
    INTEGER :: nw
    INTEGER :: ish
    INTEGER :: igh

    mxgaus=mgaus*kMaxNew+ngaus
    mxspec=mspec*kMaxloc+nspec

    ALLOCATE (spec(2*mymnMax, mxspec))
    spec = 0.0_r8

    ALLOCATE (gaus(ibMax, mxgaus, jbMax))
    gaus = 0.0_r8

    ALLOCATE (gaus_in(iMaxNew, myjmax_d, mxgaus))
    gaus_in = 0.0_r8

    ispec=0
    igaus=0
    nf=0
    ix=0
    DO m=1,mxavl
       lspec(m)=0
       lgaus(m)=0
       IF(.NOT.dodia(m))THEN
          CONTINUE
       ELSE IF(iavrq(m).LE.0)THEN
          CONTINUE
       ELSE IF(itavl(m).EQ.1)THEN
          nf=nf+1
          lgaus(m)=igaus+1
          igaus=igaus+lvavl(m)
          IF(igaus.GT.mxgaus)THEN
             WRITE(UNIT=nfprt,FMT=20100)igaus,m,ix
             WRITE(UNIT=nferr,FMT=20100)igaus,m,ix
             STOP 20100
          END IF
       ELSE IF(itavl(m).EQ.2)THEN
          nf=nf+1
          lspec(m)=ispec+1
          IF (lvavl(m).eq.1) THEN
             ispec=ispec+lvavl(m)
            ELSE
             ispec=ispec+kmaxloc
          ENDIF
          IF(ispec.GT.mxspec)THEN
             WRITE(UNIT=nfprt,FMT=20600)ispec,m,ix
             WRITE(UNIT=nferr,FMT=20600)ispec,m,ix
             STOP 20600
          END IF
       END IF
    END DO

    IF(nf+icf.NE.nof)THEN
       WRITE(UNIT=nfprt,FMT=21100)nf,icf,nof
       WRITE(UNIT=nferr,FMT=21100)nf,icf,nof
       STOP 21100
    END IF

    IF(nfctrl(71).GE.1)WRITE(UNIT=nfprt,FMT=200)nf,ispec,igaus
    IF(icf.NE.0)THEN
       ish=ispec
       igh=igaus
       DO ix=-1,-icf,-1
          IF(itcf(-ix).EQ.1)THEN
             nf=nf+1
             lgaus(ix)=igaus+1
             igaus=igaus+lvcf(-ix)
             IF(igaus.GT.mxgaus)THEN
                WRITE(UNIT=nfprt,FMT=20100)igaus,m,ix
                WRITE(UNIT=nferr,FMT=20100)igaus,m,ix
                STOP 20100
             END IF
          ELSE IF(itcf(-ix).EQ.2)THEN
             nf=nf+1
             lspec(ix)=ispec+1
             IF (lvcf(-ix).eq.1) THEN
                ispec=ispec+lvcf(-ix)
               ELSE
                ispec=ispec+kmaxloc
             ENDIF
             IF(ispec.GT.mxspec)THEN
                WRITE(UNIT=nfprt,FMT=20600)ispec,m,ix
                WRITE(UNIT=nferr,FMT=20600)ispec,m,ix
                STOP 20600
             END IF
          END IF
       END DO
       IF(nfctrl(71).GE.1)WRITE(UNIT=nfprt,FMT=400)icf,ispec-ish,igaus-igh
    END IF
    nw=ispec*2*mymnMax+igaus*ibMax*myjmax_d
    IF(nfctrl(71).GE.1)WRITE(UNIT=nfprt,FMT=500)nf,ispec,igaus,nw

200 FORMAT(' ACCUMULATORS FOR DIRECT DIAGNOSTICS ARE:'/' ',I3, &
         ' FIELDS USING',I4,' SPECTRAL SLOTS AND',I4, &
         ' GAUSSIAN SLOTS')
400 FORMAT('0ACCUMULATORS FOR COMBINED FIELDS ARE:'/' ',I3, &
         ' FIELDS USING',I4,' SPECTRAL SLOTS AND',I4, &
         ' GAUSSIAN SLOTS')
500 FORMAT(' TOTAL ACCUMULATORS FOR ALL DIAGNOSTICS ARE:'/' ',I3, &
         ' FIELDS USING',I4,' SPECTRAL SLOTS AND',I4,' GAUSSIAN SLOTS' &
         ,/,' TOTAL WORDS IN DIAGNOSTIC ACCUMULATORS=',I16//)
20600 FORMAT(' TOTAL NUMBER OF AVAILABLE SPECTRAL SLOTS EXCEEDED.'/ &
         ' NO. OF SLOTS AT LIMIT POINT:',I4,' M=',I4,' IX=',I4)
20100 FORMAT(' TOTAL NUMBER OF AVAILABLE GAUSSIAN SLOTS EXCEEDED.'/ &
         ' NO. OF SLOTS AT LIMIT POINT:',I4,' M=',I4,' IX=',I4)
21100 FORMAT(' NUMBER OF COMPUTED FIELDS =',I3,' +',I3, &
         ' IS NOT THE SAME AS THE NUMBER OF DESIRED FIELDS=',I4)
  END SUBROUTINE setdia





  SUBROUTINE upspec(field,mnRIFirst,mnRILast,loca)
    !
    ! upspec :extended diagnostics version 1 diagnostic field accumulator
    !            routine.  memory resident version.  see subroutine indiag for
    !            further discussion.
    !            for spectral fields only called for entire field
    !
    INTEGER, INTENT(in) :: mnRIFirst
    INTEGER, INTENT(in) :: mnRILast
    INTEGER, INTENT(in) :: loca
    REAL(KIND=r8),    INTENT(in) :: field(mnRIFirst:mnRILast,kMaxloc)

    REAL(KIND=r8) :: hold(mnRIFirst:mnRILast, kMaxloc)
    INTEGER :: lvl
    INTEGER :: ka
    INTEGER :: kk
    INTEGER :: kka
    INTEGER :: kg
    INTEGER :: kcf
    INTEGER :: jcf
    INTEGER :: ja
    INTEGER :: jja
    INTEGER :: jj
    INTEGER :: jx
    INTEGER :: kx
    INTEGER :: l
    INTEGER :: ll
    INTEGER :: i

    IF(.NOT.dodia(loca)) THEN
       WRITE(UNIT=nfprt,FMT=3180)loca
       WRITE(UNIT=nferr,FMT=3180)loca
       STOP 3180
    END IF
    IF(itavl(loca).NE.2)THEN
       WRITE(UNIT=nfprt,FMT=4180)itavl(loca)
       WRITE(UNIT=nferr,FMT=4180)itavl(loca)
       STOP 4180
    END IF
    lvl=lvavl(loca)
    IF (lvl.eq.1.and..not.havesurf) RETURN
    IF (lvl.eq.kmaxnew) lvl=kmaxloc
    ka=ixavl(loca)
    !
    !     case for directly saved fields
    !
    IF(iavrq(loca).GT.0)THEN
       kg=lspec(loca)
       DO l=1,lvl
          ll=kg+l-1
          DO i=mnRIFirst,mnRILast
             spec(i,ll)=spec(i,ll)+field(i,l)
          END DO
       END DO
    END IF
    !
    !     case for combined fields
    !
    IF(kravl(ka).LT.0.OR.inavl(loca).GT.1)THEN
       kka=ka+inavl(loca)-1
       IF(kravl(ka).GT.1)ka=ka+1
       !
       !     for each combined field using the supplied available diagnostic
       !
       DO kk=ka,kka
          kcf=kravl(kk)
          jcf=-kcf
          ja=ixcf(jcf)
          jja=ja+incf(jcf)-1
          !
          !     search for corresponding desired field
          !
          DO jj=ja,jja
             jx=jrcf(jj)
             kx=krcf(jj)
             IF(kx.EQ.loca)go to 200
          END DO
          WRITE(UNIT=nfprt,FMT=3680)loca,jcf,kk,ja,jja
          WRITE(UNIT=nferr,FMT=3680)loca,jcf,kk,ja,jja
          STOP 3680
          !
          !     treat each accumulation according the the sign of the desired
          !     calculation code (iclcd)
          !
200       CONTINUE
          DO l=1,lvl
             DO i=mnRIFirst,mnRILast
                hold(i,l)=field(i,l)
             END DO
          END DO
          CALL cnvray(hold, (mnRILast-mnRIFirst+1)*lvl, nuavl(loca), nucf(jcf))
          IF(iclcd(jx).LT.0)THEN
             kg=lspec(kcf)
             DO l=1,lvl
                ll=kg+l-1
                DO i=1,mnRIFirst,mnRILast
                   spec(i,ll)=spec(i,ll)-hold(i,l)
                END DO
             END DO
          ELSE
             kg=lspec(kcf)
             DO l=1,lvl
                ll=kg+l-1
                DO i=1,mnRIFirst,mnRILast
                   spec(i,ll)=spec(i,ll)+hold(i,l)
                END DO
             END DO
          END IF
       END DO
    END IF
3180 FORMAT(' ERROR IN CALLING UPSPEC WITH UNSET AVAILABLE DIAGNOSTIC', &
         I3)
3680 FORMAT(' UNABLE TO FIND MATCHING AVAILABLE DIAG. NO.',I3/ &
         ' FOR COMBINED FIELD',I3,' A.D. INDEX',I3,' C.F. RANGE',I3, &
         '-',I3)
4180 FORMAT(' ERROR IN CALLING UPSPEC WITH WRONG TYPE CODE',I2)
  END SUBROUTINE upspec





  SUBROUTINE rsdiag()
    !
    ! rsdiag :extended diagnostics version 1;
    !         reset all the diagnostic accumulators;
    !         this version is the memory resident version;
    !         see subroutine indiag for further discussion.
    !
    CALL MsgOne('**(rsdiag)**','reset all the diagnostic accumulators')
    IF (ispec > 0) THEN
       spec = 0.0_r8
    END IF
    IF (igaus > 0) THEN
       gaus = 0.0_r8
    END IF
  END SUBROUTINE rsdiag





  SUBROUTINE accpf(ifday, tod, qtmpp, qrotp, qdivp, qqp, qlnpp, nfdyn)
    !
    ! accpf  :extended diagnostics version 1 prognostic field accumulator
    !   routine;  memory resident version;  see subroutine indiag for
    !   further discussion.
    !
    INTEGER, INTENT(IN   ) :: ifday
    REAL(KIND=r8)   , INTENT(IN   ) :: tod
    REAL(KIND=r8)   , INTENT(IN   ) :: qtmpp (2*mymnMax, kMaxloc)
    REAL(KIND=r8)   , INTENT(IN   ) :: qrotp (2*mymnMax, kMaxloc)
    REAL(KIND=r8)   , INTENT(IN   ) :: qdivp (2*mymnMax, kMaxloc)
    REAL(KIND=r8)   , INTENT(IN   ) :: qqp   (2*mymnMax, kMaxloc)
    REAL(KIND=r8)   , INTENT(IN   ) :: qlnpp (2*mymnMax)
    INTEGER, INTENT(IN   ) :: nfdyn


    REAL(KIND=r8)                   :: specg(2*mnmax,5)
    INTEGER                :: lo
    INTEGER                :: l
    INTEGER                :: lv
    INTEGER                :: mn

    IF (pfbar) THEN

       IF(dodia(nDiag_tmdivg).AND.iavrq(nDiag_tmdivg).GT.0)THEN
          lo = lspec(nDiag_tmdivg) - 1
          DO l = 1, kMaxloc
             lv = lo + l
             DO mn = 1, 2*mymnMax
                spec(mn,lv) = spec(mn,lv) + qdivp(mn,l)
             END DO
          END DO
       END IF

       IF(dodia(nDiag_tmvort).AND.iavrq(nDiag_tmvort).GT.0)THEN
          lo = lspec(nDiag_tmvort) - 1
          DO l = 1, kMaxloc
             lv = lo + l
             DO mn = 1, 2*mymnMax
                spec(mn,lv) = spec(mn,lv) + qrotp(mn,l)
             END DO
          END DO
       END IF

       IF(dodia(nDiag_tmtvir).AND.iavrq(nDiag_tmtvir).GT.0)THEN
          lo = lspec(nDiag_tmtvir) - 1
          DO l = 1, kMaxloc
             lv = lo + l
             DO mn = 1, 2*mymnMax
                spec(mn,lv) = spec(mn,lv) + qtmpp(mn,l)
             END DO
          END DO
       END IF

       IF (dodia(nDiag_tmsphu) .AND. (iavrq(nDiag_tmsphu).GT.0)) THEN
          lo = lspec(nDiag_tmsphu) - 1
          DO l = 1, kMaxloc
             lv = lo + l
             DO mn = 1, 2*mymnMax
                spec(mn,lv) = spec(mn,lv) + qqp  (mn,l)
             END DO
          END DO
       END IF

       ! note that the time mean surface pressure is now computed
       ! in gloop in place of the time mean ln surface pressure.
       ! this is the time mean log surface pressure saved separately.

       IF (havesurf .AND. dodia(nDiag_tmlnps) .AND. (iavrq(nDiag_tmlnps).GT.0)) THEN
          lo = lspec(nDiag_tmlnps)
          DO mn = 1, 2*mymnMax
             spec(mn,lo) = spec(mn,lo) + qlnpp(mn)
          END DO
       END IF
    END IF

    IF (dodyn.and.havesurf) THEN

       CALL Collect_Spec(qdivp, specg(1,1), 1, 1, 0)
       CALL Collect_Spec(qrotp, specg(1,2), 1, 1, 0)
       CALL Collect_Spec(qtmpp, specg(1,3), 1, 1, 0)
       CALL Collect_Spec(qqp, specg(1,4), 1, 1, 0)
       CALL Collect_Spec(qlnpp, specg(1,5), 1, 1, 0)
       IF (myid.eq.0) THEN
          WRITE (UNIT=nfprt,FMT='(A,I5,A,F15.2,A)')' ifday=',ifday,' tod=',tod,' dyn'
          WRITE (UNIT=nfdyn) ifday, tod
          WRITE (UNIT=nfdyn) specg(:,1)
          WRITE (UNIT=nfdyn) specg(:,2)
          WRITE (UNIT=nfdyn) specg(:,3)
          WRITE (UNIT=nfdyn) specg(:,4)
          WRITE (UNIT=nfdyn) specg(:,5)
       ENDIF

    END IF

  END SUBROUTINE accpf



  SUBROUTINE Prec_Diag (ifday, tod, prct, prcc, nfprc)
    !
    ! Diagnostic of Total Precipitation (prct) and
    !               Convective Precipitation (prcc).
    ! Large Scale Precipitation can be obtained by: prct-prcc
    !
    INTEGER, INTENT(IN) :: ifday
    REAL(KIND=r8)   , INTENT(IN) :: tod
    REAL(KIND=r8)   , INTENT(IN) :: prct (ibMax,jbMax)
    REAL(KIND=r8)   , INTENT(IN) :: prcc (ibMax,jbMax)
    INTEGER, INTENT(IN) :: nfprc

    REAL(KIND=r8) :: aux1(ijmax),aux2(ijmax)

    IF (doprec) THEN
       CALL Collect_Grid_Red(prct , aux1)
       CALL Collect_Grid_Red(prcc , aux2)
       IF (myid.eq.0) THEN
          CALL WriteDiagHead (nfprc, ifday, tod)
          CALL WriteField (nfprc, aux1)
          CALL WriteField (nfprc, aux2)
       ENDIF
    END IF

  END SUBROUTINE Prec_Diag



  SUBROUTINE wridia (nfdiag, maxstp, idatec)
    !
    ! wridia :writes kistler/katz/schneider diagnostic fields on disk.
    !
    INTEGER, INTENT(IN) :: nfdiag
    INTEGER, INTENT(IN) :: maxstp
    INTEGER, INTENT(IN) :: idatec(4)

    REAL(KIND=r8)    :: gwork(1)
    LOGICAL :: fgm
    LOGICAL :: callsclout
    REAL(KIND=r8)    :: f1
    INTEGER :: j
    INTEGER :: m
    INTEGER :: nn
    INTEGER :: mm
    INTEGER :: ii
    INTEGER :: ix
    INTEGER :: ka
    INTEGER :: ki
    INTEGER :: kk
    INTEGER :: kx
    INTEGER :: jx
    INTEGER :: ji
    INTEGER :: l
    INTEGER :: i
    INTEGER :: k
    INTEGER :: kl
    CHARACTER(LEN=*), PARAMETER :: h="**(wridia)**"
    CHARACTER(LEN=256) :: line
    INTEGER :: lastUsed
    !
    !     global mean diagnostics printed when available
    !
    !     name:                                       a.d. no.
    !     total precipitation                             9
    !     convective precipitation                       10
    !     surface sensible heat flux                     16
    !     surface zonal wind stress                      18
    !     surface meridional wind stress                 19
    !     surface latent heating                         17
    !     downward longwave flux at the bottom           21
    !     outgoing longwave radiation at the top         23
    !     incident shortwave flux at the top             24
    !     downward shorwave at the ground                25
    !     shortwave absorbed by the earth/atmosphere     28
    !     shortwave absorbed at the ground               29
    !     net longwave at the ground                     30
    !
    !
    !     f1 for time intensive (1/maxstp)
    !     synoptic interval is 24 hours presumeably but can be any integral
    !     number of time steps
    !
    f1=1.0_r8/REAL(maxstp,r8)
    fgm=.TRUE.
    CALL Collect_Gauss(gaus,gaus_in,mxgaus)
    !
    !     directly available fields
    !
    line = " file also contains diagnostics at";  lastUsed = LEN_TRIM(line)
    WRITE(line(lastUsed+1:lastUsed+8), "(' ',i2.2,'Z of ')") idatec(1)
    lastUsed = lastUsed + 8
    WRITE(line(lastUsed+1:lastUsed+11), "(2(i2.2,'/'),i4,':')") &
         idatec(3), idatec(2), idatec(4)
    lastUsed=lastUsed+11
    CALL MsgOne(h,line(1:lastUsed))

    DO m=1,mxavl
       IF ((dodia(m)) .AND. (iavrq(m) > 0)) THEN
          nn=iavrq(m)
          IF (itavl(m) == 2) THEN
             mm=lspec(m)
             callsclout = (lvavl(m).eq.kmax).OR.(havesurf)
             IF (lvavl(m).eq.1) THEN
                kl = 1
               ELSE
                kl = kmaxloc
             ENDIF
             IF (callsclout) THEN
                CALL scloutsp(nfdiag, spec(1,mm), kl, lvavl(m), f1, &
                     nuavl(m), nurq(nn))
             END IF
          ELSE
             k = lgaus(m)
             IF (myjmax_d.gt.0) CALL scloutgr(nfdiag, gaus_in(1,1,k), lvavl(m), &
                  f1, nuavl(m), nurq(nn))
             DO ii=1,ngbme
                IF (igbme(ii) == m) GO TO 250
             END DO
             CYCLE
250          IF (fgm .AND. nfctrl(31) >= 1)WRITE(UNIT=nfprt,FMT=300)
             IF (fgm .AND. dodia(17) .AND. nfctrl(31) >= 2)WRITE(UNIT=nfprt,FMT=350)
             IF (myjmax_d.gt.0) THEN 
                CALL globme(gaus_in(1,1,k), dcol, scol, f1, avail(m), &
                  nuavl(m), nurq(nn))
               ELSE
                CALL globme(gwork, dcol, scol, f1, avail(m), &
                  nuavl(m), nurq(nn))
             END IF
             fgm=.FALSE.
          END IF
       END IF
    END DO
    IF (.NOT. fgm .AND. nfctrl(31) >= 1) WRITE(UNIT=nfprt,FMT=1010)
    !
    !     combined fields
    !
    IF (icf /= 0) THEN
       !
       !     first combine other combined field components
       !
       DO ix=1,icf
          IF (icfu(ix)) THEN
             ka=ixucf(ix)
             IF (itcf(ix) == 1) THEN
                ki=lgaus(-ix)
                DO j=1,myjmax_d
                   DO kk=ka,inucf(ix)+ka-1
                      kx=jrucf(kk)
                      jx=iabs(kx)
                      ji=lgaus(-jx)
                      DO l=1,lvcf(jx)
                         IF (kx > 0) THEN
                            DO i=1,iMaxNew
                               gaus_in(i,j,l+ji-1)=gaus_in(i,j,l+ji-1)+ &
                                    gaus_in(i,j,l+ki-1)
                            END DO
                         ELSE
                            DO i=1,iMaxNew
                               gaus_in(i,j,l+ji-1)=gaus_in(i,j,l+ji-1)- &
                                    gaus_in(i,j,l+ki-1)
                            END DO
                         END IF
                      END DO
                   END DO
                END DO
             ELSE IF (itcf(ix) == 2) THEN
                ki=lspec(-ix)
                DO kk=ka,inucf(ix)+ka-1
                   kx=jrucf(kk)
                   jx=iabs(kx)
                   ji=lspec(-jx)
                   DO l=1,lvcf(jx)
                      IF (kx > 0) THEN
                         DO i=1,2*mymnMax
                            spec(i,l+ji-1)=spec(i,l+ji-1)+ &
                                 spec(i,l+ki-1)
                         END DO
                      ELSE
                         DO i=1,2*mymnMax
                            spec(i,l+ji-1)=spec(i,l+ji-1)- &
                                 spec(i,l+ki-1)
                         END DO
                      END IF
                   END DO
                END DO
             END IF
          END IF
       END DO
       !
       !     obtain combined fields and write out
       !
       DO ix=1,icf
          IF (itcf(ix) == 2) THEN
             mm=lspec(-ix)
             callsclout = (lvcf(ix).eq.kmax).OR.(havesurf)
             IF (lvcf(ix).eq.1) THEN
                kl = 1
               ELSE
                kl = kmaxloc
             ENDIF
             IF (callsclout) THEN
                CALL scloutsp(nfdiag,spec(1,mm),kl,lvcf(ix), &
                     f1,nucf(ix),nucf(ix))
             ENDIF
          ELSE IF (itcf(ix) == 1)THEN
             k = lgaus(-ix)
             CALL scloutgr(nfdiag,gaus_in(1,1,k),lvcf(ix), &
                  f1,nucf(ix),nucf(ix))
          END IF
       END DO
    END IF
    gaus_in=0.0_r8
300 FORMAT(//'0GLOBAL MEAN DIAGNOSTICS'//)
350 FORMAT(' NOTE: TO COMPUTE EVAPORATION IN MM/DAY FROM LATENT '  &
         ,'HEATING DIVIDE BY 28.9'//)
1010 FORMAT(//)
  END SUBROUTINE wridia






  SUBROUTINE updia1D(field, loca, latblo)
    !
    ! updia1D  :extended diagnostics version 1 diagnostic field accumulator
    !         subroutine; memory resident version;
    !         see subroutine indiag for further discussion;
    !         for gaussian fields only called one gaussian block at a time.
    !

    REAL(KIND=r8),    INTENT(in   ) :: field(:)
    INTEGER, INTENT(in   ) :: loca
    INTEGER, INTENT(in   ) :: latblo
    REAL(KIND=r8)     :: hold(ibMax)
    INTEGER  :: imkm
    INTEGER  :: i
    INTEGER  :: ka
    INTEGER  :: kg
    INTEGER  :: kka
    INTEGER  :: kk
    INTEGER  :: kcf
    INTEGER  :: kx
    INTEGER  :: ja
    INTEGER  :: jj
    INTEGER  :: jja
    INTEGER  :: jx
    INTEGER  :: jcf

    IF (.NOT. dodia(loca)) THEN
       WRITE(UNIT=nfprt,FMT=3180)loca
       WRITE(UNIT=nferr,FMT=3180)loca
       STOP 3180
    END IF

    IF (itavl(loca) /= 1) THEN
       WRITE(UNIT=nfprt,FMT=4180)itavl(loca)
       WRITE(UNIT=nferr,FMT=4180)itavl(loca)
       STOP 4180
    END IF
    ka=ixavl(loca)
    imkm = ibMax
    !
    !    case for directly saved fields
    !
    IF (iavrq(loca) > 0) THEN
       kg=lgaus(loca)
       DO i = 1, ibMaxPerJB(latblo)
          gaus(i,kg,latblo)=gaus(i,kg,latblo)+field(i)
       END DO
    END IF
    !
    !    case for combined fields
    !
    IF (kravl(ka) < 0 .OR. inavl(loca) > 1) THEN
       kka=ka+inavl(loca)-1
       IF (kravl(ka) > 1) ka=ka+1
       !
       !    for each combined field using the supplied available diagnostic
       !
       DO kk = ka, kka
          kcf=kravl(kk)
          jcf=-kcf
          ja =ixcf(jcf)
          jja=ja+incf(jcf)-1
          !
          !    search for corresponding desired field
          !
          DO jj = ja, jja
             jx=jrcf(jj)
             kx=krcf(jj)
             IF (kx == loca) go to 200
          END DO

          WRITE(UNIT=nfprt,FMT=3680)loca,jcf,kk,ja,jja
          WRITE(UNIT=nferr,FMT=3680)loca,jcf,kk,ja,jja

          STOP 3680
          !
          !    treat each accumulation according the the sign of the desired
          !    calculation code (iclcd)
          !
200       CONTINUE

          DO i = 1, ibMaxPerJB(latblo)
             hold(i)=field(i)
          END DO

          CALL cnvray(hold,imkm,nuavl(loca),nucf(jcf))

          IF (iclcd(jx) < 0) THEN
             kg=lgaus(kcf)
             DO i = 1, ibMaxPerJB(latblo)
                gaus(i,kg,latblo)=gaus(i,kg,latblo)-hold(i)
             END DO
          ELSE
             kg=lgaus(kcf)
             DO i = 1, ibMaxPerJB(latblo)
                gaus(i,kg,latblo)=gaus(i,kg,latblo)+hold(i)
             END DO
          END IF
       END DO
    END IF
3180 FORMAT(' ERROR IN CALLING updia1D WITH UNSET AVAILABLE DIAGNOSTIC', I3)
3680 FORMAT(' UNABLE TO FIND MATCHING AVAILABLE DIAG. NO.',I3/ &
         ' FOR COMBINED FIELD',I3,' A.D. INDEX',I3,' C.F. RANGE',I3,'-',I3)
4180 FORMAT(' ERROR IN CALLING updia1D WITH WRONG TYPE CODE',I2)
  END SUBROUTINE updia1D

  SUBROUTINE updia2D(field, loca, latblo)
    !
    ! updia2D  :extended diagnostics version 1 diagnostic field accumulator
    !         subroutine; memory resident version;
    !         see subroutine indiag for further discussion;
    !         for gaussian fields only called one gaussian block at a time.
    !

    REAL(KIND=r8),    INTENT(in   ) :: field(:,:)
    INTEGER, INTENT(in   ) :: loca
    INTEGER, INTENT(in   ) :: latblo
    REAL(KIND=r8)     :: hold(ibMax,kMaxNew)
    INTEGER  :: imkm
    INTEGER  :: i
    INTEGER  :: lvl
    INTEGER  :: l
    INTEGER  :: ll
    INTEGER  :: ka
    INTEGER  :: kg
    INTEGER  :: kka
    INTEGER  :: kk
    INTEGER  :: kcf
    INTEGER  :: kx
    INTEGER  :: ja
    INTEGER  :: jj
    INTEGER  :: jja
    INTEGER  :: jx
    INTEGER  :: jcf

    IF (.NOT. dodia(loca)) THEN
       WRITE(UNIT=nfprt,FMT=3180)loca
       WRITE(UNIT=nferr,FMT=3180)loca
       STOP 3180
    END IF

    IF (itavl(loca) /= 1) THEN
       WRITE(UNIT=nfprt,FMT=4180)itavl(loca)
       WRITE(UNIT=nferr,FMT=4180)itavl(loca)
       STOP 4180
    END IF
    lvl=lvavl(loca)
    ka=ixavl(loca)
    imkm = ibMax*lvl
    !
    !    case for directly saved fields
    !
    IF (iavrq(loca) > 0) THEN
       kg=lgaus(loca)
       DO l = 1, lvl
          ll=kg+l-1
          DO i = 1, ibMaxPerJB(latblo)
             gaus(i,ll,latblo)=gaus(i,ll,latblo)+field(i,l)
          END DO
       END DO
    END IF
    !
    !    case for combined fields
    !
    IF (kravl(ka) < 0 .OR. inavl(loca) > 1) THEN
       kka=ka+inavl(loca)-1
       IF (kravl(ka) > 1) ka=ka+1
       !
       !    for each combined field using the supplied available diagnostic
       !
       DO kk = ka, kka
          kcf=kravl(kk)
          jcf=-kcf
          ja =ixcf(jcf)
          jja=ja+incf(jcf)-1
          !
          !    search for corresponding desired field
          !
          DO jj = ja, jja
             jx=jrcf(jj)
             kx=krcf(jj)
             IF (kx == loca) go to 200
          END DO

          WRITE(UNIT=nfprt,FMT=3680)loca,jcf,kk,ja,jja
          WRITE(UNIT=nferr,FMT=3680)loca,jcf,kk,ja,jja

          STOP 3680
          !
          !    treat each accumulation according the the sign of the desired
          !    calculation code (iclcd)
          !
200       CONTINUE

          DO l = 1, lvl
             DO i = 1, ibMaxPerJB(latblo)
                hold(i,l)=field(i,l)
             END DO
          END DO

          CALL cnvray(hold,imkm,nuavl(loca),nucf(jcf))

          IF (iclcd(jx) < 0) THEN
             kg=lgaus(kcf)
             DO l = 1, lvl
                ll=kg+l-1
                DO i = 1, ibMaxPerJB(latblo)
                   gaus(i,ll,latblo)=gaus(i,ll,latblo)-hold(i,l)
                END DO
             END DO
          ELSE
             kg=lgaus(kcf)
             DO l = 1, lvl
                ll=kg+l-1
                DO i = 1, ibMaxPerJB(latblo)
                   gaus(i,ll,latblo)=gaus(i,ll,latblo)+hold(i,l)
                END DO
             END DO
          END IF
       END DO
    END IF
3180 FORMAT(' ERROR IN CALLING updia2D WITH UNSET AVAILABLE DIAGNOSTIC', I3)
3680 FORMAT(' UNABLE TO FIND MATCHING AVAILABLE DIAG. NO.',I3/ &
         ' FOR COMBINED FIELD',I3,' A.D. INDEX',I3,' C.F. RANGE',I3,'-',I3)
4180 FORMAT(' ERROR IN CALLING updia2D WITH WRONG TYPE CODE',I2)
  END SUBROUTINE updia2D

  !
  SUBROUTINE rmsgt(q,x,y,w,del,r,idatec)
    !
    !
    !
    !     rmsgt : calculates and prints out standard deviations for
    !             history carrying variables of the model.  standard
    !             deviations are calculated on a level-by-level basis
    !             and for the whole atmosphere on a pressure-weighted
    !             basis.
    !
    !***********************************************************************
    !
    !     rmsgt is called by the main routine smf and the subroutine gnmini.
    !
    !     rmsgt calls the following subroutine : hbartr
    !
    !***********************************************************************
    !
    !    argument(dimensions)                       description
    !
    !          q(mnwv2)                input : global field at one level.
    !                                         (spectral).
    !         x(mnwv2,kmax)            input : global field at all levels.
    !                                         (spectral).
    !         y(mnwv2,kmax)            input : global field at all levels.
    !                                         (spectral).
    !         w(mnwv2,kmax)            input : global field at all levels.
    !                                         (spectral).
    !          del(kmax)              input : sigma spacing for each layer.
    !                                         computed in routine "setsig".
    !         r(mnwv2,kqmax)          input : global field at moist levels.
    !                                         (spectral).
    !
    !***********************************************************************
    !
    !
    !***********************************************************************
    !
    REAL(KIND=r8), INTENT(IN) :: q(2*mymnMax)
    REAL(KIND=r8), INTENT(IN) :: x(2*mymnMax, kMaxloc)
    REAL(KIND=r8), INTENT(IN) :: y(2*mymnMax, kMaxloc)
    REAL(KIND=r8), INTENT(IN) :: del(kMaxNew)
    REAL(KIND=r8), INTENT(IN) :: w(2*mymnMax, kMaxloc)
    REAL(KIND=r8), INTENT(IN) :: r(2*mymnMax, kMaxloc)
    INTEGER      , INTENT(IN) :: idatec(:)

    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: request
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: morder(MMax)
    INTEGER :: status(MPI_STATUS_SIZE)
    REAL(KIND=r8) :: sasy(myMMax,kMaxloc,5)
    REAL(KIND=r8) :: sintaux(MMax*(4*kMaxloc+1))
    REAL(KIND=r8) :: sint(MMax,kMaxloc,5)
    REAL(KIND=r8) :: gint(3,4,kMaxNew)
    REAL(KIND=r8) :: gintsurf(2)
    REAL(KIND=r8) :: gx(kMaxNew)
    REAL(KIND=r8) :: gy(kMaxNew)
    REAL(KIND=r8) :: gw(kMaxNew)
    REAL(KIND=r8) :: gr(kMaxNew)
    REAL(KIND=r8) :: gmx
    REAL(KIND=r8) :: gmy
    REAL(KIND=r8) :: gmw
    REAL(KIND=r8) :: gmr
    REAL(KIND=r8) :: sq
    REAL(KIND=r8) :: aq
    REAL(KIND=r8) :: gmq
    REAL(KIND=r8) :: sv(4)
    REAL(KIND=r8) :: av(4)
    REAL(KIND=r8) :: sq2o2


    INTEGER :: lastUsed
    CHARACTER(LEN=256) :: line
    CHARACTER(LEN=*), PARAMETER :: h="**(rmsgt)**"
    INTEGER :: k, i, i1, i2, i3, m, is, k1, k2
    INTEGER :: l
    INTEGER :: ini(0:maxNodes_four-1)

    sq2o2=SQRT(2.0_r8)/2.0_r8
    is = 0
    IF (havesurf) is = 1
    sv=0.0_r8
    av=0.0_r8
    gmx=0.0_r8
    gmy=0.0_r8
    gmw=0.0_r8
    gmr=0.0_r8
    gmq=0.0_r8
    CALL hbartr2D(x,sasy(1,1,1))
    CALL hbartr2D(y,sasy(1,1,2))
    CALL hbartr2D(w,sasy(1,1,3))
    CALL hbartr2D(r,sasy(1,1,4))
    IF (havesurf) CALL hbartr(q,sasy(1,1,5))
    !
    !   global communication (collect all m's from different sources)
    !
    comm = COMM_FOUR
    IF (myid_four.ne.0) THEN
       CALL MPI_ISEND(sasy,(4*kmaxloc+is)*mymmax,MPI_DOUBLE_PRECISION,0,96,comm,request,ierr)
       CALL MPI_WAIT(request,status,ierr)
    ELSE
       requestr(0) = MPI_REQUEST_NULL
       ini(0) = 1
       DO i=1,MaxNodes_four-1
          ini(i) = ini(i-1) + (4*kmaxloc+is)*Msperproc(i-1)
          CALL MPI_IRECV(sintaux(ini(i)),(4*kmaxloc+is)*Msperproc(i),MPI_DOUBLE_PRECISION,i,96,&
               comm,requestr(i),ierr)
       ENDDO
       sint(1:mymmax,1:kmaxloc,1:4) = sasy(1:mymmax,1:kmaxloc,1:4)
       IF (havesurf) sint(1:mymmax,1,5) = sasy(1:mymmax,1,5)
       k = 1
       DO i=0,MaxNodes_four-1
          DO m=1,Msperproc(i)
             morder(Msinproc(m,i)) = k
             k = k+1
          ENDDO
       ENDDO
       DO l=1,MaxNodes_four-1
          CALL MPI_WAITANY(MaxNodes_four,requestr(0),index,status,ierr)
          i = status(MPI_SOURCE)
          i1=mymmax
          DO k=1,i-1
             i1 = i1 + Msperproc(k)
          ENDDO
          i2 = ini(i) - 1
          DO i3=1,4
             DO k=1,kmaxloc
                sint(i1+1:i1+Msperproc(i),k,i3) = sintaux(i2+1:i2+Msperproc(i))
                i2 = i2 + Msperproc(i)
             ENDDO
          ENDDO
          IF (havesurf) sint(i1+1:i1+Msperproc(i),1,5) = sintaux(i2+1:i2+Msperproc(i))
       ENDDO
       gint=0.0_r8
       gintsurf=0.0_r8
       DO k=1,kmaxloc
          DO m=2,MMax
             gint(2,1:4,k) = gint(2,1:4,k) + sint(morder(m),k,1:4)
          ENDDO
       ENDDO
       IF (havesurf) THEN
          DO m=2,MMax
             gintsurf(2) = gintsurf(2) + sint(morder(m),1,5)
          ENDDO
       ENDIF
       gint(1,1,1:kmaxloc) = sint(1,:,1)
       gint(1,2,1:kmaxloc) = sint(1,:,2)
       gint(1,3,1:kmaxloc) = sint(1,:,3)
       gint(1,4,1:kmaxloc) = sint(1,:,4)
       gint(3,1,1:kmaxloc) = x(1,:)*sq2o2
       gint(3,2,1:kmaxloc) = y(1,:)*sq2o2
       gint(3,3,1:kmaxloc) = w(1,:)*sq2o2
       gint(3,4,1:kmaxloc) = r(1,:)*sq2o2
       IF (havesurf) gintsurf(1) = sint(1,1,5)
       comm = MPI_COMM_WORLD
       IF (myid.ne.0) THEN
          CALL MPI_ISEND(gint,12*kmaxloc,MPI_DOUBLE_PRECISION,0,97,comm,request,ierr)
          CALL MPI_WAIT(request,status,ierr)
       ELSE
          requestr(0) = MPI_REQUEST_NULL
          DO i=2,ngroups_four
             k = first_proc_four(i)
             k1 = kfirst_four(k)
             k2 = klast_four(k)
             CALL MPI_IRECV(gint(1,1,k1),12*(k2-k1+1),MPI_DOUBLE_PRECISION,k,97,&
                  comm,requestr(i-1),ierr)
          ENDDO
          DO l=1,ngroups_four-1
             CALL MPI_WAITANY(ngroups_four,requestr(0),index,status,ierr)
          ENDDO
          DO k=1, kMaxNew
             gx(k)=gint(3,1,k)
             gy(k)=gint(3,2,k)
             gw(k)=gint(3,3,k)
             gr(k)=gint(3,4,k)
             gmx=gmx+gx(k)*del(k)
             gmy=gmy+gy(k)*del(k)
             gmw=gmw+gw(k)*del(k)
             gmr=gmr+gr(k)*del(k)
          END DO
          DO l=1,4
             DO k=1, kMaxNew
                sv(l)=sv(l)+gint(1,l,k)*del(k)
                av(l)=av(l)+gint(2,l,k)*del(k)
             END DO
          END DO
          gint(1:2,:,:)=SQRT(gint(1:2,:,:))

          gmq=q(1)*sq2o2
          sq=SQRT(gintsurf(1))
          aq=SQRT(gintsurf(2))

          ! case gmx or gmw not zero

          IF(gmx /= 0.0_r8 .OR. gmw /= 0.0_r8)THEN
             line="***WARNING*** Either gmx("; lastUsed = LEN_TRIM(line)
             WRITE(line(lastUsed+1:lastUsed+11), "(g11.4)") gmx
             line = TRIM(line) // ") or gmw ("; lastUsed = LEN_TRIM(line)
             WRITE(line(lastUsed+1:lastUsed+11), "(g11.4)") gmw
             line = TRIM(line) // ") not zero"; lastUsed = LEN_TRIM(line)
             CALL MsgOne(h,line(1:lastUsed))
             RETURN
          END IF

          ! dump header

          line = " State of the atmosphere at";  lastUsed = LEN_TRIM(line)
          WRITE(line(lastUsed+1:lastUsed+8), "(' ',i2.2,'Z of ')") idatec(1)
          lastUsed = lastUsed + 8
          WRITE(line(lastUsed+1:lastUsed+11), "(2(i2.2,'/'),i4,':')") &
               idatec(3), idatec(2), idatec(4)
          lastUsed=lastUsed+11
          CALL MsgOne(h,line(1:lastUsed))
   
          ! dump Layer Means and Variances

          CALL MsgOne(h," Layers and average means and variances:")
          line=" LYR  Z.S. DIV.  Z.A. DIV.  Z.S. VOR.  Z.A. VOR.  G.M. TEM.  Z.S. TEM."//&
               &"  Z.A. TEM.  G.M. S.H.  Z.S. S.H.  Z.A. S.H."
          CALL MsgOne(h,TRIM(line))
          DO k=1,kMaxNew
             l=kMaxNew+1-k
             WRITE(line(1:114),"(I4,1P,4G11.3,0P,3(2X,F7.2,2X),1P,3G11.3)") &
                  l,gint(1,1,l),gint(2,1,l), &
                  gint(1,3,l),gint(2,3,l), gy(l), gint(1,2,l),gint(2,2,l), &
                  gr(l), gint(1,4,l),gint(2,4,l)
             CALL MsgOne(h,line(1:114))
          END DO
          sv=SQRT(sv)
          av=SQRT(av)
          WRITE(line(1:114),"(A4,1P,4G11.3,0P,3(2X,F7.2,2X),1P,3G11.3)") &
               " AVE",sv(1),av(1),sv(3),av(3),gmy,sv(2),av(2),gmr,sv(4),av(4)
          CALL MsgOne(h,line(1:110))
          CALL MsgOne(h," Log(surface pressure) mean and variance:")
          WRITE(line(1:60), "(3(a11,g9.3))")"  G.M.LNP.=",gmq,"; Z.S.LNP.=",sq,"; Z.A.LNP.=",aq
          CALL MsgOne(h,line(1:60))
       END IF
    END IF
  END SUBROUTINE rmsgt



  SUBROUTINE globme(a, dthet, costhe, cf, title, nufr, nuto)
    !
    ! globme :perfoms zonal and global mean.
    !
    !
    !     find global mean of a
    !
    REAL(KIND=r8)             , INTENT(IN   ) :: a(iMaxNew,myjMax_d)
    REAL(KIND=r8)             , INTENT(IN   ) :: dthet(jMaxNew)
    REAL(KIND=r8)             , INTENT(IN   ) :: costhe(jMaxNew)
    REAL(KIND=r8)             , INTENT(IN   ) :: cf
    CHARACTER(LEN=40), INTENT(IN   ) :: title
    INTEGER          , INTENT(IN   ) :: nufr
    INTEGER          , INTENT(IN   ) :: nuto
    INTEGER                          :: i
    INTEGER                          :: j
    INTEGER                          :: displ(0:maxnodes-1)
    REAL(KIND=r8)                             :: gm
    REAL(KIND=r8)  :: work(iMaxNew,myjMax_d)
    REAL(KIND=r8)  :: z (jMaxNew)
    REAL(KIND=r8)  :: zz(jmaxNew)
    REAL(KIND=r8)  :: z1(myjmax_d), z2(myjmax_d)

    CHARACTER(LEN=16) :: c0
    CHARACTER(LEN=256) :: line
    CHARACTER(LEN=*), PARAMETER :: h="**(globme)**"
    zz  = 0.0_r8
    z= 0.0_r8
    work= 0.0_r8
    z1  = 0.0_r8
    z2  = 0.0_r8
    gm  = 0.0_r8
    displ= 0
    i= 0
    j= 0
    DO j=1,myjmax_d
       DO i=1,iMaxNew
          work(i,j)=a(i,j)*cf
       END DO
    END DO
    CALL cnvray(work  ,iMaxNew*myjMax_d, nufr, nuto)
    !
    !     zonal mean
    !
    DO j=1,myjmax_d
       z1(j) = 0.0_r8
       z2(j) = 0.0_r8
       DO i=1,iMaxNew
          z1(j) = z1(j) + work(i,j)
          z2(j) = z2(j) + work(i,j)*work(i,j)
       END DO
    END DO
    displ(0) = 0
    DO i=1,maxnodes-1
       displ(i) = displ(i-1) + nlatsinproc_d(i-1)
    ENDDO
    !
    !     integral with latitude
    !
    IF (maxnodes == 1) THEN
       Z  =z1
       ZZ =z2
    ELSE
       CALL MPI_GATHERV(z1,myjmax_d,MPI_DOUBLE_PRECISION,Z,nlatsinproc_d(0),displ(0),MPI_DOUBLE_PRECISION, &
            0, MPI_COMM_WORLD, ierr)
       CALL MPI_GATHERV(z2,myjmax_d,MPI_DOUBLE_PRECISION,ZZ,nlatsinproc_d(0),displ(0),MPI_DOUBLE_PRECISION, &
            0, MPI_COMM_WORLD, ierr)
    END IF
    IF (myid.eq.0) THEN
       gm = 0.0_r8
       DO j=1,jMaxNew
          gm = gm + z(j)*costhe(j)*dthet(j)/imaxNew
       END DO
       !
       !     global mean
       !
       gm = 0.5_r8 * gm

       WRITE(c0,"(g16.8)") gm
       line=title//" Glob.Mean="//c0
       !
       !     standard deviation
       !
       DO j=1,jMaxNew
          z(j) = (zz(j) - 2.0_r8 * gm * z(j))/imaxNew + gm*gm
       END DO
       !
       !     integral with latitude
       !
       gm = 0.0_r8
       DO j=1,jMaxNew
          gm = gm + z(j)*costhe(j)*dthet(j)
       END DO
       gm = SQRT(0.5_r8 * gm)
       WRITE(c0,"(g16.8)") gm
       line=TRIM(line)//"; Std.Dev.="//c0//"; in units of "//&
            TRIM(ADJUSTL(aunits(nuto)))
       CALL MsgOne(h,TRIM(line))
    ENDIF
  END SUBROUTINE globme




  SUBROUTINE reord (datum, dim2, work, lev, imask, tsea, ittl)
    INTEGER, INTENT(IN ) :: dim2
    REAL(KIND=r8),    INTENT(in ) :: datum(ibMax,dim2,jbMax)
    REAL(KIND=r8),    INTENT(OUT) :: work (ibMax,jbMax)
    INTEGER, INTENT(IN ) :: lev
    INTEGER(KIND=i8), INTENT(IN ) :: imask   (ibMax,jbMax)
    REAL(KIND=r8),    INTENT(IN ) :: tsea    (ibMax,jbMax)
    CHARACTER(LEN=4), INTENT(IN) :: ittl

    INTEGER :: j
    INTEGER :: i
    INTEGER :: ncount
    LOGICAL :: case1
    LOGICAL :: case2
    case1 = ittl == 'TD  '
    case2 = ittl == 'W1  ' .OR. ittl == 'W2  ' .OR. ittl == 'W3  '
    DO j = 1, jbMax
       ncount=0
       DO i = 1, ibMax
          IF (imask(i,j) >= 1) THEN
             ncount = ncount + 1
             work(i,j) = datum(ncount,lev,j)
          ELSE IF (case1) THEN
             work(i,j)=ABS(tsea(i,j))
          ELSE IF (case2) THEN
             work(i,j)=1.0_r8
          ELSE
             work(i,j)=0.0_r8
          END IF
       END DO
    END DO
  END SUBROUTINE reord






  SUBROUTINE weprog (nedrct,neprog,nefcst,ifday ,tod   ,idate ,idatec, &
       del   ,qgzs  ,lsmk  ,qlnp  ,qdiv  , &
       qrot  ,qq    ,qtmp  ,z0    ,gtsea ,td0   ,capac0, &
       w0    ,imask ,temp2m,umes2m,uve10m,vve10m,roperm,namee ,labeli,labelf,extw  , &
       exdw  ,trunc ,lev )
    INTEGER           , INTENT(IN   ) :: nedrct
    INTEGER           , INTENT(IN   ) :: neprog
    INTEGER           , INTENT(IN   ) :: nefcst
    INTEGER           , INTENT(IN   ) :: ifday
    REAL(KIND=r8)     , INTENT(IN   ) :: tod
    INTEGER           , INTENT(IN   ) :: idate (:)
    INTEGER           , INTENT(IN   ) :: idatec(:)
    REAL(KIND=r8)     , INTENT(IN   ) :: del   (:)
    REAL(KIND=r8)     , INTENT(IN   ) :: qgzs  (:)
    REAL(KIND=r8)     , INTENT(IN   ) :: lsmk  (:)
    REAL(KIND=r8)     , INTENT(IN   ) :: qlnp  (:)
    REAL(KIND=r8)     , INTENT(IN   ) :: qdiv  (:,:)
    REAL(KIND=r8)     , INTENT(IN   ) :: qrot  (:,:)
    REAL(KIND=r8)     , INTENT(IN   ) :: qq    (:,:)
    REAL(KIND=r8)     , INTENT(IN   ) :: qtmp  (:,:)
    REAL(KIND=r8), TARGET, INTENT(IN   ) :: z0  (ibMax,jbMax)
    REAL(KIND=r8), TARGET, INTENT(IN   ) :: temp2m(ibMax,jbMax)
    REAL(KIND=r8), TARGET, INTENT(IN   ) :: umes2m(ibMax,jbMax)
    REAL(KIND=r8), TARGET, INTENT(IN   ) :: uve10m(ibMax,jbMax)
    REAL(KIND=r8), TARGET, INTENT(IN   ) :: vve10m(ibMax,jbMax)
    REAL(KIND=r8), TARGET, INTENT(IN   ) :: gtsea (ibMax,jbMax)
    REAL(KIND=r8)     , INTENT(IN   ) :: td0   (ibMax,jbMax)
    REAL(KIND=r8)     , INTENT(IN   ) :: capac0(ibMax,2,jbMax)
    REAL(KIND=r8)     , INTENT(IN   ) :: w0    (ibMax,3,jbMax)
    INTEGER(KIND=i8)  , INTENT(IN   ) :: imask (ibMax,jbMax)
    CHARACTER(LEN=200), INTENT(IN   ) :: roperm
    CHARACTER(LEN=  7), INTENT(IN   ) :: namee
    CHARACTER(LEN= 10), INTENT(IN   ) :: labeli
    CHARACTER(LEN= 10), INTENT(IN   ) :: labelf
    CHARACTER(LEN=  5), INTENT(IN   ) :: extw
    CHARACTER(LEN=  5), INTENT(IN   ) :: exdw
    CHARACTER(LEN=  *), INTENT(IN   ) :: trunc
    CHARACTER(LEN=  *), INTENT(IN   ) :: lev
    INTEGER             :: k
    REAL(KIND=r8), TARGET :: work(ibMax,jbMax,6)
    REAL(KIND=r8)       :: qspec(2*mnmax,kmaxnew)
    REAL(KIND=r8)       :: tod4
    INTEGER             :: ifday4
    INTEGER             :: idat4(4)
    INTEGER             :: idat4c(4)
    INTEGER, ALLOCATABLE :: interp_type(:)
    TYPE(p2d), TARGET, ALLOCATABLE :: fields(:)


    IF (myid.eq.0) THEN
       CALL opnprg(nedrct,neprog,nefcst,ifday,tod,idate,idatec, &
            roperm,namee ,labeli ,labelf,extw,exdw,trunc,lev)

       CALL WriteDire(nedrct, idate,idatec(1), idatec(3),idatec(2),&
            idatec(4),del,tod)
       !
       !**  write directory, label and prognostic fields
       !
       ifday4=ifday
       tod4=tod
       DO k=1,4
          idat4(k)=idate(k)
          idat4c(k)=idatec(k)
       ENDDO

       CALL WriteProgHead(neprog, ifday4, tod4, idat4, idat4c)
    ENDIF
    !
    IF (maxnodes.gt.1) THEN
       !    topography
       !
       IF (havesurf) CALL Collect_Spec(qgzs, qspec, 1, 1, 0)
       IF (myid.eq.0) THEN
          CALL WriteField(neprog, qspec(:,1))
       ENDIF
       !
       !    land sea mask
       !
       IF (myid.eq.0) THEN
          CALL WriteField(neprog, lsmk)
       ENDIF
       !
       !    ln surface pressure
       !
       IF (havesurf) CALL Collect_Spec(qlnp, qspec, 1, 1, 0)
       IF (myid.eq.0) THEN
          CALL WriteField(neprog, qspec(:,1))
       ENDIF
       !
       !    divergence
       !
       CALL Collect_Spec(qdiv, qspec, kmaxloc, kmaxnew, 0)
       IF (myid.eq.0) THEN
          CALL WriteField(neprog, qspec)
       ENDIF
       !
       !    vorticity
       !
       CALL Collect_Spec(qrot, qspec, kmaxloc, kmaxnew, 0)
       IF (myid.eq.0) THEN
          CALL WriteField(neprog, qspec)
       ENDIF
       !
       !    specific humidity
       !
       CALL Collect_Spec(qq  , qspec, kmaxloc, kmaxnew, 0)
       IF (myid.eq.0) THEN
          CALL WriteField(neprog, qspec)
       ENDIF
       !
       !    virtual temperature
       !
       CALL Collect_Spec(qtmp, qspec, kmaxloc, kmaxnew, 0)
       IF (myid.eq.0) THEN
          CALL WriteField(neprog, qspec)
       ENDIF
    ELSE
       CALL WriteField(neprog, qgzs)
       CALL WriteField(neprog, lsmk)
       CALL WriteField(neprog, qlnp)
       CALL WriteField(neprog, qdiv)
       CALL WriteField(neprog, qrot)
       CALL WriteField(neprog, qq)
       CALL WriteField(neprog, qtmp)
    ENDIF
    ALLOCATE(fields(12))
    ALLOCATE(interp_type(12))
    !     surface roughness
    !
    fields(1)%p => z0
    interp_type(1) = 1
    !
    !     surface temperature
    !
    fields(2)%p => gtsea
    interp_type(2) = 2
    !
    !     deep soil temperature
    !
    CALL reord (td0,    1, work(:,:,1), 1, imask, gtsea, 'TD  ')
    fields(3)%p => work(:,:,1)
    interp_type(3) = 1
    !
    !     storage on canopy
    !
    CALL reord (capac0, 2, work(:,:,2), 1, imask, gtsea, 'CAPC')
    fields(4)%p => work(:,:,2)
    interp_type(4) = 1
    !
    !     storage on ground cover
    !
    CALL reord (capac0, 2, work(:,:,3), 2, imask, gtsea, 'CAPG')
    fields(5)%p => work(:,:,3)
    interp_type(5) = 1
    !
    !    wetness of surface zone
    !
    CALL reord (w0,     3, work(:,:,4), 1, imask, gtsea, 'W1  ')
    fields(6)%p => work(:,:,4)
    interp_type(6) = 1
    !
    !    wetness of root zone
    !
    CALL reord (w0,     3, work(:,:,5), 2, imask, gtsea, 'W2  ')
    fields(7)%p => work(:,:,5)
    interp_type(7) = 1
    !
    !    wetness of drainage zone
    !
    CALL reord (w0,     3, work(:,:,6), 3, imask, gtsea, 'W3  ')
    fields(8)%p => work(:,:,6)
    interp_type(8) = 1
    !
    !  2-meters surface temperature
    !
    fields(9)%p => temp2m
    interp_type(9) = 2
    !
    !  2-meters specific humidity
    !
    fields(10)%p => umes2m
    interp_type(10) = 2
    !
    !  10-meters zonal wind
    !
    fields(11)%p => uve10m
    interp_type(11) = 2
    !
    !  10-meters meridional wind
    !
    fields(12)%p => vve10m
    interp_type(12) = 2
    !
    !  Collect and print fields
    !
    CALL Collect_Grid_Sur_Print(fields,interp_type,12,0,neprog)

    DEALLOCATE(fields)
    DEALLOCATE(interp_type)

    IF (nfctrl(95) .GE. 1) WRITE(UNIT=nfprt,FMT=10)idate,ifday,tod,idatec

10  FORMAT(' Done With weprog. Model Started ',3i3,i5/' Now at',i8,&
         ' Days and',f8.1,' Seconds.  Current Date is',3i3,i5)

  END SUBROUTINE weprog






  SUBROUTINE opnprg(nedrct, neprog, nefcst, ifday, tod, idate, idatec,roperm,&
       namee ,labeli ,labelf,extw   ,exdw  ,trunc ,lev)
    INTEGER,            INTENT(IN) :: nedrct
    INTEGER,            INTENT(IN) :: neprog
    INTEGER,            INTENT(IN) :: nefcst
    INTEGER,            INTENT(IN) :: ifday
    REAL(KIND=r8),               INTENT(IN) :: tod
    INTEGER,            INTENT(IN) :: idate(4)
    INTEGER,            INTENT(IN) :: idatec(4)
    CHARACTER(LEN=200), INTENT(IN) :: roperm
    CHARACTER(LEN=  7), INTENT(IN) :: namee
    CHARACTER(LEN= 10), INTENT(IN) :: labeli
    CHARACTER(LEN= 10), INTENT(IN) :: labelf
    CHARACTER(LEN=  5), INTENT(IN) :: extw
    CHARACTER(LEN=  5), INTENT(IN) :: exdw
    CHARACTER(LEN=  *), INTENT(IN) :: trunc
    CHARACTER(LEN=  *), INTENT(IN) :: lev
    INTEGER :: iyi
    INTEGER :: imi
    INTEGER :: idi
    INTEGER :: ihi
    INTEGER :: iyc
    INTEGER :: imc
    INTEGER :: idc
    INTEGER :: ihc
    INTEGER,            SAVE :: icall=1
    INTEGER,            SAVE :: is
    CHARACTER(LEN= 10), SAVE :: labelc
    CHARACTER(LEN=  3), SAVE :: ext
    CHARACTER(LEN=  6), SAVE :: extn
    CHARACTER(LEN=  6), SAVE :: exdn
    CHARACTER(LEN= 23), SAVE :: modout
    CHARACTER(LEN= 10), SAVE :: label
    LOGICAL inic

    inic=(ifday.EQ.0 .AND. tod.EQ.0)
    IF (icall.EQ.1 .AND. inic) THEN
       ext='icn'
    ELSEIF (icall.EQ.2 .AND. inic) THEN
       icall=3
       ext='inz'
    ELSE
       ext='fct'
    ENDIF
    modout='/'
    IF (icall .EQ. 1) THEN
       icall=2
       is=INDEX(roperm//' ',' ')-1
       IF (is .LE. 0) is=1
       OPEN(UNIT=nefcst,FILE=roperm(1:is)//TRIM(modout)//namee//&
            labeli//labelf//extw(1:2)//'dir'//'.'//TRIM(trunc)//TRIM(lev)//'.files',&
            FORM='formatted', ACCESS='sequential', ACTION='write',    &
            STATUS='replace', IOSTAT=ierr)
       IF (ierr /= 0) THEN
          WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
               TRIM(modout)//namee//labeli//labelf//extw(1:2)//'dir'//'.'//TRIM(trunc)//    &
               TRIM(lev)//'.files', ierr
          STOP "**(ERROR)**"
       END IF
    ENDIF

    iyi=idate(4)
    imi=idate(2)
    idi=idate(3)
    ihi=idate(1)
    WRITE(label,'(I4.4,3I2.2)')iyi,imi,idi,ihi
    iyc=idatec(4)
    imc=idatec(2)
    idc=idatec(3)
    ihc=idatec(1)
    WRITE(labelc,'(I4.4,3I2.2)')iyc,imc,idc,ihc
    extn(1:2)=extw(1:2)
    extn(3:5)=ext(1:3)
    extn(6:6)='.'
    exdn(1:2)=exdw(1:2)
    IF (ext .EQ. 'icn') THEN
       exdn(3:5)='dic'
    ELSEIF (ext .EQ. 'inz') THEN
       exdn(3:5)='din'
    ELSE
       exdn(3:5)='dir'
    ENDIF
    exdn(6:6)='.'

    WRITE(UNIT=nfprt,FMT='(A,3(2X,A))') ' OPNPRG : ',labeli,label,labelc

    CLOSE(UNIT=nedrct)
    CLOSE(UNIT=neprog)
    OPEN(UNIT=nedrct,FILE=roperm(1:is)//TRIM(modout)//namee// &
         labeli//labelc//exdn//TRIM(trunc)//TRIM(lev),FORM='FORMATTED', &
         ACCESS='sequential',ACTION='write', STATUS='replace', IOSTAT=ierr)
    IF (ierr /= 0) THEN
       WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
            TRIM(modout)//namee//labeli//labelc//exdn//TRIM(trunc)//TRIM(lev), ierr
       STOP "**(ERROR)**"
    END IF

    OPEN(UNIT=neprog,FILE=roperm(1:is)//TRIM(modout)//namee//&
         labeli//labelc//extn//TRIM(trunc)//TRIM(lev),FORM='UNFORMATTED', &
         ACCESS='sequential',ACTION='write', STATUS='replace', IOSTAT=ierr)
    IF (ierr /= 0) THEN
       WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
            TRIM(modout)//namee//labeli//labelc//extn//TRIM(trunc)//TRIM(lev), ierr
       STOP "**(ERROR)**"
    END IF

    WRITE(UNIT=nefcst,FMT='(a)')roperm(1:is)//TRIM(modout)//namee// &
         labeli//labelc//exdn//TRIM(trunc)//TRIM(lev)
    WRITE(UNIT=nefcst,FMT='(a)')roperm(1:is)//TRIM(modout)//namee// &
         labeli//labelc//extn//TRIM(trunc)//TRIM(lev)
  END SUBROUTINE opnprg








  SUBROUTINE opnfct(nfdrct, nfdiag, nffcst, ifday, tod, idate, idatec,&
       roperm,namef,labeli,labelf,extw,exdw,trunc,lev)
    INTEGER           , INTENT(IN) :: nfdrct
    INTEGER           , INTENT(IN) :: nfdiag
    INTEGER           , INTENT(IN) :: nffcst
    INTEGER           , INTENT(IN) :: ifday
    REAL(KIND=r8)     , INTENT(IN) :: tod
    INTEGER           , INTENT(IN) :: idate(4)
    INTEGER           , INTENT(IN) :: idatec(4)
    CHARACTER(LEN=200), INTENT(IN) :: roperm
    CHARACTER(LEN=  7), INTENT(IN) :: namef
    CHARACTER(LEN= 10), INTENT(IN) :: labeli
    CHARACTER(LEN= 10), INTENT(IN) :: labelf
    CHARACTER(LEN=  5), INTENT(IN) :: extw
    CHARACTER(LEN=  5), INTENT(IN) :: exdw
    CHARACTER(LEN=  *), INTENT(IN) :: trunc
    CHARACTER(LEN=  *), INTENT(IN) :: lev

    INTEGER :: iyi
    INTEGER :: imi
    INTEGER :: idi
    INTEGER :: ihi
    INTEGER :: iyc
    INTEGER :: imc
    INTEGER :: idc
    INTEGER :: ihc
    INTEGER :: lastUsed
    LOGICAL :: inic

    INTEGER,            SAVE :: icall=1
    INTEGER,            SAVE :: is
    CHARACTER(LEN= 10)       :: labelc
    CHARACTER(LEN=  3), SAVE :: ext
    CHARACTER(LEN=  6)       :: extn
    CHARACTER(LEN=  6)       :: exdn
    CHARACTER(LEN= 10)       :: label
    CHARACTER(LEN=8) :: c0
    CHARACTER(LEN=8) :: c1
    CHARACTER(LEN=*), PARAMETER :: modout="/"
    CHARACTER(LEN=LEN(namef)+LEN(labeli)+LEN(labelc)+&
         LEN(exdn)+LEN(trunc)+LEN(lev)) :: fNameAsc
    CHARACTER(LEN=LEN(namef)+LEN(labeli)+LEN(labelc)+&
         LEN(extn)+LEN(trunc)+LEN(lev)) :: fNameBin
    CHARACTER(LEN=*), PARAMETER :: h="**(opnfct)**"
    CHARACTER(LEN=256) :: line

    inic=(ifday.EQ.0 .AND. tod.EQ.0)

    IF (icall.EQ.1 .AND. inic) THEN
       ext='icn'
    ELSEIF (icall.EQ.2 .AND. inic) THEN
       icall=3
       ext='inz'
    ELSE
       ext='fct'
    ENDIF
    IF (icall .EQ. 1) THEN
       icall=2
       is=INDEX(roperm//' ',' ')-1
       IF (is .LE. 0) is=1
       OPEN(UNIT=nffcst,FILE=roperm(1:is)//modout//namef// &
            labeli//labelf//extw(1:2)//'dir'//'.'//TRIM(trunc)//TRIM(lev)//'.files', &
            FORM='formatted',ACCESS='sequential',ACTION='write',STATUS='replace',IOSTAT=ierr)
       IF (ierr /= 0) THEN
          WRITE(c0,"(i8)") ierr
          CALL FatalError(h//" Open file "//roperm(1:is)//modout//namef// &
               labeli//labelf//extw(1:2)//"dir"//"."//TRIM(trunc)//TRIM(lev)//".files"//&
               " returned iostat="//TRIM(ADJUSTL(c0)))
       END IF
    ENDIF
    iyi=idate(4)
    imi=idate(2)
    idi=idate(3)
    ihi=idate(1)
    WRITE(label,'(i4.4,3i2.2)')iyi,imi,idi,ihi
    iyc=idatec(4)
    imc=idatec(2)
    idc=idatec(3)
    ihc=idatec(1)
    WRITE(labelc,'(i4.4,3i2.2)')iyc,imc,idc,ihc
    extn(1:2)=extw(1:2)
    extn(3:5)=ext(1:3)
    extn(6:6)='.'
    exdn(1:2)=exdw(1:2)
    IF (ext .EQ. 'icn') THEN
       exdn(3:5)='dic'
    ELSEIF (ext .EQ. 'inz') THEN
       exdn(3:5)='din'
    ELSE
       exdn(3:5)='dir'
    ENDIF
    exdn(6:6)='.'

    CLOSE(UNIT=nfdrct)
    CLOSE(UNIT=nfdiag)

    ! open descriptor file

    fNameAsc=namef//labeli//labelc//exdn//TRIM(trunc)//TRIM(lev)
    OPEN(UNIT=nfdrct,FILE=roperm(1:is)//modout//TRIM(fNameAsc),FORM='formatted', &
         ACCESS='sequential',ACTION='write', STATUS='replace', IOSTAT=ierr)
    IF (ierr /= 0) THEN
       CALL FatalError(h//" Open file "//roperm(1:is)//modout//TRIM(fNameAsc)//&
            " returned iostat="//TRIM(ADJUSTL(c0)))
    END IF

    ! open binary file

    fNameBin=namef//labeli//labelc//extn//TRIM(trunc)//TRIM(lev)
    OPEN(UNIT=nfdiag,FILE=roperm(1:is)//modout//TRIM(fNameBin),FORM='unformatted', &
         ACCESS='sequential',ACTION='write', STATUS='replace', IOSTAT=ierr)
    IF (ierr /= 0) THEN
       CALL FatalError(h//" Open file "//roperm(1:is)//modout//TRIM(fNameBin)//&
            " returned iostat="//TRIM(ADJUSTL(c0)))
    END IF

    ! print file name and current time at stdout

    WRITE(c0,"(i8)") ifday
    WRITE(c1,"(f8.1)") tod
    CALL MsgOne(h," writes file "//TRIM(fNameBin)//&
         &" at simulation time")
    line = " "//TRIM(ADJUSTL(c0))//" days and "//&
         &TRIM(ADJUSTL(c1))//" seconds; contains state of the atmosphere at"
    lastUsed = LEN_TRIM(line)
    WRITE(line(lastUsed+1:lastUsed+8), "(' ',i2.2,'Z of ')") idatec(1)
    lastUsed = lastUsed + 8
    WRITE(line(lastUsed+1:lastUsed+10), "(2(i2.2,'/'),i4)") &
            idatec(3), idatec(2), idatec(4)
    lastUsed=lastUsed+10
    CALL MsgOne(h,line(1:lastUsed))

    ! write filenames at directory file

    WRITE(UNIT=nffcst,FMT='(a)')roperm(1:is)//modout//TRIM(fNameAsc)
    WRITE(UNIT=nffcst,FMT='(a)')roperm(1:is)//modout//TRIM(fNameBin)
  END SUBROUTINE opnfct







  SUBROUTINE wrprog (nfdrct,nfdiag,ifday ,tod   ,idate ,idatec,qrot  , &
       qdiv  ,qq    ,qlnp  ,qtmp  ,z0  ,gtsea ,td0   , &
       capac0,w0    ,imask ,temp2m,umes2m,uve10m,vve10m,nffcst,del   , &
       qgzs  ,lsmk  , &
       roperm,namef ,labeli,labelf,extw  ,exdw  ,trunc , &
       lev)
    INTEGER           , INTENT(IN   ) :: nfdrct
    INTEGER           , INTENT(IN   ) :: nfdiag
    INTEGER           , INTENT(IN   ) :: ifday
    REAL(KIND=r8)     , INTENT(IN   ) :: tod
    INTEGER           , INTENT(IN   ) :: idate (:)
    INTEGER           , INTENT(IN   ) :: idatec(:)
    REAL(KIND=r8)     , INTENT(IN   ) :: qlnp  (:)
    REAL(KIND=r8)     , INTENT(IN   ) :: qtmp  (:,:)
    REAL(KIND=r8)     , INTENT(IN   ) :: qdiv  (:,:)
    REAL(KIND=r8)     , INTENT(IN   ) :: qrot  (:,:)
    REAL(KIND=r8)     , INTENT(IN   ) :: qq    (:,:)
    REAL(KIND=r8)     , INTENT(IN   ) :: lsmk  (:)
    REAL(KIND=r8), TARGET, INTENT(IN   ) :: z0  (ibMax,jbMax)
    REAL(KIND=r8), TARGET, INTENT(IN   ) :: temp2m(ibMax,jbMax)
    REAL(KIND=r8), TARGET, INTENT(IN   ) :: umes2m(ibMax,jbMax)
    REAL(KIND=r8), TARGET, INTENT(IN   ) :: uve10m(ibMax,jbMax)
    REAL(KIND=r8), TARGET, INTENT(IN   ) :: vve10m(ibMax,jbMax)
    REAL(KIND=r8), TARGET, INTENT(IN   ) :: gtsea (ibMax,jbMax)
    REAL(KIND=r8)     , INTENT(IN   ) :: td0   (ibMax,jbMax)
    REAL(KIND=r8)     , INTENT(IN   ) :: capac0(ibMax,2,jbMax)
    REAL(KIND=r8)     , INTENT(IN   ) :: w0    (ibMax,3,jbMax)
    INTEGER(KIND=i8)  , INTENT(IN   ) :: imask (ibMax,jbMax)
    INTEGER           , INTENT(IN   ) :: nffcst
    REAL(KIND=r8)     , INTENT(IN   ) :: del   (kMaxNew)
    REAL(KIND=r8)     , INTENT(IN   ) :: qgzs  (:)
    CHARACTER(LEN=200), INTENT(IN   ) :: roperm
    CHARACTER(LEN=  7), INTENT(IN   ) :: namef
    CHARACTER(LEN= 10), INTENT(IN   ) :: labeli
    CHARACTER(LEN= 10), INTENT(IN   ) :: labelf
    CHARACTER(LEN=  5), INTENT(IN   ) :: extw
    CHARACTER(LEN=  5), INTENT(IN   ) :: exdw
    CHARACTER(LEN=  *), INTENT(IN   ) :: trunc
    CHARACTER(LEN=  *), INTENT(IN   ) :: lev

    REAL(KIND=r8), TARGET :: work(ibMax,jbMax,6)
    REAL(KIND=r8) :: qspec(2*mnmax,kmaxnew)
    REAL(KIND=r8) :: qspec1(2*mnmax)
    REAL(KIND=r8) :: tod4
    INTEGER       :: k
    INTEGER       :: ifday4
    INTEGER       :: idat4(4)
    INTEGER       :: idat4c(4)
    INTEGER, ALLOCATABLE :: interp_type(:)
    TYPE(p2d), TARGET, ALLOCATABLE :: fields(:)
    CHARACTER(LEN=*), PARAMETER :: h="**(wrprog)**"

    IF (myid.eq.0) THEN
       CALL opnfct(nfdrct,nfdiag,nffcst,ifday,tod,idate,idatec,&
            roperm,namef,labeli,labelf,extw,exdw,trunc,lev)

       CALL WriteDir(nfdrct, idate, idatec(1), idatec(3), idatec(2),&
            idatec(4), del, tod, ifday)
       ifday4=ifday
       tod4=tod
       DO k=1,4
          idat4(k)=idate(k)
          idat4c(k)=idatec(k)
       ENDDO

       CALL WriteProgHead(nfdiag, ifday4, tod4, idat4, idat4c)
    ENDIF

    ! dumps state of the atmosphere at stdout

    CALL rmsgt(qlnp, qdiv, qtmp, qrot, del, qq, idatec)

    ! write fields at nfdiag

    IF (maxnodes.gt.1) THEN
       !     topography
       !
       IF (havesurf) CALL Collect_Spec(qgzs, qspec1, 1, 1, 0)
       IF (myid.eq.0) THEN
          CALL WriteField(nfdiag, qspec1)
       ENDIF
       !
       !     land sea mask
       !
       IF (myid.eq.0) THEN
          CALL WriteField(nfdiag, lsmk)
       ENDIF
       !
       ! write directory and spectral prognostic fields
       !
       !     ln surface pressure
       !
       IF (havesurf) CALL Collect_Spec(qlnp, qspec1, 1, 1, 0)
       IF (myid.eq.0) THEN
          CALL WriteField(nfdiag, qspec1)
       ENDIF
       !
       !     divergence
       !
       CALL Collect_Spec(qdiv, qspec, kmaxloc, kmaxnew, 0)
       IF (myid.eq.0) THEN
          CALL WriteField(nfdiag, qspec)
       ENDIF
       !
       !     vorticity
       !
       CALL Collect_Spec(qrot, qspec, kmaxloc, kmaxnew, 0)
       IF (myid.eq.0) THEN
          CALL WriteField(nfdiag, qspec)
       ENDIF
       !
       !     specific humidity
       !
       CALL Collect_Spec(qq  , qspec, kmaxloc, kmaxnew, 0)
       IF (myid.eq.0) THEN
          CALL WriteField(nfdiag, qspec)
       ENDIF
       !
       !     virtual temperature
       !
       CALL Collect_Spec(qtmp, qspec, kmaxloc, kmaxnew, 0)
       IF (myid.eq.0) THEN
          CALL WriteField(nfdiag, qspec)
       ENDIF


    ELSE
       !
       CALL WriteField(nfdiag, qgzs)
       CALL WriteField(nfdiag, lsmk)
       CALL WriteField(nfdiag, qlnp)
       CALL WriteField(nfdiag, qdiv)
       CALL WriteField(nfdiag, qrot)
       CALL WriteField(nfdiag, qq)
       CALL WriteField(nfdiag, qtmp)
    ENDIF
    ALLOCATE(fields(12))
    ALLOCATE(interp_type(12))
    !     surface roughness
    !
    fields(1)%p => z0
    interp_type(1) = 1
    !
    !     surface temperature
    !
    fields(2)%p => gtsea
    interp_type(2) = 3
    !
    !     deep soil temperature
    !
    CALL reord (td0,    1, work(:,:,1), 1, imask, gtsea, 'TD  ')
    fields(3)%p => work(:,:,1)
    interp_type(3) = 1
    !
    !     storage on canopy
    !
    CALL reord (capac0, 2, work(:,:,2), 1, imask, gtsea, 'CAPC')
    fields(4)%p => work(:,:,2)
    interp_type(4) = 1
    !
    !     storage on ground cover
    !
    CALL reord (capac0, 2, work(:,:,3), 2, imask, gtsea, 'CAPG')
    fields(5)%p => work(:,:,3)
    interp_type(5) = 1
    !
    !    wetness of surface zone
    !
    CALL reord (w0,     3, work(:,:,4), 1, imask, gtsea, 'W1  ')
    fields(6)%p => work(:,:,4)
    interp_type(6) = 1
    !
    !    wetness of root zone
    !
    CALL reord (w0,     3, work(:,:,5), 2, imask, gtsea, 'W2  ')
    fields(7)%p => work(:,:,5)
    interp_type(7) = 1
    !
    !    wetness of drainage zone
    !
    CALL reord (w0,     3, work(:,:,6), 3, imask, gtsea, 'W3  ')
    fields(8)%p => work(:,:,6)
    interp_type(8) = 1
    !
    !  2-meters surface temperature
    !
    fields(9)%p => temp2m
    interp_type(9) = 1
    !
    !  2-meters specific humidity
    !
    fields(10)%p => umes2m
    interp_type(10) = 1
    !
    !  10-meters zonal wind
    !
    fields(11)%p => uve10m
    interp_type(11) = 1
    !
    !  10-meters meridional wind
    !
    fields(12)%p => vve10m
    interp_type(12) = 1
    !
    !  Collect and print fields
    !
    CALL Collect_Grid_Sur_Print(fields,interp_type,12,0,nfdiag)

    DEALLOCATE(fields)
    DEALLOCATE(interp_type)

!!$    IF(nfctrl(95).GE.1)WRITE(UNIT=nfprt,FMT=5000)idate,ifday,tod,idatec
!!$5000 FORMAT(' DONE WITH WRPROG. MODEL STARTED ',3I3,I5/' NOW AT',I8, &
!!$         ' DAYS AND',F8.1,' SECONDS.  CURRENT DATE IS',3I3,I5)
  END SUBROUTINE wrprog
END MODULE Diagnostics
