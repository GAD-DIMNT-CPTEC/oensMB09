!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE Diagnostics
  USE Constants, ONLY :   &
       ndavl, ndrq, ncdg, jxavl, jxcdg, numx, grav     

  USE Utils, ONLY: &
       tmstmp2,    &
       TransDiagCol, & 
       IJtoIBJB, &
       IBJBtoIJ

  USE Options, ONLY: &
       yrl,   &
       monl,  &
       nfprt, &
       nferr, &
       ifprt 

  USE IOLowLevel, ONLY: &
       WriteField   , &
       WriteDiagHead, &
       WriteProgHead, &
       WriteDir     , &
       WriteDire 

  USE InputOutput, ONLY: &
       transp,           &
       sclout,           &
       cnvray,           &
       aunits


  IMPLICIT NONE

  PRIVATE
  PUBLIC :: InitDiagnostics
  PUBLIC :: pwater
  PUBLIC :: rms
  PUBLIC :: rmsgt
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

  INTEGER, ALLOCATABLE :: SMap(:)
  INTEGER, ALLOCATABLE :: AMap(:)
  INTEGER :: sMapSize
  INTEGER :: aMapSize

  REAL,    PARAMETER :: undef =1.0e53
  INTEGER, PARAMETER :: ngbme = 13
  INTEGER, PARAMETER :: igbme(ngbme)= &
       (/9,10,16,18,19,17,21,23,24,25,28,29,30/)
  CHARACTER(LEN=4), PARAMETER :: ivar(2) = &
       (/'GAUS', 'SPEC'/)
  INTEGER (KIND=SELECTED_INT_KIND(8)) :: ierr
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
  INTEGER, PUBLIC, PARAMETER :: nDiag_nshcrm = 70 ! negative specific humidity correction moisture src.
  INTEGER, PUBLIC, PARAMETER :: nDiag_ozonmr = 71 ! ozone mixing ratio
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

  INTEGER              :: nMax
  INTEGER              :: mMax
  INTEGER              :: mnMax
  INTEGER, ALLOCATABLE :: mnMap(:,:)
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
  REAL             , ALLOCATABLE :: gaus  (:,:,:)  
  REAL             , ALLOCATABLE :: gaus_in(:,:,:)
  REAL             , ALLOCATABLE :: spec  (:,:)
  INTEGER          , ALLOCATABLE :: lspec (:)
  INTEGER          , ALLOCATABLE :: lgaus (:)
  REAL ,             ALLOCATABLE :: dcol  (:)
  REAL ,             ALLOCATABLE :: scol  (:)
  INTEGER,           ALLOCATABLE :: lvrq  (:)


CONTAINS


  SUBROUTINE InitDiagnostics (doprec_in, dodyn_in ,     colrad , &
       mMax_in   , nMax_in  , mnMax_in,  mnMap_in,      iMaxNew_in, jMaxNew_in , &
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
    INTEGER, INTENT(IN)    :: mnMap_in(:,:)
    INTEGER, INTENT(IN)    :: iMaxNew_in
    INTEGER, INTENT(IN)    :: jMaxNew_in
    INTEGER, INTENT(IN)    :: kMaxNew_in
    INTEGER, INTENT(IN)    :: ibMax_in
    INTEGER, INTENT(IN)    :: jbMax_in
    INTEGER, INTENT(IN)    :: ibMaxPerJB_in(:)
    REAL,    INTENT(IN)    :: colrad(jMaxNew_in)
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
    INTEGER                :: diag
    INTEGER                :: ele
    INTEGER                :: l
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
    REAL                   :: pie
    REAL                   :: colb(jMaxNew_in)
    LOGICAL                :: ExistDTable

    mMax    = mMax_in
    nMax    = nMax_in
    mnMax   = mnMax_in   
    iMaxNew = iMaxNew_in
    jMaxNew = jMaxNew_in
    kMaxNew = kMaxNew_in
    ibMax   = ibMax_in
    jbMax   = jbMax_in
    sMapSize = nMax-1
    ALLOCATE(SMap(sMapSize))
    ALLOCATE (mnMap(mMax_in,nMax_in))
    ALLOCATE (ibMaxPerJB(jbMax))
    ibMaxPerJB=ibMaxPerJB_in
    mnMap = mnMap_in

    DO n = 1, nMax-1
       SMap(n) = 2*mnMap(1,n+1)-1
    END DO

    aMapSize = mnMax - nMax
    ALLOCATE(AMap(aMapSize))
    l = 0
    DO diag = 1, nMax 
       DO ele = 2, mMax-diag+1
          m = ele
          n = m+diag-1
          l = l+1
          AMap(l) = 2*mnMap(m,n)
       END DO
    END DO

    !
    !     avail = name of available diagnostic
    !     lvavl = levels in available diagnostic (1 or kmax)
    !     nuavl = unit code of available diagnostic
    !     itavl = type of available diagnostic (1 gaussian, 2 spectral)
    !     jpavl = position in code of available diagnostic (1 gloop/gfidi,
    !             2 gwater, 3 both, 0 neither)

    doprec = doprec_in
    dodyn  = dodyn_in




    ihdim(1)=iMaxNew*jMaxNew
    ihdim(2)=2*mnMax

    ALLOCATE(dcol(jMaxNew))
    ALLOCATE(scol(jMaxNew))
    pie = 4.*ATAN(1.)
    !     
    !     define latitude grid for integration
    !     
    colb(1) = 0.
    DO j = 2, jMaxNew
       colb(j) = 0.5*(colrad(j)+colrad(j-1))
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
    avail(1:85)=(/  &
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
         'VERTICAL DIFFUSION HEATING              ', &
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
         'LONGWAVE CLOUD EMISSIVITY               ', &
         'SHORTWAVE CLOUD OPTICAL DEPTH           ', &
         'CANOPY AIR SPC TO REF. LVL RESISTANCE   ', &
         'CANOPY AIR SPC TO CANOPY RESISTANCE     ', &
         'CANOPY AIR SPC TO GROUND RESISTANCE     ', &
         'CANOPY RESISTANCE                       ', &
         'GROUND COVER RESISTANCE                 ', &
         'BARE SOIL SURFACE RESISTANCE            '/)

    !     lvavl = levels in available diagnostic (1 or 2=kmax)

    lvavl(1:85)=(/ &
         1,    2,    2,    2,    2,    1,    2,    2,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
         2,    2,    2,    2,    2,    2,    2,    2,    2,    2, &
         2,    2,    1,    1,    2,    2,    2,    2,    2,    2, &
         2,    2,    2,    1,    1,    1,    1,    1,    1,    1, &
         1,    1,    1,    1,    1,    1,    1,    1,    1,    2, &
         2,    2,    2,    2,    2,    2,    2,    2,    2,    1, &
         1,    1,    1,    1,    1 /)

    !     nuavl = unit code of available diagnostic

    nuavl(1:85)=(/ &
         132,   50,   50,    0,   40,   40,  153,   50,  120,  120, &
         120,  120,  120,  110,  170,  170,  170,  130,  130, 0, &
         170,  170,  170,  170,  170,  170,  170,  170,  170,  170, &
         70,   70,   70,   50,   70,   50,   70,   50,   70,   50, &
         100,  100,  130,  130,  100,  100,   70,   50,   80,   80, &
         50,   50,   50,  120,  142,  170,  170,  170,  170,  170, &
         170,  170,  170,   40,   40,   40,   40,  131,  170,   50, &
         0, 0,    0,    0,    0, 0,    0,    0,    0,  190, &
         190,  190,  190,  190,  190 /)

    !     itavl = type of available diagnostic (1 gaussian, 2 spectral)

    itavl(1:85)=(/ & 
         1, 2,    2,    2,    2, 1,    1,    1,    1, 1, &
         1, 1,    1,    1,    1, 1,    1,    1,    1, 1, &
         1, 1,    1,    1,    1, 1,    1,    1,    1, 1, &
         1, 1,    1,    1,    1, 1,    1,    1,    1, 1, &
         1, 1,    1,    1,    1, 1,    2,    2,    2, 2, &
         1, 1,    2,    2,    2, 1,    1,    1,    1, 1, &
         1, 1,    1,    1,    1, 1,    1,    1,    1, 1, &
         1, 1,    1,    1,    1, 1,    1,    1,    1, 1, &
         1, 1,    1,    1,    1 /)

    !     jpavl = position in code of available diagnostic (1 gloop/gfidi,
    !             2 gwater, 3 both, 0 neither)

    jpavl(1:85)=(/ & 
         0,    0,    0,    0,   0,    1,    1,    1,    2,    2, &
         2,    2,    1,    1,   1,    1,    1,    1,   1,    1, &
         1,    1,    1,    1,   1,    1,    1,    1,   1,    1, &
         1,    1,    2,    2,   2,    2,    2,    2,   1,    1, &
         1,    1,    1,    1,   1,    1,    0,    0,   0,    0, &
         1,    1,    0,    0,   0,    1,    1,    1,   1,    1, &
         1,    1,    1,    1,   1,    1,    1,    1,   1,    2, &
         1,    1,    1,    1,   1,    1,    1,    1,   1,    1, &
         1,    1,    1,    1,   1 /)

    IF(ifprt(51).GE.1)WRITE(nfprt,30)
    mxavl=0
    DO m=1,ndavl 
       IF (avail(m)(1:5) .NE. "     ") THEN
          IF (lvavl(m) .NE. 1) lvavl(m)=kMaxNew
          IF(ifprt(51).GE.1)WRITE(nfprt,60)avail(m),lvavl(m),nuavl(m), &
               itavl(m),jpavl(m)
          dodia(m)=.FALSE.
          iavrq(m)=0
          mxavl=mxavl+1
       END IF
    END DO

    IF(mxavl.LE.0)THEN
       WRITE(nfprt,6600)
       WRITE(nferr,6600)
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

    IF(ifprt(51).GE.1)WRITE(nfprt,130)
    INQUIRE (file=TRIM(fNameDTable), EXIST=ExistDTable)
    IF (ExistDTable) THEN
       mgaus = 0
       ngaus = 0
       mspec = 0
       nspec = 0
       PRINT*,'* The  ', TRIM(fNameDTable), ' file exists*'
       OPEN(49, file=TRIM(fNameDTable),ACTION="read")  
       mxrq=1
       DO n=1,ndrq
          READ(49,'(A40,3I5)',END=225)reqdg(n),lvrq(n),nurq(n),iclcd(n)
          IF (reqdg(n)(1:5) .NE. "     ")THEN
             IF (lvrq(n) .NE. 1) lvrq(n)=kMaxNew
             IF(ifprt(51).GE.1)WRITE(nfprt,160)reqdg(n),lvrq(n),nurq(n), &
                  iclcd(n)
             irqcf(n)=0
             irqav(n)=0
             irqu(n)=.FALSE.
             mxrq=mxrq+1
             DO nn=1,85
               IF (reqdg(n) == avail(nn)) THEN
                  IF (lvavl(nn) == 1 .AND. itavl(nn) == 1) ngaus=ngaus+1
                  IF (lvavl(nn) == 2 .AND. itavl(nn) == 1) mgaus=mgaus+1
                  IF (lvavl(nn) == 1 .AND. itavl(nn) == 2) ngaus=ngaus+1
                  IF (lvavl(nn) == 2 .AND. itavl(nn) == 2) mgaus=mgaus+1
                  EXIT
               END IF
             END DO
          END IF
       END DO
   225 mxrq=n-1
       CLOSE(49,STATUS='KEEP') 
    ELSE
       mgaus = 1
       ngaus = 21
       mspec = 4
       nspec = 0
       mxrq=20
       PRINT*,'* The  ', TRIM(fNameDTable), ' file does not exist*'
       DO n=1,mxrq
          IF (lvrq(n) .NE. 1) lvrq(n)=kMaxNew
          IF(ifprt(51).GE.1)WRITE(nfprt,160)reqdg(n),lvrq(n),nurq(n),iclcd(n)
          irqcf(n)=0
          irqav(n)=0
          irqu(n)=.FALSE.
       END DO
    END IF
    IF(mxrq.LE.0)THEN
       WRITE(nfprt,7100)
       WRITE(nferr,7100)
       STOP 7100
    END IF

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
             WRITE(nfprt,2100)n,nof
             WRITE(nferr,2100)n,nof
             STOP 2100
          END IF

          iac=iabs(iclcd(n))

          IF(iac.GE.n)THEN
             WRITE(nfprt,2600)iac,n
             WRITE(nferr,2600)iac,n
             STOP 2600
          END IF

          IF(iclcd(iac).NE.0)THEN
             WRITE(nfprt,3100)iac,n,iclcd(iac)
             WRITE(nferr,3100)iac,n,iclcd(iac)
             STOP 3100
          END IF

          IF(reqdg(iac).EQ.reqdg(n))THEN
             WRITE(nfprt,3600)iac,n,reqdg(n)
             WRITE(nferr,3600)iac,n,reqdg(n)
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

       WRITE(nfprt,4100)nn,reqdg(nn)
       WRITE(nferr,4100)nn,reqdg(nn)
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

          WRITE(nfprt,4600)nn,reqdg(nn)
          WRITE(nferr,4600)nn,reqdg(nn)
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

          WRITE(nfprt,5100)n,reqdg(n)
          WRITE(nferr,5100)n,reqdg(n)
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
                   WRITE(nfprt,9100)nuavl(mm),mm,nucf(ix),ix
                   WRITE(nferr,9100)nuavl(mm),mm,nucf(ix),ix
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
                   WRITE(nfprt,9600)nucf(kk),kk,nucf(ix),ix
                   WRITE(nferr,9600)nucf(kk),kk,nucf(ix),ix
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
                WRITE(nfprt,5600)ix,n
                WRITE(nferr,5600)ix,n
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
             WRITE(nfprt,7600)ix,combf(ix),(krcf(mx),mx=ixcf(ix),ja-1)
             WRITE(nferr,7600)ix,combf(ix),(krcf(mx),mx=ixcf(ix),ja-1)
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
             WRITE(nfprt,6100)m,ix
             WRITE(nferr,6100)m,ix
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
                      WRITE(nfprt,6100)m,ix
                      WRITE(nferr,6100)m,ix
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
             WRITE(nfprt,8600)jx,ix,jrcf(jx),jrcf(ix)
             WRITE(nferr,8600)jx,ix,jrcf(jx),jrcf(ix)
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
             WRITE(nfprt,8100)jx,ix
             WRITE(nferr,8100)jx,ix
             STOP 8100
          END IF
       END DO
       inucf(ix)=ia
    END DO

    IF(ifprt(51).GE.2)WRITE(nfprt,1410)
    mm=0
    DO m=1,mxavl
       IF(.NOT.dodia(m))CYCLE
       mm=mm+1
       IF(ifprt(51).GE.2)WRITE(nfprt,1420)mm,m,avail(m)
       IF(ifprt(51).GE.2)WRITE(nfprt,1430)lvavl(m),aunits(nuavl(m)), &
            typcd(itavl(m)),poscd(jpavl(m))
       IF(iavrq(m).NE.0.AND.ifprt(51).GE.2)WRITE(nfprt,1440)iavrq(m)
       IF(kravl(ixavl(m)).LT.0.OR.inavl(m).GT.1)THEN
          ka=ixavl(m)
          kka=ka+inavl(m)-1
          IF(kravl(ka).GT.0)ka=ka+1
          IF(ifprt(51).GE.2)WRITE(nfprt,1450)
          DO kk=ka,kka
             IF(ifprt(51).GE.2)WRITE(nfprt,1460)combf(iabs(kravl(kk)))
          END DO
       END IF
    END DO
    IF(ifprt(51).GE.2)WRITE(nfprt,1510)
    DO n=1,mxrq
       IF(ifprt(51).GE.2)WRITE(nfprt,1520)n,reqdg(n)
       IF(ifprt(51).GE.2)WRITE(nfprt,1530)lvrq(n),aunits(nurq(n)), &
            irqcf(n)
       IF(irqav(n).GT.0)THEN
          IF(iclcd(n) < 0 ) THEN
             IF(ifprt(51).GE.2)WRITE(nfprt,1545)irqav(n),reqdg(iabs(iclcd(n)))
          ELSE IF (iclcd(n) .EQ. 0 ) THEN
             IF(ifprt(51).GE.2)WRITE(nfprt,1555)irqav(n)
          ELSE
             IF(ifprt(51).GE.2)WRITE(nfprt,1565)irqav(n),reqdg(iclcd(n))
          END IF
       ELSE IF(irqav(n).LT.0)THEN
          irix=iabs(irqav(n))
          IF(iclcd(n).LT.0)THEN
             IF(ifprt(51).GE.2)WRITE(nfprt,1575)irix,reqdg(iabs(iclcd(n)))
          ELSE
             IF(ifprt(51).GE.2)WRITE(nfprt,1595)irix,reqdg(iclcd(n))
          END IF
       END IF
    END DO
    IF(icf.NE.0)THEN
       IF(ifprt(51).GE.3)WRITE(nfprt,1610)
       DO ix=1,icf
          IF(ifprt(51).GE.3)WRITE(nfprt,1620)ix,combf(ix)
          IF(ifprt(51).GE.3)WRITE(nfprt,1630)lvcf(ix),aunits(nucf(ix)), &
               kfrq(ix),poscd(jpcf(ix))
          IF(ifprt(51).GE.3)WRITE(nfprt,1640)combf(ix)
          ka=ixcf(ix)
          DO kk=ka,incf(ix)+ka-1
             kx=krcf(kk)
             IF(kx.GT.0)ocf=avail(kx)
             IF(kx.LT.0)ocf=combf(-kx)
             jx=jrcf(kk)
             IF(iclcd(jx).LT.0)THEN
                IF(ifprt(51).GE.3)WRITE(nfprt,1650)ocf,kx,jx
             ELSE IF(iclcd(jx).GT.0)THEN
                IF(ifprt(51).GE.3)WRITE(nfprt,1660)ocf,kx,jx
             END IF
          END DO
          IF(icfu(ix))THEN
             IF(ifprt(51).GE.3)WRITE(nfprt,1670)
             ka=ixucf(ix)
             DO kk=ka,inucf(ix)+ka-1
                kx=iabs(jrucf(kk))
                IF(ifprt(51).GE.3)WRITE(nfprt,1680)combf(kx)
             END DO
          END IF
       END DO
    END IF

    !     set diagnostic accumulators

    CALL setdia()

    pfbar=iavrq(1).NE.0.OR.iavrq(2).NE.0.OR.iavrq(3).NE.0.OR. &
         iavrq(4).NE.0.OR.iavrq(5).NE.0

30  FORMAT(//'0AVAILABLE DIAGNOSTIC INPUT DECK'/)
50  FORMAT(A40,4I5)
60  FORMAT(' ',A40,4I5)
130 FORMAT(//'0DESIRED DIAGNOSTIC INPUT DECK'/)
150 FORMAT(A40,3I5)
160 FORMAT(' ',A40,3I5)
920 FORMAT(A32)
925 FORMAT(A16)
930 FORMAT(5E16.8)
940 FORMAT(20I4)
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
    REAL,    INTENT(IN   ) :: q(nx,nq)
    REAL,    INTENT(INOUT) :: pwtr(nx)
    REAL,    INTENT(IN   ) :: ps(nx)
    REAL,    INTENT(IN   ) :: del(nq)  

    REAL    :: fac
    INTEGER :: i
    INTEGER :: k

    fac = 1.0e3/grav

    DO i = 1,mx
       pwtr(i) = 0.0
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



  SUBROUTINE hbartr (f, sfgbar ,afgbar)
    !
    !
    !
    !***********************************************************************
    !
    !    argument(dimensions)                       description
    !
    !        f(2,mnwv0)               input : spectral representation of a
    !                                         global field.
    !        g(2,mnwv0)               input : spectral representation of a
    !                                         global field.
    !         sfgbar                 output : zonally symmetric product
    !                                         of the two global fields.
    !
    !         afgbar                 output : zonally asymmetric product
    !                                         of the two global fields.
    !
    !***********************************************************************
    !
    !
    !
    REAL,    INTENT(IN ) :: f(2*mnMax)
    REAL,    INTENT(OUT) :: sfgbar
    REAL,    INTENT(OUT) :: afgbar
    INTEGER :: l
    sfgbar = 0.0
    DO l = 1, sMapSize
       sfgbar=sfgbar + f(SMap(l))*f(SMap(l))*0.5
    END DO
    afgbar = 0.0
    DO l = 1, aMapSize
       afgbar=afgbar + f(AMap(l)-1)*f(AMap(l)-1) + f(AMap(l))*f(AMap(l))
    END DO
  END SUBROUTINE hbartr






  SUBROUTINE hbartr2D (f, sfgbar ,afgbar)
    REAL,    INTENT(IN ) :: f(2*mnMax, kMaxNew)
    REAL,    INTENT(OUT) :: sfgbar(kMaxNew)
    REAL,    INTENT(OUT) :: afgbar(kMaxNew)
    INTEGER :: l
    INTEGER :: k
    sfgbar = 0.0
    DO k = 1, kMaxNew
       DO l = 1, sMapSize
          sfgbar(k) = sfgbar(k) + f(SMap(l),k)*f(SMap(l),k)*0.5
       END DO
    END DO
    afgbar = 0.0
    DO k = 1, kMaxNew
       DO l = 1, aMapSize
          afgbar(k) = afgbar(k) + f(AMap(l)-1,k)*f(AMap(l)-1,k) + &
               f(AMap(l),k)*f(AMap(l),k)
       END DO
    END DO
  END SUBROUTINE hbartr2D



  !     rms : calculates the standard deviation of one global field at
  !           one level and three global fields at all vertical levels
  !           from their spectral representations.  a pressure-weighted
  !           mean is also calculated for the latter three fields.  the
  !           results are printed out.  note : global means are not
  !           removed in this calculation.



  SUBROUTINE rms(q,x,y,z)
    !
    !
    !***********************************************************************
    !
    !     rms is called by the subroutine gnmini.
    !
    !     rms calls no subroutines.
    !
    !***********************************************************************
    !
    !    argument(dimensions)                       description
    !
    !        q(2,mnwv0)               input : one level of a global field
    !                                       (spectral).
    !        x(2,mnwv0,kdim)          input : all levels of a global field
    !                                       (spectral).
    !        y(2,mnwv0,kdim)          input : all levels of a global field
    !                                       (spectral).
    !        z(2,mnwv0,kdim)          input : all levels of a global field
    !                                       (spectral).
    !
    !***********************************************************************
    !
    !
    REAL, INTENT(IN) :: q(2,mnMax)
    REAL, INTENT(IN) :: x(2,mnMax,kMaxNew)
    REAL, INTENT(IN) :: y(2,mnMax,kMaxNew)
    REAL, INTENT(IN) :: z(2,mnMax,kMaxNew)
    REAL :: d(kMaxNew)
    REAL :: v(kMaxNew)
    REAL :: t(kMaxNew)
    REAL :: dt
    REAL :: qt
    REAL :: vt
    REAL :: tt
    REAL :: a
    REAL :: f
    INTEGER :: lev
    INTEGER :: ll
    INTEGER :: nn
    INTEGER :: lx
    dt=0.0
    qt=0.0
    vt=0.0
    tt=0.0
    DO lev=1,kMaxNew
       d(lev)=0.0
       v(lev)=0.0
       t(lev)=0.0
       a=1.0
       DO ll=1,mMax
          DO nn=ll,nMax
             lx=mnMap(ll,nn)
             d(lev)=d(lev)+a*(x(1,lx,lev)**2+x(2,lx,lev)**2)
             v(lev)=v(lev)+a*(y(1,lx,lev)**2+y(2,lx,lev)**2)
             t(lev)=t(lev)+a*(z(1,lx,lev)**2+z(2,lx,lev)**2)
          END DO
          a=2.0
       END DO
       dt=dt+d(lev)
       vt=vt+v(lev)
       tt=tt+t(lev)
       d(lev)=SQRT(d(lev)/2.0)
       v(lev)=SQRT(v(lev)/2.0)
       t(lev)=SQRT(t(lev)/2.0)
    END DO
    a=1.0
    DO ll=1,mMax
       DO nn=ll,nMax
          lx=mnMap(ll,nn)
          qt=qt+a*(q(1,lx)**2+q(2,lx)**2)
       END DO
       a=2.0
    END DO
    qt=SQRT(qt/2.0)
    f=2*kMaxNew
    dt=dt/f
    vt=vt/f
    tt=tt/f
    dt=SQRT(dt)
    vt=SQRT(vt)
    tt=SQRT(tt)
    IF (ifprt(63) >= 1) THEN
       WRITE(nfprt,100) dt,(d(ll),ll=1,kMaxNew)
       WRITE(nfprt,110) vt,(v(ll),ll=1,kMaxNew)
       WRITE(nfprt,120) tt,(t(ll),ll=1,kMaxNew)
       WRITE(nfprt,130) qt
    END IF
100 FORMAT(' DIV',9E12.4)
110 FORMAT(' VORT',9E12.4)
120 FORMAT(' TEMP',9E12.4)
130 FORMAT(' Q',E15.6)
  END SUBROUTINE rms





  SUBROUTINE rmsgt(q,x,y,w,del,r)
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
    REAL, INTENT(IN) :: q(2*mnMax)
    REAL, INTENT(IN) :: x(2*mnMax, kMaxNew)
    REAL, INTENT(IN) :: y(2*mnMax, kMaxNew)
    REAL, INTENT(IN) :: del(kMaxNew)
    REAL, INTENT(IN) :: w(2*mnMax, kMaxNew)
    REAL, INTENT(IN) :: r(2*mnMax, kMaxNew)

    REAL :: ax(kMaxNew)
    REAL :: ay(kMaxNew)
    REAL :: aw(kMaxNew)
    REAL :: ar(kMaxNew)
    REAL :: sa(kMaxNew)
    REAL :: sy(kMaxNew)
    REAL :: sw(kMaxNew)
    REAL :: sr(kMaxNew)
    REAL :: gx(kMaxNew)
    REAL :: gy(kMaxNew)
    REAL :: gw(kMaxNew)
    REAL :: gr(kMaxNew)
    REAL :: gmr
    REAL :: gmx
    REAL :: gmy
    REAL :: gmw
    REAL :: sq
    REAL :: aq
    REAL :: gmq
    REAL :: svr
    REAL :: svx
    REAL :: svy
    REAL :: svw
    REAL :: avr
    REAL :: avx
    REAL :: avy
    REAL :: avw
    REAL :: sq2o2
    INTEGER :: k
    INTEGER :: l

    sq2o2=SQRT(2.0)/2.0
    gmr=0.0 
    gmx=0.0 
    gmy=0.0 
    gmw=0.0 
    svr=0.0 
    svx=0.0 
    svy=0.0 
    svw=0.0 
    avr=0.0 
    avx=0.0
    avy=0.0
    avw=0.0
    CALL hbartr2D(x,sa,ax)
    CALL hbartr2D(y,sy,ay)
    CALL hbartr2D(w,sw,aw)
    DO k=1, kMaxNew
       svx=svx+sa(k)*del(k)
       svy=svy+sy(k)*del(k)
       svw=svw+sw(k)*del(k)
       avx=avx+ax(k)*del(k)
       avy=avy+ay(k)*del(k)
       avw=avw+aw(k)*del(k)
       gx(k)=x(1,k)*sq2o2
       gy(k)=y(1,k)*sq2o2
       gw(k)=w(1,k)*sq2o2
       gmx=gmx+gx(k)*del(k)
       gmy=gmy+gy(k)*del(k)
       gmw=gmw+gw(k)*del(k)
       sa(k)=SQRT(sa(k))
       sy(k)=SQRT(sy(k))
       sw(k)=SQRT(sw(k))
       ax(k)=SQRT(ax(k))
       ay(k)=SQRT(ay(k))
       aw(k)=SQRT(aw(k))
    END DO

    CALL hbartr2D(r,sr,ar)
    DO k=1, kMaxNew
       svr=svr+sr(k)*del(k)
       avr=avr+ar(k)*del(k)
       gr(k)=r(1,k)*sq2o2
       gmr=gmr+gr(k)*del(k)
       sr(k)=SQRT(sr(k))
       ar(k)=SQRT(ar(k))
    END DO

    CALL hbartr(q,sq,aq)
    gmq=q(1)*sq2o2
    sq=SQRT(sq)
    aq=SQRT(aq)
    IF(gmx /= 0.0 .OR. gmw /= 0.0)THEN
       IF(ifprt(67) >= 1) THEN
          WRITE(nfprt,80)gmx,gmw
       END IF
       RETURN
    END IF
    IF(ifprt(67) >= 1) THEN
       WRITE(nfprt,25)
    END IF
    DO k=1,kMaxNew
       l=kMaxNew+1-k
       IF(l <= kMaxNew)THEN
          IF(ifprt(67).GE.1)WRITE(nfprt,30)l,sa(l),ax(l),sw(l),aw(l), &
               gy(l),sy(l),ay(l),gr(l),sr(l),ar(l)
       ELSE
          IF(ifprt(67).GE.1)WRITE(nfprt,35)l,sa(l),ax(l),sw(l),aw(l), &
               gy(l),sy(l),ay(l)
       END IF
    END DO
    svr=SQRT(svr) 
    svx=SQRT(svx) 
    svy=SQRT(svy) 
    svw=SQRT(svw) 
    avr=SQRT(avr) 
    avx=SQRT(avx)
    avy=SQRT(avy)
    avw=SQRT(avw)
    IF(ifprt(67) >= 1) THEN
       WRITE(nfprt,45)
       WRITE(nfprt,50)svx,avx,svw,avw,gmy,svy,avy,gmr,svr,avr
       WRITE(nfprt,55)
       WRITE(nfprt,60)gmq,sq,aq
       WRITE(nfprt,65)
    END IF
25  FORMAT(30X,'LAYER MEANS AND VARIANCES'/' LYR ',' Z.S. DIV. ', &
         ' Z.A. DIV. ',' Z.S. VOR. ',' Z.A. VOR. ', &
         ' G.M. TEM. ',' Z.S. TEM. ',' Z.A. TEM. ', &
         ' G.M. S.H. ',' Z.S. S.H. ',' Z.A. S.H. ')
30  FORMAT(I4,1P,4G11.3,0P,3(2X,F7.2,2X),1P,3G11.3)
35  FORMAT(I4,1P,4G11.3,0P,3(2X,F7.2,2X))
45  FORMAT(/30X,'VERTICAL MEANS AND VARIANCES'/5X,' Z.S. DIV. ', &
         ' Z.A. DIV. ',' Z.S. VOR. ',' Z.A. VOR. ', &
         ' G.M. TEM. ',' Z.S. TEM. ',' Z.A. TEM. ', &
         ' G.M. S.H. ',' Z.S. S.H. ',' Z.A. S.H. ')
50  FORMAT(4X,1P,4G11.3,0P,3(2X,F7.2,2X),1P,3G11.3)
55  FORMAT(/30X,'LOG SFC PRESSURE MEAN AND VARIANCES'/ &
         '  G.M. LNP. ','  Z.S. LNP. ',' Z.A. LNP. ')
60  FORMAT(1X,F9.5,2X,1P,2G11.3)
65  FORMAT(/30X,'END RMSGT')
80  FORMAT(' GMX=',G11.4,' GMW=',G11.4,' NOT ZERO')
  END SUBROUTINE rmsgt





  SUBROUTINE globme(a, z, dthet, costhe, cf, title, nufr, nuto)
    !
    ! globme :perfoms zonal and global mean.
    !
    !
    !     find global mean of a
    !
    REAL             , INTENT(IN   ) :: a(iMaxNew,jMaxNew)
    REAL             , INTENT(OUT  ) :: z(jMaxNew)
    REAL             , INTENT(IN   ) :: dthet(jMaxNew)
    REAL             , INTENT(IN   ) :: costhe(jMaxNew)
    REAL             , INTENT(IN   ) :: cf   
    CHARACTER(LEN=40), INTENT(IN   ) :: title
    INTEGER          , INTENT(IN   ) :: nufr   
    INTEGER          , INTENT(IN   ) :: nuto
    INTEGER                          :: i
    INTEGER                          :: j
    REAL                             :: gm
    REAL  :: work(iMaxNew,jMaxNew)


    DO j=1,jMaxNew
       DO i=1,iMaxNew
          work(i,j)=a(i,j)*cf
       END DO
    END DO
    CALL cnvray(work  ,iMaxNew*jMaxNew, nufr, nuto)
    !
    !     zonal mean
    !
    DO j=1,jMaxNew
       z(j) = 0.
       DO i=1,iMaxNew
          z(j) = z(j) + work(i,j)
       END DO
       z(j) = z(j)/float(iMaxNew)
    END DO
    !
    !     integral with latitude
    !
    gm = 0.
    DO j=1,jMaxNew
       gm = gm + z(j)*costhe(j)*dthet(j)
    END DO
    gm = .5 * gm
    IF(ifprt(31).GE.1)WRITE(nfprt,70)title,aunits(nuto),gm
    !
    !     global mean standard deviation
    !
    DO j=1,jMaxNew
       DO i=1,iMaxNew
          work(i,j)=(work(i,j)-gm)*(work(i,j)-gm)
       END DO
    END DO

    DO j=1,jMaxNew
       z(j) = 0.
       DO i=1,iMaxNew
          z(j) = z(j) + work(i,j)
       END DO
       z(j) = z(j)/float(iMaxNew)
    END DO
    !
    !     integral with latitude
    !
    gm = 0.
    DO j=1,jMaxNew
       gm = gm + z(j)*costhe(j)*dthet(j)
    END DO
    gm = SQRT(.5 * gm)
    WRITE(6,71)title,aunits(nuto),gm
70  FORMAT(' ',A40,' IN UNITS OF ',A16,1X,G16.8,' G.M.')
71  FORMAT(' ',A40,' IN UNITS OF ',A16,1X,G16.8,' S.D.')
  END SUBROUTINE globme












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
    mxspec=mspec*kMaxNew+nspec

    ALLOCATE (spec(2*mnMax, mxspec))
    spec = 0.0

    ALLOCATE (gaus(ibMax, mxgaus, jbMax))
    gaus = 0.0

    ALLOCATE (gaus_in(iMaxNew, mxgaus, jMaxNew))
    gaus_in = 0.0

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
             WRITE(nfprt,20100)igaus,m,ix
             WRITE(nferr,20100)igaus,m,ix
             STOP 20100
          END IF
       ELSE IF(itavl(m).EQ.2)THEN
          nf=nf+1
          lspec(m)=ispec+1
          ispec=ispec+lvavl(m)
          IF(ispec.GT.mxspec)THEN
             WRITE(nfprt,20600)ispec,m,ix
             WRITE(nferr,20600)ispec,m,ix
             STOP 20600
          END IF
       END IF
    END DO

    IF(nf+icf.NE.nof)THEN
       WRITE(nfprt,21100)nf,icf,nof
       WRITE(nferr,21100)nf,icf,nof
       STOP 21100
    END IF

    IF(ifprt(71).GE.1)WRITE(nfprt,200)nf,ispec,igaus
    IF(icf.NE.0)THEN
       ish=ispec
       igh=igaus
       DO ix=-1,-icf,-1
          IF(itcf(-ix).EQ.1)THEN
             nf=nf+1
             lgaus(ix)=igaus+1
             igaus=igaus+lvcf(-ix)
             IF(igaus.GT.mxgaus)THEN
                WRITE(nfprt,20100)igaus,m,ix
                WRITE(nferr,20100)igaus,m,ix
                STOP 20100
             END IF
          ELSE IF(itcf(-ix).EQ.2)THEN
             nf=nf+1
             lspec(ix)=ispec+1
             ispec=ispec+lvcf(-ix)
             IF(ispec.GT.mxspec)THEN
                WRITE(nfprt,20600)ispec,m,ix
                WRITE(nferr,20600)ispec,m,ix
                STOP 20600
             END IF
          END IF
       END DO
       IF(ifprt(71).GE.1)WRITE(nfprt,400)icf,ispec-ish,igaus-igh
    END IF
    nw=ispec*2*mnMax+igaus*ibMax*jbMax
    IF(ifprt(71).GE.1)WRITE(nfprt,500)nf,ispec,igaus,nw

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





  SUBROUTINE upspec(field,loca)
    !
    ! upspec :extended diagnostics version 1 diagnostic field accumulator
    !            routine.  memory resident version.  see subroutine indiag for
    !            further discussion.
    !            for spectral fields only called for entire field
    !     
    REAL,    INTENT(in) :: field(2*mnMax,kMaxNew)
    INTEGER, INTENT(in) :: loca

    REAL :: hold(2*mnMax, kMaxNew)
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
       WRITE(nfprt,3180)loca
       WRITE(nferr,3180)loca
       STOP 3180
    END IF
    IF(itavl(loca).NE.2)THEN
       WRITE(nfprt,4180)itavl(loca)
       WRITE(nferr,4180)itavl(loca)
       STOP 4180
    END IF
    lvl=lvavl(loca)
    ka=ixavl(loca)
    !
    !     case for directly saved fields
    !     
    IF(iavrq(loca).GT.0)THEN
       kg=lspec(loca)
       DO l=1,lvl
          ll=kg+l-1
          DO i=1,2*mnMax
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
          WRITE(nfprt,3680)loca,jcf,kk,ja,jja
          WRITE(nferr,3680)loca,jcf,kk,ja,jja
          STOP 3680
          !
          !     treat each accumulation according the the sign of the desired
          !     calculation code (iclcd)
          !
200       CONTINUE
          DO l=1,lvl
             DO i=1,2*mnMax
                hold(i,l)=field(i,l)
             END DO
          END DO
          CALL cnvray(hold, 2*mnMax*lvl, nuavl(loca), nucf(jcf))
          IF(iclcd(jx).LT.0)THEN
             kg=lspec(kcf)
             DO l=1,lvl
                ll=kg+l-1
                DO i=1,2*mnMax
                   spec(i,ll)=spec(i,ll)-hold(i,l)
                END DO
             END DO
          ELSE
             kg=lspec(kcf)
             DO l=1,lvl
                ll=kg+l-1
                DO i=1,2*mnMax
                   spec(i,ll)=spec(i,ll)+hold(i,l)
                END DO
             END DO
          END IF
       END DO
    END IF
4555 FORMAT(' CONVERSION ERROR IN UPDIA.  ERROR=',I3,' NUAVL=',I5, &
         ' NUCF=',I5/' A.D. NO.=',I3,' C.F. NO.=',I3,' A.D. INDEX=', &
         I3)
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
    IF (ispec > 0) THEN
       spec = 0.0
    END IF
    IF (igaus > 0) THEN
       gaus = 0.0
    END IF
  END SUBROUTINE rsdiag





  SUBROUTINE accpf(ifday, tod, qtmpp, qrotp, qdivp, qqp, qlnpp, nfdyn)
    !
    ! accpf  :extended diagnostics version 1 prognostic field accumulator
    !   routine;  memory resident version;  see subroutine indiag for
    !   further discussion.
    !
    INTEGER, INTENT(IN   ) :: ifday
    REAL   , INTENT(IN   ) :: tod 
    REAL   , INTENT(IN   ) :: qtmpp (2*mnMax, kMaxNew)
    REAL   , INTENT(IN   ) :: qrotp (2*mnMax, kMaxNew)
    REAL   , INTENT(IN   ) :: qdivp (2*mnMax, kMaxNew)
    REAL   , INTENT(IN   ) :: qqp   (2*mnMax, kMaxNew)  
    REAL   , INTENT(IN   ) :: qlnpp (2*mnMax)
    INTEGER, INTENT(IN   ) :: nfdyn


    REAL                   :: qwk4(2*mnMax)
    REAL                   :: qwka(2*mnMax,5)
    INTEGER                :: lo
    INTEGER                :: l
    INTEGER                :: lv
    INTEGER                :: mn
    INTEGER                :: k

    IF (pfbar) THEN

       IF(dodia(nDiag_tmdivg).AND.iavrq(nDiag_tmdivg).GT.0)THEN
          lo = lspec(nDiag_tmdivg) - 1
          DO l = 1, kMaxNew
             lv = lo + l
             DO mn = 1, 2*mnMax
                spec(mn,lv) = spec(mn,lv) + qdivp(mn,l)
             END DO
          END DO
       END IF

       IF(dodia(nDiag_tmvort).AND.iavrq(nDiag_tmvort).GT.0)THEN
          lo = lspec(nDiag_tmvort) - 1
          DO l = 1, kMaxNew
             lv = lo + l
             DO mn = 1, 2*mnMax
                spec(mn,lv) = spec(mn,lv) + qrotp(mn,l)
             END DO
          END DO
       END IF

       IF(dodia(nDiag_tmtvir).AND.iavrq(nDiag_tmtvir).GT.0)THEN
          lo = lspec(nDiag_tmtvir) - 1
          DO l = 1, kMaxNew
             lv = lo + l
             DO mn = 1, 2*mnMax
                spec(mn,lv) = spec(mn,lv) + qtmpp(mn,l)
             END DO
          END DO
       END IF

       IF (dodia(nDiag_tmsphu) .AND. (iavrq(nDiag_tmsphu).GT.0)) THEN
          lo = lspec(nDiag_tmsphu) - 1
          DO l = 1, kMaxNew
             lv = lo + l
             DO mn = 1, 2*mnMax
                spec(mn,lv) = spec(mn,lv) + qqp  (mn,l)
             END DO
          END DO
       END IF

       ! note that the time mean surface pressure is now computed 
       ! in gloop in place of the time mean ln surface pressure.
       ! this is the time mean log surface pressure saved separately.

       IF (dodia(nDiag_tmlnps) .AND. (iavrq(nDiag_tmlnps).GT.0)) THEN
          lo = lspec(nDiag_tmlnps)
          DO mn = 1, 2*mnMax
             spec(mn,lo) = spec(mn,lo) + qlnpp(mn)
          END DO
       END IF
    END IF

    IF (dodyn) THEN      

       WRITE (6,'(A,I5,A,F15.2,A)')' ifday=',ifday,' tod=',tod,' dyn'
       WRITE (nfdyn) ifday, tod 

       DO mn = 1, 2*mnMax
          qwka(mn,1) = qdivp(mn,1)
          qwka(mn,2) = qrotp(mn,1)
          qwka(mn,3) = qtmpp(mn,1)
          qwka(mn,4) = qqp(mn,1)
          qwka(mn,5) = qlnpp(mn)
       END DO

       CALL transp(qwka,5,1)

       DO k = 1, 5
          DO mn = 1, 2*mnMax
             qwk4(mn) = qwka(mn,k)
          END DO
          WRITE (nfdyn) qwk4
       END DO

    END IF

  END SUBROUTINE accpf



  SUBROUTINE Prec_Diag (ifday, tod, prct, prcc, nfprc)
    !
    ! Diagnostic of Total Precipitation (prct) and
    !               Convective Precipitation (prcc).
    ! Large Scale Precipitation can be obtained by: prct-prcc
    !
    INTEGER, INTENT(IN) :: ifday
    REAL   , INTENT(IN) :: tod
    REAL   , INTENT(IN) :: prct (ibMax,jbMax)
    REAL   , INTENT(IN) :: prcc (ibMax,jbMax)
    INTEGER, INTENT(IN) :: nfprc

    REAL :: aux3 (ibMax*jbMax)
    INTEGER :: ij, j, i

    IF (doprec) THEN
       CALL WriteDiagHead (nfprc, ifday, tod)
       ij=1
       DO j=1,jbMax
          DO i=1,ibMaxPerJB(j)
             aux3(ij)=prct(i,j)
             ij=ij+1
          END DO
       END DO
       CALL WriteField (nfprc, aux3)
       ij=1
       DO j=1,jbMax
          DO i=1,ibMaxPerJB(j)
             aux3(ij)=prcc(i,j)
             ij=ij+1
          END DO
       END DO
       CALL WriteField (nfprc, aux3)
    END IF

  END SUBROUTINE Prec_Diag



  SUBROUTINE getgau(field, jj)
    !
    !
    ! getgau :extended diagnostics version 1 diagnostic field fetching routine
    !         memory resident version;  see subroutine indiag for further
    !         discussion; used for gaussian fields only
    !
    !
    !     version 1 diagnostic accumulators
    !
    REAL   , INTENT(OUT) :: field(iMaxNew,jMaxNew,kMaxNew)
    INTEGER, INTENT(IN ) :: jj 

    INTEGER :: kg  
    INTEGER :: lvl
    INTEGER :: j
    INTEGER :: i
    INTEGER :: l 
    INTEGER :: ll

    IF (jj == 0) THEN
       WRITE(nfprt,3270)
       WRITE(nferr,3270)
       STOP 3270
    ELSE IF (jj >  0) THEN
       lvl=lvavl(jj)
    ELSE IF (jj <  0) THEN
       lvl=lvcf(-jj)
    END IF
    kg=lgaus(jj)
    DO j = 1, jMaxNew
       DO l = 1, lvl
          ll = kg+l-1
          DO i = 1, iMaxNew
             field(i,j,l)=gaus_in(i,ll,j)
          END DO
       END DO
    END DO
3270 FORMAT(' ERROR IN CALLING GETGAU WITH NO SET LOCATION')
  END SUBROUTINE getgau






  SUBROUTINE wridia (iudiag, maxstp)
    !
    ! wridia :writes kistler/katz/schneider diagnostic fields on disk.
    !
    INTEGER, INTENT(IN) :: iudiag
    INTEGER, INTENT(IN) :: maxstp

    REAL    :: gwork(iMaxNew,jMaxNew,kMaxNew)
    REAL    :: zonmn(jMaxNew)
    LOGICAL :: fgm
    REAL    :: f1
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
    f1=1.0/float(maxstp)
    fgm=.TRUE.
    DO k=1,mxgaus
       CALL IBJBtoIJ(gaus(:,k,:),gaus_in(:,k,:))
    END DO
    !     
    !     directly available fields
    !     
    DO m=1,mxavl
       IF ((dodia(m)) .AND. (iavrq(m) > 0)) THEN
          nn=iavrq(m)
          IF (itavl(m) == 2) THEN
             mm=lspec(m)
             CALL sclout(iudiag, spec(1,mm), ihdim(itavl(m)), lvavl(m), f1, &
                  ivar(itavl(m)), nuavl(m), nurq(nn))
          ELSE
             CALL getgau(gwork, m)
             CALL sclout(iudiag, gwork, ihdim(itavl(m)), lvavl(m), &
                  f1, ivar(itavl(m)), nuavl(m), nurq(nn))
             DO ii=1,ngbme
                IF (igbme(ii) == m) GO TO 250
             END DO
             CYCLE
250          IF (fgm .AND. ifprt(31) >= 1)WRITE(nfprt,300)
             IF (fgm .AND. dodia(17) .AND. ifprt(31) >= 2)WRITE(nfprt,350)
             CALL globme(gwork, zonmn, dcol, scol, f1, avail(m), &
                  nuavl(m), nurq(nn))
             fgm=.FALSE.
          END IF
       END IF
    END DO
    IF (.NOT. fgm .AND. ifprt(31) >= 1) WRITE(nfprt,1010) 
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
                DO j=1,jMaxNew
                   DO kk=ka,inucf(ix)+ka-1
                      kx=jrucf(kk)
                      jx=iabs(kx)
                      ji=lgaus(-jx)
                      DO l=1,lvcf(jx)
                         IF (kx > 0) THEN
                            DO i=1,iMaxNew
                               gaus_in(i,l+ji-1,j)=gaus_in(i,l+ji-1,j)+ &
                                    gaus_in(i,l+ki-1,j)
                            END DO
                         ELSE
                            DO i=1,iMaxNew
                               gaus_in(i,l+ji-1,j)=gaus_in(i,l+ji-1,j)- &
                                    gaus_in(i,l+ki-1,j)
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
                         DO i=1,2*mnMax
                            spec(i,l+ji-1)=spec(i,l+ji-1)+ &
                                 spec(i,l+ki-1)
                         END DO
                      ELSE
                         DO i=1,2*mnMax
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
             CALL sclout(iudiag,spec(1,mm),ihdim(itcf(ix)),lvcf(ix), &
                  f1,ivar(itcf(ix)),nucf(ix),nucf(ix))
          ELSE IF (itcf(ix) == 1)THEN
             CALL getgau(gwork, -ix)
             CALL sclout(iudiag,gwork,ihdim(itcf(ix)),lvcf(ix), &
                  f1,ivar(itcf(ix)),nucf(ix),nucf(ix))
          END IF
       END DO
    END IF
    gaus_in=0.0
300 FORMAT(//'0GLOBAL MEAN DIAGNOSTICS'//)
350 FORMAT(' NOTE: TO COMPUTE EVAPORATION IN MM/DAY FROM LATENT '  &
         ,'HEATING DIVIDE BY 28.9'//)
1010 FORMAT(//)
  END SUBROUTINE wridia






  SUBROUTINE updia(field, loca, lat)
    !
    ! updia  :extended diagnostics version 1 diagnostic field accumulator
    !         subroutine; memory resident version;
    !         see subroutine indiag for further discussion;
    !         for gaussian fields only called one gaussian latitude at a time.
    !
    IMPLICIT NONE
    REAL,    INTENT(in   ) :: field(ibMax, *)
    INTEGER, INTENT(in   ) :: loca
    INTEGER, INTENT(in   ) :: lat
    REAL     :: hold(ibMax,kMaxNew)
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
       WRITE(nfprt,3180)loca
       WRITE(nferr,3180)loca
       STOP 3180
    END IF

    IF (itavl(loca) /= 1) THEN
       WRITE(nfprt,4180)itavl(loca)
       WRITE(nferr,4180)itavl(loca)
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
          DO i = 1, ibMaxPerJB(lat)
             gaus(i,ll,lat)=gaus(i,ll,lat)+field(i,l)
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

          WRITE(nfprt,3680)loca,jcf,kk,ja,jja
          WRITE(nferr,3680)loca,jcf,kk,ja,jja

          STOP 3680
          !    
          !    treat each accumulation according the the sign of the desired
          !    calculation code (iclcd)
          !    
200       CONTINUE

          DO l = 1, lvl
             DO i = 1, ibMaxPerJB(lat)
                hold(i,l)=field(i,l)
             END DO
          END DO

          CALL cnvray(hold,imkm,nuavl(loca),nucf(jcf))

          IF (iclcd(jx) < 0) THEN
             kg=lgaus(kcf)
             DO l = 1, lvl
                ll=kg+l-1
                DO i = 1, ibMaxPerJB(lat)
                   gaus(i,ll,lat)=gaus(i,ll,lat)-hold(i,l)
                END DO
             END DO
          ELSE
             kg=lgaus(kcf)
             DO l = 1, lvl
                ll=kg+l-1
                DO i = 1, ibMaxPerJB(lat)
                   gaus(i,ll,lat)=gaus(i,ll,lat)+hold(i,l)
                END DO
             END DO
          END IF
       END DO
    END IF
3180 FORMAT(' ERROR IN CALLING UPDIA WITH UNSET AVAILABLE DIAGNOSTIC', I3)
3680 FORMAT(' UNABLE TO FIND MATCHING AVAILABLE DIAG. NO.',I3/ &
         ' FOR COMBINED FIELD',I3,' A.D. INDEX',I3,' C.F. RANGE',I3,'-',I3)
4555 FORMAT(' CONVERSION ERROR IN UPDIA.  ERROR=',I3,' NUAVL=',I5, &
         ' NUCF=',I5/' A.D. NO.=',I3,' C.F. NO.=',I3,' A.D. INDEX=',I3)
4180 FORMAT(' ERROR IN CALLING UPDIA WITH WRONG TYPE CODE',I2)
  END SUBROUTINE updia






  SUBROUTINE reord (datum, dim2, work, lev, imask, tsea, ittl)
    INTEGER, INTENT(IN ) :: dim2
    REAL,    INTENT(in ) :: datum(ibMax,dim2,jbMax)
    REAL,    INTENT(OUT) :: work (ibMax,jbMax)
    INTEGER, INTENT(IN ) :: lev
    INTEGER, INTENT(IN ) :: imask   (ibMax,jbMax)
    REAL,    INTENT(IN ) :: tsea    (ibMax,jbMax)
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
             work(i,j)=1.0
          ELSE
             work(i,j)=0.0
          END IF
       END DO
    END DO
  END SUBROUTINE reord






  SUBROUTINE weprog (nedrct,neprog,nefcst,ifday ,tod   ,idate ,idatec, &
       del   ,qgzs  ,lsmk  ,qlnp  ,qdiv  , &
       qrot  ,qq    ,qtmp  ,zorl  ,gtsea ,td0   ,capac0, &
       w0    ,imask ,roperm,namee ,labeli,labelf,extw  , &
       exdw  ,trunc ,lev   ,ijMaxGauQua,kmax,imax,jmax  )
    INTEGER           , INTENT(IN   ) :: nedrct
    INTEGER           , INTENT(IN   ) :: neprog
    INTEGER           , INTENT(IN   ) :: nefcst
    INTEGER           , INTENT(IN   ) :: ifday
    INTEGER           , INTENT(IN   ) :: ijMaxGauQua
    INTEGER           , INTENT(IN   ) :: imax
    INTEGER           , INTENT(IN   ) :: jmax
    INTEGER           , INTENT(IN   ) :: kmax
    REAL              , INTENT(IN   ) :: tod
    INTEGER           , INTENT(IN   ) :: idate (:)
    INTEGER           , INTENT(IN   ) :: idatec(:)
    REAL              , INTENT(IN   ) :: del   (:)
    REAL              , INTENT(IN   ) :: qgzs  (2*mnMax)
    REAL              , INTENT(IN   ) :: lsmk  (:)
    REAL              , INTENT(IN   ) :: qlnp  (:)
    REAL              , INTENT(IN   ) :: qdiv  (:,:)
    REAL              , INTENT(IN   ) :: qrot  (:,:)
    REAL              , INTENT(IN   ) :: qq    (:,:)
    REAL              , INTENT(IN   ) :: qtmp  (:,:)
    REAL              , INTENT(IN   ) :: zorl  (ibMax,jbMax)
    REAL              , INTENT(IN   ) :: gtsea (ibMax,jbMax)
    REAL              , INTENT(IN   ) :: td0   (ibMax,jbMax)
    REAL              , INTENT(IN   ) :: capac0(ibMax,2,jbMax)
    REAL              , INTENT(IN   ) :: w0    (ibMax,3,jbMax)
    INTEGER           , INTENT(IN   ) :: imask (ibMax,jbMax)
    CHARACTER(LEN=200), INTENT(IN   ) :: roperm
    CHARACTER(LEN=  7), INTENT(IN   ) :: namee
    CHARACTER(LEN= 10), INTENT(IN   ) :: labeli
    CHARACTER(LEN= 10), INTENT(IN   ) :: labelf
    CHARACTER(LEN=  5), INTENT(IN   ) :: extw
    CHARACTER(LEN=  5), INTENT(IN   ) :: exdw
    CHARACTER(LEN=  4), INTENT(IN   ) :: trunc
    CHARACTER(LEN=  3), INTENT(IN   ) :: lev
    INTEGER                           :: k,ij,j,i
    REAL                              :: work (ibMax,jbMax)
    REAL                              :: work_in (iMaxNew,jMaxNew)    
    REAL                              :: tod4
    INTEGER                           :: ifday4
    INTEGER                           :: idat4(4)
    INTEGER                           :: idat4c(4)

    LOGICAL, PARAMETER                :: toCol=.FALSE.
    REAL                              :: aux1(2*mnMax)
    REAL                              :: aux2(2*mnMax,kMax)
    REAL                              :: aux3(ijMaxGauQua)



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
    !
    !    topography
    !
    CALL TransDiagCol(qgzs, aux1, toCol)
    CALL WriteField(neprog, aux1)
    !
    !    land sea mask
    !
    CALL WriteField(neprog, lsmk)
    !
    !    ln surface pressure
    ! 
    CALL TransDiagCol(qlnp, aux1, toCol)
    CALL WriteField(neprog, aux1)
    !
    !    divergence
    !
    CALL TransDiagCol(qdiv, aux2, toCol)
    CALL WriteField(neprog, aux2)
    !
    !    vorticity
    !
    CALL TransDiagCol(qrot, aux2, toCol)
    CALL WriteField(neprog, aux2)    
    !
    !    specific humidity
    !
    CALL TransDiagCol(qq, aux2, toCol)
    CALL WriteField(neprog, aux2)
    !
    !    virtual temperature
    !
    CALL TransDiagCol(qtmp, aux2, toCol)
    CALL WriteField(neprog, aux2)
    !
    !    surface roughness
    !
    ij=1
    CALL IBJBtoIJ(zorl,work_in)
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work_in(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(neprog, aux3)
    !
    !   surface temperature
    !
    ij=1
    CALL IBJBtoIJ(gtsea,work_in)
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work_in(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(neprog, aux3)
    !
    !    deep soil temperature
    !
    CALL reord (td0,    1, work, 1, imask, gtsea, 'TD  ')
    CALL IBJBtoIJ(work,work_in)
    ij=1
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(neprog, aux3)
    !
    !    storage on canopy
    !
    CALL reord (capac0, 2, work, 1, imask, gtsea, 'CAPC')
    CALL IBJBtoIJ(work,work_in)
    ij=1
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(neprog, aux3)
    !
    !    storage on ground cover
    !
    CALL reord (capac0, 2, work, 2, imask, gtsea, 'CAPG')
    CALL IBJBtoIJ(work,work_in)
    ij=1
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(neprog, aux3)
    !
    !    wetness of surface zone
    ! 
    CALL reord (w0,     3, work, 1, imask, gtsea, 'W1  ')
    CALL IBJBtoIJ(work,work_in)
    ij=1
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(neprog, aux3)
    !
    !    wetness of root zone
    !
    CALL reord (w0,     3, work, 2, imask, gtsea, 'W2  ')
    CALL IBJBtoIJ(work,work_in)
    ij=1
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(neprog, aux3)
    !
    !    wetness of drainage zone
    !
    CALL reord (w0,     3, work, 3, imask, gtsea, 'W3  ')
    CALL IBJBtoIJ(work,work_in)
    ij=1
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(neprog, aux3)

    IF (ifprt(95) .GE. 1) WRITE(nfprt,10)idate,ifday,tod,idatec

10  FORMAT(' Done With weprog. Model Started ',3i3,i5/' Now at',i8,&
         ' Days and',f8.1,' Seconds.  Current Date is',3i3,i5)

  END SUBROUTINE weprog






  SUBROUTINE opnprg(nedrct, neprog, nefcst, ifday, tod, idate, idatec,roperm,&
       namee ,labeli ,labelf,extw   ,exdw  ,trunc ,lev)
    INTEGER,            INTENT(IN) :: nedrct
    INTEGER,            INTENT(IN) :: neprog
    INTEGER,            INTENT(IN) :: nefcst
    INTEGER,            INTENT(IN) :: ifday
    REAL,               INTENT(IN) :: tod
    INTEGER,            INTENT(IN) :: idate(4)
    INTEGER,            INTENT(IN) :: idatec(4)
    CHARACTER(LEN=200), INTENT(IN) :: roperm
    CHARACTER(LEN=  7), INTENT(IN) :: namee
    CHARACTER(LEN= 10), INTENT(IN) :: labeli
    CHARACTER(LEN= 10), INTENT(IN) :: labelf
    CHARACTER(LEN=  5), INTENT(IN) :: extw
    CHARACTER(LEN=  5), INTENT(IN) :: exdw
    CHARACTER(LEN=  4), INTENT(IN) :: trunc
    CHARACTER(LEN=  3), INTENT(IN) :: lev
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
       OPEN(nefcst,FILE=roperm(1:is)//TRIM(modout)//namee//&
            labeli//labelf//extw(1:2)//'dir'//'.'//trunc//lev//'.files',&
            STATUS='UNKNOWN')
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

    WRITE(*,'(A,3(2X,A))') ' OPNPRG : ',labeli,label,labelc

    CLOSE(nedrct)
    CLOSE(neprog)
    OPEN(nedrct,FILE=roperm(1:is)//TRIM(modout)//namee// &
         labeli//labelc//exdn//trunc//lev,form='formatted', &
         ACTION='write', STATUS='replace', IOSTAT=ierr)
    OPEN(neprog,FILE=roperm(1:is)//TRIM(modout)//namee//&
         labeli//labelc//extn//trunc//lev,form='unformatted', &
         ACTION='write', STATUS='replace', IOSTAT=ierr)
    WRITE(nefcst,'(a)')roperm(1:is)//TRIM(modout)//namee// &
         labeli//labelc//exdn//trunc//lev
    WRITE(nefcst,'(a)')roperm(1:is)//TRIM(modout)//namee// &
         labeli//labelc//extn//trunc//lev
  END SUBROUTINE opnprg








  SUBROUTINE opnfct(nfdrct, nfdiag, nffcst, ifday, tod, idate, idatec,&
       roperm,namef,labeli,labelf,extw,exdw,trunc,lev)
    INTEGER, INTENT(IN ) :: nfdrct
    INTEGER, INTENT(IN ) :: nfdiag
    INTEGER, INTENT(IN ) :: nffcst
    INTEGER, INTENT(IN ) :: ifday
    REAL,    INTENT(IN ) :: tod
    INTEGER, INTENT(IN ) :: idate(4)
    INTEGER, INTENT(IN ) :: idatec(4)

    CHARACTER(LEN=200), INTENT(IN   ) :: roperm
    CHARACTER(LEN=  7), INTENT(IN   ) :: namef
    CHARACTER(LEN= 10), INTENT(IN   ) :: labeli
    CHARACTER(LEN= 10), INTENT(IN   ) :: labelf
    CHARACTER(LEN=  5), INTENT(IN   ) :: extw
    CHARACTER(LEN=  5), INTENT(IN   ) :: exdw  
    CHARACTER(LEN=  4), INTENT(IN   ) :: trunc
    CHARACTER(LEN=  3), INTENT(IN   ) :: lev

    INTEGER :: iyi
    INTEGER :: imi
    INTEGER :: idi
    INTEGER :: ihi
    INTEGER :: iyc
    INTEGER :: imc
    INTEGER :: idc
    INTEGER :: ihc
    LOGICAL :: inic

    INTEGER,            SAVE :: icall=1
    INTEGER,            SAVE :: is
    CHARACTER(LEN= 10)       :: labelc
    CHARACTER(LEN=  3), SAVE :: ext
    CHARACTER(LEN=  6)       :: extn
    CHARACTER(LEN=  6)       :: exdn
    CHARACTER(LEN= 23)       :: modout
    CHARACTER(LEN= 10)       :: label

    inic=(ifday.EQ.0 .AND. tod.EQ.0)

    IF (icall.EQ.1 .AND. inic) THEN
       ext='icn'
    ELSEIF (icall.EQ.2 .AND. inic) THEN
       icall=3
       ext='inz'
    ELSE
       ext='fct'
    ENDIF
    !modout='/model/dataout/'//trunc//lev//'/'
    modout='/'
    IF (icall .EQ. 1) THEN
       icall=2
       is=INDEX(roperm//' ',' ')-1
       IF (is .LE. 0) is=1
       OPEN(nffcst,file=roperm(1:is)//TRIM(modout)//namef// &
            labeli//labelf//extw(1:2)//'dir'//'.'//trunc//lev//'.files', &
            status='unknown')
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
    WRITE(*,'(a,3(2x,a))') ' OPNFCT : ',labeli,label,labelc
    CLOSE(nfdrct)
    CLOSE(nfdiag)

    OPEN(nfdrct,file=roperm(1:is)//TRIM(modout)//namef// &
         labeli//labelc//exdn//trunc//lev,form='formatted', &
         ACTION='write', STATUS='replace', IOSTAT=ierr)

    OPEN(nfdiag,file=roperm(1:is)//TRIM(modout)//namef// &
         labeli//labelc//extn//trunc//lev,form='unformatted', &
         ACTION='write', STATUS='replace', IOSTAT=ierr)
    WRITE(nffcst,'(a)')roperm(1:is)//TRIM(modout)//namef// &
         labeli//labelc//exdn//trunc//lev
    WRITE(nffcst,'(a)')roperm(1:is)//TRIM(modout)//namef// &
         labeli//labelc//extn//trunc//lev  

  END SUBROUTINE opnfct







  SUBROUTINE wrprog (iudrct,iudiag,ifday ,tod   ,idate ,idatec,qrot  , &
       qdiv  ,qq    ,qlnp  ,qtmp  ,zorl  ,gtsea ,td0   , &
       capac0,w0    ,imask ,iufcst,del   , &
       qgzs  ,lsmk  ,ijMaxGauQua  ,kmax  ,imax  ,jmax  , &
       roperm,namef ,labeli,labelf,extw  ,exdw  ,trunc , &
       lev)
    INTEGER           , INTENT(IN   ) :: iudrct
    INTEGER           , INTENT(IN   ) :: iudiag
    INTEGER           , INTENT(IN   ) :: ifday
    INTEGER           , INTENT(IN   ) :: ijMaxGauQua
    INTEGER           , INTENT(IN   ) :: imax
    INTEGER           , INTENT(IN   ) :: jmax
    INTEGER           , INTENT(IN   ) :: kmax
    REAL              , INTENT(IN   ) :: tod
    INTEGER           , INTENT(IN   ) :: idate (:)
    INTEGER           , INTENT(IN   ) :: idatec(:) 
    REAL              , INTENT(IN   ) :: qlnp  (:)
    REAL              , INTENT(IN   ) :: qtmp  (:,:)
    REAL              , INTENT(IN   ) :: qdiv  (:,:)
    REAL              , INTENT(IN   ) :: qrot  (:,:)
    REAL              , INTENT(IN   ) :: qq    (:,:)
    REAL              , INTENT(IN   ) :: lsmk  (:)
    REAL              , INTENT(IN   ) :: zorl  (ibMax,jbMax)
    REAL              , INTENT(IN   ) :: gtsea (ibMax,jbMax)
    REAL              , INTENT(IN   ) :: td0   (ibMax,jbMax)
    REAL              , INTENT(IN   ) :: capac0(ibMax,2,jbMax)
    REAL              , INTENT(IN   ) :: w0    (ibMax,3,jbMax)
    INTEGER           , INTENT(IN   ) :: imask (ibMax,jbMax)
    INTEGER           , INTENT(IN   ) :: iufcst
    REAL              , INTENT(IN   ) :: del   (kMaxNew)
    REAL              , INTENT(IN   ) :: qgzs  (2*mnMax)
    CHARACTER(LEN=200), INTENT(IN   ) :: roperm
    CHARACTER(LEN=  7), INTENT(IN   ) :: namef
    CHARACTER(LEN= 10), INTENT(IN   ) :: labeli
    CHARACTER(LEN= 10), INTENT(IN   ) :: labelf
    CHARACTER(LEN=  5), INTENT(IN   ) :: extw
    CHARACTER(LEN=  5), INTENT(IN   ) :: exdw  
    CHARACTER(LEN=  4), INTENT(IN   ) :: trunc
    CHARACTER(LEN=  3), INTENT(IN   ) :: lev

    REAL                              :: work(ibMax,jbMax)
    REAL                              :: work_in(iMaxNew,jMaxNew)
    INTEGER                           :: k,ij,j,i
    INTEGER                           :: ifday4
    INTEGER                           :: idat4(4)
    INTEGER                           :: idat4c(4)
    REAL                              :: tod4

    LOGICAL               , PARAMETER :: toCol=.FALSE.
    REAL                              :: aux1(2*mnMax)
    REAL                              :: aux2(2*mnMax,kMax)
    REAL                              :: aux3(ijMaxGauQua)


    CALL opnfct(iudrct,iudiag,iufcst,ifday,tod,idate,idatec,&
         roperm,namef,labeli,labelf,extw,exdw,trunc,lev)

    CALL WriteDir(iudrct, idate,idatec(1), idatec(3),idatec(2),&
         idatec(4),del,tod)    
    ifday4=ifday
    tod4=tod
    DO k=1,4
       idat4(k)=idate(k)
       idat4c(k)=idatec(k)
    ENDDO

    CALL WriteProgHead(iudiag, ifday4, tod4, idat4, idat4c)
    !
    !     topography
    !
    CALL TransDiagCol(qgzs, aux1, toCol)
    CALL WriteField(iudiag, aux1)
    !
    !     land sea mask
    !
    CALL WriteField(iudiag, lsmk)
    !
    ! write directory and spectral prognostic fields
    !     
    !     ln surface pressure
    !     
    CALL TransDiagCol(qlnp, aux1, toCol)
    CALL WriteField(iudiag, aux1)
    !
    !     divergence 
    !     
    CALL TransDiagCol(qdiv, aux2, toCol)
    CALL WriteField(iudiag, aux2)
    !
    !     vorticity
    !
    CALL TransDiagCol(qrot, aux2, toCol)
    CALL WriteField(iudiag, aux2)    
    !
    !     specific humidity
    !     
    CALL TransDiagCol(qq, aux2, toCol)
    CALL WriteField(iudiag, aux2)
    !     
    !     virtual temperature
    !     
    CALL TransDiagCol(qtmp, aux2, toCol)
    CALL WriteField(iudiag, aux2)
    !     
    !     surface roughness 
    !     
    ij=1
    CALL IBJBtoIJ(zorl,work_in)
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work_in(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(iudiag, aux3)
    !
    !     surface temperature
    !     
    ij=1
    CALL IBJBtoIJ(gtsea,work_in)
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work_in(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(iudiag, aux3)
    !
    !     deep soil temperature 
    !     
    CALL reord (td0,    1, work, 1, imask, gtsea, 'TD  ')
    CALL IBJBtoIJ(work,work_in)
    ij=1
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work_in(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(iudiag, aux3)
    !     
    !     storage on canopy
    !     
    CALL reord (capac0, 2, work, 1, imask, gtsea, 'CAPC') 
    CALL IBJBtoIJ(work,work_in)
    ij=1
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work_in(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(iudiag, aux3)
    !     
    !     storage on ground cover
    !     
    CALL reord (capac0, 2, work, 2, imask, gtsea, 'CAPG')
    CALL IBJBtoIJ(work,work_in)
    ij=1
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work_in(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(iudiag, aux3)
    !
    !     wetness of surface zone
    !     
    CALL reord (w0,     3, work, 1, imask, gtsea, 'W1  ')
    CALL IBJBtoIJ(work,work_in)
    ij=1
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work_in(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(iudiag, aux3)
    !     
    !     wetness of root zone
    !     
    CALL reord (w0,     3, work, 2, imask, gtsea, 'W2  ')
    CALL IBJBtoIJ(work,work_in)
    ij=1
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work_in(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(iudiag, aux3)
    !     
    !     wetness of drainage zone
    !     
    CALL reord (w0,     3, work, 3, imask, gtsea, 'W3  ')
    CALL IBJBtoIJ(work,work_in)
    ij=1
    DO j=1,jMax
       DO i=1,iMax
       aux3(ij)=work_in(i,j) 
          ij=ij+1
       END DO
    END DO
    CALL WriteField(iudiag, aux3)

    IF(ifprt(95).GE.1)WRITE(nfprt,5000)idate,ifday,tod,idatec
5000 FORMAT(' DONE WITH WRPROG. MODEL STARTED ',3I3,I5/' NOW AT',I8, &
         ' DAYS AND',F8.1,' SECONDS.  CURRENT DATE IS',3I3,I5)
  END SUBROUTINE wrprog
END MODULE Diagnostics
