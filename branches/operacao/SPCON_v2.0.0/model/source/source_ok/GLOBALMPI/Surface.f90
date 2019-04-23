!
!  $Author: pkubota $
!  $Date: 2010/10/21 10:46:22 $
!  $Revision: 1.23 $
!
!
!MCGACPTEC : MEDIATION_LAYER:PHYSICS
!
MODULE Surface
  USE Constants, ONLY :     &
       ityp, imon, icg, iwv, idp, ibd,tice, &
       r8,i8,cp,grav,epsfac,hl,gasr,stefan,pie


  USE Options, ONLY : &
       initlz,&
       yrl   ,&
       kt    ,&
       ktm   ,&
       iswrad  ,&
       ilwrad  ,&
       filta ,&
       monl  ,&
       epsflt,&
       istrt ,&
       idatec,&
       Model1D,schemes
  USE Diagnostics, ONLY: &
       updia, &
       dodia, &
       StartStorDiag,&
       nDiag_tep02m, & ! Temperature at 2-m from surface layer
       nDiag_mxr02m, & ! Mixing ratio at 2-m from surface layer
       nDiag_tep10m, & ! Temperature at 10-m from surface layer
       nDiag_mxr10m, & ! Mixing ratio at 10-m from surface layer
       nDiag_spw10m, &
       nDiag_spw02m, &
       nDiag_zwn10m, &
       nDiag_mwn10m, &
       nDiag_intlos, &
       nDiag_runoff, &
       nDiag_tcairs, &
       nDiag_ecairs, &
       nDiag_bsolht, &
       nDiag_cascrs, &
       nDiag_casgrs, &
       nDiag_gcovrs, &
       nDiag_mofres, &
       nDiag_casrrs, &
       nDiag_sheatf, &
       nDiag_lheatf, &
       nDiag_mofres, &
       nDiag_casrrs, &
       nDiag_ustres, &
       nDiag_vstres, &
       nDiag_lwubot
  USE GridHistory, ONLY:       &
       IsGridHistoryOn, StoreGridHistory, StoreMaskedGridHistory, dogrh, &
       nGHis_casrrs,nGHis_mofres,nGHis_dragcf,nGHis_nrdcan,nGHis_nrdgsc, &
       nGHis_cascrs,nGHis_casgrs,nGHis_canres,nGHis_gcovrs,nGHis_bssfrs, &
       nGHis_ecairs,nGHis_tcairs,nGHis_shfcan,nGHis_shfgnd,nGHis_tracan, &
       nGHis_tragcv,nGHis_inlocp,nGHis_inlogc,nGHis_bsevap,nGHis_canhea, &
       nGHis_gcheat,nGHis_runoff,nGHis_lwubot,nGHis_ustres,nGHis_vstres, &
       nGHis_sheatf,nGHis_lheatf,nGHis_hcseai,nGHis_hsseai,nGHis_tep02m, &
       nGHis_mxr02m,nGHis_zwn10m,nGHis_mwn10m
  !
  ! *** add new modules of schemes here
  !
  USE SFC_SSiB, ONLY: &
       SSiB_Driver,CopySurfaceData,InitSSiB








  IMPLICIT NONE
  PRIVATE
  PUBLIC  :: surface_driver
  PUBLIC  :: Phenology
  PUBLIC  :: InitSurface

CONTAINS
  SUBROUTINE InitSurface(ibMax ,jbMax,del_in,path_in,fNameSibVeg,fNameSibAlb)
    INTEGER, INTENT(IN) :: ibMax
    INTEGER, INTENT(IN) :: jbMax
    REAL(KIND=r8)   , INTENT(IN   ) ::  del_in(:)
    CHARACTER(LEN=*), INTENT(IN   ) ::  path_in
    CHARACTER(LEN=*), INTENT(IN   ) ::  fNameSibVeg
    CHARACTER(LEN=*), INTENT(IN   ) ::  fNameSibAlb
    IF(schemes == 1)CALL InitSSiB(ibMax ,jbMax,del_in,path_in,fNameSibVeg,fNameSibAlb)

    IF(schemes == 2)PRINT*,'ERROR schemes 2'
  END SUBROUTINE InitSurface

  SUBROUTINE surface_driver(&
       jdt   ,latitu, cosz ,sinclt,satcap,extk  ,radfac,closs , &
       gloss ,thermk, p1f  ,p2f   , tc   , tg   ,td    ,capac , &
       w     ,itype ,dtc3x ,mon   , nmax ,ncols ,td0   ,tg0   , &
       tc0   ,w0    ,capac0,tcm   ,tgm   ,tdm   ,wm    ,capacm, &
       intg  ,ssib  ,zlwup ,nsx   ,tgeff ,salb  , &
       imask ,cos2  ,dlwbot,beam_visb,beam_visd ,beam_nirb,&
       beam_nird ,zenith ,xvisb ,xvisd,xnirb,xnird,&
       ppli,ppci,tmtx,qmtx,umtx,tsea,kMax,slrad ,tsurf,qsurf,gt,&
       gq,gu,gv,gps,colrad,sigki,delsig,sens,evap,umom,vmom,&
       zorl,tseam ,SICE2, ustar2 ,qsfc0,tsfc0,qsfcm,&
       tsfcm,qsfc,tsfc,z0,qm0,qm  ,qmm,tm0,tm,tmm,htdisp,temp2m,&
       umes2m,uve10m,vve10m,mskant)

    IMPLICIT NONE

    !
    !
    !-----------------------------------------------------------------------
    !
    !  tg.........Temperatura da superficie do solo  (K)
    !  td.........Temperatura do solo profundo (K)
    !  tf.........Temperatura de congelamento (K)
    !  idp........Parametro para as camadas de solo idp=1->3
    !  nmax.......
    !  ncols......Number of grid points on a gaussian latitude circle
    !  ityp.......Numero das classes de solo 13
    !  imon.......Numero maximo de meses no ano (12)
    !  icg........Parametros da vegetacao (icg=1 topo e icg=2 base)
    !  iwv........Compriment de onda iwv=1=visivel, iwv=2=infravermelho
    !             proximo, iwv=3 infravermelho termal
    !  idp........Camadas de solo (1 a 3)
    !  ibd........Estado da vegetacao ibd=1 verde / ibd=2 seco
    !  stefan.....Constante de Stefan Boltzmann
    !  cp.........specific heat of air (j/kg/k)
    !  hl ........heat of evaporation of water   (j/kg)
    !  grav.......gravity constant      (m/s**2)
    !  tf.........Temperatura de congelamento (K)
    !  clai.......heat capacity of foliage
    !  cw.........liquid water heat capacity               (j/m**3)
    !  gasr.......Constant of dry air      (j/kg/k)
    !  epsfac.....Constante 0.622 Razao entre as massas moleculares do vapor
    !             de agua e do ar seco
    !  itype......Classe de textura do veg
    !  qm.........Reference specific humidity (fourier)
    !  tm.........Reference temperature    (fourier)                (k)
    !  um.........Razao entre zonal pseudo-wind (fourier) e seno da
    !             colatitude
    !  vm.........Razao entre meridional pseudo-wind (fourier) e seno da
    !             colatitude
    !  psur.......Surface pressure in mb
    !  ppc........Precipitation rate ( cumulus )           (mm/s)
    !  ppl........Precipitation rate ( large scale )       (mm/s)
    !  radn.......Downward sw/lw radiation at the surface
    !  tc.........Temperatura da copa "dossel"(K)
    !  tg.........Temperatura da superficie do solo (K)
    !  td.........Temperatura do solo profundo (K)
    !  capac(iv)..Agua interceptada iv=1 no dossel "water store capacity
    !             of leaves"(m)
    !  capac(iv)..Agua interceptada iv=2 na cobertura do solo (m)
    !  w(id)......Grau de saturacao de umidade do solo id=1 na camada superficial
    !  w(id)......Grau de saturacao de umidade do solo id=2 na camada de raizes
    !  w(id)......Grau de saturacao de umidade do solo id=3 na camada de drenagem
    !  ra.........Resistencia Aerodinamica (s/m)
    !  rb.........bulk boundary layer resistance
    !  rd.........Aerodynamic resistance between ground      (s/m)
    !             and canopy air space
    !  rc.........Resistencia do topo da copa
    !  rg.........Resistencia da base da copa
    !  tcta.......Diferenca entre tc-ta                      (k)
    !  tgta.......Diferenca entre tg-ta                      (k)
    !  ta.........Temperatura no nivel de fonte de calor do dossel (K)
    !  ea.........Pressure of vapor
    !  etc........Pressure of vapor at top of the copa
    !  etg........Pressao de vapor no base da copa
    !  btc........btc(i)=EXP(30.25353  -5418.0  /tc(i))/(tc(i)*tc(i)).
    !  btg........btg(i)=EXP(30.25353  -5418.0  /tg(i))/(tg(i)*tg(i))
    !  u2.........wind speed at top of canopy
    !  radt.......net heat received by canopy/ground vegetation
    !  pd.........ratio of par beam to total par
    !  rst .......Resisttencia Estomatica "Stomatal resistence" (s/m)
    !  rsoil......Resistencia do solo (s/m)
    !  phroot.....Soil moisture potentials in root zone of each
    !             vegetation layer and summed soil+root resistance.
    !  hrr........rel. humidity in top layer
    !  phsoil.....soil moisture potential of the i-th soil layer
    !  cc.........heat capacity of the canopy
    !  cg.........heat capacity of the ground
    !  satcap.....saturation liquid water capacity         (m)
    !  snow.......snow amount
    !  dtc........dtc(i)=pblsib(i,2,5)*dtc3x
    !  dtg........dtg(i)=pblsib(i,1,5)*dtc3x
    !  dtm........dtm(i)=pblsib(i,3,5)*dtc3x
    !  dqm .......dqm(i)=pblsib(i,4,5)*dtc3x
    !  stm .......Variavel utilizada mo cal. da Resisttencia
    !  extk.......extinction coefficient
    !  radfac.....Fractions of downward solar radiation at surface
    !             passed from subr.radalb
    !  closs......Radiation loss from canopy
    !  gloss......Radiation loss from ground
    !  thermk.....Canopy emissivity
    !  p1f
    !  p2f
    !  ect........Transpiracao no topo da copa (J/m*m)
    !  eci........Evaporacao da agua interceptada no topo da copa (J/m*m)
    !  egt........Transpiracao na base da copa (J/m*m)
    !  egi........Evaporacao da neve (J/m*m)
    !  egs........Evaporacao do solo arido (J/m*m)
    !  ec.........Soma da Transpiracao e Evaporacao da agua interceptada pelo
    !             topo da copa   ec   (i)=eci(i)+ect(i)
    !  eg.........Soma da transpiracao na base da copa +  Evaporacao do solo arido
    !             +  Evaporacao da neve  " eg   (i)=egt(i)+egs(i)+egi(i)"
    !  hc.........Total sensible heat lost of top from the veggies.
    !  hg.........Total sensible heat lost of base from the veggies.
    !  ecidif.....check if interception loss term has exceeded canopy storage
    !             ecidif(i)=MAX(0.0   , eci(i)-capac(i,1)*hlat3 )
    !  egidif.....check if interception loss term has exceeded canopy storage
    !             ecidif(i)=MAX(0.0   , egi(i)-capac(i,1)*hlat3 )
    !  ecmass.....Mass of water lost of top from the veggies.
    !  egmass.....Mass of water lost of base from the veggies.
    !  etmass.....Total mass of water lost from the veggies.
    !  hflux......Total sensible heat lost from the veggies
    !  chf........Heat fluxes into the canopy  in w/m**2
    !  shf........Heat fluxes into the ground, in w/m**2
    !  fluxef.....Modified to use force-restore heat fluxes
    !             fluxef(i) = shf(i) - cg(i)*dtg(i)*dtc3xi " Garrat pg. 227"
    !  roff.......runoff (escoamente superficial e drenagem)(m)
    !  drag.......tensao superficial
    !  bps
    !  psb
    !  dzm........Altura media de referencia  para o vento para o calculo
    !             da estabilidade do escoamento
    !  em.........Pressao de vapor da agua
    !  gmt(i,k,3).temperature related matrix virtual temperature tendency
    !             due to vertical diffusion
    !  gmq........specific humidity related matrix specific humidity of
    !             reference (fourier)
    !  gmu........wind related matrix
    !  cu.........Friction  transfer coefficients.
    !  cuni.......Neutral friction transfer  coefficients.
    !  ctni.......Neutral heat transfer coefficients.
    !  ustar......Surface friction velocity  (m/s)
    !  cosz.......Cosine of zenith angle
    !  sinclt.....sinclt=SIN(colrad(latitu))"seno da colatitude"
    !  rhoair.....Desnsidade do ar
    !  psy........(cp/(hl*epsfac))*psur(i)
    !  rcp........densidade do ar vezes o calor especifico do ar
    !  wc.........Minimo entre 1 e a razao entre a agua interceptada pelo
    !             indice de area foliar no topo da copa
    !  wg.........Minimo entre 1 e a razao entre a agua interceptada pelo
    !             indice de area foliar na base da copa
    !  fc.........Condicao de oravalho 0 ou 1 na topo da copa
    !  fg.........Condicao de oravalho 0 ou 1 na base da copa
    !  hr.........rel. humidity in top layer
    !-----------------------------------------------------------------------
    !
    INTEGER      , INTENT(in   ) :: nCols
    INTEGER      , INTENT(in   ) :: kMax
    INTEGER      , INTENT(in   ) :: nmax
    REAL(KIND=r8), INTENT(in   ) :: dtc3x       !time increment dt
    INTEGER      , INTENT(in   ) :: itype (ncols)!classes de vegetacao compress
    REAL(KIND=r8), INTENT(in   ) :: sinclt(ncols)!"seno da colatitude"
    INTEGER      , INTENT(in   ) :: jdt         ! time step
    INTEGER      , INTENT(in   ) :: latitu      ! indice da latidude
    !
    !     prognostic variables
    !
    INTEGER      , INTENT(inout) :: mon   (ncols)  !Number of month at year (1-12)
    REAL(KIND=r8), INTENT(inout) :: tc    (ncols) !Temperatura da copa "dossel"(K)
    REAL(KIND=r8), INTENT(inout) :: tg    (ncols) !Temperatura da superficie do solo (K)
    REAL(KIND=r8), INTENT(inout) :: td    (ncols) !Temperatura do solo profundo (K)
    REAL(KIND=r8), INTENT(inout) :: capac (ncols,2)! Agua interceptada
    ! iv=1 no dossel (m)
    ! iv=2 na cobertura do solo (m)
    REAL(KIND=r8), INTENT(inout) :: w     (ncols,3)! w(id)..Grau de saturacao de umidade do solo
    ! id=1 na camada superficial
    ! id=2 na camada de raizes
    ! id=3 na camada de drenagem

    REAL(KIND=r8), INTENT(inout) :: satcap(ncols,icg)        !saturation liquid water capacity(m)
    REAL(KIND=r8), INTENT(inout) :: extk  (ncols,icg,iwv,ibd)! extinction coefficient
    REAL(KIND=r8), INTENT(inout) :: radfac(ncols,icg,iwv,ibd)! Fractions of downward solar radiation at surface
    ! passed from subr.radalb
    REAL(KIND=r8), INTENT(inout) :: closs (ncols) !Radiation loss from canopy
    REAL(KIND=r8), INTENT(inout) :: gloss (ncols) !Radiation loss from ground
    REAL(KIND=r8), INTENT(inout) :: thermk(ncols) !Canopy emissivity
    REAL(KIND=r8), INTENT(inout) :: p1f   (ncols)
    REAL(KIND=r8), INTENT(inout) :: p2f   (ncols)
    REAL(KIND=r8), INTENT(inout) :: cosz  (ncols) !Cosine of zenith angle

    REAL(KIND=r8), INTENT(INOUT) :: td0      (ncols)
    REAL(KIND=r8), INTENT(INOUT) :: tg0      (ncols)
    REAL(KIND=r8), INTENT(INOUT) :: tc0      (ncols)
    REAL(KIND=r8), INTENT(INOUT) :: w0       (ncols,3)
    REAL(KIND=r8), INTENT(INOUT) :: capac0   (ncols,2)
    REAL(KIND=r8), INTENT(INOUT) :: tcm      (ncols)
    REAL(KIND=r8), INTENT(INOUT) :: tgm      (ncols)
    REAL(KIND=r8), INTENT(INOUT) :: tdm      (ncols)
    REAL(KIND=r8), INTENT(INOUT) :: wm       (ncols,3)
    REAL(KIND=r8), INTENT(INOUT) :: capacm   (ncols,2)
    INTEGER      , INTENT(IN   ) :: intg
    REAL(KIND=r8), INTENT(IN   ) :: ssib     (ncols)
    INTEGER      , INTENT(IN   ) :: nsx      (ncols)
    REAL(KIND=r8), INTENT(INOUT) :: zlwup    (ncols)  !upward long wave flux at ground
    REAL(KIND=r8), INTENT(INOUT) :: tgeff    (ncols)
    REAL(KIND=r8), INTENT(INOUT) :: salb     (ncols,2,2)
    INTEGER(KIND=i8), INTENT(IN) :: imask    (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: cos2     (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: dlwbot   (nCols)! downward longwave radiation at the bottom in w/m**2
    REAL(KIND=r8), INTENT(IN   ) :: beam_visb(nCols)!.Downward Surface shortwave fluxe visible beam (cloudy)
    REAL(KIND=r8), INTENT(IN   ) :: beam_visd(nCols)!.Downward Surface shortwave fluxe visible diffuse (cloudy)
    REAL(KIND=r8), INTENT(IN   ) :: beam_nirb(nCols)!.Downward Surface shortwave fluxe Near-IR beam (cloudy)
    REAL(KIND=r8), INTENT(IN   ) :: beam_nird(nCols)!.Downward Surface shortwave fluxe Near-IR diffuse (cloudy)
    REAL(KIND=r8), INTENT(IN   ) :: zenith   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: xvisb    (nCols)!.Downward Surface shortwave fluxe visible beam (cloudy)
    REAL(KIND=r8), INTENT(IN   ) :: xvisd    (nCols)!.Downward Surface shortwave fluxe visible diffuse (cloudy)
    REAL(KIND=r8), INTENT(IN   ) :: xnirb    (nCols)!.Downward Surface shortwave fluxe Near-IR beam (cloudy)
    REAL(KIND=r8), INTENT(IN   ) :: xnird    (nCols)!.Downward Surface shortwave fluxe Near-IR diffuse (cloudy)
    REAL(KIND=r8), INTENT(IN   ) :: ppli     (nCols)! Precipitation rate ( large scale )       (mm/s)
    REAL(KIND=r8), INTENT(IN   ) :: ppci     (nCols)! Precipitation rate ( cumulus )           (mm/s)
    REAL(KIND=r8), INTENT(INOUT) :: tmtx     (nCols,kmax,3)
    REAL(KIND=r8), INTENT(INOUT) :: qmtx     (nCols,kmax,3)
    REAL(KIND=r8), INTENT(INOUT) :: umtx     (nCols,kmax,4)
    REAL(KIND=r8), INTENT(INOUT) :: tsea     (nCols)! effective surface radiative temperature ( tgeff )
    REAL(KIND=r8), INTENT(IN   ) :: slrad    (nCols)
    REAL(KIND=r8), INTENT(INOUT) :: tsurf    (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: qsurf    (nCols)
    REAL(KIND=r8), INTENT(INOUT) :: gt       (nCols,kmax)!  Temperature ABSOLUTA K
    REAL(KIND=r8), INTENT(INOUT) :: gq       (nCols,kmax)!  Specific humidity    kg/kg
    REAL(KIND=r8), INTENT(IN   ) :: gu       (nCols,kmax)!  (zonal velocity)*sin(colat)
    REAL(KIND=r8), INTENT(IN   ) :: gv       (nCols,kmax)!  (meridional velocity)*sin(colat)
    REAL(KIND=r8), INTENT(IN   ) :: gps      (nCols)     !  Surface pressure in mb
    REAL(KIND=r8), INTENT(IN   ) :: colrad   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: sigki    (1)
    REAL(KIND=r8), INTENT(IN   ) :: delsig   (1)
    REAL(KIND=r8), INTENT(INOUT) :: sens     (nCols) !sensible heat flux
    REAL(KIND=r8), INTENT(INOUT) :: evap     (nCols) !latent heat flux
    REAL(KIND=r8), INTENT(INOUT) :: umom     (nCols) ! surface zonal stress
    REAL(KIND=r8), INTENT(INOUT) :: vmom     (nCols) ! surface meridional stress
    REAL(KIND=r8), INTENT(INOUT) :: zorl     (nCols)
    REAL(KIND=r8), INTENT(INOUT) :: tseam    (nCols)
    REAL(KIND=r8), INTENT(INOUT) :: SICE2    (nCols)
    REAL(KIND=r8), INTENT(INOUT) :: ustar2 (ncols)
    REAL(KIND=r8), INTENT(INOUT) :: qsfc0(ncols)
    REAL(KIND=r8), INTENT(INOUT) :: tsfc0(ncols)
    REAL(KIND=r8), INTENT(INOUT) :: qsfcm(ncols)
    REAL(KIND=r8), INTENT(INOUT) :: tsfcm(ncols)
    REAL(KIND=r8), INTENT(INOUT) :: qsfc(ncols)
    REAL(KIND=r8), INTENT(INOUT) :: tsfc(ncols)

    REAL(KIND=r8), INTENT(INOUT) :: z0  (ncols)
    REAL(KIND=r8) :: snow       (ncols,icg)
    REAL(KIND=r8) :: ustar      (ncols)



    !
    !     the size of working area is nmax*187
    !     atmospheric parameters as boudary values for sib
    !
    REAL(KIND=r8), INTENT(INOUT) :: qm0 (nCols)
    REAL(KIND=r8), INTENT(INOUT) :: qm  (nCols)
    REAL(KIND=r8), INTENT(INOUT) :: qmm (nCols)
    REAL(KIND=r8), INTENT(INOUT) :: tm0 (nCols)
    REAL(KIND=r8), INTENT(INOUT) :: tm  (nCols)
    REAL(KIND=r8), INTENT(INOUT) :: tmm (nCols)
    REAL(KIND=r8), INTENT(INOUT) :: htdisp (nCols)
    REAL(KIND=r8), INTENT(OUT  ) :: temp2m (nCols)
    REAL(KIND=r8), INTENT(OUT  ) :: umes2m (nCols)
    REAL(KIND=r8), INTENT(OUT  ) :: uve10m (nCols)
    REAL(KIND=r8), INTENT(OUT  ) :: vve10m (nCols)
    INTEGER(KIND=i8), INTENT(IN   ) :: mskant (nCols)
    REAL(KIND=r8) :: um  (nCols)
    REAL(KIND=r8) :: vm  (nCols)
    REAL(KIND=r8) :: psur(nCols)
    REAL(KIND=r8) :: ppc (nCols)
    REAL(KIND=r8) :: ppl (nCols)
    REAL(KIND=r8) :: radn(nCols,3,2)     !not used

    !
    !     variables calculated from above and ambient conditions
    !
    REAL(KIND=r8) :: ra    (ncols)
    REAL(KIND=r8) :: rb    (ncols)
    REAL(KIND=r8) :: rd    (ncols)
    REAL(KIND=r8) :: rc    (ncols)
    REAL(KIND=r8) :: rg    (ncols)
    REAL(KIND=r8) :: tcta  (ncols)
    REAL(KIND=r8) :: tgta  (ncols)
    REAL(KIND=r8) :: ta    (ncols)
    REAL(KIND=r8) :: ea    (ncols)
    REAL(KIND=r8) :: etc   (ncols)
    REAL(KIND=r8) :: etg   (ncols)
    REAL(KIND=r8) :: btc   (ncols)
    REAL(KIND=r8) :: btg   (ncols)
    REAL(KIND=r8) :: u2    (ncols)
    REAL(KIND=r8) :: radt  (ncols,icg)
    REAL(KIND=r8) :: par   (ncols,icg)   !par incident on canopy
    REAL(KIND=r8) :: pd    (ncols,icg)   !not used
    REAL(KIND=r8) :: rst   (ncols,icg)
    REAL(KIND=r8) :: rsoil (ncols)
    REAL(KIND=r8) :: phroot(ncols,icg)
    REAL(KIND=r8) :: hrr   (ncols)
    REAL(KIND=r8) :: phsoil(ncols,idp)
    REAL(KIND=r8) :: cc    (ncols)
    REAL(KIND=r8) :: cg    (ncols)
    REAL(KIND=r8) :: dtc   (ncols)
    REAL(KIND=r8) :: dtg   (ncols)
    REAL(KIND=r8) :: dtm   (ncols)
    REAL(KIND=r8) :: dqm   (ncols)
    REAL(KIND=r8) :: stm   (ncols,icg)
    !
    !     heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2
    !
    REAL(KIND=r8) :: ect   (ncols)
    REAL(KIND=r8) :: eci   (ncols)
    REAL(KIND=r8) :: egt   (ncols)
    REAL(KIND=r8) :: egi   (ncols)
    REAL(KIND=r8) :: egs   (ncols)
    REAL(KIND=r8) :: ec    (ncols)
    REAL(KIND=r8) :: eg    (ncols)
    REAL(KIND=r8) :: hc    (ncols)
    REAL(KIND=r8) :: hg    (ncols)
    REAL(KIND=r8) :: ecidif(ncols)
    REAL(KIND=r8) :: egidif(ncols)
    REAL(KIND=r8) :: ecmass(ncols)
    REAL(KIND=r8) :: egmass(ncols)
    REAL(KIND=r8) :: etmass(ncols)
    REAL(KIND=r8) :: hflux (ncols)
    REAL(KIND=r8) :: chf   (ncols)
    REAL(KIND=r8) :: shf   (ncols)
    REAL(KIND=r8) :: fluxef(ncols)
    REAL(KIND=r8) :: roff  (ncols)! Runoff (escoamente superficial e drenagem)(m)
    REAL(KIND=r8) :: drag  (ncols)
    !
    !     this is for coupling with closure turbulence model
    !
    REAL(KIND=r8) :: bps   (ncols)
    REAL(KIND=r8) :: psb   (ncols)
    REAL(KIND=r8) :: dzm   (ncols)
    REAL(KIND=r8) :: em    (ncols)
    REAL(KIND=r8) :: gmt   (ncols,3)
    REAL(KIND=r8) :: gmq   (ncols,3)
    REAL(KIND=r8) :: gmu   (ncols,4)
    REAL(KIND=r8) :: cu    (ncols)
    REAL(KIND=r8) :: cuni  (ncols)
    REAL(KIND=r8) :: ctni  (ncols)

    REAL(KIND=r8) :: rhoair(ncols)
    REAL(KIND=r8) :: psy   (ncols)
    REAL(KIND=r8) :: rcp   (ncols)
    REAL(KIND=r8) :: wc    (ncols)
    REAL(KIND=r8) :: wg    (ncols)
    REAL(KIND=r8) :: fc    (ncols)
    REAL(KIND=r8) :: fg    (ncols)
    REAL(KIND=r8) :: hr    (ncols)
    INTEGER       :: i,ncount

    REAL(KIND=r8) :: ztn       (ncols)


    REAL(KIND=r8) :: rmi      (nCols)
    REAL(KIND=r8) :: rhi      (nCols)
    REAL(KIND=r8) :: cond     (nCols)
    REAL(KIND=r8) :: stor     (nCols)

    REAL(KIND=r8) :: ztn2      (nCols)
    REAL(KIND=r8) :: VELC_2m  (nCols)
    REAL(KIND=r8) :: THETA_10M(nCols)
    REAL(KIND=r8) :: VELC_10M (nCols)
    REAL(KIND=r8) :: MIXQ_10M (nCols)
    REAL(KIND=r8) :: cpsy
    REAL(KIND=r8) :: rbyg
    REAL(KIND=r8) :: r100
    LOGICAL       :: ghl_local
    REAL(KIND=r8) :: dtc3xi
    REAL(KIND=r8) :: fmom
    REAL(KIND=r8) :: z0x   (nCols)
    REAL(KIND=r8) :: d     (nCols)
    REAL(KIND=r8) :: VELC  (nCols)
    REAL(KIND=r8) :: DirWind(nCols)

    REAL(KIND=r8) :: speedm   (nCols)
    REAL(KIND=r8) :: Ustarm   (nCols)
    REAL(KIND=r8) :: z0sea    (nCols)
    REAL(KIND=r8) :: rho      (nCols)
    REAL(KIND=r8) :: theta2m (nCols)
    REAL(KIND=r8) :: velc2m  (nCols)
    REAL(KIND=r8) :: q2m     (nCols)
    REAL(KIND=r8) :: theta10m(nCols)
    REAL(KIND=r8) :: velc10m (nCols)
    REAL(KIND=r8) :: q10m    (nCols)
    REAL(KIND=r8) :: znew
    REAL(KIND=r8) :: tvland(nCols)
    REAL(KIND=r8) :: tvsea (nCols)
    radt=0.0_r8
    par=0.0_r8
    pd=0.0_r8
    rst=0.0_r8
    phroot=0.0_r8
    phsoil=0.0_r8
    snow=0.0_r8
    stm=0.0_r8
    radn=0.0_r8

    ghl_local = IsGridHistoryOn()

    !
    !            cp
    ! cpsy = ------------
    !          L*epsfac
    !
    cpsy=cp/(hl*epsfac)
    !
    rbyg=gasr/grav*delsig(1)*0.5e0_r8
    !
    r100=100.0e0_r8 /gasr
    !
    ncount=0
    DO i=1,nCols
       IF(imask(i).GE.1_i8) THEN
          ncount=ncount+1
          gmt(ncount,1)=tmtx(i,1,1)
          gmt(ncount,2)=tmtx(i,1,2)
          gmt(ncount,3)=tmtx(i,1,3)
          gmq(ncount,1)=qmtx(i,1,1)
          gmq(ncount,2)=qmtx(i,1,2)
          gmq(ncount,3)=qmtx(i,1,3)
          gmu(ncount,1)=umtx(i,1,1)
          gmu(ncount,2)=umtx(i,1,2)
          gmu(ncount,3)=umtx(i,1,3)
          gmu(ncount,4)=umtx(i,1,4)
          psur(ncount)=gps(i)
          tm  (ncount)=gt (i,1)
          qm  (ncount)=gq (i,1)
          um  (ncount)=gu (i,1)/SIN( colrad(i))
          vm  (ncount)=gv (i,1)/SIN( colrad(i))
          psy   (ncount)=cpsy*psur(ncount)
          !
          ! P =rho*R*T   and     P = rho*g*Z
          !
          !                           P
          ! DP = rho*g*DZ and rho = ----
          !                          R*T
          !        P
          ! DP = ----*g*DZ
          !       R*T
          !
          !       RT      DP
          ! DZ = ---- * -----
          !       g       P
          !
          !              RT     P2 - P1
          ! Z2 - Z1 =  ----- *---------
          !              g        P2
          !
          !
          !              R      si(k) - si(k+1)
          ! dzm   (i) = --- * (----------------) * tm(i)
          !              g            2
          !
          !            (J/kg/K)
          ! dzm   (i) =--------*K
          !              m/s^2
          !
          !                J*s^2
          ! dzm   (i) = -----------*K
          !               kg*K * m
          !
          !               (kg*m*s^-2*m)*s^2
          ! dzm   (i) = ---------------------*K
          !                  kg*K * m
          !
          !               (m*m)
          ! dzm   (i) = --------- = m
          !                 m
          dzm   (ncount)=rbyg*tm(ncount)
          !
          ! presure of vapor at atmosphere
          !
          em    (ncount)=qm(ncount)*psur(ncount)/(epsfac+qm(ncount))
          !
          ! Factor conversion to potention temperature
          !
          bps   (ncount)=sigki(1)
          !
          ! Difference of pressure
          !
          psb   (ncount)=psur(ncount)*delsig(1)
          !
          !Density of air
          !
          !
          ! P =rho*R*T
          !
          !        P
          !rho =-------
          !       R*T
          !
          !               1       100*psur         1             Pa
          !rhoair(i) = ------- * ---------- = ----------- *-----------------
          !               R         Tm         (J/kg/K)           K
          !
          !                kg*K            N/m^2
          !rhoair(i) =  ----------- *-----------------
          !                  J              K
          !
          !                kg*K               kg*m*s^-2* m^-2
          !rhoair(i) =  ---------------- * -----------------
          !               (kg*m*s^-2*m)          K
          !
          !                 kg
          !rhoair(i) =  --------
          !                 m^3
          !
          rhoair(ncount)=r100*psur(ncount)/tm(ncount)
          !
          ! hight in meter of first level of model
          !
          !
          !         1
          ! DZ = ------- * DP
          !      rho*g
          !
          !..delsig     k=2  ****gu,gv,gt,gq,gyu,gyv,gtd,gqd,sig*** } delsig(2)
          !             k=3/2----si,ric,rf,km,kh,b,l -----------
          !             k=1  ****gu,gv,gt,gq,gyu,gyv,gtd,gqd,sig*** } delsig(1)
          !             k=1/2----si ----------------------------
          !
          !                1                           m^3 s^2       kg * m
          ! Z(k) - Zo = -------- * (Ps*si(k) - Ps) = -------    * --------    *100
          !              rho*g                          kg m          m^2 s^2
          !
          !
          tvland(ncount)=tm(ncount)*(1.0_r8+0.608_r8*qm(ncount))
          ztn   (ncount)=MAX((rbyg * tvland(ncount) ),0.5_r8)
          !ztn   (ncount)=(1.0_r8/(rhoair(ncount)*grav))*100.0_r8*(psur(ncount) - psur(ncount)*sl(1))
          !
          !         J          kg          J
          ! rcp = -------- * ------- = ----------
          !        kg * K      m^3       K * m^3
          !
          rcp   (ncount)=cp*rhoair(ncount)
       END IF
       rhi(i)=1.0e-6_r8
       rmi(i)=1.0e-6_r8
       tvsea(i)=gt(i,1)*(1.0_r8+0.608_r8*gq(i,1))
       ztn2(i)=MAX((rbyg * tvsea(i)),0.5_r8)
       !ztn2(i)=(1.0_r8/((r100*gps(i)/gt(i,1))*grav))*100.0_r8*(gps(i) - gps(i)*sl(1))
    END DO

    IF(schemes==1)THEN
       CALL SSiB_Driver(&
            jdt                ,latitu               ,bps(1:nCols)     ,psb(1:nCols)     ,&
            dzm(1:nCols)       ,em(1:nCols)          ,gmt(1:nCols,:)   ,gmq(1:nCols,:)   ,&
            gmu(1:nCols,:)     ,cu(1:nCols)          ,cuni(1:nCols)    ,ctni(1:nCols)    ,&
            ustar(1:nCols)     ,cosz(1:nCols)        ,sinclt(1:nCols)  ,rhoair(1:nCols)  ,&
            psy(1:nCols)       ,rcp(1:nCols)         ,wc(1:nCols)      ,wg(1:nCols)      ,&
            fc(1:nCols)        ,fg(1:nCols)          ,hr(1:nCols)      ,ect(1:nCols)     ,&
            eci(1:nCols)       ,egt(1:nCols)         ,egi(1:nCols)     ,egs(1:nCols)     ,&
            ec(1:nCols)        ,eg(1:nCols)          ,hc(1:nCols)      ,hg(1:nCols)      ,&
            ecidif(1:nCols)    ,egidif(1:nCols)      ,ecmass(1:nCols)  ,egmass(1:nCols)  ,&
            etmass(1:nCols)    ,hflux(1:nCols)       ,chf(1:nCols)     ,shf(1:nCols)     ,&
            fluxef(1:nCols)    ,roff(1:nCols)        ,drag(1:nCols)    ,ra(1:nCols)      ,&
            rb(1:nCols)        ,rd(1:nCols)          ,rc(1:nCols)      ,rg(1:nCols)      ,&
            tcta(1:nCols)      ,tgta(1:nCols)        ,ta(1:nCols)      ,ea(1:nCols)      ,&
            etc(1:nCols)       ,etg(1:nCols)         ,btc(1:nCols)     ,btg(1:nCols)     ,&
            u2(1:nCols)        ,radt(1:nCols,:)      ,par(1:nCols,:)   ,pd(1:nCols,:)    ,&
            rst(1:nCols,:)     ,rsoil(1:nCols)       ,phroot(1:nCols,:),hrr(1:nCols)     ,&
            phsoil(1:nCols,:)  ,cc(1:nCols)          ,cg(1:nCols)      ,satcap(1:nCols,:),&
            snow(1:nCols,:)    ,dtc(1:nCols)         ,dtg(1:nCols)     ,dtm(1:nCols)     ,&
            dqm(1:nCols)       ,stm(1:nCols,:)       ,extk(1:nCols,:,:,:),radfac(1:nCols,:,:,:),&
            closs(1:nCols)     ,gloss(1:nCols)       ,thermk(1:nCols)  ,p1f(1:nCols)     ,&
            p2f(1:nCols)       ,tc(1:nCols)          ,tg(1:nCols)      ,td(1:nCols)      ,&
            capac(1:nCols,:)   ,w(1:nCols,:)         ,qm(1:nCols)      ,tm(1:nCols)      ,&
            um(1:nCols)        ,vm(1:nCols)          ,psur(1:nCols)    ,ppc(1:nCols)     ,&
            ppl(1:nCols)       ,radn(1:nCols,:,:)    ,itype(1:nCols)   ,dtc3x           ,&
            mon (1:nCols)      ,nmax                 ,nCols                             ,&
            zlwup(1:nCols)       ,salb(1:nCols,:,:),tgeff(1:nCols)   ,&
            nsx(1:nCols)       ,tmtx(1:nCols,:,:)    ,qmtx(1:nCols,:,:),umtx (1:nCols,:,:),&
            gt(1:nCols,:)      ,gq (1:nCols,:)       ,gu (1:nCols,:)   ,gv(1:nCols,:)  ,&
            gps(1:nCols)       ,imask(1:nCols)       ,kMax             ,&
            ktm                ,initlz               ,tcm (1:nCols)    ,tgm (1:nCols)    ,&
            tdm (1:nCols)      ,wm(1:nCols,:)        ,beam_visb (1:nCols),beam_visd(1:nCols),&
            beam_nirb(1:nCols) ,beam_nird(1:nCols)   ,cos2(1:nCols)    ,dlwbot(1:nCols),&
            capacm (1:nCols,:) ,td0(1:nCols)         ,tg0(1:nCols)     ,zenith(1:nCols),&
            kt                 ,iswrad               ,ilwrad           ,xvisb(1:nCols)   ,&
            xvisd(1:nCols)     ,xnirb(1:nCols)       ,xnird(1:nCols)   ,ppli(1:nCols)    ,&
            ppci(1:nCols)      ,tsea(1:nCols)        ,ssib(1:nCols)    ,w0(1:nCols,:)    ,&
            tc0(1:nCols)       ,capac0(1:nCols,:)    ,intg             ,slrad(1:nCols)   ,&
            tseam (1:nCols)    ,tsurf(1:nCols)       ,qsurf(1:nCols)   ,colrad(1:nCols)  ,&
            sigki  (1)         ,delsig(1)            ,sens(1:nCols)    ,evap(1:nCols)    ,&
            umom(1:nCols)      ,vmom(1:nCols)        ,zorl(1:nCols)    ,rmi(1:nCols)     ,&
            rhi(1:nCols)       ,cond(1:nCols)        ,stor(1:nCols)    ,z0x(1:nCols)     ,&
            speedm(1:nCols)    ,Ustarm(1:nCols)      ,z0sea(1:nCols)   ,rho(1:nCols)     ,&
            d (1:nCols)        ,qsfc0(1:nCols)       ,tsfc0(1:nCols)   ,qsfcm(1:nCols)    ,&
            tsfcm(1:nCols)     ,qsfc(1:nCols)        ,tsfc(1:nCols)    ,qm0(1:nCols)    ,&
            qmm(1:nCols)       ,tm0(1:nCols)         ,tmm(1:nCols)    ,mskant(1:nCols))
    END IF

    IF(schemes==2)THEN
       PRINT*,'ERROR schemes 2'
    END IF
    ncount=0
    DO i=1,nCols
       IF(imask(i).GE.1) THEN
          ncount=ncount+1
          htdisp(i)=d(ncount)
          qsfc0(i)=MAX(1.0e-12_r8,qm0 (ncount))
          qsfcm(i)=MAX(1.0e-12_r8,qmm (ncount))
          tsfc0(i)=tm0 (ncount)
          tsfcm(i)=tmm (ncount)
          qm(ncount)  = gq(i,1)
          tm(ncount)  = gt(i,1)
       END IF
       speedm(i)=SQRT((gu(i,1) /SIN(colrad(i)))**2  + (gv(i,1) /SIN(colrad(i)))**2)
       speedm(i)=MAX(2.0_r8 ,speedm(i))
    END DO
    znew=2.0_r8
    theta2m=0.0_r8
    velc2m =0.0_r8
    q2m    =0.0_r8

    CALL CALC2MLAND(nmax,nmax,dtc3x,ustar(1:nmax),tm(1:nmax),tm0(1:nmax),tg(1:nmax),qm(1:nmax),qm0(1:nmax),&
         um(1:nmax),vm(1:nmax),hc(1:nmax),hg(1:nmax),ec(1:nmax),eg(1:nmax),bps(1:nmax),&
         ztn(1:nmax),z0x(1:nmax),rhoair(1:nmax),hr(1:nmax),ta(1:nmax),ea(1:nmax),&
         etc(1:nmax),etg(1:nmax),psur(1:nmax),d(1:nmax) ,znew,&
         theta2m(1:nmax),velc2m(1:nmax),q2m(1:nmax))
    znew=10.0_r8
    theta10m=0.0_r8
    velc10m =0.0_r8
    q10m    =0.0_r8
    CALL CALC2MLAND(nmax,nmax,dtc3x,&
         ustar(1:nmax),tm (1:nmax),tm0(1:nmax),tg(1:nmax),qm(1:nmax),qm0(1:nmax),&
         um   (1:nmax),vm (1:nmax),hc    (1:nmax),hg(1:nmax),ec(1:nmax),eg(1:nmax),bps(1:nmax),&
         ztn  (1:nmax),z0x(1:nmax),rhoair(1:nmax),hr(1:nmax),ta(1:nmax),ea(1:nmax),&
         etc  (1:nmax),etg(1:nmax),psur  (1:nmax),d (1:nmax),znew,&
         theta10m(1:nmax),velc10m(1:nmax),q10m(1:nmax))

    dtc3xi=1.0_r8/dtc3x
    ncount=0
    DO i=1,nCols
       IF (imask(i).GE.1) THEN
          ncount=ncount+1
          IF(mskant(i) /= 1_i8)THEN
             Ustarm(i) = ustar (ncount)
             sens  (i) = (hc   (ncount) + hg(ncount))*dtc3xi
             evap  (i) = (ec   (ncount) + eg(ncount))*dtc3xi
             z0sea (i) = z0x   (ncount)
             rho   (i) = rhoair(ncount)
	  END IF   
       END IF
    END DO

    znew              =2.0_r8
    temp2m   (1:nCols)=0.0_r8
    VELC_2m  (1:nCols)=0.0_r8
    umes2m   (1:nCols)=0.0_r8
    CALL CALC2MSEAICE(nCols,speedm(1:nCols),Ustarm(1:nCols),sens(1:nCols),evap(1:nCols),&
    	 znew,z0sea(1:nCols),tsurf(1:nCols),&
    	 gt(1:nCols,1),sigki(1) ,qsurf(1:nCols),gq(1:nCols,1),ztn2(1:nCols),rho(1:nCols),&
    	 temp2m (1:nCols),VELC_2m (1:nCols),umes2m (1:nCols))
    znew              =10.0_r8
    THETA_10M(1:nCols)= 0.0_r8
    VELC_10M (1:nCols)= 0.0_r8
    MIXQ_10M (1:nCols)= 0.0_r8
    CALL CALC2MSEAICE(nCols,speedm(1:nCols),Ustarm(1:nCols),sens(1:nCols),evap(1:nCols),&
    	 znew,z0sea(1:nCols),tsurf(1:nCols),&
    	 gt(1:nCols,1),sigki(1) ,qsurf(1:nCols),gq(1:nCols,1),ztn2(1:nCols),rho(1:nCols),&
    	 THETA_10M(1:nCols),VELC_10M(1:nCols),MIXQ_10M(1:nCols))
    
    ustar2=Ustarm
    ncount=0
    DO i=1,nCols
       z0  (i)=z0sea(i)
       IF(imask(i).GE.1) THEN
          ncount=ncount+1
          VELC_10M (i)=velc10m (ncount)!theta2m(1:nmax),velc2m(1:nmax),q2m(1:nmax)
          VELC_2M  (i)=velc2m  (ncount)
          MIXQ_10M (i)=q10m    (ncount)
          umes2m  (i)=q2m     (ncount)
          temp2m (i)=theta2m (ncount)/bps(ncount)
          THETA_10M(i)=theta10m(ncount)/bps(ncount)
          IF(imask(i)==13)SICE2(i)=10.0_r8
          ustar2(i)=ustar(ncount)
          z0    (i)=z0x(ncount)
       END IF
    END DO
    DO i = 1, ncols
       IF (tsea(i) < 0.0_r8 .AND. ABS(tsea(i)) < 271.17_r8) THEN
          SICE2(i)=5.0_r8
       END IF
    END DO

    !
    ! |V|  (u**2 + v**2)^1/2
    !
    ! u = -|V| sin ( pi/180 * phi_met )
    !
    ! v = -|V| cos ( pi/180 * phi_met )
    !
    ! phi_met = phi_vect + 180
    !
    DO i=1,nCols
       VELC   (i)=MAX(SQRT( gu(i,1)**2 + gv(i,1)**2 )*(1.0_r8 /SIN( colrad(i))),0.25_r8 )
       DirWind(i) = asin((MAX(MIN((-gu (i,1)/SIN( colrad(i)))/VELC(i),1.0_r8),-1.0_r8)))
       uve10m (i) = -MAX(MIN(VELC_10M(i)*SIN(DirWind(i)),100.0_r8),-100.0_r8)
       DirWind(i) = acos((MAX(MIN((-gv (i,1)/SIN( colrad(i)))/VELC(i),1.0_r8),-1.0_r8)))
       vve10m (i) = -MAX(MIN(VELC_10M(i)*COS(DirWind(i)),100.0_r8),-100.0_r8)
    END DO
    !
    !     pointwise diagnostics
    !
    ncount=0
    DO i=1,nCols
       IF(imask(i).GE.1_i8) THEN
          ncount=ncount+1
          fmom   =rhoair(ncount)*cu(ncount)*ustar(ncount)
          umom(i)=fmom*um(ncount)
          vmom(i)=fmom*vm(ncount)
          umom(i)=MIN( 10.0_r8,umom(i))
          vmom(i)=MIN( 10.0_r8,vmom(i))
          umom(i)=MAX(-10.0_r8,umom(i))
          vmom(i)=MAX(-10.0_r8,vmom(i))
       END IF
    END DO

   ncount=0
    DO i=1,nCols
       IF (imask(i).GE.1) THEN
          ncount=ncount+1
          sens  (i) = (hc   (ncount) + hg(ncount))*dtc3xi
          evap  (i) = (ec   (ncount) + eg(ncount))*dtc3xi
       END IF   
    END DO

    !-----------------
    ! Storage Diagnostic Fields
    !------------------
    IF( StartStorDiag)THEN
      CALL SfcDiagnStorage(nCols    ,dtc3xi,VELC_10M,VELC_2m,MIXQ_10M,& 
                         umes2m,temp2m,THETA_10M,uve10m,vve10m  ,sens	,evap	 ,&  
                         imask	,eci	,egi	 ,roff  ,ta    ,ea	,egs   ,ra	,rb	,rd	 ,&
                         rg    ,rmi   ,rhi	,tsurf ,umom	,vmom	,latitu    )
    END IF 
    !-----------------
    ! Storage Gridhistory Fields
    !------------------
    IF (ghl_local) THEN
       CALL SfcGridHistoryStorage(&
                         icg    ,nCols    ,dtc3xi ,latitu ,ra      , &
                         rhi    ,imask  ,rmi      ,drag     ,radt   ,rb     ,rd      , &
                         rc     ,rg     ,rsoil    ,ea       ,ta     ,hc     ,hg      , &  
                         ect    ,egt    ,eci      ,egi      ,egs    ,chf    ,shf     , &  
                         roff   ,tsurf  ,zlwup    ,umom     ,vmom   ,sens   ,evap    , &
                         cond   ,stor   ,temp2m   ,umes2m   ,uve10m ,vve10m  )
    END IF
  END SUBROUTINE surface_driver
  
  SUBROUTINE SfcGridHistoryStorage(&
                         icg    ,nCols    ,dtc3xi ,latitu ,ra      , &
                         rhi    ,imask  ,rmi      ,drag     ,radt   ,rb     ,rd      , &
                         rc     ,rg     ,rsoil    ,ea       ,ta     ,hc     ,hg      , &  
                         ect    ,egt    ,eci      ,egi      ,egs    ,chf    ,shf     , &  
                         roff   ,tsurf  ,zlwup    ,umom     ,vmom   ,sens   ,evap    , &
                         cond   ,stor   ,temp2m   ,umes2m   ,uve10m ,vve10m  )
   IMPLICIT NONE
   INTEGER, INTENT(IN   )       :: icg
   INTEGER, INTENT(IN   )       :: nCols 
   REAL(KIND=r8), INTENT(IN   ) :: dtc3xi
   INTEGER, INTENT(IN   )       :: latitu
   REAL(KIND=r8), INTENT(IN   ) :: ra	 (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: rhi   (nCols)
   INTEGER(KIND=i8), INTENT(IN) :: imask (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: rmi   (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: drag  (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: radt  (nCols,icg)
   REAL(KIND=r8), INTENT(IN   ) :: rb	 (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: rd	 (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: rc	 (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: rg	 (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: rsoil (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: ea	 (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: ta	 (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: hc	 (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: hg	 (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: ect   (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: egt   (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: eci   (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: egi   (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: egs   (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: chf   (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: shf   (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: roff  (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: tsurf (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: zlwup (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: umom  (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: vmom  (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: sens  (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: evap  (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: cond  (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: stor  (nCols)
   REAL(KIND=r8), INTENT(IN   ) :: temp2m(nCols)
   REAL(KIND=r8), INTENT(IN   ) :: umes2m(nCols)
   REAL(KIND=r8), INTENT(IN   ) :: uve10m(nCols)
   REAL(KIND=r8), INTENT(IN   ) :: vve10m(nCols)
   
   INTEGER :: i
   INTEGER :: ncount
   REAL(KIND=r8) :: bfrg  (nCols)


          IF (dogrh(nGHis_hcseai,latitu)) CALL StoreGridHistory(  cond  (1:nCols),nGHis_hcseai,latitu)
          IF (dogrh(nGHis_hsseai,latitu)) CALL StoreGridHistory(  stor  (1:nCols),nGHis_hsseai,latitu)
          IF (dogrh(nGHis_tep02m,latitu)) CALL StoreGridHistory(  temp2m(1:nCols),nGHis_tep02m,latitu)
          IF (dogrh(nGHis_mxr02m,latitu)) CALL StoreGridHistory(  umes2m(1:nCols),nGHis_mxr02m,latitu)
          IF (dogrh(nGHis_zwn10m,latitu)) CALL StoreGridHistory(  uve10m(1:nCols),nGHis_zwn10m,latitu)
          IF (dogrh(nGHis_mwn10m,latitu)) CALL StoreGridHistory(  vve10m(1:nCols),nGHis_mwn10m,latitu)
 
          IF(dogrh(nGHis_casrrs,latitu))THEN
             ncount=0
             DO i=1,nCols
                IF(imask(i).GE.1_i8) THEN
                   ncount=ncount+1
                   bfrg(i)=ra(ncount)
                ELSE
                   bfrg(i)=1.0_r8/rhi(i)
                END IF
             END DO
             CALL StoreGridHistory (bfrg(1:nCols  ), nGHis_casrrs, latitu)
          END IF
	  
          IF(dogrh(nGHis_mofres,latitu))THEN
             ncount=0
             DO i=1,nCols
                IF(imask(i).GE.1_i8) THEN
                   ncount=ncount+1
                   bfrg(i)=0.0e0_r8
                ELSE
                   bfrg(i)=1.0_r8/rmi(i)
                END IF
             END DO
             CALL StoreGridHistory (bfrg(1:nCols  ), nGHis_mofres, latitu)
          END IF
          
	  IF(dogrh(nGHis_dragcf,latitu))CALL StoreMaskedGridHistory(drag (1:ncols  ),imask,nGHis_dragcf,latitu)
          IF(dogrh(nGHis_nrdcan,latitu))CALL StoreMaskedGridHistory(radt (1:ncols,1),imask,nGHis_nrdcan,latitu)
          IF(dogrh(nGHis_nrdgsc,latitu))CALL StoreMaskedGridHistory(radt (1:ncols,2),imask,nGHis_nrdgsc,latitu)
          IF(dogrh(nGHis_cascrs,latitu))CALL StoreMaskedGridHistory(rb   (1:ncols  ),imask,nGHis_cascrs,latitu)
          IF(dogrh(nGHis_casgrs,latitu))CALL StoreMaskedGridHistory(rd   (1:ncols  ),imask,nGHis_casgrs,latitu)
          IF(dogrh(nGHis_canres,latitu))CALL StoreMaskedGridHistory(rc   (1:ncols  ),imask,nGHis_canres,latitu)
          IF(dogrh(nGHis_gcovrs,latitu))CALL StoreMaskedGridHistory(rg   (1:ncols  ),imask,nGHis_gcovrs,latitu)
          IF(dogrh(nGHis_bssfrs,latitu))CALL StoreMaskedGridHistory(rsoil(1:ncols ),imask,nGHis_bssfrs,latitu)
          IF(dogrh(nGHis_ecairs,latitu))CALL StoreMaskedGridHistory(ea   (1:ncols  ),imask,nGHis_ecairs,latitu)
          IF(dogrh(nGHis_tcairs,latitu))CALL StoreMaskedGridHistory(ta   (1:ncols  ),imask,nGHis_tcairs,latitu)
          IF(dogrh(nGHis_shfcan,latitu))CALL StoreMaskedGridHistory(hc   (1:ncols  ),imask,nGHis_shfcan,latitu,dtc3xi)
          IF(dogrh(nGHis_shfgnd,latitu))CALL StoreMaskedGridHistory(hg   (1:ncols  ),imask,nGHis_shfgnd,latitu,dtc3xi)
          IF(dogrh(nGHis_tracan,latitu))CALL StoreMaskedGridHistory(ect  (1:ncols  ),imask,nGHis_tracan,latitu,dtc3xi)
          IF(dogrh(nGHis_tragcv,latitu))CALL StoreMaskedGridHistory(egt  (1:ncols  ),imask,nGHis_tragcv,latitu,dtc3xi)
          IF(dogrh(nGHis_inlocp,latitu))CALL StoreMaskedGridHistory(eci  (1:ncols  ),imask,nGHis_inlocp,latitu,dtc3xi)
          IF(dogrh(nGHis_inlogc,latitu))CALL StoreMaskedGridHistory(egi  (1:ncols  ),imask,nGHis_inlogc,latitu,dtc3xi)
          IF(dogrh(nGHis_bsevap,latitu))CALL StoreMaskedGridHistory(egs  (1:ncols  ),imask,nGHis_bsevap,latitu,dtc3xi)
          IF(dogrh(nGHis_canhea,latitu))CALL StoreMaskedGridHistory(chf  (1:ncols  ),imask,nGHis_canhea,latitu)
          IF(dogrh(nGHis_gcheat,latitu))CALL StoreMaskedGridHistory(shf  (1:ncols  ),imask,nGHis_gcheat,latitu)
          IF(dogrh(nGHis_runoff,latitu))CALL StoreMaskedGridHistory(roff (1:ncols  ),imask,nGHis_runoff,latitu,1000.0_r8*dtc3xi)
          ncount=0
          DO i=1,nCols
             IF(dogrh(nGHis_lwubot,latitu))bfrg(i)=stefan*tsurf(i)**4
             IF(imask(i).GE.1) THEN
                ncount=ncount+1
                IF(dogrh(nGHis_lwubot,latitu))bfrg(i)=zlwup(ncount)
             ELSE
                IF(dogrh(nGHis_lwubot,latitu))bfrg(i)=stefan*tsurf(i)**4
             END IF
          END DO

          IF(dogrh(nGHis_lwubot,latitu))CALL StoreGridHistory(bfrg(1:nCols),nGHis_lwubot,latitu)
          IF(dogrh(nGHis_ustres,latitu))CALL StoreGridHistory(umom(1:nCols),nGHis_ustres,latitu)
          IF(dogrh(nGHis_vstres,latitu))CALL StoreGridHistory(vmom(1:nCols),nGHis_vstres,latitu)
          IF(dogrh(nGHis_sheatf,latitu))CALL StoreGridHistory(sens(1:nCols),nGHis_sheatf,latitu)
          IF(dogrh(nGHis_lheatf,latitu))CALL StoreGridHistory(evap(1:nCols),nGHis_lheatf,latitu)    
  END SUBROUTINE SfcGridHistoryStorage
  
  SUBROUTINE SfcDiagnStorage(nCols    ,dtc3xi,VELC_10M,VELC_2m,MIXQ_10M,& 
                             umes2m,temp2m,THETA_10M,uve10m,vve10m  ,sens   ,evap    ,&	 
                             imask   ,eci    ,egi     ,& 
                             roff  ,ta	  ,ea       ,egs   ,ra      ,rb     ,rd	     ,&
                             rg	   ,rmi   ,rhi	    ,tsurf ,umom    ,vmom   ,latitu	 )
    IMPLICIT NONE
    INTEGER      , INTENT(IN) :: nCols
    REAL(KIND=r8), INTENT(IN) :: dtc3xi
    INTEGER      , INTENT(IN) :: latitu
    REAL(KIND=r8), INTENT(IN) :: VELC_10M (nCols)
    REAL(KIND=r8), INTENT(IN) :: VELC_2m  (nCols)
    REAL(KIND=r8), INTENT(IN) :: MIXQ_10M (nCols)
    REAL(KIND=r8), INTENT(IN) :: umes2m   (nCols)
    REAL(KIND=r8), INTENT(IN) :: temp2m   (nCols)
    REAL(KIND=r8), INTENT(IN) :: THETA_10M(nCols)
    REAL(KIND=r8), INTENT(IN) :: uve10m   (nCols)
    REAL(KIND=r8), INTENT(IN) :: vve10m   (nCols)
    REAL(KIND=r8), INTENT(IN) :: sens     (nCols)
    REAL(KIND=r8), INTENT(IN) :: evap     (nCols)
    INTEGER(KIND=r8),INTENT(IN)::  imask  (nCols)
    REAL(KIND=r8), INTENT(IN) :: eci      (nCols)
    REAL(KIND=r8), INTENT(IN) :: egi      (nCols)
    REAL(KIND=r8), INTENT(IN) :: roff     (nCols)
    REAL(KIND=r8), INTENT(IN) :: ta       (nCols)
    REAL(KIND=r8), INTENT(IN) :: ea       (nCols)
    REAL(KIND=r8), INTENT(IN) :: egs      (nCols)  
    REAL(KIND=r8), INTENT(IN) :: ra       (nCols)
    REAL(KIND=r8), INTENT(IN) :: rb       (nCols)
    REAL(KIND=r8), INTENT(IN) :: rd       (nCols)
    REAL(KIND=r8), INTENT(IN) :: rg       (nCols)
    REAL(KIND=r8), INTENT(IN) :: rmi      (nCols)
    REAL(KIND=r8), INTENT(IN) :: rhi      (nCols)
    REAL(KIND=r8), INTENT(IN) :: tsurf    (nCols)
    REAL(KIND=r8), INTENT(IN) :: umom     (nCols)
    REAL(KIND=r8), INTENT(IN) :: vmom     (nCols)
    REAL(KIND=r8) :: bfr1     (nCols)
    REAL(KIND=r8) :: bfr2     (nCols)
    REAL(KIND=r8) :: swrk     (nCols,8)
    INTEGER :: ncount
    INTEGER :: i
       IF(dodia(nDiag_spw10m)) CALL updia(VELC_10M ,nDiag_spw10m,latitu)
 
       IF(dodia(nDiag_spw02m)) CALL updia(VELC_2m  ,nDiag_spw02m,latitu)

       IF(dodia(nDiag_mxr10m)) CALL updia(MIXQ_10M ,nDiag_mxr10m,latitu)

       IF(dodia(nDiag_mxr02m)) CALL updia(umes2m   ,nDiag_mxr02m,latitu)

       IF(dodia(nDiag_tep02m)) CALL updia(temp2m   ,nDiag_tep02m,latitu)

       IF(dodia(nDiag_tep10m)) CALL updia(THETA_10M,nDiag_tep10m,latitu)

       IF(dodia(nDiag_zwn10m)) CALL updia(uve10m   ,nDiag_zwn10m,latitu)

       IF(dodia(nDiag_mwn10m)) CALL updia(vve10m   ,nDiag_mwn10m,latitu)  
       ncount=0
       DO i=1,nCols       
          IF(dodia(nDiag_intlos))bfr1(i)=0.0_r8
          IF(dodia(nDiag_runoff))bfr2(i)=0.0_r8
          IF(dodia(nDiag_tcairs))swrk(i,1)=tsurf(i)
          IF(dodia(nDiag_ecairs))swrk(i,2)=0.0_r8
          IF(dodia(nDiag_bsolht))swrk(i,3)=0.0_r8
          IF(dodia(nDiag_cascrs))swrk(i,6)=0.0_r8
          IF(dodia(nDiag_casgrs))swrk(i,7)=0.0_r8
          IF(dodia(nDiag_gcovrs))swrk(i,8)=0.0_r8
          IF(imask(i).GE.1) THEN
    	     ncount=ncount+1
    	     IF(dodia(nDiag_intlos))bfr1(i  ) = bfr1(i)+(eci (ncount)+egi (ncount))*dtc3xi
    	     IF(dodia(nDiag_runoff))bfr2(i  ) = bfr2(i)+ roff(ncount)*dtc3xi*1000.0_r8
    	     IF(dodia(nDiag_tcairs))swrk(i,1) = ta(ncount)
    	     IF(dodia(nDiag_ecairs))swrk(i,2) = ea(ncount)
    	     IF(dodia(nDiag_bsolht))swrk(i,3) = egs(ncount)*dtc3xi
    	     IF(dodia(nDiag_mofres))swrk(i,4) = 0.0_r8
    	     IF(dodia(nDiag_casrrs))swrk(i,5) = ra(ncount)
    	     IF(dodia(nDiag_cascrs))swrk(i,6) = rb(ncount)
    	     IF(dodia(nDiag_casgrs))swrk(i,7) = rd(ncount)
    	     IF(dodia(nDiag_gcovrs))swrk(i,8) = rg(ncount)
          ELSE
    	     IF(dodia(nDiag_mofres))swrk(i,4)=1.0_r8/rmi(i)
    	     IF(dodia(nDiag_casrrs))swrk(i,5)=1.0_r8/rhi(i)
          ENDIF
       END DO
       IF(dodia(nDiag_sheatf)) CALL updia(sens	      ,nDiag_sheatf,latitu)
       IF(dodia(nDiag_lheatf)) CALL updia(evap	      ,nDiag_lheatf,latitu)
       IF(dodia(nDiag_intlos)) CALL updia(bfr1	      ,nDiag_intlos,latitu)
       IF(dodia(nDiag_runoff)) CALL updia(bfr2	      ,nDiag_runoff,latitu)
       IF(dodia(nDiag_tcairs)) CALL updia(swrk(1:nCols,1),nDiag_tcairs,latitu)
       IF(dodia(nDiag_ecairs)) CALL updia(swrk(1:nCols,2),nDiag_ecairs,latitu)
       IF(dodia(nDiag_bsolht)) CALL updia(swrk(1:nCols,3),nDiag_bsolht,latitu)
       IF(dodia(nDiag_mofres)) CALL updia(swrk(1:nCols,4),nDiag_mofres,latitu)
       IF(dodia(nDiag_casrrs)) CALL updia(swrk(1:nCols,5),nDiag_casrrs,latitu)
       IF(dodia(nDiag_cascrs)) CALL updia(swrk(1:nCols,6),nDiag_cascrs,latitu)
       IF(dodia(nDiag_casgrs)) CALL updia(swrk(1:nCols,7),nDiag_casgrs,latitu)
       IF(dodia(nDiag_gcovrs)) CALL updia(swrk(1:nCols,8),nDiag_gcovrs,latitu)

       IF(dodia(nDiag_ustres))THEN
          DO i=1,nCols
    	     bfr1(i)=-umom(i)
          END DO
          CALL updia(bfr1,nDiag_ustres,latitu)
       END IF
       IF(dodia(nDiag_vstres))THEN
          DO i=1,nCols
    	     bfr2(i)=-vmom(i)
          END DO
          CALL updia(bfr2,nDiag_vstres,latitu)
       END IF
       IF(dodia(nDiag_lwubot))THEN
          DO i=1,ncols
             bfr1(i)=stefan*tsurf(i)**4
          END DO
          CALL updia(bfr1,nDiag_lwubot,latitu)
       END IF

  END SUBROUTINE SfcDiagnStorage
  !
  !
  !
  SUBROUTINE Phenology(latco,nCols,nmax,itype,colrad2, month2, xday, idatec,nsx)
    IMPLICIT NONE
    INTEGER      , INTENT(IN   ) :: latco
    INTEGER      , INTENT(IN   ) :: nCols
    INTEGER      , INTENT(IN   ) :: nmax
    INTEGER      , INTENT(in )   :: itype   (ncols)
    REAL(KIND=r8), INTENT(in )   :: colrad2 (ncols)
    INTEGER      , INTENT(in )   :: month2  (ncols)
    REAL(KIND=r8), INTENT(in )   :: xday
    INTEGER      , INTENT(in )   :: idatec(4)
    INTEGER      , INTENT(inout) :: nsx     (ncols)
    IF(schemes==1)CALL CopySurfaceData(itype,month2,colrad2,xday,idatec,nsx,nCols,nmax,latco)
    IF(schemes==2) PRINT*,'ERROR schemes 2'
  END SUBROUTINE Phenology


  SUBROUTINE CALC2MLAND(nCols,nmax,dtc3x,ustar,tm,tm0,tg,qm,qm0,um,vm,hc,hg,ec,eg,bps,ztn,&
       z0x,rhoair,hr,ta,ea,etc,etg,psur,d,znew,theta2m,velc2m,q2m)
    INTEGER      , INTENT(IN   ) :: nCols
    INTEGER      , INTENT(IN   ) :: nmax
    REAL(KIND=r8), INTENT(IN   ) :: dtc3x
    REAL(KIND=r8), INTENT(IN   ) :: ustar(nCols)
    REAL(KIND=r8), INTENT(IN   ) :: tm   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: tm0  (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: tg   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: qm   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: qm0  (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: um   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: vm   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: hc   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: hg   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: ec   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: eg   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: bps  (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: ztn  (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: z0x  (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: rhoair(nCols)
    REAL(KIND=r8), INTENT(IN   ) :: hr    (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: ta    (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: ea    (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: etc   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: etg   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: psur  (nCols)!gps is in mb
    REAL(KIND=r8), INTENT(IN   ) :: d     (nCols)!gps is in mb
    REAL(KIND=r8), INTENT(IN   ) :: znew
    REAL(KIND=r8), INTENT(OUT  ) :: theta2m  (nCols)
    REAL(KIND=r8), INTENT(OUT  ) :: velc2m   (nCols)
    REAL(KIND=r8), INTENT(OUT  ) :: q2m     (nCols)
    REAL(KIND=r8) :: speed(nCols)
    REAL(KIND=r8) :: zrough(nCols)
    REAL(KIND=r8) :: SensC(nCols)
    REAL(KIND=r8) :: evapC(nCols)
    REAL(KIND=r8) :: SensG(nCols)
    REAL(KIND=r8) :: evapG(nCols)
    REAL(KIND=r8) :: Sens(nCols)
    REAL(KIND=r8) :: evap(nCols)
    REAL(KIND=r8) :: Qstar(nCols)
    REAL(KIND=r8) :: QstarG(nCols)
    REAL(KIND=r8) :: Tstar(nCols)
    REAL(KIND=r8) :: Tstarg(nCols)
    REAL(KIND=r8) :: thetam(nCols)
    REAL(KIND=r8) :: thetac(nCols)
    REAL(KIND=r8) :: qrm(nCols)
    REAL(KIND=r8) :: qrc(nCols)
    REAL(KIND=r8) :: qrg(nCols)
    REAL(KIND=r8) :: rca(nCols)
    REAL(KIND=r8) :: rcm(nCols)
    REAL(KIND=r8) :: speedm(nCols)
    REAL(KIND=r8) :: dtc3xi
    REAL(KIND=r8) :: Rib(nCols),ZbyL(nCols),x,FHIm(nCols),FHIh(nCols)
    INTEGER :: i
    REAL(KIND=r8), PARAMETER ::  vonk = 0.40_r8

    !  ta ..........Temperatura no nivel de fonte de calor do dossel (K)
    !  ea ..........Pressure of vapor           GSA2006.
    !  etc..........Saturation Pressure of vapor at top of the copa
    !  etg..........Saturation Pressao of vapor  at base of the copa

    dtc3xi=1.0_r8 /dtc3x

    DO i = 1, nmax
       Rib   (i)  =ea  (i)! will be used
       Rib   (i)  =psur(i)! will be used
       Rib   (i)  =etc (i)! will be used
       Rib   (i)  =ta  (i)! will be used
       Rib   (i)  =etg (i)! will be used
       Rib   (i)  =hr  (i)! will be used

       Rib   (i) = 0.0_r8
       ZbyL  (i) = 0.0_r8
       FHIm  (i) = 0.0_r8
       FHIh  (i) = 0.0_r8
       speed (i) = SQRT(um(i)**2 + vm(i)**2)
       speed (i) = MAX(2.0_r8  ,speed(i))
       speedm(i) = speed (i)
       zrough(i) = z0x(i)
       sensC (i) = hc(i)*dtc3xi
       evapC (i) = ec(i)*dtc3xi
       sensG (i) = hg(i)*dtc3xi
       evapG (i) = eg(i)*dtc3xi
       sens  (i) = hc(i)*dtc3xi  + hg(i)*dtc3xi
       evap  (i) = ec(i)*dtc3xi  + eg(i)*dtc3xi
       Tstar (i) = -sens(i)/(rhoair(i)*cp*Ustar(i))
       Qstar (i) = -evap(i)/(rhoair(i)*hl*Ustar(i))
       Tstarg(i) = -sensG(i)/(rhoair(i)*cp*Ustar(i))
       Qstarg(i) = -evapG(i)/(rhoair(i)*hl*Ustar(i))
       qrm   (i) =qm0(i)!(hr(i)*ea (i) )*epsfac/(psur(i) - ea (i))
       qrc   (i) =qm0(i)!(hr(i)*etc(i) )*epsfac/(psur(i) - etc(i))
       qrg   (i) =qm0(i)!(hr(i)*etg(i) )*epsfac/(psur(i) - etg(i))
       !
       !                     kg      J          m*K         J           W
       !Ho = rho*cp*(W'*T')=---- * -------- * ------ = -----------   =-----
       !                     m^3     kg*K        s       m^2 * s       m^2
       !
       !W'T' = -Ustar*Tstar
       !
       !                   Ho
       ! -Ustar*Tstar = --------
       !                 rho*cp
       !
       !            -Ho
       ! Tstar = ---------------
       !           rho*cp*Ustar
       !
       !
       !                      kg      J         m     kg       W
       !Eo = rho*hl*(W'*Q')= ---- * ------ * ------*------ = -----
       !                      m^3     kg        s     kg      m^2
       !
       !  W'Q' = -Ustar*Qstar
       !
       !                   Eo
       ! -Ustar*Qstar = --------
       !                 rho*hl
       !
       !            -Eo
       ! Qstar = ---------------
       !           rho*hl*Ustar
       !
       !Number Richadson Bulk
       !
       !                  --     --
       !        g*z      | z -(-d) |  (Theta -Thetao)
       ! Rib = ----- *ln |---------|---------------------
       !         To      |    zo   |      U^2
       !                  --     --
       !
       !
       Rib(i) =  ((grav*(znew+1.5_r8*d(i)))/(tg(i)/bps(i))) * &
            log(ztn(i)/zrough(i))* ((tm(i)*bps(i) - tg(i))/speed(i))
       Rib(i) = MAX(MIN(Rib(i), 0.2_r8),-5.0_r8)
       !
       !  z
       !---- = Rib  for Rib < 0
       !  L
       !
       !
       !  z       Rib
       !---- = ------------- for Rib >= 0 and Rib < 0.2
       !  L      1 - 5*Rib
       !
       IF(Rib(i) < 0.0_r8)THEN
          ZbyL(i)= Rib(i)
       ELSE IF(Rib(i) >= 0.0_r8 .and. Rib(i) < 0.2_r8)THEN
          ZbyL(i)= (Rib(i)/(1.0_r8 - 5.0_r8*Rib(i)))
       END IF
       !
       ! Wind and Temperature Profiles
       ! FHIm and FHIh are diferents similarity functions
       ! Equations 11.14 Arya
       !
       IF(ZbyL(i) >= 0.0_r8)THEN
          FHIm(i) = -5.0_r8 *  ZbyL(i)
          FHIh(i) = -5.0_r8 *  ZbyL(i)
       ELSE IF(ZbyL(i) <0.0_r8 ) THEN
          x     = (1.0_r8 - 15.0_r8*ZbyL(i))**(0.25_r8)
          FHIm(i) = log((  (1.0_r8 + x**2.0_r8) / 2.0_r8 ) * ( (1.0_r8 + x) / 2.0_r8 )**2.0_r8) &
               - 2.0_r8 * ATAN(x) + pie/2.0_r8
          FHIh(i) = 2.0_r8*log( (1.0_r8 + x**2.0_r8)/2.0_r8 )
       END IF

       IF (znew > 1.5_r8*d(i)) THEN
          thetam(i) = tm(i)*bps(i)
          thetac(i) = tm0(i)*bps(i)
          rcm(i)    = qm     (i)
          rca(i)    = qm0    (i)
          tstar (i) = tstar  (i)
          qstar (i) = qstar  (i)
       ELSE
          thetam(i) = tm(i)*bps(i)
          thetac(i) = tm0(i)*bps(i)
          rcm(i)    = qm    (i)
          rca(i)    = qm0    (i)
          Tstar (i) = Tstarg (i)
          Qstar (i) = Qstarg (i)
       END IF
       !speedm(i) = (ustar(I)/vonk)*( LOG(znew+1.5_r8*d(i))  -  FHIm(i) - LOG(zrough(i))  )
    END DO



    CALL Reduced_temp(nCols  ,speedm  ,ustar ,tstar  ,znew    ,zrough,thetac   ,thetam   , &
         ztn   ,theta2m,0)
    CALL Reduced_wind(nCols  ,speedm  ,ustar         ,znew    ,zrough,thetac   ,thetam   , &
         ztn   ,velc2m,0 )
    CALL Reduced_q   (nCols  ,speedm  ,ustar ,qstar  ,znew    ,zrough,thetac   ,thetam   , &
         rca   ,rcm          ,ztn   ,q2m ,0  )
  END SUBROUTINE CALC2MLAND
  SUBROUTINE CALC2MSEAICE(nCols,speedm,ustar,sens,evap,znew,z0,tsurf,&
       gt,sigki,qsurf,gq,ztn,rho,&
       theta2m,velc2m,q2m)
    INTEGER      , INTENT(IN   ) :: nCols
    REAL(KIND=r8), INTENT(IN   ) :: speedm(nCols)
    REAL(KIND=r8), INTENT(IN   ) :: ustar (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: sens  (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: evap  (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: znew
    REAL(KIND=r8), INTENT(IN   ) :: z0    (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: tsurf (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: gt    (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: sigki (1)
    REAL(KIND=r8), INTENT(IN   ) :: qsurf (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: gq   (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: ztn  (nCols)
    REAL(KIND=r8), INTENT(IN   ) :: rho(nCols)
    REAL(KIND=r8), INTENT(OUT  ) :: theta2m  (nCols)
    REAL(KIND=r8), INTENT(OUT  ) :: velc2m   (nCols)
    REAL(KIND=r8), INTENT(OUT  ) :: q2m     (nCols)
    REAL(KIND=r8) :: Qstar(nCols)
    REAL(KIND=r8) :: Tstar(nCols)
    REAL(KIND=r8) :: thetac(nCols)
    REAL(KIND=r8) :: thetam(nCols)
    INTEGER :: i
    DO i = 1, nCols
       thetac(i) = tsurf(i)
       thetam(i) = gt(i)*sigki (1)
       !
       !                     kg      J          m*K         J           W
       !Ho = rho*cp*(W'*T')=---- * -------- * ------ = -----------   =-----
       !                     m^3     kg*K        s       m^2 * s       m^2
       !
       !W'T' = -Ustar*Tstar
       !
       !                   Ho
       ! -Ustar*Tstar = --------
       !                 rho*cp
       !
       !            -Ho
       ! Tstar = ---------------
       !           rho*cp*Ustar
       !
       !
       !                      kg      J         m     kg       W
       !Eo = rho*hl*(W'*Q')= ---- * ------ * ------*------ = -----
       !                      m^3     kg        s     kg      m^2
       !
       !  W'Q' = -Ustar*Qstar
       !
       !                   Eo
       ! -Ustar*Qstar = --------
       !                 rho*hl
       !
       !            -Eo
       ! Qstar = ---------------
       !           rho*hl*Ustar
       !
       Tstar (i) = -sens(i)/(rho(i)*cp*Ustar(i))
       Qstar (i) = -evap(i)/(rho(i)*hl*Ustar(i))
    END DO

    CALL Reduced_temp(nCols  ,speedm  ,ustar ,tstar  ,znew    ,z0,thetac   ,thetam   , &
         ztn   ,theta2m,1)
    CALL Reduced_wind(nCols  ,speedm  ,ustar         ,znew    ,z0,thetac   ,thetam   , &
         ztn   ,velc2m,1 )
    CALL Reduced_q   (nCols  ,speedm  ,ustar ,qstar  ,znew    ,z0,thetac   ,thetam   , &
         qsurf   ,gq          ,ztn   ,q2m ,1  )

    DO i = 1, nCols
       theta2m(i)=theta2m(i)/sigki (1)
    END DO
  END SUBROUTINE CALC2MSEAICE




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!            tempc2m= temperatura a 2 m de altura a ser calculada          !!!!
!!!             velnew= vento a 10 m de altura a ser calculado               !!!!
!!!               qnew= umidade especifica a 2 m de altura a ser calculada   !!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !      a2= k**2/[ln(z/z0)]**2
  ! tc= temperatura do dossel (x,y,n4)
  ! tc= temperatura potencial do dossel
  ! qc= umidade do dossel (x,y,n4)
  ! qc= umidade do dossel (confirmar) = [qc(i,j,np)*cp/sfcpi]
  ! cp= calor especifico a pressao constante
  ! fh=
  !       g= aceleracao da gravidade
  !      nCols= n.o de pontos na direcao zonal
  !      n2= n.o de pontos na direcao meridional
  !      n3= n.o de niveis na vertical
  !      n4= fracao de terra/agua (pode ser > 2 qdo se utiliza o SIB)
  !       q= umidade especifica (x,y,z)
  !    qsup= umidade especifica na superficie (x,y,n4)
  !  Rib= numero de Richardson
  !   rtemp= variavel local para o calculo de tempc2m
  !  rtempw= variavel local para o calculo de tempc2m
  !   rmoist= variavel local para o calculo de qnew
  !   rmoistw= variavel local para o calculo de qnew
  !   rwind= variavel local para o calculo de velnew
  !   sfcpi= funcao de Exner na superficie (confirmar)
  !     spd= variavel local para o calculo de speed
  !  speed = magnitude da velocidade do vento [SQRT(u**2+v**2) : (i,j,k)]
  !   tm= temperatura potencial (x,y,z)
  !   tstar= escala de temperatura da camada superficial (calc. a partir dos
  !          fluxos) (x,y,n4)
  !   ustar= cisalhamento do vento (friction velocity) (x,y,n4)
  !    vonk= constante de von Karman (k)
  !      z0= comprimento de rugosidade = zrough
  !    zagl= altura do primeiro nivel do modelo
  !    znew= altura do primeiro nivel do modelo
  !  zrough= comprimento de rugosidade (x,y,n4)
  !     ztn= altura geometrica (m) do 1.o nivel sigma do modelo
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE Reduced_temp (nCols     ,speed  ,ustar , &
       tstar  ,znew  ,zrough,tc,tm , &
       zagl  ,tempc2m,np)

    IMPLICIT NONE
    !
    ! Declare calling parameters:
    !
    INTEGER         , INTENT(IN) :: nCols       ! nCols = n.o de pontos
    REAL(KIND=r8)   , INTENT(IN) :: speed(nCols) ! speed = magnitude da velocidade do
    !         vento [SQRT(u**2+v**2)
    REAL(KIND=r8)   , INTENT(IN) :: ustar(nCols) ! ustar = cisalhamento do vento
    !         (friction velocity) (x,y,n4)
    REAL(KIND=r8)   , INTENT(IN) :: tstar(nCols) ! tstar = escala de temperatura da
    !         camada superficial
    !         (calc. a partir dos fluxos)
    REAL(KIND=r8)   , INTENT(IN) :: znew         ! znew  = altura
    REAL(KIND=r8)   , INTENT(IN) :: zrough(nCols)! zrough= comprimento de rugosidade (x,y,n4)
    REAL(KIND=r8)   , INTENT(IN) :: tc(nCols)    ! tc    = temperatura potencial do dossel
    REAL(KIND=r8)   , INTENT(IN) :: tm(nCols)    ! tm    = temperatura potencial (x,y,z)
    REAL(KIND=r8)   , INTENT(IN) :: zagl(nCols)  ! zagl  = height of the first level of the model
    REAL(KIND=r8)   , INTENT(OUT):: tempc2m(nCols)
    INTEGER         , INTENT(IN) :: np

    !
    ! Declare local variables:
    !
    REAL(KIND=r8)    :: z0
    REAL(KIND=r8)    :: spd
    REAL(KIND=r8)    :: Rib
    REAL(KIND=r8)    :: a2
    REAL(KIND=r8)    :: rtemp
    REAL(KIND=r8), PARAMETER ::  g    = 9.80_r8 
    REAL(KIND=r8), PARAMETER ::  cp   = 1004.0_r8
    REAL(KIND=r8), PARAMETER ::  vonk = 0.40_r8
    INTEGER :: i

    REAL(KIND=r8)    :: b 
    REAL(KIND=r8)    :: d 
    REAL(KIND=r8)    :: cm
    REAL(KIND=r8)    :: cs
    REAL(KIND=r8)    :: ch
    REAL(KIND=r8)    :: fh
    
    !parameters to modify to temperature
    REAL(KIND=r8), PARAMETER ::  land_b  = 5.0_r8 , sea_b  = 5.0_r8
    REAL(KIND=r8), PARAMETER ::  land_d  = 5.0_r8 , sea_d  = 5.0_r8
    REAL(KIND=r8), PARAMETER ::  land_cm = 7.5_r8 , sea_cm = 7.5_r8
    REAL(KIND=r8), PARAMETER ::  land_cs = 5.0_r8 , sea_cs = 5.0_r8
    REAL(KIND=r8), PARAMETER ::  land_ch = 5.0_r8 , sea_ch = 5.0_r8
    REAL(KIND=r8), PARAMETER ::  land_fh = 40.0_r8 , sea_fh = 40.0_r8
    
    IF(np== 0)THEN 
         !for land
      b  = land_b  
      d  = land_d  
      cm = land_cm 
      cs = land_cs 
      ch = land_ch 
      fh = land_fh 
    ELSE
         !for water
      b  = sea_b 
      d  = sea_d 
      cm = sea_cm
      cs = sea_cs
      ch = sea_ch
      fh = sea_fh
    END IF  
    DO i=1,nCols
       rtemp=(tm(i) - tc(i))/2.0_r8

       !            --            --  (R/cp)
       !           |     pres(z)    |
       ! pi = cp * |----------------|
       !           |   pres(super)  |
       !            --            --
       !
       !          ( pi(i,j,1) + pi(i,j,2) )
       !sfcpi = ----------------------------
       !                     2
       !
       z0=zrough(i)
       !IF(np==1) z0=0.001_r8
       spd=MAX(speed(i),0.25_r8) ! Ver com saulo a utilizacao do 1.o nivel sigma
       !
       !               g*z*(tm(Z) - tm(S))
       ! Rib = ---------------------------------------
       !            0.5 * (tm(Z) + tm(S)) * Speed*Speed
       !
       !tm=temp 1 niv sigma
       !tc= temp superficie
       Rib  =g*zagl(i)*(tm(i) - tc(i))/(0.5_r8*( tm(i) + tc(i) )*spd**2.0_r8)
       !
       !       --           --
       !      |   vonk        |
       ! a2 = |---------------|
       !      |     |  Z  |   |
       !      |  LOG|---- |   |
       !      |     | Z0  |   |
       !       --           --
       a2 = (vonk / LOG( znew / z0 )) ** 2
       !
       IF (Rib > 0.0_r8) THEN
          !
          !                     1
          ! Fh = 1 - ----------------------------
          !             1 + 3*b*Rib*sqrt(1 + d*Rib)
          !
          !                     1
          ! Fm = 1 - ----------------------------
          !                            2*b*Rib
          !             1     +    -----------------
          !                         sqrt(1 + d*Rib)
          !tm(S))
          ! ustar*ustar = a*a*U*Fm
          !
          ! ustar*tstar = a*a*U*(Theta2(Z) - Theta1(s) ) * Fh
          !
          ! qstar*ustar = a*a*U * (q2(Z) - q1(s) ) * Fh
          !
          !       --          --  1/2
          !      |            |
          !  U = | u(z)^2 + v(z)^2 |
          !      |            |
          !       --          --
          !
          !
          ! ustar*tstar = a*a*U*(Theta2(Z) - Theta1(s) ) * Fh
          !
          !                               ustar*tstar
          ! (Theta2(Z) - Theta1(s) ) = ---------------------
          !                                a*a*U* Fh
          !
          !
          !                               ustar*tstar
          ! (Theta2(Z) - Theta1(s) ) = ---------------------
          !                                   a*a*U
          !                                a*a*U - ----------------------------
          !                                       1 + 3*b*Rib*sqrt(1 + d*Rib)
          !
          !                                                ustar*tstar
          ! (Theta2(Z) - Theta1(s) ) = --------------------------------------------------
          !                              a*a*U *(1 + 3*b*Rib*sqrt(1 + d*Rib))  -  a*a*U
          !                            --------------------------------------------------
          !                               1 + 3*b*Rib*sqrt(1 + d*Rib)
          !
          !
          !                              ustar*tstar * (1 + 3*b*Rib*sqrt(1 + d*Rib))
          ! (Theta2(Z) - Theta1(s) ) = --------------------------------------------------
          !                               a*a*U *(1 + 3*b*Rib*sqrt(1 + d*Rib))  - a*a*U
          !
          !
          !                                            ustar*tstar
          ! (Theta2(Z) - Theta1(s) ) = --------------------------------------------------* (1 + 3*b*Rib*sqrt(1 + d*Rib))
          !                               a*a*U * (1 + 3*b*Rib*sqrt(1 + d*Rib))  - a*a*U
          !
          !
          !                     1
          ! Fh = 1 - ----------------------------
          !             1 + 3*b*Rib*sqrt(1 + d*Rib)
          !
          !                              ustar*tstar                1
          ! (Theta2(Z) - Theta1(s) ) = ---------------- * ------------------------ * (1 + 3*b*Rib*sqrt(1 + d*Rib))
          !                               a*a*U            3*b*Rib*sqrt(1 + d*Rib)
          !
          rtemp = tc(i) - ((ustar(i)*tstar(i)*fh)/(a2*spd))*(1.0_r8/(3.0_r8*b*Rib*SQRT(1.0_r8 + d*Rib)))&
	                 *(1.0_r8 + 3.0_r8*b*Rib*SQRT(1.0_r8 + d*Rib))

          !                    --                          --
          !                   |  (ustar(i)*tstar(i,j)*fh)    |
          !   rtemp = tc(i) + |----------------------------- | * (1 + 3*b*Rib*SQRT(1 + d*Rib))
          !                   |          (a2*spd)            |
          !                    --                          --
          !

!          rtemp=tc(i) + ((ustar(i)*tstar(i)*fh)/(a2*spd))*(1.0_r8 + 3.0_r8*b*Rib*SQRT(1.0_r8 + d*Rib))
         
          
	  	  rtemp=MIN(MAX(rtemp, tc(i)),tm(i))  !limitar valor de tsurf e tsigma
	  
	  
       ELSE
          !
          !             3*b*Rib
          ! Fh = 1 - ----------------------------
          !             1 + 3*ch*b*a*a*sqrt(z/z0*ABS(rib))
          !
          !
          !             2*b*Rib
          ! Fm = 1 - ----------------------------
          !             1 + 2*cm*b*a*a*sqrt(z/z0*ABS(rib))
          !
          ! ustar*ustar = a*a*U*Fm
          !
          ! ustar*tstar = a*a*U*(Theta2(Z) - Theta1(s) ) * Fm
          !
          ! qstar*ustar = a*a*U * (q2(Z) - q1(s) ) * Fm
          !
          !       --             --  1/2
          !      |                 |
          !  U = | u(z)^2 + v(z)^2 |
          !      |                 |
          !       --             --
          !
          !
          ! ustar*tstar = a*a*U*(Theta2(Z) - Theta1(s) ) * Fm
          !
          !                               ustar*tstar
          ! (Theta2(Z) - Theta1(s) ) = ---------------------
          !                                a*a*U* Fm
          !
          !
          !                               ustar*tstar
          ! (Theta2(Z) - Theta1(s) ) = ---------------------
	  !                                       |           2*b*Rib                        |
          !                                a*a*U* | 1 - -------------------------------------|
	  !                                       |       1 + 2*cm*b*a*a*sqrt(z/z0*ABS(rib)) |
          !
          !
          
	  rtemp=tc(i) +  (ustar(i)*tstar(i)*fh) / ((a2*spd)* (1.0_r8 - ((2.0_r8*b*Rib)/(1.0_r8 + 2.0_r8*cm*b*a2 * &
	  SQRT(znew*ABS(Rib)/z0)))))
	  
	  !
	  
           
 
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          !                                      original do saulo

          !                                         (ustar(i)*tstar(i)*fh  )
          !                                         -------------------------- 
          !                                                (a2*spd) 
          !rtemp=tc(i) +             -----------------------------------------------------
          !                           (1.- 15.*richno/(1.+75.*a2 * sqrt(-znew*richno/z0)))
	  
	 
	  !                                        
          !                                        
          !               (ustar(i)*tstar(i)*fh  )                    1       
          !rtemp=tc(i) +  --------------------------*-----------------------------------------------------
          !              	 (a2*spd) 	      (1.- 15.0*richno/ (1.+75.*a2 * sqrt(-znew*richno/z0)) )
	  !
	  !
	  !
	  !               (ustar(i)*tstar(i)*fh  )                    1       
          !rtemp=tc(i) +  --------------------------*-----------------------------------------------------
	  !                                                      15.0*richno
          !              	 (a2*spd) 	      (1.- --------------------------------------  )
	  !                                                   (1.+75.*a2 * sqrt(-znew*richno/z0))

          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  
	  
	  
          !rtemp=tc(i) + ((ustar(i)*tstar(i)*fh  )/(a2*spd)) / &
          !     ((1.0_r8 - 2.0_r8*cm*Rib)/(1.0_r8 - 2.0_r8*cm*b*a2 * SQRT(-znew*Rib/z0)))
	  
	  
          rtemp=MAX(MIN(rtemp, tc(i)),tm(i))  
	  
       ENDIF
       tempc2m(i)=rtemp! temperatura potencial
    ENDDO
  END SUBROUTINE Reduced_temp

  !****************************************************

  SUBROUTINE Reduced_wind(nCols     ,speed  ,ustar , &
       znew   ,zrough ,tc,tm  , &
       zagl   ,velnew,np)

    IMPLICIT NONE
    !
    ! Declaring calling parameters
    !
    INTEGER, INTENT(IN)  :: nCols
    REAL(KIND=r8)   , INTENT(IN)  :: speed  (nCols)
    REAL(KIND=r8)   , INTENT(IN)  :: ustar  (nCols)
    REAL(KIND=r8)   , INTENT(IN)  :: znew
    REAL(KIND=r8)   , INTENT(IN)  :: zrough (nCols)
    REAL(KIND=r8)   , INTENT(IN)  :: tc(nCols)
    REAL(KIND=r8)   , INTENT(IN)  :: tm  (nCols)
    REAL(KIND=r8)   , INTENT(IN)  :: zagl   (nCols)! height of the first level of the model

    REAL(KIND=r8)   , INTENT(OUT) :: velnew (nCols)
    INTEGER         , INTENT(IN)  :: np

    !
    ! Declaring local variables
    !
    REAL(KIND=r8)    :: Rib
    REAL(KIND=r8)    :: rwind
    REAL(KIND=r8)    :: z0
    REAL(KIND=r8)    :: a2
    REAL(KIND=r8)    :: spd
    INTEGER :: i
    REAL(KIND=r8), PARAMETER ::  vonk = 0.40_r8
    REAL(KIND=r8), PARAMETER ::  g    = 9.80_r8
    REAL(KIND=r8), PARAMETER ::  cp   = 1004.0_r8
    
    REAL(KIND=r8) ::  b  
    REAL(KIND=r8) ::  d  
    REAL(KIND=r8) ::  cm 
    REAL(KIND=r8) ::  cs 
    REAL(KIND=r8) ::  ch 
    REAL(KIND=r8) ::  fh 
    
    !parameter to modify to wind    
    REAL(KIND=r8), PARAMETER ::  land_b  = 5.0_r8 , sea_b  = 5.0_r8
    REAL(KIND=r8), PARAMETER ::  land_d  = 5.0_r8 , sea_d  = 5.0_r8
    REAL(KIND=r8), PARAMETER ::  land_cm = 7.5_r8 , sea_cm = 7.5_r8
    REAL(KIND=r8), PARAMETER ::  land_cs = 5.0_r8 , sea_cs = 5.0_r8
    REAL(KIND=r8), PARAMETER ::  land_ch = 5.0_r8 , sea_ch = 5.0_r8
    REAL(KIND=r8), PARAMETER ::  land_fh = 1.0_r8 , sea_fh = 120.0_r8


    IF(np == 0)THEN 
    	 !for land
      b  = land_b  
      d  = land_d  
      cm = land_cm 
      cs = land_cs 
      ch = land_ch 
      fh = land_fh 
    ELSE
    	 !for water
      b  = sea_b 
      d  = sea_d 
      cm = sea_cm
      cs = sea_cs
      ch = sea_ch
      fh = sea_fh
    END IF  


    DO i=1,nCols
       z0=zrough(i)
       !IF(np==1) z0=0.001_r8
       spd = MAX(speed(i),0.25_r8)
       rwind=spd
       !
       !             g*z*(tm(Z) - tm(S))
       ! Rib = ---------------------------------------
       !      0.5 * (tm(Z) + tm(S)) * Speed*Speed
       !
       Rib = g*zagl(i)*( tm(i) - tc(i) )/( tm(i)*spd**2 )
       a2 = (vonk / LOG(znew / z0)) ** 2
       IF(Rib  > 0.0_r8) THEN
          !
          !                     1
          ! Fm = 1 - ----------------------------
          !                            2*b*Rib
          !             1     +    -----------------
          !                         sqrt(1 + d*Rib)
          !
          !                     1
          ! Fm = 1 - ----------------------------
          !              sqrt(1 + d*Rib) + 2*b*Rib
          !             ----------------------------
          !                   sqrt(1 + d*Rib)
          !
          !                 sqrt(1 + d*Rib)
          ! Fm = 1 - ----------------------------
          !              sqrt(1 + d*Rib) + 2*b*Rib
          !
          !               2*b*Rib
          ! Fm = ---------------------------
          !       sqrt(1 + d*Rib) + 2*b*Rib
          !
          !               1                    2*b*Rib
          ! Fm = ---------------------------* -----------
          !       sqrt(1 + d*Rib)               2*b*Rib
          !      ---------------- + 1
          !           2*b*Rib
          !
          !
          !  1    2*b*Rib
          ! --- =  1 + -----------------------
          !  Fm        sqrt(1 + d*Rib)
          !
          !

          ! ustar*ustar = a*a*U*U*Fm
          !
          !                ustar*ustar
          !     U*U = ------------------
          !                 a*a*Fm
          !

          !
          !                ustar*ustar
          !     U*U = ------------------
	  !                       |              1                     |
          !                 a*a * |1 - ----------------------------    |
          !                       !                     2*b*Rib        |
	  !                       !    1     +    -----------------    |
	  !                       !                   sqrt(1 + d*Rib)  |


          !                ustar*ustar
          !     U*U = ------------------
	  !                       |              1                     |
          !                 a*a * |1 - ----------------------------    |
          !                       !      sqrt(1 + d*Rib) +  2*b*Rib    |
	  !                       !    ----------------------------    |
	  !                       !          sqrt(1 + d*Rib)           |

          !                ustar*ustar
          !     U*U = ------------------
	  !                       |         sqrt(1 + d*Rib)            |
          !                 a*a * |1 - ----------------------------    |
          !                       !      sqrt(1 + d*Rib) +  2*b*Rib    |
          rwind=SQRT( (ustar(i)*ustar(i)*fh)/(a2 * (1.0_r8 - ((SQRT(1.0_r8 + d*Rib))/(SQRT(1.0_r8 + d*Rib) +2.0_r8*b*Rib ))) ))
 	   
        !  rwind=SQRT( ustar(i)**2/a2 * (1.0_r8 + 2.0_r8*b*Rib/SQRT(1.0_r8 + d*Rib)) )
       ELSE
          !             2*b*Rib
          ! Fm = 1 - ----------------------------
          !             1 + 2*cm*b*a*a*sqrt(z/z0*ABS(rib))
          !
          !                ustar*ustar
          !     U*U = ------------------
          !                 a*a*Fm
          !          


      
          !
          !                ustar*ustar
          !     U*U = ------------------
	  !                       | 	   2*b*Rib			      |
          !                 a*a * | 1 - --------------------------------------|
          !                       ! 	   1 + 2*cm*b*a*a*sqrt(z/z0*ABS(rib)) |

          rwind=SQRT( (ustar(i)*ustar(i)*fh)/(a2 * (1.0_r8 - (( 2.0_r8*b*Rib)/(1.0_r8 + 2.0_r8*cm*b*a2*sqrt(znew*ABS(rib)/z0))))))
	  
          !


!          rwind=SQRT( (ustar(i)**2/a2) / ((1.0_r8 - 2.0_r8*b*Rib)/(1.0_r8 + 2.0_r8*cm*b*a2  &
!               * SQRT(-znew*Rib/z0))))
       ENDIF
       rwind=MAX(MIN(rwind,speed(i)),0.0_r8)
       velnew(i)=rwind
    ENDDO
  END SUBROUTINE Reduced_wind

  !*************************************************************************************************************************

  SUBROUTINE Reduced_q (nCols     ,speed  ,ustar   , &
       qstar  ,znew   ,zrough ,tc,tm   , &
       qc   ,qm     ,zagl   ,qnew ,np  )

    IMPLICIT NONE

    !
    ! Declare calling parameters:
    !
    INTEGER, INTENT(IN) :: nCols
    REAL(KIND=r8)   , INTENT(IN) :: speed   (nCols)
    REAL(KIND=r8)   , INTENT(IN) :: ustar   (nCols)
    REAL(KIND=r8)   , INTENT(IN) :: znew
    REAL(KIND=r8)   , INTENT(IN) :: zrough  (nCols)
    REAL(KIND=r8)   , INTENT(IN) :: tc (nCols)
    REAL(KIND=r8)   , INTENT(IN) :: tm   (nCols)
    REAL(KIND=r8)   , INTENT(IN) :: qc    (nCols)
    REAL(KIND=r8)   , INTENT(IN) :: qm(nCols)
    REAL(KIND=r8)   , INTENT(IN) :: zagl    (nCols) ! height of the first level of the model
    REAL(KIND=r8)   , INTENT(IN) :: qstar   (nCols)
    REAL(KIND=r8)   , INTENT(OUT) :: qnew   (nCols)
    INTEGER         , INTENT(IN):: np

    !
    ! Declare local variables:
    !
    REAL(KIND=r8)    :: z0
    REAL(KIND=r8)    :: spd
    REAL(KIND=r8)    :: Rib
    REAL(KIND=r8)    :: a2
    REAL(KIND=r8)    :: rmoist
    INTEGER :: i
    REAL(KIND=r8), PARAMETER ::  vonk = 0.40_r8
    REAL(KIND=r8), PARAMETER ::  g    = 9.80_r8
    REAL(KIND=r8), PARAMETER ::  cp   = 1004.0_r8
    
    REAL(KIND=r8) :: b  
    REAL(KIND=r8) :: d  
    REAL(KIND=r8) :: cm 
    REAL(KIND=r8) :: cs 
    REAL(KIND=r8) :: ch 
    REAL(KIND=r8) :: fh
    
    !parameters to modify to humidity
    REAL(KIND=r8), PARAMETER ::  land_b  = 5.0_r8 , sea_b  = 5.0_r8
    REAL(KIND=r8), PARAMETER ::  land_d  = 5.0_r8 , sea_d  = 5.0_r8
    REAL(KIND=r8), PARAMETER ::  land_cm = 7.5_r8 , sea_cm = 7.5_r8
    REAL(KIND=r8), PARAMETER ::  land_cs = 5.0_r8 , sea_cs = 5.0_r8
    REAL(KIND=r8), PARAMETER ::  land_ch = 5.0_r8 , sea_ch = 5.0_r8
    REAL(KIND=r8), PARAMETER ::  land_fh = 35.0_r8 , sea_fh =35.0_r8

    IF(np == 0)THEN 
         !for land
      b  = land_b  
      d  = land_d  
      cm = land_cm 
      cs = land_cs 
      ch = land_ch 
      fh = land_fh 
    ELSE
         !for water
      b  = sea_b 
      d  = sea_d 
      cm = sea_cm
      cs = sea_cs
      ch = sea_ch
      fh = sea_fh
    END IF  
    DO i=1,nCols
       rmoist= (qc(i)+qm(i))/2.0_r8
       !            --            --  (R/cp)
       !           |     pres(z)    |
       ! pi = cp * |----------------|
       !           |   pres(super)  |
       !            --            --
       !
       !          ( pi(i,j,1) + pi(i,j,2) )
       !sfcpi = ----------------------------
       !                     2
       !
       !sfcpi=.5*(pi(i,j,1)+pi(i,j,2))      ! Exner function
       z0=zrough(i)
       !IF(np==1) z0=0.001_r8
       spd=MAX(speed(i),0.25_r8) ! Ver com saulo a utilizacao do 1.o nivel sigma
       !
       !               g*z*(tm(Z) - tm(S))
       ! Rib = ---------------------------------------
       !            0.5 * (tm(Z) + tm(S)) * Speed*Speed
       !
       Rib  =g*zagl(i)*(tm(i) - tc(i))/(0.5_r8*( tm(i) + tc(i) )*spd**2)
       !
       !       --           --
       !      |   vonk        |
       ! a2 = |---------------|
       !      |     |  Z  |   |
       !      |  LOG|---- |   |
       !      |     | Z0  |   |
       !       --           --
       a2 = (vonk / LOG( znew / z0 )) ** 2
       !
       IF (Rib > 0.0_r8) THEN
          !
          !                     1
          ! Fh = 1 - ----------------------------
          !             1 + 3*b*Rib*sqrt(1 + d*Rib)
          !
          !                     1
          ! Fm = 1 - ----------------------------
          !                            2*b*Rib
          !             1     +    -----------------
          !                         sqrt(1 + d*Rib)
          !
          ! b  = 5
          ! d  = 5
          ! cm = 7.5
          ! cs = 5
          ! ch = 5
          !
          ! ustar*ustar = a*U*U*Fm
          !
          ! ustar*tstar = a*a*U*(Theta2(Z) - Theta1(s) ) * Fh
          !
          ! qstar*ustar = a*a*U * (q2(Z) - q1(s) ) * Fh
          !
          !       --          --  1/2
          !      |            |
          !  U = | u(z)^2 + v(z)^2 |
          !      |            |
          !       --          --
          !
          !
          ! ustar*qstar = a*a*U*(Q2(Z) - Q1(s) ) * Fh
          !
          !                       ustar*qstar
          ! (Q2(Z) - Q1(s) ) = ---------------------
          !                        a*a*U* Fh
          !
          !
          !                       ustar*qstar
          ! (Q2(Z) - Q1(s) ) = ---------------------
          !                                     a*a*U
          !                    a*a*U - ----------------------------
          !                          1 + 3*b*Rib*sqrt(1 + d*Rib)
          !
          !                                         ustar*qstar
          ! (Q2(Z) - Q1(s) ) = ---------------------------------------------------------------------------
          !                     a*a*U *(1 + 3*b*Rib*sqrt(1 + d*Rib))   - a*a*U
          !                     -------------------------------------------------------------------------
          !                           1 + 3*b*Rib*sqrt(1 + d*Rib)
          !
          !
          !                      ustar*qstar * (1 + 3*b*Rib*sqrt(1 + d*Rib))
          ! (Q2(Z) - Q1(s) ) = --------------------------------------------------
          !                       a*a*U *(1 + 3*b*Rib*sqrt(1 + d*Rib))  - a*a*U
          !
          !
          !                                   ustar*qstar
          ! (Q2(Z) - Q1(s) ) = --------------------------------------------------* (1 + 3*b*Rib*sqrt(1 + d*Rib))
          !                       a*a*U * (1 + 3*b*Rib*sqrt(1 + d*Rib))  - a*a*U
          !
          !
          !                     1
          ! Fh = 1 - ----------------------------
          !             1 + 3*b*Rib*sqrt(1 + d*Rib)
          !
          !                      ustar*qstar                1
          ! (Q2(Z) - Q1(s) ) = ---------------- * ------------------------ * (1 + 3*b*Rib*sqrt(1 + d*Rib))
          !                        a*a*U            3*b*Rib*sqrt(1 + d*Rib)
          !
          !                       --                          --
          !                      |  (ustar(i)*qstar(i,j)*fh)    |
          !   rmoist = qc(i) + |----------------------------- | * (1 + 3*b*Rib*SQRT(1 + d*Rib))
          !                      |          (a2*spd)            |
          !                       --                          --
          !
	  rmoist=qc(i) - ((ustar(i)*qstar(i)*fh)/(a2*spd))*(1.0_r8/(3.0_r8*b*Rib*SQRT(1.0_r8 + d*Rib)))&
	  *(1.0_r8 + 3.0_r8*b*Rib*SQRT(1.0_r8 + d*Rib))
!          rmoist=qc(i) + (ustar(i)*qstar(i)*fh)/(a2*spd)*(1.0_r8 + 2.0_r8*cm*Rib*SQRT(1.0_r8 + d*Rib))
          
	  
	  rmoist=MIN(MAX(rmoist, qc(i)),qm(i))  
	  
	  
       ELSE
          !
          !             3*b*Rib
          ! Fh = 1 - ----------------------------
          !             1 + 3*ch*b*a*a*sqrt(z/z0*ABS(rib))
          !
          !
          !             2*b*Rib
          ! Fm = 1 - ----------------------------
          !             1 + 2*cm*b*a*a*sqrt(z/z0*ABS(rib))
          !
          !
          !
          ! ustar*qstar = a*a*U*(Q2(Z) - Q1(s) ) * Fm
          !
          !
          !       --             --  1/2
          !      |                 |
          !  U = | u(z)^2 + v(z)^2 |
          !      |                 |
          !       --             --
          !
          !
          ! ustar*qstar = a*a*U*(Q2(Z) - Q1(s) ) * Fm
          !
          !                         ustar*qstar
          ! (Q2(Z) - Q1(s) ) = ---------------------
          !                          a*a*U* Fm
          !
          !
          !                        ustar*qstar
          ! (Q2(Z) - Q1(s) ) = ---------------------
	  !                            |           2*b*Rib                   |
          !                     a*a*U* | 1 - --------------------------------|
	  !                            |       1 + 2*cm*b*a*a*sqrt(z/z0*rib) |
          !
          !         
          !                       --                       --
          !                      |  (ustar(i)*qstar(i,j))    |  /         2*b*Rib
          !   rmoist = qc(i) +   |-------------------------- | /   1 - ----------------------------
          !                      |          (a2*spd)         |/           1 + 2*cm*b*a*a*sqrt(z/z0*rib)
          !                       --                       --

          rmoist = qc(i) + ( (ustar(i)*qstar(i)*fh  )/(a2*spd)) / &
                             (1.0_r8 - ( (2.0_r8*b*Rib)/(1.0_r8 + 2.0_r8*cm*b*a2 * SQRT(znew*ABS(Rib)/z0))))

 !         rmoist = qc(i) + ( (ustar(i)*qstar(i)*fh  )/(a2*spd)) / &
 !              ((1.0_r8 - 2.0_r8*b*Rib)/(1.0_r8 - 2.0_r8*cm*b*a2 * SQRT(-znew*Rib/z0)))
         
	  rmoist=MAX(MIN(rmoist, qc(i)),qm(i))  
	 
       ENDIF
       qnew(i)=rmoist
    ENDDO
  END SUBROUTINE Reduced_q





END MODULE Surface
