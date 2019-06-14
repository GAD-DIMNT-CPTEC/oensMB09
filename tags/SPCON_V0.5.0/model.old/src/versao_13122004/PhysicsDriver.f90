!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE PhysicsDriver

  USE Constants, ONLY: &
       ityp, imon, icg, iwv, idp, ibd

  USE Radiation, ONLY: &
       spmrad,         &
       radtim,         &
       rqvirt

  USE Sizes, ONLY:       &
       kMax        , &
       jMax        , &
       ibMax        , &
       ibMaxPerJB,  &
       sl         , &
       si         , &
       del        , &
       cl


  USE Surface, ONLY: &
       CopySurfaceData, &
       fysiks,       &
       seasfc,       &
       sextrp,       &
       radalb,       &
       wheat ,       &
       x0x   ,       &
       xd    ,       &
       xdc   ,       &
       xbc

  USE Diagnostics, ONLY: &
       updia, lgaus, gaus , nucf , iavrq, &
       itavl, nuavl, lvavl, dodia, inavl, &
       ixavl, iclcd, incf , ixcf , kravl, jrcf, krcf, &
       nDiag_sheatf, nDiag_lheatf, nDiag_ustres, nDiag_vstres, nDiag_vdheat, &
       nDiag_vduzon, nDiag_vdvmer, nDiag_vdmois, nDiag_tmtsfc, nDiag_cloudc, &
       nDiag_lwheat, nDiag_swheat, nDiag_lwdbot, nDiag_lwubot, nDiag_lwutop, &
       nDiag_swdtop, nDiag_swdbot, nDiag_swabea, nDiag_swabgr, nDiag_swutop, &
       nDiag_swubot, nDiag_lwnetb, nDiag_intlos, nDiag_runoff, nDiag_lwdbtc, &
       nDiag_lwutpc, nDiag_swdbtc, nDiag_swaeac, nDiag_swabgc, nDiag_swutpc, &
       nDiag_swubtc, nDiag_lwnbtc, nDiag_tcairs, nDiag_ecairs, nDiag_bsolht, &
       nDiag_vdtclc, nDiag_invcld, nDiag_ssatcl, nDiag_cnvcld, nDiag_shcvcl, &
       nDiag_clliwp, nDiag_lwcemi, nDiag_sclopd, nDiag_mofres, nDiag_casrrs, &
       nDiag_cascrs, nDiag_casgrs, nDiag_gcovrs, nDiag_bssfrs, nDiag_txgwds, &
       nDiag_tygwds, nDiag_gwduzc, nDiag_gwdvmc 

  USE Constants, ONLY :     &
       tov,   &  ! intent(in)
       tdelt, &  ! intent(in)
       tsfc0, &  ! intent(in)
       tstrat,&  ! intent(in)
       tdampr,&  ! intent(in)
       tdampf,&  ! intent(in)
       h0,    &  ! intent(in)
       rlaps, &  ! intent(in)
       pie      ,        &
       pai12    ,        &
       cp       ,        &
       hl       ,        &
       gasr     ,        &
       rk       ,        &
       grav     ,        &
       solcon   ,        &
       stefan   ,        &
       tf       ,        &
       epsfac   ,        &
       tice     ,        &
       oceald   ,        &
       icealn   ,        &
       icealv

  USE PlanBoundLayer, ONLY: &
       ympbl0,              &
       ympbl1,              &
       InitGwdd,            &
       Gwdd

  USE GridHistory, ONLY:       &
       IsGridHistoryOn, StoreGridHistory, StoreMaskedGridHistory, dogrh, &
       nGHis_casrrs, nGHis_mofres, nGHis_vdtclc, nGHis_lwheat, nGHis_swheat, &
       nGHis_lwutop, nGHis_lwdbot, nGHis_coszen, nGHis_swdbot, nGHis_swdbvb, &
       nGHis_swdbvd, nGHis_swdbnb, nGHis_swdbnd, nGHis_vibalb, nGHis_vidalb, &
       nGHis_nibalb, nGHis_nidalb, nGHis_hcseai, nGHis_hsseai, nGHis_cloudc, &
       nGHis_dragcf, nGHis_nrdcan, nGHis_nrdgsc, nGHis_cascrs, nGHis_casgrs, &
       nGHis_canres, nGHis_gcovrs, nGHis_bssfrs, nGHis_ecairs, nGHis_tcairs, &
       nGHis_shfcan, nGHis_shfgnd, nGHis_tracan, nGHis_tragcv, nGHis_inlocp, &
       nGHis_inlogc, nGHis_bsevap, nGHis_canhea, nGHis_gcheat, nGHis_runoff, &
       nGHis_vdheat, nGHis_vduzon, nGHis_vdvmer, nGHis_vdmois, nGHis_swutop, &
       nGHis_lwubot, nGHis_ustres, nGHis_vstres, nGHis_sheatf, nGHis_lheatf

  USE Options, ONLY : &
       varcut,&
       dogwd ,&
       mxrdcc,&
       lcnvl ,&
       lthncl,&
       cdhl  ,&
       istrt ,&
       first ,&
       co2val,&
       delt  ,&
       filta ,&
       nfin0 ,&
       nfin1 ,&
       initlz,&
       nfcnv0,&
       nfcldr,&
       irad  ,&
       iccon ,&
       swint ,&
       trint ,&
       yrl   ,&
       monl  ,&
       dtc3x ,&
       epsflt,&
       intg  ,&
       maxtid,&       
       dt    ,&
       idate ,&
       idatec,&
       kt    ,&
       ktm   ,&
       jdt   ,&
       Model1D

  USE Init, ONLY:  &
       nls   ,&
       nlcs


  USE FieldsPhysics, ONLY: &
       anirb, anird, avisb, avisd, botlv, capac0, capacm, cldsav, clr, convb, &
       convbs, convc, convcs, convt, convts, dlwbot, dlwclr, dswtop, evap, geshem, &
       gl0, gtsea, htr, htrc, imask, ppci, ppli, prcc, prcp1, prcp2, &
       prcp3, prcpt, prct, rnirb, rnirbc, rnird, rnirdc, rs, rsclr, rvisb, &
       rvisbc, rvisd, rvisdc, sens, sheleg, sigki, ssib, tc0, tcm, td0, &
       tdm, tg0, tgm, toplv, tseam, ultclr, ulwtop, ustr, uswtpc, var, &
       vstr, w0, wm, xnirb, xnirbc, xnird, xnirdc, xswtop, xvisb, xvisbc, &
       xvisd, xvisdc, ynirb, ynirbc, ynird, ynirdc, yswtop, yvisb, yvisbc, yvisd, &
       yvisdc, zorl

  USE Convection, ONLY:  &
       gwater2

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: DryPhysics
  PUBLIC :: HumidPhysics
  PUBLIC :: InitSimpPhys
  PUBLIC :: SimpPhys
  REAL, ALLOCATABLE :: teq(:,:,:)
  REAL, ALLOCATABLE :: tauri(:)
  REAL, ALLOCATABLE :: alfa(:)

CONTAINS 

  SUBROUTINE DryPhysics & 
       (gt    ,gq    ,gu    ,gv    ,gps   ,gyu   ,gyv   ,gtd   , &
       gqd   ,colrad,ifday ,tod   ,gtt   ,gqq   ,omg   ,latco , &
       lonrad,glnpm ,cos2d ,intcosz   )
    REAL,    INTENT(inout) :: gt    (ibMax,kMax)
    REAL,    INTENT(inout) :: gq    (ibMax,kMax)
    REAL,    INTENT(in   ) :: gu    (ibMax,kMax)
    REAL,    INTENT(in   ) :: gv    (ibMax,kMax)
    REAL,    INTENT(in   ) :: gps   (ibMax)      
    REAL,    INTENT(inout) :: gyu   (ibMax,kMax)
    REAL,    INTENT(inout) :: gyv   (ibMax,kMax)
    REAL,    INTENT(inout) :: gtd   (ibMax,kMax)
    REAL,    INTENT(inout) :: gqd   (ibMax,kMax)
    REAL,    INTENT(in   ) :: colrad(ibMax)
    REAL,    INTENT(in   ) :: lonrad(ibMax)
    INTEGER, INTENT(in   ) :: ifday
    REAL,    INTENT(in   ) :: tod   
    REAL,    INTENT(in   ) :: gtt   (ibMax,kMax)
    REAL,    INTENT(in   ) :: gqq   (ibMax,kMax)
    REAL,    INTENT(in   ) :: omg   (ibMax,kMax)
    INTEGER, INTENT(in   ) :: latco
    REAL,    INTENT(inout) :: glnpm (ibMax)
    REAL   , INTENT(IN   ) :: cos2d (ibMax)    
    LOGICAL, INTENT(IN   ) :: intcosz
    REAL                   :: ps    (ibMax)
    INTEGER                :: ibLim

    ps=glnpm
    ibLim=ibMaxPerJB(latco)
    CALL physcs & 
         (gt    (1:ibLim,:      ), &
         gq    (1:ibLim,:      ), &
         gu    (1:ibLim,:      ), &
         gv    (1:ibLim,:      ), &
         gps   (1:ibLim        ), &
         tc0   (1:ibLim,  latco), &
         tg0   (1:ibLim,  latco), &
         td0   (1:ibLim,  latco), &
         w0    (1:ibLim,:,latco), &
         capac0(1:ibLim,:,latco), &
         tcm   (1:ibLim,  latco), &
         tgm   (1:ibLim,  latco), &
         tdm   (1:ibLim,  latco), &
         wm    (1:ibLim,:,latco), &
         capacm(1:ibLim,:,latco), &
         ppli  (1:ibLim,  latco), &
         ppci  (1:ibLim,  latco), &
         gyu   (1:ibLim,:      ), &
         gyv   (1:ibLim,:      ), &
         gtd   (1:ibLim,:      ), &
         gqd   (1:ibLim,:      ), &
         htr   (1:ibLim,:,latco), &
         clr   (1:ibLim,:,latco), &
         imask (1:ibLim,  latco), &
         rvisb (1:ibLim,  latco), &
         rvisd (1:ibLim,  latco), &
         rnirb (1:ibLim,  latco), &
         rnird (1:ibLim,  latco), &
         dlwbot(1:ibLim,  latco), &
         gtsea (1:ibLim,  latco), &
         colrad(1:ibLim        ), &
         sl                    , &
         si    (1:kMax)        , &
         del                   , &
         ifday                 , &
         tod                   , &
         avisb (1:ibLim,  latco), &
         avisd (1:ibLim,  latco), &
         anirb (1:ibLim,  latco), &
         anird (1:ibLim,  latco), &
         dswtop(1:ibLim,  latco), &
         rs    (1:ibLim,  latco), &
         ulwtop(1:ibLim,  latco), &
         gl0   (1:ibLim,  latco), &
         zorl  (1:ibLim,  latco), &
         gtt   (1:ibLim,:      ), &
         gqq   (1:ibLim,:      ), &
         sheleg(1:ibLim,  latco), &
         tseam (1:ibLim,  latco), &
         omg   (1:ibLim,:      ), &
         htrc  (1:ibLim,:,latco), &
         rvisbc(1:ibLim,  latco), &
         rvisdc(1:ibLim,  latco), & 
         rnirbc(1:ibLim,  latco), &
         rnirdc(1:ibLim,  latco), &
         dlwclr(1:ibLim,  latco), &
         uswtpc(1:ibLim,  latco), &
         rsclr (1:ibLim,  latco), &
         ultclr(1:ibLim,  latco), &
         convts(1:ibLim,  latco), &
         convcs(1:ibLim,  latco), &
         convbs(1:ibLim,  latco), &
         convc (1:ibLim,  latco), &
         convt (1:ibLim,  latco), &
         convb (1:ibLim,  latco), &
         ustr  (1:ibLim,  latco), &
         vstr  (1:ibLim,  latco), &
         latco                 , &
         yvisb (1:ibLim,  latco), &
         yvisd (1:ibLim,  latco), &
         ynirb (1:ibLim,  latco), &
         ynird (1:ibLim,  latco), &
         yswtop(1:ibLim,  latco), &
         yvisbc(1:ibLim,  latco), &
         yvisdc(1:ibLim,  latco), &
         ynirbc(1:ibLim,  latco), &
         ynirdc(1:ibLim,  latco), &
         cldsav(1:ibLim,  latco), &
         ssib  (1:ibLim,  latco), &
         ibMaxPerJB(latco)      , &
         kMax                   , &
         sigki                  , &
         xvisb (1:ibLim, latco ), &
         xvisd (1:ibLim, latco ), &
         xnirb (1:ibLim, latco ), &
         xnird (1:ibLim, latco ), &
         xswtop(1:ibLim, latco ), &
         xvisbc(1:ibLim, latco ), &
         xvisdc(1:ibLim, latco ), &
         xnirbc(1:ibLim, latco ), &
         xnirdc(1:ibLim, latco ), &
         lonrad(1:ibLim        ), &
         ps    (1:ibLim        ), & 
         var   (1:ibLim,latco  ), & 
         sens  (1:ibLim,latco  ), &
         evap  (1:ibLim,latco  ), &
         cos2d (1:ibLim        ), &
         intcosz )
  END SUBROUTINE DryPhysics




  SUBROUTINE HumidPhysics(latco, rqn, ftn, fqn, fpn, gu, gv, omg) 
    INTEGER, INTENT(IN   ) :: latco
    REAL,    INTENT(IN   ) :: rqn  (ibMax,kMax)
    REAL,    INTENT(INOUT) :: ftn  (ibMax,kMax)
    REAL,    INTENT(INOUT) :: fqn  (ibMax,kMax)
    REAL,    INTENT(IN   ) :: fpn  (ibMax)
    REAL,    INTENT(IN   ) :: gu   (ibMax,kMax)
    REAL,    INTENT(IN   ) :: gv   (ibMax,kMax)
    REAL,    INTENT(IN   ) :: omg  (ibMax,kMax)

    INTEGER                :: ibLim
    ibLim=ibMaxPerJB(latco)    
    CALL gwater2( &
         ibMaxPerJB(latco)    , &
         kMax                 , &
         geshem(1:ibLim,latco), &
         ppli  (1:ibLim,latco), &
         ppci  (1:ibLim,latco), &
         convc (1:ibLim,latco), &
         convt (1:ibLim,latco), & 
         convb (1:ibLim,latco), &
         prcp1 (1:ibLim,latco), &
         prcp2 (1:ibLim,latco), &
         prcp3 (1:ibLim,latco), &
         prcpt (1:ibLim,latco), &
         toplv (1:ibLim,latco), &
         botlv (1:ibLim,latco), &
         convts(1:ibLim,latco), &
         convcs(1:ibLim,latco), &
         convbs(1:ibLim,latco), &
         prct  (1:ibLim,latco), &
         prcc  (1:ibLim,latco), &
         si                   , &
         sl                   , &
         del                  , &
         latco                , &
         rqn  (1:ibLim,:     ), &
         ftn  (1:ibLim,:     ), &
         fqn  (1:ibLim,:     ), &
         fpn  (1:ibLim       ), &
         sens (1:ibLim,latco ), &
         evap (1:ibLim,latco ), &
         gu   (1:ibLim,:     ), &
         gv   (1:ibLim,:     ), &
         omg  (1:ibLim,:     ) )
  END SUBROUTINE HumidPhysics




  SUBROUTINE physcs &
       (gt   ,gq    ,gu    ,gv    ,gps   ,tc0   ,tg0   ,td0   ,w0    ,capac0, &
       tcm   ,tgm   ,tdm   ,wm    ,capacm,ppli  ,ppci  ,gyu   ,gyv   ,gtd   , &
       gqd   ,htr   ,clr   ,imask ,rvisb ,rvisd ,rnirb ,rnird ,dlwbot,tsea  , &
       colrad,sig   ,sigml ,delsig,ifday ,tod   ,avisb ,avisd ,anirb ,anird , &
       dswtop,rs    ,ulwtop,gl0   ,zorl  ,gtt   ,gqq   ,sheleg,tseam ,omg   , &
       htrc  ,rvisbc,rvisdc,rnirbc,rnirdc,dlwclr,uswtpc,rsclr ,ultclr,convts, &
       convcs,convbs,convc ,convt ,convb ,ustr  ,vstr  ,latco ,yvisb ,yvisd , &
       ynirb ,ynird ,yswtop,yvisbc,yvisdc,ynirbc,ynirdc,cldsav,ssib  ,ncols , &
       kmax  ,sigki ,xvisb ,xvisd ,xnirb ,xnird ,xswtop,xvisbc,xvisdc,xnirbc, &
       xnirdc,lonrad,ps    ,var   ,sens  ,evap  ,cos2d ,intcosz)

    !
    !
    ! physcs :main subroutine for turbulence closure
    !         hashvadahn radiation coupled 3-d model
    !         p.sellers  sib
    !         gps is in mb
    !==========================================================================
    ! ncols.....Number of grid points on a gaussian latitude circle  
    ! jmax......Number of gaussian latitudes  
    ! kmax......Number of sigma levels  
    ! nls..... .Number of layers in the stratosphere.   
    ! nlcs......nlcs =   30  
    ! maxtid..../include/T062L28/restim.inc:      
    !           constant integer, parameter maxtid=131760  
    ! ityp......Numero das classes de solo 13 
    ! imon......Max. number of month at year (12)
    ! icg.......Parameter of the vegetation  (icg=1 top e icg=2 bottom )
    ! iwv.......Compriment de onda iwv=1=visivel, iwv=2=infravermelho
    !           proximo, iwv=3 infravermelho termal
    ! idp.......Parameter to the layers of soils idp=1->3
    ! ibd.......Condiction of vegetation ibd=1 green / ibd=2 
    ! gt........Temperature    
    ! gq........Specific humidity    
    ! gu........(zonal      velocity)*sin(colat)    
    ! gv........(meridional velocity)*sin(colat)    
    ! gps.......Surface pressure in mb   
    ! tc0.......Temperatura da copa "dossel"(K)   modificada   
    ! tg0.......Temperatura da superficie do solo  (K)   modificada   
    ! td0.......Temperatura do solo profundo (K)   modificada   
    ! w0(id)....Grau de saturacao de umidade do solo id=1 na camada superficial
    ! w0(id)....Grau de saturacao de umidade do solo id=2 na camada de raizes
    ! w0(id)....Grau de saturacao de umidade do solo id=3 na camada de drenagem  
    ! capac0(iv).Agua interceptada iv=1 no dossel "water store capacity
    !             of leaves"(m)  modificada
    ! capac0(iv).Agua interceptada iv=2 na cobertura do solo (m)   modificada
    ! tcm........Temperatura da copa "dossel"(K)  
    ! tgm........Temperatura da superficie do solo  (K)   
    ! tdm........Temperatura do solo profundo (K)     
    ! wm    
    ! capacm.....Agua interceptada iv=2 na cobertura do solo (m)   
    ! ppli.......Precipitation rate ( large scale )       (mm/s)  
    ! ppci.......Precipitation rate ( cumulus )           (mm/s)  
    ! gyu........-(dv/dt) negative of tendency of v*cos(latitude)   
    ! gyv........(du/dt) tendency of zonal wind * cos(latitude)   
    ! gtd   
    ! gqd........Specific humidity   
    ! htr........Heating rate due to shrt wave radiation in deg/sec   
    ! clr........Cooling rate due to long wave radiation in deg/sec   
    ! imask......mascara continetal
    ! rvisb......visible beam cloudy skies (refer to downward surface fluxes) 
    ! rvisd......visible diffuse cloudy skies (refer to downward surface fluxes) 
    ! rnirb......near-ir beam cloudy skies (refer to downward surface fluxes) 
    ! rnird......near-ir diffuse cloudy skies (refer to downward surface fluxes) 
    ! dlwbot.....downward longwave radiation at the bottom in w/m**2
    ! tsea.......effective surface radiative temperature ( tgeff )  
    ! colrad.....colatitude  colrad=0-3.14 from np to sp in radians
    ! sig........sigma coordinate at middle of layer   
    ! sigml......sigma coordinate at bottom of layer 
    ! delsig      k=2  ****gu,gv,gt,gq,gyu,gyv,gtd,gqd,sig*** } delsig(2)
    !             k=3/2----sigml,ric,rf,km,kh,b,l -----------
    !             k=1  ****gu,gv,gt,gq,gyu,gyv,gtd,gqd,sig*** } delsig(1)
    !             k=1/2----sigml ----------------------------
    !
    ! istrt.......istrt = jdt =time step in getdia
    ! ifday.......model forecast day
    ! tod.........model forecast time of day in seconds   
    ! avisb.......visible beam surface albedo 
    ! avisd.......visible diffuse surface albedo 
    ! anirb.......near-ir beam surface albedo 
    ! anird.......near-ir diffuse surface albedo 
    ! uswtop......shortwave upward at top 
    ! dswtop......swinc....solar input at top of atmosphere
    ! rs..........net surface ir radiation in w/m**2     
    ! ulwtop......long wave flux at top of atmosphere in w/m**2
    ! alon........define constant alon=0.0 at subroutine gfidi.f90: 
    ! dt........time interval,usually =delt,but changes
    !             in nlnmi (dt=1.) and at dead start(delt/4,delt/2) 
    ! intg........intg =2  time integration of surface physical variable 
    !                      is done by leap-frog implicit scheme. this 
    !                      conseves enegy and h2o.
    !             intg =1  time integration of surface physical variable
    !                      is done by backward implicit scheme.
    ! gl0.........maximum mixing length l0 in blackerdar's formula
    !             l=k0*z/(1+k0*z/l0)  
    ! zorl........zorl (i)= 100.0 *zgrav*speedm(i)*rhi(i)
    !             zgrav =0.032 /grav and rhi   (i)=ct(i)*ustar(i), where
    !             ct is heat transfer coefficients.
    !             ustar is surface friction velocity  (m/s)
    !             speedm(i)=SQRT(gu(i)**2+gv(i)**2)*sincli, where
    !             sincli=1.0 /sinclt
    !
    ! gtt.........gtt =  gtmp(imx,kmax) input  : temperature (fourier).
    !                                   output : "s" as given by eq. (19)
    !                                            in noaa tech report nws 30  
    ! gqq.........gqq = gq(imx,kmax)     input : specific humidity (fourier).
    !                                   output : tendency of specific humidity
    !                                            without sources and sinks
    !                                            (fourier).   
    ! sheleg......snow amount in mm (equivalent water depth)
    ! tseam.......tseam = gtsea (ncols,jmax)  input(gsstcd) lt.0=sea surface temp.
    !                                                      gt.0=ground temp. 
    ! omg.........omg   =  vertical velocity  (cb/sec)   
    ! rvisbc......Visible beam clear sky (Refer to downward surface 
    !             shortwave fluxe)
    ! rvisdc......Visible diffuse clear sky (Refer to downward surface 
    !             shortwave fluxe)
    ! rnirbc......Near-IR beam clear skies (Refer to downward surface 
    !             shortwave fluxe)
    ! rnirdc......Near-IR diffuse clear skies (Refer to downward surface 
    !             shortwave fluxe)
    ! htrc........Heating rate due to shortwave (clear) (K/s)  
    ! dlwclr......Downward longwave (clear) At the bottom (W/m2)
    ! uswtpc......shortwave upward at top (clear) 
    ! rsclr.......net longwave at bottom (clear)                      
    ! ultclr......longwave upward at top (clear)        
    ! pie.........Constant pi=3.1415926e0   
    ! stefan......Stefan Stefan Boltzman constant    
    ! cpair.......Specific heat of air           (j/kg/k)  
    ! hl..........heat of evaporation of water     (j/kg)    
    ! grav........grav   gravity constant        (m/s**2)    
    ! snomel......Calor latente de fusao is expressed in (j m-1)
    ! tf..........Temperatura de congelamento (K)=273.16e0   
    ! clai........heat capacity of foliage          
    ! cw..........liquid water heat capacity     (j/m**3)  
    ! gasr........gas constant of dry air        (j/kg/k)  
    ! epsfac......Constante 0.622 Razao entre as massas 
    !             moleculares do vapor e do ar seco   
    ! athird......Constant athird =                 1.0e0/3.0e0
    ! tice........tice=271.16 ice temperature ice
    ! oceald......oceald = 0.0419e0
    ! z0ice ......Roughness length of ice   
    ! icealn......near-ir beam surface albedo or near-ir diffuse surface albedo
    ! icealv......visible beam surface albedo or visible diffuse surface albedo  
    ! dtc3x.......time increment dt    
    ! epsflt......time filtering factor ,where {a} is time filtered value
    !             {a(t)}=a(t)+epsflt*(a(t+dt)+{a(t-dt)}-2.0*a(t))   
    ! nmax........Number of point grid on continent     
    ! nsx.........Phenology dates to fall within one year period   
    ! itype.......Classe de textura do solo 
    ! vcover(iv)..Fracao de cobertura de vegetacao iv=1 Top
    ! vcover(iv)..Fracao de cobertura de vegetacao iv=2 Bottom
    ! z0x.........Roughness length   
    ! d...........Displacement height        
    ! rdc.........constant related to aerodynamic resistance   
    ! rbc.........Constant related to bulk boundary layer resistance   
    ! z0..........Roughness length    
    ! qm..........Reference specific humidity (fourier)  
    ! tm..........Reference temperature    (fourier)                (k)  
    ! um..........Razao entre zonal pseudo-wind (fourier) e seno da
    !             colatitude  
    ! vm..........Razao entre meridional pseudo-wind (fourier) e seno da
    !             colatitude  
    ! psur........Surface pressure in mb
    ! ppc.........Precipitation rate ( cumulus )           (mm/s) 
    ! ppl.........Precipitation rate ( large scale )       (mm/s) 
    ! radn........Downward sw/lw radiation at the surface
    ! tc..........Temperatura da copa "dossel"(K)   
    ! tg .........Temperatura da superficie do solo (K)  
    ! td..........Temperatura do solo profundo (K)   
    ! capac(iv)...Agua interceptada iv=1 no dossel "water store capacity
    !             of leaves"(m)
    ! capac(iv)...Agua interceptada iv=2 na cobertura do solo (m)
    ! w(id).......Grau de saturacao de umidade do solo id=1 na camada superficial
    ! w(id).......Grau de saturacao de umidade do solo id=2 na camada de raizes
    ! w(id).......Grau de saturacao de umidade do solo id=3 na camada de drenagem
    ! ra..........Resistencia Aerodinamica (s/m)    
    ! rb..........bulk boundary layer resistance    
    ! rd..........Aerodynamic resistance between ground      (s/m)
    !             and canopy air space   
    ! rc..........Resistencia do topo da copa    
    ! rg..........Resistencia da base da copa    
    ! tcta........Diferenca entre tc-ta                      (k)  
    ! tgta........Diferenca entre tg-ta                      (k)  
    ! ta..........Temperatura no nivel de fonte de calor do dossel (K)    
    ! ea..........Pressure of vapor    
    ! etc.........Pressure of vapor at top of the copa   
    ! etg.........Pressao de vapor no base da copa   
    ! btc.........btc(i)=EXP(30.25353  -5418.0  /tc(i))/(tc(i)*tc(i)).   
    ! btg.........btg(i)=EXP(30.25353  -5418.0  /tg(i))/(tg(i)*tg(i))   
    ! u2..........wind speed at top of canopy    
    ! radt........net heat received by canopy/ground vegetation  
    ! par.........par incident on canopy   
    ! pd..........ratio of par beam to total par    
    ! rst ........Resisttencia Estomatica "Stomatal resistence" (s/m)   
    ! rsoil.......Resistencia do solo (s/m) 
    ! phroot......Soil moisture potentials in root zone of each
    !             vegetation layer and summed soil+root resistance.
    ! hrr.........rel. humidity in top layer   
    ! phsoil......soil moisture potential of the i-th soil layer
    ! cc..........heat capacity of the canopy    
    ! cg..........heat capacity of the ground    
    ! satcap......saturation liquid water capacity         (m)
    ! snow........snow amount  
    ! dtc ........dtc(i)=pblsib(i,2,5)*dtc3x  
    ! dtg.........dtg(i)=pblsib(i,1,5)*dtc3x   
    ! dtm.........dtm(i)=pblsib(i,3,5)*dtc3x   
    ! dqm ........dqm(i)=pblsib(i,4,5)*dtc3x   
    ! stm.........Variavel utilizada mo cal. da Resisttencia   
    ! extk........extinction coefficient  
    ! radfac......Fractions of downward solar radiation at surface
    !             passed from subr.radalb
    ! closs.......Radiation loss from canopy 
    ! gloss.......Radiation loss from ground 
    ! thermk......Canopy emissivity
    ! p1f   
    ! p2f   
    ! ect.........Transpiracao no topo da copa (J/m*m)   
    ! eci.........Evaporacao da agua interceptada no topo da copa (J/m*m)   
    ! egt.........Transpiracao na base da copa (J/m*m)   
    ! egi.........Evaporacao da neve (J/m*m)   
    ! egs.........Evaporacao do solo arido (J/m*m)   
    ! ec..........Soma da Transpiracao e Evaporacao da agua interceptada pelo
    !             topo da copa   ec   (i)=eci(i)+ect(i)    
    ! eg..........Soma da transpiracao na base da copa +  Evaporacao do solo arido
    !             +  Evaporacao da neve  " eg   (i)=egt(i)+egs(i)+egi(i)"    
    ! hc..........Total sensible heat lost of top from the veggies.    
    ! hg..........Total sensible heat lost of base from the veggies.    
    ! ecidif......check if interception loss term has exceeded canopy storage
    !             ecidif(i)=MAX(0.0   , eci(i)-capac(i,1)*hlat3 )
    ! egidif......check if interception loss term has exceeded canopy storage
    !             ecidif(i)=MAX(0.0   , egi(i)-capac(i,1)*hlat3 )
    ! ecmass......Mass of water lost of top from the veggies.
    ! egmass......Mass of water lost of base from the veggies.
    ! etmass......Total mass of water lost from the veggies.
    ! hflux.......Total sensible heat lost from the veggies 
    ! chf.........Heat fluxes into the canopy  in w/m**2   
    ! shf.........Heat fluxes into the ground, in w/m**2   
    ! fluxef......Modified to use force-restore heat fluxes
    !             fluxef(i) = shf(i) - cg(i)*dtg(i)*dtc3xi " Garrat pg. 227"
    ! roff........runoff (escoamente superficial e drenagem)(m)  
    ! zlwup 
    ! drag........tensao superficial  
    ! hgdtg.......n.b. fluxes expressed in joules m-2 
    ! hgdtc.......n.b. fluxes expressed in joules m-2 
    ! hgdtm.......n.b. fluxes expressed in joules m-2 
    ! hcdtg.......n.b. fluxes expressed in joules m-2 
    ! hcdtc.......n.b. fluxes expressed in joules m-2 
    ! hcdtm.......n.b. fluxes expressed in joules m-2 
    ! egdtg.......partial derivative calculation for latent heat 
    ! egdtc.......partial derivative calculation for latent heat 
    ! egdqm.......partial derivative calculation for latent heat 
    ! ecdtg.......partial derivative calculation for latent heat 
    ! ecdtc.......partial derivative calculation for latent heat 
    ! ecdqm.......partial derivative calculation for latent heat 
    ! deadtg
    ! deadtc
    ! deadqm
    ! bps   
    ! psb   
    ! dzm.........Altura media de referencia  para o vento para o calculo
    !             da estabilidade do escoamento   
    ! em..........Pressao de vapor da agua    
    ! gmt.........temperature related matrix virtual temperature tendency
    !             due to vertical diffusion   
    ! gmq.........specific humidity related matrix specific humidity of
    !             reference (fourier)   
    ! gmu.........wind related matrix   
    ! cu..........Friction  transfer coefficients.    
    ! cuni........Neutral friction transfer  coefficients.  
    ! ctni........Neutral heat transfer coefficients.  
    ! ustar.......Surface friction velocity  (m/s) 
    ! salb........surface albedo  
    ! tgeff.......effective ground temperature 
    ! cosz........Cosine of zenith angle  
    ! rhoair......Desnsidade do ar
    ! psy.........(cp/(hl*epsfac))*psur(i)   
    ! rcp.........densidade do ar vezes o calor especifico do ar   
    ! wc..........Minimo entre 1 e a razao entre a agua interceptada pelo
    !             indice de area foliar no topo da copa    
    ! wg..........Minimo entre 1 e a razao entre a agua interceptada pelo
    !             indice de area foliar na base da copa    
    ! fc..........Condicao de oravalho 0 ou 1 na topo da copa    
    ! fg..........Condicao de oravalho 0 ou 1 na base da copa    
    ! hrr.........rel. humidity in top layer    
    ! ssib
    ! yvisb.......Downward Surface shortwave fluxe visible beam (cloudy)  
    ! yvisd.......Downward Surface shortwave fluxe visible diffuse (cloudy) 
    ! ynirb.......Downward Surface shortwave fluxe Near-IR beam (cloudy) 
    ! ynird.......Downward Surface shortwave fluxe Near-IR diffuse (cloudy) 
    ! yswtop......swinc....solar input at top of atmosphere
    ! yvisbc......Downward Surface shortwave fluxe visible beam (clear) 
    ! yvisdc......Downward Surface shortwave fluxe visible diffuse (clear)
    ! ynirbc......Downward Surface shortwave fluxe Near-IR beam (clear)
    ! ynirdc......Downward Surface shortwave fluxe Near-IR diffuse (clear)
    ! cldsav......Cloud cover
    ! cp..........Specific heat of air           (j/kg/k)      
    ! hl..........heat of evaporation of water     (j/kg)      
    ! rgas........gas constant of dry air        (j/kg/k)   
    ! g...........grav   gravity constant        (m/s**2)          
    ! solcon......solar constant (wgne value)    (w/m**2)  
    ! rmwmd.......fracao molar entre a agua e o ar   
    ! swint.......sw subr. call interval in hours
    !             swint has to be less than or equal to trint
    !                              and mod(trint,swint)=0

    ! trint.......ir subr. call interval in hours      
    ! yrl.........length of year in days        
    ! idate(4)....output : idate(1) = initial hour of
    !                      idate(2) = day of month.
    !                      idate(3) = month of year.
    !                      idate(4) = year.

    ! idatec(4)...output : idatec(1)=current hour of
    !                   idatec(2)=current day of month.
    !                   idatec(3)=current month of year.
    !                   idatec(4)=current year.

    ! kt..........hour of present  time step       
    ! ktm.........hour of previous time step       
    ! jdt.........time step in getdia      
    ! monl(12)....length of each month in days   
    ! irad........the physical process radiation        
    !                     irad = yes   included
    !                     irad = no    excluded
    ! iccon.......the physical process cumulus convection(kuo)
    !                     iccon = yes   included
    !                     iccon = no    excluded           
    ! icld........>>> icld = 1    : old cloud emisivity (optical depth) setting      
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
    ! inalb.......inalb Input two types of surface albedo 
    !                >>> inalb= 1 : input two  types surfc albedo (2 diffused)
    !                             direct beam albedos are calculated by the subr.
    !                >>> inalb= 2 : input four types surfc albedo (2 diff,2 direct)

    ! co2val......co2val is wgne standard value in ppm "co2val = /345.0/           
    ! delt........time interval in sec (fixed throuh the integration)           
    ! filta.......weight used on central time
    !              step of robert time filter.
    !              set in main routine "smf".filta=0.92e0           
    ! nfin0.......input  file at time level t-dt           
    ! nfin1.......input  file at time level t     
    ! initlz......constant initlz=2.           
    ! nfcnv0......initial information on convective clouds for int. radiation           
    ! nfcldr......constant nfcldr = 74    
    ! tbase.......constant tbase =  273.15e00        
    ! latco.......latitude         
    ! dodia.......Variable logical for search for combined field components. 
    ! lvavl.......levels in available diagnostic (1 or kmax)
    ! nuavl.......unit code of available diagnostic
    ! itavl.......type of available diagnostic (1 gaussian, 2 spectral)
    ! iavrq.......Number of requested diagnostic      
    ! ixavl.......Number available diagnostic components for combined fields
    ! inavl.......Number available diagnostic similar requested diagnostic
    ! iclcd.......requested diagnostic calculation code (0 direct
    !             calculation, > 0 add to requested field number iclcd,
    !             < 0 subtract from requested field number -iclcd )
    ! nucf........nurq  = unit code of requested diagnostic       
    ! ixcf........Number of requested diagnostic         
    ! incf........combined fields 
    ! kravl.......Number of available diagnostics equivalence 
    !              the desired diagnostic
    ! krcf........combined fields 
    ! jrcf........combined fields 
    ! ndt.........time step in getdia            
    ! latitu......latco grid point reference the latitude            
    ! cdhl........logical indicator for dhn output prognostics
    ! ustr........surface zonal stress umom(i)=fmom*um(ncount),
    !               where .fmom  momentum flux      in n/m**2
    !               fmom= rhoair(ncount)*cu(ncount)*ustar(ncount)
    !               um  (ncount)=gu (i,1)/sinclt
    !               gu          = (zonal velocity)*sin(colat)

    ! vstr........surface meridional stress.vmom(i)=rho(i)*gv(i)*rmi(i)
    !                           rho  (i)=gps(i)/(gr100*gt(i))
    !                           gr100 =gasr*0.01
    ! first.......control logical variable .true. or .false. 
    ! mxrdcc......use maximum random converage for radiative conv. clouds
    !               constant logical mxrdcc = .true.
    ! lcnvl.......the lowest layer index where non-convective clouds can
    !               occur (ben says this should be 2 or more)
    !               constant lcnvl = 2 
    ! lthncl......Minimum depth in mb of non-zero low level cloud
    !             consta lthncl=80
    ! convc.......ncols*jmax convective cloud cover in 3 hr. avrage
    ! convt.......ncols*jmax convective cloud top  (sigma layer) 
    ! convb.......ncols*jmax convective cloud base (sigma layer)
    ! convts
    ! convcs
    ! convbs
    ! sigki ......sigki (k)=1.0e0/EXP(rk*LOG(sig(k))),  where "sig"
    !             sigma coordinate at middle of layer and rk=gasr/cp 
    ! xvisb.......Downward Surface shortwave fluxe visible beam (cloudy)  
    ! xvisd.......Downward Surface shortwave fluxe visible diffuse (cloudy) 
    ! xnirb.......Downward Surface shortwave fluxe Near-IR beam (cloudy)  
    ! xnird.......Downward Surface shortwave fluxe Near-IR diffuse (cloudy)   
    ! xswtop......shortwave upward at top  or  shortwave upward at top (clear)
    ! xvisbc......Downward Surface shortwave fluxe visible beam (clear) 
    ! xvisdc......Downward Surface shortwave fluxe visible diffuse (clear)
    ! xnirbc......Downward Surface shortwave fluxe Near-IR beam (clear)
    ! xnirdc......Downward Surface shortwave fluxe Near-IR diffuse (clear)
    !==========================================================================
    !
    INTEGER, INTENT(IN   ) :: ncols
    INTEGER, INTENT(IN   ) :: kmax
    REAL,    INTENT(INOUT) :: gt    (ncols,kmax)
    REAL,    INTENT(INOUT) :: gq    (ncols,kmax)
    REAL,    INTENT(IN   ) :: gu    (ncols,kmax)
    REAL,    INTENT(IN   ) :: gv    (ncols,kmax)
    REAL,    INTENT(IN   ) :: gps   (ncols)      
    REAL,    INTENT(INOUT) :: tc0   (ncols)
    REAL,    INTENT(INOUT) :: tg0   (ncols)
    REAL,    INTENT(INOUT) :: td0   (ncols)
    REAL,    INTENT(INOUT) :: w0    (ncols,3)
    REAL,    INTENT(INOUT) :: capac0(ncols,2)
    REAL,    INTENT(INOUT) :: tcm   (ncols)
    REAL,    INTENT(INOUT) :: tgm   (ncols)
    REAL,    INTENT(INOUT) :: tdm   (ncols)
    REAL,    INTENT(INOUT) :: wm    (ncols,3)
    REAL,    INTENT(INOUT) :: capacm(ncols,2)
    REAL,    INTENT(IN   ) :: ppli  (ncols)
    REAL,    INTENT(IN   ) :: ppci  (ncols)
    REAL,    INTENT(INOUT) :: gyu   (ncols,kmax)
    REAL,    INTENT(INOUT) :: gyv   (ncols,kmax)
    REAL,    INTENT(INOUT) :: gtd   (ncols,kmax)
    REAL,    INTENT(INOUT) :: gqd   (ncols,kmax)
    REAL,    INTENT(INOUT  ) :: htr   (ncols,kmax)
    REAL,    INTENT(INOUT  ) :: clr   (ncols,kmax)
    INTEGER, INTENT(IN   ) :: imask (ncols)
    REAL,    INTENT(INOUT  ) :: rvisb (ncols)
    REAL,    INTENT(INOUT  ) :: rvisd (ncols)
    REAL,    INTENT(INOUT  ) :: rnirb (ncols)
    REAL,    INTENT(INOUT  ) :: rnird (ncols)
    REAL,    INTENT(INOUT  ) :: dlwbot(ncols)
    REAL,    INTENT(INOUT) :: tsea  (ncols)
    REAL,    INTENT(IN   ) :: colrad(ncols)
    REAL,    INTENT(IN   ) :: lonrad(ncols)
    REAL,    INTENT(IN   ) :: sig   (kmax)
    REAL,    INTENT(IN   ) :: sigml (kmax)
    REAL,    INTENT(IN   ) :: delsig(kmax)
    INTEGER, INTENT(IN   ) :: ifday
    REAL,    INTENT(IN   ) :: tod   
    REAL,    INTENT(INOUT  ) :: avisb (ncols)
    REAL,    INTENT(INOUT) :: avisd (ncols)
    REAL,    INTENT(INOUT  ) :: anirb (ncols)
    REAL,    INTENT(INOUT  ) :: anird (ncols)
    REAL,    INTENT(INOUT  ) :: dswtop(ncols)
    REAL,    INTENT(INOUT  ) :: rs    (ncols)
    REAL,    INTENT(INOUT  ) :: ulwtop(ncols)
    REAL,    INTENT(INOUT) :: gl0   (ncols)
    REAL,    INTENT(INOUT) :: zorl  (ncols)
    REAL,    INTENT(IN   ) :: gtt   (ncols,kmax)
    REAL,    INTENT(IN   ) :: gqq   (ncols,kmax)
    REAL,    INTENT(IN   ) :: sheleg(ncols)
    REAL,    INTENT(INOUT) :: tseam (ncols)
    REAL,    INTENT(IN   ) :: omg   (ncols,kmax)
    REAL,    INTENT(INOUT  ) :: rvisbc(ncols)
    REAL,    INTENT(INOUT  ) :: rvisdc(ncols)
    REAL,    INTENT(INOUT  ) :: rnirbc(ncols)
    REAL,    INTENT(INOUT  ) :: rnirdc(ncols)
    REAL,    INTENT(INOUT  ) :: htrc  (ncols,kmax)
    REAL,    INTENT(INOUT  ) :: dlwclr(ncols)
    REAL,    INTENT(INOUT  ) :: uswtpc(ncols)
    REAL,    INTENT(INOUT  ) :: rsclr (ncols)
    REAL,    INTENT(INOUT  ) :: ultclr(ncols)
    REAL,    INTENT(IN   ) :: ssib(ncols)           
    !
    !     this is for interpolating shortwave rad at ground
    !
    REAL,    INTENT(INOUT  ) :: yvisb (ncols)      
    REAL,    INTENT(INOUT  ) :: yvisd (ncols)      
    REAL,    INTENT(INOUT  ) :: ynirb (ncols)      
    REAL,    INTENT(INOUT  ) :: ynird (ncols)      
    REAL,    INTENT(INOUT  ) :: yswtop(ncols)      
    REAL,    INTENT(INOUT  ) :: yvisbc(ncols)      
    REAL,    INTENT(INOUT  ) :: yvisdc(ncols)      
    REAL,    INTENT(INOUT  ) :: ynirbc(ncols)      
    REAL,    INTENT(INOUT  ) :: ynirdc(ncols)      
    REAL,    INTENT(INOUT) :: cldsav(ncols)      
    !
    !     these are for monitoring of gpv in gfidi.
    !

    INTEGER, INTENT(IN   ) :: latco                     

    REAL,    INTENT(INOUT  ) :: ustr(ncols)           
    REAL,    INTENT(INOUT  ) :: vstr(ncols)           

    REAL,    INTENT(IN   ) :: convc (ncols)            
    REAL,    INTENT(IN   ) :: convt (ncols)            
    REAL,    INTENT(IN   ) :: convb (ncols)            

    REAL,    INTENT(IN   ) :: convts(ncols)         
    REAL,    INTENT(IN   ) :: convcs(ncols)         
    REAL,    INTENT(IN   ) :: convbs(ncols)         

    REAL,    INTENT(IN ) :: sigki (kmax)
    REAL,    INTENT(INOUT) :: xvisb (ncols)
    REAL,    INTENT(INOUT) :: xvisd (ncols)
    REAL,    INTENT(INOUT) :: xnirb (ncols)
    REAL,    INTENT(INOUT) :: xnird (ncols)
    REAL,    INTENT(INOUT) :: xswtop(ncols)
    REAL,    INTENT(INOUT) :: xvisbc(ncols)
    REAL,    INTENT(INOUT) :: xvisdc(ncols)
    REAL,    INTENT(INOUT) :: xnirbc(ncols)
    REAL,    INTENT(INOUT) :: xnirdc(ncols)  

    REAL,    INTENT(INOUT) :: ps    (ncols)
    REAL   , INTENT(INOUT) :: var   (ncols)              
    REAL,    INTENT(INOUT  ) :: sens  (ncols)
    REAL,    INTENT(INOUT  ) :: evap  (ncols)    
    REAL   , INTENT(IN   ) :: cos2d  (ncols)    
    LOGICAL, INTENT(IN   ) :: intcosz

    REAL   , PARAMETER :: alon   =  0.0 
    INTEGER :: nmax                 
    INTEGER :: icld            
    INTEGER :: inalb            
    REAL                   :: uswtop(ncols)
    INTEGER :: nsx            
    INTEGER :: itype(ncols)            
    REAL :: zlt2    (ncols,icg)
    REAL :: green2  (ncols,icg)
    REAL :: chil2   (ncols,icg)
    REAL :: vcover  (ncols,icg)
    REAL :: topt2   (ncols,icg)
    REAL :: tll2        (ncols,icg)
    REAL :: tu2        (ncols,icg)
    REAL :: defac2  (ncols,icg)
    REAL :: ph12        (ncols,icg)
    REAL :: ph22        (ncols,icg)
    REAL :: rstpar2(ncols,icg,iwv)       
    REAL :: z0x      (ncols)            
    REAL :: d      (ncols)            
    REAL :: rdc      (ncols)            
    REAL :: rbc      (ncols)            
    REAL :: z0      (ncols)            
    !
    !     the size of working area is ncols*187
    !     atmospheric parameters as boudary values for sib
    !
    REAL :: qm  (ncols)                 
    REAL :: tm  (ncols)                 
    REAL :: um  (ncols)                 
    REAL :: vm  (ncols)                 
    REAL :: psur(ncols)                 
    REAL :: ppc (ncols)                 
    REAL :: ppl (ncols)                 
    REAL :: radn(ncols,3,2)           
    !
    !     prognostic variables
    !
    REAL :: tc   (ncols)             
    REAL :: tg   (ncols)             
    REAL :: td   (ncols)             
    REAL :: capac(ncols,2)           
    REAL :: w    (ncols,3)           
    !
    !     variables calculated from above and ambient conditions
    !
    REAL :: ra      (ncols)          
    REAL :: rb      (ncols)          
    REAL :: rd      (ncols)          
    REAL :: rc      (ncols)          
    REAL :: rg      (ncols)          
    REAL :: tcta  (ncols)          
    REAL :: tgta  (ncols)          
    REAL :: ta      (ncols)          
    REAL :: ea      (ncols)          
    REAL :: etc      (ncols)          
    REAL :: etg      (ncols)          
    REAL :: btc      (ncols)          
    REAL :: btg      (ncols)          
    REAL :: u2      (ncols)          
    REAL :: radt  (ncols,icg)         
    REAL :: par      (ncols,icg)         
    REAL :: pd      (ncols,icg)         
    REAL :: rst      (ncols,icg)         
    REAL :: rsoil (ncols)          
    REAL :: phroot(ncols,icg)         
    REAL :: hrr      (ncols)          
    REAL :: phsoil(ncols,idp)         
    REAL :: cc      (ncols)          
    REAL :: cg      (ncols)          
    REAL :: satcap(ncols,icg)         
    REAL :: snow  (ncols,icg)         
    REAL :: dtc      (ncols)          
    REAL :: dtg      (ncols)          
    REAL :: dtm      (ncols)          
    REAL :: dqm      (ncols)          
    REAL :: stm      (ncols,icg)         
    REAL :: extk  (ncols,icg,iwv,ibd) 
    REAL :: radfac(ncols,icg,iwv,ibd) 
    REAL :: closs (ncols)          
    REAL :: gloss (ncols)          
    REAL :: thermk(ncols)          
    REAL :: p1f      (ncols)          
    REAL :: p2f      (ncols)          
    !
    !     heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2
    !
    REAL :: ect      (ncols)            
    REAL :: eci      (ncols)            
    REAL :: egt      (ncols)            
    REAL :: egi      (ncols)            
    REAL :: egs      (ncols)            
    REAL :: ec      (ncols)            
    REAL :: eg      (ncols)            
    REAL :: hc      (ncols)            
    REAL :: hg      (ncols)            
    REAL :: ecidif(ncols)            
    REAL :: egidif(ncols)            
    REAL :: ecmass(ncols)            
    REAL :: egmass(ncols)            
    REAL :: etmass(ncols)            
    REAL :: hflux (ncols)            
    REAL :: chf      (ncols)            
    REAL :: shf      (ncols)            
    REAL :: fluxef(ncols)            
    REAL :: roff  (ncols)            
    REAL :: zlwup (ncols)            
    REAL :: drag  (ncols)            
    !
    !     this is for coupling with closure turbulence model
    !
    REAL :: bps      (ncols)            
    REAL :: psb      (ncols)            
    REAL :: dzm      (ncols)            
    REAL :: em      (ncols)            
    REAL :: gmt      (ncols,3)           
    REAL :: gmq      (ncols,3)           
    REAL :: gmu      (ncols,4)           
    REAL :: cu      (ncols)            
    REAL :: cuni  (ncols)            
    REAL :: ctni  (ncols)            
    REAL :: ustar (ncols)            
    REAL :: salb  (ncols,2,2)           
    REAL :: tgeff (ncols)            
    REAL :: cosz  (ncols)            
    REAL :: rhoair(ncols)            
    REAL :: psy      (ncols)            
    REAL :: rcp      (ncols)            
    REAL :: wc      (ncols)            
    REAL :: wg      (ncols)            
    REAL :: fc      (ncols)            
    REAL :: fg      (ncols)            
    REAL :: hr      (ncols)       


    INTEGER :: ndt                     
    INTEGER :: latitu                  

    REAL    :: swrk  (ncols,57)        
    REAL    :: tmtx  (ncols,kmax,3)
    REAL    :: qmtx  (ncols,kmax,3)
    REAL    :: umtx  (ncols,kmax,4)
    REAL    :: bfrg  (ncols)
    REAL    :: bfr1  (ncols)
    REAL    :: bfr2  (ncols)
    REAL    :: gtmp  (ncols,kmax)   
    REAL    :: gwv   (ncols,kmax)
    REAL    :: rnet  (ncols)
    REAL    :: q     (ncols,kmax)   
    REAL    :: pcb   (ncols)      
    REAL    :: xsea  (ncols)
    REAL    :: relhum(ncols,kmax)
    REAL    :: tsurf (ncols)
    REAL    :: qsurf (ncols)
    REAL    :: rmi   (ncols)
    REAL    :: rhi   (ncols)
    REAL    :: umom  (ncols)
    REAL    :: vmom  (ncols)
    REAL    :: strcut
    REAL    :: slrad (ncols)
    REAL    :: zenith(ncols)
    REAL    :: cond  (ncols)
    REAL    :: stor  (ncols)

    REAL    :: tmin  (ncols)
    REAL    :: tmax  (ncols)
    REAL    :: cldtot(ncols,kmax)
    REAL    :: cldinv(ncols,kmax)
    REAL    :: cldsat(ncols,kmax)
    REAL    :: cldcon(ncols,kmax)
    REAL    :: cldson(ncols,kmax)
    REAL    :: clwd  (ncols,kmax)
    REAL    :: emisd (ncols,kmax)
    REAL    :: taud  (ncols,kmax)
    !
    !     the names of some variables in comphc have to be changed to
    !     avoid conflict with names in sibcon (comsibw)
    !
    !     intg=1  implicit one dt integration
    !     =2  implicit two dt leapfrog integration
    !     epsflt  time filtering factor ,where {a} is time filtered value
    !     {a(t)}=a(t)+epsflt*(a(t+dt)+{a(t-dt)}-2.0*a(t))
    !     
    LOGICAL :: cloudr
    REAL    :: sdelt
    REAL    :: ratio
    REAL    :: etime
    REAL    :: xday

    LOGICAL :: ghl_local
    INTEGER :: k 
    INTEGER :: i 
    INTEGER :: lrecl 
    INTEGER :: ncount 
    INTEGER :: ntyp 
    INTEGER :: jlat 
    INTEGER :: ind 
    INTEGER :: itr 
    INTEGER :: intstp 
    INTEGER :: modstp 
    INTEGER :: mmax
    REAL    :: csqiv2 (ncols)
    REAL    :: deltm 
    REAL    :: sindel 
    REAL    :: cosdel 
    REAL    :: fimxi
    REAL    :: ctime 
    REAL    :: cos2  (ncols)
    REAL    :: frh 
    REAL    :: btime 
    REAL    :: atime 
    REAL    :: s0 
    REAL    :: cpsy 
    REAL    :: rbyg 
    REAL    :: r100 
    REAL    :: f
    REAL    :: ocealb 
    REAL    :: tice01 
    REAL    :: radint 
    REAL    :: fstp 
    REAL    :: fstp1 
    REAL    :: fmom 
    REAL    :: rfac 
    REAL    :: dtc3xi
    INTEGER :: month   (ncols) 
    INTEGER :: month2  (ncols) 
    REAL    :: colrad2 (ncols)
    REAL    :: zenith1 (ncols) 
    REAL    :: zenith2 (ncols)
    REAL    :: sinclt2 (ncols)
    REAL    :: cos3 
    REAL    :: chug  (ncols,kmax)
    REAL    :: chvg  (ncols,kmax)
    REAL    :: xdrag (ncols)
    REAL    :: ydrag (ncols)


    REAL    :: xcover(ityp, imon, icg)
    REAL    :: zlt(ityp, imon, icg)
    REAL    :: green(ityp,imon,icg)
    REAL    :: ph2(ityp,icg)
    REAL    :: ph1(ityp,icg)
    REAL    :: defac(ityp,icg)
    REAL    :: tu(ityp,icg)
    REAL    :: tll(ityp,icg)
    REAL    :: topt(ityp,icg)
    REAL    :: rstpar(ityp,icg,iwv)
    REAL    :: chil(ityp,icg)

    !--------------------------------------------------------------------------
    !       locations for available diagnostics in this subroutine
    !--------------------------------------------------------------------------     
    !  nDiag_tmtsfc =  6   ! time mean surface temperature          
    !  nDiag_runoff = 13   ! runoff                            
    !  nDiag_intlos = 15   ! interception loss                  
    !  nDiag_sheatf = 16   ! sensible heat flux                 
    !  nDiag_lheatf = 17   ! latent heat flux                            
    !  nDiag_ustres = 18   ! surface zonal stress                
    !  nDiag_vstres = 19   ! surface meridional stress            
    !  nDiag_cloudc = 20   ! cloud cover                      
    !  nDiag_lwdbot = 21   ! longwave downward at bottom          
    !  nDiag_lwubot = 22   ! longwave upward at bottom            
    !  nDiag_lwutop = 23   ! longwave upward at top                
    !  nDiag_swdtop = 24   ! shortwave downward at top            
    !  nDiag_swdbot = 25   ! shortwave downward at ground          
    !  nDiag_swubot = 26   ! shortwave upward at bottom           
    !  nDiag_swutop = 27   ! shortwave upward at top                      
    !  nDiag_swabea = 28   ! shortwave absorbed by the earth/atmosphere 
    !  nDiag_swabgr = 29   ! shortwave absorbed by the ground                
    !  nDiag_lwnetb = 30   ! net longwave at bottom                
    !  nDiag_lwheat = 31   ! longwave heating                            
    !  nDiag_swheat = 32   ! shortwave heating                  
    !  nDiag_vdheat = 39   ! vertical diffusion heating           
    !  nDiag_vdmois = 40   ! vertical diffusion moistening          
    !  nDiag_vduzon = 41   ! vertical diffusion zonal momentum change          
    !  nDiag_vdvmer = 42   ! vertical diffusion meridional momentum change
    !  nDiag_txgwds = 43   ! gravity wave drag surface zonal stress 
    !  nDiag_tygwds = 44   ! gravity wave drag surface meridional stress
    !  nDiag_gwduzc = 45   ! gravity wave drag zonal momentum change          
    !  nDiag_gwdvmc = 46   ! gravity wave drag meridional momentum change
    !  nDiag_lwdbtc = 56   ! longwave downward at bottom (clear)    
    !  nDiag_lwutpc = 57   ! longwave upward at top (clear)          
    !  nDiag_swdbtc = 58   ! shortwave downward at ground (clear)   
    !  nDiag_swubtc = 59   ! shortwave upward at bottom (clear)     
    !  nDiag_swutpc = 60   ! shortwave upward at top (clear)                
    !  nDiag_swaeac = 61   ! shortwave absorbed by the earth/atmosphere (clear)
    !  nDiag_swabgc = 62   ! shortwave absorbed by the ground (clear)          
    !  nDiag_lwnbtc = 63   ! net longwave at bottom (clear)          
    !  nDiag_tcairs = 67   ! temperature of canopy air space                
    !  nDiag_ecairs = 68   ! vapor pressure of canopy air space     
    !  nDiag_bsolht = 69   ! bare soil latent heat                
    !  nDiag_vdtclc = 72   ! vertical dist total cloud cover                
    !  nDiag_invcld = 73   ! inversion cloud                            
    !  nDiag_ssatcl = 74   ! supersaturation cloud                
    !  nDiag_cnvcld = 75   ! convective cloud                            
    !  nDiag_shcvcl = 76   ! shallow convective cloud                      
    !  nDiag_clliwp = 77   ! cloud liquid water path                      
    !  nDiag_lwcemi = 78   ! longwave cloud emissivity            
    !  nDiag_sclopd = 79   ! shortwave cloud optical depth          
    !  nDiag_mofres = 80   ! momentum flux resistance                      
    !  nDiag_casrrs = 81   ! canopy air spc to ref. lvl resistance  
    !  nDiag_cascrs = 82   ! canopy air spc to canopy resistance    
    !  nDiag_casgrs = 83   ! canopy air spc to ground resistance    
    !  nDiag_gcovrs = 84   ! ground cover resistance                      
    !  nDiag_bssfrs = 85   ! bare soil surface resistance          
    !--------------------------------------------------------------------------

    ghl_local = IsGridHistoryOn() 
    IF(dogwd.EQ.0)THEN
       CALL Gwdd(ps ,gu   ,gv   ,gt   , chug, chvg, xdrag, ydrag, &
            var, varcut, sigml, sig, delsig, ncols, kmax)
       DO  k=1, kmax
          DO i=1, ncols                
             gyu(i,k) = gyu(i,k) - chug(i,k)
             gyv(i,k) = gyv(i,k) - chvg(i,k)
          END DO
       END DO
    ENDIF

    CALL CopySurfaceData(xcover, zlt, green, ph2, ph1, defac, tu, tll, topt, rstpar, chil)
    vcover  = 0.0
    zlt2    = 0.0
    green2  = 0.0
    chil2   = 0.0
    rstpar2 = 0.0
    topt2   = 0.0
    tll2    = 0.0
    tu2     = 0.0
    defac2  = 0.0
    ph12    = 0.0
    ph22    = 0.0

    IF(dodia(nDiag_txgwds))CALL updia(xdrag,nDiag_txgwds,latco)
    IF(dodia(nDiag_tygwds))CALL updia(ydrag,nDiag_tygwds,latco)
    IF(dodia(nDiag_gwduzc))CALL updia(chug, nDiag_gwduzc,latco)
    IF(dodia(nDiag_gwdvmc))CALL updia(chvg, nDiag_gwdvmc,latco)
    ndt=jdt
    latitu=latco

    cloudr=dodia(nDiag_vdtclc).OR.dodia(nDiag_invcld).OR.dodia(nDiag_ssatcl).OR. &
         dodia(nDiag_cnvcld).OR.dodia(nDiag_shcvcl).OR.dodia(nDiag_clliwp).OR. &
         dodia(nDiag_lwcemi).OR.dodia(nDiag_sclopd).OR.(ghl_local.AND.dogrh(nGHis_vdtclc,latco))

    IF(cloudr.AND.jdt.EQ.1.AND.ktm.EQ.-1.AND.kt.EQ.0)THEN
       INQUIRE (IOLENGTH=lrecl) cldtot,cldinv,cldsat,cldcon, &
            cldson,clwd,emisd,taud
       OPEN(nfcldr,access='DIRECT',form='UNFORMATTED',recl=lrecl, &
            status='UNKNOWN')
    END IF
    IF(cloudr)THEN
       DO k=1,kmax
          DO i=1,ncols
             cldtot(i,k)=0.0e0
             cldinv(i,k)=0.0e0
             cldsat(i,k)=0.0e0
             cldcon(i,k)=0.0e0
             cldson(i,k)=0.0e0
             clwd(i,k)=0.0e0
             emisd(i,k)=0.0e0
             taud(i,k)=0.0e0
          END DO
       END DO
    ENDIF

    dtc3x   =dt*float(intg)

    DO i=1,ncols,1 
       csqiv2(i)   = 1.0e0/SIN(colrad(i))**2
    END DO
    !
    !
    !
    DO k=1,kmax
       DO i=1,ncols
          gq(i,k)=MAX(1.0e-12,gq(i,k))
          gt(i,k)=gt(i,k)/(1.0e0+0.608e0*gq(i,k))
       END DO
    END DO

    IF(initlz.GE.0.AND.kt.EQ.0.AND.jdt.EQ.1) THEN
       ncount=0
       DO i=1,ncols
          IF(imask(i).GE.1) THEN
             ncount=ncount+1
             tc0(ncount)=gt(i,1)
             tg0(ncount)=gt(i,1)
             tcm(ncount)=gt(i,1)
             tgm(ncount)=gt(i,1)
             IF(sheleg(i).GT.0.0e0) THEN
                tg0(ncount)=MIN(tg0(ncount),tf-0.01e0)
                tgm(ncount)=MIN(tgm(ncount),tf-0.01e0)
             END IF
          END IF
       END DO
    END IF
    ncount=0
    DO i=1,ncols
       rhi(i)=1.0e-6
       rmi(i)=1.0e-6
       IF(imask(i).GE.1) THEN
          ncount=ncount+1
          itype(ncount)=imask(i)
       END IF
    END DO
    nmax=ncount
    !
    !     yamada-mellor pbl parameterization
    !
    deltm =0.5e0 *dtc3x
    CALL ympbl0( &
         gu    ,gv    ,gt    ,gq    ,delsig,ncols, &
         kmax  ,deltm ,csqiv2,tmtx  ,qmtx  ,umtx  ,gl0   )
    !
    !     computation of astronomical parameters
    !     sdelt ;solar inclination
    !     etime ;correction factor to local time
    !     ratio ;factor relating to the distance between the earth and the sun
    !
    CALL radtim(idatec,sdelt ,ratio ,etime ,tod   ,xday  ,yrl)
    !     
    !     mon is the month used for vegetation data input
    ! 
    DO i=1,ncols,1
       month(i)=idatec(2)
       IF((((colrad(i)*180.0)/3.1415926e0)-90.0)  > 0.0 ) THEN
          month(i)  =  month(i) + 6
          IF(month(i).GE.13) month(i) = month(i)-12
       END IF
    END DO

    ncount=0

    DO i=1,ncols
       IF(imask(i).GE.1) THEN
          ncount=ncount+1
          month2(ncount)=month(i)
          colrad2(ncount)=colrad(i)
          sinclt2(ncount) = SIN(colrad(i))
       END IF
    END DO

    sindel=SIN(sdelt)
    cosdel=COS(sdelt)
    fimxi  =24.0e0 /360.0
    ctime  =alon/15.0e0
    cos2   =0.0e0
    cos3   =0.0e0
    ncount =0
    frh=( MOD(tod+0.03125,3600.0)-0.03125)/3600.0

    DO i=1,ncols
       zenith1(i)  = sindel*COS(colrad(i))
    ENDDO

    DO i=1,ncols
       btime       = fimxi*lonrad(i)+ctime
       atime       = etime+pai12*(12.0-idatec(1)-frh-btime)
       zenith2(i)  = cosdel*SIN(colrad(i))*COS(atime)
       zenith(i)   = zenith1(i) + zenith2(i)
       IF(zenith(i).GT.0.0e0) THEN
          ncount   =ncount+1
          !cos2     =cos2+zenith(i) !!!!mudanca forcada 
       END IF
    END DO

    IF(ncount.EQ.0) ncount=1
    IF(intcosz)THEN
       !cos2=cos2/float(ncount)!!!!mudanca forcada 
       cos2=cos2d
    ELSE  
       cos2=zenith
    END IF
    s0     =ratio*solcon
    !
    !     sib setting **
    !
    IF(nmax.GE.1) THEN
       DO i=1,nmax
          ntyp=itype(i)
          topt2      (i,1) =   topt  (ntyp,1)
          tll2      (i,1) =   tll   (ntyp,1)
          tu2      (i,1) =   tu    (ntyp,1)
          defac2  (i,1) =   defac (ntyp,1)
          ph12      (i,1) =   ph1   (ntyp,1)
          ph22      (i,1) =   ph2   (ntyp,1)
          rstpar2 (i,1,1)= rstpar(ntyp,1,1)
          rstpar2 (i,1,2)= rstpar(ntyp,1,2)
          rstpar2 (i,1,3)= rstpar(ntyp,1,3)
          zlt2   (i,1) =  zlt   (ntyp,month2(i),1)
          green2 (i,1) =  green (ntyp,month2(i),1)
          chil2  (i,1) =  chil  (ntyp,1)
          vcover (i,1) =  xcover(ntyp,month2(i),1)
       END DO
       CALL wheat (itype  (1:nmax)  ,&
            nmax             ,&
            colrad2(1:nmax)  ,&
            month2 (1:nmax)  ,&
            xday             ,&
            yrl              ,&
            idatec           ,&
            monl             ,&
            nsx              ,&
            vcover (1:nmax,:),&
            zlt2   (1:nmax,:),&
            green2 (1:nmax,:),&
            chil2  (1:nmax,:),&
            rstpar2(1:nmax,:,:),&
            topt2  (1:nmax,:),&
            tll2   (1:nmax,:),&
            tu2    (1:nmax,:),&
            defac2 (1:nmax,:),&
            ph12   (1:nmax,:),&
            ph22   (1:nmax,:),&
            xcover, zlt, green, ph2, ph1, defac, tu, tll, topt, rstpar, chil)

       DO i=1,nmax
          ntyp=itype(i)
          topt2      (i,2) =   topt  (ntyp,2)
          tll2      (i,2) =   tll   (ntyp,2)
          tu2      (i,2) =   tu    (ntyp,2)
          defac2  (i,2) =   defac (ntyp,2)
          ph12      (i,2) =   ph1   (ntyp,2)
          ph22      (i,2) =   ph2   (ntyp,2)
          rstpar2 (i,2,1)= rstpar(ntyp,2,1)
          rstpar2 (i,2,2)= rstpar(ntyp,2,2)
          rstpar2 (i,2,3)= rstpar(ntyp,2,3)
          zlt2   (i,2) =  zlt   (ntyp,month2(i),2)
          green2 (i,2) =  green (ntyp,month2(i),2)
          chil2  (i,2) =  chil  (ntyp,2)
          vcover (i,2) =  xcover(ntyp,month2(i),2)
          rbc    (i)   =  xbc   (ntyp,month2(i))
          rdc    (i)   =  xdc   (ntyp,month2(i))
          z0x    (i)   =  x0x   (ntyp,month2(i))
          d      (i)   =  xd    (ntyp,month2(i))
          td     (i)   =  tdm   (i)
          tg     (i)   =  tgm   (i)
          tc     (i)   =  tcm   (i)
          capac  (i,1) =  capacm(i,1)
          capac  (i,2) =  capacm(i,2)
          w      (i,1) =  wm    (i,1)
          w      (i,2) =  wm    (i,2)
          w      (i,3) =  wm    (i,3)
       END DO
       ncount=0
       DO i=1,ncols
          IF(imask(i).GE.1) THEN
             ncount=ncount+1
             psur(ncount)=gps(i)
             tm  (ncount)=gt (i,1)
             qm  (ncount)=gq (i,1)
             um  (ncount)=gu (i,1)/ SIN( colrad(i))
             vm  (ncount)=gv (i,1)/ SIN( colrad(i))
             cosz(ncount)=zenith(i)
          END IF
       END DO
       cpsy=cp/(hl*epsfac)
       rbyg=gasr/grav*delsig(1)*0.5e0
       r100=100.0e0 /gasr
       DO i=1,nmax
          psy   (i)=cpsy*psur(i)
          bps   (i)=sigki(1)
          psb   (i)=psur(i)*delsig(1)
          em    (i)=qm(i)*psur(i)/(epsfac+qm(i))
          dzm   (i)=rbyg*tm(i)
          rhoair(i)=r100*psur(i)/tm(i)
          rcp   (i)=cp*rhoair(i)
       END DO
       ncount=0
       DO i=1,ncols
          IF(imask(i).GE.1) THEN
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
          END IF
       END DO
    END IF
    !
    !     surface albedo (vis/nir and beam/diffuse)
    !     extinction coefficients
    !
    IF(nmax.GE.1) THEN
       CALL radalb ( &
            ncols ,month2   ,nmax  ,itype ,vcover,tc    , &
            tg    ,capac ,satcap,extk  ,radfac,closs ,gloss ,thermk,p1f   , &
            p2f   ,zlwup ,salb  ,tgeff ,cosz  ,nsx  ,  zlt2   )
    END IF
    ncount=0
    DO i=1,ncols
       IF(imask(i).GE.1) THEN
          ncount=ncount+1
          avisb(i)=salb(ncount,1,1)
          avisd(i)=salb(ncount,1,2)
          anirb(i)=salb(ncount,2,1)
          anird(i)=salb(ncount,2,2)
       ELSE IF(ABS(tsea(i)).GE.271.16e0 +0.01e0) THEN
          f=MAX(zenith(i),0.0e0 )
          ocealb=0.12347e0 +f*(0.34667e0+f*(-1.7485e0 + &
               f*(2.04630e0 -0.74839e0 *f)))
          avisb(i)=ocealb
          avisd(i)=oceald
          anirb(i)=ocealb
          anird(i)=oceald
       ELSE
          avisb(i)=icealv
          avisd(i)=icealv
          anirb(i)=icealn
          anird(i)=icealn
       END IF
       tsurf(i)=ABS(tsea(i))
       qsurf(i)=0.622e0*EXP(21.65605e0 -5418.0e0 /tsurf(i))/gps(i)
    END DO
    !
    !     sw/ir heating/cooling rate
    !     downward shortwave radiation at the surface
    !     downward  longwave radiation at the surface
    !
    jlat  = latco
    DO k=1,kmax
       DO i=1,ncols
          gtmp(i,k)=gtt(i,k)
       END DO
    END DO
    DO k=1,kmax
       DO i=1,ncols
          gwv (i,k)=gqq(i,k)
       END DO
    END DO
    DO i=1,ncols
       pcb(i)=0.1*gps(i)
    END DO
    CALL rqvirt(pcb   ,gwv   ,gtmp  ,q     ,relhum,sig   ,ncols , &
         kmax  )
    !
    !     virtual temperature correction for temperature used in radiation
    !     and setting minimum specific humidity
    !
    !     pcb     surface pressure in cb
    !     tsurf   earth's surface temperature used for radiation
    !     for the first time step when ground temperature is not yet
    !     computed (this is done by subr.tsinit ),
    !     gt(k=1)(with some correction below) is used.
    !
    tice01=tice+0.01
    DO i=1,ncols
       IF(initlz.GE.0.AND.kt.EQ.0.AND.ktm.EQ.-1) THEN
          IF(tsea(i).GT.0.0e0.OR. &
               (tsea(i).LT.0.0e0.AND.ABS(tsea(i)).LT.tice01) ) THEN
             tsurf(i)=gt(i,1)
          END IF
          IF(tsea(i).GT.0.0e0.AND.sheleg(i).GT.0.0e0) THEN
             tsurf(i)=MIN(tf  ,tsurf(i))
          END IF
          IF(tsea(i).LT.0.0e0.AND.ABS(tsea(i)).LT.tice01) THEN
             tsurf(i)=MIN(tice,tsurf(i))
          END IF
       END IF
       qsurf(i)=0.622e0*EXP(21.65605e0 -5418.0e0 /tsurf(i))/gps(i)
    END DO
    !
    !     inalb=0:albedo is calculated outside the subr.spmrad.
    !     inalb=2:albedo is calculated outside the subr.spmrad.
    !
    inalb=2
    IF(irad.NE.'YES ')go to 608
    icld=1
    IF(iccon.EQ.'ARA ')icld=3
    CALL spmrad (&
         idatec, latco , colrad, gps   , gtmp  , q     , tsurf , relhum,&
         omg   , clr   , htr   , avisd , anird , avisb , anirb , dlwbot,&
         ulwtop, dswtop, sig   , sigml , rvisd , rnird , rvisb , rnirb ,&
         inalb , trint , swint , ultclr, rsclr , dlwclr, htrc  , rvisdc,&
         rnirdc, rvisbc, rnirbc, yvisdc, ynirdc, yvisbc, ynirbc, icld  ,&
         zenith, yvisd , ynird , yvisb , ynirb , yswtop, xvisdc, xnirdc,&
         xvisbc, xnirbc, xvisd , xnird , xvisb , xnirb , cldsav, rs    ,&
         ifday , tod   , idate , cldtot, cldinv, cldsat, cldcon, cldson,&
         clwd  , emisd , taud  , tsea  , yrl   , convts, convcs,&
         convbs, mxrdcc, lcnvl , lthncl, convc , convt , convb , first ,&
         dodia , co2val, delt  , nfin0 ,&
         nfin1 , nfcnv0, nls   , nlcs  , ncols , jmax  , kmax  , lonrad,&
         cos2d , intcosz)
    !
    !     save cloud radiation fields from initial spmrad call for 
    !     future inclusion into diagnostics accumulators
    !
    IF(cloudr.AND.jdt.EQ.1.AND.ktm.EQ.-1.AND.kt.EQ.0)THEN
       WRITE(nfcldr,rec=latco)cldtot,cldinv,cldsat,cldcon,cldson,clwd, &
            emisd,taud
    END IF
    !
    !     retrieve cloud radiation fields from initial spmrad call for
    !     inclusion into diagnostics accumulators
    !
    IF(cloudr.AND.jdt.EQ.2.AND.ktm.NE.-1.AND.kt.EQ.0)THEN
       READ(nfcldr,rec=latco)cldtot,cldinv,cldsat,cldcon,cldson,clwd, &
            emisd,taud
    END IF
    IF(ghl_local) THEN
       IF (dogrh(nGHis_vdtclc,latco)) CALL StoreGridHistory (cldtot, nGHis_vdtclc, latco)
    END IF

    IF(cloudr.AND.(ABS( MOD((tod-delt)/3600.0+0.03125e0,swint)) &
         .LE.0.0625e0.OR.(jdt.EQ.2.AND.ktm.NE.-1.AND.kt.EQ.0)))THEN
       !
       !     radint is the number of time steps between short wave radiation
       !     calculation, hence the cloud radiation interval
       !
       radint=swint*3600.0/delt
       !
       !     use relhum as scratch pad
       !
       IF(dodia(nDiag_vdtclc))THEN
          DO k=1,kmax
             DO i=1,ncols
                relhum(i,k)=radint*cldtot(i,k)
             END DO
          END DO
          CALL updia(relhum,nDiag_vdtclc,latco)
       END IF
       IF(dodia(nDiag_invcld))THEN
          DO k=1,kmax
             DO i=1,ncols
                relhum(i,k)=radint*cldinv(i,k)
             END DO
          END DO
          CALL updia(relhum,nDiag_invcld,latco)
       END IF
       IF(dodia(nDiag_ssatcl))THEN
          DO k=1,kmax
             DO i=1,ncols
                relhum(i,k)=radint*cldsat(i,k)
             END DO
          END DO
          CALL updia(relhum,nDiag_ssatcl,latco)
       END IF

       IF(dodia(nDiag_cnvcld))THEN
          DO k=1,kmax
             DO i=1,ncols
                relhum(i,k)=radint*cldcon(i,k)
             END DO
          END DO
          CALL updia(relhum,nDiag_cnvcld,latco)
       END IF
       IF(dodia(nDiag_shcvcl))THEN
          DO k=1,kmax
             DO i=1,ncols
                relhum(i,k)=radint*cldson(i,k)
             END DO
          END DO
          CALL updia(relhum,nDiag_shcvcl,latco)
       END IF
       IF(dodia(nDiag_clliwp))THEN
          DO k=1,kmax
             DO i=1,ncols
                relhum(i,k)=radint*clwd(i,k)
             END DO
          END DO
          CALL updia(relhum,nDiag_clliwp,latco)
       END IF
       IF(dodia(nDiag_sclopd))THEN
          DO k=1,kmax
             DO i=1,ncols
                relhum(i,k)=radint*taud(i,k)
             END DO
          END DO
          CALL updia(relhum,nDiag_sclopd,latco)
       END IF
    END IF
    IF(cloudr.AND.(ABS( MOD((tod-delt)/3600.0+0.03125e0,trint)) &
         .LE.0.0625e0.OR.(jdt.EQ.2.AND.ktm.NE.-1.AND.kt.EQ.0)))THEN
       !
       !     radint is the number of time steps between long wave radiation
       !     calculation, hence the cloud radiation interval
       !
       radint=trint*3600.0/delt
       !
       !     use relhum as scratch pad
       !
       IF(dodia(nDiag_lwcemi))THEN
          DO k=1,kmax
             DO i=1,ncols
                relhum(i,k)=radint*emisd(i,k)
             END DO
          END DO
          CALL updia(relhum,nDiag_lwcemi,latco)
       END IF
    END IF
    !
    !     td  initialization
    !     there are two problems of this scheme
    !     over antactica tg seems to be too high compared with control tg0
    !     (or contol tg0 may be too low)
    !     in   siberia   td seems to be too low
    !     modification is recommended
    !
    IF(initlz.GE.0.AND.ktm.EQ.-1.AND.kt.EQ.0.AND.nmax.GE.001)THEN
       DO ind=1,2
          ncount=0
          DO i=1,ncols
             IF(imask(i).GE.1) THEN
                ncount=ncount+1
                IF(ind.EQ.1) THEN
                   !
                   !     night
                   !
                   radn(ncount,1,1)=0.0e0
                   radn(ncount,1,2)=0.0e0
                   radn(ncount,2,1)=0.0e0
                   radn(ncount,2,2)=0.0e0
                   cosz(ncount)    =0.0e0
                ELSE
                   !
                   !     noon
                   !
                   radn(ncount,1,1)=xvisb (i)
                   radn(ncount,1,2)=xvisd (i)
                   radn(ncount,2,1)=xnirb (i)
                   radn(ncount,2,2)=xnird (i)
                   cosz(ncount)    =cos2(i)
                END IF
                radn(ncount,3,1)=0.0e0
                radn(ncount,3,2)=dlwbot(i)
                !
                !     precipitation
                !
                ppl (ncount)    =0.0e0
                ppc (ncount)    =0.0e0
             END IF
          END DO
          DO itr=1,5
             CALL radalb( &
                  ncols ,month2   ,nmax  ,itype ,vcover,tc    , &
                  tg    ,capac ,satcap,extk  ,radfac,closs ,gloss ,thermk,p1f   , &
                  p2f   ,zlwup ,salb  ,tgeff ,cosz  ,nsx  , zlt2)


             CALL fysiks(vcover, z0x  , d    , rdc  , rbc  , z0   , ndt  , &
                  latitu, bps  ,psb   ,dzm   ,em    ,gmt   ,gmq   , &
                  gmu   ,cu    ,cuni  ,ctni  ,ustar ,cosz  ,sinclt2,rhoair, &
                  psy   ,rcp   ,wc    ,wg    ,fc    ,fg    ,hr    , ect  , &
                  eci   , egt  , egi  , egs  , ec   , eg   , hc   , hg   , &
                  ecidif,egidif,ecmass,egmass,etmass, hflux, chf  , shf  , &
                  fluxef, roff , drag ,ra    , rb   , rd   , rc   , rg   , &
                  tcta  , tgta , ta   , ea   , etc  , etg  , btc  , btg  , &
                  u2    , radt , par  , pd   , rst  ,rsoil ,phroot, hrr  , &
                  phsoil, cc   , cg   ,satcap, snow , dtc  , dtg  , dtm  , &
                  dqm   , stm  , extk ,radfac, closs,gloss ,thermk, p1f  , &
                  p2f   , tc   , tg   , td   , capac, w    ,  qm  , tm   , &
                  um    , vm   , psur , ppc  , ppl  , radn ,itype , dtc3x, &
                  month2, nmax , ncols,zlt2  ,green2,chil2 ,rstpar2,topt2, &
                  tll2  ,tu2   , defac2,ph12  ,ph22 ,xcover,zlt)


             ncount=0
             DO i=1,ncols
                IF(imask(i).GE.1) THEN
                   ncount=ncount+1
                   tm (ncount  )=gt  (i,1)
                   qm (ncount  )=gq  (i,1)
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
                END IF
             END DO
          END DO
          DO i=1,nmax
             capac(i,1)=capacm(i,1)
             capac(i,2)=capacm(i,2)
             w    (i,1)=wm(i,1)
             w    (i,2)=wm(i,2)
             w    (i,3)=wm(i,3)
             td   (i)  =tdm(i)
             tc   (i)  =tcm(i)
             IF(ind.EQ.1) THEN
                tmin (i) =tg (i)
             ELSE
                tmax (i) =tg (i)
             END IF
             tg   (i) =tgm(i)
          END DO
       END DO
       DO i=1,nmax
          td   (i) =0.9*0.5*(tmax(i)+tmin(i))+0.1*tdm(i)
          tdm  (i) =td(i)
          td0  (i) =td(i)
       END DO
       !
       !     this is a start of equilibrium tg,tc comp.
       !     
       ncount=0
       DO i=1,ncols
          IF(imask(i).GE.1) THEN
             ncount=ncount+1
             cosz(ncount)    =zenith(i)
          END IF
       END DO
       DO i=1,nmax
          IF(cosz(i).LT.0.0e0) THEN
             tgm  (i)  =tmin(i)
             tg0  (i)  =tmin(i)
          END IF
       END DO
       CALL radalb ( &
            ncols ,month2,nmax  ,itype ,vcover,tc    , &
            tg    ,capac ,satcap,extk  ,radfac,closs ,gloss ,thermk,p1f   , &
            p2f   ,zlwup ,salb  ,tgeff ,cosz  ,nsx   ,zlt2)
    END IF
608 CONTINUE
    !
    !     this is for radiation interpolation
    !
    intstp=3600.0*swint/delt+0.01
    modstp=MOD(jdt-1,intstp)
    fstp  =float(modstp)/float(intstp)
    IF(jdt.LE.2.AND.kt.EQ.0) fstp=0.0
    fstp1 =1.0-fstp
    DO i=1,ncols
       xvisd (i)=fstp1*rvisd (i)+fstp*yvisd (i)
       xvisb (i)=fstp1*rvisb (i)+fstp*yvisb (i)
       xnird (i)=fstp1*rnird (i)+fstp*ynird (i)
       xnirb (i)=fstp1*rnirb (i)+fstp*ynirb (i)
       xvisdc(i)=fstp1*rvisdc(i)+fstp*yvisdc(i)
       xvisbc(i)=fstp1*rvisbc(i)+fstp*yvisbc(i)
       xnirdc(i)=fstp1*rnirdc(i)+fstp*ynirdc(i)
       xnirbc(i)=fstp1*rnirbc(i)+fstp*ynirbc(i)
       xswtop(i)=fstp1*dswtop(i)+fstp*yswtop(i)
    END DO
    IF(modstp.EQ.intstp-1) THEN
       DO i=1,ncols
          rvisd (i)=yvisd (i)
          rvisb (i)=yvisb (i)
          rnird (i)=ynird (i)
          rnirb (i)=ynirb (i)
          rvisdc(i)=yvisdc(i)
          rvisbc(i)=yvisbc(i)
          rnirdc(i)=ynirdc(i)
          rnirbc(i)=ynirbc(i)
          dswtop(i)=yswtop(i)
       END DO
    END IF
    IF(nmax.GE.1) THEN
       ncount=0
       DO i=1,ncols
          IF(imask(i).GE.1) THEN
             ncount=ncount+1
             !
             !     this is for radiation interpolation
             !
             IF(cosz(ncount).GE.0.01746e0 ) THEN
                radn(ncount,1,1)=xvisb (i)
                radn(ncount,1,2)=xvisd (i)
                radn(ncount,2,1)=xnirb (i)
                radn(ncount,2,2)=xnird (i)
             ELSE
                radn(ncount,1,1)=0.0e0
                radn(ncount,1,2)=0.0e0
                radn(ncount,2,1)=0.0e0
                radn(ncount,2,2)=0.0e0
             END IF
             radn(ncount,3,1)=0.0e0
             radn(ncount,3,2)=dlwbot(i)
             !
             !     precipitation
             !
             ppl (ncount)    =ppli  (i)
             ppc (ncount)    =ppci  (i)
          END IF
       END DO
    END IF
    !
    !     surface parameterization
    !     simple biosphere parameterization
    !
    IF(nmax.GE.1) THEN

       CALL fysiks(vcover, z0x  , d    , rdc  , rbc  , z0   ,ndt  , &
            latitu, bps  ,psb   ,dzm   ,em    ,gmt   ,gmq   , &
            gmu   ,cu    ,cuni  ,ctni  ,ustar ,cosz  ,sinclt2,rhoair, &
            psy   ,rcp   ,wc    ,wg    ,fc    ,fg    ,hr    , ect  , &
            eci   , egt  , egi  , egs  , ec   , eg   , hc   , hg   , &
            ecidif,egidif,ecmass,egmass,etmass,hflux , chf  , shf  , &
            fluxef, roff , drag ,ra    , rb   , rd   , rc   , rg   , &
            tcta  , tgta , ta   , ea   , etc  , etg  , btc  , btg  , &
            u2    , radt , par  , pd   , rst  ,rsoil ,phroot, hrr  , &
            phsoil, cc   , cg   ,satcap, snow , dtc  , dtg  , dtm  , &
            dqm   , stm  , extk ,radfac, closs,gloss ,thermk, p1f  , &
            p2f   , tc   , tg   , td   , capac, w    , qm   , tm   , &
            um    , vm   , psur , ppc  , ppl  , radn ,itype , dtc3x, &
            month2, nmax , ncols,zlt2  ,green2,chil2 ,rstpar2,topt2, &
            tll2  ,tu2   , defac2,ph12  ,ph22 ,xcover,zlt)

       ncount=0
       DO i=1,ncols
          IF(imask(i).GE.1) THEN
             ncount=ncount+1
             tmtx(i,1,3)=gmt(ncount,3)
             qmtx(i,1,3)=gmq(ncount,3)
             umtx(i,1,3)=gmu(ncount,3)
             umtx(i,1,4)=gmu(ncount,4)
             tsea(i)    =tgeff(ncount)
          END IF
       END DO
    END IF
    !
    !     sea or sea ice
    !
    ! mmax=ncols-nmax+1
    ! including case 1D physics
    IF (Model1D) THEN
       mmax=MAX(1,ncols-nmax)
    ELSE
       mmax=ncols-nmax
    END IF
    IF(mmax.GE.1) THEN
       DO i=1,ncols
          !
          !     this is for radiation interpolation
          !
          slrad(i)=dlwbot(i)+(1.0-avisd(i))*xvisd(i) &
               +(1.0-avisb(i))*xvisb(i) &
               +(1.0-anird(i))*xnird(i) &
               +(1.0-anirb(i))*xnirb(i)
          slrad(i)=-14.3353e-04*slrad(i)
          xsea (i)=        tseam(i)
       END DO

       CALL seasfc( &
            tmtx  ,umtx  ,qmtx  ,kmax  ,kmax  ,slrad ,tsurf ,qsurf , &
            gu    ,gv    ,gt    ,gq    ,gps   ,xsea  ,dtc3x ,SIN(colrad), &
            sigki ,delsig,sens  ,evap  ,umom  ,vmom  ,rmi   ,rhi   , &
            cond  ,stor  ,zorl  ,rnet  ,ncols)

       DO i=1,ncols
          IF(tsea(i).LE.0.0e0.AND.tsurf(i).LT.tice+0.01e0 ) THEN
             IF(intg.EQ.2) THEN
                IF(istrt.EQ.0) THEN
                   tseam(i)=filta*tsea(i)+epsflt*(tseam(i)+xsea(i))
                END IF
                tsea (i)=      xsea(i)
             ELSE
                tsea (i)=xsea(i)
                tseam(i)=xsea(i)
             END IF
          END IF
          IF(tsea(i).LT.0.0e0.AND.tsurf(i).GE.tice+0.01e0) THEN
             tseam(i) = tsea(i)
          ENDIF
       END DO
    END IF
    !
    !     yamada-mellor pbl parameterization
    !     ( solving the matrices from bottom to top )
    !     
    CALL ympbl1 (tmtx,qmtx,umtx,ncols,kmax,kmax)

    DO k=1,kmax
       DO i=1,ncols
          !gyu(i,k)=gyu(i,k)-umtx(i,k,4)
          !gyv(i,k)=gyv(i,k)+umtx(i,k,3)     
          gyu(i,k)  = gyu(i,k) + umtx(i,k,3)
          gyv(i,k)  = gyv(i,k) + umtx(i,k,4)
       END DO
    END DO

    DO k=1,kmax
       DO i=1,ncols
          gqd(i,k)=gqd(i,k)+qmtx(i,k,3)
       END DO
    END DO
    !     
    !     tmtx(i,k,3) virtual temperature tendency due to vertical diffusion
    !
    DO k=1,kmax
       DO i=1,ncols
          tmtx(i,k,3) = (1.0 + 0.608 * gq(i,k)) * tmtx(i,k,3) + &
               0.608 * gt(i,k) * qmtx(i,k,3)
          gtd(i,k)=gtd(i,k) + tmtx(i,k,3)
          gtd(i,k)=gtd(i,k) + (1.0+0.608*gq(i,k)) * (htr(i,k)+clr(i,k))
       END DO
    END DO
    IF(kmax.LT.kmax) THEN
       DO k=kmax+1,kmax
          DO i=1,ncols
             gtd(i,k)=gtd(i,k) + htr(i,k)+clr(i,k)
             gtd(i,k)=gtd(i,k) + tmtx(i,k,3)
          END DO
       END DO
    END IF
    !
    !     pointwise diagnostics
    !
    ncount=0
    DO i=1,ncols
       IF(imask(i).GE.1) THEN
          ncount=ncount+1
          fmom= rhoair(ncount)*cu(ncount)*ustar(ncount)
          umom(i)=fmom*um(ncount)
          vmom(i)=fmom*vm(ncount)
          strcut=10.0
          umom(i)=MIN(strcut,umom(i))
          vmom(i)=MIN(strcut,vmom(i))
          umom(i)=MAX(-strcut,umom(i))
          vmom(i)=MAX(-strcut,vmom(i))
       END IF
    END DO
    IF (ghl_local) THEN
       IF (dogrh(nGHis_lwheat,latco)) CALL StoreGridHistory(   clr,nGHis_lwheat,latco)
       IF (dogrh(nGHis_swheat,latco)) CALL StoreGridHistory(   htr,nGHis_swheat,latco)
       IF (dogrh(nGHis_lwutop,latco)) CALL StoreGridHistory(ulwtop,nGHis_lwutop,latco)
       IF (dogrh(nGHis_lwdbot,latco)) CALL StoreGridHistory(dlwbot,nGHis_lwdbot,latco)
       IF (dogrh(nGHis_coszen,latco)) CALL StoreGridHistory(zenith,nGHis_coszen,latco)
       IF (dogrh(nGHis_swdbot,latco)) CALL StoreGridHistory (xswtop,nGHis_swdbot,latco)
       IF (dogrh(nGHis_swdbvb,latco)) CALL StoreGridHistory( xvisb,nGHis_swdbvb,latco)
       IF (dogrh(nGHis_swdbvd,latco)) CALL StoreGridHistory( xvisd,nGHis_swdbvd,latco)
       IF (dogrh(nGHis_swdbnb,latco)) CALL StoreGridHistory( xnirb,nGHis_swdbnb,latco)
       IF (dogrh(nGHis_swdbnd,latco)) CALL StoreGridHistory( xnird,nGHis_swdbnd,latco)
       IF (dogrh(nGHis_vibalb,latco)) CALL StoreGridHistory( avisb,nGHis_vibalb,latco)
       IF (dogrh(nGHis_vidalb,latco)) CALL StoreGridHistory( avisd,nGHis_vidalb,latco)
       IF (dogrh(nGHis_nibalb,latco)) CALL StoreGridHistory( anirb,nGHis_nibalb,latco)
       IF (dogrh(nGHis_nidalb,latco)) CALL StoreGridHistory( anird,nGHis_nidalb,latco)
       IF (dogrh(nGHis_hcseai,latco)) CALL StoreGridHistory(  cond,nGHis_hcseai,latco)
       IF (dogrh(nGHis_hsseai,latco)) CALL StoreGridHistory(  stor,nGHis_hsseai,latco)
    END IF
    !
    !     diagnostics of radiation
    !
    IF( (kt.NE.0) .OR. (jdt.NE.1) ) THEN
       IF(dodia(nDiag_lwheat)) CALL updia(clr,nDiag_lwheat,latco)

       IF(dodia(nDiag_swheat)) CALL updia(htr,nDiag_swheat,latco)

       IF(dodia(nDiag_cloudc)) CALL updia(cldsav,nDiag_cloudc,latco)

       IF (ghl_local) THEN
          IF(dogrh(nGHis_cloudc,latco)) CALL StoreGridHistory(cldsav, nGHis_cloudc, latco)
       END IF

       uswtop=0.0
       uswtpc=0.0

       DO k=1,kmax
          rfac=100.0*cp*delsig(k)/grav
          DO i=1,ncols
             uswtop(i)=uswtop(i)+rfac*gps(i)*htr(i,k)
             uswtpc(i)=uswtpc(i)+rfac*gps(i)*htrc(i,k)
          END DO
       END DO
       DO i=1,ncols
          uswtop(i)=xswtop(i) &
               -(1.0-avisd(i))*xvisd(i) &
               -(1.0-avisb(i))*xvisb(i) &
               -(1.0-anird(i))*xnird(i) &
               -(1.0-anirb(i))*xnirb(i) &
               -uswtop(i)
          uswtpc(i)=xswtop(i) &
               -(1.0-avisd(i))*xvisdc(i) &
               -(1.0-avisb(i))*xvisbc(i) &
               -(1.0-anird(i))*xnirdc(i) &
               -(1.0-anirb(i))*xnirbc(i) &
               -uswtpc(i)
       END DO
       IF(dodia(nDiag_swabea))THEN
          DO i=1,ncols
             bfr1(i)=xswtop(i)-uswtop(i)
          END DO
          CALL updia(bfr1,nDiag_swabea,latco)
       END IF
       IF(dodia(nDiag_swaeac))THEN
          DO i=1,ncols
             bfr1(i)=xswtop(i)-uswtpc(i)
          END DO
          CALL updia(bfr1,nDiag_swaeac,latco)

       END IF
       IF(dodia(nDiag_lwdbot)) CALL updia(dlwbot,nDiag_lwdbot,latco)

       IF(dodia(nDiag_lwdbtc)) CALL updia(dlwclr,nDiag_lwdbtc,latco)

       IF(dodia(nDiag_lwubot))THEN
          DO i=1,ncols
             bfr1(i)=stefan*tsurf(i)**4
          END DO
          CALL updia(bfr1,nDiag_lwubot,latco)

       END IF
       IF(dodia(nDiag_lwutop)) CALL updia(ulwtop,nDiag_lwutop,latco)

       IF(dodia(nDiag_lwutpc)) CALL updia(ultclr,nDiag_lwutpc,latco)

       IF(dodia(nDiag_lwnetb)) CALL updia(rs,    nDiag_lwnetb,latco)

       IF(dodia(nDiag_lwnbtc)) CALL updia(rsclr, nDiag_lwnbtc,latco)

       IF(dodia(nDiag_swdbot))THEN
          DO i=1,ncols
             bfr1(i)=xvisd(i)+xvisb(i)+xnird(i)+xnirb(i)
          END DO
          CALL updia(bfr1,nDiag_swdbot,latco)

       END IF
       IF(dodia(nDiag_swdbtc))THEN
          DO i=1,ncols
             bfr1(i)=xvisdc(i)+xvisbc(i)+xnirdc(i)+xnirbc(i)
          END DO
          CALL updia(bfr1,nDiag_swdbtc,latco)
       END IF
       IF(dodia(nDiag_swabgr))THEN
          DO i=1,ncols
             bfr1(i)=(1.0-avisd(i))*xvisd(i)+(1.0-avisb(i))*xvisb(i) &
                  +(1.0-anird(i))*xnird(i)+(1.0-anirb(i))*xnirb(i) 
          END DO
          CALL updia(bfr1,nDiag_swabgr,latco)

       END IF
       IF(dodia(nDiag_swabgc))THEN
          DO i=1,ncols
             bfr1(i)=(1.0-avisd(i))*xvisdc(i)+(1.0-avisb(i))*xvisbc(i) &
                  +(1.0-anird(i))*xnirdc(i)+(1.0-anirb(i))*xnirbc(i)
          END DO
          CALL updia(bfr1,nDiag_swabgc,latco)
       END IF
       IF(dodia(nDiag_swubot))THEN
          DO i=1,ncols
             bfr1(i)=avisd(i)*xvisd(i)+avisb(i)*xvisb(i)+ &
                  anird(i)*xnird(i)+anirb(i)*xnirb(i)
          END DO
          CALL updia(bfr1,nDiag_swubot,latco)
       END IF
       IF(dodia(nDiag_swubtc))THEN
          DO i=1,ncols
             bfr1(i)=avisd(i)*xvisdc(i)+avisb(i)*xvisbc(i)+ &
                  anird(i)*xnirdc(i)+anirb(i)*xnirbc(i)
          END DO
          CALL updia(bfr1,nDiag_swubtc,latco)
       END IF
       IF(dodia(nDiag_swdtop)) CALL updia(xswtop,nDiag_swdtop,latco)
       IF(dodia(nDiag_swutop)) CALL updia(uswtop,nDiag_swutop,latco)
       IF(dodia(nDiag_swutpc)) CALL updia(uswtpc,nDiag_swutpc,latco)
       !
       !     diagnostics of pbl parameterization
       !
       dtc3xi=1.0/dtc3x
       IF(ghl_local)THEN
          IF(dogrh(nGHis_casrrs,latco))THEN
             ncount=0
             DO i=1,ncols
                IF(imask(i).GE.1) THEN
                   ncount=ncount+1
                   bfrg(i)=ra(ncount)
                ELSE
                   bfrg(i)=1.0/rhi(i)
                END IF
             END DO
             CALL StoreGridHistory (bfrg, nGHis_casrrs, latco)
          END IF
          IF(dogrh(nGHis_mofres,latco))THEN
             ncount=0
             DO i=1,ncols
                IF(imask(i).GE.1) THEN
                   ncount=ncount+1
                   bfrg(i)=0.0e0
                ELSE
                   bfrg(i)=1.0/rmi(i)
                END IF
             END DO
             CALL StoreGridHistory (bfrg, nGHis_mofres, latco)
          END IF

          IF(dogrh(nGHis_dragcf,latco))CALL StoreMaskedGridHistory(drag,     imask,nGHis_dragcf,latco)
          IF(dogrh(nGHis_nrdcan,latco))CALL StoreMaskedGridHistory(radt(1,1),imask,nGHis_nrdcan,latco)
          IF(dogrh(nGHis_nrdgsc,latco))CALL StoreMaskedGridHistory(radt(1,2),imask,nGHis_nrdgsc,latco)
          IF(dogrh(nGHis_cascrs,latco))CALL StoreMaskedGridHistory(rb,       imask,nGHis_cascrs,latco)
          IF(dogrh(nGHis_casgrs,latco))CALL StoreMaskedGridHistory(rd,       imask,nGHis_casgrs,latco)
          IF(dogrh(nGHis_canres,latco))CALL StoreMaskedGridHistory(rc,       imask,nGHis_canres,latco)
          IF(dogrh(nGHis_gcovrs,latco))CALL StoreMaskedGridHistory(rg,       imask,nGHis_gcovrs,latco)
          IF(dogrh(nGHis_bssfrs,latco))CALL StoreMaskedGridHistory(rsoil,    imask,nGHis_bssfrs,latco)
          IF(dogrh(nGHis_ecairs,latco))CALL StoreMaskedGridHistory(ea,       imask,nGHis_ecairs,latco)
          IF(dogrh(nGHis_tcairs,latco))CALL StoreMaskedGridHistory(ta,       imask,nGHis_tcairs,latco)
          IF(dogrh(nGHis_shfcan,latco))CALL StoreMaskedGridHistory(hc,       imask,nGHis_shfcan,latco,dtc3xi)
          IF(dogrh(nGHis_shfgnd,latco))CALL StoreMaskedGridHistory(hg,       imask,nGHis_shfgnd,latco,dtc3xi)
          IF(dogrh(nGHis_tracan,latco))CALL StoreMaskedGridHistory(ect,      imask,nGHis_tracan,latco,dtc3xi)
          IF(dogrh(nGHis_tragcv,latco))CALL StoreMaskedGridHistory(egt,      imask,nGHis_tragcv,latco,dtc3xi)
          IF(dogrh(nGHis_inlocp,latco))CALL StoreMaskedGridHistory(eci,      imask,nGHis_inlocp,latco,dtc3xi)
          IF(dogrh(nGHis_inlogc,latco))CALL StoreMaskedGridHistory(egi,      imask,nGHis_inlogc,latco,dtc3xi)
          IF(dogrh(nGHis_bsevap,latco))CALL StoreMaskedGridHistory(egs,      imask,nGHis_bsevap,latco,dtc3xi)
          IF(dogrh(nGHis_canhea,latco))CALL StoreMaskedGridHistory(chf,      imask,nGHis_canhea,latco)
          IF(dogrh(nGHis_gcheat,latco))CALL StoreMaskedGridHistory(shf,      imask,nGHis_gcheat,latco)
          IF(dogrh(nGHis_runoff,latco))CALL StoreMaskedGridHistory(roff,     imask,nGHis_runoff,latco,1000.0*dtc3xi)
       END IF
       ncount=0
       DO i=1,ncols
          IF(ghl_local .AND. dogrh(nGHis_lwubot,latco))bfrg(i)=stefan*tsurf(i)**4
          IF(dodia(nDiag_intlos))bfr1(i)=0.0
          IF(dodia(nDiag_runoff))bfr2(i)=0.0
          IF(dodia(nDiag_tcairs))swrk(i,1)=tsurf(i)
          IF(dodia(nDiag_ecairs))swrk(i,2)=0.0
          IF(dodia(nDiag_bsolht))swrk(i,3)=0.0
          IF(dodia(nDiag_cascrs))swrk(i,6)=0.0
          IF(dodia(nDiag_casgrs))swrk(i,7)=0.0
          IF(dodia(nDiag_gcovrs))swrk(i,8)=0.0
          IF(dodia(nDiag_bssfrs))swrk(i,9)=0.0
          IF(imask(i).GE.1) THEN
             ncount=ncount+1
             IF(ghl_local .AND. dogrh(nGHis_lwubot,latco))bfrg(i)=zlwup(ncount)
             sens(i)=(hc(ncount)+hg(ncount))*dtc3xi
             evap(i)=(ec(ncount)+eg(ncount))*dtc3xi
             IF(dodia(nDiag_intlos))bfr1(i)= &
                  bfr1(i)+(eci(ncount)+egi(ncount))*dtc3xi
             IF(dodia(nDiag_runoff))bfr2(i)=bfr2(i)+roff(ncount)*dtc3xi*1000.0
             IF(dodia(nDiag_tcairs))swrk(i,1)=ta(ncount)
             IF(dodia(nDiag_ecairs))swrk(i,2)=ea(ncount)
             IF(dodia(nDiag_bsolht))swrk(i,3)=egs(ncount)*dtc3xi
             IF(dodia(nDiag_mofres))swrk(i,4)=0.0
             IF(dodia(nDiag_casrrs))swrk(i,5)=ra(ncount)
             IF(dodia(nDiag_cascrs))swrk(i,6)=rb(ncount)
             IF(dodia(nDiag_casgrs))swrk(i,7)=rd(ncount)
             IF(dodia(nDiag_gcovrs))swrk(i,8)=rg(ncount)
             IF(dodia(nDiag_bssfrs))swrk(i,9)=rs(ncount)
          ELSE
             IF(ghl_local .AND. dogrh(nGHis_lwubot,latco))bfrg(i)=stefan*tsurf(i)**4
             IF(dodia(nDiag_mofres))swrk(i,4)=1.0/rmi(i)
             IF(dodia(nDiag_casrrs))swrk(i,5)=1.0/rhi(i)
          ENDIF
       END DO

       IF(dodia(nDiag_sheatf)) CALL updia(sens,nDiag_sheatf,latco)
       IF(dodia(nDiag_lheatf)) CALL updia(evap,nDiag_lheatf,latco)
       IF(dodia(nDiag_intlos)) CALL updia(bfr1,nDiag_intlos,latco)
       IF(dodia(nDiag_runoff)) CALL updia(bfr2,nDiag_runoff,latco)

       IF(dodia(nDiag_tcairs)) CALL updia(swrk(1,1),nDiag_tcairs,latco)
       IF(dodia(nDiag_ecairs)) CALL updia(swrk(1,2),nDiag_ecairs,latco)
       IF(dodia(nDiag_bsolht)) CALL updia(swrk(1,3),nDiag_bsolht,latco)
       IF(dodia(nDiag_mofres)) CALL updia(swrk(1,4),nDiag_mofres,latco)
       IF(dodia(nDiag_casrrs)) CALL updia(swrk(1,5),nDiag_casrrs,latco)
       IF(dodia(nDiag_cascrs)) CALL updia(swrk(1,6),nDiag_cascrs,latco)
       IF(dodia(nDiag_casgrs)) CALL updia(swrk(1,7),nDiag_casgrs,latco)
       IF(dodia(nDiag_gcovrs)) CALL updia(swrk(1,8),nDiag_gcovrs,latco)
       IF(dodia(nDiag_bssfrs)) CALL updia(swrk(1,9),nDiag_bssfrs,latco)

       IF (cdhl(jdt)) THEN
          DO i=1,ncols
             ustr(i)=-umom(i)
             vstr(i)=-vmom(i)
          ENDDO
       ENDIF

       IF(dodia(nDiag_ustres))THEN
          DO i=1,ncols
             bfr1(i)=-umom(i)
          END DO
          CALL updia(bfr1,nDiag_ustres,latco)
       END IF
       IF(dodia(nDiag_vstres))THEN
          DO i=1,ncols
             bfr2(i)=-vmom(i)
          END DO
          CALL updia(bfr2,nDiag_vstres,latco)
       END IF
       IF(dodia(nDiag_vdheat)) CALL updia(tmtx(1,1,3),nDiag_vdheat,latco)

       IF(dodia(nDiag_vduzon)) CALL updia(umtx(1,1,3),nDiag_vduzon,latco)

       IF(dodia(nDiag_vdvmer)) CALL updia(umtx(1,1,4),nDiag_vdvmer,latco)

       IF(dodia(nDiag_vdmois)) CALL updia(qmtx(1,1,3),nDiag_vdmois,latco)

       IF(ghl_local)THEN
          IF(dogrh(nGHis_vdheat,latco))CALL StoreGridHistory(tmtx(:,:,3),nGHis_vdheat,latco)
          IF(dogrh(nGHis_vduzon,latco))CALL StoreGridHistory(umtx(:,:,3),nGHis_vduzon,latco)
          IF(dogrh(nGHis_vdvmer,latco))CALL StoreGridHistory(umtx(:,:,4),nGHis_vdvmer,latco)
          IF(dogrh(nGHis_vdmois,latco))CALL StoreGridHistory(qmtx(:,:,3),nGHis_vdmois,latco)
          IF(dogrh(nGHis_swutop,latco))CALL StoreGridHistory(uswtop,     nGHis_swutop,latco)
          IF(dogrh(nGHis_lwubot,latco))CALL StoreGridHistory(bfrg,       nGHis_lwubot,latco)
          IF(dogrh(nGHis_ustres,latco))CALL StoreGridHistory(umom,       nGHis_ustres,latco)
          IF(dogrh(nGHis_vstres,latco))CALL StoreGridHistory(vmom,       nGHis_vstres,latco)
          IF(dogrh(nGHis_sheatf,latco))CALL StoreGridHistory(sens,       nGHis_sheatf,latco)
          IF(dogrh(nGHis_lheatf,latco))CALL StoreGridHistory(evap,       nGHis_lheatf,latco)
       END IF

       IF(dodia(nDiag_tmtsfc)) CALL updia(tsurf,nDiag_tmtsfc,latco)

    END IF
    !
    !     sib time integaration and time filter
    !
    IF(nmax.GE.1) THEN
       CALL sextrp ( &
            td    ,tg    ,tc    ,w     ,capac ,td0   ,tg0   ,tc0   ,w0    , &
            capac0,tdm   ,tgm   ,tcm   ,wm    ,capacm,istrt ,ncols ,nmax  , &
            epsflt,intg  ,latco ,imask)
       !
       !     fix soil moisture at selected locations
       !
       DO i=1,nmax
          IF(ssib(i).GT.0.0)THEN
             w0(i,1)=ssib(i)
             w0(i,2)=ssib(i)
             w0(i,3)=ssib(i) 
             wm(i,1)=ssib(i)
             wm(i,2)=ssib(i)
             wm(i,3)=ssib(i)
          END IF
       END DO
    END IF
    DO k=1,kmax
       DO i=1,ncols
          gt(i,k)=gt(i,k)*(1.0e0+0.608e0*gq(i,k))
       END DO
    END DO
  END SUBROUTINE physcs




  SUBROUTINE InitSimpPhys(colrad2D, sl)
    REAL, INTENT(IN) :: colrad2D(:,:)
    REAL, INTENT(IN) :: sl(:)
    INTEGER :: kMax, ibMax, jbMax
    INTEGER :: ib, jb, k
    REAL :: pi
    REAL, ALLOCATABLE :: tesurf(:,:)

    ibMax = SIZE(colrad2D,1)
    jbMax = SIZE(colrad2D,2)
    kMax = SIZE(sl)
    ALLOCATE(tesurf(ibMax, jbMax))
    ALLOCATE(teq(ibMax, kMax, jbMax))
    ALLOCATE(tauri(kMax))
    ALLOCATE(alfa(kMax))

    pi = 4.0*ATAN(1.0)
    DO jb = 1, jbMax
       DO ib = 1, ibMax
          tesurf(ib,jb) = tsfc0-tdelt*(SIN(.5*pi-colrad2D(ib,jb)))**2
       END DO
    END DO


    DO k = 1, kMax
       DO jb = 1, jbMax
          DO ib = 1, ibMax
             teq(ib,k,jb) = MAX(tstrat, tesurf(ib,jb) + h0*rlaps*LOG(sl(k)))
          END DO
       END DO
    END DO
    !
    !     radiative damping inverse time
    !
    DO k=1,kmax
       tauri(k) = tdampr(k)*86400.
       tauri(k) = 1./tauri(k)
    END DO
    !
    !     frictional damping inverse time
    !
    DO k=1,kmax
       alfa(k) = tdampf(k)*86400.
       alfa(k) = 1./alfa(k)
    END DO
  END SUBROUTINE InitSimpPhys



  SUBROUTINE SimpPhys(gu, gv, gtmp, gyv, gyu, gtd, ibMax, ibLim, kMax, jb)
    !
    ! simfiz :simplified physics package
    !            newtonian cooling
    !            raleigh damping
    !
    ! arguments
    !     gu:     u*cos(latitude)
    !     gv:     v*cos(latitude)
    !     gtmp:   absolute temperature
    !     gyv:    (du/dt) tendency of zonal wind * cos(latitude)
    !     gyu:    -(dv/dt) negative of tendency of v*cos(latitude)
    !
    INTEGER, INTENT(IN)    :: ibMax
    INTEGER, INTENT(IN)    :: ibLim
    INTEGER, INTENT(IN)    :: kMax
    INTEGER, INTENT(IN)    :: jb
    REAL,    INTENT(IN)    :: gu(ibMax,kMax)
    REAL,    INTENT(IN)    :: gv(ibMax,kMax)
    REAL,    INTENT(IN)    :: gtmp(ibMax,kMax)
    REAL,    INTENT(INOUT) :: gyv(ibMax,kMax)
    REAL,    INTENT(INOUT) :: gyu(ibMax,kMax)
    REAL,    INTENT(INOUT) :: gtd(ibMax,kmax)
    INTEGER :: ib, k
    !
    !     radiative damping
    !
    DO k = 1, kmax
       DO ib = 1, ibLim
          gtd(ib,k) = gtd(ib,k) + tauri(k) * (teq(ib,k,jb)-(tov(k)+gtmp(ib,k)))
       END DO
    END DO
    !
    !     raleigh friction: zonal momentum equation
    !
    DO k = 1, kmax
       DO ib = 1, ibLim
          gyu(ib,k) = gyu(ib,k) - alfa(k)*gu(ib,k)
       END DO
    END DO
    !
    !     raleigh friction: meridional momentum equation
    !
    DO k = 1, kmax
       DO ib = 1, ibLim
          gyv(ib,k) = gyv(ib,k) - alfa(k)*gv(ib,k)
       END DO
    END DO
  END SUBROUTINE SimpPhys
END MODULE PhysicsDriver
