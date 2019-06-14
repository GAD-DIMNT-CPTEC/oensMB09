!
!  $Author: panetta $
!  $Date: 2007/07/20 13:58:43 $
!  $Revision: 1.16 $
!
MODULE PhysicsDriver

  USE Constants, ONLY: &
       ityp, imon, icg, iwv, idp, ibd, &
       tov,   &  ! intent(in)
       tdelt, &  ! intent(in)
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
       icealv   ,        &
       i8       ,        &
       r8

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
       surface_driver,Phenology

  USE SFC_SSiB, ONLY: &
       radalb







  USE Diagnostics, ONLY: &
       updia, lgaus, gaus , nucf , iavrq, &
       itavl, nuavl, lvavl, dodia, inavl, &
       ixavl, iclcd, incf , ixcf , kravl, jrcf, krcf, &
       nDiag_tmtsfc, & ! time mean surface temperature
       nDiag_runoff, & ! runoff
       nDiag_intlos, & ! interception loss
       nDiag_sheatf, & ! sensible heat flux
       nDiag_lheatf, & ! latent heat flux
       nDiag_ustres, & ! surface zonal stress
       nDiag_vstres, & ! surface meridional stress
       nDiag_cloudc, & ! cloud cover
       nDiag_lwdbot, & ! longwave downward at bottom
       nDiag_lwubot, & ! longwave upward at bottom
       nDiag_lwutop, & ! longwave upward at top
       nDiag_swdtop, & ! shortwave downward at top
       nDiag_swdbot, & ! shortwave downward at ground
       nDiag_swubot, & ! shortwave upward at bottom
       nDiag_swutop, & ! shortwave upward at top
       nDiag_swabea, & ! shortwave absorbed by the earth/atmosphere
       nDiag_swabgr, & ! shortwave absorbed by the ground
       nDiag_lwnetb, & ! net longwave at bottom
       nDiag_lwheat, & ! longwave heating
       nDiag_swheat, & ! shortwave heating
       nDiag_vdheat, & ! vertical diffusion heating
       nDiag_vdmois, & ! vertical diffusion moistening
       nDiag_vduzon, & ! vertical diffusion zonal momentum change
       nDiag_vdvmer, & ! vertical diffusion meridional momentum change
       nDiag_txgwds, & ! gravity wave drag surface zonal stress
       nDiag_tygwds, & ! gravity wave drag surface meridional stress
       nDiag_gwduzc, & ! gravity wave drag zonal momentum change
       nDiag_gwdvmc, & ! gravity wave drag meridional momentum change
       nDiag_lwdbtc, & ! longwave downward at bottom (clear)
       nDiag_lwutpc, & ! longwave upward at top (clear)
       nDiag_swdbtc, & ! shortwave downward at ground (clear)
       nDiag_swubtc, & ! shortwave upward at bottom (clear)
       nDiag_swutpc, & ! shortwave upward at top (clear)
       nDiag_swaeac, & ! shortwave absorbed by the earth/atmosphere (clear)
       nDiag_swabgc, & ! shortwave absorbed by the ground (clear)
       nDiag_lwnbtc, & ! net longwave at bottom (clear)
       nDiag_tcairs, & ! temperature of canopy air space
       nDiag_ecairs, & ! vapor pressure of canopy air space
       nDiag_bsolht, & ! bare soil latent heat
       nDiag_vdtclc, & ! vertical dist total cloud cover
       nDiag_invcld, & ! inversion cloud
       nDiag_ssatcl, & ! supersaturation cloud
       nDiag_cnvcld, & ! convective cloud
       nDiag_shcvcl, & ! shallow convective cloud
       nDiag_clliwp, & ! cloud liquid water path
       nDiag_lwcemi, & ! longwave cloud emissivity
       nDiag_sclopd, & ! shortwave cloud optical depth
       nDiag_mofres, & ! momentum flux resistance
       nDiag_casrrs, & ! canopy air spc to ref. lvl resistance
       nDiag_cascrs, & ! canopy air spc to canopy resistance
       nDiag_casgrs, & ! canopy air spc to ground resistance
       nDiag_gcovrs, & ! ground cover resistance
       nDiag_bssfrs, & ! bare soil surface resistance
       nDiag_lwhtcl, & !hmjb ! longwave heating (clear)
       nDiag_swhtcl, & !hmjb ! shortwave heating (clear)
       nDiag_tep02m, & ! Temperature at 2-m from surface layer
       nDiag_mxr02m, & ! Mixing ratio at 2-m from surface layer
       nDiag_spw02m, & ! Speed wind at 2-m from surface layer
       nDiag_tep10m, & ! Temperature at 10-m from surface layer
       nDiag_mxr10m, & ! Mixing ratio at 10-m from surface layer
       nDiag_spw10m, & ! Speed wind at 10-m from surface layer
       nDiag_ozonmr, & ! ozone mass mixing ratio (g/g)
       nDiag_viozoc    ! Vertically Integrated Ozone Content (Dobson units)

  USE PlanBoundLayer, ONLY: &
       pbl_driver,          &
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
       iswrad,&
       ilwrad,&
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
       start ,&
       ifilt ,&
       Model1D, &
       dirfNameOutput, &
       schemes,&
       crdcld !hmjb

  USE Init, ONLY:  &
       nls   ,&
       nlcs


  USE FieldsPhysics, ONLY: &
       anirb, anird, avisb, avisd, botlv, capac0, capacm, cldsav, clr, convb, &
       convbs, convc, convcs, convt, convts, dlwbot, dlwclr, dswtop, evap, geshem, &
       gl0,Mmlen, gtsea, htr, htrc, imask, ppci, ppli, prcc, prcp1, prcp2, &
       prcp3, prcpt, prct, rnirb, rnirbc, rnird, rnirdc, rs, rsclr, rvisb, &
       rvisbc, rvisd, rvisdc, sens, sheleg, sigki, ssib,tm0, tc0,tmm, tcm,qm0,qmm, td0, &
       tdm, tg0, tgm, toplv, tseam, ultclr, ulwtop, ustr, uswtpc, var, &
       vstr, w0, wm, xnirb, xnirbc, xnird, xnirdc, xswtop, xvisb, xvisbc, &
       xvisd, xvisdc, ynirb, ynirbc, ynird, ynirdc, yswtop, yvisb, yvisbc, yvisd, &
       yvisdc, zorl, clrc ,cloud,xland,lowlyr,ustar,z0,tkemyj,&
       qsfcm,tsfcm,qsfc0,tsfc0,thz0,qz0,uz0,&
       vz0,znt,pblh,akhs,akms,rmol,ct,o3mix,snow,htdisp,temp2m,umes2m,uve10m,vve10m !hmjb



  USE Convection, ONLY:  &
       cumulus_driver

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: DryPhysics
  PUBLIC :: HumidPhysics
  PUBLIC :: InitSimpPhys
  PUBLIC :: SimpPhys
  REAL(KIND=r8), ALLOCATABLE :: teq(:,:,:)
  REAL(KIND=r8), ALLOCATABLE :: tauri(:)
  REAL(KIND=r8), ALLOCATABLE :: alfa(:)

CONTAINS

  SUBROUTINE DryPhysics &
       (zs   ,gt    ,gq    ,gu    ,gv    ,gps   ,gyu   ,gyv   ,gtd   , &
       gqd   ,colrad,ifday ,tod   ,gtt   ,gqq   ,omg   ,latco , &
       lonrad,glnpm ,cos2d ,intcosz   )
    REAL(KIND=r8),    INTENT(in   ) :: zs    (ibMax)
    REAL(KIND=r8),    INTENT(inout) :: gt    (ibMax,kMax)
    REAL(KIND=r8),    INTENT(inout) :: gq    (ibMax,kMax)
    REAL(KIND=r8),    INTENT(in   ) :: gu    (ibMax,kMax)
    REAL(KIND=r8),    INTENT(in   ) :: gv    (ibMax,kMax)
    REAL(KIND=r8),    INTENT(in   ) :: gps   (ibMax)
    REAL(KIND=r8),    INTENT(inout) :: gyu   (ibMax,kMax)
    REAL(KIND=r8),    INTENT(inout) :: gyv   (ibMax,kMax)
    REAL(KIND=r8),    INTENT(inout) :: gtd   (ibMax,kMax)
    REAL(KIND=r8),    INTENT(inout) :: gqd   (ibMax,kMax)
    REAL(KIND=r8),    INTENT(in   ) :: colrad(ibMax)
    REAL(KIND=r8),    INTENT(in   ) :: lonrad(ibMax)
    INTEGER, INTENT(in   ) :: ifday
    REAL(KIND=r8),    INTENT(in   ) :: tod
    REAL(KIND=r8),    INTENT(in   ) :: gtt   (ibMax,kMax)
    REAL(KIND=r8),    INTENT(in   ) :: gqq   (ibMax,kMax)
    REAL(KIND=r8),    INTENT(in   ) :: omg   (ibMax,kMax)
    INTEGER, INTENT(in   ) :: latco
    REAL(KIND=r8),    INTENT(inout) :: glnpm (ibMax)
    REAL(KIND=r8)   , INTENT(IN   ) :: cos2d (ibMax)
    LOGICAL, INTENT(IN   ) :: intcosz
    REAL(KIND=r8)                   :: ps    (ibMax)
    INTEGER                :: ibLim
    REAL(KIND=r8)    :: topog     (ibMax)
    topog    = zs/grav
    ps=glnpm
    ibLim=ibMaxPerJB(latco)
    CALL physcs  ( gt(1:ibLim,:),gq(1:ibLim,:),gu(1:ibLim,:),gv(1:ibLim,:),&
         gps(1:ibLim),tc0(1:ibLim,latco),tg0(1:ibLim,latco),               &
         td0(1:ibLim,latco),w0(1:ibLim,:,latco),capac0(1:ibLim,:,latco),   &
         tcm(1:ibLim,latco),tgm(1:ibLim,latco),tdm(1:ibLim,latco),         &
         wm(1:ibLim,:,latco),capacm(1:ibLim,:,latco),ppli(1:ibLim,latco),  &
         ppci(1:ibLim,latco),gyu(1:ibLim,:),gyv(1:ibLim,:),gtd(1:ibLim,:), &
         gqd(1:ibLim,:),htr(1:ibLim,:,latco),clr(1:ibLim,:,latco),         &
         imask(1:ibLim,latco),rvisb(1:ibLim,latco),rvisd(1:ibLim,latco),   &
         rnirb(1:ibLim,latco),rnird(1:ibLim,latco),dlwbot(1:ibLim,latco),  &
         gtsea(1:ibLim,latco),colrad(1:ibLim),sl, si(1:kMax),del, ifday,   &
         tod,avisb(1:ibLim,latco),avisd(1:ibLim,latco),                    &
         anirb(1:ibLim,latco),anird(1:ibLim,latco),dswtop(1:ibLim,latco),  &
         rs(1:ibLim,latco),ulwtop(1:ibLim,latco),gl0(1:ibLim,latco),       &
         zorl(1:ibLim,latco),gtt(1:ibLim,:),gqq(1:ibLim,:),                &
         sheleg(1:ibLim,latco),tseam(1:ibLim,latco),omg(1:ibLim,:),        &
         htrc(1:ibLim,:,latco),rvisbc(1:ibLim,latco),rvisdc(1:ibLim,latco),&
         rnirbc(1:ibLim,latco),rnirdc(1:ibLim,latco),dlwclr(1:ibLim,latco),&
         uswtpc(1:ibLim,latco),rsclr(1:ibLim,latco),ultclr(1:ibLim,latco), &
         convts(1:ibLim,latco),convcs(1:ibLim,latco),convbs(1:ibLim,latco),&
         convc(1:ibLim,latco),convt(1:ibLim,latco),convb(1:ibLim,latco),   &
         ustr(1:ibLim,latco),vstr(1:ibLim,latco),latco,                    &
         yvisb(1:ibLim,latco),yvisd(1:ibLim,latco),ynirb(1:ibLim,latco),   &
         ynird(1:ibLim,latco),yswtop(1:ibLim,latco),yvisbc(1:ibLim,latco), &
         yvisdc(1:ibLim,latco),ynirbc(1:ibLim,latco),ynirdc(1:ibLim,latco),&
         cldsav(1:ibLim,latco),ssib(1:ibLim,latco),ibMaxPerJB(latco),kMax, &
         sigki,xvisb(1:ibLim,latco),xvisd(1:ibLim,latco),                  &
         xnirb(1:ibLim,latco),xnird(1:ibLim,latco),xswtop(1:ibLim,latco),  &
         xvisbc(1:ibLim,latco),xvisdc(1:ibLim,latco),xnirbc(1:ibLim,latco),&
         xnirdc(1:ibLim,latco),lonrad(1:ibLim),ps(1:ibLim),                &
         var(1:ibLim,latco),sens(1:ibLim,latco),evap(1:ibLim,latco),       &
         cos2d(1:ibLim),intcosz ,clrc(1:ibLim,:,latco),topog(1:ibLim), &
         cloud(1:ibLim,:,:,latco), o3mix(1:ibLim,:,latco),Mmlen(1:ibLim,latco),&
         xland(1:ibLim,latco),lowlyr(1:ibLim,latco),&
         ustar(1:ibLim,latco),z0(1:ibLim,latco),tkemyj(1:ibLim,:,latco),&
         qsfcm(1:ibLim,latco),tsfcm(1:ibLim,latco),qsfc0(1:ibLim,latco),tsfc0(1:ibLim,latco),&
         thz0(1:ibLim,latco),qz0 (1:ibLim,latco),&
         uz0 (1:ibLim,latco),vz0 (1:ibLim,latco),znt (1:ibLim,latco),&
         pblh(1:ibLim,latco),akhs(1:ibLim,latco),akms(1:ibLim,latco),&
         rmol(1:ibLim,latco),CT(1:ibLim,latco),snow(1:ibLim,latco),&
         tm0(1:ibLim,latco),&
         tmm(1:ibLim,latco),qm0(1:ibLim,latco),qmm(1:ibLim,latco),&
         htdisp(1:ibLim,latco),temp2m(1:ibLim,latco),umes2m(1:ibLim,latco),&
         uve10m(1:ibLim,latco),vve10m(1:ibLim,latco))


  END SUBROUTINE DryPhysics




  SUBROUTINE HumidPhysics(latco, rqn, ftn, fqn, fpn, gu, gv, omg,     &
       gtmpm1, gtmpm2, fgqm2,fgps2, fgzs, colrad)

    ! fgqmm  -> fgqmm    time -> t-1
    ! fgtmp  -> fgtmp    time -> t+1
    ! fgq    -> fgq      time -> t+1
    ! fgps   -> fgps     time -> t+1
    ! fgumm  -> fgumm2   time -> t
    ! fgvmm  -> fgvmm2   time -> t
    ! omg    -> omg2     time -> t
    ! gtmpm  -> fgtmpmm  time -> t-1
    ! gtmpm2 -> fgtmpmm2 time -> t
    ! fgqm2  -> fgqmm2   time -> t
    ! fgps2  -> fgps     time -> t

    INTEGER, INTENT(IN   ) :: latco
    REAL(KIND=r8),    INTENT(INOUT) :: rqn  (ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: ftn  (ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: fqn  (ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: fpn  (ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gu   (ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gv   (ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: omg  (ibMax,kMax)

    !snf
    !
    REAL(KIND=r8),    INTENT(IN   ) :: gtmpm1  (ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gtmpm2  (ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: fgqm2   (ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: fgps2   (ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: fgzs    (ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: colrad  (ibMax)
    INTEGER                :: ibLim
    INTEGER                :: i
    INTEGER                :: k
    REAL(KIND=r8)    :: topog     (ibMax)
    REAL(KIND=r8)    :: StpC_Temp (ibMax,kMax)
    REAL(KIND=r8)    :: StpM_Temp (ibMax,kMax)
    REAL(KIND=r8)    :: StpC_Umes (ibMax,kMax)
    REAL(KIND=r8)    :: StpC_Uvel (ibMax,kMax)
    REAL(KIND=r8)    :: StpC_Vvel (ibMax,kMax)
    REAL(KIND=r8)    :: fac
    REAL(KIND=r8)    :: fac2
    REAL(KIND=r8)    :: fac2x
    !snf
    !-------------------------------------------------
    !t-1     StpM_Temp
    !t       StpC_Temp ,StpC_Umes ,StpC_Uvel ,StpC_Vvel ,StpC_Pslc
    !t+1     StpP_Pslc
    !-----
    ibLim=ibMaxPerJB(latco)
    topog(1:ibLim)       = fgzs(1:ibLim)/(grav)
    fac=0.5_r8
    IF(ifilt.EQ.0.AND.kt.EQ.0.AND.jdt.EQ.1) fac=0.0_r8
    fac2 =2.0_r8*fac
    fac2x=2.0_r8*fac
    IF(ifilt.EQ.0.AND.kt.EQ.0.AND.jdt.EQ.2) fac2x=2.0_r8
    DO k=1,kMax
       DO i=1,ibLim
          StpC_Uvel(i,k)  = gu(i,k) / (SIN(colrad (i)))
          StpC_Vvel(i,k)  = gv(i,k) / (SIN(colrad (i)))
          StpM_Temp(i,k)  = gtmpm1(i,k) + tov(k)
          StpC_Temp(i,k)  = gtmpm2(i,k) + tov(k)
          StpC_Umes(i,k)  = fgqm2 (i,k)
       END DO
    END DO

    CALL cumulus_driver( &
         ibMaxPerJB(latco       ) , &
         kMax                     , &
         StpM_Temp(1:ibLim,:    ) , &
         StpC_Temp(1:ibLim,:    ) , &
         ftn  (1:ibLim,:        ) , &
         rqn  (1:ibLim,:        ) , &
         StpC_Umes(1:ibLim,:    ) , &
         fqn  (1:ibLim,:        ) , &
         StpC_Uvel(1:ibLim,:    ) , &
         StpC_Vvel(1:ibLim,:    ) , &
         omg      (1:ibLim,:    ) , &
         fpn      (1:ibLim      ) , &
         fgps2    (1:ibLim      ) , &
         del                      , &
         sl                       , &
         si                       , &
         topog    (1:ibLim      ) , &
         sens     (1:ibLim,latco) , &
         evap     (1:ibLim,latco) , &
         imask    (1:ibLim,latco) , &
         latco                    , &
         fac2x                    , &
         convc    (1:ibLim,latco) , &
         convt    (1:ibLim,latco) , &
         convb    (1:ibLim,latco) , &
         prcp1    (1:ibLim,latco) , &
         prcp2    (1:ibLim,latco) , &
         prcp3    (1:ibLim,latco) , &
         prcpt    (1:ibLim,latco) , &
         toplv    (1:ibLim,latco) , &
         botlv    (1:ibLim,latco) , &
         convts   (1:ibLim,latco) , &
         convcs   (1:ibLim,latco) , &
         convbs   (1:ibLim,latco) , &
         fac2                     , &
         fac                      , &
         geshem   (1:ibLim,latco) , &
         ppli     (1:ibLim,latco) , &
         ppci     (1:ibLim,latco) , &
         prct     (1:ibLim,latco) , &
         prcc     (1:ibLim,latco) , &
         snow     (1:ibLim,latco)   )
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
       xnirdc,lonrad,ps    ,var   ,sens  ,evap  ,cos2d ,intcosz, clrc,topog , &
       cloud ,o3mix ,Mmlen ,xland ,lowlyr,ustar ,z0    ,tkemyj,qsfcm ,tsfcm , &
       qsfc0 ,tsfc0 ,thz0  ,qz0   ,uz0   ,vz0   ,znt   ,pblh  ,akhs  ,akms  , &
       rmol  ,ct    ,snow  ,tm0   ,tmm   ,qm0   ,qmm   ,htdisp,temp2m,umes2m, &
       uve10m,vve10m)

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
    ! clrc........Cooling rate due to longwave (clear) (K/s)   !hmjb
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
    ! iswrad........shortwave radiation
    !               irad = NON: excluded
    !               irad = LCH: lacis & hansen
    !               irad = CRD: clirad (chou&lee, modified by tarasova&fomin)
    !               irad = UKM: ukmet office
    ! ilwrad........longwave radiation
    !               irad = NON: excluded
    !               irad = HRS: harshvardhan
    !               irad = CRD: clirad (chou&lee, modified by tarasova&fomin)
    !               irad = UKM: ukmet office
    ! iccon.......the physical process cumulus convection(kuo)
    !               iccon = NON: excluded
    !               iccon = KUO: kuo
    !               iccon = ARA: arakawa
    !               iccon = GRE: grell ensemble
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
    ! jdt.........time step in getdia
    ! latco.......latco grid point reference the latitude
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
    ! convc.......ncols convective cloud cover in 3 hr. avrage
    ! convt.......ncols convective cloud top  (sigma layer)
    ! convb.......ncols convective cloud base (sigma layer)
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
    REAL(KIND=r8),    INTENT(INOUT) :: gt    (ncols,kmax)
    REAL(KIND=r8),    INTENT(INOUT) :: gq    (ncols,kmax)
    REAL(KIND=r8),    INTENT(IN   ) :: gu    (ncols,kmax)
    REAL(KIND=r8),    INTENT(IN   ) :: gv    (ncols,kmax)
    REAL(KIND=r8),    INTENT(IN   ) :: gps   (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: tc0   (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: tg0   (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: td0   (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: w0    (ncols,3)
    REAL(KIND=r8),    INTENT(INOUT) :: capac0(ncols,2)
    REAL(KIND=r8),    INTENT(INOUT) :: tcm   (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: tgm   (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: tdm   (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: wm    (ncols,3)
    REAL(KIND=r8),    INTENT(INOUT) :: capacm(ncols,2)
    REAL(KIND=r8),    INTENT(IN   ) :: ppli  (ncols)
    REAL(KIND=r8),    INTENT(IN   ) :: ppci  (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: gyu   (ncols,kmax)
    REAL(KIND=r8),    INTENT(INOUT) :: gyv   (ncols,kmax)
    REAL(KIND=r8),    INTENT(INOUT) :: gtd   (ncols,kmax)
    REAL(KIND=r8),    INTENT(INOUT) :: gqd   (ncols,kmax)
    REAL(KIND=r8),    INTENT(INOUT  ) :: htr   (ncols,kmax)
    REAL(KIND=r8),    INTENT(INOUT  ) :: clr   (ncols,kmax)
    INTEGER(KIND=i8), INTENT(IN   ) :: imask (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: rvisb (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: rvisd (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: rnirb (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: rnird (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: dlwbot(ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: tsea  (ncols)
    REAL(KIND=r8),    INTENT(IN   ) :: colrad(ncols)
    REAL(KIND=r8),    INTENT(IN   ) :: lonrad(ncols)
    REAL(KIND=r8),    INTENT(IN   ) :: sig   (kmax)
    REAL(KIND=r8),    INTENT(IN   ) :: sigml (kmax)
    REAL(KIND=r8),    INTENT(IN   ) :: delsig(kmax)
    INTEGER, INTENT(IN   ) :: ifday
    REAL(KIND=r8),    INTENT(IN   ) :: tod
    REAL(KIND=r8),    INTENT(INOUT  ) :: avisb (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: avisd (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: anirb (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: anird (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: dswtop(ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: rs    (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: ulwtop(ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: gl0   (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: zorl  (ncols)
    REAL(KIND=r8),    INTENT(IN   ) :: gtt   (ncols,kmax)
    REAL(KIND=r8),    INTENT(IN   ) :: gqq   (ncols,kmax)
    REAL(KIND=r8),    INTENT(IN   ) :: sheleg(ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: tseam (ncols)
    REAL(KIND=r8),    INTENT(IN   ) :: omg   (ncols,kmax)
    REAL(KIND=r8),    INTENT(INOUT  ) :: rvisbc(ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: rvisdc(ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: rnirbc(ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: rnirdc(ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: htrc  (ncols,kmax)
    REAL(KIND=r8),    INTENT(INOUT  ) :: clrc  (ncols,kmax) !hmjb
    REAL(KIND=r8),    INTENT(INOUT  ) :: dlwclr(ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: uswtpc(ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: rsclr (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: ultclr(ncols)
    REAL(KIND=r8),    INTENT(IN   ) :: ssib(ncols)
    !
    !     this is for interpolating shortwave rad at ground
    !
    REAL(KIND=r8),    INTENT(INOUT  ) :: yvisb (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: yvisd (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: ynirb (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: ynird (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: yswtop(ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: yvisbc(ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: yvisdc(ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: ynirbc(ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: ynirdc(ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: cldsav(ncols)
    !
    !     these are for monitoring of gpv in gfidi.
    !
    INTEGER, INTENT(IN   ) :: latco

    REAL(KIND=r8),    INTENT(INOUT  ) :: ustr(ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: vstr(ncols)

    REAL(KIND=r8),    INTENT(IN   ) :: convc (ncols)
    REAL(KIND=r8),    INTENT(IN   ) :: convt (ncols)
    REAL(KIND=r8),    INTENT(IN   ) :: convb (ncols)

    REAL(KIND=r8),    INTENT(IN   ) :: convts(ncols)
    REAL(KIND=r8),    INTENT(IN   ) :: convcs(ncols)
    REAL(KIND=r8),    INTENT(IN   ) :: convbs(ncols)

    REAL(KIND=r8),    INTENT(IN ) :: sigki (kmax)
    REAL(KIND=r8),    INTENT(INOUT) :: xvisb (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: xvisd (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: xnirb (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: xnird (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: xswtop(ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: xvisbc(ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: xvisdc(ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: xnirbc(ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: xnirdc(ncols)

    REAL(KIND=r8),    INTENT(INOUT) :: ps    (ncols)
    REAL(KIND=r8)   , INTENT(INOUT) :: var   (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: sens  (ncols)
    REAL(KIND=r8),    INTENT(INOUT  ) :: evap  (ncols)
    REAL(KIND=r8)   , INTENT(IN   ) :: cos2d  (ncols)
    REAL(KIND=r8),    INTENT(IN   ) :: topog(ncols)
    LOGICAL, INTENT(IN   ) :: intcosz
    REAL(KIND=r8),    INTENT(INOUT) :: cloud(ncols,kMax,8)
    REAL(KIND=r8),    INTENT(INOUT) :: o3mix(ncols,kMax)


    REAL(KIND=r8),    INTENT(INOUT) :: Mmlen(ncols)

    REAL(KIND=r8)   , PARAMETER :: alon   =  0.0_r8
    REAL(KIND=r8),    INTENT(IN   ) :: xland(ncols)
    INTEGER      ,    INTENT(IN   ) :: lowlyr(ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: ustar(ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: z0(ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: tkemyj(ncols,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: qsfc0 (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: tsfc0 (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: qsfcm (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: tsfcm (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: snow (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: thz0 (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: qz0  (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: uz0  (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: vz0  (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: znt  (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: pblh (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: akhs (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: akms (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: rmol (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: ct   (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: tm0  (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: tmm  (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: qm0  (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: qmm  (ncols)
    REAL(KIND=r8),    INTENT(INOUT) :: htdisp(ncols)
    REAL(KIND=r8),    INTENT(OUT  ) :: temp2m(ncols)
    REAL(KIND=r8),    INTENT(OUT  ) :: umes2m(ncols)
    REAL(KIND=r8),    INTENT(OUT  ) :: uve10m(ncols)
    REAL(KIND=r8),    INTENT(OUT  ) :: vve10m(ncols)
    REAL(KIND=r8) :: SICE(ncols)

    ! Dobson Unit (DU): It is assumed, that the ozone of the column in pure
    ! form is present at normal pressure (1013.15 hPa) and at 0deg C (273.15 K).
    ! The thickness of this layer is than given in "milli-centimetre" (10-5 m).
    ! For example 300 DU correspond to a layer of 3 mm thickness under the
    ! conditions given above.
    ! Conversion:
    !      dp_ozone = -rho_ozone g dz_ozone
    !      dz_ozone =  -dp_ozone/g/rho_ozone
    !                          dp_air    R_ozone * To
    !               = -o3mix * ------ * -------------- = -o3mix * dp_air/g * gm2dob
    !                            g            Po
    !
    ! But R_ozone = R*/M_ozone = 8.316963 J/mol/K /(47.998 g/mol) = 173.28 W/kg/K
    ! Therefore, the conversion constant (kg/m2 to dobson):
    !                gm2dob = 46716.3694_r8
    !
    REAL(KIND=r8)   , PARAMETER :: gm2dob = 46716.3694_r8 ! hmjb
    INTEGER       :: nmax
    INTEGER       :: icld
    INTEGER       :: inalb
    REAL(KIND=r8) :: uswtop(ncols)
    INTEGER       :: nsx
    INTEGER       :: itype(ncols)
    !
    !     prognostic variables
    !
    REAL(KIND=r8) :: tm   (ncols)
    REAL(KIND=r8) :: qm   (ncols)
    REAL(KIND=r8) :: tc   (ncols)
    REAL(KIND=r8) :: tg   (ncols)
    REAL(KIND=r8) :: td   (ncols)
    REAL(KIND=r8) :: capac(ncols,2)
    REAL(KIND=r8) :: w    (ncols,3)
    REAL(KIND=r8) :: qsfc  (ncols)
    REAL(KIND=r8) :: tsfc  (ncols)
    !
    !     variables calculated from above and ambient conditions
    !
    REAL(KIND=r8) :: satcap(ncols,icg)
    REAL(KIND=r8) :: extk  (ncols,icg,iwv,ibd)
    REAL(KIND=r8) :: radfac(ncols,icg,iwv,ibd)
    REAL(KIND=r8) :: closs (ncols)
    REAL(KIND=r8) :: gloss (ncols)
    REAL(KIND=r8) :: thermk(ncols)
    REAL(KIND=r8) :: p1f      (ncols)
    REAL(KIND=r8) :: p2f      (ncols)
    !
    !     heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2
    !
    REAL(KIND=r8) :: zlwup (ncols)
    !
    !     this is for coupling with closure turbulence model
    !
    REAL(KIND=r8)    :: salb  (ncols,2,2)
    REAL(KIND=r8)    :: tgeff (ncols)
    REAL(KIND=r8)    :: cosz  (ncols)
    REAL(KIND=r8)    :: swrk  (ncols)
    REAL(KIND=r8)    :: tmtx  (ncols,kmax,3)
    REAL(KIND=r8)    :: qmtx  (ncols,kmax,3)
    REAL(KIND=r8)    :: umtx  (ncols,kmax,4)

    REAL(KIND=r8)    :: tmsfc  (ncols,kmax,3)
    REAL(KIND=r8)    :: qmsfc  (ncols,kmax,3)
    REAL(KIND=r8)    :: umsfc  (ncols,kmax,4)

    REAL(KIND=r8)    :: bfr1  (ncols)
    REAL(KIND=r8)    :: gtmp  (ncols,kmax)
    REAL(KIND=r8)    :: gwv   (ncols,kmax)
    REAL(KIND=r8)    :: q     (ncols,kmax)
    REAL(KIND=r8)    :: pcb   (ncols)
    REAL(KIND=r8)    :: relhum(ncols,kmax)
    REAL(KIND=r8)    :: tsurf (ncols)
    REAL(KIND=r8)    :: qsurf (ncols)
    REAL(KIND=r8)    :: umom  (ncols)
    REAL(KIND=r8)    :: vmom  (ncols)
    REAL(KIND=r8)    :: slrad (ncols)
    REAL(KIND=r8)    :: zenith(ncols)

    REAL(KIND=r8)    :: cldtot(ncols,kmax)
    REAL(KIND=r8)    :: cldinv(ncols,kmax)
    REAL(KIND=r8)    :: cldsat(ncols,kmax)
    REAL(KIND=r8)    :: cldcon(ncols,kmax)
    REAL(KIND=r8)    :: cldson(ncols,kmax)
    REAL(KIND=r8)    :: clwd  (ncols,kmax)
    REAL(KIND=r8)    :: emisd (ncols,kmax)
    REAL(KIND=r8)    :: taud  (ncols,kmax)
    !
    !     the names of some variables in comphc have to be changed to
    !     avoid conflict with names in sibcon (comsibw)
    !
    !     intg=1  implicit one dt integration
    !     =2  implicit two dt leapfrog integration
    !
    LOGICAL :: cloudr
    REAL(KIND=r8)    :: sdelt
    REAL(KIND=r8)    :: ratio
    REAL(KIND=r8)    :: etime
    REAL(KIND=r8)    :: xday
    REAL(KIND=r8)    :: beam_visb (ncols)
    REAL(KIND=r8)    :: beam_visd (ncols)
    REAL(KIND=r8)    :: beam_nirb (ncols)
    REAL(KIND=r8)    :: beam_nird (ncols)

    LOGICAL :: ghl_local
    INTEGER :: k
    INTEGER :: i
    INTEGER :: ncount
    INTEGER :: jlat
    INTEGER :: intstp
    INTEGER :: modstp
    REAL(KIND=r8)    :: deltm
    REAL(KIND=r8)    :: sindel
    REAL(KIND=r8)    :: cosdel
    REAL(KIND=r8)    :: fimxi
    REAL(KIND=r8)    :: ctime
    REAL(KIND=r8)    :: cos2  (ncols)
    REAL(KIND=r8)    :: frh
    REAL(KIND=r8)    :: btime
    REAL(KIND=r8)    :: atime
    REAL(KIND=r8)    :: r100
    REAL(KIND=r8)    :: f
    REAL(KIND=r8)    :: ocealb
    REAL(KIND=r8)    :: tice01
    REAL(KIND=r8)    :: radint
    REAL(KIND=r8)    :: fstp
    REAL(KIND=r8)    :: fstp1
    REAL(KIND=r8)    :: rfac, rfac2
    INTEGER :: month   (ncols)
    INTEGER :: month2  (ncols)
    REAL(KIND=r8)    :: colrad2 (ncols)
    REAL(KIND=r8)    :: zenith1 (ncols)
    REAL(KIND=r8)    :: zenith2 (ncols)
    REAL(KIND=r8)    :: sinclt2 (ncols)
    REAL(KIND=r8)    :: cos3
    REAL(KIND=r8)    :: chug  (ncols,kmax)
    REAL(KIND=r8)    :: chvg  (ncols,kmax)
    REAL(KIND=r8)    :: xdrag (ncols)
    REAL(KIND=r8)    :: ydrag (ncols)
    REAL(KIND=r8)    :: topo (ncols)
    REAL(KIND=r8)    :: rshort(ncols)

    qmsfc = qmsfc
    umsfc = umsfc
    tmsfc = tmsfc
    tkemyj =tkemyj

    DO i=1, ncols
       znt   (i) =znt   (i)
       snow  (i) =snow  (i)
       akhs  (i) =akhs  (i)
       rfac      =lowlyr(i)
       akms  (i) =akms  (i)
       pblh  (i) =pblh  (i)
       thz0  (i) =thz0  (i)
       ct    (i) =ct    (i)
       qz0   (i) =qz0   (i)
       rmol  (i) =rmol  (i)
       vz0   (i) =vz0   (i)
       uz0   (i) =uz0   (i)
       rfac      =xland (i)
    END DO

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
    IF(dodia(nDiag_txgwds))CALL updia(xdrag,nDiag_txgwds,latco)
    IF(dodia(nDiag_tygwds))CALL updia(ydrag,nDiag_tygwds,latco)
    IF(dodia(nDiag_gwduzc))CALL updia(chug, nDiag_gwduzc,latco)
    IF(dodia(nDiag_gwdvmc))CALL updia(chvg, nDiag_gwdvmc,latco)

    cloudr=dodia(nDiag_vdtclc).OR.dodia(nDiag_invcld).OR.dodia(nDiag_ssatcl).OR. &
         dodia(nDiag_cnvcld).OR.dodia(nDiag_shcvcl).OR.dodia(nDiag_clliwp).OR. &
         dodia(nDiag_lwcemi).OR.dodia(nDiag_sclopd).OR.(ghl_local.AND.dogrh(nGHis_vdtclc,latco))

    IF(cloudr)THEN
       DO k=1,kmax
          DO i=1,ncols
             cldtot(i,k)=0.0e0_r8
             cldinv(i,k)=0.0e0_r8
             cldsat(i,k)=0.0e0_r8
             cldcon(i,k)=0.0e0_r8
             cldson(i,k)=0.0e0_r8
             clwd(i,k)=0.0e0_r8
             emisd(i,k)=0.0e0_r8
             taud(i,k)=0.0e0_r8
          END DO
       END DO
    ENDIF
    !
    dtc3x   =dt*REAL(intg,r8)
    !
    DO k=1,kmax
       DO i=1,ncols
          gq(i,k)=MAX(1.0e-12_r8,gq(i,k))
          gt(i,k)=gt(i,k)/(1.0e0_r8+0.608e0_r8*gq(i,k))
       END DO
    END DO

    IF(initlz.GE.0.AND.kt.EQ.0.AND.jdt.EQ.1) THEN
       ncount=0
       DO i=1,ncols
          tsfc0(i) = gt(i,1)
          qsfc0(i) = gq(i,1)
          tsfcm(i) = gt(i,1)
          qsfcm(i) = gq(i,1)
          IF(imask(i).GE.1_i8) THEN
             ncount=ncount+1
             tc0(ncount)=gt(i,1)
             tg0(ncount)=gt(i,1)
             tcm(ncount)=gt(i,1)
             tgm(ncount)=gt(i,1)
             tm0(ncols)=gt(i,1)
             tmm(ncols)=gt(i,1)
             qm0(ncols)=gq(i,1)
             qmm(ncols)=gq(i,1)
             IF(sheleg(i).GT.0.0e0_r8) THEN
                tg0(ncount)=MIN(tg0(ncount),tf-0.01e0_r8)
                tgm(ncount)=MIN(tgm(ncount),tf-0.01e0_r8)
             END IF
          END IF
       END DO
    END IF

    ncount=0
    DO i=1,ncols
       IF(imask(i).GE.1_i8) THEN
          ncount=ncount+1
          itype(ncount)=imask(i)
       END IF
    END DO
    nmax=ncount
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
       IF((((colrad(i)*180.0_r8)/3.1415926e0_r8)-90.0_r8)  > 0.0_r8 ) THEN
          month(i)  =  month(i) + 6
          IF(month(i).GE.13) month(i) = month(i)-12
       END IF
    END DO

    ncount=0

    DO i=1,ncols
       IF(imask(i).GE.1_i8) THEN
          ncount=ncount+1
          month2 (ncount) = month(i)
          colrad2(ncount) = colrad(i)
          sinclt2(ncount) = SIN(colrad(i))
       END IF
    END DO

    sindel = SIN(sdelt)
    cosdel = COS(sdelt)
    fimxi  = 24.0e0_r8 /360.0_r8
    ctime  = alon/15.0e0_r8
    cos2(1:ncols)   =0.0e0_r8
    cos3   =0.0e0_r8
    ncount =0
    frh=( MOD(tod+0.03125_r8,3600.0_r8)-0.03125_r8)/3600.0_r8

    DO i=1,ncols
       zenith1(i)  = sindel*COS(colrad(i))
    ENDDO

    DO i=1,ncols
       btime       = fimxi*lonrad(i)+ctime
       atime       = etime+pai12*(12.0_r8-idatec(1)-frh-btime)
       zenith2(i)  = cosdel*SIN(colrad(i))*COS(atime)
       zenith(i)   = zenith1(i) + zenith2(i)
    END DO

    IF(ncount.EQ.0) ncount=1
    IF(intcosz)THEN
       !cos2=cos2/REAL(ncount,r8)!!!!mudanca forcada
       cos2(1:ncols)=cos2d(1:ncols)
    ELSE
       cos2(1:ncols)=zenith(1:ncols)
    END IF
    !
    !     sib setting  *phenology*
    !
    IF(nmax.GE.1) THEN
       nsx=0
       CALL Phenology(latco,nCols,nmax,itype,colrad2, month2, xday, idatec,nsx)



       DO i=1,nmax
          tm      (i)    = tmm   (i)
          qm      (i)    = qmm   (i)
          td      (i)    = tdm   (i)
          tg      (i)    = tgm   (i)
          tc      (i)    = tcm   (i)
          capac   (i,1)  = capacm(i,1)
          capac   (i,2)  = capacm(i,2)
          w       (i,1)  = wm    (i,1)
          w       (i,2)  = wm    (i,2)
          w       (i,3)  = wm    (i,3)
       END DO
    END IF

    r100   = 100.0e0_r8 / gasr
    ncount = 0
    DO i=1,ncols
       IF(imask(i).GE.1_i8) THEN
          ncount=ncount+1
          cosz(ncount)=zenith(i)
          topo(i)=topog (i)
       END IF
    END DO
    !
    !     surface albedo (vis/nir and beam/diffuse)
    !     extinction coefficients
    !
    IF(nmax.GE.1) THEN
       IF(schemes==1)&
            CALL radalb ( &
            nmax              ,month2(1:nmax)      ,nmax                ,itype(1:nmax)       , &
            tc(1:nmax)        ,tg(1:nmax)          ,capac(1:nmax,:)     , &
            satcap(1:nmax,:)  ,extk(1:nmax,:,:,:)  ,radfac(1:nmax,:,:,:),closs(1:nmax)       , &
            gloss(1:nmax)     ,thermk(1:nmax)      ,p1f(1:nmax)         ,p2f(1:nmax)         , &
            zlwup(1:nmax)     ,salb(1:nmax,:,:)    ,tgeff(1:nmax)       ,cosz(1:nmax)        , &
            nsx               ,latco     )
       IF(schemes==2)PRINT*,'ERROR schemes 2'
    END IF
    ncount=0
    DO i=1,ncols
       IF(imask(i).GE.1_i8) THEN
          ncount=ncount+1
          avisb(i)=salb(ncount,1,1)
          avisd(i)=salb(ncount,1,2)
          anirb(i)=salb(ncount,2,1)
          anird(i)=salb(ncount,2,2)
       ELSE IF(ABS(tsea(i)).GE.271.16e0_r8 +0.01e0_r8) THEN
          f=MAX(zenith(i),0.0e0_r8 )
          ocealb=0.12347e0_r8 +f*(0.34667e0_r8+f*(-1.7485e0_r8 + &
               f*(2.04630e0_r8 -0.74839e0_r8 *f)))
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
       qsurf(i)=0.622e0_r8*EXP(21.65605e0_r8 -5418.0e0_r8 /tsurf(i))/gps(i)
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
       pcb(i)=0.1_r8*gps(i)! gps is in mb --->cb
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
    tice01=tice+0.01_r8
    DO i=1,ncols
       IF(initlz.GE.0.AND.kt.EQ.0.AND.ktm.EQ.-1) THEN
          IF(tsea(i).GT.0.0e0_r8.OR.(tsea(i).LT.0.0e0_r8.AND.ABS(tsea(i)).LT.tice01) ) THEN
             tsurf(i)=gt(i,1)
          END IF
          IF(tsea(i).GT.0.0e0_r8.AND.sheleg(i).GT.0.0e0_r8) THEN
             tsurf(i)=MIN(tf  ,tsurf(i))
          END IF
          IF(tsea(i).LT.0.0e0_r8.AND.ABS(tsea(i)).LT.tice01) THEN
             tsurf(i)=MIN(tice,tsurf(i))
          END IF
       END IF
       qsurf(i)=0.622e0_r8*EXP(21.65605e0_r8 -5418.0e0_r8 /tsurf(i))/gps(i)
    END DO
    !
    !     inalb=0:albedo is calculated outside the subr.spmrad.
    !     inalb=2:albedo is calculated outside the subr.spmrad.
    !
    inalb=2
    IF(TRIM(iswrad).NE.'NON'.OR.TRIM(ilwrad).NE.'NON') THEN
       icld=1
       IF(TRIM(iccon).EQ.'ARA') icld=3
       IF(TRIM(iswrad).EQ.'CRD') icld=crdcld
       CALL spmrad (&
            idatec, latco , colrad, gps   , gtmp  , q     , tsurf , relhum,&
            omg   , clr   , htr   , avisd , anird , avisb , anirb , dlwbot,&
            ulwtop, dswtop, sig   , sigml , rvisd , rnird , rvisb , rnirb ,&
            inalb , trint , swint , ultclr, rsclr , dlwclr, htrc  , rvisdc,&
            rnirdc, rvisbc, rnirbc, yvisdc, ynirdc, yvisbc, ynirbc, icld  ,&
            zenith, yvisd , ynird , yvisb , ynirb , yswtop, xvisdc, xnirdc,&
            xvisbc, xnirbc, xvisd , xnird , xvisb , xnirb , cldsav, rs    ,&
            ifday , tod   , idate , cldtot, cldinv, cldsat, cldcon, cldson,&
            clwd  , emisd , taud  , tsea  , yrl   , convts, convcs, convbs,&
            mxrdcc, lcnvl , lthncl, convc , convt , convb , first , co2val,&
            delt  , nfin0 , nfin1 , nfcnv0, nls   , nlcs  , ncols , jmax  ,&
            kmax  , lonrad, cos2d , intcosz, clrc, imask, o3mix)
       beam_visb= xvisb
       beam_visd= xvisd
       beam_nirb= xnirb
       beam_nird= xnird
       !
       !     this is for radiation interpolation
       !
       intstp=3600.0_r8*swint/delt+0.01_r8
       modstp=MOD(jdt-1,intstp)
       fstp  =REAL(modstp,r8)/REAL(intstp,r8)
       IF(jdt.LE.2.AND.kt.EQ.0) fstp=0.0_r8
       fstp1 =1.0_r8-fstp
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
       DO i=1,ncols
          !
          !     this is for radiation interpolation
          !
          slrad(i)=dlwbot(i)+(1.0_r8-avisd(i))*xvisd(i) &
               +(1.0_r8-avisb(i))*xvisb(i) &
               +(1.0_r8-anird(i))*xnird(i) &
               +(1.0_r8-anirb(i))*xnirb(i)

          slrad(i)=-14.3353e-04_r8*slrad(i)

          rshort(i)=avisd(i)*xvisd(i)+avisb(i)*xvisb(i)+ &
               anird(i)*xnird(i)+anirb(i)*xnirb(i)

       END DO
       !
       !     save cloud radiation fields from initial spmrad call for
       !     future inclusion into diagnostics accumulators
       !
       IF(cloudr.AND.jdt.EQ.1.AND.ktm.EQ.-1.AND.kt.EQ.0)THEN
          cloud(1:ncols,1:kmax,1)=cldtot(1:ncols,1:kmax)
          cloud(1:ncols,1:kmax,2)=cldinv(1:ncols,1:kmax)
          cloud(1:ncols,1:kmax,3)=cldsat(1:ncols,1:kmax)
          cloud(1:ncols,1:kmax,4)=cldcon(1:ncols,1:kmax)
          cloud(1:ncols,1:kmax,5)=cldson(1:ncols,1:kmax)
          cloud(1:ncols,1:kmax,6)=clwd  (1:ncols,1:kmax)
          cloud(1:ncols,1:kmax,7)=emisd (1:ncols,1:kmax)
          cloud(1:ncols,1:kmax,8)=taud  (1:ncols,1:kmax)
       END IF
       !
       !     retrieve cloud radiation fields from initial spmrad call for
       !     inclusion into diagnostics accumulators
       !
       IF(cloudr.AND.jdt.EQ.2.AND.ktm.NE.-1.AND.kt.EQ.0)THEN
          cldtot(1:ncols,1:kmax) = cloud(1:ncols,1:kmax,1)
          cldinv(1:ncols,1:kmax) = cloud(1:ncols,1:kmax,2)
          cldsat(1:ncols,1:kmax) = cloud(1:ncols,1:kmax,3)
          cldcon(1:ncols,1:kmax) = cloud(1:ncols,1:kmax,4)
          cldson(1:ncols,1:kmax) = cloud(1:ncols,1:kmax,5)
          clwd  (1:ncols,1:kmax) = cloud(1:ncols,1:kmax,6)
          emisd (1:ncols,1:kmax) = cloud(1:ncols,1:kmax,7)
          taud  (1:ncols,1:kmax) = cloud(1:ncols,1:kmax,8)
       END IF
       IF(ghl_local) THEN
          IF (dogrh(nGHis_vdtclc,latco)) CALL StoreGridHistory (cldtot, nGHis_vdtclc, latco)
       END IF

       IF(cloudr.AND.(ABS( MOD((tod-delt)/3600.0_r8+0.03125e0_r8,swint)) &
            .LE.0.0625e0_r8.OR.(jdt.EQ.2.AND.ktm.NE.-1.AND.kt.EQ.0)))THEN
          !
          !     radint is the number of time steps between short wave radiation
          !     calculation, hence the cloud radiation interval
          !
          radint=swint*3600.0_r8/delt
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
       IF(cloudr.AND.(ABS( MOD((tod-delt)/3600.0_r8+0.03125e0_r8,trint)) &
            .LE.0.0625e0_r8.OR.(jdt.EQ.2.AND.ktm.NE.-1.AND.kt.EQ.0)))THEN
          !
          !     radint is the number of time steps between long wave radiation
          !     calculation, hence the cloud radiation interval
          !
          radint=trint*3600.0_r8/delt
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
    END IF
    !
    !     yamada-mellor pbl parameterization
    !
    deltm =0.5e0_r8 *dtc3x
    Mmlen =gl0

    CALL pbl_driver( &
         gu    , &!INTENT(in   )
         gv    , &!INTENT(in   )
         gt    , &!INTENT(in   )
         gq    , &!INTENT(in   )
         delsig, &!INTENT(in   )
         ncols , &!INTENT(in   )
         kmax  , &!INTENT(in   )
         deltm , &!INTENT(in   )
         colrad, &!INTENT(in   )
         tmtx  , &!INTENT(inout)
         qmtx  , &!INTENT(inout)
         umtx  , &!INTENT(inout)
         gl0   , &!INTENT(inout)
         1       )!INTENT(in   )
    !
    !     surface parameterization
    !
    CALL surface_driver( &
         jdt                ,latco               , &
         cosz(1:nCols)       ,sinclt2(1:nCols)     ,satcap(1:nCols,:)  ,extk(1:nCols,:,:,:),&
         radfac(1:nCols,:,:,:),closs(1:nCols)      ,gloss(1:nCols)     ,thermk(1:nCols)   , &
         p1f(1:nCols)        ,p2f(1:nCols)         ,tc(1:nCols)        ,tg(1:nCols)       , &
         td(1:nCols)         ,capac(1:nCols,:)     ,w(1:nCols,:)       ,itype(1:nCols)    , &
         dtc3x               ,month2(1:nCols)      ,nmax               ,nCols             , &
         td0(1:nCols)        ,tg0(1:nCols)         ,tc0(1:nCols)       ,w0(1:nCols,:)     , &
         capac0(1:nCols,:)   ,tcm(1:nCols)         ,tgm(1:nCols)       ,tdm(1:nCols)      , &
         wm(1:nCols,:)       ,capacm(1:nCols,:)    ,intg               ,ssib(1:nCols)     , &
         zlwup(1:nCols)      ,nsx                  ,tgeff(1:nCols)     ,salb(1:nCols,:,:) , &
         topo(1:nCols)       ,imask(1:nCols)       ,cos2(1:nCols)     , &
         dlwbot (1:nCols)    ,beam_visb(1:nCols)   ,beam_visd(1:nCols) ,beam_nirb(1:nCols),&
         beam_nird(1:nCols)  ,zenith(1:nCols)      ,xvisb(1:nCols)     ,xvisd(1:nCols)    , &
         xnirb(1:nCols)      ,xnird (1:nCols)      ,ppli (1:nCols)     ,ppci  (1:nCols)   , &
         tmtx(1:nCols,:,:)   ,qmtx(1:nCols,:,:)    ,umtx (1:nCols,:,:) ,tsea(1:nCols)     , &
         kMax                ,slrad(1:nCols)       ,tsurf (1:nCols)    ,qsurf(1:nCols)    , &
         gt  (1:nCols,:)     ,gq (1:nCols,:)       ,gu(1:nCols,:)      ,gv (1:nCols,:)    , &
         gps  (1:nCols)      ,colrad(1:nCols)      ,sigki(1)           ,delsig(1)         , &
         sens  (1:nCols)     ,evap (1:nCols)       ,umom (1:nCols)     ,vmom (1:nCols)    , &
         zorl(1:nCols)       ,tseam(1:nCols)       ,rshort(1:nCols)    , &
         sice(1:ncols)       ,ustar(1:nCols)   ,qsfc0  (1:nCols)   ,tsfc0(1:nCols)    , &
         qsfcm  (1:nCols)    ,tsfcm(1:nCols)   ,qsfc  (1:nCols)    ,tsfc(1:nCols)  , &
         z0  (1:ncols)       ,qm0(1:nCols)   ,qm  (1:nCols)      ,qmm(1:nCols)  , &
         tm0(1:nCols)        ,tm  (1:nCols)   ,tmm(1:nCols)       ,htdisp(1:nCols)   , &
         temp2m(1:nCols)     ,umes2m(1:nCols)      ,uve10m(1:nCols)    ,vve10m(1:nCols)     )
    !
    !     yamada-mellor pbl parameterization
    !     ( solving the matrices from bottom to top )
    !
    CALL pbl_driver( &
         gu    ,gv    ,gt    ,gq    ,delsig,ncols, &
         kmax  ,deltm ,colrad,tmtx  ,qmtx  ,umtx  ,gl0 ,2  )

    DO k=1,kmax
       DO i=1,ncols
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
          tmtx(i,k,3) = (1.0_r8+ 0.608_r8 * gq(i,k)) * tmtx(i,k,3) + &
               0.608_r8 * gt(i,k) * qmtx(i,k,3)
          gtd(i,k)=gtd(i,k) + tmtx(i,k,3)
          gtd(i,k)=gtd(i,k) + (1.0_r8+0.608_r8*gq(i,k)) * (htr(i,k)+clr(i,k))
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
    END IF
    !
    !     diagnostics of radiation
    !
    IF( (kt.NE.0) .OR. (jdt.NE.1) ) THEN
       IF(dodia(nDiag_lwheat)) CALL updia(clr,nDiag_lwheat,latco)

       IF(dodia(nDiag_swheat)) CALL updia(htr,nDiag_swheat,latco)

       IF(dodia(nDiag_lwhtcl)) CALL updia(clrc,nDiag_lwhtcl,latco) !hmjb

       IF(dodia(nDiag_swhtcl)) CALL updia(htrc,nDiag_swhtcl,latco) !hmjb

       IF(dodia(nDiag_ozonmr)) CALL updia(o3mix,nDiag_ozonmr,latco) !hmjb

       IF(dodia(nDiag_cloudc)) CALL updia(cldsav,nDiag_cloudc,latco)

       IF (ghl_local) THEN
          IF(dogrh(nGHis_cloudc,latco)) CALL StoreGridHistory(cldsav, nGHis_cloudc, latco)
       END IF

       uswtop=0.0_r8
       uswtpc=0.0_r8
       bfr1=0.0_r8

       DO k=1,kmax
          rfac=cp*100.0_r8*delsig(k)/grav
          rfac2=gm2dob*100.0_r8*delsig(k)/grav
          DO i=1,ncols
             uswtop(i)=uswtop(i)+rfac*gps(i)*htr(i,k)
             uswtpc(i)=uswtpc(i)+rfac*gps(i)*htrc(i,k)
             bfr1(i)=  bfr1(i)+rfac2*gps(i)*o3mix(i,k) !hmjb integrated ozone
          END DO
       END DO

       IF(dodia(nDiag_viozoc)) CALL updia(bfr1,nDiag_viozoc,latco) !hmjb

       DO i=1,ncols
          uswtop(i)=xswtop(i) &
               -(1.0_r8-avisd(i))*xvisd(i) &
               -(1.0_r8-avisb(i))*xvisb(i) &
               -(1.0_r8-anird(i))*xnird(i) &
               -(1.0_r8-anirb(i))*xnirb(i) &
               -uswtop(i)
          uswtpc(i)=xswtop(i) &
               -(1.0_r8-avisd(i))*xvisdc(i) &
               -(1.0_r8-avisb(i))*xvisbc(i) &
               -(1.0_r8-anird(i))*xnirdc(i) &
               -(1.0_r8-anirb(i))*xnirbc(i) &
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
             bfr1(i)=(1.0_r8-avisd(i))*xvisd(i)+(1.0_r8-avisb(i))*xvisb(i) &
                  +(1.0_r8-anird(i))*xnird(i)+(1.0_r8-anirb(i))*xnirb(i)
          END DO
          CALL updia(bfr1,nDiag_swabgr,latco)
       END IF

       IF(dodia(nDiag_swabgc))THEN
          DO i=1,ncols
             bfr1(i)=(1.0_r8-avisd(i))*xvisdc(i)+(1.0_r8-avisb(i))*xvisbc(i) &
                  +(1.0_r8-anird(i))*xnirdc(i)+(1.0_r8-anirb(i))*xnirbc(i)
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

       ncount=0
       DO i=1,ncols
          IF(dodia(nDiag_bssfrs))swrk(i)=0.0_r8
          IF(imask(i).GE.1) THEN
             ncount=ncount+1
             IF(dodia(nDiag_bssfrs))swrk(i)=rs(ncount)
          ENDIF
       END DO

       IF (cdhl(jdt)) THEN
          DO i=1,ncols
             ustr(i)=-umom(i)
             vstr(i)=-vmom(i)
          ENDDO
       ENDIF

       IF(dodia(nDiag_vdheat)) CALL updia(tmtx(1:nCols,1:kMax,3),nDiag_vdheat,latco)

       IF(dodia(nDiag_vduzon)) CALL updia(umtx(1:nCols,1:kMax,3),nDiag_vduzon,latco)

       IF(dodia(nDiag_vdvmer)) CALL updia(umtx(1:nCols,1:kMax,4),nDiag_vdvmer,latco)

       IF(dodia(nDiag_vdmois)) CALL updia(qmtx(1:nCols,1:kMax,3),nDiag_vdmois,latco)

       IF(ghl_local)THEN
          IF(dogrh(nGHis_vdheat,latco))CALL StoreGridHistory(tmtx(:,:,3),nGHis_vdheat,latco)
          IF(dogrh(nGHis_vduzon,latco))CALL StoreGridHistory(umtx(:,:,3),nGHis_vduzon,latco)
          IF(dogrh(nGHis_vdvmer,latco))CALL StoreGridHistory(umtx(:,:,4),nGHis_vdvmer,latco)
          IF(dogrh(nGHis_vdmois,latco))CALL StoreGridHistory(qmtx(:,:,3),nGHis_vdmois,latco)
          IF(dogrh(nGHis_swutop,latco))CALL StoreGridHistory(uswtop,     nGHis_swutop,latco)
       END IF

       IF(dodia(nDiag_tmtsfc)) CALL updia(tsurf,nDiag_tmtsfc,latco)

    END IF

    DO k=1,kmax
       DO i=1,ncols
          gt(i,k)=gt(i,k)*(1.0e0_r8+0.608e0_r8*gq(i,k))
       END DO
    END DO
  END SUBROUTINE physcs




  SUBROUTINE InitSimpPhys(fgtmp,tequi, sl,dt)
    REAL(KIND=r8)   , INTENT(IN) :: fgtmp(:,:,:)
    REAL(KIND=r8)   , INTENT(INOUT) :: tequi(:,:,:)
    REAL(KIND=r8)   , INTENT(IN) :: sl(:)
    REAL(KIND=r8)   , INTENT(IN) :: dt
    INTEGER :: kMax, ibMax, jbMax
    INTEGER :: ib, jb, k
    INTEGER :: NstepSimpPhys=1000
    REAL(KIND=r8)    :: pi
    REAL(KIND=r8)    :: beta
    REAL(KIND=r8)    :: kappa
    REAL(KIND=r8)    :: tf
    ibMax = SIZE(fgtmp,1)
    jbMax = SIZE(fgtmp,3)
    kMax  = SIZE(sl)
    ALLOCATE(teq(ibMax, kMax, jbMax))
    ALLOCATE(tauri(kMax))
    ALLOCATE(alfa(kMax))
    pi = 4.0_r8*ATAN(1.0_r8)
    !PKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPK
    ! DO jb = 1, jbMax
    !    DO ib = 1, ibMax
    !         tesurf(ib,jb) = tsfc0-tdelt*(SIN(.5*pi-colrad2D(ib,jb)))**2
    !    END DO
    ! END DO
    ! DO k = 1, kMax
    !    DO jb = 1, jbMax
    !       DO ib = 1, ibMax
    !       teq(ib,k,jb) = MAX(tstrat, fgtmp(ib,1,jb) + h0*rlaps*LOG(sl(k)))
    !       END DO
    !    END DO
    ! END DO
    !PKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPKPK
    !
    !     radiative damping inverse time
    !
    DO k=1,kmax
       tauri(k) = tdampr(k)*86400.0_r8
       tauri(k) = 1.0_r8/tauri(k)
    END DO
    !
    !     frictional damping inverse time
    !
    DO k=1,kmax
       alfa(k) = tdampf(k)*86400.0_r8
       alfa(k) = 1.0_r8/alfa(k)
    END DO

    IF( TRIM(start) == "cold") THEN
       beta  = (1.0_r8/365.25_r8)          ! days/days
       kappa =  1.0_r8/tdampr(1)        ! days^-1
       tf    = (dt*REAL(NstepSimpPhys,r8))/86400.0_r8
       DO k = 1, kMax
          DO jb = 1, jbMax
             DO ib = 1, ibMax
                teq (ib,k,jb) = fgtmp(ib,k,jb) * ( exp(-beta) * (1.0_r8 - exp(-kappa*tf)) + exp(-kappa*tf) )
                tequi(ib,k,jb)=teq (ib,k,jb)
             END DO
          END DO
       END DO
    ELSE
       DO k = 1, kMax
          DO jb = 1, jbMax
             DO ib = 1, ibMax
                teq (ib,k,jb)=tequi(ib,k,jb)
             END DO
          END DO
       END DO
    END IF
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
    REAL(KIND=r8),    INTENT(IN)    :: gu(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN)    :: gv(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN)    :: gtmp(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: gyv(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: gyu(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: gtd(ibMax,kmax)
    INTEGER :: ib, k
    LOGICAL :: simfiz=.TRUE.

    IF (simfiz) THEN
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

    END IF

  END SUBROUTINE SimpPhys
END MODULE PhysicsDriver
