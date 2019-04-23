!
!  $Author: pkubota $
!  $Date: 2010/04/20 20:18:04 $
!  $Revision: 1.18 $
!
MODULE GridDynamics
  USE FieldsDynamics, ONLY : &
       fgtmp  ,  & ! intent(in)
       fgdivm ,  & ! intent(inout)
       fgdiv  ,  & ! intent(in)
       fgum   ,  & ! intent(inout)
       fgu    ,  & ! intent(in)
       fgvm   ,  & ! intent(inout)
       fgv    ,  & ! intent(in)
       fgw    ,  & ! intent(in)
       fgwm   ,  & ! intent(inout)
       fgqm   ,  & ! intent(inout)
       fgqmm  ,  & ! intent(out)
       fgq    ,  & ! intent(inout)
       fgqp   ,  & ! intent(inout)
       fgice  ,  & !
       fgicem ,  & !
       fgicep ,  & !
       fgliq  ,  & !
       fgliqm ,  & !
       fgliqp ,  & !
       fgvar  ,  & !
       fgvarm ,  & !
       fgvarp ,  & !
       fgtlamm,  & ! intent(inout)
       fgtlam ,  & ! intent(in)
       fgtphim,  & ! intent(inout)
       fgtphi ,  & ! intent(in)
       fglnpm ,  & ! intent(inout)
       fglnps ,  & ! intent(in)
       fgplamm,  & ! intent(inout)
       fgplam ,  & ! intent(in)
       fgpphim,  & ! intent(inout)
       fgpphi,   & ! intent(in)
       fgyum,    & ! intent(in)
       fgyvm,    & ! intent(in)
       fgtdm,    & ! intent(in)
       fgvdlnpm, & ! intent(in)
       fgtmpm,   & ! intent(inout)
       fgyu,     & ! intent(inout)
       fgyv,     & ! intent(inout)
       fgtd,     & ! intent(inout)
       fgqd,     & ! intent(inout)
       fgvdlnp,  & ! intent(inout)
       fgps,     & ! intent(in)
       fgpass_scalars,  &
       adr_scalars,     &
       fgpass_fluxscalars, &
       fgpsp

  USE Sizes, ONLY:        &
       ibMaxPerJB,   & ! intent(in)
       iPerIJB      ,&
       jPerIJB      ,&
       ibPerIJ,      & 
       jbPerIJ,      &
       imaxperj,     &
       myfirstlat_diag, &
       mylastlat_diag,  &
       myfirstlon,   &
       mylastlon,    &
       myjmax_d,     &
       jmax,         &
       imax,         &
       nlatsinproc_d,&
       ibMax,        & ! intent(in)
       jbMax,        & ! intent(in)
       kMax,         &
       del,          & ! intent(in)
       delcl,        & ! intent(in)
       rdel2,        & ! intent(in)
       ci,si,        & ! intent(in)
       sl,cl           ! intent(in)

  USE Constants,   ONLY : &
       tov,               & ! intent(in)
       tbar,              & ! intent(in)
       gasr,              & ! intent(in)
       cp,                & ! intent(in)
       qmin,              & ! intent(in)
       i8,                & ! intent(in)
       r8,                & ! intent(in)
       grav 

  USE SpecDynamics, ONLY : &
       p1,                & ! intent(in)
       p2,                & ! intent(in)
       h1,                & ! intent(in)
       h2,                & ! intent(in)
       hmt                  ! intent(in)

  USE PhysicsDriver, ONLY : &
       DryPhysics,          &
       SimpPhys


  USE GridHistory, ONLY:  &
       StoreGridHistory , &
       IsGridHistoryOn  , &
       dogrh            , &
       nGHis_temper    , &
       nGHis_uzonal    , &
       nGHis_vmerid    , &
       nGHis_spchum    , &
       nGHis_tvirsf    , &
       nGHis_uzonsf    , &
       nGHis_vmersf    , &
       nGHis_sphusf    , &
       nGHis_snowdp    , &
       nGHis_rouglg    , &
       nGHis_tcanop    , &
       nGHis_tgfccv    , &
       nGHis_tgdeep    , &
       nGHis_swtsfz    , &
       nGHis_swtrtz    , &
       nGHis_swtrcz    , &
       nGHis_mostca    , &
       nGHis_mostgc    , &
       nGHis_vegtyp    , &
       nGHis_presfc

  USE Diagnostics, ONLY:   &
       StartStorDiag     , &
       pwater            , &
       updia             , &
       dodia             , &
       nDiag_tmpsfc      , & ! time mean surface pressure
       nDiag_tmtsfc      , & ! time mean surface temperature
       nDiag_omegav      , & ! omega
       nDiag_sigdot      , & ! sigma dot
       nDiag_pwater      , & ! precipitable water
       nDiag_divgxq      , & ! divergence * specific humidity
       nDiag_vmoadv      , & ! vertical moisture advection
       nDiag_tmtdps      , & ! time mean deep soil temperature
       nDiag_tgfccv      , & ! ground/surface cover temperature
       nDiag_tcanop      , & ! canopy temperature
       nDiag_homtvu      , & ! Horizontal Momentum Transport
       nDiag_vzmtwu      , & ! Vertical Zonal Momentum Transport
       nDiag_vmmtwv      , & ! Vertical Meridional Momentum Transport
       nDiag_mshtvt      , & ! Meridional Sensible Heat Transport
       nDiag_zshtut      , & ! Zonal Sensible Heat Transport
       nDiag_vshtwt      , & ! Vertical Sensible Heat Transport
       nDiag_mshtuq      , & ! Meridional Specific Humidity Transport
       nDiag_zshtuq      , & ! Zonal Specific Humidity Transport
       nDiag_vshtwq      , & ! Vertical Specific Humidity Transport
       nDiag_dewptt          ! Dew Point Temperature K

  USE FieldsPhysics, ONLY: &
       zorl              , &
       sheleg            , &
       imask             , &
       gtsea             , &
       tcm               , &
       tgm               , &
       tdm               , &
       wm                , &
       capacm

  USE Options, ONLY:       &
       isimp             , &
       nscalars          , &
       nClass            , &
       nAeros            , &
       microphys         , &
       SL_twotime_scheme , &
       istrt

  USE Utils  , ONLY:       &
       cel_area          , &
       massconsrv        , &
       fconsrv           , &
       fconsrv_flux      , &
       totmas            , &
       totflux           , &
       total_mass        , &
       total_flux

  USE Parallelism, ONLY:       &
       maxnodes

  USE Communications, ONLY:    &
       Collect_Gauss

  IMPLICIT NONE

  INCLUDE 'mpif.h'

  PRIVATE
  PUBLIC :: AddTend
  PUBLIC :: GrpComp
  PUBLIC :: TimeFilterStep1
  PUBLIC :: TimeFilterStep2
  PUBLIC :: GlobConservation
  PUBLIC :: GlobFluxConservation
  PUBLIC :: UpdateConserv
  PUBLIC :: Scalardiffusion

  REAL(KIND=r8) :: totmass
  REAL(KIND=r8), ALLOCATABLE :: fg (:,:,:)
  REAL(KIND=r8), ALLOCATABLE :: fgs(:,:,:)
  REAL(KIND=r8), ALLOCATABLE :: fg_flux (:,:,:)
  REAL(KIND=r8), ALLOCATABLE :: fgs_flux(:,:,:)

  REAL(KIND=r8), ALLOCATABLE :: fps(:,:)
  INTEGER      , ALLOCATABLE :: displ(:)
  INTEGER      , ALLOCATABLE :: displ_flux(:)

  LOGICAL      , PUBLIC      :: do_globconserv
  LOGICAL      , PUBLIC      :: do_globfluxconserv
  LOGICAL      , PUBLIC      :: init_globconserv
  LOGICAL      , PUBLIC      :: init_globfluxconserv
  INTEGER :: ierr

CONTAINS


  SUBROUTINE GrpComp(&
       gyu    , gyv    , gtd    , gqd    , &
       gvdlnp , gdiv   , gtmp   , grot   , &
       gu     , gv     , gw     , gq     , &
       gplam  , gpphi  , gum    , gzs    , &
       gvm    , gtm    , gqm    , omg    , &
       ps     , gtlam  , gtphi  , gqlam  , &
       gqphi  , gulam  , guphi  , gvlam  , &
       gvphi  , gtlamm , gtphim , gplamm , &
       gpphim , glnpm  , gdivm  , gzslam , &
       gzsphi , gyum   , gyvm   , gtdm   , &
       gqdm   , gvdlnpm, colrad , rcl    , &
       vmax   , ifday  , tod    ,          &
       ibMax  , kMax   , ibLim  , slagr  , &
       slhum  , jb     , lonrad , cos2d  , &
       intcosz, cos2lat, ercossin, fcor  , &
       cosiv  , initial,                   &
       gicem  , gice   ,gicet  , &
       gliqm  , gliq   ,gliqt  , &
       gvarm  ,gvar  , gvart)
    !
    ! grpcomp: grid-point computations (all tendencies are computed) 
    !
    !
    ! slagr is the option for eulerian (slagr=.false.) or
    ! semi-Lagrangian integration (slagr=.true.)
    !
    !
    !
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: initial
    REAL(KIND=r8),    INTENT(OUT  ) :: gyu    (ibMax, kMax)
    REAL(KIND=r8),    INTENT(OUT  ) :: gyv    (ibMax, kMax)
    REAL(KIND=r8),    INTENT(OUT  ) :: gtd    (ibMax, kMax)
    REAL(KIND=r8),    INTENT(OUT  ) :: gqd    (ibMax, kMax)
    REAL(KIND=r8),    INTENT(OUT  ) :: gvdlnp (ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gdiv   (ibMax, kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: gtmp   (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: grot   (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gu     (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gv     (ibMax, kMax)
    REAL(KIND=r8),    INTENT(OUT  ) :: gw     (ibMax, kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: gq     (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gplam  (ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gpphi  (ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gzs    (ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gum    (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gvm    (ibMax, kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: gtm    (ibMax, kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: gqm    (ibMax, kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: omg    (ibMax, kMax) 
    REAL(KIND=r8),    INTENT(IN   ) :: ps     (ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gtlam  (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gtphi  (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gqlam  (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gqphi  (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gulam  (ibMax, kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: guphi  (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gvlam  (ibMax, kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: gvphi  (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gtlamm (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gtphim (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gplamm (ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gpphim (ibMax)
    REAL(KIND=r8),    INTENT(INOUT) :: glnpm  (ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gdivm  (ibMax, kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gzslam (ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: gzsphi (ibMax)
    REAL(KIND=r8),    INTENT(INOUT) :: gyum   (ibMax, kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: gyvm   (ibMax, kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: gtdm   (ibMax, kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: gqdm   (ibMax, kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: gvdlnpm(ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: colrad (ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: rcl    (ibMax)
    REAL(KIND=r8),    INTENT(INOUT) :: vmax   (kMax)
    REAL(KIND=r8),    OPTIONAL,   INTENT(INOUT) :: gicem  (ibMax, kMax)
    REAL(KIND=r8),    OPTIONAL,   INTENT(INOUT) :: gice   (ibMax, kMax)
    REAL(KIND=r8),    OPTIONAL,   INTENT(INOUT) :: gicet  (ibMax, kMax)
    REAL(KIND=r8),    OPTIONAL,   INTENT(INOUT) :: gliqm  (ibMax, kMax)
    REAL(KIND=r8),    OPTIONAL,   INTENT(INOUT) :: gliq   (ibMax, kMax)
    REAL(KIND=r8),    OPTIONAL,   INTENT(INOUT) :: gliqt  (ibMax, kMax)
    REAL(KIND=r8),    OPTIONAL,   INTENT(INOUT) :: gvarm  (ibMax, kMax,nClass+nAeros)
    REAL(KIND=r8),    OPTIONAL,   INTENT(INOUT) :: gvar   (ibMax, kMax,nClass+nAeros)
    REAL(KIND=r8),    OPTIONAL,   INTENT(INOUT) :: gvart  (ibMax, kMax,nClass+nAeros)
    
    INTEGER, INTENT(IN   ) :: ifday
    REAL(KIND=r8),    INTENT(IN   ) :: tod
    INTEGER, INTENT(IN   ) :: ibLim
    LOGICAL, INTENT(IN   ) :: slagr
    LOGICAL, INTENT(IN   ) :: slhum
    INTEGER, INTENT(IN   ) :: jb
    REAL(KIND=r8)   , INTENT(IN   ) :: lonrad (ibMax)
    REAL(KIND=r8)   , INTENT(IN   ) :: cos2d  (ibMax)    
    REAL(KIND=r8)   , INTENT(IN   ) :: cos2lat(ibMax)
    REAL(KIND=r8)   , INTENT(IN   ) :: ercossin(ibMax)
    REAL(KIND=r8)   , INTENT(IN   ) :: fcor   (ibMax)
    REAL(KIND=r8)   , INTENT(IN   ) :: cosiv  (ibMax)
    LOGICAL, INTENT(IN   ) :: intcosz
    !
    !  local variables 
    !
    REAL(KIND=r8)   , DIMENSION(ibMax,kMax) :: zlam
    REAL(KIND=r8)   , DIMENSION(ibMax,kMax) :: zphi
    REAL(KIND=r8)   , DIMENSION(ibMax,kMax) :: gdt
    REAL(KIND=r8)   , DIMENSION(ibMax,kMax) :: psint
    REAL(KIND=r8)   , DIMENSION(ibMax     ) :: zsint
    REAL(KIND=r8)   , DIMENSION(ibMax,kMax) :: adveps
    REAL(KIND=r8)   , DIMENSION(ibMax,kMax) :: divint
    REAL(KIND=r8)   , DIMENSION(ibMax,kMax) :: divintm
    REAL(KIND=r8)   , DIMENSION(ibMax,kMax) :: dot
    REAL(KIND=r8)   , DIMENSION(ibMax,kMax) :: dewpoint
    REAL(KIND=r8)                           :: rk
    INTEGER                        :: i
    INTEGER                        :: k
    INTEGER                        :: ncount
    INTEGER                        :: latco

    gyu=0.0_r8
    gyv=0.0_r8
    gtd=0.0_r8
    gqd=0.0_r8
    gvdlnp=0.0_r8
    gdt=0.0_r8
    IF (microphys) THEN
       gicet =0.0_r8
       gliqt=0.0_r8
       IF((nClass+nAeros) >0 .and. PRESENT(gvarm))THEN
           gvart  =0.0_r8
       END IF
    END IF

    IF (initial.eq.1.or..not.slagr.or..not.SL_twotime_scheme) THEN
       gyum=0.0_r8
       gyvm=0.0_r8
       gtdm=0.0_r8
       gqdm=0.0_r8
       gvdlnpm=0.0_r8
    END IF
    latco=jb
    rk= gasr/cp
    ! 
    !
    ! enforce humidity to be above a certain level (avoid negative values...)
    ! ----------------------------------------------------------------------
    !
    IF(TRIM(isimp).ne.'YES')THEN
       gq=MAX(gq,qmin)
    ENDIF
    !
    IF(dodia(nDiag_tmtdps).or.dodia(nDiag_tgfccv).or.dodia(nDiag_tcanop).or. &
       dodia(nDiag_tmtsfc))THEN
       ncount=0
       DO i=1,ibLim
         IF(imask(i,jb).ge.1_i8)THEN
             ncount=ncount+1
             gdt(i,1)=tcm(ncount,jb)
             gdt(i,2)=tgm(ncount,jb)
             gdt(i,3)=tdm(ncount,jb)
         ELSE 
             gdt(i,1)=ABS(gtsea (i,jb))
             gdt(i,2)=ABS(gtsea (i,jb))
             gdt(i,3)=ABS(gtsea (i,jb))
         END IF
       END DO
       IF(dodia(nDiag_tmtdps))CALL updia(gdt(1:ibLim,3:3),nDiag_tmtdps,latco)
       IF(dodia(nDiag_tgfccv))CALL updia(gdt(1:ibLim,2:2),nDiag_tgfccv,latco)
       IF(dodia(nDiag_tcanop))CALL updia(gdt(1:ibLim,1:1),nDiag_tcanop,latco)
       IF(dodia(nDiag_tmtsfc))CALL updia(ABS(gtsea(1:ibLim,jb)),nDiag_tmtsfc,latco)
    END IF
    !
    !     obtain grid history fields if requested
    !
    IF(IsGridHistoryOn())THEN
       DO k=1,kMax
          DO i=1,ibLim
             gtd(i,k)=tov(k)+gtmp(i,k)
          END DO
       END DO

       IF(dogrh(nGHis_temper,latco)) CALL StoreGridHistory(gtd(1:ibLim,1:kMax),nGHis_temper,latco)
       IF(dogrh(nGHis_uzonal,latco)) CALL StoreGridHistory( gu(1:ibLim,1:kMax),nGHis_uzonal,latco,sqrt(rcl(1:ibLim)))
       IF(dogrh(nGHis_vmerid,latco)) CALL StoreGridHistory( gv(1:ibLim,1:kMax),nGHis_vmerid,latco,sqrt(rcl(1:ibLim)))
       IF(dogrh(nGHis_spchum,latco)) CALL StoreGridHistory( gq(1:ibLim,1:kMax),nGHis_spchum,latco)

       IF(dogrh(nGHis_tvirsf,latco)) CALL StoreGridHistory(gtd(1:ibLim,1),nGHis_tvirsf,latco)
       IF(dogrh(nGHis_uzonsf,latco)) CALL StoreGridHistory( gu(1:ibLim,1),nGHis_uzonsf,latco,sqrt(rcl(1:ibLim)))
       IF(dogrh(nGHis_vmersf,latco)) CALL StoreGridHistory( gv(1:ibLim,1),nGHis_vmersf,latco,sqrt(rcl(1:ibLim)))
       IF(dogrh(nGHis_sphusf,latco)) CALL StoreGridHistory( gq(1:ibLim,1),nGHis_sphusf,latco)

       IF(dogrh(nGHis_snowdp,latco)) CALL StoreGridHistory(sheleg(1:ibLim,latco),nGHis_snowdp,latco)
       IF(dogrh(nGHis_rouglg,latco)) CALL StoreGridHistory(  zorl(1:ibLim,latco),nGHis_rouglg,latco)

       ncount=0
       DO i=1,ibLim
          gtd(i,1)=gtsea(i,latco)
          gtd(i,2)=gtsea(i,latco)
          gtd(i,3)=gtsea(i,latco)
          gtd(i,4)=1.0_r8
          gtd(i,5)=1.0_r8
          gtd(i,6)=1.0_r8
          gtd(i,7)=0.0e0_r8
          gtd(i,8)=0.0e0_r8
          gtd(i,9)=imask(i,latco)
          IF(imask(i,latco).ge.1_i8)THEN
             ncount=ncount+1
             gtd(i,1)=tcm(ncount,latco)
             gtd(i,2)=tgm(ncount,latco)
             gtd(i,3)=tdm(ncount,latco)
             gtd(i,4)=wm (ncount,1,latco)
             gtd(i,5)=wm (ncount,2,latco)
             gtd(i,6)=wm (ncount,3,latco)
             gtd(i,7)=capacm(ncount,1,latco)
             gtd(i,8)=capacm(ncount,2,latco)
          END IF
       END DO

       IF(dogrh(nGHis_tcanop,latco)) CALL StoreGridHistory(gtd(1:ibLim,1),nGHis_tcanop,latco)
       IF(dogrh(nGHis_tgfccv,latco)) CALL StoreGridHistory(gtd(1:ibLim,2),nGHis_tgfccv,latco)
       IF(dogrh(nGHis_tgdeep,latco)) CALL StoreGridHistory(gtd(1:ibLim,3),nGHis_tgdeep,latco)
       IF(dogrh(nGHis_swtsfz,latco)) CALL StoreGridHistory(gtd(1:ibLim,4),nGHis_swtsfz,latco)
       IF(dogrh(nGHis_swtrtz,latco)) CALL StoreGridHistory(gtd(1:ibLim,5),nGHis_swtrtz,latco)
       IF(dogrh(nGHis_swtrcz,latco)) CALL StoreGridHistory(gtd(1:ibLim,6),nGHis_swtrcz,latco)
       IF(dogrh(nGHis_mostca,latco)) CALL StoreGridHistory(gtd(1:ibLim,7),nGHis_mostca,latco,1000.0_r8)
       IF(dogrh(nGHis_mostgc,latco)) CALL StoreGridHistory(gtd(1:ibLim,8),nGHis_mostgc,latco,1000.0_r8)
       IF(dogrh(nGHis_vegtyp,latco)) CALL StoreGridHistory(gtd(1:ibLim,9),nGHis_vegtyp,latco)

    END IF
    gtd=0.0_r8
    !
    !     computation of maximum wind 
    !     ---------------------------
    !
    DO k=1,kMax
       DO i=1,ibLim
          vmax(k)=MAX(vmax(k),cosiv(i)*SQRT(gu(i,k)*gu(i,k)+gv(i,k)*gv(i,k)))
       ENDDO
    ENDDO
    !
    !     Computation of tendencies (part related to intermediate time-step)
    !     ------------------------------------------------------------------
    !
    !     wind derivatives with respect to phi 
    !     ------------------------------------
    !
    IF (.NOT. slagr) THEN
       CALL delwind(gulam,gvlam,grot,gdiv,guphi,gvphi,cos2lat,&
            ibMax, kMax, ibLim)
    END IF
    !
    !     computation of sigma_dot and vertical integrals of div and lnps
    !     ---------------------------------------------------------------
    !
    CALL vertint(slagr,zsint,psint,adveps,divint,divintm, &
         dot,gu,gv,gdiv,gdivm,gplam,gpphi,gzslam,gzsphi,rcl,del,ibMax,kMax,ibLim)
    !
    ! using the vertical velocity @level (dot), compute the vertical 
    ! velocity @ layer (gw)
    !
    DO k = 1, kmax -1
       DO i=1,ibLim
          gw(i,k) = 0.5_r8 * (dot(i,k+1) + dot(i,k))
       END DO   
    END DO
    k = kmax
    DO i=1,ibLim
       gw(i,k) = 0.5_r8 * dot(i,k)
    END DO
    !
    !     computation of omega
    !     --------------------
    !
    CALL omega(omg,psint,adveps,divint,dot,ps,sl,ibMax,kMax,ibLim)
    !
    ! *JPB - Begin
    !
    !     Computation of Diagnostic for Transportation Fluxes
    !     ---------------------------------------------------
    !
    !     Horizontal Momentum Transport
    IF (dodia(nDiag_homtvu)) CALL updia (gv*gu, nDiag_homtvu, latco)
    !     Vertical Zonal Momentum Transport
    IF (dodia(nDiag_vzmtwu)) CALL updia (omg*gu,  nDiag_vzmtwu, latco)
    !     Vertical Meridional Momentum Transport
    IF (dodia(nDiag_vmmtwv)) CALL updia (omg*gv,  nDiag_vmmtwv, latco)
    IF (dodia(nDiag_mshtvt) .OR. dodia(nDiag_zshtut) .OR. dodia(nDiag_vshtwt)) THEN
       !  Add Basic State Tov to Tv' and Get Dry Absolute Temperature
       DO k=1,kMax
          DO i=1,ibLim
             gtd(i,k)=(tov(k)+gtmp(i,k))/(1.0_r8+0.608_r8*gq(i,k))
          END DO
       END DO
       !     Meridional Sensible Heat Transport
       IF (dodia(nDiag_mshtvt)) CALL updia (gv*gtd,  nDiag_mshtvt, latco)
       !     Zonal Sensible Heat Transport
       IF (dodia(nDiag_zshtut)) CALL updia (gu*gtd,  nDiag_zshtut, latco)
       !     Vertical Sensible Heat Transport
       IF (dodia(nDiag_vshtwt)) CALL updia (omg*gtd, nDiag_vshtwt, latco)
       !     Reset gtd to Zero for Tendencies Calculations
       gtd=0.0_r8
    END IF
    !     Meridional Specific Humidity Transport
    IF (dodia(nDiag_mshtuq)) CALL updia (gv*gq,   nDiag_mshtuq, latco)
    !     Zonal Specific Humidity Transport
    IF (dodia(nDiag_zshtuq)) CALL updia (gu*gq,   nDiag_zshtuq, latco)
    !     Vertical Specific Humidity Transport
    IF (dodia(nDiag_vshtwq)) CALL updia (omg*gq,  nDiag_vshtwq, latco)
    !
    !     Dew Point Temperature K
    IF (dodia(nDiag_dewptt)) THEN
       DO k=1,kMax
          DO i=1,ibLim
             dewpoint(i,k)=(log(gq(i,k)*(1000.0_r8*ps(i)*sl(k))/380.042_r8)&
                           * 35.86_r8 - (4717.4732_r8))&
                           /(log(gq(i,k)*(1000.0_r8*ps(i)*sl(k))&
                           /380.042_r8) - 17.27_r8)
          END DO
       END DO
       CALL updia (dewpoint,nDiag_dewptt,latco)
    END IF
    ! *JPB - End
    !
    !     computation of geopotential gradient
    !     ------------------------------------
    !
    IF (slagr.and.SL_twotime_scheme) THEN
       CALL delgeo(gtlam,zlam,gzslam,gtphi,zphi,gzsphi,hmt,ibMax,ibLim,kMax)
     ELSE 
       CALL delgeo(gtlamm,zlam,gzslam,gtphim,zphi,gzsphi,hmt,ibMax,ibLim,kMax)
    END IF
    !
    !
    IF (.NOT. slagr) THEN
       !
       !     horizontal advection of wind
       !     ----------------------------
       !
       CALL hadvec(gu,gv,gulam,guphi,gyu,rcl,ibMax,kMax,ibLim)
       CALL hadvec(gu,gv,gvlam,gvphi,gyv,rcl,ibMax,kMax,ibLim)
       !
       !     vertical advection of wind
       !     --------------------------
       !
       CALL vadvec(gu,dot,rdel2,gyu,ibMax,kMax,ibLim)
       CALL vadvec(gv,dot,rdel2,gyv,ibMax,kMax,ibLim)
       !
       !     metric term
       !     -----------
       !
       CALL metric(gu,gv,gyv,ercossin,ibMax,kMax,ibLim)
       !
    ENDIF
    !
    !     coriolis terms
    !     --------------
    !
    CALL coriol(gu,gv,gyu,gyv,fcor,ibMax,kMax,ibLim)
    !
    !     non-linear part of pressure gradient
    !     ------------------------------------
    !
    CALL nlprgr(gplam,gpphi,gtmp,gyu,gyv,gasr,ibMax,kMax,ibLim)
    !
    !
    !     horizontal advection of temperature 
    !     -----------------------------------
    !
    IF (.NOT. slagr) THEN
       CALL hadvec(gu,gv,gtlam,gtphi,gtd,rcl,ibMax,kMax,ibLim)
    END IF
    !
    !     vertical advection of temperature 
    !     ---------------------------------
    !
    CALL vadvtmp(gtmp, p1, p2, h1, h2, dot, psint,&
         ci, rdel2, gtd, ibMax, kMax, ibLim, slagr)
    !
    !     complete non-linear part of temperature tendency
    !     ------------------------------------------------
    !
    CALL tmptend(gtd,gtmp,tov,psint,adveps,divint,rk,ibMax,kMax,ibLim)
    !
    IF (.NOT. slagr) THEN
       !
       IF (.NOT. slhum) THEN
          !     horizontal advection of humidity 
          !     --------------------------------
          !
          CALL hadvec(gu,gv,gqlam,gqphi,gqd,rcl,ibMax,kMax,ibLim)
          !
          !     vertical advection of humidity
          !     ------------------------------
          !
          CALL vadvec(gq,dot,rdel2,gqd,ibMax,kMax,ibLim)
       ENDIF
       !
       !     log pressure tendency
       !     --------------------- 
       !
       gvdlnp(      1:ibLim) = - psint(1:ibLim,kMax)
       !
    ELSE
       !
       gvdlnp(      1:ibLim) = zsint(1:ibLim) / (gasr*tbar)
       !
    ENDIF

    IF(dodia(nDiag_divgxq)) CALL updia(psint, nDiag_divgxq,latco)
    IF(dodia(nDiag_vmoadv)) CALL updia(divint,nDiag_vmoadv,latco)
    IF(dodia(nDiag_omegav)) CALL updia(omg,   nDiag_omegav,latco)
    !
    !
    !
    IF (IsGridHistoryOn()) THEN
       IF(dogrh(nGHis_presfc,latco)) CALL StoreGridHistory(ps(1:ibLim),nGHis_presfc,latco,10.0_r8)
    END IF
    !     
    !     sigma gke computed only at interior interfaces.
    !
    !
    IF(dodia(nDiag_sigdot))CALL updia(dot,nDiag_sigdot,latco)

    !
    !     tendency from old time-step
    !     ---------------------------
    IF (slagr.and.SL_twotime_scheme) THEN
       IF (initial.ne.2) THEN
          CALL tndtold(gyum,gyvm,gtdm,gvdlnpm,zlam,zphi,gplam,gpphi,divint, &
               rdel2,ci,h1,h2,tov,gasr,rk,dot,ibMax,kMax,ibLim)
       ENDIF
     ELSE
       CALL tndtold(gyum,gyvm,gtdm,gvdlnpm,zlam,zphi,gplamm,gpphim,divintm, &
            rdel2,ci,h1,h2,tov,gasr,rk,dot,ibMax,kMax,ibLim)
    ENDIF

    IF(TRIM(isimp).ne.'YES')THEN
       !
       !     gplam surface pressure in mb
       !
       DO k=1,kMax
          DO i=1,ibLim
             gtmp(i,k)= gtmp(i,k)+tov(k)
             gtm(i,k) = gtm(i,k)+tov(k)
          END DO
       END DO

       IF (microphys) THEN
          IF((nClass+nAeros)>0 .and. PRESENT(gvarm))THEN
             CALL DryPhysics & 
               (ibMax,gzs,  gtm, gqm, gum  ,gvm  ,10.0_r8*ps,gyu    ,gyv    ,gtd   ,&
               gqd    ,colrad,ifday ,tod   ,gtmp   ,gq     ,omg    ,jb    ,&
               lonrad ,glnpm  ,cos2d ,intcosz, &
               gicem  ,gice   ,gicet , &
               gliqm  ,gliq   ,gliqt ,&
               gvarm  ,gvar   ,gvart)
          ELSE
             CALL DryPhysics & 
               (ibMax,gzs,  gtm, gqm, gum  ,gvm  ,10.0_r8*ps,gyu    ,gyv    ,gtd   ,&
               gqd    ,colrad,ifday ,tod   ,gtmp   ,gq     ,omg    ,jb    ,&
               lonrad ,glnpm  ,cos2d ,intcosz, &
               gicem  ,gice   ,gicet , &
               gliqm  ,gliq   , gliqt  )
          END IF
         ELSE
          CALL DryPhysics & 
            (ibMax,gzs,  gtm, gqm, gum  ,gvm  ,10.0_r8*ps,gyu    ,gyv    ,gtd   ,&
            gqd    ,colrad,ifday ,tod   ,gtmp   ,gq     ,omg    ,jb    ,&
            lonrad,glnpm  ,cos2d ,intcosz )
       ENDIF

       DO k=1,kMax
          DO i=1,ibLim
             gtm(i,k)=gtm(i,k)-tov(k)
             gtmp(i,k)=gtmp(i,k)-tov(k)
          END DO
       END DO
       !     
       !     diagnostic of precipitable water
       !
       CALL pwater(gq    ,dot   ,ps ,del   ,ibMax,ibLim ,kMax  )
       IF(dodia(nDiag_pwater))CALL updia(dot,nDiag_pwater,latco)
       !
    ELSE
       ! Simplified physics
       !
       CALL SimpPhys(gu, gv, gtmp, gyv, gyu, gtd, ibMax, ibLim, kMax, jb,jbMax,ibMaxPerJB, &
                     iPerIJB    ,jPerIJB   )
       !
    END IF
    !
    !     diagnostic of time mean surface pressure
    !
    IF(dodia(nDiag_tmpsfc))CALL updia(ps  ,nDiag_tmpsfc,latco)
    !
    IF (initial.eq.1.and.slagr.and.SL_twotime_scheme) THEN
       gyum = gyum - 0.5_r8 * gyu 
       gyvm = gyvm - 0.5_r8 * gyv
       gtdm = gtdm - 0.5_r8 * gtd
       gqdm = gqdm - 0.5_r8 * gqd
       gvdlnpm = gvdlnpm - 0.5_r8 * gvdlnp
    END IF
  END SUBROUTINE GrpComp






  SUBROUTINE delwind(ulam, vlam, vor, div, uphi, vphi, cos2lat, &
       ibMax, kMax, ibLim)
    INTEGER, INTENT(IN ) :: ibMax
    INTEGER, INTENT(IN ) :: kMax
    REAL(KIND=r8),    INTENT(IN ) :: cos2lat(ibMax)
    REAL(KIND=r8),    INTENT(IN ) :: ulam(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN ) :: vlam(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN ) :: div (ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN ) :: vor (ibMax,kMax)
    REAL(KIND=r8),    INTENT(OUT) :: uphi(ibMax,kMax)
    REAL(KIND=r8),    INTENT(OUT) :: vphi(ibMax,kMax)
    INTEGER, INTENT(IN ) :: ibLim
    INTEGER :: ib, k
    !      
    !      From the vorticity, divergence and the e-w derivatives of 
    !      U and V, computes the values of cos(phi) d/d phi F , where F = U,
    !      and F = V.
    !
    DO k=1,kMax   
       DO ib=1,ibLim
          uphi(ib,k) = vlam(ib,k)  - cos2lat(ib) * vor(ib,k)
          vphi(ib,k) = cos2lat(ib) * div(ib,k) - ulam(ib,k)
       ENDDO
    ENDDO
  END SUBROUTINE delwind






  SUBROUTINE vertint(slagr,zsint,psint, adveps, divint, divintm, dot, &
       u, v, div, divm, plam, pphi, zlam, zphi, rcl, del, ibMax, kMax, ibLim)
    LOGICAL, INTENT(IN ) :: slagr
    INTEGER, INTENT(IN ) :: ibMax
    INTEGER, INTENT(IN ) :: kMax
    INTEGER, INTENT(IN ) :: ibLim
    REAL(KIND=r8),    INTENT(IN ) :: u(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN ) :: v(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN ) :: div(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN ) :: divm(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN ) :: plam(ibMax)
    REAL(KIND=r8),    INTENT(IN ) :: pphi(ibMax)
    REAL(KIND=r8),    INTENT(IN ) :: zlam(ibMax)
    REAL(KIND=r8),    INTENT(IN ) :: zphi(ibMax)
    REAL(KIND=r8),    INTENT(IN ) :: del(kMax)
    REAL(KIND=r8),    INTENT(IN ) :: rcl(ibMax)
    REAL(KIND=r8),    INTENT(INOUT) :: zsint(ibMax)
    REAL(KIND=r8),    INTENT(INOUT) :: psint(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: divint(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: divintm(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: adveps(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: dot(ibMax,kMax)
    !      
    !      Computes the vertical integral (in finite differences)
    !      of the divergence field (to be stored in divint). The scalar product  
    !      of the wind field (in each level) with the gradient of the surface
    !      pressure is stored in adveps. Its vertical integral is stored in psint.
    !      The vertical velocity (sigma_dot) is stored in dot.
    !
    INTEGER :: i, k
    !

    DO k=1,kMax
       DO i=1,ibLim
          adveps(i,k) = rcl(i)*(u(i,k) * plam(i) + v(i,k) * pphi(i) )
       ENDDO
    ENDDO
    k=1   
    DO i=1,ibLim
       dot(i,k) = 0.0_r8
       psint(i,k) = del(k) * adveps(i,k)
       divint(i,k) = del(k) * div(i,k)
       divintm(i,k) = del(k) * divm(i,k)
    ENDDO
    IF (slagr) THEN
       DO i=1,ibLim
          zsint(i) = del(k) * rcl(i)*(u(i,k)*zlam(i)+v(i,k)*zphi(i))
       ENDDO
    ENDIF
    DO k=2,kMax
       DO i=1,ibLim
          psint(i,k) = psint(i,k-1) + del(k) * adveps(i,k)
          divint(i,k) = divint(i,k-1) + del(k) * div(i,k)
          divintm(i,k) = divintm(i,k-1) + del(k) * divm(i,k)
       ENDDO
       IF (slagr) THEN
          DO i=1,ibLim
             zsint(i) = zsint(i) + del(k)*rcl(i)*(u(i,k)*zlam(i)+v(i,k)*zphi(i))
          ENDDO
       ENDIF
    ENDDO
    DO k=1,kMax-1
       DO i=1,ibLim
          dot(i,k+1)=dot(i,k) + del(k) * ( divint(i,kMax) + psint(i,kMax) &
               - div(i,k) - adveps(i,k) )
       ENDDO
    ENDDO
  END SUBROUTINE vertint






  SUBROUTINE omega(omg, psint, adveps, divint, dot, ps, sl, &
       ibMax, kMax, ibLim)
    INTEGER, INTENT(IN ) :: ibMax, ibLim, kMax
    REAL(KIND=r8),    INTENT(IN ) :: divint(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN ) :: adveps(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN ) :: dot(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN ) :: psint(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN ) :: ps(ibMax)
    REAL(KIND=r8),    INTENT(IN ) :: sl(kMax)
    REAL(KIND=r8),    INTENT(OUT) :: omg(ibMax,kMax)
    INTEGER              :: i
    INTEGER              :: k
    !      
    !      Computes omega
    !
    !

    DO k=1,kMax-1
       DO i=1,ibLim
          omg(i,k) = ps(i) * ( sl(k) * &
               ( adveps(i,k) - psint(i,kMax) - divint(i,kMax) ) &
               - 0.5_r8 * ( dot(i,k+1) + dot(i,k) ) )
       ENDDO
    ENDDO
    k=kMax
    DO i=1,ibLim
       omg(i,k) = ps(i) * ( sl(k) * &
            ( adveps(i,k) - psint(i,kMax) - divint(i,kMax) ) &
            - 0.5_r8 * dot(i,k) )
    ENDDO
  END SUBROUTINE omega






  SUBROUTINE delgeo (tlam, zlam, zslam, tphi, zphi, zsphi, hmt, imx, imax, kmax)
    INTEGER, INTENT(IN ) :: imx
    INTEGER, INTENT(IN ) :: imax
    INTEGER, INTENT(IN ) :: kmax
    REAL(KIND=r8),    INTENT(IN ) :: tlam (imx,kmax)
    REAL(KIND=r8),    INTENT(OUT) :: zlam (imx,kmax)
    REAL(KIND=r8),    INTENT(IN ) :: zslam(imx)
    REAL(KIND=r8),    INTENT(IN ) :: tphi (imx,kmax)
    REAL(KIND=r8),    INTENT(IN ) :: zsphi(imx)
    REAL(KIND=r8),    INTENT(OUT) :: zphi (imx,kmax)
    REAL(KIND=r8),    INTENT(IN ) :: hmt  (kmax,kmax)

    !      
    !      From the derivatives of temperature and surface geopotential 
    !      computes the values of the gradient of the geopotential
    !      (using the hydrostatic equation:  z = HM^T * T + zs )
    !
    INTEGER :: k,j,i
    !
    DO k = 1, kmax
      DO i = 1, imax
       zlam(i,k) = zslam(i)
       zphi(i,k) = zsphi(i)     
      END DO
    END DO
    DO k = 1, kMax
       DO i = 1, iMax
          DO j = 1, kMax
             zlam(i,k) = zlam(i,k) +  (tlam(i,j)*hmt(j,k))
             zphi(i,k) = zphi(i,k) +  (tphi(i,j)*hmt(j,k))
          END DO
       END DO
    END DO
  END SUBROUTINE delgeo





  SUBROUTINE hadvec(u, v, flam, fphi, tend, rcl, ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL(KIND=r8),    INTENT(IN   ) :: rcl(ibMax) ! 1.0_r8 / ( cos(lat)**2 )
    REAL(KIND=r8),    INTENT(IN   ) :: u(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: v(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: flam(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: fphi(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: tend(ibMax,kMax)
    !      
    ! Computes the horizontal advection term of field f (whose horizontal
    ! derivatives are given in flam and fphi) and add its contribution to current
    ! tendency (stored in tend).
    !
    INTEGER :: i, k
    DO k=1,kMax   
       DO i=1,ibLim
          tend(i,k) = tend(i,k) - rcl(i) * (u(i,k)*flam(i,k) + v(i,k)*fphi(i,k))
       END DO
    END DO
  END SUBROUTINE hadvec





  SUBROUTINE vadvec(f, dot, rdel2, tend, ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL(KIND=r8),    INTENT(IN   ) :: f(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: dot(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: rdel2(kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: tend(ibMax,kMax)
    !      
    ! Computes the vertical advection of field f (in finite differences)
    ! using the values fo sigma dot (given in dot) 
    ! and of 1 / 2 Delta_k (in rdel2)
    ! and add its contribution to current tendency (in tend)
    !
    INTEGER :: i, k
    k=1   
    DO i=1,ibLim
       tend(i,k) = tend(i,k) - rdel2(k) * (dot(i,k+1)*(f(i,k+1) -f(i,k)))
    ENDDO
    DO k=2,kMax-1   
       DO i=1,ibLim
          tend(i,k) = tend(i,k) - rdel2(k) * (dot(i,k+1)*(f(i,k+1)-f(i,k)) &
               + dot(i,k)*(f(i,k)-f(i,k-1)))
       ENDDO
    ENDDO
    k=kMax   
    DO i=1,ibLim
       tend(i,k) = tend(i,k) - rdel2(k) * (dot(i,k)*(f(i,k)-f(i,k-1))) 
    ENDDO
  END SUBROUTINE vadvec






  SUBROUTINE metric(u, v, tend, ercossin, ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL(KIND=r8),    INTENT(IN   ) :: ercossin(ibMax) ! sin(lat) / ( er * cos(lat)**2 )
    REAL(KIND=r8),    INTENT(IN   ) :: u(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: v(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: tend(ibMax,kMax)
    INTEGER :: i, k
    !      
    ! Computes the metric term and add its contribution  to current v-tendency
    !     ercossin(1:ibLim) = COS(colrad(1:ibLim)) * rcl(1:ibLim) / er
    !                                             1.0_r8/cos(latitude)**2.0_r8
    !     ercossin(1:ibLim) =   1.0_r8/cos(latitude)/ er
    !                                             
    !
    DO k=1,kMax   
       DO i=1,ibLim
          tend(i,k) = tend(i,k) - ercossin(i)*(u(i,k)*u(i,k) + v(i,k)*v(i,k))
       END DO
    END DO
  END SUBROUTINE metric






  SUBROUTINE coriol(u, v, tendu, tendv, fcor, ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL(KIND=r8),    INTENT(IN   ) :: fcor(ibMax) ! 2 * omega * sin(phi)
    REAL(KIND=r8),    INTENT(IN   ) :: u(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: v(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: tendu(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: tendv(ibMax,kMax)
    INTEGER :: i, k
    !      
    ! Computes the coriolis contributions  to current u- and v- tendencies
    !
    !
    DO k=1,kMax   
       DO i=1,ibLim
          tendu(i,k) = tendu(i,k) + fcor(i) * v(i,k)
          tendv(i,k) = tendv(i,k) - fcor(i) * u(i,k)
       ENDDO
    ENDDO
  END SUBROUTINE coriol






  SUBROUTINE nlprgr(plam, pphi, tmp, tendu, tendv, rc, ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL(KIND=r8),    INTENT(IN   ) :: rc
    REAL(KIND=r8),    INTENT(IN   ) :: tmp(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: plam(ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: pphi(ibMax)
    REAL(KIND=r8),    INTENT(INOUT) :: tendu(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: tendv(ibMax,kMax)
    INTEGER :: i, k
    !      
    ! Computes the non-linear part of the pressure gradient contribution for the 
    ! current u-  and v- tendencies.
    !
    !
    DO k=1,kMax   
       DO i=1,ibLim
          tendu(i,k) = tendu(i,k) - rc * tmp(i,k) * plam(i)
          tendv(i,k) = tendv(i,k) - rc * tmp(i,k) * pphi(i)
       ENDDO
    ENDDO
  END SUBROUTINE nlprgr






  SUBROUTINE vadvtmp(tmp, p1, p2, h1, h2, dot, psint, &
       ci, rdel2, tend, ibMax, kMax, ibLim, slagr)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    LOGICAL, INTENT(IN   ) :: slagr
    REAL(KIND=r8),    INTENT(IN   ) :: tmp(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: dot(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: psint(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: rdel2(kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: p1(kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: p2(kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: h1(kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: h2(kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: ci(kMax+1)
    REAL(KIND=r8),    INTENT(INOUT) :: tend(ibMax,kMax)
    INTEGER :: i, k
    REAL(KIND=r8) :: w1(ibMax,kMax), w2(ibMax,kMax), w3(ibMax,kMax)   ! work space
    !      
    ! Computes the vertical advection contribution in the temperature equation
    ! using the values fo sigma dot (given in dot) and of 1/2 Delta_k (in rdel2)
    ! and add its contribution to current tendency (in tend)
    !
    !
    IF (.NOT. slagr) THEN
       DO k=1,kMax-1
          DO i=1,ibLim
             w1(i,k) = p1(k) * tmp(i,k+1) - tmp(i,k)
             w2(i,k+1) = tmp(i,k+1) - p2(k+1) * tmp(i,k)
             w3(i,k) = ci(k+1) * psint(i,kMax) - psint(i,k)
          ENDDO
       ENDDO
    ELSE
       DO k=1,kMax-1
          DO i=1,ibLim
             w1(i,k) = ( p1(k) - 1.0_r8 ) * tmp(i,k+1)
             w2(i,k+1) = ( 1.0_r8 - p2(k+1) ) * tmp(i,k)
             w3(i,k) = ci(k+1) * psint(i,kMax) - psint(i,k)
          ENDDO
       ENDDO
    ENDIF
    k=1   
    DO i=1,ibLim
       tend(i,k) = tend(i,k) - rdel2(k) * (dot(i,k+1)*w1(i,k) + h1(k)*w3(i,k))
    ENDDO
    DO k=2,kMax-1   
       DO i=1,ibLim
          tend(i,k) = tend(i,k) - rdel2(k) * (dot(i,k+1)*w1(i,k) + h1(k)*w3(i,k) &
               + dot(i,k)*w2(i,k) + h2(k)*w3(i,k-1))
       ENDDO
    ENDDO
    k=kMax   
    DO i=1,ibLim
       tend(i,k) = tend(i,k) - rdel2(k) * (dot(i,k)*w2(i,k) + h2(k)*w3(i,k-1))
    ENDDO
  END SUBROUTINE vadvtmp






  SUBROUTINE tmptend(tend, tmp, tov, psint, adveps, divint, rk, &
       ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL(KIND=r8),    INTENT(IN   ) :: tov(kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: rk
    REAL(KIND=r8),    INTENT(IN   ) :: psint(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: divint(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: adveps(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: tmp(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: tend(ibMax,kMax)
    INTEGER :: i, k
    !      
    ! Computes the non-linear contributions to the temperature tendency
    !
    !
    DO k=1,kMax
       DO i=1,ibLim
          tend(i,k) = tend(i,k) - tmp(i,k) * rk * divint(i,kMax) &
               + rk * ( tov(k) + tmp(i,k) ) * ( adveps(i,k) - psint(i,kMax) )
       ENDDO
    ENDDO
  END SUBROUTINE tmptend






  SUBROUTINE tndtold(tdu, tdv, tdt, tdlnp, zlam, zphi, plam, pphi, divint, &
       rdel2, ci, h1, h2, tov, rc, rk, w, ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL(KIND=r8),    INTENT(IN   ) :: rc
    REAL(KIND=r8),    INTENT(IN   ) :: rk
    REAL(KIND=r8),    INTENT(IN   ) :: zlam(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: zphi(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: plam(ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: pphi(ibMax)
    REAL(KIND=r8),    INTENT(IN   ) :: tov(kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: divint(ibMax,kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: ci(kMax+1)
    REAL(KIND=r8),    INTENT(IN   ) :: h1(kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: h2(kMax)
    REAL(KIND=r8),    INTENT(IN   ) :: rdel2(kMax)
    REAL(KIND=r8),    INTENT(OUT  ) :: w(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: tdu(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: tdv(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: tdt(ibMax,kMax)
    REAL(KIND=r8),    INTENT(INOUT) :: tdlnp(ibMax)
    INTEGER :: i, k
    REAL(KIND=r8) :: half=0.5e0_r8

    !      
    ! Computes the part of the tendencies relative to the old time-step
    !
    !
    !   pressure gradient terms
    !   -----------------------
    DO k=1,kMax   
       DO i=1,ibLim
          tdu(i,k) = tdu(i,k) - half * ( zlam(i,k) + rc * tov(k) * plam(i) )
          tdv(i,k) = tdv(i,k) - half * ( zphi(i,k) + rc * tov(k) * pphi(i) )
       ENDDO
    ENDDO
    !
    !   log pressure tendency
    !   ---------------------
    DO i=1,ibLim
       tdlnp(i) = tdlnp(i) - half * divint(i,kMax)
    ENDDO
    !
    !   Temperature tendency
    !   --------------------
    DO k=1,kMax-1
       DO i=1,ibLim
          w(i,k) = ci(k+1) * divint(i,kMax) - divint(i,k)
       ENDDO
    ENDDO
    k=1
    DO i=1,ibLim
       tdt(i,k) =  tdt(i,k) - half * ( rk * tov(k) * divint(i,kMax) &
            + rdel2(k) * h1(k) * w(i,k) )
    ENDDO
    DO k=2,kMax-1
       DO i=1,ibLim
          tdt(i,k) = tdt(i,k) - half * ( rk * tov(k) * divint(i,kMax) &
               + rdel2(k) * ( h1(k) * w(i,k) + h2(k) * w(i,k-1) ) )
       ENDDO
    ENDDO
    k=kMax
    DO i=1,ibLim
       tdt(i,k) = tdt(i,k) - half * ( rk * tov(k) * divint(i,kMax) &
            + rdel2(k) *  h2(k) * w(i,k-1) )
    ENDDO
  END SUBROUTINE tndtold

  !
  ! addtend: finish tendency computations, adding contributions from
  !          old and current time step.

  SUBROUTINE AddTend(dt, nlnminit, jbFirst, jbLast, slhum)
    REAL(KIND=r8),    INTENT(IN) :: dt
    LOGICAL, INTENT(IN) :: nlnminit
    LOGICAL, INTENT(IN) :: slhum
    INTEGER, INTENT(IN) :: jbFirst
    INTEGER, INTENT(IN) :: jbLast
    INTEGER :: ib, jb, k
    REAL(KIND=r8) :: dt2
    dt2 = dt + dt
    IF (.NOT.nlnminit) THEN

       DO jb = jbFirst, jbLast
          DO k = 1, kMax
             DO ib = 1, ibMaxPerJB(jb)
                fgyu(ib,k,jb) = fgum(ib,k,jb) + &
                     dt2 * ( fgyu(ib,k,jb) + fgyum(ib,k,jb) ) 
                fgyv(ib,k,jb) = fgvm(ib,k,jb) + &
                     dt2 * ( fgyv(ib,k,jb) + fgyvm(ib,k,jb) ) 
                fgtd(ib,k,jb) = fgtmpm(ib,k,jb) + &
                     dt2 * ( fgtd(ib,k,jb) + fgtdm(ib,k,jb) ) 
             END DO
             IF (.not.slhum) THEN
                DO ib = 1, ibMaxPerJB(jb)
                   fgqd(ib,k,jb) = fgqm(ib,k,jb) + &
                        dt2 * fgqd(ib,k,jb) 
                END DO
             ENDIF
          END DO
          DO ib = 1, ibMaxPerJB(jb)
             fgvdlnp(ib,jb) = fglnpm(ib,jb) + &
                  dt2 * ( fgvdlnp(ib,jb) + fgvdlnpm(ib,jb) )
          END DO
       END DO

    ELSE

       DO jb = jbFirst, jbLast
          DO k = 1, kMax
             DO ib = 1, ibMaxPerJB(jb)
                fgyu(ib,k,jb) = fgyu(ib,k,jb) + 2.0_r8 * fgyum(ib,k,jb)
                fgyv(ib,k,jb) = fgyv(ib,k,jb) + 2.0_r8 * fgyvm(ib,k,jb)
                fgtd(ib,k,jb) = fgtd(ib,k,jb) + 2.0_r8 * fgtdm(ib,k,jb)
                fgqd(ib,k,jb) = fgqd(ib,k,jb)
             END DO
          END DO
          DO ib = 1, ibMaxPerJB(jb)
             fgvdlnp(ib,jb) = fgvdlnp(ib,jb) + 2.0_r8 * fgvdlnpm(ib,jb)
          END DO
       END DO

    ENDIF
  END SUBROUTINE AddTend


  ! TimeFilterStep1: First part of the asselin/robert time-filter
  ! (computes a partially filtered value of fold)

  SUBROUTINE TimeFilterStep1(fa, fb, jbFirst, jbLast, slhum)
    REAL(KIND=r8),    INTENT(IN) :: fa
    REAL(KIND=r8),    INTENT(IN) :: fb
    INTEGER, INTENT(IN) :: jbFirst
    INTEGER, INTENT(IN) :: jbLast
    LOGICAL, INTENT(IN) :: slhum
    INTEGER :: ib, jb, k,kk

    DO jb = jbFirst, jbLast
       DO k = 1, kMax
          DO ib = 1, ibMaxPerJB(jb)
             fgtmpm  (ib,k,jb) = fa*fgtmp (ib,k,jb) + fb*fgtmpm (ib,k,jb)
             fgdivm  (ib,k,jb) = fa*fgdiv (ib,k,jb) + fb*fgdivm (ib,k,jb)
             fgum    (ib,k,jb) = fa*fgu   (ib,k,jb) + fb*fgum   (ib,k,jb)
             fgvm    (ib,k,jb) = fa*fgv   (ib,k,jb) + fb*fgvm   (ib,k,jb)
             fgtlamm (ib,k,jb) = fa*fgtlam(ib,k,jb) + fb*fgtlamm(ib,k,jb)
             fgtphim (ib,k,jb) = fa*fgtphi(ib,k,jb) + fb*fgtphim(ib,k,jb)
          END DO
       END DO
       IF(.not.slhum) THEN
          DO k = 1, kMax
             DO ib = 1, ibMaxPerJB(jb)
                fgqm(ib,k,jb) = fa*fgq(ib,k,jb) + fb*fgqm(ib,k,jb)
             END DO
          END DO
        ELSE 
          IF (fa.ne.0.0_r8) THEN 
             DO k = 1, kMax
                DO ib = 1, ibMaxPerJB(jb)
                   fgqm(ib,k,jb) = fa*fgq(ib,k,jb) + &
                                      fb*(fgqm(ib,k,jb)+fgqp(ib,k,jb))
                END DO
             END DO
             IF (microphys) THEN
                DO k = 1, kMax
                   DO ib = 1, ibMaxPerJB(jb)
                      fgicem(ib,k,jb) = fa*fgice(ib,k,jb) + &
                                        fb*(fgicem(ib,k,jb)+fgicep(ib,k,jb))
                      fgliqm(ib,k,jb) = fa*fgliq(ib,k,jb) + &
                                        fb*(fgliqm(ib,k,jb)+fgliqp(ib,k,jb))
                   END DO
                END DO
                DO kk=1,nClass+nAeros
                   DO k = 1, kMax
                      DO ib = 1, ibMaxPerJB(jb)
                         fgvarm(ib,k,jb,kk) = fa*fgvar(ib,k,jb,kk) + &
                                        fb*(fgvarm(ib,k,jb,kk)+fgvarp(ib,k,jb,kk))
                      END DO
                   END DO
                END DO
             ENDIF
          ENDIF
          DO k = 1, kMax
             DO ib = 1, ibMaxPerJB(jb)
                fgq(ib,k,jb) = fgqp(ib,k,jb)
             END DO
          END DO
          IF (microphys) THEN
             DO k = 1, kMax
                DO ib = 1, ibMaxPerJB(jb)
                   fgice(ib,k,jb) = fgicep(ib,k,jb)
                   fgliq(ib,k,jb) = fgliqp(ib,k,jb)
                END DO
             END DO
             DO kk=1,nClass+nAeros
                DO k = 1, kMax
                   DO ib = 1, ibMaxPerJB(jb)
                      fgvar(ib,k,jb,kk) = fgvarp(ib,k,jb,kk)
                   END DO
                END DO
             END DO
          ENDIF
       ENDIF
       DO ib = 1, ibMaxPerJB(jb)
          fglnpm  (ib,jb) = fa*fglnps(ib,jb) + fb*fglnpm (ib,jb)
          fgplamm (ib,jb) = fa*fgplam(ib,jb) + fb*fgplamm(ib,jb)
          fgpphim (ib,jb) = fa*fgpphi(ib,jb) + fb*fgpphim(ib,jb)
       END DO
    END DO
  END SUBROUTINE TimeFilterStep1


  ! TimeFilterStep2: Second part of the asselin/robert time-filter
  ! (the partially filtered value of fold is filtered completely)

  SUBROUTINE TimeFilterStep2(fb1, jbFirst, jbLast, slhum, bckhum)
    REAL(KIND=r8),    INTENT(IN) :: fb1
    INTEGER, INTENT(IN) :: jbFirst
    INTEGER, INTENT(IN) :: jbLast
    LOGICAL, INTENT(IN) :: slhum
    LOGICAL, INTENT(IN) :: bckhum
    INTEGER :: ib, jb, k

    DO jb = jbFirst, jbLast
       DO k = 1, kMax
          DO ib = 1, ibMaxPerJB(jb)
             fgtmpm (ib,k,jb) = fgtmpm (ib,k,jb) + fb1*fgtmp (ib,k,jb)
             fgdivm (ib,k,jb) = fgdivm (ib,k,jb) + fb1*fgdiv (ib,k,jb)
             fgum   (ib,k,jb) = fgum   (ib,k,jb) + fb1*fgu   (ib,k,jb)
             fgvm   (ib,k,jb) = fgvm   (ib,k,jb) + fb1*fgv   (ib,k,jb)
             fgtlamm(ib,k,jb) = fgtlamm(ib,k,jb) + fb1*fgtlam(ib,k,jb)
             fgtphim(ib,k,jb) = fgtphim(ib,k,jb) + fb1*fgtphi(ib,k,jb)
          END DO
       END DO
       IF(.not.slhum.or.bckhum) THEN
          DO k = 1, kMax
             DO ib = 1, ibMaxPerJB(jb)
                fgqm   (ib,k,jb) = fgqm   (ib,k,jb) + fb1*fgq   (ib,k,jb)
             END DO
          END DO
!          IF (microphys) THEN
!             DO k = 1, kMax
!                DO ib = 1, ibMaxPerJB(jb)
!                   fgicem(ib,k,jb) = fgicem(ib,k,jb) + fb1*(fgice(ib,k,jb))
!                   fgliqm(ib,k,jb) = fgliqm(ib,k,jb) + fb1*(fgliq(ib,k,jb))
!!!                END DO
!             END DO
!          ENDIF
       ENDIF
       DO ib = 1, ibMaxPerJB(jb)
          fglnpm (ib,jb) = fglnpm (ib,jb) + fb1*fglnps(ib,jb)
          fgplamm(ib,jb) = fgplamm(ib,jb) + fb1*fgplam(ib,jb)
          fgpphim(ib,jb) = fgpphim(ib,jb) + fb1*fgpphi(ib,jb)
       END DO
    END DO
  END SUBROUTINE TimeFilterStep2


  SUBROUTINE GlobConservation(jFirst, jLast, jbFirst, jbLast, &
                              jFirst_d, jLast_d)
    INTEGER, INTENT(IN) :: jFirst
    INTEGER, INTENT(IN) :: jLast
    INTEGER, INTENT(IN) :: jbFirst
    INTEGER, INTENT(IN) :: jbLast
    INTEGER, INTENT(IN) :: jFirst_d
    INTEGER, INTENT(IN) :: jLast_d
    INTEGER :: ib, jb, k
    INTEGER :: i, j, ns, j1, ins
    REAL(KIND=r8) :: s
    
    !
    !  Mass Conservation
    !  -----------------
    !
    ! fgpsp = ps global field, arbitray grid
    ! fps = ps global field, 1 latitude per processor
    !
    ! To guarantee numerical reproducibility we need to make sure the
    ! mass summation is done the same way (order) no matter how the
    ! grid is divided. The problem is that the division of grid-cells
    ! between processors is arbitrary, and hence the order and the
    ! number of terms in the summation done by each processor will be
    ! different! To avoid that we redistribute the grid-cells as
    ! one-full-latitude-circle per processor
    !
    !$OMP SINGLE
    IF (.NOT.ALLOCATED(fps)) THEN
       ALLOCATE (fps(imax,myjmax_d))
       IF (nscalars > 0) ALLOCATE (fgs(imax,myjmax_d,nscalars))
       IF (nscalars > 0) ALLOCATE (fg(ibmax,nscalars,jbmax))
       ALLOCATE (displ(0:maxnodes-1))
    ENDIF
    IF (init_globconserv) THEN
       CALL Collect_Gauss(fgps, fps, 1) 
      ELSE
       CALL Collect_Gauss(fgpsp, fps, 1) 
    ENDIF
    !$OMP END SINGLE
    DO j=jFirst_d, jLast_d
       s = 0.0      
       j1 = j - myfirstlat_diag + 1
       DO i=1,imaxperj(j)
          s = s + fps(i,j1)
       ENDDO
       massconsrv(j) = s * cel_area(j)
    ENDDO
    !$OMP BARRIER
    !$OMP SINGLE
    displ(0) = 0
    DO i=1,maxnodes-1
       displ(i) = displ(i-1) + nlatsinproc_d(i-1)
    ENDDO
    ! Up to now, this processor has filled only the j-th positions
    ! assigned to him. The remaining positions are now filled with the
    ! values calculated by the other processors. 
    CALL MPI_ALLGATHERV(massconsrv(myfirstlat_diag),myjmax_d,MPI_DOUBLE_PRECISION, &
                        massconsrv,nlatsinproc_d,displ,MPI_DOUBLE_PRECISION, & 
                        MPI_COMM_WORLD, ierr)
    totmass = 0.
    DO j=1,jmax
       totmass = totmass + massconsrv(j)
    ENDDO
    !$OMP END SINGLE

    ! If that is the first time the total mass is calculated then save
    ! the result for future reference, otherwise enforce mass
    ! conservation.
    IF (.NOT. init_globconserv) THEN
       s = total_mass(0) / totmass
       DO j=jbFirst,jbLast
          DO i=1,ibMaxPerJB(j)
             fgpsp(i,j) = fgpsp(i,j)*s
          ENDDO
       ENDDO
     ELSE
       total_mass(0) = totmass
    ENDIF

    IF (.not. do_globconserv) RETURN
    !
    !  Passive scalars Conservation
    !  ----------------------------
    !$OMP BARRIER
    ins = adr_scalars
    DO ns=1,nscalars
       DO j=jFirst,jLast
          ! Before redistributing the grid points, each processor sum
          ! over the kmax vertical levels, which are independent of
          ! the grid division.
          DO i=myfirstlon(j),mylastlon(j)
             ib = ibPerIJ(i,j)
             jb = jbPerIJ(i,j)
             s = 0.0      
             DO k=1,kmax
                s = s + fgpass_scalars(ib,k,jb,ns,ins)*del(k)   
             ENDDO
             IF (.not. init_globconserv) THEN
                fg(ib,ns,jb) = s*fgpsp(ib,jb)
               ELSE
                fg(ib,ns,jb) = s*fgps(ib,jb)
             ENDIF
          ENDDO
       ENDDO
    ENDDO
    !$OMP BARRIER
    !$OMP SINGLE
    !
    CALL Collect_Gauss(fg, fgs, nscalars) 
    !$OMP END SINGLE
    DO ns=1,nscalars
       DO j=jFirst_d, jLast_d
          s = 0.0      
          j1 = j-myfirstlat_diag+1
          DO i=1,imaxperj(j)
             s = s + fgs(i,j1,ns)
          ENDDO
          fconsrv(ns,j) = s * cel_area(j)
       ENDDO
    ENDDO
    !$OMP BARRIER
    !$OMP SINGLE

    displ(0) = 0
    DO i=1,maxnodes-1
       displ(i) = displ(i-1) + nlatsinproc_d(i-1)*nscalars
    ENDDO
    CALL MPI_ALLGATHERV(&
         fconsrv(1,myfirstlat_diag), myjmax_d*nscalars, MPI_DOUBLE_PRECISION, &
         fconsrv, nlatsinproc_d(:)*nscalars,displ, MPI_DOUBLE_PRECISION, & 
         MPI_COMM_WORLD, ierr)
    DO ns=1,nscalars
       totmas(ns) = 0.
       DO j=1,jmax
          totmas(ns) = totmas(ns) + fconsrv(ns,j)
       ENDDO
    ENDDO
    !$OMP END SINGLE
    IF (.not. init_globconserv) THEN
       DO ns=1,nscalars
          IF (totmas(ns).ne.0.) THEN
             !$OMP SINGLE
             totmas(ns) = total_mass(ns) / totmas(ns)
             !$OMP END SINGLE
             DO j=jbFirst,jbLast
                DO i=1,ibMaxPerJB(j)
                   fgpass_scalars(i,:,j,ns,ins)=fgpass_scalars(i,:,j,ns,ins) * &
                                                   totmas(ns)
                ENDDO
             ENDDO
          ENDIF
       ENDDO
     ELSE
       !$OMP SINGLE
       DO ns=1,nscalars
          total_mass(ns) = totmas(ns)
       ENDDO
       !$OMP END SINGLE
    ENDIF
       
  END SUBROUTINE GlobConservation


  SUBROUTINE GlobFluxConservation(jFirst, jLast,  jbFirst, jbLast, &
                                 jFirst_d, jLast_d)
    INTEGER, INTENT(IN) :: jFirst
    INTEGER, INTENT(IN) :: jLast
    INTEGER, INTENT(IN) :: jbFirst
    INTEGER, INTENT(IN) :: jbLast
    INTEGER, INTENT(IN) :: jFirst_d
    INTEGER, INTENT(IN) :: jLast_d
    INTEGER :: ib, jb, k
    INTEGER :: i, j, ns, j1, ins
    REAL(KIND=r8) :: s

    IF (nAeros == 0) RETURN

    !$OMP SINGLE
    IF (.NOT.ALLOCATED(fgs_flux)) THEN
       IF (nAeros > 0) ALLOCATE (fgs_flux(imax,myjmax_d,nAeros))
       IF (nAeros > 0) ALLOCATE (fg_flux(ibmax,nAeros   ,jbmax))
       ALLOCATE (displ_flux(0:maxnodes-1))
    ENDIF
    !$OMP END SINGLE
    IF (.not. do_globfluxconserv) RETURN

    !
    !  Passive scalars Conservation
    !  ----------------------------
    DO ns=1,nAeros
       DO j=jFirst,jLast
          DO i=myfirstlon(j),mylastlon(j)
             ib = ibPerIJ(i,j)
             jb = jbPerIJ(i,j)
             fg_flux(ib,ns,jb) =  fgpass_fluxscalars(ib,jb,ns)
          ENDDO
       ENDDO
    ENDDO
    !$OMP BARRIER
    !$OMP SINGLE
    CALL Collect_Gauss(fg_flux, fgs_flux, nAeros)
    !$OMP END SINGLE
    DO ns=1,nAeros
       DO j=jFirst_d, jLast_d
          s = 0.0_r8
          j1 = j-myfirstlat_diag+1
          DO i=1,imaxperj(j)
             s = s + fgs_flux(i,j1,ns)
          ENDDO
          fconsrv_flux(ns,j) = s * cel_area(j)
       ENDDO
    ENDDO
    !$OMP BARRIER
    !$OMP SINGLE
    displ_flux(0) = 0
    DO i=1,maxnodes-1
       displ_flux(i) = displ_flux(i-1) + nlatsinproc_d(i-1)*nAeros
    ENDDO
    CALL MPI_ALLGATHERV(&
         fconsrv_flux(1,myfirstlat_diag),myjmax_d*nAeros,MPI_DOUBLE_PRECISION, &
         fconsrv_flux,nlatsinproc_d(:)*nAeros,displ_flux,MPI_DOUBLE_PRECISION, &
         MPI_COMM_WORLD, ierr)
    DO ns=1,nAeros
       totflux(ns) = 0.0_r8
       DO j=1,jmax
          totflux(ns) = totflux(ns) + fconsrv_flux(ns,j)
       ENDDO
    ENDDO
    !$OMP END SINGLE

    IF (.not. init_globfluxconserv) THEN
       DO ns=1,nAeros
          IF (totflux(ns).ne.0.) THEN
             !$OMP SINGLE
             totflux(ns) = (total_flux(ns) - totflux(ns))/REAL(imax*jMax)
             !$OMP END SINGLE
             DO j=jbFirst,jbLast
                DO i=1,ibMaxPerJB(j)
                    fgpass_fluxscalars(i,j,ns) =  totflux(ns)
                ENDDO
             ENDDO
          ENDIF
       ENDDO
     ELSE
       !$OMP SINGLE
       DO ns=1,nAeros
          total_flux(ns) = totflux(ns)
       ENDDO
       !$OMP END SINGLE
    ENDIF

  END SUBROUTINE GlobFluxConservation
  !
  !  Vertical Diffusion of Scalar variables
  !  --------------------------------------
  !
  !  Solves diffusion equation implicitly:
  !
  !      dq/dt = - d(<w'q'>)/dz = d[ K dq/dz ]/dz
  !
  !  By finite differencing the equation as:
  !
  !      (q^n+1 - q^n-1)/dt = d[ K dq^n+1/dz ]/dz
  !
  !  This lead to kmax equations that can be written in matrix form
  !
  !                                               (n+1)                    (n-1)
  !      | a(1) c(1)                 0   |   |q(1)|        |q(1)-2 Dt Fs/D1|
  !      | b(2) a(2) c(2)                |   |    |        |               | 
  !      |      b(3) a(3)   c(3)         |   | .  |        | .             | 
  !      |                               | * | .  |      = | .             | 
  !      |                               |   | .  |        | .             | 
  !      |          b(m-1) a(m-1) c(m-1) |   |    |        |               | 
  !      | 0                b(m)   a(m)  |   |q(m)|        |q(m)           | 
  !
  !
  SUBROUTINE Scalardiffusion(iblim, jb, deltat, Kh, tov, tv, gq)

    ! IN/OUT VARIBLES --------------------------------------------------
    INTEGER, INTENT(IN) :: iblim
    INTEGER, INTENT(IN) :: jb
    REAL(KIND=r8), INTENT(IN) :: deltat

    ! Diffusion coefficient on top each layer (m^2/s)
    ! Note that:
    !  - at k=kmax it should be zero (no flux across the model top)
    !  - there is no k=0 (surface) value because this is acounted 
    !    for in the tendency
    REAL(KIND=r8), INTENT(IN) :: Kh(ibmax,kmax+1)

    ! Virtual temperature in the middle of layer (K)
    REAL(KIND=r8), INTENT(IN) :: tv(ibmax,kmax)
    REAL(KIND=r8), INTENT(IN) :: tov(kmax)

    ! Specific humidity in the middle of layer (kg/kg)
    REAL(KIND=r8), INTENT(IN) :: gq(ibmax,kmax)

    ! LOCAL VARIABLES --------------------------------------------------

    INTEGER :: i, k, ns, ins ! counters
    REAL(KIND=r8) :: s1, s2 ! aux variables

    ! matrix coefficients
    REAL(KIND=r8) :: a(ibmax,kmax), b(ibmax,kmax), c(ibmax,kmax), dt2

    ! Thermodinamic temperature in the middle of layer (K)
    REAL(KIND=r8) :: gt(ibmax,kmax)

    !
    !  Compute coefficients
    !  --------------------

    dt2 = - deltat

    ! Caculate thermodinamic temperature
    DO k=1,kmax
       DO i=1,iblim
          gt(i,k) = (tv(i,k)+tov(k))/(1.0_r8+0.608_r8*gq(i,k))
       ENDDO
    ENDDO

    ! Calculate b_k coefficients
    DO i=1,iblim
       b(i,1) = 0.0_r8
    ENDDO
    DO k=2,kmax
       s2 = dt2 * (grav/gasr)**2 * sl(k) * si(k) / (delcl(k-1)*del(k))
       DO i=1,iblim
          b(i,k) = s2 * Kh(i,k-1) * 2._r8 / (gt(i,k-1)+gt(i,k)) / gt(i,k)
       ENDDO
    ENDDO

    ! Calculate c_k coefficients 
    DO k=1,kmax-1
       s1 = dt2 * (grav/gasr)**2 * sl(k) * si(k+1) / (delcl(k)*del(k))
       DO i=1,iblim
          c(i,k) = s1 * Kh(i,k) * 2._r8 / (gt(i,k)+gt(i,k+1)) / gt(i,k)
       ENDDO
    ENDDO
    DO i=1,iblim
       c(i,kmax) = 0.0_r8
    ENDDO

    ! Calculate a_k coefficients
    ! We can run 1:kmax because we set b(1)=c(kmax)=0.0
    DO k=1,kmax
       DO i=1,iblim
          a(i,k) = 1.0_r8 - b(i,k) - c(i,k)
       ENDDO
    ENDDO

    !
    !  Solve implicit systems
    !  ----------------------
    ! 
    ! First pass, going down and eliminating all b(k)
    ins = adr_scalars
    DO k=2,kmax
       DO i=1,iblim
          a(i,k) = a(i,k) - c(i,k-1) * b(i,k) / a(i,k-1)
       ENDDO
       DO ns=1,nscalars
          DO i=1,iblim
             fgpass_scalars(i,k,jb,ns,ins) = fgpass_scalars(i,k,jb,ns,ins) &
                  - fgpass_scalars(i,k-1,jb,ns,ins) * b(i,k) / a(i,k-1)
          ENDDO
       ENDDO
    ENDDO
    ! The m-th equation is now trivial as only a(kmax) is non-zero
    DO ns=1,nscalars
       DO i=1,iblim
          fgpass_scalars(i,kmax,jb,ns,ins) = fgpass_scalars(i,kmax,jb,ns,ins) &
                                              /  a(i,kmax)
       ENDDO
    ENDDO
    ! Now that we have q(kmax), go upwards calculating q(kmax-1), ... q(1)
    DO k=kmax-1,1,-1
       DO ns=1,nscalars
          DO i=1,iblim
             fgpass_scalars(i,k,jb,ns,ins) = ( fgpass_scalars(i,k,jb,ns,ins) &
                           - c(i,k) * fgpass_scalars(i,k+1,jb,ns,ins) ) / a(i,k)
          ENDDO
       ENDDO
    ENDDO
       
  END SUBROUTINE Scalardiffusion


  SUBROUTINE UpdateConserv(jFirst, jLast, &
                          jFirst_d, jLast_d)
    INTEGER, INTENT(IN) :: jFirst
    INTEGER, INTENT(IN) :: jLast
    INTEGER, INTENT(IN) :: jFirst_d
    INTEGER, INTENT(IN) :: jLast_d
    INTEGER :: ib, jb, k
    INTEGER :: i, j, ns, j1, ins
    REAL(KIND=r8) :: s

    !
    !  Passive scalars Conservation
    !  ----------------------------
    ins = adr_scalars
    DO ns=1,nscalars
       DO j=jFirst,jLast
          DO i=myfirstlon(j),mylastlon(j)
             ib = ibPerIJ(i,j)
             jb = jbPerIJ(i,j)
             s = 0.0
             DO k=1,kmax
                s = s + fgpass_scalars(ib,k,jb,ns,ins)*del(k)
             ENDDO
             fg(ib,ns,jb) = s*fgpsp(ib,jb)
          ENDDO
       ENDDO
    ENDDO
    !$OMP BARRIER
    !$OMP SINGLE
    CALL Collect_Gauss(fg, fgs, nscalars)
    !$OMP END SINGLE
    DO ns=1,nscalars
       DO j=jFirst_d, jLast_d
          s = 0.0
          j1 = j-myfirstlat_diag+1!hmjb
          DO i=1,imaxperj(j)
             s = s + fgs(i,j1,ns)
          ENDDO
          fconsrv(ns,j) = s * cel_area(j)
       ENDDO
    ENDDO
    !$OMP BARRIER
    !$OMP SINGLE
    displ(0) = 0
    DO i=1,maxnodes-1
       displ(i) = displ(i-1) + nlatsinproc_d(i-1)*nscalars
    ENDDO
    CALL MPI_ALLGATHERV(&
         fconsrv(1,myfirstlat_diag),myjmax_d*nscalars,MPI_DOUBLE_PRECISION, &
         fconsrv,nlatsinproc_d(:)*nscalars,displ,MPI_DOUBLE_PRECISION, &
         MPI_COMM_WORLD, ierr)
    DO ns=1,nscalars
       total_mass(ns) = 0.
       DO j=1,jmax
          total_mass(ns) = total_mass(ns) + fconsrv(ns,j)
       ENDDO
    ENDDO
    !$OMP END SINGLE

  END SUBROUTINE UpdateConserv
       
END MODULE GridDynamics
