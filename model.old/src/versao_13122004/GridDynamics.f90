!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE GridDynamics
  USE FieldsDynamics, ONLY : &
       fgtmpmm,  & ! intent(out); **(JP)** parece desnecessaria; candidata a delecao
       fgtmp  ,  & ! intent(in)
       fgdivm ,  & ! intent(inout)
       fgdivmm,  & ! intent(out); **(JP)** parece desnecessaria; candidata a delecao
       fgdiv  ,  & ! intent(in)
       fgum   ,  & ! intent(inout)
       fgumm  ,  & ! intent(out)     
       fgu    ,  & ! intent(in)
       fgvm   ,  & ! intent(inout)
       fgvmm  ,  & ! intent(inout)       
       fgv    ,  & ! intent(in)
       fgqm   ,  & ! intent(inout)
       fgqmm  ,  & ! intent(out)
       fgq    ,  & ! intent(in)
       fgtlamm,  & ! intent(inout)
       fgtlam ,  & ! intent(in)
       fgtphim,  & ! intent(inout)
       fgtphimm, & ! intent(out); **(JP)** parece desnecessaria; candidata a delecao
       fgtphi ,  & ! intent(in)
       fglnpm ,  & ! intent(inout)
       fglnpmm,  & ! intent(out); **(JP)** parece desnecessaria; candidata a delecao
       fglnps ,  & ! intent(in)
       fgplamm,  & ! intent(inout)
       fgtlammm, & ! intent(out); **(JP)** parece desnecessaria; candidata a delecao
       fgplam ,  & ! intent(in)
       fgplammm, & ! intent(out); **(JP)** parece desnecessaria; candidata a delecao
       fgpphim,  & ! intent(inout)
       fgpphimm, & ! intent(out); **(JP)** parece desnecessaria; candidata a delecao
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
       fgvdlnp     ! intent(inout)

  USE Sizes, ONLY:        &
       ibMaxPerJB,   & ! intent(in)
       jbMax,        & ! intent(in)
       kMax,         &
       imax,         &
       jmax,         &
       del,          & ! intent(in)
       rdel2,        & ! intent(in)
       ci,           & ! intent(in)
       sl              ! intent(in)

  USE Constants,   ONLY : &
       tov,               & ! intent(in)
       gasr,              & ! intent(in)
       twomg,             & ! intent(in)
       cp,                & ! intent(in)
       er,                & ! intent(in)
       qmin                 ! intent(in)

  USE SpecDynamics, ONLY : &
       p1,                & ! intent(in)
       p2,                & ! intent(in)
       h1,                & ! intent(in)
       h2,                & ! intent(in)
       hmt                  ! intent(in)

  USE SemiLagrangian,   ONLY : &
       tanplane              !intent(in)

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
       pwater            , &
       updia             , &
       dodia             , &
       nDiag_tmpsfc      , &
       nDiag_sigdot      , &
       nDiag_omegav      , &
       nDiag_pwater      , &
       nDiag_divgxq      , &
       nDiag_vmoadv      , &
       nDiag_tmtdps      , &
       nDiag_tgfccv      , &
       nDiag_tcanop

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
       istrt


  IMPLICIT NONE

  PRIVATE
  PUBLIC :: AddTend
  PUBLIC :: GrpComp
  PUBLIC :: TimeFilterStep1
  PUBLIC :: TimeFilterStep2


CONTAINS


  SUBROUTINE GrpComp(&
       gyu    , gyv    , gtd    , gqd    , &
       gvdlnp , gdiv   , gtmp   , grot   , &
       gu     , gv     , gw     , gq     , &
       gplam  , gpphi  , glnp   , gum    , &
       gvm    , gtm    , gqm    , omg    , &
       ps     , gtlam  , gtphi  , gqlam  , &
       gqphi  , gulam  , guphi  , gvlam  , &
       gvphi  , gtlamm , gtphim , gplamm , &
       gpphim , glnpm  , gdivm  , gzslam , &
       gzsphi , gyum   , gyvm   , gtdm   , &
       gvdlnpm, colrad , rcl    ,          &
       vmax   , ifday  , tod    ,          &
       ibMax  , kMax   , ibLim  ,          &
       slagr  , jb     , lonrad , cos2d  , &
       intcosz, gyu1   , gyv1   , gtd1   , &
       gqd1)
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
    REAL,    INTENT(OUT  ) :: gyu    (ibMax, kMax)
    REAL,    INTENT(OUT  ) :: gyv    (ibMax, kMax)
    REAL,    INTENT(OUT  ) :: gtd    (ibMax, kMax)
    REAL,    INTENT(OUT  ) :: gqd    (ibMax, kMax)
    REAL,    INTENT(OUT  ) :: gvdlnp (ibMax)
    REAL,    INTENT(IN   ) :: gdiv   (ibMax, kMax)
    REAL,    INTENT(INOUT) :: gtmp   (ibMax, kMax)
    REAL,    INTENT(IN   ) :: grot   (ibMax, kMax)
    REAL,    INTENT(IN   ) :: gu     (ibMax, kMax)
    REAL,    INTENT(IN   ) :: gv     (ibMax, kMax)
    REAL,    INTENT(OUT  ) :: gw     (ibMax, kMax)
    REAL,    INTENT(INOUT) :: gq     (ibMax, kMax)
    REAL,    INTENT(IN   ) :: gplam  (ibMax)
    REAL,    INTENT(IN   ) :: gpphi  (ibMax)
    REAL,    INTENT(IN   ) :: glnp   (ibMax)
    REAL,    INTENT(IN   ) :: gum    (ibMax, kMax)
    REAL,    INTENT(IN   ) :: gvm    (ibMax, kMax)
    REAL,    INTENT(INOUT) :: gtm    (ibMax, kMax)
    REAL,    INTENT(INOUT) :: gqm    (ibMax, kMax)
    REAL,    INTENT(INOUT) :: omg    (ibMax, kMax) 
    REAL,    INTENT(IN   ) :: ps     (ibMax)
    REAL,    INTENT(IN   ) :: gtlam  (ibMax, kMax)
    REAL,    INTENT(IN   ) :: gtphi  (ibMax, kMax)
    REAL,    INTENT(IN   ) :: gqlam  (ibMax, kMax)
    REAL,    INTENT(IN   ) :: gqphi  (ibMax, kMax)
    REAL,    INTENT(IN   ) :: gulam  (ibMax, kMax)
    REAL,    INTENT(INOUT) :: guphi  (ibMax, kMax)
    REAL,    INTENT(IN   ) :: gvlam  (ibMax, kMax)
    REAL,    INTENT(INOUT) :: gvphi  (ibMax, kMax)
    REAL,    INTENT(IN   ) :: gtlamm (ibMax, kMax)
    REAL,    INTENT(IN   ) :: gtphim (ibMax, kMax)
    REAL,    INTENT(IN   ) :: gplamm (ibMax)
    REAL,    INTENT(IN   ) :: gpphim (ibMax)
    REAL,    INTENT(INOUT) :: glnpm  (ibMax)
    REAL,    INTENT(IN   ) :: gdivm  (ibMax, kMax)
    REAL,    INTENT(IN   ) :: gzslam (ibMax)
    REAL,    INTENT(IN   ) :: gzsphi (ibMax)
    REAL,    INTENT(OUT  ) :: gyum   (ibMax, kMax)
    REAL,    INTENT(OUT  ) :: gyvm   (ibMax, kMax)
    REAL,    INTENT(OUT  ) :: gtdm   (ibMax, kMax)
    REAL,    INTENT(OUT  ) :: gvdlnpm(ibMax)
    REAL,    INTENT(IN   ) :: colrad (ibMax)
    REAL,    INTENT(IN   ) :: rcl    (ibMax)
    REAL,    INTENT(INOUT) :: vmax   (ibMax)
    INTEGER, INTENT(IN   ) :: ifday
    REAL,    INTENT(IN   ) :: tod
    INTEGER, INTENT(IN   ) :: ibLim
    LOGICAL, INTENT(IN   ) :: slagr
    INTEGER, INTENT(IN   ) :: jb
    REAL   , INTENT(IN   ) :: lonrad (ibMax)
    REAL   , INTENT(IN   ) :: cos2d  (ibMax)    
    LOGICAL, INTENT(IN   ) :: intcosz
    REAL   , INTENT(OUT  ) :: gyu1   (ibMax, kMax)
    REAL   , INTENT(OUT  ) :: gyv1   (ibMax, kMax)
    REAL   , INTENT(OUT  ) :: gtd1   (ibMax, kMax)
    REAL   , INTENT(OUT  ) :: gqd1   (ibMax, kMax)
    !
    !  local variables 
    !
    REAL   , DIMENSION(ibMax,kMax) :: TMP1
    REAL   , DIMENSION(ibMax,kMax) :: TM1
    REAL   , DIMENSION(ibMax,kMax) :: zlam
    REAL   , DIMENSION(ibMax,kMax) :: zphi
    REAL   , DIMENSION(ibMax,kMax) :: gdt
    REAL   , DIMENSION(ibMax,kMax) :: psint
    REAL   , DIMENSION(ibMax,kMax) :: adveps
    REAL   , DIMENSION(ibMax,kMax) :: divint
    REAL   , DIMENSION(ibMax,kMax) :: divintm
    REAL   , DIMENSION(ibMax,kMax) :: dot

    REAL   , DIMENSION(ibMax,kMax) :: gqu      
    REAL   , DIMENSION(ibMax,kMax) :: gqv      
    REAL   , DIMENSION(ibMax,kMax) :: gtu      
    REAL   , DIMENSION(ibMax,kMax) :: gtv      

    REAL                           :: rk
    INTEGER                        :: i
    INTEGER                        :: k
    REAL   , DIMENSION(ibMax)      :: cos2lat
    REAL   , DIMENSION(ibMax)      :: ercossin
    REAL   , DIMENSION(ibMax)      :: fcor
    REAL   , DIMENSION(ibMax)      :: cosiv
    INTEGER                        :: ncount
    INTEGER                        :: latco
    !--------------------------------------------------------------------------
    !  locations for available diagnostics in this subroutine
    !-------------------------------------------------------------------------- 
    ! nDiag_tmpsfc =  1 ! time mean surface pressure
    ! nDiag_omegav =  7 ! omega
    ! nDiag_sigdot =  8 ! sigma dot
    ! nDiag_pwater = 14 ! precipitable water     
    ! nDiag_divgxq = 51 ! divergence * specific humidity
    ! nDiag_vmoadv = 52 ! vertical moisture advection
    ! nDiag_tmtdps = 64 ! time mean deep soil temperature
    ! nDiag_tgfccv = 65 ! ground/surface cover temperature
    ! nDiag_tcanop = 66 ! canopy temperature
    !--------------------------------------------------------------------------              

    zlam=0.0
    zphi=0.0
    psint=0.0
    adveps=0.0
    divint=0.0
    divintm=0.0
    dot=0.0
    omg=0.0
    gyu=0.0
    gyv=0.0
    gtd=0.0
    gqd=0.0
    gyu1=0.0
    gyv1=0.0
    gtd1=0.0
    gqd1=0.0
    gvdlnp=0.0
    gvdlnpm=0.0
    gyum=0.0
    gyvm=0.0
    gtdm=0.0
    latco=jb
    rk= gasr/cp
    !cdir novector
    cos2lat(      1:ibLim) = 1.0/rcl(1:ibLim)
    cos2lat(ibLim+1:ibMax) = 0.0 
    !cdir novector
    ercossin(      1:ibLim) = COS(colrad(1:ibLim)) * rcl(1:ibLim) / er
    ercossin(ibLim+1:ibMax) = 0.0
    !cdir novector
    fcor(      1:ibLim) = twomg * COS(colrad(1:ibLim))
    fcor(ibLim+1:ibMax) = 0.0
    !cdir novector
    cosiv(      1:ibLim) = SQRT(rcl(1:ibLim))
    cosiv(ibLim+1:ibMax) = 0.0
    ! 
    !
    ! enforce humidity to be above a certain level (avoid negative values...)
    ! ----------------------------------------------------------------------
    !
    IF(isimp.ne.'YES ')THEN  
       gq=MAX(gq,qmin)
    ENDIF
    !
    IF(dodia(nDiag_tmtdps).or.dodia(nDiag_tgfccv).or.dodia(nDiag_tcanop))THEN
       ncount=0
       DO i=1,ibLim
          gdt(i,1)=gtsea (i,jb)
          gdt(i,2)=gtsea (i,jb)
          gdt(i,3)=gtsea (i,jb)
          IF(imask(i,jb).ge.1)THEN
             ncount=ncount+1
             gdt(i,1)=tcm(ncount,jb)
             gdt(i,2)=tgm(ncount,jb)
             gdt(i,3)=tdm(ncount,jb)
          END IF
       END DO
    END IF
    IF(dodia(nDiag_tmtdps))CALL updia(gdt(1,3),nDiag_tmtdps,latco)
    IF(dodia(nDiag_tgfccv))CALL updia(gdt(1,2),nDiag_tgfccv,latco)
    IF(dodia(nDiag_tcanop))CALL updia(gdt(1,1),nDiag_tcanop,latco)
    !
    !     obtain grid history fields if requested
    !
    IF(IsGridHistoryOn())THEN
       DO k=1,kMax
          DO i=1,ibLim
             gtd(i,k)=tov(k)+gtmp(i,k)
          END DO
       END DO

       IF(dogrh(nGHis_temper,latco)) CALL StoreGridHistory(gtd,nGHis_temper,latco)
       IF(dogrh(nGHis_uzonal,latco)) CALL StoreGridHistory( gu,nGHis_uzonal,latco,sqrt(rcl))
       IF(dogrh(nGHis_vmerid,latco)) CALL StoreGridHistory( gv,nGHis_vmerid,latco,sqrt(rcl))
       IF(dogrh(nGHis_spchum,latco)) CALL StoreGridHistory( gq,nGHis_spchum,latco)

       IF(dogrh(nGHis_tvirsf,latco)) CALL StoreGridHistory(gtd(:,1),nGHis_tvirsf,latco)
       IF(dogrh(nGHis_uzonsf,latco)) CALL StoreGridHistory( gu(:,1),nGHis_uzonsf,latco,sqrt(rcl))
       IF(dogrh(nGHis_vmersf,latco)) CALL StoreGridHistory( gv(:,1),nGHis_vmersf,latco,sqrt(rcl))
       IF(dogrh(nGHis_sphusf,latco)) CALL StoreGridHistory( gq(:,1),nGHis_sphusf,latco)

       IF(dogrh(nGHis_snowdp,latco)) CALL StoreGridHistory(sheleg(:,latco),nGHis_snowdp,latco)
       IF(dogrh(nGHis_rouglg,latco)) CALL StoreGridHistory(  zorl(:,latco),nGHis_rouglg,latco)

       ncount=0
       DO i=1,ibLim
          gtd(i,1)=gtsea(i,latco)
          gtd(i,2)=gtsea(i,latco)
          gtd(i,3)=gtsea(i,latco)
          gtd(i,4)=1.0
          gtd(i,5)=1.0
          gtd(i,6)=1.0
          gtd(i,7)=0.0e0
          gtd(i,8)=0.0e0
          gtd(i,9)=imask(i,latco)
          IF(imask(i,latco).ge.1)THEN
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

       IF(dogrh(nGHis_tcanop,latco)) CALL StoreGridHistory(gtd(:,1),nGHis_tcanop,latco)
       IF(dogrh(nGHis_tgfccv,latco)) CALL StoreGridHistory(gtd(:,2),nGHis_tgfccv,latco)
       IF(dogrh(nGHis_tgdeep,latco)) CALL StoreGridHistory(gtd(:,3),nGHis_tgdeep,latco)
       IF(dogrh(nGHis_swtsfz,latco)) CALL StoreGridHistory(gtd(:,4),nGHis_swtsfz,latco)
       IF(dogrh(nGHis_swtrtz,latco)) CALL StoreGridHistory(gtd(:,5),nGHis_swtrtz,latco)
       IF(dogrh(nGHis_swtrcz,latco)) CALL StoreGridHistory(gtd(:,6),nGHis_swtrcz,latco)
       IF(dogrh(nGHis_mostca,latco)) CALL StoreGridHistory(gtd(:,7),nGHis_mostca,latco,1000.0)
       IF(dogrh(nGHis_mostgc,latco)) CALL StoreGridHistory(gtd(:,8),nGHis_mostgc,latco,1000.0)
       IF(dogrh(nGHis_vegtyp,latco)) CALL StoreGridHistory(gtd(:,9),nGHis_vegtyp,latco)

    END IF
    gtd=0.0
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
    CALL vertint(psint,adveps,divint,divintm, &
         dot,gu,gv,gdiv,gdivm,gplam,gpphi,rcl,del,ibMax,kMax,ibLim)
    !
    ! using the vertical velocity @level (dot), compute the vertical 
    ! velocity @ layer (gw)
    !
    DO k = 1, kmax -1
       gw(:,k) = 0.5d0 * (dot(:,k+1) + dot(:,k))
    END DO
    k = kmax
    gw(:,k) = 0.5d0 * dot(:,k) 
    !
    !     computation of omega
    !     --------------------
    !
    CALL omega(omg,psint,adveps,divint,dot,ps,sl,ibMax,kMax,ibLim)
    !
    !     computation of geopotential gradient
    !     ------------------------------------
    !
    CALL delgeo(gtlamm,zlam,gzslam,gtphim,zphi,gzsphi,hmt,ibMax,ibLim,kMax)
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
    ENDIF
    !
    !     metric term
    !     -----------
    !
    IF ((.not. slagr) .or. (.not. tanplane)) THEN
       CALL metric(gu,gv,gyv,ercossin,ibMax,kMax,ibLim)
    END IF
    !
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
    !     cfl controlling damping
    !     -----------------------
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
       !     horizontal advection of humidity 
       !     --------------------------------
       !
       CALL hadvec(gu,gv,gqlam,gqphi,gqd,rcl,ibMax,kMax,ibLim)
       !
       !     vertical advection of humidity
       !     ------------------------------
       !
       CALL vadvec(gq,dot,rdel2,gqd,ibMax,kMax,ibLim)
       !
       !     log pressure tendency
       !     --------------------- 
       !
       gvdlnp(      1:ibLim) = - psint(1:ibLim,kMax)
       !
    ENDIF

    IF(dodia(nDiag_divgxq)) CALL updia(psint, nDiag_divgxq,latco)
    IF(dodia(nDiag_vmoadv)) CALL updia(divint,nDiag_vmoadv,latco)
    IF(dodia(nDiag_omegav)) CALL updia(omg,   nDiag_omegav,latco)
    !
    !
    !     tendency from old time-step
    !     ---------------------------
    !
    !
    IF (IsGridHistoryOn()) THEN
       IF(dogrh(nGHis_presfc,latco)) CALL StoreGridHistory(10.0*ps,nGHis_presfc,latco)
    END IF
    !     
    !     sigma gke computed only at interior interfaces.
    !
    !
    IF(dodia(nDiag_sigdot))CALL updia(dot,nDiag_sigdot,latco)


    DO k=1,kMax
       DO i=1,ibLim
          gtu(i,k)=gtmp(i,k)*gu(i,k)
          gtv(i,k)=gtmp(i,k)*gv(i,k)
          gqu(i,k)=gu(i,k)*gq(i,k)
          gqv(i,k)=gv(i,k)*gq(i,k)
       END DO
    END DO

    CALL tndtold(gyum,gyvm,gtdm,gvdlnpm,zlam,zphi,gplamm,gpphim,divintm, &
         rdel2,ci,h1,h2,tov,gasr,rk,dot,ibMax,kMax,ibLim)

    IF(isimp.ne.'YES ')THEN  
       !
       !     gplam surface pressure in mb
       !
       DO k=1,kMax
          DO i=1,ibLim
             TMP1(i,k)= gtmp(i,k)+tov(k)
             TM1(i,k) = gtm(i,k)+tov(k)
          END DO
       END DO

!**(JP)** Isola as tendencias da fisica das outras tendencias, para comparar com modelo anterior;
!         zera gyu1, gyv1, gtd1, gqd1 e soma, na saida, as tendencias
!         dos outros processos ja calculadas (gyu, gyv, gtd, gqd)

       CALL DryPhysics & 
            (TM1    ,gqm   ,gum   ,gvm   ,10.0*ps,gyu1   ,gyv1   ,gtd1  ,&
            gqd1   ,colrad,ifday ,tod   ,TMP1   ,gq     ,omg    ,jb    ,&
            lonrad,glnpm  ,cos2d ,intcosz  )

       gyu = gyu + gyu1
       gyv = gyv + gyv1
       gtd = gtd + gtd1
       gqd = gqd + gqd1

!**(JP)** fim da mudanca para comparacao;

       DO k=1,kMax
          DO i=1,ibLim
             gtm(i,k)=TM1(i,k)-tov(k)
          END DO
       END DO
       !     
       !     diagnostic of precipitable water
       !
       CALL pwater(gq    ,dot   ,exp(glnp) ,del   ,ibMax,ibLim ,kMax  )
       IF(dodia(nDiag_pwater))CALL updia(dot,nDiag_pwater,latco)
       !
       !     compute vertical integral of v*q and u*q for later calculation
       !     of horizontal moisture flux convergence
       !
       CALL pwater(gqu   ,psint(:,1),exp(glnp) ,del   ,ibMax,ibLim ,kMax  )
       CALL pwater(gqv   ,psint(:,2),exp(glnp) ,del   ,ibMax,ibLim ,kMax  )
       !
       !     cb(,1) and cb(,2) correspond to gpphi and glnp on transform
       !
    ELSE
       ! Simplified physics
       !
       CALL SimpPhys(gu, gv, gtmp, gyv, gyu, gtd, ibMax, ibLim, kMax, jb)
       !
    END IF
    !
    !     diagnostic of time mean surface pressure
    !
    !
    IF(dodia(nDiag_tmpsfc))CALL updia(exp(glnp)  ,nDiag_tmpsfc,latco)
    !
  END SUBROUTINE GrpComp






  SUBROUTINE delwind(ulam, vlam, vor, div, uphi, vphi, cos2lat, &
       ibMax, kMax, ibLim)
    INTEGER, INTENT(IN ) :: ibMax
    INTEGER, INTENT(IN ) :: kMax
    REAL,    INTENT(IN ) :: cos2lat(ibMax)
    REAL,    INTENT(IN ) :: ulam(ibMax,kMax)
    REAL,    INTENT(IN ) :: vlam(ibMax,kMax)
    REAL,    INTENT(IN ) :: div (ibMax,kMax)
    REAL,    INTENT(IN ) :: vor (ibMax,kMax)
    REAL,    INTENT(OUT) :: uphi(ibMax,kMax)
    REAL,    INTENT(OUT) :: vphi(ibMax,kMax)
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






  SUBROUTINE vertint(psint, adveps, divint, divintm, dot, &
       u, v, div, divm, plam, pphi, rcl, del, ibMax, kMax, ibLim)
    INTEGER, INTENT(IN ) :: ibMax
    INTEGER, INTENT(IN ) :: kMax
    INTEGER, INTENT(IN ) :: ibLim
    REAL,    INTENT(IN ) :: u(ibMax,kMax)
    REAL,    INTENT(IN ) :: v(ibMax,kMax)
    REAL,    INTENT(IN ) :: div(ibMax,kMax)
    REAL,    INTENT(IN ) :: divm(ibMax,kMax)
    REAL,    INTENT(IN ) :: plam(ibMax)
    REAL,    INTENT(IN ) :: pphi(ibMax)
    REAL,    INTENT(IN ) :: del(kMax)
    REAL,    INTENT(IN ) :: rcl(ibMax)
    REAL,    INTENT(OUT) :: psint(ibMax,kMax)
    REAL,    INTENT(OUT) :: divint(ibMax,kMax)
    REAL,    INTENT(OUT) :: divintm(ibMax,kMax)
    REAL,    INTENT(OUT) :: adveps(ibMax,kMax)
    REAL,    INTENT(OUT) :: dot(ibMax,kMax)
    !      
    !      Computes the vertical integral (in finite differences)
    !      of the divergence field (to be stored in divint). The scalar product  
    !      of the wind field (in each level) with the gradient of the surface
    !      pressure is stored in adveps. Its vertical integral is stored in psint.
    !      The vertical velocity (sigma_dot) is stored in dot.
    !
    INTEGER :: i, k
    !
    adveps   = 0.0
    psint    = 0.0
    divint   = 0.0
    divintm  = 0.0

    DO k=1,kMax
       DO i=1,ibLim
          adveps(i,k) = rcl(i)*(u(i,k) * plam(i) + v(i,k) * pphi(i) )
       ENDDO
    ENDDO
    k=1   
    DO i=1,ibLim
       dot(i,k) = 0.0
       psint(i,k) = del(k) * adveps(i,k)
       divint(i,k) = del(k) * div(i,k)
       divintm(i,k) = del(k) * divm(i,k)
    ENDDO
    DO k=2,kMax
       DO i=1,ibLim
          psint(i,k) = psint(i,k-1) + del(k) * adveps(i,k)
          divint(i,k) = divint(i,k-1) + del(k) * div(i,k)
          divintm(i,k) = divintm(i,k-1) + del(k) * divm(i,k)
       ENDDO
    ENDDO
    DO k=1,kMax-1
       DO i=1,ibLim
          dot(i,k+1)=dot(i,k) + del(k) * ( divint(i,kMax) + psint(i,kMax) &
               - div(i,k) - adveps(i,k) )
       ENDDO
    ENDDO
    psint  (ibLim+1:ibMax,:) = 0.0
    divint (ibLim+1:ibMax,:) = 0.0
    divintm(ibLim+1:ibMax,:) = 0.0
    adveps (ibLim+1:ibMax,:) = 0.0
    dot    (ibLim+1:ibMax,:) = 0.0
  END SUBROUTINE vertint






  SUBROUTINE omega(omg, psint, adveps, divint, dot, ps, sl, &
       ibMax, kMax, ibLim)
    INTEGER, INTENT(IN ) :: ibMax, ibLim, kMax
    REAL,    INTENT(IN ) :: divint(ibMax,kMax)
    REAL,    INTENT(IN ) :: adveps(ibMax,kMax)
    REAL,    INTENT(IN ) :: dot(ibMax,kMax)
    REAL,    INTENT(IN ) :: psint(ibMax,kMax)
    REAL,    INTENT(IN ) :: ps(ibMax)
    REAL,    INTENT(IN ) :: sl(kMax)
    REAL,    INTENT(OUT) :: omg(ibMax,kMax)
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
               - 0.5 * ( dot(i,k+1) + dot(i,k) ) )
       ENDDO
    ENDDO
    k=kMax
    DO i=1,ibLim
       omg(i,k) = ps(i) * ( sl(k) * &
            ( adveps(i,k) - psint(i,kMax) - divint(i,kMax) ) &
            - 0.5 * dot(i,k) )
    ENDDO
    omg(ibLim+1:ibMax,:) = 0.0
  END SUBROUTINE omega






  SUBROUTINE delgeo (tlam, zlam, zslam, tphi, zphi, zsphi, hmt, imx, imax, kmax)
    INTEGER, INTENT(IN ) :: imx
    INTEGER, INTENT(IN ) :: imax
    INTEGER, INTENT(IN ) :: kmax
    REAL,    INTENT(IN ) :: tlam (imx,kmax)
    REAL,    INTENT(OUT) :: zlam (imx,kmax)
    REAL,    INTENT(IN ) :: zslam(imx)
    REAL,    INTENT(IN ) :: tphi (imx,kmax)
    REAL,    INTENT(IN ) :: zsphi(imx)
    REAL,    INTENT(OUT) :: zphi (imx,kmax)
    REAL,    INTENT(IN ) :: hmt  (kmax,kmax)
    !      
    !      From the derivatives of temperature and surface geopotential 
    !      computes the values of the gradient of the geopotential
    !      (using the hydrostatic equation:  z = HM^T * T + zs )
    !
    INTEGER :: k
    !
    DO k = 1, kmax
       zlam(1:imax,k) = zslam(1:imax)
       zphi(1:imax,k) = zsphi(1:imax)
    END DO

    zlam(1:imax,:) = zlam(1:imax,:) + MATMUL(tlam(1:imax,:),hmt)
    zphi(1:imax,:) = zphi(1:imax,:) + MATMUL(tphi(1:imax,:),hmt)
  END SUBROUTINE delgeo






  SUBROUTINE hadvec(u, v, flam, fphi, tend, rcl, ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL,    INTENT(IN   ) :: rcl(ibMax) ! 1. / ( cos(lat)**2 )
    REAL,    INTENT(IN   ) :: u(ibMax,kMax)
    REAL,    INTENT(IN   ) :: v(ibMax,kMax)
    REAL,    INTENT(IN   ) :: flam(ibMax,kMax)
    REAL,    INTENT(IN   ) :: fphi(ibMax,kMax)
    REAL,    INTENT(INOUT) :: tend(ibMax,kMax)
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
    tend(ibLim+1:ibMax,:) = 0.0
  END SUBROUTINE hadvec





  SUBROUTINE vadvec(f, dot, rdel2, tend, ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL,    INTENT(IN   ) :: f(ibMax,kMax)
    REAL,    INTENT(IN   ) :: dot(ibMax,kMax)
    REAL,    INTENT(IN   ) :: rdel2(kMax)
    REAL,    INTENT(INOUT) :: tend(ibMax,kMax)
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
    tend(ibLim+1:ibMax,:)=0.0
  END SUBROUTINE vadvec






  SUBROUTINE metric(u, v, tend, ercossin, ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL,    INTENT(IN   ) :: ercossin(ibMax) ! sin(lat) / ( er * cos(lat)**2 )
    REAL,    INTENT(IN   ) :: u(ibMax,kMax)
    REAL,    INTENT(IN   ) :: v(ibMax,kMax)
    REAL,    INTENT(INOUT) :: tend(ibMax,kMax)
    INTEGER :: i, k
    !      
    ! Computes the metric term and add its contribution  to current v-tendency
    !     ercossin(1:ibLim) = COS(colrad(1:ibLim)) * rcl(1:ibLim) / er
    !                                             1.0/cos(latitude)**2.
    !     ercossin(1:ibLim) =   1.0/cos(latitude)/ er
    !                                             
    !
    DO k=1,kMax   
       DO i=1,ibLim
          tend(i,k) = tend(i,k) - ercossin(i)*(u(i,k)*u(i,k) + v(i,k)*v(i,k))
       END DO
    END DO
    tend(ibLim+1:ibMax,:)=0.0
  END SUBROUTINE metric






  SUBROUTINE coriol(u, v, tendu, tendv, fcor, ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL,    INTENT(IN   ) :: fcor(ibMax) ! 2 * omega * sin(phi)
    REAL,    INTENT(IN   ) :: u(ibMax,kMax)
    REAL,    INTENT(IN   ) :: v(ibMax,kMax)
    REAL,    INTENT(INOUT) :: tendu(ibMax,kMax)
    REAL,    INTENT(INOUT) :: tendv(ibMax,kMax)
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
    tendu(ibLim+1:ibMax,:) = 0.0
    tendv(ibLim+1:ibMax,:) = 0.0
  END SUBROUTINE coriol






  SUBROUTINE nlprgr(plam, pphi, tmp, tendu, tendv, rc, ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL,    INTENT(IN   ) :: rc
    REAL,    INTENT(IN   ) :: tmp(ibMax,kMax)
    REAL,    INTENT(IN   ) :: plam(ibMax)
    REAL,    INTENT(IN   ) :: pphi(ibMax)
    REAL,    INTENT(INOUT) :: tendu(ibMax,kMax)
    REAL,    INTENT(INOUT) :: tendv(ibMax,kMax)
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
    tendu(ibLim+1:ibMax,:)=0.0
    tendv(ibLim+1:ibMax,:)=0.0
  END SUBROUTINE nlprgr






  SUBROUTINE vadvtmp(tmp, p1, p2, h1, h2, dot, psint, &
       ci, rdel2, tend, ibMax, kMax, ibLim, slagr)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    LOGICAL, INTENT(IN   ) :: slagr
    REAL,    INTENT(IN   ) :: tmp(ibMax,kMax)
    REAL,    INTENT(IN   ) :: dot(ibMax,kMax)
    REAL,    INTENT(IN   ) :: psint(ibMax,kMax)
    REAL,    INTENT(IN   ) :: rdel2(kMax)
    REAL,    INTENT(IN   ) :: p1(kMax)
    REAL,    INTENT(IN   ) :: p2(kMax)
    REAL,    INTENT(IN   ) :: h1(kMax)
    REAL,    INTENT(IN   ) :: h2(kMax)
    REAL,    INTENT(IN   ) :: ci(kMax+1)
    REAL,    INTENT(INOUT) :: tend(ibMax,kMax)
    INTEGER :: i, k
    REAL :: w1(ibMax,kMax), w2(ibMax,kMax), w3(ibMax,kMax)   ! work space
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
             w1(i,k) = ( 1.0 - p2(k+1) ) * tmp(i,k)
             w2(i,k+1) = ( p1(k) - 1.0 ) * tmp(i,k+1)
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
    tend(ibLim+1:ibMax,:)=0.0
  END SUBROUTINE vadvtmp






  SUBROUTINE tmptend(tend, tmp, tov, psint, adveps, divint, rk, &
       ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL,    INTENT(IN   ) :: tov(kMax)
    REAL,    INTENT(IN   ) :: rk
    REAL,    INTENT(IN   ) :: psint(ibMax,kMax)
    REAL,    INTENT(IN   ) :: divint(ibMax,kMax)
    REAL,    INTENT(IN   ) :: adveps(ibMax,kMax)
    REAL,    INTENT(IN   ) :: tmp(ibMax,kMax)
    REAL,    INTENT(INOUT) :: tend(ibMax,kMax)
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
    tend(ibLim+1:ibMax,:)=0.0
  END SUBROUTINE tmptend






  SUBROUTINE tndtold(tdu, tdv, tdt, tdlnp, zlam, zphi, plam, pphi, divint, &
       rdel2, ci, h1, h2, tov, rc, rk, w, ibMax, kMax, ibLim)
    INTEGER, INTENT(IN   ) :: ibMax
    INTEGER, INTENT(IN   ) :: kMax
    INTEGER, INTENT(IN   ) :: ibLim
    REAL,    INTENT(IN   ) :: rc
    REAL,    INTENT(IN   ) :: rk
    REAL,    INTENT(IN   ) :: zlam(ibMax,kMax)
    REAL,    INTENT(IN   ) :: zphi(ibMax,kMax)
    REAL,    INTENT(IN   ) :: plam(ibMax)
    REAL,    INTENT(IN   ) :: pphi(ibMax)
    REAL,    INTENT(IN   ) :: tov(kMax)
    REAL,    INTENT(IN   ) :: divint(ibMax,kMax)
    REAL,    INTENT(IN   ) :: ci(kMax+1)
    REAL,    INTENT(IN   ) :: h1(kMax)
    REAL,    INTENT(IN   ) :: h2(kMax)
    REAL,    INTENT(IN   ) :: rdel2(kMax)
    REAL,    INTENT(OUT  ) :: w(ibMax,kMax)
    REAL,    INTENT(INOUT) :: tdu(ibMax,kMax)
    REAL,    INTENT(INOUT) :: tdv(ibMax,kMax)
    REAL,    INTENT(INOUT) :: tdt(ibMax,kMax)
    REAL,    INTENT(INOUT) :: tdlnp(ibMax)
    INTEGER :: i, k
    REAL :: half=0.5e0

    !      
    ! Computes the part of the tendencies relative to the old time-step
    !
    !
    !   pressure gradient terms
    !   -----------------------
    DO k=1,kMax   
       DO i=1,ibLim
          tdu(i,k) = - half * ( zlam(i,k) + rc * tov(k) * plam(i) )
          tdv(i,k) = - half * ( zphi(i,k) + rc * tov(k) * pphi(i) )
       ENDDO
    ENDDO
    !
    !   log pressure tendency
    !   ---------------------
    DO i=1,ibLim
       tdlnp(i) = - half * divint(i,kMax)
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
       tdt(i,k) =  - half * ( rk * tov(k) * divint(i,kMax) &
            + rdel2(k) * h1(k) * w(i,k) )
    ENDDO
    DO k=2,kMax-1
       DO i=1,ibLim
          tdt(i,k) = - half * ( rk * tov(k) * divint(i,kMax) &
               + rdel2(k) * ( h1(k) * w(i,k) + h2(k) * w(i,k-1) ) )
       ENDDO
    ENDDO
    k=kMax
    DO i=1,ibLim
       tdt(i,k) = - half * ( rk * tov(k) * divint(i,kMax) &
            + rdel2(k) *  h2(k) * w(i,k-1) )
    ENDDO
    tdu(ibLim+1:ibMax,:)=0.0
    tdv(ibLim+1:ibMax,:)=0.0
    tdlnp(ibLim+1:ibMax)=0.0
    tdt(ibLim+1:ibMax,:)=0.0
  END SUBROUTINE tndtold

  !
  ! addtend: finish tendency computations, adding contributions from
  !          old and current time step.

  SUBROUTINE AddTend(dt, nlnminit, jbFirst, jbLast)
    REAL,    INTENT(IN) :: dt
    LOGICAL, INTENT(IN) :: nlnminit
    INTEGER, INTENT(IN) :: jbFirst
    INTEGER, INTENT(IN) :: jbLast
    INTEGER :: ib, jb, k
    REAL :: dt2
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
                fgqd(ib,k,jb) = fgqm(ib,k,jb) + &
                     dt2 * fgqd(ib,k,jb) 
             END DO
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
                fgyu(ib,k,jb) = fgyu(ib,k,jb) + 2. * fgyum(ib,k,jb)
                fgyv(ib,k,jb) = fgyv(ib,k,jb) + 2. * fgyvm(ib,k,jb)
                fgtd(ib,k,jb) = fgtd(ib,k,jb) + 2. * fgtdm(ib,k,jb)
                fgqd(ib,k,jb) = fgqd(ib,k,jb)
             END DO
          END DO
          DO ib = 1, ibMaxPerJB(jb)
             fgvdlnp(ib,jb) = fgvdlnp(ib,jb) + 2. * fgvdlnpm(ib,jb)
          END DO
       END DO

    ENDIF
  END SUBROUTINE AddTend


  ! TimeFilterStep1: First part of the asselin/robert time-filter
  ! (computes a partially filtered value of fold)

  SUBROUTINE TimeFilterStep1(fa, fb, jbFirst, jbLast)
    REAL,    INTENT(IN) :: fa
    REAL,    INTENT(IN) :: fb
    INTEGER, INTENT(IN) :: jbFirst
    INTEGER, INTENT(IN) :: jbLast
    INTEGER :: ib, jb, k

    DO jb = jbFirst, jbLast
       DO k = 1, kMax
          DO ib = 1, ibMaxPerJB(jb)
             fgtmpmm (ib,k,jb) = fgtmpm   (ib,k,jb)
             fgtmpm  (ib,k,jb) = fa*fgtmp (ib,k,jb) + fb*fgtmpm (ib,k,jb)
             fgdivmm (ib,k,jb) = fgdivm   (ib,k,jb)
             fgdivm  (ib,k,jb) = fa*fgdiv (ib,k,jb) + fb*fgdivm (ib,k,jb)
             fgumm   (ib,k,jb) = fgum     (ib,k,jb)
             fgum    (ib,k,jb) = fa*fgu   (ib,k,jb) + fb*fgum   (ib,k,jb)
             fgvmm   (ib,k,jb) = fgvm     (ib,k,jb)
             fgvm    (ib,k,jb) = fa*fgv   (ib,k,jb) + fb*fgvm   (ib,k,jb)
             fgqmm   (ib,k,jb) = fgqm     (ib,k,jb)
             fgqm    (ib,k,jb) = fa*fgq   (ib,k,jb) + fb*fgqm   (ib,k,jb)
             fgtlammm(ib,k,jb) = fgtlamm  (ib,k,jb)
             fgtlamm (ib,k,jb) = fa*fgtlam(ib,k,jb) + fb*fgtlamm(ib,k,jb)
             fgtphimm(ib,k,jb) = fgtphim  (ib,k,jb)
             fgtphim (ib,k,jb) = fa*fgtphi(ib,k,jb) + fb*fgtphim(ib,k,jb)
          END DO
       END DO
       DO ib = 1, ibMaxPerJB(jb)
          fglnpmm (ib,jb) = fglnpm(ib,jb)
          fglnpm  (ib,jb) = fa*fglnps(ib,jb) + fb*fglnpm (ib,jb)
          fgplammm(ib,jb) = fgplamm(ib,jb) 
          fgplamm (ib,jb) = fa*fgplam(ib,jb) + fb*fgplamm(ib,jb)
          fgpphimm(ib,jb) = fgpphim(ib,jb)
          fgpphim (ib,jb) = fa*fgpphi(ib,jb) + fb*fgpphim(ib,jb)
       END DO
    END DO
  END SUBROUTINE TimeFilterStep1


  ! TimeFilterStep2: Second part of the asselin/robert time-filter
  ! (the partially filtered value of fold is filtered completely)

  SUBROUTINE TimeFilterStep2(fb1, jbFirst, jbLast)
    REAL,    INTENT(IN) :: fb1
    INTEGER, INTENT(IN) :: jbFirst
    INTEGER, INTENT(IN) :: jbLast
    INTEGER :: ib, jb, k

    DO jb = jbFirst, jbLast
       DO k = 1, kMax
          DO ib = 1, ibMaxPerJB(jb)
             fgtmpm (ib,k,jb) = fgtmpm (ib,k,jb) + fb1*fgtmp (ib,k,jb)
             fgdivm (ib,k,jb) = fgdivm (ib,k,jb) + fb1*fgdiv (ib,k,jb)
             fgum   (ib,k,jb) = fgum   (ib,k,jb) + fb1*fgu   (ib,k,jb)
             fgvm   (ib,k,jb) = fgvm   (ib,k,jb) + fb1*fgv   (ib,k,jb)
             fgqm   (ib,k,jb) = fgqm   (ib,k,jb) + fb1*fgq   (ib,k,jb)
             fgtlamm(ib,k,jb) = fgtlamm(ib,k,jb) + fb1*fgtlam(ib,k,jb)
             fgtphim(ib,k,jb) = fgtphim(ib,k,jb) + fb1*fgtphi(ib,k,jb)
          END DO
       END DO
       DO ib = 1, ibMaxPerJB(jb)
          fglnpm (ib,jb) = fglnpm (ib,jb) + fb1*fglnps(ib,jb)
          fgplamm(ib,jb) = fgplamm(ib,jb) + fb1*fgplam(ib,jb)
          fgpphim(ib,jb) = fgpphim(ib,jb) + fb1*fgpphi(ib,jb)
       END DO
    END DO
  END SUBROUTINE TimeFilterStep2
END MODULE GridDynamics
