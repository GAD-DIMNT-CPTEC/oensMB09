!
!  $Author: pkubota $
!  $Date: 2011/04/20 14:34:46 $
!  $Revision: 1.17 $
!
MODULE ModTimeStep

  USE Parallelism, ONLY: &
       MsgOne,           &
       mygroup_four,     &
       myId,             &
       maxNodes

  USE Communications, ONLY: &
       Spread_surf_Spec,    &
       Collect_Grid_Red
  USE Sizes, ONLY: myMMap,lm2m,myMMax,myMNMap
  USE Options, ONLY: nscalars, MasCon_ps, SL_twotime_scheme
  USE Constants, ONLY: r8, pai, tov
  INCLUDE 'mpif.h'
  PRIVATE
  PUBLIC :: TimeStep
  PUBLIC :: SfcGeoTrans
  PUBLIC :: InitBoundSimpPhys

  INTEGER :: jbGlob  ! loop index, global among threads
CONTAINS


  ! timestep: performs a time-step of the model. Through the values
  !           of fb1, fa and fb we control if this is a initial
  !           time step,  part of a cold start or if it is a normal
  !           time step. 


  SUBROUTINE TimeStep (fb1, fa, fb, &
       slagr, nlnminit, &
       idiaten, enhdif, dt,  &
       jdt, ifday, tod, idatec, initial)
    USE Constants, ONLY:     &
         coriol             , &
         root2              

    USE Sizes, ONLY:         &
         ibMax              , &
         jbMax              , &
         ijmax              , &
         mymnmax            , &
         mymnextmax         , &
         myfirstlev         , &
         myfirstlat         , &
         mylastlat          , &
         myfirstlat_diag    , &
         mylastlat_diag     , &
         myfirstlon         , &
         mylastlon          , &
         nodehasm           , &
         ibMaxPerJB         , &
         jPerIJB            , &
         iPerIJB            , & 
         ibperij            , & 
         jbperij            , & 
         imaxperj           , & 
         mnMax              , &
         mnMax_si           , &
         mnExtMax           , &
         mMax               , &
         jMax               , &
         kMax               , &
         kMaxloc            , &
         iMax               , &
         ngroups_four       , &
         havesurf           , &
         ThreadDecomp,        &
         sl

    USE Utils, ONLY :    &
         lati               , &
         long               , &
         colrad2d           , &
         colrad             , &
         lonrad             , &
         rcl                , &
         cos2lat            , &
         ercossin           , &
         fcor               , &
         cosiv              , &
         cosz               , &
         cos2d              , &
         vmax               , &
         vaux               , &
         vmaxVert           , &
         total_mass         , &
         rcs2

    USE FieldsDynamics, ONLY: &
         fgyu, fgyv, fgtd, fgqd, fgvdlnp, &
         fgu, fgv, fgw, fgum, fgvm, fgwm, fgtmpm, &
         fgqm, fglnpm, fgyum, fgyvm, fgtdm, fgqdm, fgvdlnpm, &
         fgdiv, fgtmp, fgrot, fgq, fgplam, fgpphi, fglnps,  &
         omg, fgps, fgtlam, fgtphi, fgqlam, fgqphi, fgulam,  &
         fguphi, fgvlam, fgvphi, fgtlamm, fgtphim, fgplamm, &
         fgpphim, fgdivm, fgzs, fgzslam, fgzsphi, fgqmm, &
         fgqp, fgtmpp, fgpsp, qlnpt, qtmpt, &
         qdivp, qdivt, qup, qvp, qlnpp, &
         qqp, qqt, qrotp, qrott, qtmpp, qdiaten, &
         fgpass_scalars, adr_scalars


    USE FieldsPhysics, ONLY:  &
         ustr               , &
         vstr               , &
         imask              , &
         gtsea              , &
         PBL_CoefKm         , &
         PBL_CoefKh !hmjb            



    USE GridDynamics, ONLY:   &
         AddTend            , &
         GrpComp            , &
         GlobConservation   , &
         init_globconserv   , &
         do_globconserv     , &
         Scalardiffusion    , &
         UpdateConserv      , &
         TimeFilterStep1    , &
         TimeFilterStep2

    USE SpecDynamics, ONLY:   &
         FiltDiss,            &
         ImplDifu,            &
         SemiImpl_si,         &
         SemiImpl

    USE SemiLagrangian, ONLY: &
         SemiLagr, SemiLagr_2tl

    USE PhysicsDriver, ONLY :  &
         HumidPhysics,         &
         InitSimpPhys

    USE Options, ONLY :       &
         isimp              , &
         cdhl               , &
         first              , &
         nfdhn              , &
         vcrit              , &
         alpha              , &
         yrl                , &
         monl               , &
         intcosz            , &
         initlz             , &
         trint 

    USE IOLowLevel, ONLY: &
         WriteField         , &
         WriteDiagHead

    USE ModRadiationDriver,  ONLY:    &
         coszmed

    !--(DMK-CCATT-INI)------------------------------------------------------
!   USE ChemSourcesDriver, ONLY: &
!        MCGASourcesDriver
!
!   USE Constants, ONLY: &
!        gasr,           &
!        grav,           &
!        tov
!
!   USE Sizes, ONLY: &
!        si,         &
!        sl,         &
!        del
!
!   USE FieldsPhysics, ONLY: &
!        xland,              &
!        htdisp,             &
!        tsfc0,              &
!        sigki
    !--(DMK-CCATT-FIM)------------------------------------------------------

    IMPLICIT NONE
    REAL(KIND=r8),    INTENT(IN) :: fb1
    REAL(KIND=r8),    INTENT(IN) :: fa
    REAL(KIND=r8),    INTENT(IN) :: fb
    LOGICAL, INTENT(IN) :: slagr
    LOGICAL, INTENT(IN) :: nlnminit    
    LOGICAL, INTENT(IN) :: idiaten   
    LOGICAL, INTENT(IN) :: enhdif
    REAL(KIND=r8),    INTENT(IN) :: dt
    INTEGER, INTENT(IN) :: jdt
    INTEGER, INTENT(IN) :: ifday
    INTEGER, INTENT(IN) :: initial
    REAL(KIND=r8),    INTENT(IN) :: tod
    INTEGER, INTENT(IN) :: idatec(4)
    INTEGER  :: jb 
    INTEGER  :: ib
    INTEGER  :: ierr
    INTEGER  :: k
    INTEGER  :: mn
    INTEGER  :: j


    REAL(KIND=r8)     :: aux3(ijmax)
    REAL(KIND=r8)     :: delta2
    INTEGER  :: mnFirst
    INTEGER  :: mnLast
    INTEGER  :: mnRIFirst
    INTEGER  :: mnRILast
    INTEGER  :: mnRIFirst_si
    INTEGER  :: mnRILast_si
    INTEGER  :: mnExtFirst
    INTEGER  :: mnExtLast
    INTEGER  :: jFirst
    INTEGER  :: jLast
    INTEGER  :: jFirst_d
    INTEGER  :: jLast_d
    INTEGER  :: jbFirst
    INTEGER  :: jbLast
    INTEGER  :: kFirst
    INTEGER  :: kLast
    INTEGER  :: kFirstloc
    INTEGER  :: kLastloc
    LOGICAL  :: inistep
    CHARACTER(LEN=*), PARAMETER :: h="**(TimeStep)**"
    CHARACTER(LEN=3) :: c0
    CHARACTER(LEN=8) :: c1, c2, c3, c4


    WRITE(c3,"(i8)") jdt
    WRITE(c1,"(i8)") ifday
    WRITE(c2,"(f8.1)") tod
    WRITE(c4,"(f8.1)") dt
    CALL MsgOne(h," timestep "//TRIM(ADJUSTL(c3))//&
         " of length "//TRIM(ADJUSTL(c4))//&
         " seconds at simulation time "//TRIM(ADJUSTL(c1))//&
         " days and "//TRIM(ADJUSTL(c2))//" seconds")


    WRITE(c0,"(i3.3)") jdt
    CALL ThreadDecomp(1, mymnMax, mnFirst, mnLast, "TimeStep"//c0)
    CALL ThreadDecomp(1, mymnExtMax, mnExtFirst, mnExtLast, "TimeStep"//c0)
    CALL ThreadDecomp(1, 2*mymnMax, mnRIFirst, mnRILast, "TimeStep"//c0)
    CALL ThreadDecomp(1, 2*mnMax_si, mnRIFirst_si, mnRILast_si, "TimeStep"//c0)
    CALL ThreadDecomp(myfirstlat,mylastlat, jFirst, jLast, "TimeStep"//c0)
    CALL ThreadDecomp(myfirstlat_diag,mylastlat_diag,jFirst_d,jLast_d,"TimeStep"//c0)
    CALL ThreadDecomp(1, jbMax, jbFirst, jbLast, "TimeStep"//c0)
    CALL ThreadDecomp(1, kMax, kFirst, kLast, "TimeStep"//c0)
    CALL ThreadDecomp(1, kMaxloc, kFirstloc, kLastloc, "TimeStep"//c0)

    DO j = jFirst, jLast
       CALL coszmed(idatec,tod,yrl,lati(j),long,cosz(j),iMax)
    END DO
    !$OMP BARRIER

    delta2 = dt
    IF(slagr.and.SL_twotime_scheme) delta2 = dt/2._r8
    ! This j loop produces cosz, used by next jb loop
    DO jb = jbFirst, jbLast
       DO ib = 1, ibMaxPerJB(jb)
          j = jPerIJB(ib,jb)
          cos2d(ib,jb)=cosz(j)
       END DO
       IF (cdhl(jdt)) THEN
          DO ib = 1, ibMax
             ustr(ib,jb) = 0.0_r8
             vstr(ib,jb) = 0.0_r8
          END DO
       END IF
       DO k = 1, kMax
          vmax(k,jb) = 0.0_r8
       END DO
    END DO

    DO k = kFirst, kLast
       vmaxVert(k) = 0.0_r8
    END DO
    !
    !  Spectral to Grid-Point transforms
    !  ---------------------------------    
    !
    CALL BackTrans(slagr)
    !$OMP BARRIER
    !
    !  Complete filtering of previous time-step variables
    !  --------------------------------------------------
    !
    inistep = init_globconserv
    IF (MasCon_ps.and.init_globconserv) THEN
       !$OMP BARRIER
       CALL GlobConservation(jFirst, jLast, jbFirst, jbLast, &
                             jFirst_d, jLast_d)
    ENDIF
    IF (fb1.ne.0.0_r8) THEN
       !$OMP BARRIER
       CALL TimeFilterStep2(fb1, jbFirst, jbLast)
    END IF
    !
    ! Grid-point computations over latitudes
    ! --------------------------------------
    !
    !$OMP SINGLE
    jbGlob = 0
    !$OMP END SINGLE
    init_globconserv = .FALSE.
    DO
       !$OMP CRITICAL(jbb)
       jbGlob = jbGlob + 1
       jb = jbGlob
       !$OMP END CRITICAL(jbb)
       IF (jb > jbMax) EXIT
       CALL grpcomp ( &
            fgyu    (1,1,jb), fgyv    (1,1,jb), fgtd    (1,1,jb), fgqd    (1,1,jb), &
            fgvdlnp (1  ,jb), fgdiv   (1,1,jb), fgtmp   (1,1,jb), fgrot   (1,1,jb), &
            fgu     (1,1,jb), fgv     (1,1,jb), fgw     (1,1,jb), fgq     (1,1,jb), &
            fgplam  (1  ,jb), fgpphi  (1  ,jb), fgum    (1,1,jb), fgzs    (1  ,jb), &
            fgvm    (1,1,jb), fgtmpm  (1,1,jb), fgqm    (1,1,jb), omg     (1,1,jb), &
            fgps    (1  ,jb), fgtlam  (1,1,jb), fgtphi  (1,1,jb), fgqlam  (1,1,jb), &
            fgqphi  (1,1,jb), fgulam  (1,1,jb), fguphi  (1,1,jb), fgvlam  (1,1,jb), &
            fgvphi  (1,1,jb), fgtlamm (1,1,jb), fgtphim (1,1,jb), fgplamm (1  ,jb), &
            fgpphim (1  ,jb), fglnpm  (1  ,jb), fgdivm  (1,1,jb), fgzslam (1  ,jb), &
            fgzsphi (1  ,jb), fgyum   (1,1,jb), fgyvm   (1,1,jb), fgtdm   (1,1,jb), &
            fgqdm   (1,1,jb), fgvdlnpm(1  ,jb), colrad2D(1,jb)  , rcl     (1,jb)  , & 
            vmax(1,jb)      , ifday           , tod             ,                   & 
            ibMax           , kMax            , ibMaxPerJb(jb)  ,                   &
            slagr           , jb              , lonrad(1,jb)    , cos2d(1,jb)     , &
            intcosz         , cos2lat(1,jb)   , ercossin(1,jb)  , fcor(1,jb)      , &
            cosiv(1,jb)     , initial )
       IF(slagr.and.SL_twotime_scheme.and.fb1.eq.1.0_r8) fgwm(:,:,jb) = fgw(:,:,jb)
    END DO
    !$OMP BARRIER
    IF (cdhl(jdt)) THEN
       !$OMP SINGLE
       CALL Collect_Grid_Red(ustr, aux3)
       IF (myid.eq.0) THEN
          CALL WriteDiagHead(nfdhn,ifday,tod)
          CALL WriteField(nfdhn, aux3)
       ENDIF
       CALL Collect_Grid_Red(vstr, aux3)
       IF (myid.eq.0) THEN
          CALL WriteField(nfdhn, aux3)
       ENDIF
       !$OMP END SINGLE
    END IF
    !
    first = .FALSE.
    IF (slagr)  THEN 
       !
       !  Perform semi-Lagrangian computations and finish tendencies
       !  ----------------------------------------------------------
       !
       IF (SL_twotime_scheme) THEN
          CALL SemiLagr_2tl (2, dt, fa)
        ELSE
          CALL SemiLagr (2, dt)
       END IF
    ELSE
       !
       !  Finish tendencies
       !  -----------------
       ! 
       CALL AddTend  (dt, nlnminit, jbFirst, jbLast)
    END IF
    !
    !  Grid-point to spectral transforms
    !  ---------------------------------
    !
    !$OMP BARRIER

    CALL DirTrans(rcl, delta2, nlnminit, slagr, &
         jbFirst, jbLast, mnRIFirst, mnRILast, kFirstloc, kLastloc)

    !$OMP BARRIER
    !
    !  Return now if only computing tendencies for nlnmi
    !  -------------------------------------------------
    !
    IF (nlnminit) CALL TimeFilterStep1(fa, fb, jbFirst, jbLast)
    IF (.NOT. nlnminit) THEN
       !
       !  Semi-implicit computations (spectral integration)
       !  -------------------------------------------------
       !
       IF (ngroups_four.eq.1) THEN
          CALL SemiImpl(delta2, slagr, mnRIFirst, mnRILast)
        ELSE
          CALL SemiImpl_si(delta2, slagr, mnRIFirst, mnRILast, &
                           mnRIFirst_si, mnRILast_si)
       ENDIF
       !
       !  humidity and vorticity update
       !  -----------------------------
       !
       !
       IF (idiaten) THEN
          DO k = 1, kMaxloc
             DO mn = mnRIFirst, mnRILast
                qqp  (mn,k) = qqt(mn,k)
                qrotp(mn,k) = qrott(mn,k)
                qrott(mn,k) = qtmpp(mn,k)
             END DO
          END DO
       ELSE
          DO k = 1, kMaxloc
             DO mn = mnRIFirst, mnRILast
                qqp  (mn,k) = qqt(mn,k)
                qrotp(mn,k) = qrott(mn,k)
             END DO
          END DO
       END IF
       !$OMP BARRIER
       !
       !  Spectral to Grid-Point transforms for water
       !  ----------------------------------------------    

       CALL HumidBackTrans(jbFirst, jbLast)
       !
       !  Global mass-conservation
       !  ------------------------
    !$OMP BARRIER
       IF (MasCon_ps) THEN
          CALL GlobConservation(jFirst, jLast, jbFirst, jbLast, &
                                jFirst_d, jLast_d)
       ENDIF

       IF (do_globconserv)  THEN 

!         !--(DMK-CCATT-INI)----------------------------------------------------
!         ! Chemistry Emission Source Driver + plumerise driver
!         
!         !$OMP SINGLE
!         jbGlob = 0
!         !$OMP END SINGLE
!         DO
!            !$OMP CRITICAL(jbb3)
!            jbGlob = jbGlob + 1
!            jb = jbGlob
!            !$OMP END CRITICAL(jbb3)
!            IF (jb > jbMax) EXIT
!
!            CALL MCGASourcesDriver(ibMaxPerJB(jb), kMax,         ibMax,              jbMax,           &
!                                   si,             del,          sl,                 jb,              &
!                                   tod,            jdt,          nscalars,           pai,             &
!                                   gasr,           grav,         tov,                colrad2d(:,jb),  &
!                                   lonrad(:,jb),   cos2d(:,jb),  10.0_r8*fgps(:,jb), fgtmp(:,:,jb),  &
!                                   fgq(:,:,jb),   fgu(:,:,jb), fgv(:,:,jb),       fgzs(:,jb)/grav, &
!                                   xland(:,jb),    htdisp(:,jb), tsfc0(:,jb),        sigki)           
!                                   
!         END DO
!         !--(DMK-CCATT-FIM)----------------------------------------------------
          !
          ! Grid-point computations for scalars
          ! --------------------------------------
          !
          !$OMP BARRIER
          !$OMP SINGLE
          jbGlob = 0
          !$OMP END SINGLE
          DO
             !$OMP CRITICAL(jbb2)
             jbGlob = jbGlob + 1
             jb = jbGlob
             !$OMP END CRITICAL(jbb2)
             IF (jb > jbMax) EXIT
             CALL Scalardiffusion(ibMaxPerJb(jb), jb, delta2, &
                  PBL_CoefKh(1,1,jb), tov, fgtmp(1,1,jb), fgq(1,1,jb))
          END DO
   
          !$OMP BARRIER
          CALL UpdateConserv(jFirst, jLast, jbFirst, jbLast, &
                             jFirst_d, jLast_d)
       ENDIF

 
       !
       ! Grid-point computations for water
       ! ---------------------------------
       !
       !     
       !     perform moist ,large scale & dry convection
       !     
       IF(TRIM(isimp).ne.'YES') THEN
          !$OMP SINGLE
          jbGlob = 0
          !$OMP END SINGLE
          DO
             !$OMP CRITICAL(jbb1)
             jbGlob = jbGlob + 1
             jb = jbGlob
             !$OMP END CRITICAL(jbb1)
             IF (jb > jbMax) EXIT

           fgqmm(:,:,jb) = fgqm(:,:,jb)
           CALL HumidPhysics(jb, fgqmm(1,1,jb), fgtmpp(1,1,jb),&
                  fgqp(1,1,jb), fgpsp(1,  jb),fgu(1,1,jb)  , &
                  fgv(1,1,jb),omg(1,1,jb),fgtmpm(1,1,jb), &
                  fgtmp(1,1,jb), fgq(1,1,jb),fgps(1,jb),&
                  fgzs(1,jb),colrad2D(1,jb))
          END DO
       END IF
       !
       !  Begin filtering of previous time-step variables
       !  -----------------------------------------------
       !$OMP BARRIER
       CALL TimeFilterStep1(fa, fb, jbFirst, jbLast)
       !$OMP BARRIER
       !
       !  Grid-Point to Spectral transforms for water
       !  ----------------------------------------------    
       !
       CALL HumidDirTrans(jbFirst, jbLast)    
       !
       IF (idiaten) THEN
          DO k = 1, kMaxloc
             DO mn = mnRIFirst, mnRILast
                qrott  (mn,k) = qtmpp  (mn,k) - qrott(mn,k)
                qdiaten(mn,k) = qdiaten(mn,k) + qrott(mn,k)
             END DO
          END DO
       END IF
       !
       !  implicit diffusion
       !  ------------------
       !$OMP BARRIER
       !
       CALL ImplDifu(delta2, mnRIFirst, mnRILast)

       !
       !  enhanced diffusion
       !  ------------------
       !
       IF (enhdif) THEN
          DO jb = 1, jbMax
             DO k = kFirst, kLast
                vmaxVert(k) = MAX(vmaxVert(k), vmax(k,jb))
                vaux(k) = vmaxVert(k)
             END DO
          END DO
          !$OMP BARRIER
          !$OMP SINGLE
          IF (maxnodes.gt.1) THEN 
             CALL MPI_ALLREDUCE(vaux, vmaxVert, kmax, MPI_DOUBLE_PRECISION, MPI_MAX, &
                                MPI_COMM_WORLD,ierr)
          ENDIF
          !$OMP END SINGLE
          CALL FiltDiss(dt, vmaxVert(myfirstlev), kFirstloc, kLastloc, mnRIFirst, mnRILast)
       END IF
     ELSE
       !
       !  Begin filtering of previous time-step variables
       !  -----------------------------------------------
       !$OMP BARRIER
       CALL TimeFilterStep1(fa, fb, jbFirst, jbLast)
    END IF
  END SUBROUTINE TimeStep

  !InitBoundSimpPhys: adjust inicial condition  for SimpPhys

  SUBROUTINE InitBoundSimpPhys()
    USE Sizes, ONLY:         &
         ibMax              , &
         jbMax              , &
         ThreadDecomp,        &
         kMax
    USE FieldsDynamics, ONLY : &
         fgtmp              , & ! intent(inout)
         fgq                    ! intent(inout)


    IMPLICIT NONE
    INTEGER                :: ib
    INTEGER                :: jb
    INTEGER                :: k
    INTEGER                :: jbFirst
    INTEGER                :: jbLast

    CALL ThreadDecomp(1, jbMax, jbFirst, jbLast, "InitBoundSimpPhys")
    CALL SimpPhysBackTrans()    
    DO jb=jbFirst,jbLast
       DO k=1,kMax
          DO ib=1,ibMax
             fgtmp (ib,k,jb)=fgtmp(ib,k,jb)/ &
                 (1.0e0_r8+0.608e0_r8*MAX(1.0e-12_r8,fgq(ib,k,jb)))
          END DO
       END DO
       fgq(:,:,jb) = 0.0_r8
    END DO
    !$OMP BARRIER
    CALL SimpPhysDirTrans ()

  END SUBROUTINE InitBoundSimpPhys


  !sfcgeotrans: surface geopotential (and derivatives) transform


  SUBROUTINE SfcGeoTrans(slagr)

    USE Constants, ONLY: &
         tbar,           & ! intent(in)
         gasr,           & ! intent(in)
         grav,           & ! intent(in)
         ga2               ! intent(in)

    USE FieldsDynamics,    ONLY: &
         qlnpp,          & ! intent(in)
         qlnpl,          & ! intent(out)
         qgzs,           & ! intent(inout)
         qgzslap,        & ! intent(out)
         qgzsphi,        & ! intent(out)
         fgzs,           & ! intent(out)
         fgzslam,        & ! intent(out)
         fgzsphi           ! intent(out)

    USE Sizes,     ONLY: &
         mnMax,          & ! intent(in)
         mnExtMax,       & ! intent(in)
         mMax,           & ! intent(in)
         kMax,           & ! intent(in)
         mymnmax,        & ! intent(in)
         mymnextmax,     & ! intent(in)
         havesurf,       & ! intent(in)
         ThreadDecomp

    USE SpecDynamics, ONLY: &
         gozrim,         & ! intent(in)
         snnp1             ! intent(in)

    USE Transform, ONLY:                 &
         CreateSpecToGrid,               &
         DepositSpecToGrid,              &
         DepositSpecToGridAndDelLamGrid, &
         DoSpecToGrid,                   &
         DestroySpecToGrid

    IMPLICIT NONE
    LOGICAL, INTENT(IN) ::  slagr
    INTEGER :: mnRIFirst
    INTEGER :: mnRILast
    INTEGER :: mnRIExtFirst
    INTEGER :: mnRIExtLast
    INTEGER :: mn

    !$OMP PARALLEL PRIVATE(mnRIFirst, mnRILast, mnRIExtFirst, mnRIExtLast,mn)
    CALL ThreadDecomp(1, 2*mymnMax, mnRIFirst, mnRILast, "SfcGeoTrans")
    CALL ThreadDecomp(1, 2*mymnExtMax, mnRIExtFirst, mnRIExtLast, "SfcGeoTrans")
    IF (.not.slagr) THEN
       DO mn = mnRIFirst, mnRILast
          qgzslap(mn)=qgzs(mn)*snnp1(mn)*ga2
          qgzs(mn)=qgzs(mn)*grav
       END DO
    ELSE
       DO mn = mnRIFirst, mnRILast
          qgzs(mn)=qgzs(mn)*grav
          qlnpl(mn)=qlnpp(mn)+qgzs(mn)/(tbar*gasr)
       END DO
    ENDIF
    !$OMP BARRIER

    IF (havesurf) CALL gozrim(qgzs, qgzsphi, mnRIExtFirst, mnRIExtLast)
    !$OMP BARRIER

    !$OMP SINGLE
    CALL CreateSpecToGrid(0, 2, 0, 3)
    CALL DepositSpecToGridAndDelLamGrid(qgzs, fgzs, fgzslam)
    CALL DepositSpecToGrid(qgzsphi, fgzsphi)
    !$OMP END SINGLE
    CALL DoSpecToGrid()
    !$OMP BARRIER
    !$OMP SINGLE
    CALL DestroySpecToGrid()
    !$OMP END SINGLE
    !$OMP END PARALLEL
  END SUBROUTINE SfcGeoTrans


  !backtrans: spectral to grid transforms

  SUBROUTINE BackTrans(slagr)
    USE Constants, ONLY: &
         root2,          & ! intent(in)
         tov               ! intent(in)

    USE Sizes,  ONLY:    &
         ibMax,          &
         jbMax,          &
         ibMaxperjb,     &
         mymnextmax,     &
         havesurf,       &
         haveM1,         &
         kMax,           & ! intent(in)
         kMaxloc,        & ! intent(in)
         mMax,           & ! intent(in)
         mnExtMax,       & ! intent(in)
         ThreadDecomp

    USE FieldsDynamics, ONLY :   &
         qtmpp,          & ! intent(in) 
         qrotp,          & ! intent(in)
         qdivp,          & ! intent(in)
         qqp,            & ! intent(in)
         qup,            & ! intent(in)
         qvp,            & ! intent(in)
         qtphi,          & ! intent(in)
         qqphi,          & ! intent(in)
         qlnpp,          & ! intent(in)
         qpphi,          & ! intent(in)
         fgyu,       & ! intent(out) so zerado
         fgyv,       & ! intent(out) so zerado
         fgtd,       & ! intent(out) so zerado
         fgqd,       & ! intent(out) so zerado
         fgu,        & ! intent(out)
         fgv,        & ! intent(out)
         fgdiv,      & ! intent(out)
         fgrot,      & ! intent(out)
         fgq,        & ! intent(out)
         fgtmp,      & ! intent(out)
         fgtphi,     & ! intent(out)
         fgqphi,     & ! intent(out)
         fguphi,     & ! intent(out) so zerado
         fgvphi,     & ! intent(out) so zerado
         fgtlam,     & ! intent(out)
         fgqlam,     & ! intent(out)
         fgulam,     & ! intent(out)
         fgvlam,     & ! intent(out)
         fgvdlnp,    & ! intent(out) so zerado
         fglnps,     & ! intent(out)
         fgps,       & ! intent(out)
         fgpphi,     & ! intent(out)
         fgplam        ! intent(out)

    USE SpecDynamics, ONLY: &
         dztouv,            &
         gozrim

    USE Transform, ONLY:                 &
         CreateSpecToGrid,               &
         DepositSpecToGrid,              &
         DepositSpecToGridAndDelLamGrid, &
         DoSpecToGrid,                   &
         DestroySpecToGrid

    IMPLICIT NONE
    LOGICAL, INTENT(IN) ::  slagr
    INTEGER :: k, ib, jb
    INTEGER :: jbFirst
    INTEGER :: jbLast
    INTEGER :: ibFirst
    INTEGER :: ibLast
    INTEGER :: kFirst
    INTEGER :: kLast
    INTEGER :: mnRIExtFirst
    INTEGER :: mnRIExtLast

    CALL ThreadDecomp(1, jbMax, jbFirst, jbLast, "BackTrans")
    CALL ThreadDecomp(1, ibMax, ibFirst, ibLast, "BackTrans")
    CALL ThreadDecomp(1,  kMaxloc,  kFirst,  kLast, "BackTrans")
    CALL ThreadDecomp(1, 2*mymnExtMax, mnRIExtFirst, mnRIExtLast, "BackTrans")
    !

    IF (havesurf) CALL gozrim(qlnpp, qpphi, mnRIExtFirst, mnRIExtLast)
    CALL gozrim(qqp,   qqphi, mnRIExtFirst, mnRIExtLast)
    CALL gozrim(qtmpp, qtphi, mnRIExtFirst, mnRIExtLast)
    CALL dztouv(qdivp, qrotp, qup, qvp, mnRIExtFirst, mnRIExtLast)


    !$OMP BARRIER    ! due to qtmpp
    !     
    !     remove mean from temp.
    !
    IF (HaveM1) THEN
       DO k=kFirst, kLast
          qtmpp(1,k)=qtmpp(1,k)-tov(k)*root2
       END DO
    ENDIF
    !$OMP BARRIER    


    IF (slagr) THEN
       !$OMP SINGLE
       CALL CreateSpecToGrid(6, 2, 7, 3)
       CALL DepositSpecToGrid(qdivp, fgdiv)
       CALL DepositSpecToGrid(qpphi, fgpphi)
       CALL DepositSpecToGrid(qtphi, fgtphi)
       CALL DepositSpecToGridAndDelLamGrid(qtmpp, fgtmp,  fgtlam)
       CALL DepositSpecToGridAndDelLamGrid(qlnpp, fglnps, fgplam)
       CALL DepositSpecToGrid(qup,   fgu(:,:,1:jbmax))
       CALL DepositSpecToGrid(qvp,   fgv(:,:,1:jbmax))
       CALL DepositSpecToGrid(qqp,   fgq)
       !$OMP END SINGLE
       CALL DoSpecToGrid()
       !$OMP BARRIER
       !$OMP SINGLE
       CALL DestroySpecToGrid()
       !$OMP END SINGLE
    ELSE
       !$OMP SINGLE
       CALL CreateSpecToGrid(8, 2, 12, 3)
       CALL DepositSpecToGrid(qqphi,  fgqphi)
       CALL DepositSpecToGrid(qrotp,  fgrot)
       CALL DepositSpecToGrid(qdivp,  fgdiv)
       CALL DepositSpecToGrid(qpphi,  fgpphi)
       CALL DepositSpecToGrid(qtphi,  fgtphi)
       CALL DepositSpecToGridAndDelLamGrid(qtmpp, fgtmp,  fgtlam)
       CALL DepositSpecToGridAndDelLamGrid(qlnpp, fglnps, fgplam)
       CALL DepositSpecToGridAndDelLamGrid(qup,   fgu(:,:,1:jbmax),    fgulam)
       CALL DepositSpecToGridAndDelLamGrid(qvp,   fgv(:,:,1:jbmax),    fgvlam)
       CALL DepositSpecToGridAndDelLamGrid(qqp,   fgq,    fgqlam)
       !$OMP END SINGLE
       CALL DoSpecToGrid()
       !$OMP BARRIER
       !$OMP SINGLE
       CALL DestroySpecToGrid()
       !$OMP END SINGLE
    END IF

    DO jb = jbFirst, jbLast
       DO ib = 1, ibMaxperjb(jb)
          fgps(ib,jb) = EXP(fglnps(ib,jb))
       END DO
    END DO
  END SUBROUTINE BackTrans


  !dirtrans: grid to spectral transforms


  SUBROUTINE DirTrans(rcl, dt, nlnminit, slagr, &
       jbFirst, jbLast, mnRIFirst, mnRILast, kFirst, kLast)

    USE Constants, ONLY: &
         root2,          &
         tov

    USE Sizes, ONLY : &
         ibMaxperjb,  &
         ibMax,       &
         jbMax,       &
         mnMax,       &
         HaveM1,      &
         kMaxloc,     &
         kMax

    USE SpecDynamics, ONLY: &
         Uvtodz

    USE FieldsDynamics, ONLY : &
         qtmpp, &
         qtmpt, &
         qrott, &
         qdivt, &
         qqt, &
         qup, &
         qvp, &
         qlnpt, &
         qgzslap, &
         fgyu,     & ! intent(in)
         fgyv,     & ! intent(in)
         fgtd,     & ! intent(in)
         fgqd,     & ! intent(in)
         fgvdlnp     ! intent(in)

    USE Transform, ONLY:    &
         CreateGridToSpec,  &
         DepositGridToSpec, &
         DoGridToSpec,      &
         DestroyGridToSpec

    IMPLICIT NONE
    REAL(KIND=r8),    INTENT(IN) :: rcl(ibMax,jbMax)
    REAL(KIND=r8),    INTENT(IN) :: dt
    LOGICAL, INTENT(IN) :: nlnminit
    LOGICAL, INTENT(IN) :: slagr
    INTEGER, INTENT(IN) :: jbFirst
    INTEGER, INTENT(IN) :: jbLast
    INTEGER, INTENT(IN) :: mnRIFirst
    INTEGER, INTENT(IN) :: mnRILast
    INTEGER, INTENT(IN) :: kFirst
    INTEGER, INTENT(IN) :: kLast
    INTEGER :: ib, jb, mn, k
    !
    !
    DO jb = jbFirst, jbLast
       DO k = 1, kmax
          DO ib = 1, ibMaxperjb(jb)
             fgyu(ib,k,jb) = fgyu(ib,k,jb) * rcl(ib,jb)
             fgyv(ib,k,jb) = fgyv(ib,k,jb) * rcl(ib,jb)
          END DO
       END DO
    END DO
    !
    !
    !$OMP BARRIER
    !$OMP SINGLE
    CALL CreateGridToSpec(4, 1)
    CALL DepositGridToSpec(qup,   fgyu)
    CALL DepositGridToSpec(qvp,   fgyv)
    CALL DepositGridToSpec(qqt,   fgqd)
    CALL DepositGridToSpec(qtmpt, fgtd)
    CALL DepositGridToSpec(qlnpt, fgvdlnp)
    !$OMP END SINGLE
    CALL DoGridToSpec()
    !$OMP BARRIER
    !$OMP SINGLE
    CALL DestroyGridToSpec()
    !$OMP END SINGLE
    !
    !   obtain div and vort tendencies
    !
    CALL Uvtodz(qup, qvp, qdivt, qrott, mnRIFirst, mnRILast)
    !
    !     add contribution from topography to divergence tendency
    !
    IF (.NOT.nlnminit.AND..NOT.slagr) THEN
       DO k=1,kmaxloc
          DO mn = mnRIFirst, mnRILast
             qdivt(mn,k)=qdivt(mn,k)+dt*qgzslap(mn)
          END DO
       END DO
    ENDIF
    !
    !   restore  temperature and add mean also to temperature tendency
    !   
    IF (HaveM1) THEN
       IF (nlnminit) THEN
          DO k = kFirst, kLast
             qtmpp(1,k)=qtmpp(1,k)+tov(k)*root2
          END DO
       ELSE
          DO k = kFirst, kLast
             qtmpp(1,k)=qtmpp(1,k)+tov(k)*root2
             qtmpt(1,k)=qtmpt(1,k)+tov(k)*root2
          END DO
       END IF
    END IF
  END SUBROUTINE DirTrans


  ! Humid Back Trans


  SUBROUTINE HumidBackTrans(jbFirst, jbLast)

    USE Sizes, ONLY:     &
         ibMaxperjb,     &
         mnmax,     &
         kmax,     &
         jbMax

    USE FieldsDynamics, ONLY :   &
         qqp,            & ! intent(in)
         qtmpp,          & ! intent(in) 
         qlnpp,          & ! intent(in)
         fgtmpp,     & ! intent(out)
         fgqp,       & ! intent(out)
         fgpsp         ! intent(out)

    USE Transform, ONLY:    &
         CreateSpecToGrid,  &
         DepositSpecToGrid, &
         DoSpecToGrid,      &
         DestroySpecToGrid

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: jbFirst
    INTEGER, INTENT(IN) :: jbLast
    INTEGER :: ib, jb

    
    !$OMP SINGLE
    CALL CreateSpecToGrid(2, 1, 2, 1)
    CALL DepositSpecToGrid(qtmpp, fgtmpp(:,:,1:jbmax))
    CALL DepositSpecToGrid(qqp,   fgqp(:,:,1:jbmax))
    CALL DepositSpecToGrid(qlnpp, fgpsp(:,1:jbmax))
    !$OMP END SINGLE
    CALL DoSpecToGrid()
    !$OMP BARRIER
    !$OMP SINGLE
    CALL DestroySpecToGrid()
    !$OMP END SINGLE

    DO jb = jbFirst, jbLast
       DO ib = 1, ibMaxperjb(jb)
          fgpsp(ib,jb) = EXP(fgpsp(ib,jb))
       END DO
    END DO
  END SUBROUTINE HumidBackTrans


  ! Humid Dir Trans


  SUBROUTINE HumidDirTrans(jbFirst, jbLast)

    USE FieldsDynamics, ONLY : &
         qqp,        & ! intent(out)
         qtmpp,      & ! intent(out)
         qlnpp,      & ! intent(out)
         fgtmpp,     & ! intent(in)
         fgpsp,      & ! intent(in)
         fgqp          ! intent(in)

    USE Transform, ONLY:    &
         CreateGridToSpec,  &
         DepositGridToSpec, &
         DoGridToSpec,      &
         DestroyGridToSpec

    USE Sizes, ONLY:    &
         jbMaX,         &
         ibMaxperjb

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: jbFirst
    INTEGER, INTENT(IN) :: jbLast
    INTEGER :: ib, jb
    CHARACTER(LEN=*), PARAMETER :: h="**(HumidDirTrans)**"

    IF (MasCon_ps) THEN
       DO jb = jbFirst, jbLast
          DO ib = 1, ibMaxperjb(jb)
             fgpsp(ib,jb) = LOG(fgpsp(ib,jb))
          END DO
       END DO
    ENDIF

    !$OMP BARRIER
    !$OMP SINGLE
    IF (MasCon_ps) THEN
       CALL CreateGridToSpec(2, 1)
       CALL DepositGridToSpec(qlnpp, fgpsp(:,1:jbmax))
     ELSE
       CALL CreateGridToSpec(2, 0)
    ENDIF
    CALL DepositGridToSpec(qqp,   fgqp(:,:,1:jbmax))
    CALL DepositGridToSpec(qtmpp, fgtmpp(:,:,1:jbmax))
    !$OMP END SINGLE
    CALL DoGridToSpec()
    !$OMP BARRIER
    !$OMP SINGLE
    CALL DestroyGridToSpec()
    IF (MasCon_ps) THEN
       CALL Spread_surf_Spec(qlnpp)
    ENDIF
    !$OMP END SINGLE
  END SUBROUTINE HumidDirTrans


  ! SimpPhys Back Trans


  SUBROUTINE SimpPhysBackTrans()
    USE FieldsDynamics, ONLY :   &
         qqp,            & ! intent(in)
         qtmpp,          & ! intent(in) 
         fgtmp,          & ! intent(out)
         fgq               ! intent(out)

    USE Transform, ONLY:    &
         CreateSpecToGrid,  &
         DepositSpecToGrid, &
         DoSpecToGrid,      &
         DestroySpecToGrid

    IMPLICIT NONE
    !$OMP SINGLE
    CALL CreateSpecToGrid(2, 0, 2, 0)
    CALL DepositSpecToGrid(qqp  ,   fgq)
    CALL DepositSpecToGrid(qtmpp, fgtmp)
    !$OMP END SINGLE
    CALL DoSpecToGrid()
    !$OMP BARRIER
    !$OMP SINGLE
    CALL DestroySpecToGrid()
    !$OMP END SINGLE
  END SUBROUTINE SimpPhysBackTrans


  ! SimpPhy Dir Trans


  SUBROUTINE SimpPhysDirTrans()

    USE FieldsDynamics, ONLY : &
         qqp,        & ! intent(out)
         qqt,        & ! intent(out)
         qtmpp,      & ! intent(out)
         fgtmp,      & ! intent(in)
         fgqm,       & ! intent(out)
         fgq           ! intent(in)

    USE Transform, ONLY:    &
         createGridToSpec,  &
         DepositGridToSpec, &
         DoGridToSpec,      &
         DestroyGridToSpec

    USE Options, ONLY :       &
         isimp              

    IMPLICIT NONE
    CHARACTER(LEN=*), PARAMETER :: h="**(SimpPhyDirTrans)**"

    !$OMP SINGLE
    CALL CreateGridToSpec(1, 0)
    qqp  = 0.0_r8 
    CALL DepositGridToSpec(qtmpp, fgtmp)
    !$OMP END SINGLE
    CALL DoGridToSpec()
    !$OMP BARRIER
    !$OMP SINGLE
    CALL DestroyGridToSpec()
    !$OMP END SINGLE
  END SUBROUTINE SimpPhysDirTrans
END MODULE ModTimeStep
