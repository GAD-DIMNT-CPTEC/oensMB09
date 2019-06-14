!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:37 $
!  $Revision: 1.1.1.1 $
!
MODULE ModTimeStep

  USE Constants, ONLY:     &
       coriol             , &
       root2              

  USE Sizes, ONLY:         &
       ibMax              , &
       jbMax              , &
       ibMaxPerJB         , &
       jPerIJB            , &
       iPerIJB            , & 
       mnMax              , &
       mnExtMax           , &
       mMax               , &
       jMax               , &
       kMax               , &
       iMax               , &
       ThreadDecomp,        &
       sl

  USE Utils, ONLY :    &
       colrad             , &
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
       vmaxVert           , &
       rcs2

  USE FieldsDynamics, ONLY: &
       fgyu, fgyv, fgtd, fgqd, fgvdlnp, &
       fgu, fgv, fgw, fgum, fgvm, fgtmpm, &
       fgqm, fglnpm, fgyum, fgyvm, fgtdm, fgvdlnpm, &
       fgdiv, fgtmp, fgrot, fgq, fgplam, fgpphi, fglnps,  &
       omg, fgps, fgtlam, fgtphi, fgqlam, fgqphi, fgulam,  &
       fguphi, fgvlam, fgvphi, fgtlamm, fgtphim, fgplamm, &
       fgpphim, fgdivm, fgzslam, fgzsphi, fgqmm, fgumm, fgvmm,   &
       qqp, qqt, qrotp, qrott, qtmpp, qdiaten

  USE FieldsPhysics, ONLY:  &
       ustr               , &
       vstr               , &
       imask              , &
       gtsea


  USE GridDynamics, ONLY:    &
       AddTend,              &
       GrpComp,              &
       TimeFilterStep1    , &
       TimeFilterStep2

  USE SpecDynamics, ONLY:   &
       FiltDiss,            &
       ImplDifu,            &
       SemiImpl

  USE SemiLagrangian, ONLY: &
       SemiLagr

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
       initlz

  USE IOLowLevel, ONLY: &
       WriteField         , &
       WriteDiagHead

  USE Radiation,  ONLY:    & 
       coszmed

  PRIVATE
  PUBLIC :: TimeStep
  PUBLIC :: SfcGeoTrans

  INTEGER :: jbGlob  ! loop index, global among threads
CONTAINS


  ! timestep: performs a time-step of the model. Through the values
  !           of fb1, fa and fb we control if this is a initial
  !           time step,  part of a cold start or if it is a normal
  !           time step. 


  SUBROUTINE TimeStep (fb1, fa, fb, &
       slagr, nlnminit, inirestart, &
       idiaten, enhdif, dt,  &
       jdt, ifday, tod, idatec)
    IMPLICIT NONE
    REAL,    INTENT(IN) :: fb1
    REAL,    INTENT(IN) :: fa
    REAL,    INTENT(IN) :: fb
    LOGICAL, INTENT(IN) :: slagr
    LOGICAL, INTENT(IN) :: nlnminit    
    LOGICAL, INTENT(IN) :: inirestart
    LOGICAL, INTENT(IN) :: idiaten   
    LOGICAL, INTENT(IN) :: enhdif
    REAL,    INTENT(IN) :: dt
    INTEGER, INTENT(IN) :: jdt
    INTEGER, INTENT(IN) :: ifday
    REAL,    INTENT(IN) :: tod
    INTEGER, INTENT(IN) :: idatec(4)
    INTEGER  :: jb 
    INTEGER  :: ib 
    INTEGER  :: k
    INTEGER  :: mn
    INTEGER  :: j 
    INTEGER  :: i
    INTEGER  :: ij
    REAL     :: aux3(ibMax*jbMax)
    REAL     :: fgyu1(ibMax,kMax,jbMax)
    REAL     :: fgyv1(ibMax,kMax,jbMax)
    REAL     :: fgtd1(ibMax,kMax,jbMax)
    REAL     :: fgqd1(ibMax,kMax,jbMax)  
    INTEGER  :: mnFirst
    INTEGER  :: mnLast
    INTEGER  :: mnRIFirst
    INTEGER  :: mnRILast
    INTEGER  :: mnExtFirst
    INTEGER  :: mnExtLast
    INTEGER  :: mFirst
    INTEGER  :: mLast
    INTEGER  :: jFirst
    INTEGER  :: jLast
    INTEGER  :: jbFirst
    INTEGER  :: jbLast
    INTEGER  :: kFirst
    INTEGER  :: kLast
    !$ INTEGER, EXTERNAL :: OMP_GET_THREAD_NUM
    !$ INTEGER, EXTERNAL :: OMP_GET_NUM_THREADS
    !$ INTEGER, EXTERNAL :: OMP_GET_MAX_THREADS
    CHARACTER(LEN=*), PARAMETER :: h="**(TimeStep)**"
    CHARACTER(LEN=3) :: c0
    WRITE(c0,"(i3.3)") jdt
    CALL ThreadDecomp(1, mnMax, mnFirst, mnLast, "TimeStep"//c0)
    CALL ThreadDecomp(1, mnExtMax, mnExtFirst, mnExtLast, "TimeStep"//c0)
    CALL ThreadDecomp(1, 2*mnMax, mnRIFirst, mnRILast, "TimeStep"//c0)
    CALL ThreadDecomp(1, mMax, mFirst, mLast, "TimeStep"//c0)
    CALL ThreadDecomp(1, jMax, jFirst, jLast, "TimeStep"//c0)
    CALL ThreadDecomp(1, jbMax, jbFirst, jbLast, "TimeStep"//c0)
    CALL ThreadDecomp(1, kMax, kFirst, kLast, "TimeStep"//c0)

    DO j = jFirst, jLast
       CALL coszmed(idatec,tod,yrl,lati(:,j),long(:,j),cosz(:,j),iMax)
    END DO
    !$OMP BARRIER

    ! This j loop produces cosz, used by next jb loop
    DO jb = jbFirst, jbLast
       DO ib = 1, ibMaxPerJB(jb)
          j = jPerIJB(ib,jb)
          i = iPerIJB(ib,jb)
          cos2d(ib,jb)=cosz(i,j)
       END DO
       IF (cdhl(jdt)) THEN
          DO ib = 1, ibMax
             ustr(ib,jb) = 0.0
             vstr(ib,jb) = 0.0
          END DO
       END IF
       DO k = 1, kMax
          vmax(k,jb) = 0.0
       END DO
    END DO

    DO k = kFirst, kLast
       vmaxVert(k) = 0.0
    END DO

    !
    !  Spectral to Grid-Point transforms
    !  ---------------------------------    
    !
    CALL BackTrans(slagr)
    !
    !  Complete filtering of previous time-step variables
    !  --------------------------------------------------
    !
    CALL TimeFilterStep2(fb1, jbFirst, jbLast)
    IF (.NOT. inirestart) THEN
       !
       ! Grid-point computations over latitudes
       ! --------------------------------------
       !
       !$OMP SINGLE
       jbGlob = 0
       !$OMP END SINGLE
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
               fgplam  (1  ,jb), fgpphi  (1  ,jb), fglnps  (1  ,jb), fgum    (1,1,jb), &
               fgvm    (1,1,jb), fgtmpm  (1,1,jb), fgqm    (1,1,jb), omg     (1,1,jb), &
               fgps    (1  ,jb), fgtlam  (1,1,jb), fgtphi  (1,1,jb), fgqlam  (1,1,jb), &
               fgqphi  (1,1,jb), fgulam  (1,1,jb), fguphi  (1,1,jb), fgvlam  (1,1,jb), &
               fgvphi  (1,1,jb), fgtlamm (1,1,jb), fgtphim (1,1,jb), fgplamm (1  ,jb), &
               fgpphim (1  ,jb), fglnpm  (1  ,jb), fgdivm  (1,1,jb), fgzslam (1  ,jb), &
               fgzsphi (1  ,jb), fgyum   (1,1,jb), fgyvm   (1,1,jb), fgtdm   (1,1,jb), &
               fgvdlnpm(1  ,jb), colrad2D(1,jb)  , rcl     (1,jb)  ,                   &      
               vmax(1,jb)      , ifday           , tod             ,                   &    
               ibMax           , kMax            , ibMaxPerJb(jb)  ,                   &
               slagr           , jb              , lonrad(1,jb)    , cos2d(1,jb)     , &
               intcosz         , cos2lat(1,jb)   , ercossin(1,jb)  , fcor(1,jb)      , &
               cosiv(1,jb)     , fgyu1(1,1,jb)   , fgyv1(1,1,jb)   , fgtd1(1,1,jb)   , &
               fgqd1(1,1,jb))
       END DO
       !$OMP BARRIER
       IF (cdhl(jdt)) THEN
          !$OMP SINGLE
          CALL WriteDiagHead(nfdhn,ifday,tod)
          ij=1
          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)
                aux3(ij)=ustr(i,j) 
                ij=ij+1
             END DO
          END DO
          CALL WriteField(nfdhn, aux3)
          ij=1
          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)
                aux3(ij)=vstr(i,j) 
                ij=ij+1
             END DO
          END DO
          CALL WriteField(nfdhn, aux3)      
          !$OMP END SINGLE
          !$OMP BARRIER
       END IF
       !
       first = .FALSE.
       IF (slagr)  THEN 
          !
          !  Perform semi-Lagrangian computations and finish tendencies
          !  ----------------------------------------------------------
          !
          !$OMP SINGLE
          CALL SemiLagr (2, dt, nlnminit)
          !$OMP END SINGLE
          !$OMP BARRIER
       ELSE
          !
          !  Finish tendencies
          !  -----------------
          ! 
          CALL AddTend  (dt, nlnminit, jbFirst, jbLast)
       END IF
       !
       !  Begin filtering of previous time-step variables
       !  -----------------------------------------------
       !
       CALL TimeFilterStep1(fa, fb, jbFirst, jbLast)
       !
       !  Grid-point to spectral transforms
       !  ---------------------------------
       !
       CALL DirTrans(rcl, dt, nlnminit, &
            jbFirst, jbLast, mnRIFirst, mnRILast, kFirst, kLast)
       !$OMP BARRIER
       !
       !  Return now if only computing tendencies for nlnmi
       !  -------------------------------------------------
       !
       IF (.NOT. nlnminit) THEN
          !
          !  Semi-implicit computations (spectral integration)
          !  -------------------------------------------------
          !
          CALL SemiImpl(dt, mnRIFirst, mnRILast)
          !$OMP BARRIER
          !
          !  humidity and vorticity update
          !  -----------------------------
          !
          !
          IF (idiaten) THEN
             DO k = 1, kMax
                DO mn = mnRIFirst, mnRILast
                   qqp  (mn,k) = qqt(mn,k)
                   qrotp(mn,k) = qrott(mn,k)
                   qrott(mn,k) = qtmpp(mn,k)
                END DO
             END DO
          ELSE
             DO k = 1, kMax
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
          !
          CALL HumidBackTrans(jbFirst, jbLast)
          !$OMP BARRIER
          !
          ! Grid-point computations for water
          ! ---------------------------------
          !
          !     
          !     perform moist ,large scale & dry convection
          !     
          IF(isimp.ne.'YES ') THEN
             !$OMP SINGLE
             jbGlob = 0
             !$OMP END SINGLE
             DO
                !$OMP CRITICAL(jbb)
                jbGlob = jbGlob + 1
                jb = jbGlob
                !$OMP END CRITICAL(jbb)
                IF (jb > jbMax) EXIT
                CALL HumidPhysics(jb, fgqmm(1,1,jb), fgtmp(1,1,jb),&
                     fgq(1,1,jb), fgps(1,  jb),fgumm(1,1,jb)  , &
                     fgvmm(1,1,jb),omg(1,1,jb))
             END DO
             !$OMP BARRIER
          END IF
          !
          !  Grid-Point to Spectral transforms for water
          !  ----------------------------------------------    
          !
          CALL HumidDirTrans()    
          !
          IF (idiaten) THEN
             DO k = 1, kMax
                DO mn = mnRIFirst, mnRILast
                   qrott  (mn,k) = qtmpp  (mn,k) - qrott(mn,k)
                   qdiaten(mn,k) = qdiaten(mn,k) + qrott(mn,k)
                END DO
             END DO
             !$OMP BARRIER
          END IF
          !
          !  implicit diffusion
          !  ------------------
          !
          CALL ImplDifu(dt, mnRIFirst, mnRILast)
          !
          !  enhanced diffusion
          !  ------------------
          !
          IF (enhdif) THEN
             DO jb = 1, jbMax
                DO k = kFirst, kLast
                   vmaxVert(k) = MAX(vmaxVert(k), vmax(k,jb))
                END DO
             END DO
             !$OMP BARRIER
             CALL FiltDiss(dt, vmaxVert, kFirst, kLast, mnRIFirst, mnRILast)
          END IF
       END IF
    END IF
  END SUBROUTINE TimeStep

  !sfcgeotrans: surface geopotential (and derivatives) transform


  SUBROUTINE SfcGeoTrans()

    USE Constants, ONLY: &
         grav,           & ! intent(in)
         ga2               ! intent(in)

    USE FieldsDynamics,    ONLY: &
         qgzs,           & ! intent(inout)
         qgzslap,        & ! intent(out)
         qgzsphi,        & ! intent(out)
         fgzslam,    & ! intent(out)
         fgzsphi       ! intent(out)

    USE Sizes,     ONLY: &
         mnMax,          & ! intent(in)
         mnExtMax,       & ! intent(in)
         mMax,           & ! intent(in)
         ThreadDecomp,   &
         snnp1             ! intent(in)

    USE SpecDynamics, ONLY: gozrim

    USE Transform, ONLY:          &
         CreateSpecToGrid,        &
         DepositSpecToGrid,       &
         DepositSpecToDelLamGrid, &
         DoSpecToGrid,            &
         DestroySpecToGrid

    IMPLICIT NONE
    INTEGER :: mnRIFirst
    INTEGER :: mnRILast
    INTEGER :: mnRIExtFirst
    INTEGER :: mnRIExtLast
    INTEGER :: mn

    !$OMP PARALLEL PRIVATE(mnRIFirst, mnRILast, mnRIExtFirst, mnRIExtLast)
    CALL ThreadDecomp(1, 2*mnMax, mnRIFirst, mnRILast, "SfcGeoTrans")
    CALL ThreadDecomp(1, 2*mnExtMax, mnRIExtFirst, mnRIExtLast, "SfcGeoTrans")
    DO mn = mnRIFirst, mnRILast
       qgzslap(mn)=qgzs(mn)*snnp1(mn)*ga2
       qgzs(mn)=qgzs(mn)*grav
    END DO
    !$OMP BARRIER

    CALL gozrim(qgzs, qgzsphi, mnRIExtFirst, mnRIExtLast)
    !$OMP BARRIER

    !$OMP SINGLE
    CALL CreateSpecToGrid(0, 2, 0, 2)
    !$OMP END SINGLE
    !$OMP SECTIONS
    CALL DepositSpecToDelLamGrid(qgzs, fgzslam)
    !$OMP SECTION
    CALL DepositSpecToGrid(qgzsphi, fgzsphi)
    !$OMP END SECTIONS
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
         kMax,           & ! intent(in)
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
    CALL ThreadDecomp(1,  kMax,  kFirst,  kLast, "BackTrans")
    CALL ThreadDecomp(1, 2*mnExtMax, mnRIExtFirst, mnRIExtLast, "BackTrans")
    !
    ! Is this realy necessary?
    !
    IF (slagr) THEN
       DO jb = jbFirst, jbLast
          fgrot(:,:,jb)  =0.0
          fgqphi(:,:,jb) =0.0
          fgqlam(:,:,jb) =0.0
          fgulam(:,:,jb) =0.0
          fgvlam(:,:,jb) =0.0
       END DO
    END IF

    DO jb = jbFirst, jbLast
       fgyu(:,:,jb)   =0.0
       fgyv(:,:,jb)   =0.0
       fgtd(:,:,jb)   =0.0
       fgqd(:,:,jb)   =0.0
       fguphi(:,:,jb) =0.0
       fgvphi(:,:,jb) =0.0
    END DO
    DO jb = jbFirst, jbLast
       DO ib = 1, ibMax
          fgvdlnp(ib, jb) =0.0
       END DO
    END DO

    CALL gozrim(qlnpp, qpphi, mnRIExtFirst, mnRIExtLast)
    CALL gozrim(qqp,   qqphi, mnRIExtFirst, mnRIExtLast)
    CALL gozrim(qtmpp, qtphi, mnRIExtFirst, mnRIExtLast)
    CALL dztouv(qdivp, qrotp, qup, qvp, mnRIExtFirst, mnRIExtLast)
    !$OMP BARRIER    ! due to qtmpp 
    !     
    !     remove mean from temp.
    !
    DO k=kFirst, kLast
       qtmpp(1,k)=qtmpp(1,k)-tov(k)*root2
    END DO
    !$OMP BARRIER    ! due to qtmpp 


    IF (slagr) THEN
       !$OMP SINGLE
       CALL CreateSpecToGrid(6, 2, 7, 3)
       !$OMP END SINGLE
       !$OMP SECTIONS
       CALL DepositSpecToGrid(qdivp, fgdiv)
       !$OMP SECTION
       CALL DepositSpecToGrid(qpphi, fgpphi)
       !$OMP SECTION
       CALL DepositSpecToGrid(qtphi, fgtphi)
       !$OMP SECTION
       CALL DepositSpecToGridAndDelLamGrid(qtmpp, fgtmp,  fgtlam)
       !$OMP SECTION
       CALL DepositSpecToGridAndDelLamGrid(qlnpp, fglnps, fgplam)
       !$OMP SECTION
       CALL DepositSpecToGrid(qup,   fgu)
       !$OMP SECTION
       CALL DepositSpecToGrid(qvp,   fgv)
       !$OMP SECTION
       CALL DepositSpecToGrid(qqp,   fgq)
       !$OMP END SECTIONS
       CALL DoSpecToGrid()
       !$OMP BARRIER
       !$OMP SINGLE
       CALL DestroySpecToGrid()
       !$OMP END SINGLE
    ELSE
       !$OMP SINGLE
       CALL CreateSpecToGrid(8, 2, 12, 3)
       !$OMP END SINGLE
       !$OMP SECTIONS
       CALL DepositSpecToGrid(qqphi,  fgqphi)
       !$OMP SECTION
       CALL DepositSpecToGrid(qrotp,  fgrot)
       !$OMP SECTION
       CALL DepositSpecToGrid(qdivp,  fgdiv)
       !$OMP SECTION
       CALL DepositSpecToGrid(qpphi,  fgpphi)
       !$OMP SECTION
       CALL DepositSpecToGrid(qtphi,  fgtphi)
       !$OMP SECTION
       CALL DepositSpecToGridAndDelLamGrid(qtmpp, fgtmp,  fgtlam)
       !$OMP SECTION
       CALL DepositSpecToGridAndDelLamGrid(qlnpp, fglnps, fgplam)
       !$OMP SECTION
       CALL DepositSpecToGridAndDelLamGrid(qup,   fgu,    fgulam)
       !$OMP SECTION
       CALL DepositSpecToGridAndDelLamGrid(qvp,   fgv,    fgvlam)
       !$OMP SECTION
       CALL DepositSpecToGridAndDelLamGrid(qqp,   fgq,    fgqlam)
       !$OMP END SECTIONS
       CALL DoSpecToGrid()
       !$OMP BARRIER
       !$OMP SINGLE
       CALL DestroySpecToGrid()
       !$OMP END SINGLE
    END IF
    !$OMP BARRIER

    DO jb = jbFirst, jbLast
       DO ib = 1, ibMax
          fgps(ib,jb) = EXP(fglnps(ib,jb))
       END DO
    END DO
  END SUBROUTINE BackTrans


  !dirtrans: grid to spectral transforms


  SUBROUTINE DirTrans(rcl, dt, nlnminit, &
       jbFirst, jbLast, mnRIFirst, mnRILast, kFirst, kLast)

    USE Constants, ONLY: &
         root2,          &
         tov

    USE Sizes, ONLY : &
         ibMax,       &
         jbMax,       &
         mnMax,       &
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
    REAL,    INTENT(IN) :: rcl(ibMax,jbMax)
    REAL,    INTENT(IN) :: dt
    LOGICAL, INTENT(IN) :: nlnminit
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
          DO ib = 1, ibMax
             fgyu(ib,k,jb) = fgyu(ib,k,jb) * rcl(ib,jb)
             fgyv(ib,k,jb) = fgyv(ib,k,jb) * rcl(ib,jb)
          END DO
       END DO
    END DO
    !
    !
    !$OMP SINGLE
    CALL CreateGridToSpec(4, 1)
    !$OMP END SINGLE
    !$OMP SECTIONS
    CALL DepositGridToSpec(qup,   fgyu)
    !$OMP SECTION
    CALL DepositGridToSpec(qvp,   fgyv)
    !$OMP SECTION
    CALL DepositGridToSpec(qqt,   fgqd)
    !$OMP SECTION
    CALL DepositGridToSpec(qtmpt, fgtd)
    !$OMP SECTION
    CALL DepositGridToSpec(qlnpt, fgvdlnp)
    !$OMP END SECTIONS
    CALL DoGridToSpec()
    !$OMP BARRIER
    !$OMP SINGLE
    CALL DestroyGridToSpec()
    !$OMP END SINGLE
    !$OMP BARRIER
    !
    !   obtain div and vort tendencies
    !
    CALL Uvtodz(qup, qvp, qdivt, qrott, mnRIFirst, mnRILast)
    !
    !     add contribution from topography to divergence tendency
    !
    IF (.NOT.nlnminit) THEN
       DO k=1,kmax
          DO mn = mnRIFirst, mnRILast
             qdivt(mn,k)=qdivt(mn,k)+dt*qgzslap(mn)
          END DO
       END DO
    ENDIF
    !
    !   restore  temperature and add mean also to temperature tendency
    !   
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
  END SUBROUTINE DirTrans


  ! Humid Back Trans


  SUBROUTINE HumidBackTrans(jbFirst, jbLast)

    USE Sizes, ONLY:     &
         ibMax,          &
         jbMax

    USE FieldsDynamics, ONLY :   &
         qqp,            & ! intent(in)
         qtmpp,          & ! intent(in) 
         qlnpp,          & ! intent(in)
         fgtmp,      & ! intent(out)
         fgq,        & ! intent(out)
         fglnps,     & ! intent(out)
         fgps          ! intent(out)

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
    !$OMP END SINGLE
    !$OMP SECTIONS
    CALL DepositSpecToGrid(qqp,   fgq)
    !$OMP SECTION
    CALL DepositSpecToGrid(qtmpp, fgtmp)
    !$OMP SECTION
    CALL DepositSpecToGrid(qlnpp, fglnps)
    !$OMP END SECTIONS
    CALL DoSpecToGrid()
    !$OMP BARRIER
    !$OMP SINGLE
    CALL DestroySpecToGrid()
    !$OMP END SINGLE
    !$OMP BARRIER

    DO jb = jbFirst, jbLast
       DO ib = 1, ibMax
          fgps(ib,jb) = EXP(fglnps(ib,jb))
       END DO
    END DO
  END SUBROUTINE HumidBackTrans


  ! Humid Dir Trans


  SUBROUTINE HumidDirTrans()

    USE FieldsDynamics, ONLY : &
         qqp,        & ! intent(out)
         qtmpp,      & ! intent(out)
         fgtmp,      & ! intent(in)
         fgq           ! intent(in)

    USE Transform, ONLY:    &
         CreateGridToSpec,  &
         DepositGridToSpec, &
         DoGridToSpec,      &
         DestroyGridToSpec

    IMPLICIT NONE
    CHARACTER(LEN=*), PARAMETER :: h="**(HumidDirTrans)**"

    !$OMP SINGLE
    CALL CreateGridToSpec(2, 0)
    !$OMP END SINGLE
    !$OMP SECTIONS
    CALL DepositGridToSpec(qqp,   fgq)
    !$OMP SECTION
    CALL DepositGridToSpec(qtmpp, fgtmp)
    !$OMP END SECTIONS
    CALL DoGridToSpec()
    !$OMP BARRIER
    !$OMP SINGLE
    CALL DestroyGridToSpec()
    !$OMP END SINGLE
  END SUBROUTINE HumidDirTrans
END MODULE ModTimeStep
