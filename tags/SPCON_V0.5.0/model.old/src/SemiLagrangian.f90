MODULE SemiLagrangian

!
!    In this module we compute the lagrangian trajectories and
!    perform the interpolations. The interpolation routines are
!    written for the block data structure, but assume that a 
!    full latitude is stored in the same block. If we shall allow
!    a latitude to be brocken in more blocks, the routines will need revision
!    -----------------------------------------------------------------------
!
!
  USE Constants,    ONLY: &
       er               , &
       pihalf           , &
       pai

  USE Options,      ONLY: &
       reducedGrid

  USE Sizes,        ONLY: &
       iPerIJB          , &
       jPerIJB          , &
       ibPerIJ          , &
       jbPerIJ          , &
       ibmaxperJB       , &
       imaxperJ         , &
       ibmax            , &
       jbmax            , &
       kmax             , &
       imax             , &
       jmax             , &
       del              , &
       delcl            , & 
       ci               , &
       cl                 
       
  USE FieldsDynamics,ONLY: &
       fgyu             , &
       fgyv             , &
       fgqd             , &
       fgtd             , &
       fgvdlnp          , &
       fgyum            , &
       fgyvm            , &
       fgqdm            , &
       fgtdm            , &
       fgvdlnpm         , &
       fgum             , &
       fgvm             , &
       fgumm            , &
       fgvmm            , &
       fgqm             , &
       fgtmpm           , &
       fglnpm           , &
       fgu              , &
       fgv              , &
       fgw
       
  USE Utils,       ONLY : &
       coslat           , &
       coslon           , &
       sinlat           , &
       sinlon           , &
       longit           , &
       colrad

  IMPLICIT NONE
  ! Use to store location of the 3d dep

  REAL, ALLOCATABLE :: ulonm  (:,:)
  REAL, ALLOCATABLE :: ulatm  (:,:)
  REAL, ALLOCATABLE :: usigm  (:,:)

  ! Use to store location of the 2d dep

  REAL, ALLOCATABLE :: ulonm2D  (:,:)
  REAL, ALLOCATABLE :: ulatm2D  (:,:)

  REAL, ALLOCATABLE :: phi(:)
  REAL, ALLOCATABLE :: delphi(:)
  REAL, ALLOCATABLE :: dellon(:)
  REAL, ALLOCATABLE :: cubcoef(:,:)
  REAL, ALLOCATABLE :: cubcoefv(:,:)

  INTEGER, ALLOCATABLE :: jphiref(:)
  INTEGER, ALLOCATABLE :: ksigref(:)
  
  INTEGER :: kmaxref
  INTEGER :: jmaxref
  REAL    :: delsigref
  REAL    :: delphiref
  PRIVATE
  PUBLIC :: SemiLagr, InitSL
CONTAINS
  SUBROUTINE SemiLagr(nit , delt, nlnminit)
    !
    ! Trajectory: Determination of the departure point of Lagrangian trajectory
    !
    !
    INTEGER, INTENT(IN   ) :: nit 
    LOGICAL, INTENT(IN   ) :: nlnminit
    REAL   , INTENT(IN   ) :: delt
    !
    !  local variables 
    !
    INTEGER :: iloc  (ibmax*kmax*4)
    INTEGER :: jloc  (ibmax*kmax)
    INTEGER :: kloc  (ibmax*kmax)
    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: klats
    INTEGER :: jb
    INTEGER :: ibdim
    INTEGER :: ip(ibmax*kmax*12)
    INTEGER :: j1(ibmax*kmax),j2(ibmax*kmax),j3(ibmax*kmax),j4(ibmax*kmax)
    REAL    :: s1(ibmax*kmax),s2(ibmax*kmax),s3(ibmax*kmax),s4(ibmax*kmax)
    REAL    :: fu    (ibmax,jbmax)
    REAL    :: fv    (ibmax,jbmax)
    REAL    :: ulat  (ibmax*kmax)
    REAL    :: ulon  (ibmax*kmax)
    REAL    :: usig  (ibmax*kmax)
    REAL    :: ulondp(ibmax*kmax*4)
    REAL    :: ulatdp(ibmax*kmax)
    REAL    :: usigdp(ibmax*kmax)
    REAL    :: fint  (ibmax*kmax)
    REAL    :: aux
    REAL    :: dt2
    REAL    :: delta
    REAL    :: delta1
  !
  ! Compute normalized winds for trajectory computations
  ! ----------------------------------------------------
  DO j = 1, jbmax
     DO i = 1, ibmaxperJB(j)
        aux = coslat(i,j) * er
        fgu (i,:,j)  =  fgu(i,:,j) / aux
        fgv (i,:,j)  =  fgv(i,:,j) / aux
     ENDDO
  ENDDO
  !
  ! Compute average wind on all levels
  ! ----------------------------------
  fu = 0.
  fv = 0.
  DO k = 1, kmax
     fu(:,:) = fu(:,:) + fgu(:,k,:) * del(k)
     fv(:,:) = fv(:,:) + fgv(:,k,:) * del(k)
  ENDDO
  !
  ! Prepare tendencies for interpolation on departure point
  ! -------------------------------------------------------
  delta = delt
  delta1 = delt
  if (nlnminit) then
     delta = 0.5
     delta1 = 1.0
  endif
  dt2 = delta1 + delta1
  fgyum = fgum + dt2 * fgyum + delta * fgyu
  fgyvm = fgvm + dt2 * fgyvm + delta * fgyv
  fgtdm = fgtmpm + dt2 * fgtdm + delta * fgtd
  fgqdm = fgqm + delta * fgqd
  fgvdlnpm = fglnpm + dt2 * fgvdlnpm + delta * fgvdlnp
  !
  ! Integration on all blocks and levels
  ! ------------------------------------
  DO jb=1,jbmax
     ibdim = ibmaxperJB(jb)
  !
  ! compute departure points 
  ! ------------------------
     CALL Trajectory(ulonm(1,jb),ulatm(1,jb),usigm(1,jb),&
          ulon  , ulat  , usig   , &
          ulondp, ulatdp, usigdp , &
          iloc  , jloc  , kloc   , &
          ibdim , jb    ,          &
          nit   , delta1  )           
  !
  ! Locate trajectory points for interpolation
  ! ------------------------------------------
     klats = 1 
     IF (reducedGrid) klats = 4
     CALL Locate( 2     ,.TRUE.  , &
          ulon  , ulat  , usig   , &
          ulondp, ulatdp, usigdp , &
          iloc  , jloc  , kloc   , &
          ibdim , jb    , klats)
  !
  ! Interpolate and finish tendencies
  ! ---------------------------------
     CALL Interpcublin3d(fint  , fgyum , .FALSE. , &
                         ulondp, ulatdp, usigdp, &
                         iloc  , jloc  , kloc  , &
                         ibdim , -1.0  , .TRUE., &
                         ip , j1, j2, j3, j4   , &
                         s1 , s2, s3, s4 )
!
!    The Variables fgumm and fgvmm are used here as auxiliary 
!     to hold the interpolated values of fgyum and fgyvm
!    This is safe to do, since this variables will be redefined
!    later (in Timefilterstep1) before used again.
!    
     DO k=1,kmax
        DO i=1,ibdim
           fgumm(i,k,jb) = fint((k-1)*ibdim+i)
        ENDDO
     ENDDO
     CALL Interpcublin3d(fint  , fgyvm , .TRUE.  , &
                         ulondp, ulatdp, usigdp, &
                         iloc  , jloc  , kloc  , &
                         ibdim , -1.0  ,.FALSE., &
                         ip , j1, j2, j3, j4   , &
                         s1 , s2, s3, s4 )
     DO k=1,kmax
        DO i=1,ibdim
           fgvmm(i,k,jb) = fint((k-1)*ibdim+i)
        ENDDO
     ENDDO
     CALL Interpcublin3d(fint  , fgqdm , .FALSE. , &
                         ulondp, ulatdp, usigdp, &
                         iloc  , jloc  , kloc  , &
                         ibdim ,  1.0  ,.FALSE., &
                         ip , j1, j2, j3, j4   , &
                         s1 , s2, s3, s4 )

     DO k=1,kmax
        DO i=1,ibdim
           fgqd(i,k,jb) = fint((k-1)*ibdim+i) + delta * fgqd(i,k,jb)
        ENDDO
     ENDDO
     CALL Interpcublin3d(fint  , fgtdm , .TRUE.  , &
                         ulondp, ulatdp, usigdp, &
                         iloc  , jloc  , kloc  , &
                         ibdim ,  1.0  ,.FALSE., &
                         ip , j1, j2, j3, j4   , &
                         s1 , s2, s3, s4 )
     DO k=1,kmax
        DO i=1,ibdim
           fgtd(i,k,jb) = fint((k-1)*ibdim+i)  + delta * fgtd(i,k,jb)
        ENDDO
     ENDDO
  !
  ! compute departure points (2d-trajectory) 
  ! ----------------------------------------
     CALL Trajectory2D(ulonm2D(1,jb),ulatm2D(1,jb),&
          ulon  , ulat  ,          &
          ulondp, ulatdp,          &
          iloc  , jloc  ,          &
          ibdim , jb    , nit    , delta1, fu, fv)
  !
  ! Locate trajectory points for interpolation
  ! ------------------------------------------
     CALL Locate2D( &
          ulon  , ulat  , &
          ulondp, ulatdp, &
          iloc  , jloc  , &
          ibdim , jb    , klats)
  !
  ! Interpolate and finish tendencies
  ! ---------------------------------
     CALL Interpcublin2d(fint  , fgvdlnpm, &
                         ulondp, ulatdp, &
                         iloc  , jloc  , &
                         ibdim)
     fgvdlnp(1:ibdim,jb) = fint(1:ibdim) + delta * fgvdlnp(1:ibdim,jb)
  ENDDO
  !
  !  transform tendencies of momentum equations in vector form
  !  ---------------------------------------------------------
  CALL Vectorialtend
  !
  !  finalize tendencies
  !  -------------------
  fgyu = fgyum + delta * fgyu
  fgyv = fgyvm + delta * fgyv
  !
  ! Restore winds
  ! -------------
  DO j = 1, jbmax
     DO i = 1, ibmaxperJB(j)
        aux = coslat(i,j) * er
        fgu (i,:,j)  =  fgu(i,:,j) * aux
        fgv (i,:,j)  =  fgv(i,:,j) * aux
     ENDDO
  ENDDO
  !
  if (nlnminit) then
     fgyu = fgyu - fgum
     fgyv = fgyv - fgvm
     fgtd = fgtd - fgtmpm
     fgqd = fgqd - fgqm
     fgvdlnp = fgvdlnp - fglnpm
  endif
  END SUBROUTINE SemiLagr
  SUBROUTINE Trajectory(        &
       ulonm , ulatm , usigm  , &
       ulon  , ulat  , usig   , &
       ulondp, ulatdp, usigdp , &
       iloc  , jloc  , kloc   , &
       ibdim , jb    ,          &
       nit   , delt   )
    !
    ! Trajectory: Determination of the departure point of Lagrangian trajectory
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: jb  
    INTEGER, INTENT(IN   ) :: nit 
    INTEGER, INTENT(OUT  ) :: iloc  (ibdim,kmax,4)
    INTEGER, INTENT(OUT  ) :: jloc  (ibdim,kmax)
    INTEGER, INTENT(OUT  ) :: kloc  (ibdim,kmax)
    REAL   , INTENT(IN   ) :: delt
    REAL   , INTENT(INOUT) :: ulon  (ibdim,kmax)
    REAL   , INTENT(INOUT) :: ulat  (ibdim,kmax)
    REAL   , INTENT(INOUT) :: usig  (ibdim,kmax)
    REAL   , INTENT(INOUT) :: ulonm (ibdim,kmax)
    REAL   , INTENT(INOUT) :: ulatm (ibdim,kmax)
    REAL   , INTENT(INOUT) :: usigm (ibdim,kmax)
    REAL   , INTENT(OUT  ) :: ulondp(ibdim,kmax,4)
    REAL   , INTENT(OUT  ) :: ulatdp(ibdim,kmax)
    REAL   , INTENT(OUT  ) :: usigdp(ibdim,kmax)
    !
    !  local variables 
    !
    INTEGER                        :: kit
    INTEGER                        :: klats
    LOGICAL                        :: final
    REAL                           :: uint(ibmax*kmax)
    REAL                           :: vint(ibmax*kmax)
    REAL                           :: wint(ibmax*kmax)
  !
  ! Iterate to find departure point
  ! -------------------------------
  !
  klats = 1
  IF (reducedGrid) klats = 2
  DO kit=1,nit
     final = kit.eq.nit
  !
  ! compute locations and indices for interpolation of the wind
  ! -----------------------------------------------------------
  !
     CALL Locate( 2     ,.TRUE.  , &
          ulonm , ulatm , usigm  , &
          ulondp, ulatdp, usigdp , &
          iloc  , jloc  , kloc   , &
          ibdim , jb    , klats)
  !
  ! interpolate vertical wind 
  ! -------------------------
  !
     CALL InterplG(&
          wint  , fgw            , &
          ulondp, ulatdp, usigdp , &
          iloc  , jloc  , kloc   , &
          ibdim )
  !
  ! update sigma level of mid-point of trajectory
  ! ---------------------------------------------
  !
     CALL Upsig (usig, usigm, wint, delt, ibdim, final)
  !
  ! computes new sigma location for interpolation
  ! ---------------------------------------------
  !
     CALL Locate( 2     ,.FALSE. , &
          ulonm , ulatm , usigm  , &
          ulondp, ulatdp, usigdp , &
          iloc  , jloc  , kloc   , &
          ibdim , jb    , klats)
  !
  ! interpolate horizontal wind field
  ! ---------------------------------
  !
     CALL InterpluvG(&
          uint  , vint           , &
          ulondp, ulatdp, usigdp , &
          iloc  , jloc  , kloc   , &
          ibdim )
  !
  ! update latitude and longitude of mid-point of trajectory
  ! --------------------------------------------------------
  !
     CALL Uplatlon(ulon,ulat,ulonm,ulatm,&
          coslon(1,jb),sinlon(1,jb),coslat(1,jb),sinlat(1,jb),&
          uint,vint,delt,ibdim,final)
  !

  ENDDO
  !
  END SUBROUTINE Trajectory
  SUBROUTINE Trajectory2D(        &
       ulonm , ulatm ,          &
       ulon  , ulat  ,          &
       ulondp, ulatdp,          &
       iloc  , jloc  ,          &
       ibdim , jb    ,          &
       nit   , delt  , fu     , fv )
    !
    ! Trajectory2D: Determination of the departure point of Lagrangian trajectory
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: jb  
    INTEGER, INTENT(IN   ) :: nit 
    INTEGER, INTENT(OUT  ) :: iloc  (ibdim,4)
    INTEGER, INTENT(OUT  ) :: jloc  (ibdim)
    REAL   , INTENT(IN   ) :: delt
    REAL   , INTENT(IN   ) :: fu    (ibmax,jbmax)
    REAL   , INTENT(IN   ) :: fv    (ibmax,jbmax)
    REAL   , INTENT(INOUT) :: ulon  (ibdim)
    REAL   , INTENT(INOUT) :: ulat  (ibdim)
    REAL   , INTENT(INOUT) :: ulonm (ibdim)
    REAL   , INTENT(INOUT) :: ulatm (ibdim)
    REAL   , INTENT(OUT  ) :: ulondp(ibdim,4)
    REAL   , INTENT(OUT  ) :: ulatdp(ibdim)
    !
    !  local variables 
    !
    INTEGER                        :: kit
    INTEGER                        :: klats
    LOGICAL                        :: final
    REAL                           :: uint(ibmax)
    REAL                           :: vint(ibmax)
  !
  ! Iterate to find departure point
  ! -------------------------------
  !
  klats = 1
  IF (reducedGrid) klats = 2
  DO kit=1,nit
     final = kit.eq.nit
  !
  ! compute locations and indices for interpolation of the wind
  ! -----------------------------------------------------------
  !
     CALL Locate2D( &
          ulonm , ulatm , &
          ulondp, ulatdp, &
          iloc  , jloc  , &
          ibdim , jb    , klats)
  !
  ! interpolate horizontal wind field
  ! ---------------------------------
  !
     CALL InterpluvG2D(&
          uint  , vint  , &
          ulondp, ulatdp, &
          iloc  , jloc  , &
          fu    , fv    , &
          ibdim)
  !
  ! update latitude and longitude of mid-point of trajectory
  ! --------------------------------------------------------
  !
     CALL Uplatlon2D(ulon,ulat,ulonm,ulatm,&
          coslon(1,jb),sinlon(1,jb),coslat(1,jb),sinlat(1,jb),&
          uint,vint,delt,ibdim,final)
  !
  ENDDO
  !
  END SUBROUTINE Trajectory2D
  SUBROUTINE Upsig (usig, usigm, w, delt, ibdim, final)
    !
    ! Upsig  : update sigma upstream
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    LOGICAL, INTENT(IN   ) :: final
    REAL   , INTENT(IN   ) :: w     (ibdim,kmax)
    REAL   , INTENT(OUT  ) :: usig  (ibdim,kmax)
    REAL   , INTENT(OUT  ) :: usigm (ibdim,kmax)
    REAL   , INTENT(IN   ) :: delt
    !
    !  local variables 
    !
    INTEGER                        :: i
    INTEGER                        :: k
  !
  DO k=1,kmax
     DO i=1,ibdim
        usigm(i,k) = MIN(cl(kmax),MAX(cl(1),cl(k) - delt * w(i,k)))
     ENDDO
  ENDDO
  IF (final) THEN
     DO k=1,kmax
        DO i=1,ibdim
           usig(i,k) = MIN(cl(kmax),MAX(cl(1),usigm(i,k)+usigm(i,k)-cl(k)))
        ENDDO
     ENDDO
  ENDIF
  !
  END SUBROUTINE Upsig
  SUBROUTINE Uplatlon(ulon,ulat,ulonm,ulatm,coslon,sinlon,coslat,sinlat,&
                      u,v,delt,ibdim,final)
    !
    ! Uplatlon: update latitude and longitude of upstream points
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    LOGICAL, INTENT(IN   ) :: final
    REAL   , INTENT(IN   ) :: coslon(ibdim)
    REAL   , INTENT(IN   ) :: sinlon(ibdim)
    REAL   , INTENT(IN   ) :: coslat(ibdim)
    REAL   , INTENT(IN   ) :: sinlat(ibdim)
    REAL   , INTENT(INOUT) :: ulat  (ibdim,kmax)
    REAL   , INTENT(INOUT) :: ulon  (ibdim,kmax)
    REAL   , INTENT(INOUT) :: ulatm (ibdim,kmax)
    REAL   , INTENT(INOUT) :: ulonm (ibdim,kmax)
    REAL   , INTENT(IN   ) :: u     (ibdim,kmax)
    REAL   , INTENT(IN   ) :: v     (ibdim,kmax)
    REAL   , INTENT(IN   ) :: delt
    !
    !  local variables 
    !
    INTEGER :: i
    INTEGER :: k
    REAL    :: dt2     ,&
               twodt   ,&
               dpi     ,&
               coslba  ,&
               sinlba  ,&
               cosphi  ,&
               sinphi  ,&
               xdot    ,&
               ydot    ,&
               zdot    ,&
               dot2    ,&
               x(ibmax),&
               y(ibmax),&
               z(ibmax),&
               xb      ,&
               yb      ,&
               zb      ,&
               xm      ,&
               ym      ,&
               zm      ,&
               r       ,&
               b
  !
  !
  dt2 = delt*delt
  twodt = 2. * delt
  dpi = 2. * pai
  DO i=1,ibdim
     x(i) = coslon(i) * coslat(i)
     y(i) = sinlon(i) * coslat(i)
     z(i) = sinlat(i)
  ENDDO
  DO k=1,kmax
     DO i=1,ibdim
        coslba = COS(ulonm(i,k))
        sinlba = SIN(ulonm(i,k))
        cosphi = COS(ulatm(i,k))
        sinphi = SIN(ulatm(i,k))
        xdot = - ( u(i,k) * sinlba + v(i,k) * coslba * sinphi )
        ydot =  u(i,k) * coslba - v(i,k) * sinlba * sinphi
        zdot = cosphi * v(i,k)
        r = 1. + dt2 * (xdot*xdot+ydot*ydot+zdot*zdot) - &
            twodt * (xdot*x(i)+ydot*y(i)+zdot*z(i))
        b = 1. / SQRT(r)
        xb = b * (x(i)-delt*xdot) 
        yb = b * (y(i)-delt*ydot) 
        zb = b * (z(i)-delt*zdot) 
        ulonm(i,k)=ATAN2(yb,xb)
        IF (ulonm(i,k).lt.0.) ulonm(i,k) = ulonm(i,k) + dpi
        ulatm(i,k)=ASIN(zb)
        IF (final) THEN
           dot2 = 2. * (x(i)*xb + y(i)*yb + z(i)*zb)
           xm = dot2 * xb - x(i)
           ym = dot2 * yb - y(i)
           zm = dot2 * zb - z(i)
           ulon(i,k)=ATAN2(ym,xm)
           IF (ulon(i,k).lt.0.) ulon(i,k) = ulon(i,k) + dpi
           ulat(i,k)=ASIN(zm)
        ENDIF
     ENDDO
  ENDDO
  !
  END SUBROUTINE Uplatlon
  SUBROUTINE Uplatlon2D(ulon,ulat,ulonm,ulatm,coslon,sinlon,coslat,sinlat,&
                      u,v,delt,ibdim,final)
    !
    ! Uplatlon: update latitude and longitude of upstream points
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    LOGICAL, INTENT(IN   ) :: final
    REAL   , INTENT(IN   ) :: coslon(ibdim)
    REAL   , INTENT(IN   ) :: sinlon(ibdim)
    REAL   , INTENT(IN   ) :: coslat(ibdim)
    REAL   , INTENT(IN   ) :: sinlat(ibdim)
    REAL   , INTENT(INOUT) :: ulat  (ibdim)
    REAL   , INTENT(INOUT) :: ulon  (ibdim)
    REAL   , INTENT(INOUT) :: ulatm (ibdim)
    REAL   , INTENT(INOUT) :: ulonm (ibdim)
    REAL   , INTENT(IN   ) :: u     (ibdim)
    REAL   , INTENT(IN   ) :: v     (ibdim)
    REAL   , INTENT(IN   ) :: delt
    !
    !  local variables 
    !
    INTEGER :: i
    REAL    :: dt2    ,&
               twodt  ,&
               dpi    ,&
               coslba ,&
               sinlba ,&
               cosphi ,&
               sinphi ,&
               xdot   ,&
               ydot   ,&
               zdot   ,&
               dot2   ,&
               x      ,&
               y      ,&
               z      ,&
               xb     ,&
               yb     ,&
               zb     ,&
               xm     ,&
               ym     ,&
               zm     ,&
               r      ,&
               b
  !
  !
  dt2 = delt*delt
  twodt = 2. * delt
  dpi = 2. * pai
  DO i=1,ibdim
     coslba = COS(ulonm(i))
     sinlba = SIN(ulonm(i))
     cosphi = COS(ulatm(i))
     sinphi = SIN(ulatm(i))
     x = coslon(i) * coslat(i)
     y = sinlon(i) * coslat(i)
     z = sinlat(i)
     xdot = - ( u(i) * sinlba + v(i) * coslba * sinphi )
     ydot =  u(i) * coslba - v(i) * sinlba * sinphi
     zdot = cosphi * v(i)
     r = 1. + dt2 * (xdot*xdot+ydot*ydot+zdot*zdot) - &
         twodt * (xdot*x+ydot*y+zdot*z)
     b = 1. / SQRT(r)
     xb = b * (x-delt*xdot) 
     yb = b * (y-delt*ydot) 
     zb = b * (z-delt*zdot) 
     ulonm(i)=ATAN2(yb,xb)
     IF (ulonm(i).lt.0.) ulonm(i) = ulonm(i) + dpi
     ulatm(i)=ASIN(zb)
     IF (final) THEN
        dot2 = 2. * (x*xb + y*yb + z*zb)
        xm = dot2 * xb - x
        ym = dot2 * yb - y
        zm = dot2 * zb - z
        ulon(i)=ATAN2(ym,xm)
        IF (ulon(i).lt.0.) ulon(i) = ulon(i) + dpi
        ulat(i)=ASIN(zm)
     ENDIF
  ENDDO
  !
  END SUBROUTINE Uplatlon2D
  SUBROUTINE Locate(&
       lsig  , llatlon        , &
       ulon  , ulat  , usig   , &
       ulondp, ulatdp, usigdp , &
       iloc  , jloc  , kloc   , &
       ibdim , jb    , klats)
    !
    ! locate : localize a set of points within the grid
    !
    !
    LOGICAL, INTENT(IN   ) :: llatlon
    INTEGER, INTENT(IN   ) :: lsig
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: jb  
    INTEGER, INTENT(IN   ) :: klats
    INTEGER, INTENT(OUT  ) :: iloc  (ibdim,kmax,klats)
    INTEGER, INTENT(OUT  ) :: jloc  (ibdim,kmax)
    INTEGER, INTENT(OUT  ) :: kloc  (ibdim,kmax)
    REAL   , INTENT(IN   ) :: ulon  (ibdim,kmax)
    REAL   , INTENT(IN   ) :: ulat  (ibdim,kmax)
    REAL   , INTENT(IN   ) :: usig  (ibdim,kmax)
    REAL   , INTENT(OUT  ) :: ulondp(ibdim,kmax,klats)
    REAL   , INTENT(OUT  ) :: ulatdp(ibdim,kmax)
    REAL   , INTENT(OUT  ) :: usigdp(ibdim,kmax)
    !
    !  local variables 
    !
    INTEGER                        :: i
    INTEGER                        :: ik
    INTEGER                        :: k
    INTEGER                        :: ks
    INTEGER                        :: j
    INTEGER                        :: kl
    INTEGER                        :: kp(4)
  !
  ! compute relative locations and indices for interpolation
  ! --------------------------------------------------------
  !
  IF (lsig.eq.2) THEN
     DO k=1,kmax
        DO i=1,ibdim
           ik = (usig(i,k)-cl(1)) / delsigref
           kloc(i,k) = ksigref(ik)
           IF (usig(i,k).gt.cl(kloc(i,k)+1)) kloc(i,k)=kloc(i,k)+1
           usigdp(i,k) = (usig(i,k)-cl(kloc(i,k)))/delcl(kloc(i,k))
        ENDDO
     ENDDO
  ENDIF

  IF (llatlon) THEN
     IF (klats.eq.4) THEN
        kp(1)=-1
        kp(2)=0
        kp(3)=1
        kp(4)=2
     ELSE
        kp(1)=0
        kp(2)=1
     ENDIF

     DO k=1,kmax
        DO i=1,ibdim
           ik = (ulat(i,k)+pihalf) / delphiref
           jloc(i,k) = jphiref(ik)
           IF (ulat(i,k).gt.phi(jloc(i,k)+1)) jloc(i,k)=jloc(i,k)+1
           ulatdp(i,k) = (ulat(i,k)-phi(jloc(i,k)))/delphi(jloc(i,k))
        ENDDO
     ENDDO

     DO kl=1,klats
        ks = kp(kl)
        DO k=1,kmax
           DO i=1,ibdim
              j = jPerIJB(i,jb)
              ulondp(i,k,kl) = ulon(i,k) / dellon(j+ks) + 1
              iloc(i,k,kl)   = ulondp(i,k,kl)
              ulondp(i,k,kl) = ulondp(i,k,kl) - iloc(i,k,kl)
           ENDDO
        ENDDO
     ENDDO
     IF (klats.eq.1) THEN
        iloc(:,:,2) = iloc(:,:,1)
        iloc(:,:,3) = iloc(:,:,1)
        iloc(:,:,4) = iloc(:,:,1)
        ulondp(:,:,2) = ulondp(:,:,1)
        ulondp(:,:,3) = ulondp(:,:,1)
        ulondp(:,:,4) = ulondp(:,:,1)
     ENDIF
  ENDIF
  !
  END SUBROUTINE Locate
  SUBROUTINE Locate2D(&
       ulon  , ulat  , &
       ulondp, ulatdp, &
       iloc  , jloc  , &
       ibdim , jb    , klats)
    !
    ! locate : localize a set of points within the grid
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: jb  
    INTEGER, INTENT(IN   ) :: klats
    INTEGER, INTENT(OUT  ) :: iloc  (ibdim,klats)
    INTEGER, INTENT(OUT  ) :: jloc  (ibdim)
    REAL   , INTENT(IN   ) :: ulon  (ibdim)
    REAL   , INTENT(IN   ) :: ulat  (ibdim)
    REAL   , INTENT(OUT  ) :: ulondp(ibdim,klats)
    REAL   , INTENT(OUT  ) :: ulatdp(ibdim)
    !
    !  local variables 
    !
    INTEGER                        :: i
    INTEGER                        :: ik
    INTEGER                        :: ks
    INTEGER                        :: j
    INTEGER                        :: kl
    INTEGER                        :: kp(2)
  !
  ! compute relative locations and indices for interpolation
  ! --------------------------------------------------------
  !
     kp(1)=0
     kp(2)=1

     DO i=1,ibdim
        ik = (ulat(i)+pihalf) / delphiref
        jloc(i) = jphiref(ik)
        IF (ulat(i).gt.phi(jloc(i)+1)) jloc(i)=jloc(i)+1
        ulatdp(i) = (ulat(i)-phi(jloc(i)))/delphi(jloc(i))
     ENDDO

     DO kl=1,klats
        ks = kp(kl)
        DO i=1,ibdim
           j = jPerIJB(i,jb)
           ulondp(i,kl) = ulon(i) / dellon(j+ks) + 1
           iloc(i,kl)   = ulondp(i,kl)
           ulondp(i,kl) = ulondp(i,kl) - iloc(i,kl)
        ENDDO
     ENDDO
     IF (klats.eq.1) THEN
        iloc(:,2) = iloc(:,1)
        iloc(:,3) = iloc(:,1)
        iloc(:,4) = iloc(:,1)
        ulondp(:,2) = ulondp(:,1)
        ulondp(:,3) = ulondp(:,1)
        ulondp(:,4) = ulondp(:,1)
     ENDIF
  !
  END SUBROUTINE Locate2D
  SUBROUTINE InterpluvG(&
       uint  , vint           , &
       ulondp, ulatdp, usigdp , &
       iloc  , jloc  , kloc   , &
       ibdim )
    !
    ! interpluvG : linear interpolation of horizontal components of wind
    !              ( on Gaussian grid )
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: iloc  (ibdim,kmax,2)
    INTEGER, INTENT(IN   ) :: jloc  (ibdim,kmax)
    INTEGER, INTENT(IN   ) :: kloc  (ibdim,kmax)
    REAL   , INTENT(IN   ) :: ulondp(ibdim,kmax,2)
    REAL   , INTENT(IN   ) :: ulatdp(ibdim,kmax)
    REAL   , INTENT(IN   ) :: usigdp(ibdim,kmax)
    REAL   , INTENT(OUT  ) :: uint  (ibdim,kmax)
    REAL   , INTENT(OUT  ) :: vint  (ibdim,kmax)
    !
    !  local variables 
    !
    INTEGER  :: i
    INTEGER  :: k
    INTEGER  :: jp 
    INTEGER  :: i1
    INTEGER  :: i2
    INTEGER  :: i3
    INTEGER  :: i4
    INTEGER  :: j1
    INTEGER  :: j2
    REAL     :: f1
    REAL     :: f2
    REAL     :: f3
    REAL     :: f4
    REAL     :: g1
    REAL     :: g2
    REAL     :: g3
    REAL     :: g4
    REAL     :: s1
    REAL     :: s2
  !
  ! define indices for interpolation
  ! --------------------------------
  !
  
  !
  ! perform the interpolation of u and v
  ! ------------------------------------
  !
  DO k=1,kmax
     DO i=1,ibdim
        jp = jmax+1-jloc(i,k)
        IF (jloc(i,k).eq.0) THEN
            s1 = -1.
         ELSE
            s1 = 1.
        ENDIF
        IF (jloc(i,k).eq.jmax) THEN
            s2 = -1.
         ELSE
            s2 = 1.
        ENDIF
        i1 = ibPerIJ(iloc(i,k,1),jp)
        i2 = ibPerIJ(iloc(i,k,2),jp-1)
        i3 = ibPerIJ(iloc(i,k,1)+1,jp)
        i4 = ibPerIJ(iloc(i,k,2)+1,jp-1)
        j1 = jbPerIJ(iloc(i,k,1),jp)
        j2 = jbPerIJ(iloc(i,k,2),jp-1)
        f1 = fgu(i1,kloc(i,k),j1) + ulondp(i,k,1) * &
              ( fgu(i3,kloc(i,k),j1) - fgu(i1,kloc(i,k),j1) )
        g1 = fgv(i1,kloc(i,k),j1) + ulondp(i,k,1) * &
              ( fgv(i3,kloc(i,k),j1) - fgv(i1,kloc(i,k),j1) )
        f2 = fgu(i2,kloc(i,k),j2) + ulondp(i,k,2) * &
              ( fgu(i4,kloc(i,k),j2) - fgu(i2,kloc(i,k),j2) )
        g2 = fgv(i2,kloc(i,k),j2) + ulondp(i,k,2) * &
              ( fgv(i4,kloc(i,k),j2) - fgv(i2,kloc(i,k),j2) )
        f3 = fgu(i1,kloc(i,k)+1,j1) + ulondp(i,k,1) * &
              ( fgu(i3,kloc(i,k)+1,j1) - fgu(i1,kloc(i,k)+1,j1) )
        g3 = fgv(i1,kloc(i,k)+1,j1) + ulondp(i,k,1) * &
              ( fgv(i3,kloc(i,k)+1,j1) - fgv(i1,kloc(i,k)+1,j1) )
        f4 = fgu(i2,kloc(i,k)+1,j2) + ulondp(i,k,2) * &
              ( fgu(i4,kloc(i,k)+1,j2) - fgu(i2,kloc(i,k)+1,j2) )
        g4 = fgv(i2,kloc(i,k)+1,j2) + ulondp(i,k,2) * &
              ( fgv(i4,kloc(i,k)+1,j2) - fgv(i2,kloc(i,k)+1,j2) )
        f1 = s1 * (f1 + usigdp(i,k) * (f3-f1))
        f2 = s2 * (f2 + usigdp(i,k) * (f4-f2))
        g1 = s1 * (g1 + usigdp(i,k) * (g3-g1))
        g2 = s2 * (g2 + usigdp(i,k) * (g4-g2))
        uint(i,k) = f1 + ulatdp(i,k) * (f2-f1)
        vint(i,k) = g1 + ulatdp(i,k) * (g2-g1)
     ENDDO
  ENDDO
  !
  END SUBROUTINE InterpluvG
  SUBROUTINE InterpluvG2D(&
       uint  , vint  , &
       ulondp, ulatdp, &
       iloc  , jloc  , &
       fu    , fv    , &
       ibdim)
    !
    ! interpluvG2D : linear interpolation of horizontal components of wind
    !              ( on Gaussian grid )
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: iloc  (ibdim,2)
    INTEGER, INTENT(IN   ) :: jloc  (ibdim)
    REAL   , INTENT(IN   ) :: ulondp(ibdim,2)
    REAL   , INTENT(IN   ) :: ulatdp(ibdim)
    REAL   , INTENT(IN   ) :: fu(ibmax,jbmax)
    REAL   , INTENT(IN   ) :: fv(ibmax,jbmax)
    REAL   , INTENT(OUT  ) :: uint  (ibdim)
    REAL   , INTENT(OUT  ) :: vint  (ibdim)
    !
    !  local variables 
    !
    INTEGER  :: i
    INTEGER  :: jp 
    INTEGER  :: i1
    INTEGER  :: i2
    INTEGER  :: i3
    INTEGER  :: i4
    INTEGER  :: j1
    INTEGER  :: j2
    REAL     :: f1
    REAL     :: f2
    REAL     :: g1
    REAL     :: g2
    REAL     :: s1(ibmax)
    REAL     :: s2(ibmax)
  !
  ! define indices for interpolation
  ! --------------------------------
  !
  DO i=1,ibdim
     jp = jmax+1-jloc(i) ! This is due to the reversed order of latitudes
     s1(i) = 1.          ! If across the pole a sign change 
     s2(i) = 1.          ! will be necessary
     if (jp.eq.jmax+1) s1(i) = -1.
     if (jp.eq.1) s2(i) = -1.
  ENDDO
  !
  ! perform the interpolation of u and v
  ! ------------------------------------
  !
  DO i=1,ibdim
     jp = jmax+1-jloc(i)
     i1 = ibPerIJ(iloc(i,1),jp)
     i2 = ibPerIJ(iloc(i,2),jp-1)
     i3 = ibPerIJ(iloc(i,1)+1,jp)
     i4 = ibPerIJ(iloc(i,2)+1,jp-1)
     j1 = jbPerIJ(iloc(i,1),jp)
     j2 = jbPerIJ(iloc(i,2),jp-1)
     f1 = s1(i) * ( fu(i1,j1) + ulondp(i,1) * &
           ( fu(i3,j1) - fu(i1,j1) ) )
     g1 = s1(i) * ( fv(i1,j1) + ulondp(i,1) * &
           ( fv(i3,j1) - fv(i1,j1) ) )
     f2 = s2(i) * ( fu(i2,j2) + ulondp(i,2) * &
           ( fu(i4,j2) - fu(i2,j2) ) )
     g2 = s2(i) * ( fv(i2,j2) + ulondp(i,2) * &
           ( fv(i4,j2) - fv(i2,j2) ) )
     uint(i) = f1 + ulatdp(i) * (f2-f1)
     vint(i) = g1 + ulatdp(i) * (g2-g1)
  ENDDO
  !
  END SUBROUTINE InterpluvG2D
  SUBROUTINE InterplG(&
       fint  , f              , &
       ulondp, ulatdp, usigdp , &
       iloc  , jloc  , kloc   , &
       ibdim )
    !
    ! interplG : linear interpolation of a field f
    !              ( on Gaussian grid )
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: iloc  (ibdim,kmax,2)
    INTEGER, INTENT(IN   ) :: jloc  (ibdim,kmax)
    INTEGER, INTENT(IN   ) :: kloc  (ibdim,kmax)
    REAL   , INTENT(IN   ) :: ulondp(ibdim,kmax,2)
    REAL   , INTENT(IN   ) :: ulatdp(ibdim,kmax)
    REAL   , INTENT(IN   ) :: usigdp(ibdim,kmax)
    REAL   , INTENT(IN   ) :: f     (ibmax,kmax,jbmax)
    REAL   , INTENT(OUT  ) :: fint  (ibdim,kmax)
    !
    !  local variables 
    !
    INTEGER  :: i
    INTEGER  :: k
    INTEGER  :: jp 
    INTEGER  :: i1
    INTEGER  :: i2
    INTEGER  :: i3
    INTEGER  :: i4
    INTEGER  :: j1
    INTEGER  :: j2
    REAL     :: f1
    REAL     :: f2
    REAL     :: f3
    REAL     :: f4
  !
  !
  ! perform the interpolation of f
  ! ------------------------------
  !
  DO k=1,kmax
     DO i=1,ibdim
        jp = jmax+1-jloc(i,k) ! This is due to the reversed order 
        i1 = ibPerIJ(iloc(i,k,1),jp)
        i2 = ibPerIJ(iloc(i,k,2),jp-1)
        i3 = ibPerIJ(iloc(i,k,1)+1,jp)
        i4 = ibPerIJ(iloc(i,k,2)+1,jp-1)
        j1 = jbPerIJ(iloc(i,k,1),jp)
        j2 = jbPerIJ(iloc(i,k,2),jp-1)
        f1 = f(i1,kloc(i,k),j1) + ulondp(i,k,1) * &
              ( f(i3,kloc(i,k),j1) - f(i1,kloc(i,k),j1) )
        f2 = f(i2,kloc(i,k),j2) + ulondp(i,k,2) * &
              ( f(i4,kloc(i,k),j2) - f(i2,kloc(i,k),j2) )
        f3 = f(i1,kloc(i,k)+1,j1) + ulondp(i,k,1) * &
              ( f(i3,kloc(i,k)+1,j1) - f(i1,kloc(i,k)+1,j1) )
        f4 = f(i2,kloc(i,k)+1,j2) + ulondp(i,k,2) * &
              ( f(i4,kloc(i,k)+1,j2) - f(i2,kloc(i,k)+1,j2) )
        f1 = f1 + usigdp(i,k) * (f3-f1)
        f2 = f2 + usigdp(i,k) * (f4-f2)
        fint(i,k) = f1 + ulatdp(i,k) * (f2-f1)
     ENDDO
  ENDDO
  !
  END SUBROUTINE InterplG
  SUBROUTINE Interpcublin2d(&
       fint  , f     , &
       ulondp, ulatdp, &
       iloc  , jloc  , &
       ibdim)
    !
    ! Interpcublin2d : quasi-cubic interpolation of a field f
    !              ( on Gaussian grid )
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: iloc  (ibdim,4)
    INTEGER, INTENT(IN   ) :: jloc  (ibdim)
    REAL   , INTENT(IN   ) :: ulondp(ibdim,4)
    REAL   , INTENT(IN   ) :: ulatdp(ibdim)
    REAL   , INTENT(IN   ) :: f     (ibmax,jbmax)
    REAL   , INTENT(OUT  ) :: fint  (ibdim)
    !
    !  local variables 
    !
    INTEGER  :: i
    INTEGER  :: j
    INTEGER  :: jp 
    INTEGER  :: ip(ibmax,12)
    INTEGER  :: j1(ibmax),j2(ibmax),j3(ibmax),j4(ibmax)
    REAL     :: f1(ibmax)
    REAL     :: f2(ibmax)
    REAL     :: f3(ibmax)
    REAL     :: f4(ibmax)
    REAL     :: fa,fb,fc,fd,s,t,t1,t2,t3,t4
  !
  !
  ! perform the interpolation of f
  ! ------------------------------
  !
  DO i=1,ibdim
     jp = jmax+1-jloc(i) ! This is due to the reversed order 
     ip(i,1) = ibPerIJ(iloc(i,1),jp+1)
     ip(i,2) = ibPerIJ(iloc(i,1)+1,jp+1)
     ip(i,3) = ibPerIJ(iloc(i,2)-1,jp)
     ip(i,4) = ibPerIJ(iloc(i,2),jp)
     ip(i,5) = ibPerIJ(iloc(i,2)+1,jp)
     ip(i,6) = ibPerIJ(iloc(i,2)+2,jp)
     ip(i,7) = ibPerIJ(iloc(i,3)-1,jp-1)
     ip(i,8) = ibPerIJ(iloc(i,3),jp-1)
     ip(i,9) = ibPerIJ(iloc(i,3)+1,jp-1)
     ip(i,10) = ibPerIJ(iloc(i,3)+2,jp-1)
     ip(i,11) = ibPerIJ(iloc(i,4),jp-2)
     ip(i,12) = ibPerIJ(iloc(i,4)+1,jp-2)
     j1(i) = jbPerIJ(iloc(i,1),jp+1)
     j2(i) = jbPerIJ(iloc(i,2),jp)
     j3(i) = jbPerIJ(iloc(i,3),jp-1)
     j4(i) = jbPerIJ(iloc(i,4),jp-2)
  ENDDO
  DO i=1,ibdim
     f1(i) = f(ip(i,1),j1(i)) + ulondp(i,1) * &
           ( f(ip(i,2),j1(i)) - f(ip(i,1),j1(i)) )
     s = ulondp(i,2)-0.5
     fa = 0.5 * ( f(ip(i,5),j2(i)) + f(ip(i,4),j2(i)) ) + &
            s * ( f(ip(i,5),j2(i)) - f(ip(i,4),j2(i)) )
     fb = 0.5 * ( f(ip(i,6),j2(i)) + f(ip(i,3),j2(i)) ) + &
            s * ( f(ip(i,6),j2(i)) - f(ip(i,3),j2(i)) ) / 3.
     f2(i) = fa + (0.125 - 0.5*s*s) * (fa-fb) 
     t = ulondp(i,3)-0.5
     fc = 0.5 * ( f(ip(i,9),j3(i)) + f(ip(i,8),j3(i)) ) + &
            t * ( f(ip(i,9),j3(i)) - f(ip(i,8),j3(i)) )
     fd = 0.5 * ( f(ip(i,10),j3(i)) + f(ip(i,7),j3(i)) ) + &
            t * ( f(ip(i,10),j3(i)) - f(ip(i,7),j3(i)) ) / 3.
     f3(i) = fc + (0.125 - 0.5*t*t) * (fc-fd) 
     f4(i) = f(ip(i,11),j4(i)) + ulondp(i,4) * &
           ( f(ip(i,12),j4(i)) - f(ip(i,11),j4(i)) )
  ENDDO
  DO i=1,ibdim
     j = jloc(i)
     s = ulatdp(i)
     t1 = 1. - s
     t2 = cubcoef(j,2) + s
     t3 = cubcoef(j,1) + t1
     t4 = s * t2
     fint(i) = t3 * ( t1 * ( cubcoef(j,3) * s * f1(i) + &
                             cubcoef(j,4) * t2 * f2(i) ) + &
                             cubcoef(j,5) * t4 * f3(i) ) + &
                             cubcoef(j,6) * t4 * t1 * f4(i)
  ENDDO
  !
  END SUBROUTINE Interpcublin2d
  SUBROUTINE Interpcublin3d(&
       fint  , f     , again , &
       ulondp, ulatdp, usigdp, &
       iloc  , jloc  , kloc  , &
       ibdim , signal, first , &
       ip    , j1, j2, j3, j4, &
       s1 , s2, s3, s4 )
    !
    ! Interpcublin3d : quasi-cubic interpolation of a field f
    !              ( on Gaussian grid )
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: iloc  (ibdim,kmax,4)
    INTEGER, INTENT(IN   ) :: jloc  (ibdim,kmax)
    INTEGER, INTENT(IN   ) :: kloc  (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: j1    (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: j2    (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: j3    (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: j4    (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: ip    (ibdim,kmax,12)
    LOGICAL, INTENT(IN   ) :: first
    LOGICAL, INTENT(IN   ) :: again
    REAL   , INTENT(INOUT) :: s1    (ibdim,kmax)
    REAL   , INTENT(INOUT) :: s2    (ibdim,kmax)
    REAL   , INTENT(INOUT) :: s3    (ibdim,kmax)
    REAL   , INTENT(INOUT) :: s4    (ibdim,kmax)
    REAL   , INTENT(IN   ) :: signal
    REAL   , INTENT(IN   ) :: ulondp(ibdim,kmax,4)
    REAL   , INTENT(IN   ) :: ulatdp(ibdim,kmax)
    REAL   , INTENT(IN   ) :: usigdp(ibdim,kmax)
    REAL   , INTENT(IN   ) :: f     (ibmax,kmax,jbmax)
    REAL   , INTENT(OUT  ) :: fint  (ibdim,kmax)
    !
    !  local variables 
    !
    INTEGER  :: i
    INTEGER  :: j
    INTEGER  :: k
    INTEGER  :: km
    INTEGER  :: k1
    INTEGER  :: jp 
    REAL     :: f1(ibmax,kmax)
    REAL     :: f2(ibmax,kmax)
    REAL     :: f3(ibmax,kmax)
    REAL     :: f4(ibmax,kmax)
    REAL     :: fint1(ibmax,kmax)
    REAL     :: fint2(ibmax,kmax)
    REAL     :: fint3(ibmax,kmax)
    REAL     :: fint4(ibmax,kmax)
    REAL     :: fa,fb,fc,fd,s,t,t1,t2,t3,t4
  !
  !
  ! perform the interpolation of f
  ! ------------------------------
  !
  IF (first) THEN 
     DO km=1,kmax
        DO i=1,ibdim
           jp = jmax+1-jloc(i,km) ! This is due to the reversed order 
           ip(i,km,1) = ibPerIJ(iloc(i,km,1),jp+1)
           ip(i,km,2) = ibPerIJ(iloc(i,km,1)+1,jp+1)
           ip(i,km,3) = ibPerIJ(iloc(i,km,2)-1,jp)
           ip(i,km,4) = ibPerIJ(iloc(i,km,2),jp)
           ip(i,km,5) = ibPerIJ(iloc(i,km,2)+1,jp)
           ip(i,km,6) = ibPerIJ(iloc(i,km,2)+2,jp)
           ip(i,km,7) = ibPerIJ(iloc(i,km,3)-1,jp-1)
           ip(i,km,8) = ibPerIJ(iloc(i,km,3),jp-1)
           ip(i,km,9) = ibPerIJ(iloc(i,km,3)+1,jp-1)
           ip(i,km,10) = ibPerIJ(iloc(i,km,3)+2,jp-1)
           ip(i,km,11) = ibPerIJ(iloc(i,km,4),jp-2)
           ip(i,km,12) = ibPerIJ(iloc(i,km,4)+1,jp-2)
           j1(i,km) = jbPerIJ(iloc(i,km,1),jp+1)
           j2(i,km) = jbPerIJ(iloc(i,km,2),jp)
           j3(i,km) = jbPerIJ(iloc(i,km,3),jp-1)
           j4(i,km) = jbPerIJ(iloc(i,km,4),jp-2)
        ENDDO
     ENDDO
  ENDIF
  IF (.not.again) THEN 
     s1 = 1.
     s2 = 1.
     s3 = 1.
     s4 = 1.
     DO km=1,kmax
        DO i=1,ibdim
           jp = jmax+1-jloc(i,km) ! This is due to the reversed order 
           IF (jp.eq.jmax+1) THEN 
              s1(i,km) = signal
              s2(i,km) = signal
           ENDIF
           IF (jp.eq.jmax) THEN 
              s1(i,km) = signal
           ENDIF
           IF (jp.eq.1) THEN 
              s3(i,km) = signal
              s4(i,km) = signal
           ENDIF
           IF (jp.eq.2) THEN 
              s4(i,km) = signal
           ENDIF
        ENDDO
     ENDDO
  ENDIF
  DO km=1,kmax
     DO i=1,ibdim
        k = kloc(i,km)
        f1(i,km) = s1(i,km) * ( f(ip(i,km,1),k,j1(i,km)) + ulondp(i,km,1) * &
              ( f(ip(i,km,2),k,j1(i,km)) - f(ip(i,km,1),k,j1(i,km)) ) )
        s = ulondp(i,km,2)-0.5
        fa = 0.5 * ( f(ip(i,km,5),k,j2(i,km)) + f(ip(i,km,4),k,j2(i,km)) ) + &
               s * ( f(ip(i,km,5),k,j2(i,km)) - f(ip(i,km,4),k,j2(i,km)) )
        fb = 0.5 * ( f(ip(i,km,6),k,j2(i,km)) + f(ip(i,km,3),k,j2(i,km)) ) + &
               s * ( f(ip(i,km,6),k,j2(i,km)) - f(ip(i,km,3),k,j2(i,km)) ) / 3.
        f2(i,km) = s2(i,km) * ( fa + (0.125 - 0.5*s*s) * (fa-fb) ) 
        t = ulondp(i,km,3)-0.5
        fc = 0.5 * ( f(ip(i,km,9),k,j3(i,km)) + f(ip(i,km,8),k,j3(i,km)) ) + &
               t * ( f(ip(i,km,9),k,j3(i,km)) - f(ip(i,km,8),k,j3(i,km)) )
        fd = 0.5 * ( f(ip(i,km,10),k,j3(i,km)) + f(ip(i,km,7),k,j3(i,km)) ) + &
               t * ( f(ip(i,km,10),k,j3(i,km)) - f(ip(i,km,7),k,j3(i,km)) ) / 3.
        f3(i,km) = s3(i,km) * ( fc + (0.125 - 0.5*t*t) * (fc-fd) )
        f4(i,km) = s4(i,km) * ( f(ip(i,km,11),k,j4(i,km)) + ulondp(i,km,4) * &
              ( f(ip(i,km,12),k,j4(i,km)) - f(ip(i,km,11),k,j4(i,km)) ) )
     ENDDO
  ENDDO
     
  DO km=1,kmax
     DO i=1,ibdim
        j = jloc(i,km)
        s = ulatdp(i,km)
        t1 = 1. - s
        t2 = cubcoef(j,2) + s
        t3 = cubcoef(j,1) + t1
        t4 = s * t2
        fint2(i,km) = t3 * ( t1 * ( cubcoef(j,3) * s * f1(i,km) + &
                                    cubcoef(j,4) * t2 * f2(i,km) ) + &
                                    cubcoef(j,5) * t4 * f3(i,km) ) + &
                                    cubcoef(j,6) * t4 * t1 * f4(i,km)
     ENDDO
  ENDDO
  !
  DO km=1,kmax
     DO i=1,ibdim
        k = kloc(i,km)+1
        f1(i,km) = s1(i,km) * ( f(ip(i,km,1),k,j1(i,km)) + ulondp(i,km,1) * &
              ( f(ip(i,km,2),k,j1(i,km)) - f(ip(i,km,1),k,j1(i,km)) ) )
        s = ulondp(i,km,2)-0.5
        fa = 0.5 * ( f(ip(i,km,5),k,j2(i,km)) + f(ip(i,km,4),k,j2(i,km)) ) + &
               s * ( f(ip(i,km,5),k,j2(i,km)) - f(ip(i,km,4),k,j2(i,km)) )
        fb = 0.5 * ( f(ip(i,km,6),k,j2(i,km)) + f(ip(i,km,3),k,j2(i,km)) ) + &
               s * ( f(ip(i,km,6),k,j2(i,km)) - f(ip(i,km,3),k,j2(i,km)) ) / 3.
        f2(i,km) = s2(i,km) * ( fa + (0.125 - 0.5*s*s) * (fa-fb) ) 
        t = ulondp(i,km,3)-0.5
        fc = 0.5 * ( f(ip(i,km,9),k,j3(i,km)) + f(ip(i,km,8),k,j3(i,km)) ) + &
               t * ( f(ip(i,km,9),k,j3(i,km)) - f(ip(i,km,8),k,j3(i,km)) )
        fd = 0.5 * ( f(ip(i,km,10),k,j3(i,km)) + f(ip(i,km,7),k,j3(i,km)) ) + &
               t * ( f(ip(i,km,10),k,j3(i,km)) - f(ip(i,km,7),k,j3(i,km)) ) / 3.
        f3(i,km) = s3(i,km) * ( fc + (0.125 - 0.5*t*t) * (fc-fd) )
        f4(i,km) = s4(i,km) * ( f(ip(i,km,11),k,j4(i,km)) + ulondp(i,km,4) * &
              ( f(ip(i,km,12),k,j4(i,km)) - f(ip(i,km,11),k,j4(i,km)) ) )
     ENDDO
  ENDDO
  DO km=1,kmax
     DO i=1,ibdim
        j = jloc(i,km)
        s = ulatdp(i,km)
        t1 = 1. - s
        t2 = cubcoef(j,2) + s
        t3 = cubcoef(j,1) + t1
        t4 = s * t2
        fint3(i,km) = t3 * ( t1 * ( cubcoef(j,3) * s * f1(i,km) + &
                                    cubcoef(j,4) * t2 * f2(i,km) ) + &
                                    cubcoef(j,5) * t4 * f3(i,km) ) + &
                                    cubcoef(j,6) * t4 * t1 * f4(i,km)
     ENDDO
  ENDDO
  !
  DO km=1,kmax
     DO i=1,ibdim
        IF (kloc(i,km).ne.1.and.kloc(i,km).ne.kmax-1) THEN
           k = kloc(i,km) - 1
           k1 = kloc(i,km) + 2
           f2(i,km) = s2(i,km) * ( f(ip(i,km,4),k,j2(i,km)) + ulondp(i,km,2) * &
                 ( f(ip(i,km,5),k,j2(i,km)) - f(ip(i,km,4),k,j2(i,km)) ) )
           f3(i,km) = s3(i,km) * ( f(ip(i,km,8),k,j3(i,km)) + ulondp(i,km,3) * &
                 ( f(ip(i,km,9),k,j3(i,km)) - f(ip(i,km,8),k,j3(i,km)) ) )
           f1(i,km) = s2(i,km) * ( f(ip(i,km,4),k1,j2(i,km)) + ulondp(i,km,2) * &
                 ( f(ip(i,km,5),k1,j2(i,km)) - f(ip(i,km,4),k1,j2(i,km)) ) )
           f4(i,km) = s3(i,km) * ( f(ip(i,km,8),k1,j3(i,km)) + ulondp(i,km,3) * &
                 ( f(ip(i,km,9),k1,j3(i,km)) - f(ip(i,km,8),k1,j3(i,km)) ) )
           fint1(i,km) = f2(i,km) + ulatdp(i,km) * (f3(i,km)-f2(i,km)) 
           fint4(i,km) = f1(i,km) + ulatdp(i,km) * (f4(i,km)-f1(i,km)) 
           s = usigdp(i,km)
           t1 = 1. - s
           t2 = cubcoefv(kloc(i,km),2) + s
           t3 = cubcoefv(kloc(i,km),1) + t1
           t4 = s * t2
           fint(i,km) = t3 * ( t1 * ( cubcoefv(kloc(i,km),3) * s * fint1(i,km) + &
                                      cubcoefv(kloc(i,km),4) * t2 * fint2(i,km) ) + &
                                      cubcoefv(kloc(i,km),5) * t4 * fint3(i,km) ) + &
                                      cubcoefv(kloc(i,km),6) * t4 * t1 * fint4(i,km)
        ELSE
           fint(i,km) = fint2(i,km) + usigdp(i,km) * ( fint3(i,km)-fint2(i,km) )
        ENDIF
     ENDDO
  ENDDO
  !
  END SUBROUTINE Interpcublin3d
  SUBROUTINE Vectorialtend ( )
  !
  ! Vectorialtend : Compute tendencies for vectorial formulation
  !               of horizontal momentum equations through projection
  !               on tangent plane
  !
  !  local variables 
  !
  INTEGER  :: i
  INTEGER  :: jb
  INTEGER  :: k
  REAL     :: amf = 1.0
  REAL     :: dlon
  REAL     :: sdlon
  REAL     :: cdlon
  REAL     :: cgca
  REAL     :: alpha
  REAL     :: alpham
  REAL     :: sgam
  REAL     :: cgam
  REAL     :: gamm
  REAL     :: clat
  REAL     :: slat
  REAL     :: sgamma
  REAL     :: cgamma
  REAL     :: fsalpx
  REAL     :: fsalpy
  REAL     :: fcalpx
  REAL     :: fcalpy
  REAL     :: salph 
  REAL     :: calph 
  REAL     :: salphm 
  REAL     :: calphm 
  REAL     :: xp
  REAL     :: yp
  REAL     :: xm
  REAL     :: ym
  REAL     :: px
  REAL     :: py
  REAL     :: x2y2
  !
  DO jb=1,jbmax
     DO k=1,kmax
        DO i=1,ibmaxperJB(jb)
           slat = SIN(ulatm(i+ibmaxperJB(jb)*(k-1),jb))
           slat =sign(max(abs(slat),1.e-20),slat)
           clat = COS(ulatm(i+ibmaxperJB(jb)*(k-1),jb))
           dlon = longit(i,jb)-ulonm(i+ibmaxperJB(jb)*(k-1),jb)
           sdlon = SIN(dlon)
           cdlon = COS(dlon)
           cgca = sinlat(i,jb) * slat + coslat(i,jb) * clat * cdlon
!          alpha = ACOS(cgca)
           alpha=ACOS(sign(min(abs(cgca),1.),cgca))
           sgam = (clat * cgca - cdlon * coslat(i,jb)) / slat
           cgam = coslat(i,jb) * sdlon
           cgam=sign(max(abs(cgam),1.e-20),cgam)
           gamm = ATAN2(sgam,cgam)
           sgamma = SIN(gamm)
           cgamma = COS(gamm)
           alpham = - amf * alpha
           fsalpx = cgamma * slat
           fcalpx = sgamma * cgamma * clat
           fsalpy = sgamma * slat
           fcalpy = sgamma**2 * clat
           salph  = SIN(alpha)
           calph  = 1. - COS(alpha)
           salphm = SIN(alpham)
           calphm = 1. - COS(alpham)
           xp = salph * fsalpx + calph * fcalpx
           yp = clat - salph * fsalpy - calph * fcalpy
           xm = salphm * fsalpx + calphm * fcalpx
           ym = clat - salphm * fsalpy - calphm * fcalpy
           x2y2 = xm * xm + ym * ym
           px = ( ym * fgumm(i,k,jb) - xm * fgvmm(i,k,jb) ) / x2y2
           py = ( xm * fgumm(i,k,jb) + ym * fgvmm(i,k,jb) ) / x2y2
           fgyum(i,k,jb) = xp * py + yp * px
           fgyvm(i,k,jb) = yp * py - xp * px
        ENDDO
     ENDDO
  ENDDO
  END SUBROUTINE Vectorialtend


  SUBROUTINE InitSL

    ! local variables 
    INTEGER :: i, j, k, ib, jb
    REAL    :: p1, p2


    ALLOCATE(phi(-1:jmax+2))
    ALLOCATE(delphi(-1:jmax+2))
    ALLOCATE(dellon(-1:jmax+2))
    ALLOCATE(cubcoef(0:jmax,6))
    ALLOCATE(cubcoefv(1:kmax,6))
    ALLOCATE(ulonm  (ibmax*kmax,jbmax))
    ALLOCATE(ulatm  (ibmax*kmax,jbmax))
    ALLOCATE(usigm  (ibmax*kmax,jbmax))
    ALLOCATE(ulonm2D  (ibmax,jbmax))
    ALLOCATE(ulatm2D  (ibmax,jbmax))


    DO j=1,jmax 
       dellon(j) = 2. * pai /imaxperj(j)
       phi(j) = colrad(j) - pihalf
    ENDDO
    phi(0)= - colrad(1) - pihalf
    phi(-1) = -colrad(2) - pihalf
    phi(jmax+1) = - phi(0)
    phi(jmax+2) = - phi(-1)
    dellon(0) = dellon(1)
    dellon(-1) = dellon(2)
    dellon(jmax+1) = dellon(jmax)
    dellon(jmax+2) = dellon(jmax-1)
    DO j=-1,jmax+1
       delphi(j) = phi(j+1)-phi(j)
    ENDDO
    delphiref = delphi(-1)
    DO j=0,jmax+1
       delphiref = MIN(delphiref,delphi(j))
    ENDDO
    delsigref = delcl(1)
    DO k=2,kmax-1
       delsigref = MIN(delsigref,delcl(k))
    ENDDO
    delphiref = delphiref/2.
    delsigref = delsigref/2.
    kmaxref = ( cl(kmax)-cl(1) ) / delsigref + 1
    jmaxref = pai / delphiref + 1
    delphiref = pai / jmaxref
    delsigref = ( cl(kmax)-cl(1) ) / kmaxref
 
    ALLOCATE(jphiref(0:jmaxref))
    ALLOCATE(ksigref(0:kmaxref))

    jphiref(0) = 0
    k = 0
    DO j=1,jmaxref
       IF (j*delphiref-pihalf.gt.phi(k+1)) k = k + 1
       jphiref(j) = k
    ENDDO
    ksigref(0) = 1
    j = 1
    DO k=1,kmaxref
       IF (k*delsigref+cl(1).gt.cl(j+1)) j = j + 1
       j = MIN(j,kmax-1)
       ksigref(k) = j
    ENDDO

    DO j=0,jmax
       p1 = delphi(j+1) / delphi(j)
       p2 = delphi(j-1) / delphi(j)
       cubcoef(j,1) = p1
       cubcoef(j,2) = p2
       cubcoef(j,3) = -1. / (p2*(1.+p2)*(1.+p2+p1))
       cubcoef(j,4) = 1. / (p2*(1.+p1))
       cubcoef(j,5) = 1. / (p1*(1.+p2))
       cubcoef(j,6) = -1. / (p1*(1.+p1)*(1.+p2+p1))
    ENDDO
    DO j=2,kmax-2
       p1 = delcl(j+1) / delcl(j)
       p2 = delcl(j-1) / delcl(j)
       cubcoefv(j,1) = p1
       cubcoefv(j,2) = p2
       cubcoefv(j,3) = -1. / (p2*(1.+p2)*(1.+p2+p1))
       cubcoefv(j,4) = 1. / (p2*(1.+p1))
       cubcoefv(j,5) = 1. / (p1*(1.+p2))
       cubcoefv(j,6) = -1. / (p1*(1.+p1)*(1.+p2+p1))
    ENDDO

    DO jb=1,jbmax
       DO ib=1,ibmaxperJB(jb)
          i = iperIJB(ib,jb)
          j = jperIJB(ib,jb)
          ulonm2D(ib,jb) = (i-1)*dellon(j)
          ulatm2D(ib,jb) = - phi(j)
          DO k=1,kmax
             usigm  (ib+(k-1)*ibmaxperJB(jb),jb) = cl(k)
             ulonm  (ib+(k-1)*ibmaxperJB(jb),jb) = (i-1)*dellon(j)
             ulatm  (ib+(k-1)*ibmaxperJB(jb),jb) = - phi(j)
          ENDDO
       ENDDO
    ENDDO

  END SUBROUTINE InitSL

END MODULE SemiLagrangian
