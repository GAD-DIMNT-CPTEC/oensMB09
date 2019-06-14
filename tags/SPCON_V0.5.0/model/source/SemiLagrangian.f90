!
!  $Author: panetta $
!  $Date: 2007/08/12 13:52:18 $
!  $Revision: 1.7 $
!
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
  USE Parallelism, ONLY: &
       maxnodes

  USE Constants,    ONLY: &
       er               , &
       gasr             , &
       tbar             , &
       pihalf           , &
       pai              , &
       r8

  USE Options,      ONLY: &
       reducedGrid      , &
       nscalars         , &
       start            , &
       jovlap

  USE Sizes,        ONLY: &
       iPerIJB          , &
       jPerIJB          , &
       ibPerIJ          , &
       jbPerIJ          , &
       ibmaxperJB       , &
       imaxperJ         , &
       myfirstlat       , &
       mylastlat        , &
       ibmax            , &
       jbmax            , &
       jbminus          , &
       jbplus           , &
       kmax             , &
       imax             , &
       jmax             , &
       del              , &
       delcl            , & 
       ci               , &
       cl                 
       
  USE FieldsDynamics,ONLY: &
       fgpass_scalars   , &
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
       fgzs             , &
       fgumean          , &
       fgvmean          , &
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

  USE Communications, ONLY : &
       Exchange_Winds   , &
       Exchange_Fields  , &
       Complete_Exchange

  IMPLICIT NONE
  ! Use to store location of the 3d dep

  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: ulonm  (:,:)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: ulatm  (:,:)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: usigm  (:,:)

  ! Use to store location of the 2d dep

  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: ulonm2D  (:,:)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: ulatm2D  (:,:)

  REAL(KIND=r8), ALLOCATABLE :: phi(:,:)
  REAL(KIND=r8), ALLOCATABLE :: delphi(:,:)
  REAL(KIND=r8), ALLOCATABLE :: sigma(:,:)
  REAL(KIND=r8), ALLOCATABLE :: delsigma(:,:)
  REAL(KIND=r8), ALLOCATABLE :: dellon(:,:)
  REAL(KIND=r8), ALLOCATABLE :: cubcoef(:,:,:)
  REAL(KIND=r8), ALLOCATABLE :: cubcoefv(:,:,:)

  INTEGER, ALLOCATABLE :: jphiref(:,:)
  INTEGER, ALLOCATABLE :: ksigref(:,:)
  INTEGER, ALLOCATABLE :: requestsm(:)
  INTEGER, ALLOCATABLE :: requestrm(:)
  INTEGER, ALLOCATABLE :: requests(:)
  INTEGER, ALLOCATABLE :: requestr(:)
  
  INTEGER :: kmaxref
  INTEGER :: jmaxref
  INTEGER :: jov
  INTEGER :: jglob
  REAL(KIND=r8)    :: delsigref
  REAL(KIND=r8)    :: delphiref
  PRIVATE
  PUBLIC :: SemiLagr, InitSL
CONTAINS
  SUBROUTINE SemiLagr(nit , delt, nlnminit, inew, iold)
    !
    ! Trajectory: Determination of the departure point of Lagrangian trajectory
    !
    !
    INTEGER, INTENT(IN   ) :: nit 
    INTEGER, INTENT(IN   ) :: inew
    INTEGER, INTENT(IN   ) :: iold
    LOGICAL, INTENT(IN   ) :: nlnminit
    REAL(KIND=r8)   , INTENT(IN   ) :: delt
    !
    !  local variables 
    !
    INTEGER :: iloc  (ibmax*kmax*4)
    INTEGER :: jloc  (ibmax*kmax)
    INTEGER :: kloc  (ibmax*kmax)
    INTEGER :: i
    INTEGER :: j
    INTEGER :: jp1
    INTEGER :: jm2
    INTEGER :: k
    INTEGER :: klats
    INTEGER :: jb
    INTEGER :: ibdim
    INTEGER :: ip(ibmax*kmax*12)
    INTEGER :: j1(ibmax*kmax),j2(ibmax*kmax),j3(ibmax*kmax),j4(ibmax*kmax)
    REAL(KIND=r8)    :: s1(ibmax*kmax),s2(ibmax*kmax),s3(ibmax*kmax),s4(ibmax*kmax)
    REAL(KIND=r8)    :: ulat  (ibmax*kmax)
    REAL(KIND=r8)    :: ulon  (ibmax*kmax)
    REAL(KIND=r8)    :: usig  (ibmax*kmax)
    REAL(KIND=r8)    :: ulondp(ibmax*kmax*4)
    REAL(KIND=r8)    :: ulatdp(ibmax*kmax)
    REAL(KIND=r8)    :: usigdp(ibmax*kmax)
    REAL(KIND=r8)    :: fint  (ibmax*kmax)
    REAL(KIND=r8)    :: aux
    REAL(KIND=r8)    :: dt2
    REAL(KIND=r8)    :: delta
    REAL(KIND=r8)    :: delta1
    !
    ! Compute normalized winds for trajectory computations
    ! ----------------------------------------------------
    !$OMP SINGLE
    jGlob = 0
    !$OMP END SINGLE
    DO
       !$OMP CRITICAL(slcrit1)
       jGlob = jGlob + 1
       j = jGlob
       !$OMP END CRITICAL(slcrit1)
       IF (j > jbMax) EXIT

       DO i = 1, ibmaxperJB(j)
          aux = coslat(i,j) * er
          fgu (i,:,j)  =  fgu(i,:,j) / aux
          fgv (i,:,j)  =  fgv(i,:,j) / aux
       ENDDO
    !
    ! Compute average wind on all levels
    ! ----------------------------------
       fgumean(:,j) = 0.0_r8
       fgvmean(:,j) = 0.0_r8
       DO k = 1, kmax
          fgumean(:,j) = fgumean(:,j) + fgu(:,k,j) * del(k)
          fgvmean(:,j) = fgvmean(:,j) + fgv(:,k,j) * del(k)
       ENDDO
    ENDDO
    !
    ! Exchange Winds for overlapping areas
    ! ------------------------------------
    IF (maxnodes.gt.1) THEN
       !$OMP BARRIER
       !$OMP SINGLE
       CALL Exchange_Winds (fgu,fgv,fgw,fgumean,fgvmean,jbminus,jbplus,&
                            jov,requestsm,requestrm)
       !$OMP END SINGLE
    ENDIF
    !
    ! Prepare tendencies for interpolation on departure point
    ! -------------------------------------------------------
    delta = delt
    delta1 = delt
    if (nlnminit) then
       delta = 0.5_r8
       delta1 = 1.0_r8
    endif
    dt2 = delta1 + delta1
    !$OMP BARRIER
    !$OMP SINGLE
    jGlob = 0
    !$OMP END SINGLE
    DO
       !$OMP CRITICAL(slcrit2)
       jGlob = jGlob + 1
       j = jGlob
       !$OMP END CRITICAL(slcrit2)
       IF (j > jbMax) EXIT
 
       fgyum(:,:,j) = fgum(:,:,j) + dt2 * fgyum(:,:,j) + delta * fgyu(:,:,j)
       fgyvm(:,:,j) = fgvm(:,:,j) + dt2 * fgyvm(:,:,j) + delta * fgyv(:,:,j)
       fgtdm(:,:,j) = fgtmpm(:,:,j) + dt2 * fgtdm(:,:,j) + delta * fgtd(:,:,j)
       fgqdm(:,:,j) = fgqm(:,:,j) + delta * fgqd(:,:,j)
       fgvdlnpm(:,j) = fglnpm(:,j) + fgzs(:,j) / (tbar*gasr) & 
            + dt2 * fgvdlnpm(:,j) + delta * fgvdlnp(:,j)
    ENDDO
    !
    ! Exchange fields for interpolation (overlapping area) 
    ! ----------------------------------------------------
    !$OMP BARRIER
    IF (maxnodes.gt.1) THEN
       !$OMP SINGLE
       CALL Complete_Exchange(requestsm,requestrm,10*jov)
       CALL Exchange_Fields(fgyum,fgyvm,fgtdm,fgqdm,fgvdlnpm,fgpass_scalars, &
                            nscalars,iold,jbminus,jbplus,jovlap,requests,requestr)
       CALL Complete_Exchange(requests,requestr,(10+2*nscalars)*jovlap)
       !$OMP END SINGLE
    ENDIF
    !
    ! Integration on all blocks and levels
    ! ------------------------------------
    !$OMP SINGLE
    jGlob = 0
    !$OMP END SINGLE
    DO
       !$OMP CRITICAL(slcrit3)
       jGlob = jGlob + 1
       jb = jGlob
       !$OMP END CRITICAL(slcrit3)
       IF (jb > jbMax) EXIT
 
       ibdim = ibmaxperJB(jb)
       !
       ! compute departure points 
       ! ------------------------
       CALL Trajectory(ulonm(1,jb),ulatm(1,jb),usigm(1,jb),&
            ulon  , ulat  , usig   , &
            ulondp, ulatdp, usigdp , &
            iloc  , jloc  , kloc   , &
            ibdim , jb    ,          &
            nit   , delta1 , ip, j1, j2)           
       !
       ! Locate trajectory points for interpolation
       ! ------------------------------------------
       klats = 1 
       IF (reducedGrid) klats = 4
       CALL Locate(.TRUE. ,.TRUE.  , &
            ulon  , ulat  , usig   , &
            ulondp, ulatdp, usigdp , &
            iloc  , jloc  , kloc   , &
            ibdim , klats)
       IF (maxnodes.gt.1) THEN
          jp1 = jmax - 1 - MAXVAL(jloc(1:ibdim*kmax))
          jm2 = jmax + 2 - MINVAL(jloc(1:ibdim*kmax))
          IF (jp1.lt.myfirstlat-jovlap.or.jm2.gt.mylastlat+jovlap) THEN
             STOP " jovlap not sufficient in Semi-Lagrangian "
          ENDIF
       ENDIF
       !
       ! Interpolate and finish tendencies
       ! ---------------------------------
       CALL Interpcublin3d(fint  , fgyum , .FALSE. , &
            ulondp, ulatdp, usigdp, &
            iloc  , jloc  , kloc  , &
            ibdim , -1.0_r8  , .TRUE., &
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
            ibdim , -1.0_r8  ,.FALSE., &
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
            ibdim ,  1.0_r8  ,.FALSE., &
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
            ibdim ,  1.0_r8  ,.FALSE., &
            ip , j1, j2, j3, j4   , &
            s1 , s2, s3, s4 )
       DO k=1,kmax
          DO i=1,ibdim
             fgtd(i,k,jb) = fint((k-1)*ibdim+i)  + delta * fgtd(i,k,jb)
          ENDDO
       ENDDO
       !
       ! advection of passive scalars
       ! ----------------------------
       DO k=1,nscalars
          CALL Interpcublin3dlim(fgpass_scalars(1,1,jb,k,inew), &
               fgpass_scalars(1,1,jbminus,k,iold), .TRUE. , &
               ulondp, ulatdp, usigdp, &
               iloc  , jloc  , kloc  , &
               ibdim ,  1.0_r8  ,.FALSE.,  &
               ip , j1, j2, j3, j4   , &
               s1 , s2, s3, s4 )
       ENDDO
       !
       ! compute departure points (2d-trajectory) 
       ! ----------------------------------------
       CALL Trajectory2D(ulonm2D(1,jb),ulatm2D(1,jb),&
            ulon  , ulat  ,          &
            ulondp, ulatdp,          &
            iloc  , jloc  ,          &
            ibdim , jb    , nit    , delta1)
       !
       ! Locate trajectory points for interpolation
       ! ------------------------------------------
       CALL Locate2D( &
            ulon  , ulat  , &
            ulondp, ulatdp, &
            iloc  , jloc  , &
            ibdim , klats)
       IF (maxnodes.gt.1) THEN
          jp1 = jmax - 1 - MAXVAL(jloc(1:ibdim))
          jm2 = jmax + 2 - MINVAL(jloc(1:ibdim))
          IF (jp1.lt.myfirstlat-jovlap.or.jm2.gt.mylastlat+jovlap) THEN
             STOP " jovlap not sufficient in Semi-Lagrangian "
          ENDIF
       ENDIF
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
    !$OMP BARRIER
    CALL Vectorialtend()
    !
    !  finalize tendencies
    !  -------------------
    !$OMP BARRIER
    !$OMP SINGLE
    jGlob = 0
    !$OMP END SINGLE
    DO
       !$OMP CRITICAL(slcrit4)
       jGlob = jGlob + 1
       j = jGlob
       !$OMP END CRITICAL(slcrit4)
       IF (j > jbMax) EXIT
 
       fgyu(:,:,j) = fgyum(:,:,j) + delta * fgyu(:,:,j)
       fgyv(:,:,j) = fgyvm(:,:,j) + delta * fgyv(:,:,j)
    !
    ! Restore winds
    ! -------------
       DO i = 1, ibmaxperJB(j)
          aux = coslat(i,j) * er
          fgu (i,:,j)  =  fgu(i,:,j) * aux
          fgv (i,:,j)  =  fgv(i,:,j) * aux
       ENDDO
    !
       IF (nlnminit) THEN
          fgyu(:,:,j) = fgyu(:,:,j) - fgum(:,:,j)
          fgyv(:,:,j) = fgyv(:,:,j) - fgvm(:,:,j)
          fgtd(:,:,j) = fgtd(:,:,j) - fgtmpm(:,:,j)
          fgqd(:,:,j) = fgqd(:,:,j) - fgqm(:,:,j)
          fgvdlnp(:,j) = fgvdlnp(:,j) - fglnpm(:,j) - fgzs(:,j) / (tbar*gasr)
       ENDIF
    ENDDO
    END SUBROUTINE SemiLagr
  SUBROUTINE Trajectory(        &
       ulonm , ulatm , usigm  , &
       ulon  , ulat  , usig   , &
       ulondp, ulatdp, usigdp , &
       iloc  , jloc  , kloc   , &
       ibdim , jb    ,          &
       nit   , delt , ip, j1, j2)
    !
    ! Trajectory: Determination of the departure point of Lagrangian trajectory
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: jb  
    INTEGER, INTENT(IN   ) :: nit 
    INTEGER, INTENT(INOUT) :: iloc  (ibdim,kmax,4)
    INTEGER, INTENT(INOUT) :: jloc  (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: kloc  (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: j1    (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: j2    (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: ip    (ibdim,kmax,4)
    REAL(KIND=r8)   , INTENT(IN   ) :: delt
    REAL(KIND=r8)   , INTENT(INOUT) :: ulon  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulat  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: usig  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulonm (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulatm (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: usigm (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulondp(ibdim,kmax,4)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulatdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: usigdp(ibdim,kmax)
    !
    !  local variables 
    !
    INTEGER                        :: kit
    INTEGER                        :: klats
    INTEGER                        :: jp1
    INTEGER                        :: jm2
    LOGICAL                        :: final
    LOGICAL                        :: lsig
    REAL(KIND=r8)                           :: uint(ibmax*kmax)
    REAL(KIND=r8)                           :: vint(ibmax*kmax)
    REAL(KIND=r8)                           :: wint(ibmax*kmax)
    !
    ! Iterate to find departure point
    ! -------------------------------
    !
    klats = 1
    IF (reducedGrid) klats = 2
    DO kit=1,nit
       final = kit.eq.nit
       lsig = kit.eq.1
       !
       ! compute locations and indices for interpolation of the wind
       ! -----------------------------------------------------------
       !
       CALL Locate( lsig  ,.TRUE.  , &
            ulonm , ulatm , usigm  , &
            ulondp, ulatdp, usigdp , &
            iloc  , jloc  , kloc   , &
            ibdim , klats)
       IF (maxnodes.gt.1) THEN
          jp1 = jmax - MAXVAL(jloc)
          jm2 = jmax + 1 - MINVAL(jloc)
          IF (jp1.lt.myfirstlat-jov.or.jm2.gt.mylastlat+jov) THEN
             STOP " jovlap not sufficient in Semi-Lagrangian "
          ENDIF
       ENDIF
       !
       ! interpolate vertical wind 
       ! -------------------------
       !
       CALL InterplG(&
            wint  , fgw            , &
            ulondp, ulatdp, usigdp , &
            iloc  , jloc  , kloc   , &
            ibdim , ip , j1 , j2 )
       !
       ! update sigma level of mid-point of trajectory
       ! ---------------------------------------------
       !
       CALL Upsig (usig, usigm, wint, delt, ibdim, final)
       !
       ! computes new sigma location for interpolation
       ! ---------------------------------------------
       !
       CALL Locate(.TRUE. ,.FALSE. , &
            ulonm , ulatm , usigm  , &
            ulondp, ulatdp, usigdp , &
            iloc  , jloc  , kloc   , &
            ibdim , klats)
       !
       ! interpolate horizontal wind field
       ! ---------------------------------
       !
       CALL InterpluvG(&
            uint  , vint           , &
            ulondp, ulatdp, usigdp , &
            jloc  , kloc  , ibdim  , &
            ip    , j1    ,   j2 )
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
       nit   , delt  )
    !
    ! Trajectory2D: Determination of the departure point of Lagrangian trajectory
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: jb  
    INTEGER, INTENT(IN   ) :: nit 
    INTEGER, INTENT(INOUT) :: iloc  (ibdim,4)
    INTEGER, INTENT(INOUT) :: jloc  (ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: delt
    REAL(KIND=r8)   , INTENT(INOUT) :: ulon  (ibdim)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulat  (ibdim)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulonm (ibdim)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulatm (ibdim)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulondp(ibdim,4)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulatdp(ibdim)
    !
    !  local variables 
    !
    INTEGER                        :: kit
    INTEGER                        :: jp1
    INTEGER                        :: jm2
    INTEGER                        :: klats
    LOGICAL                        :: final
    REAL(KIND=r8)                           :: uint(ibmax)
    REAL(KIND=r8)                           :: vint(ibmax)
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
            ibdim , klats)
       IF (maxnodes.gt.1) THEN
          jp1 = jmax - MAXVAL(jloc(1:ibdim))
          jm2 = jmax + 1 - MINVAL(jloc(1:ibdim))
          IF (jp1.lt.myfirstlat-jov.or.jm2.gt.mylastlat+jov) THEN
             STOP " jovlap not sufficient in Semi-Lagrangian "
          ENDIF
       ENDIF
       !
       ! interpolate horizontal wind field
       ! ---------------------------------
       !
       CALL InterpluvG2D(&
            uint  , vint  , &
            ulondp, ulatdp, &
            iloc  , jloc  , &
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
    REAL(KIND=r8)   , INTENT(IN   ) :: w     (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(OUT  ) :: usig  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(OUT  ) :: usigm (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: delt
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
    REAL(KIND=r8)   , INTENT(IN   ) :: coslon(ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: sinlon(ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: coslat(ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: sinlat(ibdim)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulat  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulon  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulatm (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulonm (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: u     (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: v     (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: delt
    !
    !  local variables 
    !
    INTEGER :: i
    INTEGER :: k
    REAL(KIND=r8)    :: dt2     ,&
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
    twodt = 2.0_r8 * delt
    dpi = 2.0_r8 * pai
    DO i=1,ibdim
       x(i) = coslon(i) * coslat(i)
       y(i) = sinlon(i) * coslat(i)
       z(i) = sinlat(i)
    ENDDO
    IF (final) THEN
       DO k=1,kmax
          DO i=1,ibdim
             coslba = COS(ulonm(i,k))
             sinlba = SIN(ulonm(i,k))
             cosphi = COS(ulatm(i,k))
             sinphi = SIN(ulatm(i,k))
             xdot = - ( u(i,k) * sinlba + v(i,k) * coslba * sinphi )
             ydot =  u(i,k) * coslba - v(i,k) * sinlba * sinphi
             zdot = cosphi * v(i,k)
             r = 1.0_r8 + dt2 * (xdot*xdot+ydot*ydot+zdot*zdot) - &
                 twodt * (xdot*x(i)+ydot*y(i)+zdot*z(i))
             b = 1.0_r8 / SQRT(r)
             xb = b * (x(i)-delt*xdot) 
             yb = b * (y(i)-delt*ydot) 
             zb = b * (z(i)-delt*zdot) 
             ulonm(i,k)=ATAN2(yb,xb)
             IF (ulonm(i,k).lt.0.0_r8) ulonm(i,k) = ulonm(i,k) + dpi
             ulatm(i,k)=ASIN(zb)
             dot2 = 2.0_r8 * (x(i)*xb + y(i)*yb + z(i)*zb)
             xm = dot2 * xb - x(i)
             ym = dot2 * yb - y(i)
             zm = dot2 * zb - z(i)
             ulon(i,k)=ATAN2(ym,xm)
             IF (ulon(i,k).lt.0.0_r8) ulon(i,k) = ulon(i,k) + dpi
             ulat(i,k)=ASIN(zm)
          ENDDO
       ENDDO
    ELSE
       DO k=1,kmax
          DO i=1,ibdim
             coslba = COS(ulonm(i,k))
             sinlba = SIN(ulonm(i,k))
             cosphi = COS(ulatm(i,k))
             sinphi = SIN(ulatm(i,k))
             xdot = - ( u(i,k) * sinlba + v(i,k) * coslba * sinphi )
             ydot =  u(i,k) * coslba - v(i,k) * sinlba * sinphi
             zdot = cosphi * v(i,k)
             r = 1.0_r8 + dt2 * (xdot*xdot+ydot*ydot+zdot*zdot) - &
                 twodt * (xdot*x(i)+ydot*y(i)+zdot*z(i))
             b = 1.0_r8 / SQRT(r)
             xb = b * (x(i)-delt*xdot) 
             yb = b * (y(i)-delt*ydot) 
             zb = b * (z(i)-delt*zdot) 
             ulonm(i,k)=ATAN2(yb,xb)
             IF (ulonm(i,k).lt.0.0_r8) ulonm(i,k) = ulonm(i,k) + dpi
             ulatm(i,k)=ASIN(zb)
          ENDDO
       ENDDO
    ENDIF
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
    REAL(KIND=r8)   , INTENT(IN   ) :: coslon(ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: sinlon(ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: coslat(ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: sinlat(ibdim)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulat  (ibdim)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulon  (ibdim)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulatm (ibdim)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulonm (ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: u     (ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: v     (ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: delt
    !
    !  local variables 
    !
    INTEGER :: i
    REAL(KIND=r8)    :: dt2    ,&
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
    twodt = 2.0_r8 * delt
    dpi = 2.0_r8 * pai
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
       r = 1.0_r8 + dt2 * (xdot*xdot+ydot*ydot+zdot*zdot) - &
            twodt * (xdot*x+ydot*y+zdot*z)
       b = 1.0_r8 / SQRT(r)
       xb = b * (x-delt*xdot) 
       yb = b * (y-delt*ydot) 
       zb = b * (z-delt*zdot) 
       ulonm(i)=ATAN2(yb,xb)
       IF (ulonm(i).lt.0.0_r8) ulonm(i) = ulonm(i) + dpi
       ulatm(i)=ASIN(zb)
       IF (final) THEN
          dot2 = 2.0_r8 * (x*xb + y*yb + z*zb)
          xm = dot2 * xb - x
          ym = dot2 * yb - y
          zm = dot2 * zb - z
          ulon(i)=ATAN2(ym,xm)
          IF (ulon(i).lt.0.0_r8) ulon(i) = ulon(i) + dpi
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
       ibdim , klats)
    !
    ! locate : localize a set of points within the grid
    !
    !
    LOGICAL, INTENT(IN   ) :: llatlon
    LOGICAL, INTENT(IN   ) :: lsig
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: klats
    INTEGER, INTENT(INOUT) :: iloc  (ibdim,kmax,klats)
    INTEGER, INTENT(INOUT) :: jloc  (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: kloc  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulon  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulat  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: usig  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulondp(ibdim,kmax,klats)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulatdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: usigdp(ibdim,kmax)
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
    IF (lsig) THEN
       DO k=1,kmax
          DO i=1,ibdim
             ik = (usig(i,k)-sigma(i,1)) / delsigref
             kloc(i,k) = ksigref(i,ik)
             IF (usig(i,k).gt.sigma(i,kloc(i,k)+1)) kloc(i,k)=kloc(i,k)+1
             usigdp(i,k) = (usig(i,k)-sigma(i,kloc(i,k)))/delsigma(i,kloc(i,k))
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
             jloc(i,k) = jphiref(i,ik)
             IF (ulat(i,k).gt.phi(i,jloc(i,k)+1)) jloc(i,k)=jloc(i,k)+1
             ulatdp(i,k) = (ulat(i,k)-phi(i,jloc(i,k)))/delphi(i,jloc(i,k))
          ENDDO
       ENDDO
  
       DO kl=1,klats
          ks = kp(kl)
          DO k=1,kmax
             DO i=1,ibdim
                j = jmax + 1 - jloc(i,k)
                ulondp(i,k,kl) = ulon(i,k) / dellon(i,j-ks) + 1
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
       ibdim , klats)
    !
    ! locate : localize a set of points within the grid
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: klats
    INTEGER, INTENT(INOUT) :: iloc  (ibdim,klats)
    INTEGER, INTENT(INOUT) :: jloc  (ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulon  (ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulat  (ibdim)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulondp(ibdim,klats)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulatdp(ibdim)
    !
    !  local variables 
    !
    INTEGER                        :: i
    INTEGER                        :: ik
    INTEGER                        :: ks
    INTEGER                        :: j
    INTEGER                        :: kl
    INTEGER                        :: kp(4)
    !
    ! compute relative locations and indices for interpolation
    ! --------------------------------------------------------
    !
    IF (klats.eq.4) THEN
       kp(1)=-1
       kp(2)=0
       kp(3)=1
       kp(4)=2
    ELSE
       kp(1)=0
       kp(2)=1
    ENDIF

    DO i=1,ibdim
       ik = (ulat(i)+pihalf) / delphiref
       jloc(i) = jphiref(i,ik)
       IF (ulat(i).gt.phi(i,jloc(i)+1)) jloc(i)=jloc(i)+1
       ulatdp(i) = (ulat(i)-phi(i,jloc(i)))/delphi(i,jloc(i))
    ENDDO

    DO kl=1,klats
       ks = kp(kl)
       DO i=1,ibdim
          j = jmax + 1 - jloc(i)
          ulondp(i,kl) = ulon(i) / dellon(i,j-ks) + 1
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
       jloc  , kloc  , ibdim  , &
       ip    , j1    , j2 )
    !
    ! interpluvG : linear interpolation of horizontal components of wind
    !              ( on Gaussian grid )
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: jloc  (ibdim,kmax)
    INTEGER, INTENT(IN   ) :: kloc  (ibdim,kmax)
    INTEGER, INTENT(IN   ) :: j1    (ibdim,kmax)
    INTEGER, INTENT(IN   ) :: j2    (ibdim,kmax)
    INTEGER, INTENT(IN   ) :: ip    (ibdim,kmax,4)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulondp(ibdim,kmax,2)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulatdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: usigdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(OUT  ) :: uint  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(OUT  ) :: vint  (ibdim,kmax)
    !
    !  local variables 
    !
    INTEGER  :: i
    INTEGER  :: k
    REAL(KIND=r8)     :: f1
    REAL(KIND=r8)     :: f2
    REAL(KIND=r8)     :: f3
    REAL(KIND=r8)     :: f4
    REAL(KIND=r8)     :: g1
    REAL(KIND=r8)     :: g2
    REAL(KIND=r8)     :: g3
    REAL(KIND=r8)     :: g4
    REAL(KIND=r8)     :: s1
    REAL(KIND=r8)     :: s2
    !
    !
    ! perform the interpolation of u and v
    ! ------------------------------------
    !
    DO k=1,kmax
       DO i=1,ibdim
          IF (jloc(i,k).eq.0) THEN
             s1 = -1.0_r8
           ELSE
             s1 = 1.0_r8
          ENDIF
          IF (jloc(i,k).eq.jmax) THEN
             s2 = -1.0_r8
           ELSE
             s2 = 1.0_r8
          ENDIF
          f1 = fgu(ip(i,k,1),kloc(i,k),j1(i,k)) + ulondp(i,k,1) * &
             ( fgu(ip(i,k,3),kloc(i,k),j1(i,k)) - fgu(ip(i,k,1),kloc(i,k),j1(i,k)) )
          g1 = fgv(ip(i,k,1),kloc(i,k),j1(i,k)) + ulondp(i,k,1) * &
             ( fgv(ip(i,k,3),kloc(i,k),j1(i,k)) - fgv(ip(i,k,1),kloc(i,k),j1(i,k)) )
          f2 = fgu(ip(i,k,2),kloc(i,k),j2(i,k)) + ulondp(i,k,2) * &
             ( fgu(ip(i,k,4),kloc(i,k),j2(i,k)) - fgu(ip(i,k,2),kloc(i,k),j2(i,k)) )
          g2 = fgv(ip(i,k,2),kloc(i,k),j2(i,k)) + ulondp(i,k,2) * &
             ( fgv(ip(i,k,4),kloc(i,k),j2(i,k)) - fgv(ip(i,k,2),kloc(i,k),j2(i,k)) )
          f3 = fgu(ip(i,k,1),kloc(i,k)+1,j1(i,k)) + ulondp(i,k,1) * &
           ( fgu(ip(i,k,3),kloc(i,k)+1,j1(i,k)) - fgu(ip(i,k,1),kloc(i,k)+1,j1(i,k)) )
          g3 = fgv(ip(i,k,1),kloc(i,k)+1,j1(i,k)) + ulondp(i,k,1) * &
           ( fgv(ip(i,k,3),kloc(i,k)+1,j1(i,k)) - fgv(ip(i,k,1),kloc(i,k)+1,j1(i,k)) )
          f4 = fgu(ip(i,k,2),kloc(i,k)+1,j2(i,k)) + ulondp(i,k,2) * &
           ( fgu(ip(i,k,4),kloc(i,k)+1,j2(i,k)) - fgu(ip(i,k,2),kloc(i,k)+1,j2(i,k)) )
          g4 = fgv(ip(i,k,2),kloc(i,k)+1,j2(i,k)) + ulondp(i,k,2) * &
           ( fgv(ip(i,k,4),kloc(i,k)+1,j2(i,k)) - fgv(ip(i,k,2),kloc(i,k)+1,j2(i,k)) )
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
       ibdim)
    !
    ! interpluvG2D : linear interpolation of horizontal components of wind
    !              ( on Gaussian grid )
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: iloc  (ibdim,2)
    INTEGER, INTENT(IN   ) :: jloc  (ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulondp(ibdim,2)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulatdp(ibdim)
    REAL(KIND=r8)   , INTENT(OUT  ) :: uint  (ibdim)
    REAL(KIND=r8)   , INTENT(OUT  ) :: vint  (ibdim)
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
    REAL(KIND=r8)     :: f1
    REAL(KIND=r8)     :: f2
    REAL(KIND=r8)     :: g1
    REAL(KIND=r8)     :: g2
    REAL(KIND=r8)     :: s1(ibmax)
    REAL(KIND=r8)     :: s2(ibmax)
    !
    ! define indices for interpolation
    ! --------------------------------
    !
    DO i=1,ibdim
       jp = jmax+1-jloc(i) ! This is due to the reversed order of latitudes
       s1(i) = 1.0_r8          ! If across the pole a sign change 
       s2(i) = 1.0_r8          ! will be necessary
       if (jp.eq.jmax+1) s1(i) = -1.0_r8
       if (jp.eq.1) s2(i) = -1.0_r8
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
       f1 = s1(i) * ( fgumean(i1,j1) + ulondp(i,1) * &
            ( fgumean(i3,j1) - fgumean(i1,j1) ) )
       g1 = s1(i) * ( fgvmean(i1,j1) + ulondp(i,1) * &
            ( fgvmean(i3,j1) - fgvmean(i1,j1) ) )
       f2 = s2(i) * ( fgumean(i2,j2) + ulondp(i,2) * &
            ( fgumean(i4,j2) - fgumean(i2,j2) ) )
       g2 = s2(i) * ( fgvmean(i2,j2) + ulondp(i,2) * &
            ( fgvmean(i4,j2) - fgvmean(i2,j2) ) )
       uint(i) = f1 + ulatdp(i) * (f2-f1)
       vint(i) = g1 + ulatdp(i) * (g2-g1)
    ENDDO
    !
  END SUBROUTINE InterpluvG2D
  SUBROUTINE InterplG(&
       fint  , f              , &
       ulondp, ulatdp, usigdp , &
       iloc  , jloc  , kloc   , &
       ibdim , ip , j1 , j2   )
    !
    ! interplG : linear interpolation of a field f
    !              ( on Gaussian grid )
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: iloc  (ibdim,kmax,2)
    INTEGER, INTENT(IN   ) :: jloc  (ibdim,kmax)
    INTEGER, INTENT(IN   ) :: kloc  (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: j1    (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: j2    (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: ip    (ibdim,kmax,4)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulondp(ibdim,kmax,2)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulatdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: usigdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: f     (ibmax,kmax,jbminus:jbplus)
    REAL(KIND=r8)   , INTENT(OUT  ) :: fint  (ibdim,kmax)
    !
    !  local variables 
    !
    INTEGER  :: i
    INTEGER  :: k
    INTEGER  :: jp 
    REAL(KIND=r8)     :: f1
    REAL(KIND=r8)     :: f2
    REAL(KIND=r8)     :: f3
    REAL(KIND=r8)     :: f4
    !
    !
    ! perform the interpolation of f
    ! ------------------------------
    !
    DO k=1,kmax
       DO i=1,ibdim
          jp = jmax+1-jloc(i,k) ! This is due to the reversed order 
          ip(i,k,1) = ibPerIJ(iloc(i,k,1),jp)
          ip(i,k,2) = ibPerIJ(iloc(i,k,2),jp-1)
          ip(i,k,3) = ibPerIJ(iloc(i,k,1)+1,jp)
          ip(i,k,4) = ibPerIJ(iloc(i,k,2)+1,jp-1)
          j1(i,k) = jbPerIJ(iloc(i,k,1),jp)
          j2(i,k) = jbPerIJ(iloc(i,k,2),jp-1)
       ENDDO
    ENDDO
    DO k=1,kmax
       DO i=1,ibdim
          f1 = f(ip(i,k,1),kloc(i,k),j1(i,k)) + ulondp(i,k,1) * &
                ( f(ip(i,k,3),kloc(i,k),j1(i,k)) - f(ip(i,k,1),kloc(i,k),j1(i,k)) )
          f2 = f(ip(i,k,2),kloc(i,k),j2(i,k)) + ulondp(i,k,2) * &
                ( f(ip(i,k,4),kloc(i,k),j2(i,k)) - f(ip(i,k,2),kloc(i,k),j2(i,k)) )
          f3 = f(ip(i,k,1),kloc(i,k)+1,j1(i,k)) + ulondp(i,k,1) * &
              ( f(ip(i,k,3),kloc(i,k)+1,j1(i,k)) - f(ip(i,k,1),kloc(i,k)+1,j1(i,k)) )
          f4 = f(ip(i,k,2),kloc(i,k)+1,j2(i,k)) + ulondp(i,k,2) * &
              ( f(ip(i,k,4),kloc(i,k)+1,j2(i,k)) - f(ip(i,k,2),kloc(i,k)+1,j2(i,k)) )
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
    REAL(KIND=r8)   , INTENT(IN   ) :: ulondp(ibdim,4)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulatdp(ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: f     (ibmax,jbminus:jbplus)
    REAL(KIND=r8)   , INTENT(OUT  ) :: fint  (ibdim)
    !
    !  local variables 
    !
    INTEGER  :: i
    INTEGER  :: j
    INTEGER  :: jp 
    INTEGER  :: ip(ibmax,12)
    INTEGER  :: j1(ibmax),j2(ibmax),j3(ibmax),j4(ibmax)
    REAL(KIND=r8)     :: f1(ibmax)
    REAL(KIND=r8)     :: f2(ibmax)
    REAL(KIND=r8)     :: f3(ibmax)
    REAL(KIND=r8)     :: f4(ibmax)
    REAL(KIND=r8)     :: fa,fb,fc,fd,s,t,t1,t2,t3,t4
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
       s = ulondp(i,2)-0.5_r8
       fa = 0.5_r8 * ( f(ip(i,5),j2(i)) + f(ip(i,4),j2(i)) ) + &
            s * ( f(ip(i,5),j2(i)) - f(ip(i,4),j2(i)) )
       fb = 0.5_r8 * ( f(ip(i,6),j2(i)) + f(ip(i,3),j2(i)) ) + &
            s * ( f(ip(i,6),j2(i)) - f(ip(i,3),j2(i)) ) / 3.0_r8
       f2(i) = fa + (0.125_r8 - 0.5_r8*s*s) * (fa-fb) 
       t = ulondp(i,3)-0.5_r8
       fc = 0.5_r8 * ( f(ip(i,9),j3(i)) + f(ip(i,8),j3(i)) ) + &
            t * ( f(ip(i,9),j3(i)) - f(ip(i,8),j3(i)) )
       fd = 0.5_r8 * ( f(ip(i,10),j3(i)) + f(ip(i,7),j3(i)) ) + &
            t * ( f(ip(i,10),j3(i)) - f(ip(i,7),j3(i)) ) / 3.0_r8
       f3(i) = fc + (0.125_r8 - 0.5_r8*t*t) * (fc-fd) 
       f4(i) = f(ip(i,11),j4(i)) + ulondp(i,4) * &
            ( f(ip(i,12),j4(i)) - f(ip(i,11),j4(i)) )
    ENDDO
    DO i=1,ibdim
       j = jloc(i)
       s = ulatdp(i)
       t1 = 1.0_r8 - s
       t2 = cubcoef(i,j,2) + s
       t3 = cubcoef(i,j,1) + t1
       t4 = s * t2
       fint(i) = t3 * ( t1 * ( cubcoef(i,j,3) * s * f1(i) + &
            cubcoef(i,j,4) * t2 * f2(i) ) + &
            cubcoef(i,j,5) * t4 * f3(i) ) + &
            cubcoef(i,j,6) * t4 * t1 * f4(i)
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
    REAL(KIND=r8)   , INTENT(INOUT) :: s1    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: s2    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: s3    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: s4    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: signal
    REAL(KIND=r8)   , INTENT(IN   ) :: ulondp(ibdim,kmax,4)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulatdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: usigdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: f     (ibmax,kmax,jbminus:jbplus)
    REAL(KIND=r8)   , INTENT(OUT  ) :: fint  (ibdim,kmax)
    !
    !  local variables 
    !
    INTEGER  :: i
    INTEGER  :: j
    INTEGER  :: k
    INTEGER  :: km
    INTEGER  :: k1
    INTEGER  :: jp 
    REAL(KIND=r8)     :: f1(ibmax)
    REAL(KIND=r8)     :: f2(ibmax)
    REAL(KIND=r8)     :: f3(ibmax)
    REAL(KIND=r8)     :: f4(ibmax)
    REAL(KIND=r8)     :: g1(ibmax)
    REAL(KIND=r8)     :: g2(ibmax)
    REAL(KIND=r8)     :: g3(ibmax)
    REAL(KIND=r8)     :: g4(ibmax)
    REAL(KIND=r8)     :: fint1(ibmax)
    REAL(KIND=r8)     :: fint2(ibmax)
    REAL(KIND=r8)     :: fint3(ibmax)
    REAL(KIND=r8)     :: fint4(ibmax)
    REAL(KIND=r8)     :: fa,fb,fc,fd,s,t,t0,t1,t2,t3,t4
    REAL(KIND=r8)     :: fa1,fb1,fc1,fd1,ss,tt
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
       s1 = 1.0_r8
       s2 = 1.0_r8
       s3 = 1.0_r8
       s4 = 1.0_r8
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
          k1 = k+1
          f1(i) = s1(i,km) * ( f(ip(i,km,1),k,j1(i,km)) + ulondp(i,km,1) * &
                ( f(ip(i,km,2),k,j1(i,km)) - f(ip(i,km,1),k,j1(i,km)) ) )
          g1(i) = s1(i,km) * ( f(ip(i,km,1),k1,j1(i,km)) + ulondp(i,km,1) * &
                ( f(ip(i,km,2),k1,j1(i,km)) - f(ip(i,km,1),k1,j1(i,km)) ) )
          f4(i) = s4(i,km) * ( f(ip(i,km,11),k,j4(i,km)) + ulondp(i,km,4) * &
                ( f(ip(i,km,12),k,j4(i,km)) - f(ip(i,km,11),k,j4(i,km)) ) )
          g4(i) = s4(i,km) * ( f(ip(i,km,11),k1,j4(i,km)) + ulondp(i,km,4) * &
                ( f(ip(i,km,12),k1,j4(i,km)) - f(ip(i,km,11),k1,j4(i,km)) ) )
       ENDDO
       DO i=1,ibdim
          k = kloc(i,km)
          k1 = k+1
          s = ulondp(i,km,2)-0.5_r8
          ss = 0.125_r8 - 0.5_r8*s*s
          fa = 0.5_r8 * ( f(ip(i,km,5),k,j2(i,km)) + f(ip(i,km,4),k,j2(i,km)) ) + &
                 s * ( f(ip(i,km,5),k,j2(i,km)) - f(ip(i,km,4),k,j2(i,km)) )
          fb = 0.5_r8 * ( f(ip(i,km,6),k,j2(i,km)) + f(ip(i,km,3),k,j2(i,km)) ) + &
                 s * ( f(ip(i,km,6),k,j2(i,km)) - f(ip(i,km,3),k,j2(i,km)) ) /3.0_r8
          f2(i) = s2(i,km) * ( fa + ss * (fa-fb) ) 
          fa1 = 0.5_r8 * ( f(ip(i,km,5),k1,j2(i,km)) + f(ip(i,km,4),k1,j2(i,km)) ) + &
                 s * ( f(ip(i,km,5),k1,j2(i,km)) - f(ip(i,km,4),k1,j2(i,km)) )
          fb1 = 0.5_r8 * ( f(ip(i,km,6),k1,j2(i,km)) + f(ip(i,km,3),k1,j2(i,km)) ) + &
                 s * ( f(ip(i,km,6),k1,j2(i,km)) - f(ip(i,km,3),k1,j2(i,km)) ) / 3.0_r8
          g2(i) = s2(i,km) * ( fa1 + ss * (fa1-fb1) ) 
       ENDDO
       DO i=1,ibdim
          k = kloc(i,km)
          k1 = k+1
          t = ulondp(i,km,3)-0.5_r8
          tt = 0.125_r8 - 0.5_r8*t*t
          fc = 0.5_r8 * ( f(ip(i,km,9),k,j3(i,km)) + f(ip(i,km,8),k,j3(i,km)) ) + &
                 t * ( f(ip(i,km,9),k,j3(i,km)) - f(ip(i,km,8),k,j3(i,km)) )
          fd = 0.5_r8 * ( f(ip(i,km,10),k,j3(i,km)) + f(ip(i,km,7),k,j3(i,km)) ) + &
                 t * ( f(ip(i,km,10),k,j3(i,km)) - f(ip(i,km,7),k,j3(i,km)) ) / 3.0_r8
          f3(i) = s3(i,km) * ( fc + tt * (fc-fd) )
          fc1 = 0.5_r8 * ( f(ip(i,km,9),k1,j3(i,km)) + f(ip(i,km,8),k1,j3(i,km)) ) + &
                 t * ( f(ip(i,km,9),k1,j3(i,km)) - f(ip(i,km,8),k1,j3(i,km)) )
          fd1 = 0.5_r8 * ( f(ip(i,km,10),k1,j3(i,km)) + f(ip(i,km,7),k1,j3(i,km)) ) + &
                 t * ( f(ip(i,km,10),k1,j3(i,km)) - f(ip(i,km,7),k1,j3(i,km)) ) /3.0_r8
          g3(i) = s3(i,km) * ( fc1 + tt * (fc1-fd1) )
       ENDDO
       DO i=1,ibdim
          j = jloc(i,km)
          t0 = ulatdp(i,km)
          t1 = 1.0_r8 - t0
          t2 = cubcoef(i,j,2) + t0
          t3 = cubcoef(i,j,1) + t1
          t4 = t0 * t2
          fint2(i) = t3 * ( t1 * ( cubcoef(i,j,3) * t0 * f1(i) + &
                     cubcoef(i,j,4) * t2 * f2(i) ) + &
                     cubcoef(i,j,5) * t4 * f3(i) ) + &
                     cubcoef(i,j,6) * t4 * t1 * f4(i)
          fint3(i) = t3 * ( t1 * ( cubcoef(i,j,3) * t0 * g1(i) + &
                     cubcoef(i,j,4) * t2 * g2(i) ) + &
                     cubcoef(i,j,5) * t4 * g3(i) ) + &
                     cubcoef(i,j,6) * t4 * t1 * g4(i)
       ENDDO
       DO i=1,ibdim
          IF (kloc(i,km).ne.1.and.kloc(i,km).ne.kmax-1) THEN
             k = kloc(i,km) - 1
             k1 = kloc(i,km) + 2
             fb = s2(i,km) * ( f(ip(i,km,4),k,j2(i,km)) + ulondp(i,km,2) * &
                   ( f(ip(i,km,5),k,j2(i,km)) - f(ip(i,km,4),k,j2(i,km)) ) )
             fc = s3(i,km) * ( f(ip(i,km,8),k,j3(i,km)) + ulondp(i,km,3) * &
                   ( f(ip(i,km,9),k,j3(i,km)) - f(ip(i,km,8),k,j3(i,km)) ) )
             fa = s2(i,km) * ( f(ip(i,km,4),k1,j2(i,km)) + ulondp(i,km,2) * &
                   ( f(ip(i,km,5),k1,j2(i,km)) - f(ip(i,km,4),k1,j2(i,km)) ) )
             fd = s3(i,km) * ( f(ip(i,km,8),k1,j3(i,km)) + ulondp(i,km,3) * &
                   ( f(ip(i,km,9),k1,j3(i,km)) - f(ip(i,km,8),k1,j3(i,km)) ) )
             fint1(i) = fb + ulatdp(i,km) * (fc-fb) 
             fint4(i) = fa + ulatdp(i,km) * (fd-fa) 
             s = usigdp(i,km)
             t1 = 1.0_r8 - s
             t2 = cubcoefv(i,kloc(i,km),2) + s
             t3 = cubcoefv(i,kloc(i,km),1) + t1
             t4 = s * t2
             fint(i,km) = t3 * ( t1 * ( cubcoefv(i,kloc(i,km),3) * s * fint1(i) + &
                          cubcoefv(i,kloc(i,km),4) * t2 * fint2(i) ) + &
                          cubcoefv(i,kloc(i,km),5) * t4 * fint3(i) ) + &
                          cubcoefv(i,kloc(i,km),6) * t4 * t1 * fint4(i)
          ELSE
             fint(i,km) = fint2(i) + usigdp(i,km) * ( fint3(i)-fint2(i) )
          ENDIF
       ENDDO
    ENDDO
    !
  END SUBROUTINE Interpcublin3d
  SUBROUTINE Interpcublin3dlim(&
       fint  , f     , again , &
       ulondp, ulatdp, usigdp, &
       iloc  , jloc  , kloc  , &
       ibdim , signal, first , &
       ip    , j1, j2, j3, j4, &
       s1 , s2, s3, s4 )
    !
    ! Interpcublin3dlim : quasi-cubic interpolation of a field f
    !              ( on Gaussian grid ) with a monotone limiter
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
    REAL(KIND=r8)   , INTENT(INOUT) :: s1    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: s2    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: s3    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: s4    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: signal
    REAL(KIND=r8)   , INTENT(IN   ) :: ulondp(ibdim,kmax,4)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulatdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: usigdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: f     (ibmax,kmax,jbminus:jbplus)
    REAL(KIND=r8)   , INTENT(INOUT) :: fint  (ibmax,kmax)
    !
    !  local variables
    !
    INTEGER  :: i
    INTEGER  :: j
    INTEGER  :: k
    INTEGER  :: km
    INTEGER  :: k1
    INTEGER  :: jp
    REAL(KIND=r8)     :: f1(ibmax)
    REAL(KIND=r8)     :: f2(ibmax)
    REAL(KIND=r8)     :: f3(ibmax)
    REAL(KIND=r8)     :: f4(ibmax)
    REAL(KIND=r8)     :: g1(ibmax)
    REAL(KIND=r8)     :: g2(ibmax)
    REAL(KIND=r8)     :: g3(ibmax)
    REAL(KIND=r8)     :: g4(ibmax)
    REAL(KIND=r8)     :: fint1(ibmax)
    REAL(KIND=r8)     :: fint2(ibmax)
    REAL(KIND=r8)     :: fint3(ibmax)
    REAL(KIND=r8)     :: fint4(ibmax)
    REAL(KIND=r8)     :: fa,fb,fc,fd,s,t,t0,t1,t2,t3,t4
    REAL(KIND=r8)     :: fa1,fb1,fc1,fd1,ss,tt
    REAL(KIND=r8)     :: sma, sma1, smi, smi1
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
       s1 = 1.0_r8
       s2 = 1.0_r8
       s3 = 1.0_r8
       s4 = 1.0_r8
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
          k1 = k+1
          f1(i) = s1(i,km) * ( f(ip(i,km,1),k,j1(i,km)) + ulondp(i,km,1) * &
                ( f(ip(i,km,2),k,j1(i,km)) - f(ip(i,km,1),k,j1(i,km)) ) )
          g1(i) = s1(i,km) * ( f(ip(i,km,1),k1,j1(i,km)) + ulondp(i,km,1) * &
                ( f(ip(i,km,2),k1,j1(i,km)) - f(ip(i,km,1),k1,j1(i,km)) ) )
          f4(i) = s4(i,km) * ( f(ip(i,km,11),k,j4(i,km)) + ulondp(i,km,4) * &
                ( f(ip(i,km,12),k,j4(i,km)) - f(ip(i,km,11),k,j4(i,km)) ) )
          g4(i) = s4(i,km) * ( f(ip(i,km,11),k1,j4(i,km)) + ulondp(i,km,4) * &
                ( f(ip(i,km,12),k1,j4(i,km)) - f(ip(i,km,11),k1,j4(i,km)) ) )
       ENDDO
       DO i=1,ibdim
          k = kloc(i,km)
          k1 = k+1
          s = ulondp(i,km,2)-0.5_r8
          ss = 0.125_r8 - 0.5_r8*s*s
          fa = 0.5_r8 * ( f(ip(i,km,5),k,j2(i,km)) + f(ip(i,km,4),k,j2(i,km)) ) + &
                 s * ( f(ip(i,km,5),k,j2(i,km)) - f(ip(i,km,4),k,j2(i,km)) )
          fb = 0.5_r8 * ( f(ip(i,km,6),k,j2(i,km)) + f(ip(i,km,3),k,j2(i,km)) ) + &
                 s * ( f(ip(i,km,6),k,j2(i,km)) - f(ip(i,km,3),k,j2(i,km)) ) /3.0_r8
          f2(i) = s2(i,km) * ( fa + ss * (fa-fb) )
          fa1 = 0.5_r8 * ( f(ip(i,km,5),k1,j2(i,km)) + f(ip(i,km,4),k1,j2(i,km)) )+ &
                 s * ( f(ip(i,km,5),k1,j2(i,km)) - f(ip(i,km,4),k1,j2(i,km)) )
          fb1 = 0.5_r8 * ( f(ip(i,km,6),k1,j2(i,km)) + f(ip(i,km,3),k1,j2(i,km)) )+ &
                s * ( f(ip(i,km,6),k1,j2(i,km)) - f(ip(i,km,3),k1,j2(i,km)) ) / 3.0_r8
          g2(i) = s2(i,km) * ( fa1 + ss * (fa1-fb1) )
       ENDDO
       DO i=1,ibdim
          k = kloc(i,km)
          k1 = k+1
          t = ulondp(i,km,3)-0.5_r8
          tt = 0.125_r8 - 0.5_r8*t*t
          fc = 0.5_r8 * ( f(ip(i,km,9),k,j3(i,km)) + f(ip(i,km,8),k,j3(i,km)) ) + &
                 t * ( f(ip(i,km,9),k,j3(i,km)) - f(ip(i,km,8),k,j3(i,km)) )
          fd = 0.5_r8 * ( f(ip(i,km,10),k,j3(i,km)) + f(ip(i,km,7),k,j3(i,km)) ) + &
                t * ( f(ip(i,km,10),k,j3(i,km)) - f(ip(i,km,7),k,j3(i,km)) ) / 3.0_r8
          f3(i) = s3(i,km) * ( fc + tt * (fc-fd) )
          fc1 = 0.5_r8 * ( f(ip(i,km,9),k1,j3(i,km)) + f(ip(i,km,8),k1,j3(i,km)) ) +&
                 t * ( f(ip(i,km,9),k1,j3(i,km)) - f(ip(i,km,8),k1,j3(i,km)) )
          fd1 = 0.5_r8 * ( f(ip(i,km,10),k1,j3(i,km)) + f(ip(i,km,7),k1,j3(i,km)) )+&
                t * ( f(ip(i,km,10),k1,j3(i,km)) - f(ip(i,km,7),k1,j3(i,km)) ) /3.0_r8
          g3(i) = s3(i,km) * ( fc1 + tt * (fc1-fd1) )
       ENDDO
       DO i=1,ibdim
          j = jloc(i,km)
          t0 = ulatdp(i,km)
          t1 = 1.0_r8 - t0
          t2 = cubcoef(i,j,2) + t0
          t3 = cubcoef(i,j,1) + t1
          t4 = t0 * t2
          fint2(i) = t3 * ( t1 * ( cubcoef(i,j,3) * t0 * f1(i) + &
                     cubcoef(i,j,4) * t2 * f2(i) ) + &
                     cubcoef(i,j,5) * t4 * f3(i) ) + &
                     cubcoef(i,j,6) * t4 * t1 * f4(i)
          fint3(i) = t3 * ( t1 * ( cubcoef(i,j,3) * t0 * g1(i) + &
                     cubcoef(i,j,4) * t2 * g2(i) ) + &
                     cubcoef(i,j,5) * t4 * g3(i) ) + &
                     cubcoef(i,j,6) * t4 * t1 * g4(i)
       ENDDO
       DO i=1,ibdim
          IF (kloc(i,km).ne.1.and.kloc(i,km).ne.kmax-1) THEN
             k = kloc(i,km) - 1
             k1 = kloc(i,km) + 2
             fb = s2(i,km) * ( f(ip(i,km,4),k,j2(i,km)) + ulondp(i,km,2) * &
                   ( f(ip(i,km,5),k,j2(i,km)) - f(ip(i,km,4),k,j2(i,km)) ) )
             fc = s3(i,km) * ( f(ip(i,km,8),k,j3(i,km)) + ulondp(i,km,3) * &
                   ( f(ip(i,km,9),k,j3(i,km)) - f(ip(i,km,8),k,j3(i,km)) ) )
             fa = s2(i,km) * ( f(ip(i,km,4),k1,j2(i,km)) + ulondp(i,km,2) * &
                   ( f(ip(i,km,5),k1,j2(i,km)) - f(ip(i,km,4),k1,j2(i,km)) ) )
             fd = s3(i,km) * ( f(ip(i,km,8),k1,j3(i,km)) + ulondp(i,km,3) * &
                   ( f(ip(i,km,9),k1,j3(i,km)) - f(ip(i,km,8),k1,j3(i,km)) ) )
             fint1(i) = fb + ulatdp(i,km) * (fc-fb)
             fint4(i) = fa + ulatdp(i,km) * (fd-fa)
             s = usigdp(i,km)
             t1 = 1.0_r8 - s
             t2 = cubcoefv(i,kloc(i,km),2) + s
             t3 = cubcoefv(i,kloc(i,km),1) + t1
             t4 = s * t2
             fint(i,km) = t3 * ( t1 * ( cubcoefv(i,kloc(i,km),3) * s * fint1(i) + &
                          cubcoefv(i,kloc(i,km),4) * t2 * fint2(i) ) + &
                          cubcoefv(i,kloc(i,km),5) * t4 * fint3(i) ) + &
                          cubcoefv(i,kloc(i,km),6) * t4 * t1 * fint4(i)
          ELSE
             fint(i,km) = fint2(i) + usigdp(i,km) * ( fint3(i)-fint2(i) )
          ENDIF
       ENDDO
       DO i=1,ibdim
             k = kloc(i,km)
             k1 = k + 1
             sma = MAX( f(ip(i,km,4),k,j2(i,km)), f(ip(i,km,5),k,j2(i,km)) , &
                        f(ip(i,km,8),k,j3(i,km)), f(ip(i,km,9),k,j3(i,km)) )
             sma1 = MAX( f(ip(i,km,4),k1,j2(i,km)), f(ip(i,km,5),k1,j2(i,km)) , &
                         f(ip(i,km,8),k1,j3(i,km)), f(ip(i,km,9),k1,j3(i,km)) )
             smi = MIN( f(ip(i,km,4),k,j2(i,km)), f(ip(i,km,5),k,j2(i,km)) , &
                        f(ip(i,km,8),k,j3(i,km)), f(ip(i,km,9),k,j3(i,km)) )
             smi1 = MIN( f(ip(i,km,4),k1,j2(i,km)), f(ip(i,km,5),k1,j2(i,km)) , &
                         f(ip(i,km,8),k1,j3(i,km)), f(ip(i,km,9),k1,j3(i,km)) )
             fint(i,km) = MIN(fint(i,km),MAX(sma,sma1))
             fint(i,km) = MAX(fint(i,km),MIN(smi,smi1))
       ENDDO
    ENDDO
    !
  END SUBROUTINE Interpcublin3dlim
  SUBROUTINE Vectorialtend ()
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
    REAL(KIND=r8)     :: amf = 1.0_r8
    REAL(KIND=r8)     :: dlon
    REAL(KIND=r8)     :: sdlon
    REAL(KIND=r8)     :: cdlon
    REAL(KIND=r8)     :: cgca
    REAL(KIND=r8)     :: alpha
    REAL(KIND=r8)     :: alpham
    REAL(KIND=r8)     :: sgam
    REAL(KIND=r8)     :: cgam
    REAL(KIND=r8)     :: gamm
    REAL(KIND=r8)     :: clat
    REAL(KIND=r8)     :: slat
    REAL(KIND=r8)     :: sgamma
    REAL(KIND=r8)     :: cgamma
    REAL(KIND=r8)     :: fsalpx
    REAL(KIND=r8)     :: fsalpy
    REAL(KIND=r8)     :: fcalpx
    REAL(KIND=r8)     :: fcalpy
    REAL(KIND=r8)     :: salph 
    REAL(KIND=r8)     :: calph 
    REAL(KIND=r8)     :: salphm 
    REAL(KIND=r8)     :: calphm 
    REAL(KIND=r8)     :: xp
    REAL(KIND=r8)     :: yp
    REAL(KIND=r8)     :: xm
    REAL(KIND=r8)     :: ym
    REAL(KIND=r8)     :: px
    REAL(KIND=r8)     :: py
    REAL(KIND=r8)     :: x2y2
    !
    !$OMP SINGLE
    jGlob = 0
    !$OMP END SINGLE
    DO
       !$OMP CRITICAL(slcritvt)
       jGlob = jGlob + 1
       jb = jGlob
       !$OMP END CRITICAL(slcritvt)
       IF (jb > jbMax) EXIT
 
       DO k=1,kmax
          DO i=1,ibmaxperJB(jb)
             slat = SIN(ulatm(i+ibmaxperJB(jb)*(k-1),jb))
             slat =sign(max(abs(slat),1.e-20_r8),slat)
             clat = COS(ulatm(i+ibmaxperJB(jb)*(k-1),jb))
             dlon = longit(i,jb)-ulonm(i+ibmaxperJB(jb)*(k-1),jb)
             sdlon = SIN(dlon)
             cdlon = COS(dlon)
             cgca = sinlat(i,jb) * slat + coslat(i,jb) * clat * cdlon
             alpha=ACOS(sign(min(abs(cgca),1.0_r8),cgca))
             sgam = (clat * cgca - cdlon * coslat(i,jb)) / slat
             cgam = coslat(i,jb) * sdlon
             cgam=sign(max(abs(cgam),1.e-20_r8),cgam)
             gamm = ATAN2(sgam,cgam)
             sgamma = SIN(gamm)
             cgamma = COS(gamm)
             alpham = - amf * alpha
             fsalpx = cgamma * slat
             fcalpx = sgamma * cgamma * clat
             fsalpy = sgamma * slat
             fcalpy = sgamma**2 * clat
             salph  = SIN(alpha)
             calph  = 1.0_r8 - COS(alpha)
             salphm = SIN(alpham)
             calphm = 1.0_r8 - COS(alpham)
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
    REAL(KIND=r8)    :: p1, p2


    ALLOCATE(phi(ibmax,-1:jmax+2))
    ALLOCATE(delphi(ibmax,-1:jmax+2))
    ALLOCATE(sigma(ibmax,-1:jmax+2))
    ALLOCATE(delsigma(ibmax,-1:jmax+2))
    ALLOCATE(dellon(ibmax,-1:jmax+2))
    ALLOCATE(cubcoef(ibmax,0:jmax,6))
    ALLOCATE(cubcoefv(ibmax,1:kmax,6))
    ALLOCATE(ulonm  (ibmax*kmax,jbmax))
    ALLOCATE(ulatm  (ibmax*kmax,jbmax))
    ALLOCATE(usigm  (ibmax*kmax,jbmax))
    ALLOCATE(ulonm2D  (ibmax,jbmax))
    ALLOCATE(ulatm2D  (ibmax,jbmax))
    ALLOCATE(requests(10*jovlap))
    ALLOCATE(requestr(10*jovlap))
    ALLOCATE(requestsm(10*((jovlap+1)/2)))
    ALLOCATE(requestrm(10*((jovlap+1)/2)))


    jov = (jovlap+1)/2
    DO j=1,jmax 
       dellon(1,j) = 2.0_r8 * pai /imaxperj(j)
       phi(1,j) = colrad(j) - pihalf
    ENDDO
    phi(1,0)= - colrad(1) - pihalf
    phi(1,-1) = -colrad(2) - pihalf
    phi(1,jmax+1) = - phi(1,0)
    phi(1,jmax+2) = - phi(1,-1)
    dellon(1,0) = dellon(1,1)
    dellon(1,-1) = dellon(1,2)
    dellon(1,jmax+1) = dellon(1,jmax)
    dellon(1,jmax+2) = dellon(1,jmax-1)
    DO j=-1,jmax+1
       delphi(1,j) = phi(1,j+1)-phi(1,j)
    ENDDO
    delphiref = delphi(1,-1)
    DO j=0,jmax+1
       delphiref = MIN(delphiref,delphi(1,j))
    ENDDO
    delsigref = delcl(1)
    DO k=2,kmax-1
       delsigref = MIN(delsigref,delcl(k))
    ENDDO
    delphiref = delphiref/2.0_r8
    delsigref = delsigref/2.0_r8
    kmaxref = ( cl(kmax)-cl(1) ) / delsigref + 1
    jmaxref = pai / delphiref + 1
    delphiref = pai / jmaxref
    delsigref = ( cl(kmax)-cl(1) ) / kmaxref
 
    ALLOCATE(jphiref(ibmax,0:jmaxref))
    ALLOCATE(ksigref(ibmax,0:kmaxref))

    jphiref(1,0) = 0
    k = 0
    DO j=1,jmaxref
       IF (j*delphiref-pihalf.gt.phi(1,k+1)) k = k + 1
       jphiref(1,j) = k
    ENDDO
    ksigref(1,0) = 1
    j = 1
    DO k=1,kmaxref
       IF (k*delsigref+cl(1).gt.cl(j+1)) j = j + 1
       j = MIN(j,kmax-1)
       ksigref(1,k) = j
    ENDDO

    DO j=0,jmax
       p1 = delphi(1,j+1) / delphi(1,j)
       p2 = delphi(1,j-1) / delphi(1,j)
       cubcoef(1,j,1) = p1
       cubcoef(1,j,2) = p2
       cubcoef(1,j,3) = -1.0_r8 / (p2*(1.0_r8+p2)*(1.0_r8+p2+p1))
       cubcoef(1,j,4) = 1.0_r8 / (p2*(1.0_r8+p1))
       cubcoef(1,j,5) = 1.0_r8 / (p1*(1.0_r8+p2))
       cubcoef(1,j,6) = -1.0_r8 / (p1*(1.0_r8+p1)*(1.0_r8+p2+p1))
    ENDDO
    DO j=2,kmax-2
       p1 = delcl(j+1) / delcl(j)
       p2 = delcl(j-1) / delcl(j)
       cubcoefv(1,j,1) = p1
       cubcoefv(1,j,2) = p2
       cubcoefv(1,j,3) = -1.0_r8 / (p2*(1.0_r8+p2)*(1.0_r8+p2+p1))
       cubcoefv(1,j,4) = 1.0_r8 / (p2*(1.0_r8+p1))
       cubcoefv(1,j,5) = 1.0_r8 / (p1*(1.0_r8+p2))
       cubcoefv(1,j,6) = -1.0_r8 / (p1*(1.0_r8+p1)*(1.0_r8+p2+p1))
    ENDDO

    IF( TRIM(start) == "cold") THEN
       DO jb=1,jbmax
          DO ib=1,ibmaxperJB(jb)
             i = iperIJB(ib,jb)
             j = jperIJB(ib,jb)
             ulonm2D(ib,jb) = (i-1)*dellon(1,j)
             ulatm2D(ib,jb) = - phi(1,j)
             DO k=1,kmax
                usigm  (ib+(k-1)*ibmaxperJB(jb),jb) = cl(k)
                ulonm  (ib+(k-1)*ibmaxperJB(jb),jb) = (i-1)*dellon(1,j)
                ulatm  (ib+(k-1)*ibmaxperJB(jb),jb) = - phi(1,j)
             ENDDO
          ENDDO
       ENDDO
    ENDIF 
    
    DO i=2,ibmax
       phi(i,:) = phi(1,:)
       delphi(i,:) = delphi(1,:)
       dellon(i,:) = dellon(1,:)
       ksigref(i,:) = ksigref(1,:)
       jphiref(i,:) = jphiref(1,:)
       cubcoef(i,:,:) = cubcoef(1,:,:)
       cubcoefv(i,:,:) = cubcoefv(1,:,:)
    ENDDO
    DO k=1,kmax
       DO i=1,ibmax
          sigma(i,k) = cl(k)
          delsigma(i,k) = delcl(k)
       ENDDO
    ENDDO

  END SUBROUTINE InitSL

END MODULE SemiLagrangian
