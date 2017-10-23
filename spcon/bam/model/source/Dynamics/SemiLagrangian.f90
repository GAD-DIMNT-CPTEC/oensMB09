!
!  $Author: pkubota $
!  $Date: 2009/03/10 12:03:46 $
!  $Revision: 1.11 $
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
       maxnodes        , &
       DestroyParallelism, &
       myid

  USE Constants,    ONLY: &
       qmin             , &
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
       nfprt            , &
       microphys        , &
       nClass           , &
       nAeros           , &
       SL_twotime_scheme

  USE Sizes,        ONLY: &
       iPerIJB          , &
       jPerIJB          , &
       ibPerIJ          , &
       jbPerIJ          , &
       ibmaxperJB       , &
       imaxperJ         , &
       nodeHasJ         , &
       myfirstlat       , &
       mylastlat        , &
       myfirstlon       , &
       mylastlon        , &
       nrecs_gr         , &
       nsends_gr        , &
       ibmax            , &
       jbmax            , &
       jbmax_ext        , &
       jovlap           , &
       kmax             , &
       imax             , &
       jmax             , &
       del              , &
       delcl            , & 
       ci               , &
       cl                 
       
  USE FieldsDynamics,ONLY: &
       fgpass_scalars   , &
       adr_scalars      , &
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
       fgqmm            , &
       fgpsp            , &
       fgqp             , &
       fgtmpp           , &
       fgqm             , &
       fgtmpm           , &
       fglnpm           , &
       fgzs             , &
       fgumean          , &
       fgvmean          , &
       fgum             , &
       fgvm             , &
       fgwm             , &
       fgu              , &
       fgv              , &
       fgq              , &
       fgtmp            , &
       fglnps           , &
       fgvarm           , &
       fgvarp           , &
       fgvart           , &
       fgicem           , &
       fgicep           , &
       fgicet           , &
       fgliqm           , &
       fgliqp           , &
       fgliqt           , &
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
       Exchange_Fields

! USE Dumpgraph, ONLY : &
!      Dumpgra

  IMPLICIT NONE

  REAL(KIND=r8), ALLOCATABLE :: fgtmm     (:,:,:)
  REAL(KIND=r8), ALLOCATABLE :: fgumm     (:,:,:)
  REAL(KIND=r8), ALLOCATABLE :: fgvmm     (:,:,:)
  REAL(KIND=r8), ALLOCATABLE :: fgummean  (:,:)
  REAL(KIND=r8), ALLOCATABLE :: fgvmmean  (:,:)
  
  ! Use to store location of the 3d dep

  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: ulon   (:,:)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: ulat   (:,:)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: ulonm  (:,:)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: ulatm  (:,:)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: usig   (:,:)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: usigm  (:,:)

  ! Use to store location of the 2d dep

  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: ulon2D   (:,:)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: ulat2D   (:,:)
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
  
  INTEGER :: nsend
  INTEGER :: nsendw
  INTEGER :: kmaxref
  INTEGER :: jmaxref
  INTEGER :: jov
  INTEGER :: jglob
  REAL(KIND=r8)    :: delsigref
  REAL(KIND=r8)    :: delphiref
  PRIVATE
  PUBLIC :: SemiLagr, SemiLagr_2tl, InitSL
CONTAINS
  SUBROUTINE SemiLagr(nit , delt, slagr, slhum)
    !
    ! Trajectory: Determination of the departure point of Lagrangian trajectory
    !
    !
    INTEGER, INTENT(IN   ) :: nit 
    LOGICAL, INTENT(IN   ) :: slagr
    LOGICAL, INTENT(IN   ) :: slhum
    REAL(KIND=r8)   , INTENT(IN   ) :: delt
    !
    !  local variables 
    !
    INTEGER :: iloc  (ibmax*kmax*4)
    INTEGER :: jloc  (ibmax*kmax)
    INTEGER :: kloc  (ibmax*kmax)
    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: kk
    INTEGER :: n
    INTEGER :: klats
    INTEGER :: kprox
    INTEGER :: jb
    INTEGER :: ibdim
    INTEGER :: ip(ibmax*kmax*12)
    INTEGER :: jn(ibmax*kmax*12)
    LOGICAL :: first
    LOGICAL :: log1
    LOGICAL :: log2
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
       IF (slagr) THEN
          fgumean(:,j) = 0.0_r8
          fgvmean(:,j) = 0.0_r8
          DO k = 1, kmax
             fgumean(:,j) = fgumean(:,j) + fgu(:,k,j) * del(k)
             fgvmean(:,j) = fgvmean(:,j) + fgv(:,k,j) * del(k)
          ENDDO
       ENDIF
    ENDDO
    !
    ! Exchange Winds for overlapping areas
    ! ------------------------------------
    IF (maxnodes.gt.1) THEN
       !$OMP BARRIER
       !$OMP SINGLE
       CALL Exchange_Winds (fgu,fgv,fgw,fgumean,fgvmean,nrecs_gr,nsends_gr)
       !$OMP END SINGLE
    ENDIF
!   write(0,*) myid, ' wind to be exchanged '
!  CALL DestroyParallelism("***MODEL stopped  NORMALY***")
!   stop
    !
    ! Prepare tendencies for interpolation on departure point
    ! -------------------------------------------------------
    delta = delt
    delta1 = delt
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
 
      IF (slagr) THEN
        fgyum(:,:,j) = fgum(:,:,j) + dt2 * fgyum(:,:,j) + delta * fgyu(:,:,j)
        fgyvm(:,:,j) = fgvm(:,:,j) + dt2 * fgyvm(:,:,j) + delta * fgyv(:,:,j)
        fgtdm(:,:,j) = fgtmpm(:,:,j) + dt2 * fgtdm(:,:,j) + delta * fgtd(:,:,j)
        fgvdlnpm(:,j) = fglnpm(:,j) + fgzs(:,j) / (tbar*gasr) & 
             + dt2 * fgvdlnpm(:,j) + delta * fgvdlnp(:,j)
      ENDIF
      fgqp(:,:,j) = delta * fgqd(:,:,j)
      fgqd(:,:,j) = fgqm(:,:,j) + fgqp(:,:,j)
      IF (microphys) THEN
         fgicep(:,:,j) = delta * fgicet(:,:,j)
         fgicet(:,:,j) = fgicem(:,:,j) + fgicep(:,:,j)

         fgliqp(:,:,j) = delta * fgliqt(:,:,j)
         fgliqt(:,:,j) = fgliqm(:,:,j) + fgliqp(:,:,j)

         DO kk=1,nClass+nAeros
            fgvarp(:,:,j,kk) = delta * fgvart(:,:,j,kk)
            fgvart(:,:,j,kk) = fgvarm(:,:,j,kk) + fgvarp(:,:,j,kk)
         END DO
      ENDIF
    ENDDO
    !
    ! Exchange fields for interpolation (overlapping area) 
    ! ----------------------------------------------------
    !$OMP BARRIER
    IF (maxnodes.gt.1) THEN
       !$OMP SINGLE
       IF (microphys) THEN
          IF((nClass+nAeros) >0)THEN
             CALL Exchange_Fields(fgyum,fgyvm,fgtdm,fgqd,fgvdlnpm,fgpass_scalars, &
               adr_scalars,nscalars,nrecs_gr,nsends_gr,slagr,fgicet,fgliqt,fgvart)
          ELSE
             CALL Exchange_Fields(fgyum,fgyvm,fgtdm,fgqd,fgvdlnpm,fgpass_scalars, &
               adr_scalars,nscalars,nrecs_gr,nsends_gr,slagr,fgicet,fgliqt)
          END IF
        ELSE
          CALL Exchange_Fields(fgyum,fgyvm,fgtdm,fgqd,fgvdlnpm,fgpass_scalars, &
               adr_scalars,nscalars,nrecs_gr,nsends_gr,slagr)
       ENDIF
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
            nit   , delta1 , ip, jn)           
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
       !
       ! Interpolate and finish tendencies
       ! ---------------------------------
       IF (slagr) THEN
          CALL Interpcublin3d(fint  , fgyum , .FALSE. , &
            ulondp, ulatdp, usigdp, &
            iloc  , jloc  , kloc  , &
            ibdim , -1.0_r8  , .TRUE., &
            ip , jn, &
            s1 , s2, s3, s4, .FALSE. )
       !
       !    The Variables fgumm and fgvmm are used here as auxiliary 
       !     to hold the interpolated values of fgyum and fgyvm
       !    This is safe to do, since this variables will be redefined
       !    later (in Timefilterstep1) before used again.
       !    
          DO k=1,kmax
             DO i=1,ibdim
                fgqmm(i,k,jb) = fint((k-1)*ibdim+i)
             ENDDO
          ENDDO
          CALL Interpcublin3d(fint  , fgyvm , .TRUE.  , &
               ulondp, ulatdp, usigdp, &
               iloc  , jloc  , kloc  , &
               ibdim , -1.0_r8  ,.FALSE., &
               ip , jn, &
               s1 , s2, s3, s4, .FALSE. )
          DO k=1,kmax
             DO i=1,ibdim
                fgtmm(i,k,jb) = fint((k-1)*ibdim+i)
             ENDDO
          ENDDO
       ENDIF 
       first = .not.slagr
       CALL Interpcublin3d(fint  , fgqd , .FALSE. , &
            ulondp, ulatdp, usigdp, &
            iloc  , jloc  , kloc  , &
            ibdim ,  1.0_r8  ,first  , &
            ip , jn, &
            s1 , s2, s3, s4, slhum)
       IF (.not.slhum) THEN
          DO k=1,kmax
             DO i=1,ibdim
                fgqd(i,k,jb) = fint((k-1)*ibdim+i) + fgqp(i,k,jb)
             ENDDO
          ENDDO
        ELSE
          DO k=1,kmax
             DO i=1,ibdim
                fgqp(i,k,jb) = MAX(fint((k-1)*ibdim+i) + fgqp(i,k,jb),qmin)
             ENDDO
          ENDDO
       ENDIF
       IF (microphys) THEN
          CALL Interpcublin3d(fint  , fgicet , .FALSE. , &
               ulondp, ulatdp, usigdp, &
               iloc  , jloc  , kloc  , &
               ibdim ,  1.0_r8  ,first, &
               ip , jn, &
               s1 , s2, s3, s4, slhum)
          DO k=1,kmax
             DO i=1,ibdim
                fgicep(i,k,jb) = MAX(fint((k-1)*ibdim+i) + fgicep(i,k,jb),qmin)
             ENDDO
          ENDDO
          CALL Interpcublin3d(fint  , fgliqt , .FALSE. , &
               ulondp, ulatdp, usigdp, &
               iloc  , jloc  , kloc  , &
               ibdim ,  1.0_r8  ,first, &
               ip , jn, &
               s1 , s2, s3, s4, slhum)
          DO k=1,kmax
             DO i=1,ibdim
                fgliqp(i,k,jb) = MAX(fint((k-1)*ibdim+i) + fgliqp(i,k,jb),qmin)
             ENDDO
          ENDDO
          DO kk=1,nClass+nAeros
             CALL Interpcublin3d(fint  , fgvart(:,:,:,kk) , .FALSE. , &
                  ulondp, ulatdp, usigdp, &
                  iloc  , jloc  , kloc  , &
                  ibdim ,  1.0_r8  ,first, &
                  ip , jn, &
                  s1 , s2, s3, s4, slhum)
             DO k=1,kmax
                DO i=1,ibdim
                   fgvarp(i,k,jb,kk) = MAX(fint((k-1)*ibdim+i) + fgvarp(i,k,jb,kk),qmin)
                END DO
             END DO
          END DO
       ENDIF
       IF (slagr) THEN
          CALL Interpcublin3d(fint  , fgtdm , .TRUE.  , &
               ulondp, ulatdp, usigdp, &
               iloc  , jloc  , kloc  , &
               ibdim ,  1.0_r8  ,.FALSE., &
               ip , jn, &
               s1 , s2, s3, s4, .FALSE. )
          DO k=1,kmax
             DO i=1,ibdim
                fgtd(i,k,jb) = fint((k-1)*ibdim+i)  + delta * fgtd(i,k,jb)
             ENDDO
          ENDDO
       ENDIF
       !
       ! advection of passive scalars
       ! ----------------------------
       !
       ! Locate trajectory mid-points for interpolation
       ! ----------------------------------------------
       IF (nscalars.gt.0) THEN
          klats = 1 
          IF (reducedGrid) klats = 4
          CALL Locate(.TRUE. ,.TRUE.  , &
               ulonm(1,jb), ulatm(1,jb), usigm(1,jb), &
               ulondp, ulatdp, usigdp , &
               iloc  , jloc  , kloc   , &
               ibdim , klats)
          log1 = .FALSE.
          log2 = .TRUE.
       ENDIF
       DO n=1,nscalars
          kprox = 3-adr_scalars
          CALL Interpcublin3d(fint, &
               fgpass_scalars(1,1,1,n,adr_scalars),log1 , &
               ulondp, ulatdp, usigdp, &
               iloc  , jloc  , kloc  , &
               ibdim ,  1.0_r8  ,log2 ,  &
               ip , jn, &
               s1 , s2, s3, s4, .TRUE. )
          DO k=1,kmax
             DO i=1,ibdim
                fgpass_scalars(i,k,jb,n,kprox) &
                              = fint((k-1)*ibdim+i)
             ENDDO
          ENDDO
          log1 = .TRUE.
          log2 = .FALSE.
       ENDDO
       IF (slagr) THEN
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
       !
       ! Interpolate and finish tendencies
       ! ---------------------------------
          CALL Interpcublin2d(fint  , fgvdlnpm, &
               ulondp, ulatdp, &
               iloc  , jloc  , &
               ibdim)
          fgvdlnp(1:ibdim,jb) = fint(1:ibdim) + delta * fgvdlnp(1:ibdim,jb)
       ENDIF
    ENDDO
    !
    !  transform tendencies of momentum equations in vector form
    !  ---------------------------------------------------------
    !$OMP BARRIER
    IF (slagr) THEN 
       CALL Vectorialtend(fgqmm,fgtmm)
    !
    !  finalize tendencies
    !  -------------------
       !$OMP BARRIER
    ENDIF
    !$OMP SINGLE
    adr_scalars = 3 - adr_scalars
    jGlob = 0
    !$OMP END SINGLE
    DO
       !$OMP CRITICAL(slcrit4)
       jGlob = jGlob + 1
       j = jGlob
       !$OMP END CRITICAL(slcrit4)
       IF (j > jbMax) EXIT
 
       IF (slagr) THEN
          fgyu(:,:,j) = fgqmm(:,:,j) + delta * fgyu(:,:,j)
          fgyv(:,:,j) = fgtmm(:,:,j) + delta * fgyv(:,:,j)
       ENDIF
    !
    ! Restore winds
    ! -------------
       DO i = 1, ibmaxperJB(j)
          aux = coslat(i,j) * er
          fgu (i,:,j)  =  fgu(i,:,j) * aux
          fgv (i,:,j)  =  fgv(i,:,j) * aux
       ENDDO
    !
    ENDDO
    END SUBROUTINE SemiLagr
  SUBROUTINE SemiLagr_2tl(nit , delt, fa)
    !
    ! Trajectory: Determination of the departure point of Lagrangian trajectory
    !
    !
    INTEGER, INTENT(IN   ) :: nit 
    REAL(KIND=r8)   , INTENT(IN   ) :: delt
    REAL(KIND=r8)   , INTENT(IN   ) :: fa
    !
    !  local variables 
    !
    INTEGER :: iloc  (ibmax*kmax*4)
    INTEGER :: jloc  (ibmax*kmax)
    INTEGER :: kloc  (ibmax*kmax)
    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: kprox
    INTEGER :: klats
    INTEGER :: jb
    INTEGER :: ibdim
    INTEGER :: ip(ibmax*kmax*12)
    INTEGER :: jn(ibmax*kmax*12)
    REAL(KIND=r8)    :: s1(ibmax*kmax),s2(ibmax*kmax),s3(ibmax*kmax),s4(ibmax*kmax)
    REAL(KIND=r8)    :: ulondp(ibmax*kmax*4)
    REAL(KIND=r8)    :: ulatdp(ibmax*kmax)
    REAL(KIND=r8)    :: usigdp(ibmax*kmax)
    REAL(KIND=r8)    :: fint  (ibmax*kmax)
    REAL(KIND=r8)    :: aux
    REAL(KIND=r8)    :: dt2
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
          fgum(i,:,j)  =  (2._r8*fgu(i,:,j)-fgum(i,:,j)) / aux
          fgvm(i,:,j)  =  (2._r8*fgv(i,:,j)-fgvm(i,:,j)) / aux
          fgwm(i,:,j)  =  (2._r8*fgw(i,:,j)-fgwm(i,:,j))
       ENDDO
    !
    ! Compute average wind on all levels
    ! ----------------------------------
       fgummean(:,j) = 0.0_r8
       fgvmmean(:,j) = 0.0_r8
       DO k = 1, kmax
          fgummean(:,j) = fgummean(:,j) + fgum(:,k,j) * del(k)
          fgvmmean(:,j) = fgvmmean(:,j) + fgvm(:,k,j) * del(k)
       ENDDO
    ENDDO
    !
    ! Exchange Winds for overlapping areas
    ! ------------------------------------
    IF (maxnodes.gt.1) THEN
       !$OMP BARRIER
       !$OMP SINGLE
       CALL Exchange_Winds (fgum,fgvm,fgwm,fgummean,fgvmmean,nrecs_gr,nsends_gr)
       !$OMP END SINGLE
    ENDIF
    !
    ! Prepare tendencies for interpolation on departure point
    ! -------------------------------------------------------
    dt2 = delt * 0.5_r8
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
 
       fgumm(:,:,j) = fgu(:,:,j) +  delt * ( fgyum(:,:,j) + fgyu(:,:,j) )
       fgvmm(:,:,j) = fgv(:,:,j) +  delt * ( fgyvm(:,:,j) + fgyv(:,:,j) )
       fgtmpp(:,:,j) = fgtmp(:,:,j) + delt * ( fgtdm(:,:,j) + fgtd(:,:,j) )
       fgqp(:,:,j) = fgq(:,:,j) +  delt * ( fgqdm(:,:,j) + fgqd(:,:,j) )
       fgpsp(:,j) = fglnps(:,j) + fgzs(:,j) / (tbar*gasr) & 
            + delt * ( fgvdlnpm(:,j) + fgvdlnp(:,j) )
       DO i = 1, ibmaxperJB(j)
          aux = coslat(i,j) * er
          fgu (i,:,j)  =  fgu(i,:,j) / aux
          fgv (i,:,j)  =  fgv(i,:,j) / aux
       ENDDO
       fgumean(:,j) = 0.0_r8
       fgvmean(:,j) = 0.0_r8
       DO k = 1, kmax
          fgumean(:,j) = fgumean(:,j) + fgu(:,k,j) * del(k)
          fgvmean(:,j) = fgvmean(:,j) + fgv(:,k,j) * del(k)
       ENDDO
    ENDDO
    !
    ! Exchange fields for interpolation (overlapping area) 
    ! ----------------------------------------------------
    !$OMP BARRIER
    IF (maxnodes.gt.1) THEN
       !$OMP SINGLE
       CALL Exchange_Fields(fgumm,fgvmm,fgtmpp,fgqp,fgpsp,fgpass_scalars, &
                            adr_scalars,nscalars,nrecs_gr,nsends_gr,.true.)
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
       CALL Trajectory_2L(ulonm(1,jb),ulatm(1,jb),usig(1,jb),&
            ulon(1,jb)  , ulat(1,jb)  , &
            ulondp, ulatdp, usigdp , &
            iloc  , jloc  , kloc   , &
            ibdim , jb    ,          &
            nit   , delt  , ip, jn)           
       !
       ! Locate trajectory points for interpolation
       ! ------------------------------------------
       klats = 1 
       IF (reducedGrid) klats = 4
       CALL Locate(.TRUE. ,.TRUE.  , &
            ulon(1,jb)  , ulat(1,jb)  , usig(1,jb)   , &
            ulondp, ulatdp, usigdp , &
            iloc  , jloc  , kloc   , &
            ibdim , klats)
       !
       ! Interpolate and finish tendencies
       ! ---------------------------------
       CALL Interpcublin3d(fint  , fgumm , .FALSE. , &
            ulondp, ulatdp, usigdp, &
            iloc  , jloc  , kloc  , &
            ibdim , -1.0_r8  , .TRUE., &
            ip , jn, &
            s1 , s2, s3, s4, .FALSE. )
       !
       !    The Variables fgumm and fgvmm are used here as auxiliary 
       !     to hold the interpolated values of fgyum and fgyvm
       !    This is safe to do, since this variables will be redefined
       !    later (in Timefilterstep1) before used again.
       !    
       DO k=1,kmax
          DO i=1,ibdim
             fgqmm(i,k,jb) = fint((k-1)*ibdim+i)
          ENDDO
       ENDDO
       CALL Interpcublin3d(fint  , fgvmm , .TRUE.  , &
            ulondp, ulatdp, usigdp, &
            iloc  , jloc  , kloc  , &
            ibdim , -1.0_r8  ,.FALSE., &
            ip , jn, &
            s1 , s2, s3, s4, .FALSE. )
       DO k=1,kmax
          DO i=1,ibdim
             fgtmm(i,k,jb) = fint((k-1)*ibdim+i)
          ENDDO
       ENDDO
       CALL Interpcublin3d(fint  , fgqp , .FALSE. , &
            ulondp, ulatdp, usigdp, &
            iloc  , jloc  , kloc  , &
            ibdim ,  1.0_r8  ,.FALSE., &
            ip , jn, &
            s1 , s2, s3, s4, .FALSE. )

       IF(fa.ne.0.0_r8) fgqdm(:,:,jb) = - 0.5_r8 * fgqd(:,:,jb)
       DO k=1,kmax
          DO i=1,ibdim
             fgqd(i,k,jb) = fint((k-1)*ibdim+i) + dt2 * fgqd(i,k,jb)
          ENDDO
       ENDDO
       CALL Interpcublin3d(fint  , fgtmpp , .TRUE.  , &
            ulondp, ulatdp, usigdp, &
            iloc  , jloc  , kloc  , &
            ibdim ,  1.0_r8  ,.FALSE., &
            ip , jn, &
            s1 , s2, s3, s4, .FALSE. )
       IF(fa.ne.0.0_r8) fgtdm(:,:,jb) = - 0.5_r8 * fgtd(:,:,jb)
       DO k=1,kmax
          DO i=1,ibdim
             fgtd(i,k,jb) = fint((k-1)*ibdim+i)  + dt2 * fgtd(i,k,jb)
          ENDDO
       ENDDO
       !
       ! advection of passive scalars
       ! ----------------------------
       DO k=1,nscalars
          kprox = 3-adr_scalars
          CALL Interpcublin3d(fgpass_scalars(1,1,jb,k,kprox), &
               fgpass_scalars(1,1,1,k,adr_scalars), .TRUE. , &
               ulondp, ulatdp, usigdp, &
               iloc  , jloc  , kloc  , &
               ibdim ,  1.0_r8  ,.FALSE.,  &
               ip , jn, &
               s1 , s2, s3, s4, .TRUE. )
       ENDDO
       !
       ! compute departure points (2d-trajectory) 
       ! ----------------------------------------
       CALL Trajectory2D_2L(ulonm2D(1,jb),ulatm2D(1,jb),&
            ulon2D(1,jb)  , ulat2D(1,jb)  ,          &
            ulondp, ulatdp,          &
            iloc  , jloc  ,          &
            ibdim , jb    , nit    , delt)
       !
       ! Locate trajectory points for interpolation
       ! ------------------------------------------
       CALL Locate2D( &
            ulon2D(1,jb)  , ulat2D(1,jb)  , &
            ulondp, ulatdp, &
            iloc  , jloc  , &
            ibdim , klats)
       !
       ! Interpolate and finish tendencies
       ! ---------------------------------
       IF(fa.ne.0.0_r8) fgvdlnpm(1:ibdim,jb) = - 0.5_r8 * fgvdlnp(1:ibdim,jb)
       CALL Interpcublin2d(fint  , fgpsp, &
            ulondp, ulatdp, &
            iloc  , jloc  , &
            ibdim)
       fgvdlnp(1:ibdim,jb) = fint(1:ibdim) + dt2 * fgvdlnp(1:ibdim,jb)
    ENDDO
    !
    !  transform tendencies of momentum equations in vector form
    !  ---------------------------------------------------------
    !$OMP BARRIER
    CALL Vectorialtend(fgqmm,fgtmm)
    !
    !  finalize tendencies
    !  -------------------
    !$OMP BARRIER
    !$OMP SINGLE
    adr_scalars = 3-adr_scalars
    jGlob = 0
    !$OMP END SINGLE
    DO
       !$OMP CRITICAL(slcrit4)
       jGlob = jGlob + 1
       j = jGlob
       !$OMP END CRITICAL(slcrit4)
       IF (j > jbMax) EXIT
 
       IF(fa.ne.0.0_r8) fgyum(:,:,j) = - 0.5_r8 * fgyu(:,:,j)
       IF(fa.ne.0.0_r8) fgyvm(:,:,j) = - 0.5_r8 * fgyv(:,:,j)
       fgyu(:,:,j) = fgqmm(:,:,j) + dt2 * fgyu(:,:,j)
       fgyv(:,:,j) = fgtmm(:,:,j) + dt2 * fgyv(:,:,j)
       fgwm(:,:,j) = fgw(:,:,j)
    !
    ! Restore winds
    ! -------------
       DO i = 1, ibmaxperJB(j)
          aux = coslat(i,j) * er
          fgu (i,:,j)  =  fgu(i,:,j) * aux
          fgv (i,:,j)  =  fgv(i,:,j) * aux
       ENDDO
    !
    ENDDO
    END SUBROUTINE SemiLagr_2tl
  SUBROUTINE Trajectory_2L(        &
       ulonm , ulatm , usig   , &
       ulon  , ulat  ,          &
       ulondp, ulatdp, usigdp , &
       iloc  , jloc  , kloc   , &
       ibdim , jb    ,          &
       nit   , delt , ip, jn)
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
    INTEGER, INTENT(INOUT) :: jn    (ibdim,kmax,4)
    INTEGER, INTENT(INOUT) :: ip    (ibdim,kmax,4)
    REAL(KIND=r8)   , INTENT(IN   ) :: delt
    REAL(KIND=r8)   , INTENT(INOUT) :: ulon  (ibmax,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulat  (ibmax,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: usig  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulonm (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulatm (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulondp(ibdim,kmax,4)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulatdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: usigdp(ibdim,kmax)
    !
    !  local variables 
    !
    INTEGER                        :: kit
    INTEGER                        :: klats
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
       lsig = kit.eq.1
       !
       ! compute locations and indices for interpolation of the wind
       ! -----------------------------------------------------------
       !
       CALL Locate( lsig  ,.TRUE.  , &
            ulon  , ulat  , usig   , &
            ulondp, ulatdp, usigdp , &
            iloc  , jloc  , kloc   , &
            ibdim , klats)
       !
       ! interpolate vertical wind 
       ! -------------------------
       !
       CALL InterplG(&
            wint  , fgwm           , &
            ulondp, ulatdp, usigdp , &
            iloc  , jloc  , kloc   , &
            ibdim , ip , jn)
       !
       ! update sigma level of mid-point of trajectory
       ! ---------------------------------------------
       !
       CALL Upsig_2L(usig, fgw(1,1,jb), wint, delt, ibdim)
       !
       ! computes new sigma location for interpolation
       ! ---------------------------------------------
       !
       CALL Locate(.TRUE. ,.FALSE. , &
            ulon  , ulat  , usig   , &
            ulondp, ulatdp, usigdp , &
            iloc  , jloc  , kloc   , &
            ibdim , klats)
       !
       ! interpolate horizontal wind field
       ! ---------------------------------
       !
       CALL InterpluvG(&
            fgum  , fgvm  , uint   , vint, &
            ulondp, ulatdp, usigdp , &
            jloc  , kloc  , ibdim  , &
            ip    , jn)
       !
       ! update latitude and longitude of mid-point of trajectory
       ! --------------------------------------------------------
       !
       CALL Uplatlon_2L(ulon,ulat,ulonm,ulatm,&
            coslon(1,jb),sinlon(1,jb),coslat(1,jb),sinlat(1,jb),&
            fgu(1,1,jb),fgv(1,1,jb),uint,vint,delt,ibdim)
       !

    ENDDO
    !
  END SUBROUTINE Trajectory_2L
  SUBROUTINE Trajectory2D_2L(        &
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
    INTEGER                        :: klats
    REAL(KIND=r8)                           :: uint(ibmax)
    REAL(KIND=r8)                           :: vint(ibmax)
    !
    ! Iterate to find departure point
    ! -------------------------------
    !
    klats = 1
    IF (reducedGrid) klats = 2
    DO kit=1,nit
       !
       ! compute locations and indices for interpolation of the wind
       ! -----------------------------------------------------------
       !
       CALL Locate2D( &
            ulon  , ulat  , &
            ulondp, ulatdp, &
            iloc  , jloc  , &
            ibdim , klats)
       !
       ! interpolate horizontal wind field
       ! ---------------------------------
       !
       CALL InterpluvG2D(&
            fgummean, fgvmmean, &
            uint  , vint  , &
            ulondp, ulatdp, &
            iloc  , jloc  , &
            ibdim)
       !
       ! update latitude and longitude of mid-point of trajectory
       ! --------------------------------------------------------
       !
       CALL Uplatlon2D_2L(ulon,ulat,ulonm,ulatm,&
            coslon(1,jb),sinlon(1,jb),coslat(1,jb),sinlat(1,jb),&
            uint,vint,fgumean(1,jb),fgvmean(1,jb),delt,ibdim)
       !
    ENDDO
    !
  END SUBROUTINE Trajectory2D_2L
  SUBROUTINE Trajectory(        &
       ulonm , ulatm , usigm  , &
       ulon  , ulat  , usig   , &
       ulondp, ulatdp, usigdp , &
       iloc  , jloc  , kloc   , &
       ibdim , jb    ,          &
       nit   , delt , ip, jn)
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
    INTEGER, INTENT(INOUT) :: jn    (ibdim,kmax,4)
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
       !
       ! interpolate vertical wind 
       ! -------------------------
       !
       CALL InterplG(&
            wint  , fgw            , &
            ulondp, ulatdp, usigdp , &
            iloc  , jloc  , kloc   , &
            ibdim , ip , jn)
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
            fgu   , fgv            , &
            uint  , vint           , &
            ulondp, ulatdp, usigdp , &
            jloc  , kloc  , ibdim  , &
            ip    , jn )
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
       !
       ! interpolate horizontal wind field
       ! ---------------------------------
       !
       CALL InterpluvG2D(&
            fgumean,fgvmean,&
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
  SUBROUTINE Upsig_2L (usig, w, wm, delt, ibdim)
    !
    ! Upsig  : update sigma upstream
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    REAL(KIND=r8)   , INTENT(IN   ) :: w     (ibmax,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: wm    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(OUT  ) :: usig  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: delt
    !
    !  local variables 
    !
    REAL(KIND=r8)                  :: dth
    INTEGER                        :: i
    INTEGER                        :: k
    !
    dth = 0.5_r8*delt
    DO k=1,kmax
       DO i=1,ibdim
          usig(i,k) = MIN(cl(kmax),MAX(cl(1),cl(k) - dth * (w(i,k)+wm(i,k))))
       ENDDO
    ENDDO
    !
  END SUBROUTINE Upsig_2L
  SUBROUTINE Uplatlon_2L(ulon,ulat,ulonm,ulatm,coslon,sinlon,coslat,sinlat,&
       u,v,um,vm,delt,ibdim)
    !
    ! Uplatlon: update latitude and longitude of upstream points
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    REAL(KIND=r8)   , INTENT(IN   ) :: coslon(ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: sinlon(ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: coslat(ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: sinlat(ibdim)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulat  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulon  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulatm (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulonm (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: u     (ibmax,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: v     (ibmax,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: um    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: vm    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: delt
    !
    !  local variables 
    !
    INTEGER :: i
    INTEGER :: k
    REAL(KIND=r8)    :: dt2     ,&
         dt2sq   ,&
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
         uh      ,&
         vh      ,&
         xb      ,&
         yb      ,&
         zb      ,&
         xm      ,&
         ym      ,&
         zm      ,&
         b
    !
    !
    dt2 = 0.5_r8*delt
    dt2sq = dt2*dt2
    dpi = 2.0_r8 * pai
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
          uh = 0.5_r8 * (u(i,k)+um(i,k))
          vh = 0.5_r8 * (v(i,k)+vm(i,k))
          xdot = - ( uh * sinlba + vh * coslba * sinphi )
          ydot =  uh * coslba - vh * sinlba * sinphi
          zdot = cosphi * vh
          xb = x(i)-dt2*xdot 
          yb = y(i)-dt2*ydot 
          zb = z(i)-dt2*zdot 
          b = 1.0_r8 / SQRT(xb*xb+yb*yb+zb*zb)
          xb = b * xb
          yb = b * yb
          zb = b * zb
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
    !
  END SUBROUTINE Uplatlon_2L
  SUBROUTINE Uplatlon2D_2L(ulon,ulat,ulonm,ulatm,coslon,sinlon,coslat,sinlat,&
       u,v,um,vm,delt,ibdim)
    !
    ! Uplatlon: update latitude and longitude of upstream points
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
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
    REAL(KIND=r8)   , INTENT(IN   ) :: um    (ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: vm    (ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: delt
    !
    !  local variables 
    !
    INTEGER :: i
    REAL(KIND=r8)    :: dt2    ,&
         dt2sq  ,&
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
         uh     ,&
         vh     ,&
         xb     ,&
         yb     ,&
         zb     ,&
         xm     ,&
         ym     ,&
         zm     ,&
         b
    !
    !
    dt2 = 0.5_r8*delt
    dt2sq = dt2*dt2
    dpi = 2.0_r8 * pai
    DO i=1,ibdim
       coslba = COS(ulonm(i))
       sinlba = SIN(ulonm(i))
       cosphi = COS(ulatm(i))
       sinphi = SIN(ulatm(i))
       uh = 0.5_r8 * (u(i)+um(i))
       vh = 0.5_r8 * (v(i)+vm(i))
       x = coslon(i) * coslat(i)
       y = sinlon(i) * coslat(i)
       z = sinlat(i)
       xdot = - ( uh * sinlba + vh * coslba * sinphi )
       ydot =  uh * coslba - vh * sinlba * sinphi
       zdot = cosphi * vh
       xb = x-dt2*xdot
       yb = y-dt2*ydot
       zb = z-dt2*zdot
       b = 1.0_r8 / SQRT(xb*xb+yb*yb+zb*zb)
       xb = b * xb
       yb = b * yb
       zb = b * zb
       ulonm(i)=ATAN2(yb,xb)
       IF (ulonm(i).lt.0.0_r8) ulonm(i) = ulonm(i) + dpi
       ulatm(i)=ASIN(zb)
       dot2 = 2.0_r8 * (x*xb + y*yb + z*zb)
       xm = dot2 * xb - x
       ym = dot2 * yb - y
       zm = dot2 * zb - z
       ulon(i)=ATAN2(ym,xm)
       IF (ulon(i).lt.0.0_r8) ulon(i) = ulon(i) + dpi
       ulat(i)=ASIN(zm)
    ENDDO
    !
  END SUBROUTINE Uplatlon2D_2L
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
    INTEGER, INTENT(INOUT) :: iloc  (ibdim,kmax,4)
    INTEGER, INTENT(INOUT) :: jloc  (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: kloc  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulon  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulat  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: usig  (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulondp(ibdim,kmax,4)
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
    INTEGER, INTENT(INOUT) :: iloc  (ibdim,4)
    INTEGER, INTENT(INOUT) :: jloc  (ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulon  (ibdim)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulat  (ibdim)
    REAL(KIND=r8)   , INTENT(INOUT) :: ulondp(ibdim,4)
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
       u,   v, uint  , vint   , &
       ulondp, ulatdp, usigdp , &
       jloc  , kloc  , ibdim  , &
       ip    , jn )
    !
    ! interpluvG : linear interpolation of horizontal components of wind
    !              ( on Gaussian grid )
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: jloc  (ibdim,kmax)
    INTEGER, INTENT(IN   ) :: kloc  (ibdim,kmax)
    INTEGER, INTENT(IN   ) :: jn    (ibdim,kmax,4)
    INTEGER, INTENT(IN   ) :: ip    (ibdim,kmax,4)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulondp(ibdim,kmax,2)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulatdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: usigdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: u     (ibmax,kmax,jbmax_ext)
    REAL(KIND=r8)   , INTENT(IN   ) :: v     (ibmax,kmax,jbmax_ext)
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
          f1 = u(ip(i,k,1),kloc(i,k),jn(i,k,1)) + ulondp(i,k,1) * &
             ( u(ip(i,k,3),kloc(i,k),jn(i,k,3)) - u(ip(i,k,1),kloc(i,k),jn(i,k,1)) )
          g1 = v(ip(i,k,1),kloc(i,k),jn(i,k,1)) + ulondp(i,k,1) * &
             ( v(ip(i,k,3),kloc(i,k),jn(i,k,3)) - v(ip(i,k,1),kloc(i,k),jn(i,k,1)) )
          f2 = u(ip(i,k,2),kloc(i,k),jn(i,k,2)) + ulondp(i,k,2) * &
             ( u(ip(i,k,4),kloc(i,k),jn(i,k,4)) - u(ip(i,k,2),kloc(i,k),jn(i,k,2)) )
          g2 = v(ip(i,k,2),kloc(i,k),jn(i,k,2)) + ulondp(i,k,2) * &
             ( v(ip(i,k,4),kloc(i,k),jn(i,k,4)) - v(ip(i,k,2),kloc(i,k),jn(i,k,2)) )
          f3 = u(ip(i,k,1),kloc(i,k)+1,jn(i,k,1)) + ulondp(i,k,1) * &
           ( u(ip(i,k,3),kloc(i,k)+1,jn(i,k,3)) - u(ip(i,k,1),kloc(i,k)+1,jn(i,k,1)) )
          g3 = v(ip(i,k,1),kloc(i,k)+1,jn(i,k,1)) + ulondp(i,k,1) * &
           ( v(ip(i,k,3),kloc(i,k)+1,jn(i,k,3)) - v(ip(i,k,1),kloc(i,k)+1,jn(i,k,1)) )
          f4 = u(ip(i,k,2),kloc(i,k)+1,jn(i,k,2)) + ulondp(i,k,2) * &
           ( u(ip(i,k,4),kloc(i,k)+1,jn(i,k,4)) - u(ip(i,k,2),kloc(i,k)+1,jn(i,k,2)) )
          g4 = v(ip(i,k,2),kloc(i,k)+1,jn(i,k,2)) + ulondp(i,k,2) * &
           ( v(ip(i,k,4),kloc(i,k)+1,jn(i,k,4)) - v(ip(i,k,2),kloc(i,k)+1,jn(i,k,2)) )
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
       u     , v     , &
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
    REAL(KIND=r8)   , INTENT(IN   ) :: u     (ibmax,jbmax_ext)
    REAL(KIND=r8)   , INTENT(IN   ) :: v     (ibmax,jbmax_ext)
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
    INTEGER  :: j3
    INTEGER  :: j4
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
       j3 = jbPerIJ(iloc(i,1)+1,jp)
       j4 = jbPerIJ(iloc(i,2)+1,jp-1)
       f1 = s1(i) * ( u(i1,j1) + ulondp(i,1) * &
            ( u(i3,j3) - u(i1,j1) ) )
       g1 = s1(i) * ( v(i1,j1) + ulondp(i,1) * &
            ( v(i3,j3) - v(i1,j1) ) )
       f2 = s2(i) * ( u(i2,j2) + ulondp(i,2) * &
            ( u(i4,j4) - u(i2,j2) ) )
       g2 = s2(i) * ( v(i2,j2) + ulondp(i,2) * &
            ( v(i4,j4) - v(i2,j2) ) )
       uint(i) = f1 + ulatdp(i) * (f2-f1)
       vint(i) = g1 + ulatdp(i) * (g2-g1)
    ENDDO
    !
  END SUBROUTINE InterpluvG2D
  SUBROUTINE InterplG(&
       fint  , f              , &
       ulondp, ulatdp, usigdp , &
       iloc  , jloc  , kloc   , &
       ibdim , ip , jn  )
    !
    ! interplG : linear interpolation of a field f
    !              ( on Gaussian grid )
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: iloc  (ibdim,kmax,2)
    INTEGER, INTENT(IN   ) :: jloc  (ibdim,kmax)
    INTEGER, INTENT(IN   ) :: kloc  (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: jn    (ibdim,kmax,4)
    INTEGER, INTENT(INOUT) :: ip    (ibdim,kmax,4)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulondp(ibdim,kmax,2)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulatdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: usigdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: f     (ibmax,kmax,jbmax_ext)
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
    CHARACTER(LEN=*), PARAMETER :: h="**(InterplG)**"
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
          jn(i,k,1) = jbPerIJ(iloc(i,k,1),jp)
          jn(i,k,2) = jbPerIJ(iloc(i,k,2),jp-1)
          jn(i,k,3) = jbPerIJ(iloc(i,k,1)+1,jp)
          jn(i,k,4) = jbPerIJ(iloc(i,k,2)+1,jp-1)

       ENDDO
    ENDDO
    IF (ANY(ip(1:ibdim,1:kmax,1:4).eq.0)) THEN
       WRITE(nfprt,"(a,' jovlap too small')") h
       WRITE(nfprt,"(a,' try to increase vmax_est in Options.f90 ')") h
       STOP h
    ENDIF
    DO k=1,kmax
       DO i=1,ibdim
          f1 = f(ip(i,k,1),kloc(i,k),jn(i,k,1)) + ulondp(i,k,1) * &
                ( f(ip(i,k,3),kloc(i,k),jn(i,k,3)) - f(ip(i,k,1),kloc(i,k),jn(i,k,1)) )
          f2 = f(ip(i,k,2),kloc(i,k),jn(i,k,2)) + ulondp(i,k,2) * &
                ( f(ip(i,k,4),kloc(i,k),jn(i,k,4)) - f(ip(i,k,2),kloc(i,k),jn(i,k,2)) )
          f3 = f(ip(i,k,1),kloc(i,k)+1,jn(i,k,1)) + ulondp(i,k,1) * &
              ( f(ip(i,k,3),kloc(i,k)+1,jn(i,k,3)) - f(ip(i,k,1),kloc(i,k)+1,jn(i,k,1)) )
          f4 = f(ip(i,k,2),kloc(i,k)+1,jn(i,k,2)) + ulondp(i,k,2) * &
              ( f(ip(i,k,4),kloc(i,k)+1,jn(i,k,4)) - f(ip(i,k,2),kloc(i,k)+1,jn(i,k,2)) )
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
    REAL(KIND=r8)   , INTENT(IN   ) :: f     (ibmax,jbmax_ext)
    REAL(KIND=r8)   , INTENT(OUT  ) :: fint  (ibdim)
    !
    !  local variables 
    !
    INTEGER  :: i
    INTEGER  :: j
    INTEGER  :: jp 
    INTEGER  :: ip(ibmax,12)
    INTEGER  :: jn(ibmax,12)
    REAL(KIND=r8)     :: f1(ibmax)
    REAL(KIND=r8)     :: f2(ibmax)
    REAL(KIND=r8)     :: f3(ibmax)
    REAL(KIND=r8)     :: f4(ibmax)
    REAL(KIND=r8)     :: fa,fb,fc,fd,s,t,t1,t2,t3,t4
    CHARACTER(LEN=*), PARAMETER :: h="**(Interpcublin2d)**"
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
       jn(i,1) = jbPerIJ(iloc(i,1),jp+1)
       jn(i,2) = jbPerIJ(iloc(i,1)+1,jp+1)
       jn(i,3) = jbPerIJ(iloc(i,2)-1,jp)
       jn(i,4) = jbPerIJ(iloc(i,2),jp)
       jn(i,5) = jbPerIJ(iloc(i,2)+1,jp)
       jn(i,6) = jbPerIJ(iloc(i,2)+2,jp)
       jn(i,7) = jbPerIJ(iloc(i,3)-1,jp-1)
       jn(i,8) = jbPerIJ(iloc(i,3),jp-1)
       jn(i,9) = jbPerIJ(iloc(i,3)+1,jp-1)
       jn(i,10) = jbPerIJ(iloc(i,3)+2,jp-1)
       jn(i,11) = jbPerIJ(iloc(i,4),jp-2)
       jn(i,12) = jbPerIJ(iloc(i,4)+1,jp-2)
    ENDDO
    IF (ANY(ip(1:ibdim,1:12).eq.0)) THEN
       WRITE(nfprt,"(a,' jovlap too small')") h
       WRITE(nfprt,"(a,' try to increase vmax_est in Options.f90 ')") h
       STOP h
    ENDIF
    DO i=1,ibdim
       f1(i) = f(ip(i,1),jn(i,1)) + ulondp(i,1) * &
            ( f(ip(i,2),jn(i,2)) - f(ip(i,1),jn(i,1)) )
       s = ulondp(i,2)-0.5_r8
       fa = 0.5_r8 * ( f(ip(i,5),jn(i,5)) + f(ip(i,4),jn(i,4)) ) + &
            s * ( f(ip(i,5),jn(i,5)) - f(ip(i,4),jn(i,4)) )
       fb = 0.5_r8 * ( f(ip(i,6),jn(i,6)) + f(ip(i,3),jn(i,3)) ) + &
            s * ( f(ip(i,6),jn(i,6)) - f(ip(i,3),jn(i,3)) ) / 3.0_r8
       f2(i) = fa + (0.125_r8 - 0.5_r8*s*s) * (fa-fb) 
       t = ulondp(i,3)-0.5_r8
       fc = 0.5_r8 * ( f(ip(i,9),jn(i,9)) + f(ip(i,8),jn(i,8)) ) + &
            t * ( f(ip(i,9),jn(i,9)) - f(ip(i,8),jn(i,8)) )
       fd = 0.5_r8 * ( f(ip(i,10),jn(i,10)) + f(ip(i,7),jn(i,7)) ) + &
            t * ( f(ip(i,10),jn(i,10)) - f(ip(i,7),jn(i,7)) ) / 3.0_r8
       f3(i) = fc + (0.125_r8 - 0.5_r8*t*t) * (fc-fd) 
       f4(i) = f(ip(i,11),jn(i,11)) + ulondp(i,4) * &
            ( f(ip(i,12),jn(i,12)) - f(ip(i,11),jn(i,11)) )
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
       ip    , jn, &
       s1 , s2, s3, s4 , lim)
    !
    ! Interpcublin3d : quasi-cubic interpolation of a field f
    !              ( on Gaussian grid )
    !
    !
    INTEGER, INTENT(IN   ) :: ibdim
    INTEGER, INTENT(IN   ) :: iloc  (ibdim,kmax,4)
    INTEGER, INTENT(IN   ) :: jloc  (ibdim,kmax)
    INTEGER, INTENT(IN   ) :: kloc  (ibdim,kmax)
    INTEGER, INTENT(INOUT) :: jn    (ibdim,kmax,12)
    INTEGER, INTENT(INOUT) :: ip    (ibdim,kmax,12)
    LOGICAL, INTENT(IN   ) :: first
    LOGICAL, INTENT(IN   ) :: again
    LOGICAL, INTENT(IN   ) :: lim 
    REAL(KIND=r8)   , INTENT(INOUT) :: s1    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: s2    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: s3    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: s4    (ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: signal
    REAL(KIND=r8)   , INTENT(IN   ) :: ulondp(ibdim,kmax,4)
    REAL(KIND=r8)   , INTENT(IN   ) :: ulatdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: usigdp(ibdim,kmax)
    REAL(KIND=r8)   , INTENT(IN   ) :: f     (ibmax,kmax,jbmax_ext)
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
    REAL(KIND=r8)     :: sma, sma1, smi, smi1
    CHARACTER(LEN=*), PARAMETER :: h="**(Interpcublin3d)**"
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
             jn(i,km,1) = jbPerIJ(iloc(i,km,1),jp+1)
             jn(i,km,2) = jbPerIJ(iloc(i,km,1)+1,jp+1)
             jn(i,km,3) = jbPerIJ(iloc(i,km,2)-1,jp)
             jn(i,km,4) = jbPerIJ(iloc(i,km,2),jp)
             jn(i,km,5) = jbPerIJ(iloc(i,km,2)+1,jp)
             jn(i,km,6) = jbPerIJ(iloc(i,km,2)+2,jp)
             jn(i,km,7) = jbPerIJ(iloc(i,km,3)-1,jp-1)
             jn(i,km,8) = jbPerIJ(iloc(i,km,3),jp-1)
             jn(i,km,9) = jbPerIJ(iloc(i,km,3)+1,jp-1)
             jn(i,km,10) = jbPerIJ(iloc(i,km,3)+2,jp-1)
             jn(i,km,11) = jbPerIJ(iloc(i,km,4),jp-2)
             jn(i,km,12) = jbPerIJ(iloc(i,km,4)+1,jp-2)
          ENDDO
       ENDDO
       IF (ANY(ip(1:ibdim,1:kmax,1:12).eq.0)) THEN
          write(*,*) ' processor ',myid, 'will have to stop '
          write(*,*) myid, ' firstextlat =',MAX(myfirstlat-jovlap,1)
          write(*,*) myid, ' lastextlat =',MIN(mylastlat+jovlap,jmax)
          write(*,*) myid, ' myfirstlon ',myfirstlon
          write(*,*) myid, ' mylastlon ',mylastlon
          do i=1,ibdim
             do km=1,kmax
                IF (ANY(ip(i,km,1:12).eq.0)) then
          write(*,*) myid, ' ip de 1 a 12', ip(i,km,1:12)
          write(*,*) myid, ' jn de 1 a 12', jn(i,km,1:12)
                  jp = jmax+1-jloc(i,km)
                  write(*,*) 'i,k ',i,km,&
                 ' jloc, jp ',jloc(i,km),jp,'iloc',iloc(i,km,2)
                  write(*,*) 'ilocs ', iloc(i,km,1),iloc(i,km,2),&
                              iloc(i,km,3),iloc(i,km,4)
                  exit
                ENDIF
             enddo
          enddo
          WRITE(nfprt,"(a,' jovlap too small')") h
          WRITE(nfprt,"(a,' try to increase vmax_est in Options.f90 ')") h
          STOP h
       ENDIF
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
          f1(i) = s1(i,km) * ( f(ip(i,km,1),k,jn(i,km,1)) + ulondp(i,km,1) * &
                ( f(ip(i,km,2),k,jn(i,km,2)) - f(ip(i,km,1),k,jn(i,km,1)) ) )
          g1(i) = s1(i,km) * ( f(ip(i,km,1),k1,jn(i,km,1)) + ulondp(i,km,1) * &
                ( f(ip(i,km,2),k1,jn(i,km,2)) - f(ip(i,km,1),k1,jn(i,km,1)) ) )
          f4(i) = s4(i,km) * ( f(ip(i,km,11),k,jn(i,km,11)) + ulondp(i,km,4) * &
                ( f(ip(i,km,12),k,jn(i,km,12)) - f(ip(i,km,11),k,jn(i,km,11)) ) )
          g4(i) = s4(i,km) * ( f(ip(i,km,11),k1,jn(i,km,11)) + ulondp(i,km,4) * &
                ( f(ip(i,km,12),k1,jn(i,km,12)) - f(ip(i,km,11),k1,jn(i,km,11)) ) )
       ENDDO
       DO i=1,ibdim
          k = kloc(i,km)
          k1 = k+1
          s = ulondp(i,km,2)-0.5_r8
          ss = 0.125_r8 - 0.5_r8*s*s
          fa = 0.5_r8 * ( f(ip(i,km,5),k,jn(i,km,5)) + f(ip(i,km,4),k,jn(i,km,4)) ) + &
                 s * ( f(ip(i,km,5),k,jn(i,km,5)) - f(ip(i,km,4),k,jn(i,km,4)) )
          fb = 0.5_r8 * ( f(ip(i,km,6),k,jn(i,km,6)) + f(ip(i,km,3),k,jn(i,km,3)) ) + &
                 s * ( f(ip(i,km,6),k,jn(i,km,6)) - f(ip(i,km,3),k,jn(i,km,3)) ) /3.0_r8
          f2(i) = s2(i,km) * ( fa + ss * (fa-fb) ) 
          fa1 = 0.5_r8 * ( f(ip(i,km,5),k1,jn(i,km,5)) + f(ip(i,km,4),k1,jn(i,km,4)) ) + &
                 s * ( f(ip(i,km,5),k1,jn(i,km,5)) - f(ip(i,km,4),k1,jn(i,km,4)) )
          fb1 = 0.5_r8 * ( f(ip(i,km,6),k1,jn(i,km,6)) + f(ip(i,km,3),k1,jn(i,km,3)) ) + &
                 s * ( f(ip(i,km,6),k1,jn(i,km,6)) - f(ip(i,km,3),k1,jn(i,km,3)) ) / 3.0_r8
          g2(i) = s2(i,km) * ( fa1 + ss * (fa1-fb1) ) 
       ENDDO
       DO i=1,ibdim
          k = kloc(i,km)
          k1 = k+1
          t = ulondp(i,km,3)-0.5_r8
          tt = 0.125_r8 - 0.5_r8*t*t
          fc = 0.5_r8 * ( f(ip(i,km,9),k,jn(i,km,9)) + f(ip(i,km,8),k,jn(i,km,8)) ) + &
                 t * ( f(ip(i,km,9),k,jn(i,km,9)) - f(ip(i,km,8),k,jn(i,km,8)) )
          fd = 0.5_r8 * ( f(ip(i,km,10),k,jn(i,km,10)) + f(ip(i,km,7),k,jn(i,km,7)) ) + &
                 t * ( f(ip(i,km,10),k,jn(i,km,10)) - f(ip(i,km,7),k,jn(i,km,7)) ) / 3.0_r8
          f3(i) = s3(i,km) * ( fc + tt * (fc-fd) )
          fc1 = 0.5_r8 * ( f(ip(i,km,9),k1,jn(i,km,9)) + f(ip(i,km,8),k1,jn(i,km,8)) ) + &
                 t * ( f(ip(i,km,9),k1,jn(i,km,9)) - f(ip(i,km,8),k1,jn(i,km,8)) )
          fd1 = 0.5_r8 * ( f(ip(i,km,10),k1,jn(i,km,10)) + f(ip(i,km,7),k1,jn(i,km,7)) ) + &
                 t * ( f(ip(i,km,10),k1,jn(i,km,10)) - f(ip(i,km,7),k1,jn(i,km,7)) ) /3.0_r8
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
             fb = s2(i,km) * ( f(ip(i,km,4),k,jn(i,km,4)) + ulondp(i,km,2) * &
                   ( f(ip(i,km,5),k,jn(i,km,5)) - f(ip(i,km,4),k,jn(i,km,4)) ) )
             fc = s3(i,km) * ( f(ip(i,km,8),k,jn(i,km,8)) + ulondp(i,km,3) * &
                   ( f(ip(i,km,9),k,jn(i,km,9)) - f(ip(i,km,8),k,jn(i,km,8)) ) )
             fa = s2(i,km) * ( f(ip(i,km,4),k1,jn(i,km,4)) + ulondp(i,km,2) * &
                   ( f(ip(i,km,5),k1,jn(i,km,5)) - f(ip(i,km,4),k1,jn(i,km,4)) ) )
             fd = s3(i,km) * ( f(ip(i,km,8),k1,jn(i,km,8)) + ulondp(i,km,3) * &
                   ( f(ip(i,km,9),k1,jn(i,km,9)) - f(ip(i,km,8),k1,jn(i,km,8)) ) )
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
       IF (lim) THEN
          DO i=1,ibdim
             k = kloc(i,km)
             k1 = k + 1
             sma = MAX( f(ip(i,km,4),k,jn(i,km,4)), f(ip(i,km,5),k,jn(i,km,5)) , &
                        f(ip(i,km,8),k,jn(i,km,8)), f(ip(i,km,9),k,jn(i,km,9)) )
             sma1 = MAX( f(ip(i,km,4),k1,jn(i,km,4)), f(ip(i,km,5),k1,jn(i,km,5)) , &
                         f(ip(i,km,8),k1,jn(i,km,8)), f(ip(i,km,9),k1,jn(i,km,9)) )
             smi = MIN( f(ip(i,km,4),k,jn(i,km,4)), f(ip(i,km,5),k,jn(i,km,5)) , &
                        f(ip(i,km,8),k,jn(i,km,8)), f(ip(i,km,9),k,jn(i,km,9)) )
             smi1 = MIN( f(ip(i,km,4),k1,jn(i,km,4)), f(ip(i,km,5),k1,jn(i,km,5)) , &
                         f(ip(i,km,8),k1,jn(i,km,8)), f(ip(i,km,9),k1,jn(i,km,9)) )
             fint(i,km) = MIN(fint(i,km),MAX(sma,sma1))
             fint(i,km) = MAX(fint(i,km),MIN(smi,smi1))
          ENDDO
       ENDIF
    ENDDO
    !
  END SUBROUTINE Interpcublin3d

  SUBROUTINE Vectorialtend (tendu,tendv)
    !
    REAL(KIND=r8)   , INTENT(INOUT) :: tendu (ibmax,kmax,jbmax)
    REAL(KIND=r8)   , INTENT(INOUT) :: tendv (ibmax,kmax,jbmax)
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
             px = ( ym * tendu(i,k,jb) - xm * tendv(i,k,jb) ) / x2y2
             py = ( xm * tendu(i,k,jb) + ym * tendv(i,k,jb) ) / x2y2
             tendu(i,k,jb) = xp * py + yp * px
             tendv(i,k,jb) = yp * py - xp * px
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
    ALLOCATE(sigma(ibmax,1:kmax))
    ALLOCATE(delsigma(ibmax,1:kmax))
    ALLOCATE(dellon(ibmax,-1:jmax+2))
    ALLOCATE(cubcoef(ibmax,0:jmax,6))
    ALLOCATE(cubcoefv(ibmax,1:kmax,6))
    IF (SL_twotime_scheme) THEN
       ALLOCATE(ulon   (ibmax*kmax,jbmax))
       ALLOCATE(ulat   (ibmax*kmax,jbmax))
       ALLOCATE(usig   (ibmax*kmax,jbmax))
       ALLOCATE(ulon2D   (ibmax,jbmax))
       ALLOCATE(ulat2D   (ibmax,jbmax))
       ALLOCATE(fgummean  (ibmax,jbmax_ext))
       ALLOCATE(fgvmmean  (ibmax,jbmax_ext))
       ALLOCATE(fgumm     (ibmax,kmax,jbmax_ext))
       ALLOCATE(fgvmm     (ibmax,kmax,jbmax_ext))
      ELSE
       ALLOCATE(usigm  (ibmax*kmax,jbmax))
    END IF
    ALLOCATE(ulonm  (ibmax*kmax,jbmax))
    ALLOCATE(ulatm  (ibmax*kmax,jbmax))
    ALLOCATE(ulonm2D  (ibmax,jbmax))
    ALLOCATE(ulatm2D  (ibmax,jbmax))

    ALLOCATE(fgtmm     (ibmax,kmax,jbmax))
    fgtmm = 0.0_r8

    jov = (jovlap+1)/2
    IF (SL_twotime_scheme) jov = jovlap

    ALLOCATE(requests((5+nscalars)*nsend))
    ALLOCATE(requestr((10+2*nscalars)*jovlap))
    ALLOCATE(requestsm(5*nsend))
    ALLOCATE(requestrm(10*jovlap))

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
             IF (SL_twotime_scheme) THEN
                DO k=1,kmax
                   usig   (ib+(k-1)*ibmaxperJB(jb),jb) = cl(k)
                   ulonm (ib+(k-1)*ibmaxperJB(jb),jb) = (i-1)*dellon(1,j)
                   ulatm (ib+(k-1)*ibmaxperJB(jb),jb) = - phi(1,j)
                ENDDO
              ELSE
                DO k=1,kmax
                   usigm  (ib+(k-1)*ibmaxperJB(jb),jb) = cl(k)
                   ulonm (ib+(k-1)*ibmaxperJB(jb),jb) = (i-1)*dellon(1,j)
                   ulatm (ib+(k-1)*ibmaxperJB(jb),jb) = - phi(1,j)
                ENDDO
             END IF
          ENDDO
       ENDDO
       IF (SL_twotime_scheme) THEN
          ulon = ulonm
          ulat = ulatm
          ulon2D = ulonm2D
          ulat2D = ulatm2D
       ENDIF 
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
    DO k=1,kmax-1
       DO i=1,ibmax
          sigma(i,k) = cl(k)
          delsigma(i,k) = delcl(k)
       ENDDO
    ENDDO
    DO i=1,ibmax
       sigma(i,kmax) = cl(kmax)
    ENDDO

  END SUBROUTINE InitSL

END MODULE SemiLagrangian
