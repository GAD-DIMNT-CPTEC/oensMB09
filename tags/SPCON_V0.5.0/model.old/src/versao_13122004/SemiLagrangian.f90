!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE SemiLagrangian
  USE Constants, ONLY : &
       pai, &
       er

  USE Sizes,     ONLY : &
       iMax, &
       jMax, &
       kMax, &
       ibMax, &
       jbMax, &
       ibPerIJ, &
       jbPerIJ, &
       ibMaxPerJB, &
       iMaxPerJ, &
       iPerIJB, &
       jPerIJB, &
       cl,              &
       ci,              &
       del,             &
       delcl
        
  USE FieldsDynamics,    ONLY : &
       fgu,             &
       fgv,             &
       fgw,             & ! velocidade. 
       fgyum,           &
       fgyvm,           &
       fgtdm,           &
       fgqdm,           &
       fgvdlnpm,        & ! tendencias no departure
       fgum,            &
       fgvm,            &
       fgtmpm,          &
       fgqm,            &
       fglnpm,          & ! campos no departure 
       fgyu,            &
       fgyv,            &
       fgtd,            &
       fgqd,            &
       fgvdlnp            ! tendencias no middle

  USE Utils, ONLY : colrad


  IMPLICIT NONE

  INTERFACE LinearInterpolation
     MODULE PROCEDURE LinearInterpolation2DV
     MODULE PROCEDURE LinearInterpolationUV3DV
     MODULE PROCEDURE LinearInterpolationUVW3DV
  END INTERFACE

  INTERFACE ComputeWeightSL
     MODULE PROCEDURE ComputeWeightSL2DV
     MODULE PROCEDURE ComputeWeightSL3DV
  END INTERFACE

  INTERFACE GetIndex
     MODULE PROCEDURE GetIndex2DV
     MODULE PROCEDURE GetIndex2DVM
     MODULE PROCEDURE GetIndex3DV
     MODULE PROCEDURE GetIndex3DVM
  END INTERFACE

! coslat, latitudeperj, deltalatper tem a ordem  sul norte... 
!Estamos assumindo que a grade sera' simetrica em relacao ao equador. 
! Os campos acima estao na ordem sul norte (assim os valores de latitude 
! estao em ordem crescente- razao desta opcao). Para o calculo da trajetoria 
! no semi-lagrangeano nao faz diferenca se esses campos estao na
! ordem inversa pois existe essa  simetria em relacao ao equador e para a 
! interpolacao cubica requer somente os  valores relativos aos intervalos. 

! latitude: malha gaussiana definida no intervalo (-pi/2, pi/2). 
! longitude: para cada latitude fixa os pontos de malha sao igualmente 
!            espacados no intervalo [0,2 pi) 
! 
! coslat = coseno da latitude: (-pi/2, pi/2). 
!          tem dimensao = numero de latitude (jmax). 
! LatitudePerJ.  grade de latitude extendida. 
!                Tem dimensao (jmax + 4). os valores da latitude
!                estao nas posicoes 3 a jmax+2. As posicoes 1, 2 
!                contem a extensao no polo sul. As posicoes jmax+3, jmax+4 
!                contem a extensao no polo norte. 
! DeltaLatPerJ: espacamento entre os pontos de  malha de latitude extendida. 
! 
! LongitudePerIJ: tem dimensao (imax+3, jmax).  Para cada latitude
!                 contem os valores de longitude extendida. As posicoes 
!                 de longitude estao nas posicoes 2 a imax+1. posicoes 
!                 1 eh a extensao para pontos 'a esquerda e imax+2, imax+3 
!                 a extensao para a direita. Para cada latitude 
!                 a grade de longitude eh  igualmente espacada. 
! DeltaLonPerJ: tem dimensao jmax+4. O espacamento da longitude 
!                eh uniforme para cada latitude. Nas posicoes
!                3 a jmax+2  para as latitudes compreendidas no intervalo
!                (-pi/2, pi/2). posicoes 1, 2 extensao pelo polo sul, posicoes
!                jmax+3, jmax+4 extensao pelo polo norte. 

  ! Use to store location of the 3d dep

  REAL, ALLOCATABLE :: deplam(:,:,:)
  REAL, ALLOCATABLE :: depphi(:,:,:)
  REAL, ALLOCATABLE :: depsig(:,:,:)

  ! Use to store angles used in tangent plane strategy

  REAL, ALLOCATABLE :: midphi(:,:,:)
  REAL, ALLOCATABLE :: alfasl(:,:,:)
  REAL, ALLOCATABLE :: gamasl(:,:,:)

  ! Use to store location of the 2d dep

  REAL, ALLOCATABLE :: deplnplam(:,:)
  REAL, ALLOCATABLE :: deplnpphi(:,:)

  ! Use to store position j-k of the 3d dep 

  INTEGER, ALLOCATABLE :: j_dep(:,:,:)
  INTEGER, ALLOCATABLE :: k_dep(:,:,:)

  ! Use to store position  i of the 3d dep 

  INTEGER, ALLOCATABLE :: i_dep(:,:,:,:)

  ! Use to store position j  of the 2d dep 

  INTEGER, ALLOCATABLE :: j_deplnp(:,:)

  !  Use to store position i  of the 2d dep

  INTEGER, ALLOCATABLE :: i_deplnp(:,:,:)


  INTEGER, PARAMETER  :: MAXITER = 2  !max iterations for dep.
  REAL,    PARAMETER  :: EPSTOL  = 0.001 ! stop crit for dep.
  REAL,    PARAMETER  :: PI =  3.14159265358979           

  LOGICAL, SAVE ::  tanplane  !  tangent plane 

  LOGICAL, SAVE ::  project_on_layer  !  project on level or layer
  LOGICAL, SAVE ::  recompute         ! recalcula middle 

  REAL, ALLOCATABLE :: DeltaLatPerJ(:)
  REAL, ALLOCATABLE :: DeltaLonPerJ(:)
  REAL, ALLOCATABLE :: LongitudePerIJ(:,:)
  REAL, ALLOCATABLE :: LatitudePerJ(:)
  REAL, ALLOCATABLE :: CosLat(:)

  INTEGER, DIMENSION(:,:,:,:), ALLOCATABLE, PRIVATE :: KeyI
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE, PRIVATE :: KeyJ, KeyK, ClampK
  REAL, DIMENSION(:,:), ALLOCATABLE, PRIVATE :: lam
  REAL, DIMENSION(:), ALLOCATABLE, PRIVATE :: phi, dphi, z, dz, &
           dx, rdx, rdx6
  REAL, DIMENSION(:,:,:), ALLOCATABLE, PRIVATE :: wgt1z, wgt2z, wgt3z, wgt4z, &
      hb, ht, dhb, dht, rdz
  REAL, DIMENSION(:,:,:), ALLOCATABLE, PRIVATE :: lbasiy, wiz, wdz, lbasdy
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE, PRIVATE :: xl,  xr, &
    wgt1x, wgt2x, wgt3x , wgt4x, &
    hl, hr , dhl, dhr
  REAL, DIMENSION(:,:,:), ALLOCATABLE, PRIVATE :: ys, &
    yn, wgt1y, wgt2y, wgt3y, wgt4y, &
    hs, hn , dhs, dhn, rdphi

  INTEGER, DIMENSION(:,:,:,:), ALLOCATABLE, PRIVATE :: StencilIB, StencilJB
  LOGICAL, PRIVATE :: Is3D
  REAL, PRIVATE :: fac
  INTEGER, PARAMETER, PRIVATE :: IOffset = 1, JOffset = 2


CONTAINS





  SUBROUTINE  SemiLagr(dt) 

    ! This routine computes the departure and middle points (2D and 3D),
    ! interpolates the several tendencies and adds them together for the
    ! spectral semi-implicit computations 

    REAL, INTENT(IN) :: dt
    INTEGER          :: i
    INTEGER          :: j
    REAL             :: dt2
    REAL             :: aux
    REAL             :: gu(ibmax, kmax,jbmax)
    REAL             :: gv(ibmax, kmax,jbmax) !velocidade zonal/meridional
    REAL             :: interpoltmp(ibmax, kmax,jbmax)
    REAL             :: interpoltmp2(ibmax,jbmax)

    ! iPerIJB gives which longitude is stored at first dimension index
    !         i and third dimension index j of Grid representations.
    !         It is indexed iPerIJB(i,j)
    ! jPerIJB gives which latitude is stored at first dimension index
    !         i and third dimension index j of Grid representations.
    !         It is indexed jPerIJB(i,j)

    dt2 = dt + dt 

    gu = 0
    gv = 0

          ! The advecting velocities are the meridional and zonal velocities, therefore
          ! we need to divide the Robert Functions fgu and fgv by cos (latitude)
          ! recall that jperIJB gives latitude from grid representation 
          ! indexed from north to south

    DO  j = 1, jbmax  
       DO  i = 1, ibmaxperJB(j)
          aux = CosLat (jperIJB(i,j)) 
          gu (i,:,j)  =  fgu(i,:,j) / aux
          ! minus sign because of the new meridional coordinates
          gv (i,:,j)  =  - fgv(i,:,j) / aux    
       END DO
    END DO

    ! IF TANPLANE TECHNIQUE MODIFY  rhs of Momentum Equations
    !
    ! TENDENCIES @ TIME T - DT (DEPARTURE) 
    ! _____________________________________
    !
    ! add each field at time t-dt to its respective tendency at time t-dt before
    ! interpolation (m at end of the variables names stands for "minus", refering
    ! to t - dt)
    !
    ! add also the tendency at time t in order to interpolate at the departure 
    ! point (we will aproximate the tendency at time t as an average between 
    ! its values at the grid points and its values interpolated at the 
    ! departure points. dt2 *F(t, xm ) ~= 0.5*dt2*(F(t,xd) + F(t, xa))


    fgyum  =  fgum   + dt2 * fgyum  + dt*fgyu
    fgyvm  =  fgvm   + dt2 * fgyvm  + dt*fgyv
    fgtdm  =  fgtmpm + dt2 * fgtdm  + dt*fgtd
    fgqdm  =  fgqm   + dt * fgqd 
    fgvdlnpm = fglnpm + dt2 * fgvdlnpm + dt * fgvdlnp 


    ! TENDENCIES @ TIME T COMPUTED AT THE ARRIVAL POINT. 
    ! _____________________________________

    fgyu   =  dt * fgyu 
    fgyv   =  dt * fgyv 
    fgtd   =  dt * fgtd 
    fgqd   =  dt * fgqd 
    fgvdlnp = dt * fgvdlnp

    CALL Departure (gu, gv, fgw, dt)


    CALL ComputeWeights3D (deplam, depsig, depphi, &
         i_dep-1, k_dep, j_dep-2 )

    interpoltmp = fgyum
    CALL Interpolate3D (interpoltmp, fgyum, .FALSE., .FALSE.)

    interpoltmp = fgyvm
    CALL Interpolate3D (interpoltmp, fgyvm, .FALSE., .FALSE.)

    interpoltmp = fgtdm
    CALL Interpolate3D (interpoltmp, fgtdm, .FALSE., .FALSE.)

    interpoltmp = fgqdm
    CALL Interpolate3D (interpoltmp, fgqdm, .FALSE., .FALSE.)

    CALL ComputeWeights2D (deplnplam, deplnpphi, &
         i_deplnp-1, j_deplnp-2)

    interpoltmp2 = fgvdlnpm
    CALL Interpolate2D (interpoltmp2, fgvdlnpm, .FALSE.)

    ! add tendencies

    IF (tanplane)  THEN
       CALL TangentPlane(fgyum, fgyvm) 
    END IF

    fgyu = fgyum +  fgyu  
    fgyv = fgyvm +  fgyv     


    fgtd = fgtdm +  fgtd 
    fgqd = fgqdm +  fgqd  
    fgvdlnp = fgvdlnpm  + fgvdlnp      
    
  END SUBROUTINE SemiLagr



  !########################################################################
  ! DEPARTURE 
  !########################################################################



  SUBROUTINE Departure(u, v, w, dt)
    REAL, INTENT(IN) :: u(ibMax, kMax, jbMax)
    REAL, INTENT(IN) :: v(ibMax, kMax, jbMax)
    REAL, INTENT(IN) :: w(ibMax, kMax, jbMax)    
    REAL, INTENT(IN) :: dt

    ! local variables 

    INTEGER          :: k 
    REAL             :: MeanU(ibMax, jbMax)
    REAL             :: MeanV(ibMax, jbMax)

    ! compute departure point for 3D fields (U, V, T, Q) 

    CALL Dep3D(u, v, w, dt)

    ! compute integral of u and integral of v. surface pressure is 
    ! advected with mean velocity 

    MeanU = 0.0
    MeanV = 0.0
    DO k = 1, kmax 
       MeanU =  MeanU + u(:,k,:) * del(k)
       MeanV =  MeanV + v(:,k,:) * del(k)
    END DO

    CALL Dep2D(MeanU, MeanV, dt) 

  END SUBROUTINE Departure


  !#######################################################################
  ! DEP 3D 
  !########################################################################


  SUBROUTINE Dep3D(u, v, w, dt)
    IMPLICIT NONE
    REAL, INTENT(IN) :: u(ibmax,  kmax, jbmax)
    REAL, INTENT(IN) :: v(ibmax,  kmax, jbmax)
    REAL, INTENT(IN) :: w(ibmax,  kmax, jbmax)
    REAL, INTENT(IN) :: dt
    LOGICAL          :: Done(ibmax)
    LOGICAL          :: Donelocal(ibmax)
    INTEGER          :: i
    INTEGER          :: j
    INTEGER          :: k
    INTEGER          :: cnt
    INTEGER          :: ib
    INTEGER          :: jb
    INTEGER          :: id(ibmax,4) 
    INTEGER          :: jd(ibmax)
    INTEGER          :: kd(ibmax)
    REAL             :: lama
    REAL             :: phia
    REAL             :: siga(ibmax) !arrival, middle and departure points
    REAL             :: lamm(ibmax)
    REAL             :: phim(ibmax)
    REAL             :: sigm(ibmax) !in polar coordinates
    REAL             :: lamd(ibmax)
    REAL             :: phid(ibmax)
    REAL             :: sigd(ibmax)
    REAL             :: lamm_old(ibmax)
    REAL             :: phim_old(ibmax)
    REAL             :: sigm_old(ibmax)
    REAL             :: xa(ibmax)
    REAL             :: ya(ibmax)
    REAL             :: za(ibmax) !arrival, middle and departure points
    REAL             :: xm(ibmax)
    REAL             :: ym(ibmax)
    REAL             :: zm(ibmax) !in cartesian coordinates
    REAL             :: xd(ibmax)  ! nao sei isto eh preciso!!!!!!!
    REAL             :: yd(ibmax)  ! nao sei isto eh preciso!!!!!!!
    REAL             :: zd(ibmax)  ! nao sei isto eh preciso!!!!!!!
    REAL             :: wx
    REAL             :: wy
    REAL             :: wz !wind in cartesian system
    REAL             :: um(ibmax)
    REAL             :: vm(ibmax)
    REAL             :: wm(ibmax) !zonal, meridional and vertical velocity 
    REAL             :: b !rescaling factor
    REAL             :: dotprod ! temporary for dot product
    REAL             :: aux1
    REAL             :: aux2
    REAL             :: aux3 !temporaries for the stop condition

    DO jb = 1, jbmax 

       DO  k = 1, kmax 

          DO ib = 1, ibmaxperJB(jb)

             !  longitude and latitude index 

             i = iperIJB(ib,jb) 
             j = jperIJB(ib,jb)

             ! set arrival point   (can move to outside k loop)

             lama = LongitudePerIJ(i+1,j)     !long. pos. is offset by 1 
             phia = LatitudePerJ(j+2)         !lat.  pos. is offset by 2
             siga(ib) = cl(k) 

             !arrival point in cartesian coordinates   (can move this outside k loop)

             xa(ib) = COS(lama)  * COS(phia)        
             ya(ib) = SIN(lama)  * COS(phia)
             za(ib) = SIN(phia)

             !first  iteration.  Start with middle point == arrival point       

             lamm(ib) = lama 
             phim(ib) = phia 
             sigm(ib) = siga(ib)

             !before updating points, save old values

             lamm_old(ib)  = lamm(ib) 
             phim_old(ib)  = phim(ib)
             sigm_old(ib)  = sigm(ib)

             ! vertical velocity of first iteration is known
             wm(ib) = w(ib, k, jb) 

             Done(ib) = .FALSE.

          END DO


          ! iterate where necessary


          DO cnt = 1, MAXITER

             ! correct in the vertical direction  


             DO ib = 1, ibmaxperJB(jb)

                IF (.NOT. Done(ib)) THEN

                   IF (project_on_layer) THEN 
                      sigm(ib) = MIN(cl(kmax), MAX(siga(ib) - dt * wm(ib), cl(1)))
                   ELSE
                      sigm(ib) = MIN(1.0d0, MAX(siga(ib) - dt * wm(ib), 0.0d0))
                   END IF

                END IF
             END DO

             ! correct in the horizontal direction.  compute lamm, phim as in 2D case
             ! update horiz. velocity: (um, vm) = f(u(:,:,:), v(:,:,:), lamm, phim, sigm)


             CALL LinearInterpolation (um, vm, u, v, lamm, phim, sigm, Done, ibmaxperJB(jb))

             ! perform implicit euler step from arrival to middle
             ! compute angles

             DO ib = 1, ibmaxperJB(jb)

                IF (.NOT. Done(ib)) THEN
                   wx = (- um(ib) * SIN(lamm(ib)) - vm(ib) * COS(lamm(ib)) * SIN(phim(ib))) / er
                   wy = (  um(ib) * COS(lamm(ib)) - vm(ib) * SIN(lamm(ib)) * SIN(phim(ib))) / er
                   wz = (  vm(ib) * COS(phim(ib))) / er 
                   b  = 1.0 / SQRT(1.0 + dt**2 * (wx**2 + wy**2 + wz**2) - &
                     2 * dt * (wx * xa(ib) + wy * ya(ib) + wz * za(ib)))

                   xm(ib) = b * (xa(ib) - dt * wx)
                   ym(ib) = b * (ya(ib) - dt * wy)
                   zm(ib) = b * (za(ib) - dt * wz)

                   lamm(ib) = ATAN2(ym(ib), xm(ib))
                   IF ( lamm(ib) < 0 ) lamm(ib) = lamm(ib) + 2 * PI  ! recall atan2 is in [-pi.pi]
                   phim(ib) = ASIN(zm(ib))            

                END IF

             END DO

             ! update  velocity             

             CALL LinearInterpolation (um, vm, wm, u, v, w, lamm, phim, sigm, Done, ibmaxperJB(jb))


             ! stopping criteria

             DO ib = 1, ibmaxperJB(jb)

                IF (.NOT. Done(ib)) THEN
                   aux1 = ABS(lamm(ib) - lamm_old(ib)) / 2 * PI
                   aux2 = ABS(phim(ib) - phim_old(ib)) / PI 
                   aux3 = ABS(sigm(ib) - sigm_old(ib)) 
                   Done(ib) = aux1 <= EPSTOL .AND. aux2 <= EPSTOL .AND. aux3 <= EPSTOL
                END IF

                IF (.NOT. Done(ib)) THEN
                   lamm_old(ib) = lamm(ib)
                   phim_old(ib) = phim(ib)
                   sigm_old(ib) = sigm(ib)
                END IF

             END DO
             
          END DO


          cnt = COUNT(.NOT. Done(1:ibmaxperJB(jb)))

!          IF (cnt > 0) THEN
!             PRINT * , "Max iterations exceeded at ", cnt," points at layer ",k
!          END IF

          ! find departure point from middle and arrival        
          ! in the horizontal points are over a great circle

          donelocal = .true.  ! variable used to check if we'll recompute
                              ! velocity at the middle point 
          
          DO ib = 1, ibmaxperJB(jb)

             dotprod = xa(ib) * xm(ib) + ya(ib) * ym(ib) + za(ib) * zm (ib) 
             xd(ib) = 2 * dotprod * xm(ib) - xa(ib)
             yd(ib) = 2 * dotprod * ym(ib) - ya(ib)
             zd(ib) = MIN(1.0, MAX(-1.0, 2 * dotprod * zm(ib) - za(ib)))
             lamd(ib) = ATAN2(yd(ib), xd(ib))
             IF (lamd(ib) < 0 ) lamd(ib) = lamd(ib) + 2 * PI 
             phid(ib) = ASIN(zd(ib))
             
             ! in the vertical, correction is over a straight line 
             
             sigd(ib) = siga(ib) - 2 * dt * wm(ib)  

             if (project_on_layer) then 
                sigd(ib) = min(cl(kmax), max(sigd(ib), cl(1)))
             else
                sigd(ib) = min(1.0d0, max(sigd(ib), 0.0d0))
             end if
            
             ! save departure points and its positions
             
             deplam(ib,k, jb) = lamd(ib)
             depphi(ib,k, jb) = phid(ib)
             depsig(ib,k, jb) = sigd(ib)

             IF (recompute .AND. &
                ((sigd(ib) .eq. 1.0d0) .OR. (sigd(ib) .eq. 0.0d0))) THEN  
! if sig departure is at top level, we must recompute middle and the mid vel 
                dotprod = xa(ib) * xd(ib) + ya(ib) * yd(ib) + za(ib) * zd(ib)  
                xm(ib) = (xd(ib) +xa(ib))/dotprod  
                ym(ib) = (yd(ib) +ya(ib))/dotprod     
                zm(ib) = (zd(ib) +za(ib))/dotprod      
                lamm(ib) = atan2(ym(ib), xm(ib))
                if (lamm(ib) < 0 ) lamm(ib) = lamm(ib) + 2.0d0 * PI 
                phim(ib) = asin(zm(ib))
                sigm(ib) = (siga(ib) + sigd(ib))/2.0d0 
                donelocal(ib) = .false. 
             END IF

          END DO

          CALL GetIndex(id, jd, kd, lamd, phid, sigd, ibmaxperJB(jb))

! update vel at middle if  "recompute" is true. we'll update the vel
! only for the middle points that we actually recomputed (sigd.eq.1) - 
! see logical variable donelocal 

          if (recompute) &
             CALL LinearInterpolation (um, vm, wm, u, v, w, lamm, phim, sigm,&
                  DoneLocal, ibmaxperJB(jb))

          DO ib = 1, ibmaxperJB(jb)
             i_dep(ib, 1, k, jb)  = id(ib,1) 
             i_dep(ib, 2, k, jb)  = id(ib,2) 
             i_dep(ib, 3, k, jb)  = id(ib,3) 
             i_dep(ib, 4, k, jb)  = id(ib,4) 
             j_dep(ib,k, jb)  = jd(ib)
             k_dep(ib,k, jb)  = kd(ib)
             
             IF (tanplane) THEN  ! case using tangent plane 
                
                ! save mid point because of minus lat.cg sign phim 
                ! save alfasl, gamasl
                
                midphi(ib,k, jb) = -phim(ib)  
                alfasl(ib,k,jb) = SQRT(um(ib) **2 + vm(ib) ** 2) * dt / er 
                gamasl(ib,k,jb) = ATAN2(-vm(ib),um(ib))
                IF ( gamasl(ib,k,jb) < 0 )  &
                     gamasl(ib,k,jb) = gamasl(ib,k,jb) + 2*PI
             END IF

          END DO
       END DO
    END DO
  END SUBROUTINE Dep3D


  SUBROUTINE GetIndex3DV (i, j, k, lam, pphi, sig, ibend)
    INTEGER, INTENT(OUT) :: i(ibmax,4)
    INTEGER, INTENT(OUT) :: j(ibmax)
    INTEGER, INTENT(OUT) :: k(ibmax) 
    REAL,    INTENT(IN ) :: lam(ibmax)
    REAL,    INTENT(IN ) :: pphi(ibmax)
    REAL,    INTENT(IN ) :: sig(ibmax)
    INTEGER, INTENT(IN ) :: ibend
    INTEGER              :: ii
    INTEGER              :: jj
    INTEGER              :: kk
    LOGICAL              :: Done(ibmax)

    Done = .FALSE.
    j = jmax+3

    DO jj = 2, jmax+2 
       DO ii = 1, ibend
          IF (.NOT. Done(ii)) THEN
             IF (LatitudePerJ(jj+1) > pphi(ii)) THEN
                Done(ii) = .TRUE.
                j(ii) = jj
             END IF
          END IF
       END DO
    END DO

    DO ii = 1, ibend
       i(ii,1) = INT(lam(ii)/DeltaLonPerJ(j(ii)-1)) +2
       i(ii,2) = INT(lam(ii)/DeltaLonPerJ(j(ii)))   +2
       i(ii,3) = INT(lam(ii)/DeltaLonPerJ(j(ii)+1)) +2
       i(ii,4) = INT(lam(ii)/DeltaLonPerJ(j(ii)+2)) +2
    END DO

!find layer
    Done = .FALSE.
    k = kmax
    DO kk = 1, kmax - 1  
       DO ii = 1, ibend
          IF (.NOT. Done(ii)) THEN
             IF (cl(kk+1) > sig(ii)) THEN
                Done(ii) = .TRUE.
                k(ii) = kk
             END IF
          END IF
       END DO
    END DO

    DO ii = 1, ibend
       IF( k(ii) == kmax)  &
            k(ii) = kmax - 1 ! if above last layer, save penultimate
                                  !  layer for extrapolation
    END DO

  END SUBROUTINE GetIndex3DV

  SUBROUTINE GetIndex3DVM (i, j, k, lam, pphi, sig, Done, ibend)
    INTEGER, INTENT(OUT) :: i(ibmax,4)
    INTEGER, INTENT(OUT) :: j(ibmax)
    INTEGER, INTENT(OUT) :: k(ibmax) 
    REAL,    INTENT(IN ) :: lam(ibmax)
    REAL,    INTENT(IN ) :: pphi(ibmax)
    REAL,    INTENT(IN ) :: sig(ibmax)
    LOGICAL, INTENT(IN ) :: Done(ibmax)
    INTEGER, INTENT(IN ) :: ibend
    INTEGER              :: ii
    INTEGER              :: jj
    INTEGER              :: kk
    LOGICAL              :: DoneLocal(ibmax)

    DoneLocal = .FALSE.
    j = jmax+3

    DO jj = 2, jmax+2 
       DO ii = 1, ibend
          IF ((.NOT. Done(ii)) .AND. (.NOT. DoneLocal(ii)) .AND. (LatitudePerJ(jj+1) > pphi(ii))) THEN
             DoneLocal(ii) = .TRUE.
             j(ii) = jj
          END IF
       END DO
    END DO

    DO ii = 1, ibend
       IF (.NOT. Done(ii)) THEN
          i(ii,1) = INT(lam(ii)/DeltaLonPerJ(j(ii)-1)) +2
          i(ii,2) = INT(lam(ii)/DeltaLonPerJ(j(ii)))   +2
          i(ii,3) = INT(lam(ii)/DeltaLonPerJ(j(ii)+1)) +2
          i(ii,4) = INT(lam(ii)/DeltaLonPerJ(j(ii)+2)) +2
       END IF
    END DO

    DoneLocal = .FALSE.
    k = kmax
    DO kk = 1, kmax - 1  
       DO ii = 1, ibend
          IF ((.NOT. Done(ii)) .AND. (.NOT. DoneLocal(ii)) .AND. (cl(kk+1) > sig(ii))) THEN
             DoneLocal(ii) = .TRUE.
             k(ii) = kk
          END IF
       END DO
    END DO
    
    DO ii = 1, ibend
       IF (k(ii) == kmax .and. (.not. done(ii))) &
            k(ii) = kmax-1
    END DO

  END SUBROUTINE GetIndex3DVM


  SUBROUTINE GetIndex2DV (i, j, lam, pphi, ibend)
    INTEGER, INTENT(OUT) :: i(ibmax,4)
    INTEGER, INTENT(OUT) :: j(ibmax)
    REAL,    INTENT(IN ) :: lam(ibmax)
    REAL,    INTENT(IN ) :: pphi(ibmax)
    INTEGER, INTENT(IN ) :: ibend
    INTEGER              :: ii
    INTEGER              :: jj
    LOGICAL              :: Done(ibmax)

    Done = .FALSE.
    j = jmax+3
    DO jj = 2, jmax+2
       DO ii = 1, ibend
          IF (.NOT. Done(ii)) THEN
             IF (LatitudePerJ(jj+1) > pphi(ii)) THEN
                Done(ii) = .TRUE.
                j(ii) = jj
             END IF
          END IF
       END DO
    END DO

    DO ii = 1, ibend
       i(ii,1)  = INT ( lam(ii) /DeltaLonPerJ(j(ii)-1)) + 2
       i(ii,2)  = INT ( lam(ii) /DeltaLonPerJ(j(ii)))   + 2
       i(ii,3)  = INT ( lam(ii) /DeltaLonPerJ(j(ii)+1)) + 2
       i(ii,4)  = INT ( lam(ii) /DeltaLonPerJ(j(ii)+2)) + 2
    END DO
  END SUBROUTINE GetIndex2DV


  SUBROUTINE GetIndex2DVM (i, j, lam, pphi, Done, ibend)
    INTEGER, INTENT(OUT) :: i(ibmax,4)
    INTEGER, INTENT(OUT) :: j(ibmax)
    REAL,    INTENT(IN ) :: lam(ibmax)
    REAL,    INTENT(IN ) :: pphi(ibmax)
    LOGICAL, INTENT(IN ) :: Done(ibmax)
    INTEGER, INTENT(IN ) :: ibend
    INTEGER              :: ii
    INTEGER              :: jj
    LOGICAL              :: DoneLocal(ibmax)

    DoneLocal = .FALSE.
    j = jmax+3
    DO jj = 2, jmax+2
       DO ii = 1, ibend
          IF ((.NOT. Done(ii)) .AND. (.NOT. DoneLocal(ii)) .AND. (LatitudePerJ(jj+1) > pphi(ii))) THEN
             DoneLocal(ii) = .TRUE.
             j(ii) = jj
          END IF
       END DO
    END DO

    DO ii = 1, ibend
       IF (.NOT. Done(ii)) THEN
          i(ii,1)  = INT ( lam(ii) /DeltaLonPerJ(j(ii)-1)) + 2
          i(ii,2)  = INT ( lam(ii) /DeltaLonPerJ(j(ii)))   + 2
          i(ii,3)  = INT ( lam(ii) /DeltaLonPerJ(j(ii)+1)) + 2
          i(ii,4)  = INT ( lam(ii) /DeltaLonPerJ(j(ii)+2)) + 2
       END IF
    END DO
  END SUBROUTINE GetIndex2DVM



  SUBROUTINE LinearInterpolation2DV(um, vm, u, v, lam, pphi, Done, ibend)
    REAL,    INTENT(OUT) :: um(ibmax)
    REAL,    INTENT(OUT) :: vm(ibmax)
    REAL,    INTENT(IN ) :: u(ibMax, jbMax)
    REAL,    INTENT(IN ) :: v(ibMax, jbMax)
    REAL,    INTENT(IN ) :: lam(ibmax)
    REAL,    INTENT(IN ) :: pphi(ibmax)
    LOGICAL, INTENT(IN ) :: Done(ibmax)
    INTEGER, INTENT(IN ) :: ibend
    INTEGER              :: ib
    INTEGER              :: ibjbdx(ibmax,4,2)
    INTEGER              :: flag(ibmax)
    REAL                 :: wgts(ibmax,4) 

     CALL ComputeWeightSL(ibjbdx, wgts, lam, pphi, Done, ibend, flag)

     DO ib = 1, ibend
        IF (.NOT. Done(ib)) THEN
          IF (flag(ib) == 1)  THEN 
             wgts(ib,1) = -wgts(ib,1)  ! simpler way to change the sign of velocity at north pole
             wgts(ib,2) = -wgts(ib,2)  
          ELSE IF (flag(ib) == 2) THEN 
             wgts(ib,3) = -wgts(ib,3)  ! simpler way to change the sign of velocity at south pole
             wgts(ib,4) = -wgts(ib,4)  
          END IF

          um(ib) =                                                 &
               wgts(ib,1) * u (ibjbdx(ib,1,1),ibjbdx(ib,1,2))   +  &
               wgts(ib,2) * u (ibjbdx(ib,2,1),ibjbdx(ib,2,2))   +  &
               wgts(ib,3) * u (ibjbdx(ib,3,1),ibjbdx(ib,3,2))   +  & 
               wgts(ib,4) * u (ibjbdx(ib,4,1),ibjbdx(ib,4,2))
          
          vm(ib) =                                                 &
               wgts(ib,1) * v (ibjbdx(ib,1,1),ibjbdx(ib,1,2))   +  &
               wgts(ib,2) * v (ibjbdx(ib,2,1),ibjbdx(ib,2,2))   +  &
               wgts(ib,3) * v (ibjbdx(ib,3,1),ibjbdx(ib,3,2))   +  & 
               wgts(ib,4) * v (ibjbdx(ib,4,1),ibjbdx(ib,4,2))
       END IF
    END DO
  END SUBROUTINE LinearInterpolation2DV


  SUBROUTINE LinearInterpolationUVW3DV (um, vm, wm, u, v, w, lam, pphi, sig, Done, ibend)

    ! vertices of the cube where point (lam, pphi, sig) belongs are 
    ! ordered counter-clockwise and from bottom to top. 
    ! points 1,2,3,4 refers to (ill,jl,k) (ilr,jl,k) (iur,ju,k) (iul,ju,k) 
    ! points 5,6,7,8 refers to (ill,jl,k+1) (ilr,jl,k+1) (iur,ju,k+1) (iul,ju,k+1) 

    REAL,    INTENT(OUT) :: um(ibmax)
    REAL,    INTENT(OUT) :: vm(ibmax)
    REAL,    INTENT(OUT) :: wm(ibmax)
    REAL,    INTENT(IN ) :: u(ibmax, kmax, jbmax)
    REAL,    INTENT(IN ) :: v(ibmax, kmax, jbmax)
    REAL,    INTENT(IN ) :: w(ibmax, kmax, jbmax)
    REAL,    INTENT(IN ) :: lam(ibmax)
    REAL,    INTENT(IN ) :: pphi(ibmax)
    REAL,    INTENT(IN ) :: sig(ibmax)
    LOGICAL, INTENT(IN ) :: Done(ibmax)
    INTEGER, INTENT(IN ) :: ibend
    INTEGER              :: ib
    INTEGER              :: ibjbkdx(ibmax,8,3)
    REAL                 :: wgts(ibmax,8)
    integer              :: flag(ibmax) 


    CALL ComputeWeightSL(ibjbkdx, wgts, lam, pphi, sig, Done, ibend, flag)

    DO ib = 1, ibend
       IF (.NOT. Done(ib)) THEN

          wm(ib) = &
               wgts(ib,1) * w(ibjbkdx(ib,1,1), ibjbkdx(ib,1,2), ibjbkdx(ib,1,3)) + &
               wgts(ib,2) * w(ibjbkdx(ib,2,1), ibjbkdx(ib,2,2), ibjbkdx(ib,2,3)) + &
               wgts(ib,3) * w(ibjbkdx(ib,3,1), ibjbkdx(ib,3,2), ibjbkdx(ib,3,3)) + &
               wgts(ib,4) * w(ibjbkdx(ib,4,1), ibjbkdx(ib,4,2), ibjbkdx(ib,4,3)) + &
               wgts(ib,5) * w(ibjbkdx(ib,5,1), ibjbkdx(ib,5,2), ibjbkdx(ib,5,3)) + &
               wgts(ib,6) * w(ibjbkdx(ib,6,1), ibjbkdx(ib,6,2), ibjbkdx(ib,6,3)) + &
               wgts(ib,7) * w(ibjbkdx(ib,7,1), ibjbkdx(ib,7,2), ibjbkdx(ib,7,3)) + &
               wgts(ib,8) * w(ibjbkdx(ib,8,1), ibjbkdx(ib,8,2), ibjbkdx(ib,8,3))
    ! if mid point cross any pole change sign of velocity before interpolation 

          IF (flag(ib)  == 1) THEN 
             wgts(ib,1) = -wgts(ib,1)  ! simpler way to change the sign of velocity at north pole
             wgts(ib,2) = -wgts(ib,2)  
             wgts(ib,5) = -wgts(ib,5)  
             wgts(ib,6) = -wgts(ib,6)  
          ELSE IF (flag(ib)  == 2) THEN
             wgts(ib,3) = -wgts(ib,3)  ! simpler way to change the sign of velocity
             wgts(ib,4) = -wgts(ib,4)  
             wgts(ib,7) = -wgts(ib,7)  
             wgts(ib,8) = -wgts(ib,8)  
          END IF


          
          um(ib) = &
               wgts(ib,1) * u(ibjbkdx(ib,1,1), ibjbkdx(ib,1,2), ibjbkdx(ib,1,3)) + &
               wgts(ib,2) * u(ibjbkdx(ib,2,1), ibjbkdx(ib,2,2), ibjbkdx(ib,2,3)) + &
               wgts(ib,3) * u(ibjbkdx(ib,3,1), ibjbkdx(ib,3,2), ibjbkdx(ib,3,3)) + &
               wgts(ib,4) * u(ibjbkdx(ib,4,1), ibjbkdx(ib,4,2), ibjbkdx(ib,4,3)) + &
               wgts(ib,5) * u(ibjbkdx(ib,5,1), ibjbkdx(ib,5,2), ibjbkdx(ib,5,3)) + &
               wgts(ib,6) * u(ibjbkdx(ib,6,1), ibjbkdx(ib,6,2), ibjbkdx(ib,6,3)) + &
               wgts(ib,7) * u(ibjbkdx(ib,7,1), ibjbkdx(ib,7,2), ibjbkdx(ib,7,3)) + &
               wgts(ib,8) * u(ibjbkdx(ib,8,1), ibjbkdx(ib,8,2), ibjbkdx(ib,8,3))
          
          vm(ib) = &
               wgts(ib,1) * v(ibjbkdx(ib,1,1), ibjbkdx(ib,1,2), ibjbkdx(ib,1,3)) + &
               wgts(ib,2) * v(ibjbkdx(ib,2,1), ibjbkdx(ib,2,2), ibjbkdx(ib,2,3)) + &
               wgts(ib,3) * v(ibjbkdx(ib,3,1), ibjbkdx(ib,3,2), ibjbkdx(ib,3,3)) + &
               wgts(ib,4) * v(ibjbkdx(ib,4,1), ibjbkdx(ib,4,2), ibjbkdx(ib,4,3)) + &
               wgts(ib,5) * v(ibjbkdx(ib,5,1), ibjbkdx(ib,5,2), ibjbkdx(ib,5,3)) + &
               wgts(ib,6) * v(ibjbkdx(ib,6,1), ibjbkdx(ib,6,2), ibjbkdx(ib,6,3)) + &
               wgts(ib,7) * v(ibjbkdx(ib,7,1), ibjbkdx(ib,7,2), ibjbkdx(ib,7,3)) + &
               wgts(ib,8) * v(ibjbkdx(ib,8,1), ibjbkdx(ib,8,2), ibjbkdx(ib,8,3))
       END IF
    END DO
  END SUBROUTINE LinearInterpolationUVW3DV


  SUBROUTINE LinearInterpolationUV3DV (um , vm, u, v, lam, pphi, sig, Done, ibend)

    ! vertices of the cube where point (lam, pphi, sig) belongs are 
    ! ordered counter-clockwise and from bottom to top. 
    ! points 1,2,3,4 refers to (ill,jl,k) (ilr,jl,k) (iur,ju,k) (iul,ju,k) 
    ! points 5,6,7,8 refers to (ill,jl,k+1) (ilr,jl,k+1) (iur,ju,k+1) (iul,ju,k+1) 

    REAL,    INTENT(OUT) :: um(ibmax)
    REAL,    INTENT(OUT) :: vm(ibmax)
    REAL,    INTENT(IN ) :: u(ibmax, kmax, jbmax)
    REAL,    INTENT(IN ) :: v(ibmax, kmax, jbmax)
    REAL,    INTENT(IN ) :: lam(ibmax)
    REAL,    INTENT(IN ) :: pphi(ibmax)
    REAL,    INTENT(IN ) :: sig(ibmax)
    LOGICAL, INTENT(IN ) :: Done(ibmax)
    INTEGER, INTENT(IN ) :: ibend
    INTEGER              :: ib
    INTEGER              :: ibjbkdx(ibmax,8,3)
    REAL                 :: wgts(ibmax,8)
    INTEGER              :: flag(ibmax)

    CALL ComputeWeightSL(ibjbkdx, wgts, lam, pphi, sig, Done, ibend, flag)

    ! if mid point cross any pole change sign of velocity before interpolation 

    DO ib = 1, ibend
       IF (.NOT. Done(ib)) THEN
          IF (flag(ib) == 1) THEN 
             wgts(ib,1) = -wgts(ib,1)  ! simpler way to change the sign of velocity at north pole
             wgts(ib,2) = -wgts(ib,2)  
             wgts(ib,5) = -wgts(ib,5)  
             wgts(ib,6) = -wgts(ib,6)  
          ELSE IF (flag(ib) == 2) THEN 
             wgts(ib,3) = -wgts(ib,3)  ! simpler way to change the sign of velocity at south pole
             wgts(ib,4) = -wgts(ib,4)  
             wgts(ib,7) = -wgts(ib,7)  
             wgts(ib,8) = -wgts(ib,8)  
          END IF

          um(ib) = &
               wgts(ib,1) * u(ibjbkdx(ib,1,1), ibjbkdx(ib,1,2), ibjbkdx(ib,1,3)) + &
               wgts(ib,2) * u(ibjbkdx(ib,2,1), ibjbkdx(ib,2,2), ibjbkdx(ib,2,3)) + &
               wgts(ib,3) * u(ibjbkdx(ib,3,1), ibjbkdx(ib,3,2), ibjbkdx(ib,3,3)) + &
               wgts(ib,4) * u(ibjbkdx(ib,4,1), ibjbkdx(ib,4,2), ibjbkdx(ib,4,3)) + &
               wgts(ib,5) * u(ibjbkdx(ib,5,1), ibjbkdx(ib,5,2), ibjbkdx(ib,5,3)) + &
               wgts(ib,6) * u(ibjbkdx(ib,6,1), ibjbkdx(ib,6,2), ibjbkdx(ib,6,3)) + &
               wgts(ib,7) * u(ibjbkdx(ib,7,1), ibjbkdx(ib,7,2), ibjbkdx(ib,7,3)) + &
               wgts(ib,8) * u(ibjbkdx(ib,8,1), ibjbkdx(ib,8,2), ibjbkdx(ib,8,3))
          
          
          
          vm(ib) = &
               wgts(ib,1) * v(ibjbkdx(ib,1,1), ibjbkdx(ib,1,2), ibjbkdx(ib,1,3)) + &
               wgts(ib,2) * v(ibjbkdx(ib,2,1), ibjbkdx(ib,2,2), ibjbkdx(ib,2,3)) + &
               wgts(ib,3) * v(ibjbkdx(ib,3,1), ibjbkdx(ib,3,2), ibjbkdx(ib,3,3)) + &
               wgts(ib,4) * v(ibjbkdx(ib,4,1), ibjbkdx(ib,4,2), ibjbkdx(ib,4,3)) + &
               wgts(ib,5) * v(ibjbkdx(ib,5,1), ibjbkdx(ib,5,2), ibjbkdx(ib,5,3)) + &
               wgts(ib,6) * v(ibjbkdx(ib,6,1), ibjbkdx(ib,6,2), ibjbkdx(ib,6,3)) + &
               wgts(ib,7) * v(ibjbkdx(ib,7,1), ibjbkdx(ib,7,2), ibjbkdx(ib,7,3)) + &
               wgts(ib,8) * v(ibjbkdx(ib,8,1), ibjbkdx(ib,8,2), ibjbkdx(ib,8,3))
       END IF
    END DO
  END SUBROUTINE LinearInterpolationUV3DV




  SUBROUTINE ComputeWeightSL2DV(ibjbdx, wgts, lam, pphi, Done, ibend, flag) 
    INTEGER, INTENT(OUT) :: ibjbdx(ibmax,4,2)
    REAL,    INTENT(OUT) :: wgts(ibmax,4)
    REAL,    INTENT(IN ) :: lam(ibmax)
    REAL,    INTENT(IN ) :: pphi(ibmax) 
    INTEGER, INTENT(OUT) :: flag(ibmax)
    LOGICAL, INTENT(IN ) :: Done(ibmax)
    INTEGER, INTENT(IN ) :: ibend
    INTEGER              :: ib
    INTEGER              :: ill
    INTEGER              :: ilr
    INTEGER              :: iul
    INTEGER              :: iur
    INTEGER              :: jl
    INTEGER              :: ju 
    INTEGER              :: i(ibmax,4)
    INTEGER              :: j(ibmax) 
    REAL                 :: d(3) 



    flag = 0 
     CALL GetIndex(i, j, lam, pphi, Done, ibend)

    ! use index of grid cell containing middle point to 
    ! interpolate wind for next iteration

    DO ib = 1, ibend
       IF (.NOT. Done(ib)) THEN
          jl = j(ib)
          ju = j(ib)+1

          ! if above last or below first, use two pairs
          ! from the same latitude

          IF (jl == 2) THEN
             iul = i(ib,3)
             iur = i(ib,3)+1
             ill = iul + imaxperj(1) / 2
             ilr = iur + imaxperj(1) / 2

             ! normalized distance to left longitude at lower and upper latitude 

             d(3) = (lam(ib) - LongitudePerIJ(iul,ju-2)) / DeltaLonPerJ(ju)
             d(1) = d(3) 

             ! normalized distance to lower latitude 

             d(2) = (pphi(ib) - LatitudePerJ(jl) ) / DeltaLatPerJ(jl)
             jl = 3
             flag(ib) = 1 
          ELSE IF (ju > jmax+2) THEN
             ill = i(ib,2)
             ilr = i(ib,2)  + 1
             iul = ill + imaxperj(jmax) / 2
             iur = ilr + imaxperj(jmax) / 2

             ! normalized distance to left longitude at lower and upper latitude 

             d(1) = (lam(ib) - LongitudePerIJ(ill,jl-2)) / DeltaLonPerJ(jl)
             d(3) = d(1) 

             ! normalized distance to lower latitude 

             d(2) = (pphi(ib) - LatitudePerJ(jl)) / DeltaLatPerJ(jl) 
             ju = jmax + 2
             flag(ib) = 2 
          ELSE 
             ill = i(ib,2)
             ilr = i(ib,2) + 1
             iul = i(ib,3)
             iur = i(ib,3) + 1

             ! normalized distance to left longitude at lower and upper latitude 

             d(1) = (lam(ib) - LongitudePerIJ(ill,jl-2)) / DeltaLonPerJ(jl)
             d(3) = (lam(ib) - LongitudePerIJ(iul,ju-2)) / DeltaLonPerJ(ju)

             ! normalized distance to lower latitude 

             d(2) = (pphi(ib) - LatitudePerJ(jl)) / DeltaLatPerJ(jl)
          END IF

          IF (ill > imaxperj(jl-2)+1) ill = ill - imaxperj(jl-2)
          IF (ilr > imaxperj(jl-2)+1) ilr = ilr - imaxperj(jl-2)
          IF (iul > imaxperj(ju-2)+1) iul = iul - imaxperj(ju-2)
          IF (iur > imaxperj(ju-2)+1) iur = iur - imaxperj(ju-2)

          ! compute weights. points are numbered counterclockwise - lower left first is
          ! the first point

          wgts(ib,1) = (1- d(1)) * (1-d(2)) 
          wgts(ib,2) =  d(1)     * (1- d(2)) 
          wgts(ib,3) =  d(3)     * d(2) 
          wgts(ib,4) = (1-d(3))  * d(2) 

          ! ib jb indexing of points ...  
          
          ibjbdx(ib,1,1)  = ibperij(ill-1,jl-2)
          ibjbdx(ib,2,1)  = ibperij(ilr-1,jl-2)
          ibjbdx(ib,4,1)  = ibperij(iul-1,ju-2)
          ibjbdx(ib,3,1)  = ibperij(iur-1,ju-2)

          ibjbdx(ib,1,2)  = jbperij(ill-1,jl-2)
          ibjbdx(ib,2,2)  = jbperij(ilr-1,jl-2)
          ibjbdx(ib,4,2)  = jbperij(iul-1,ju-2)
          ibjbdx(ib,3,2)  = jbperij(iur-1,ju-2)

       END IF
    END DO

  END SUBROUTINE ComputeWeightSL2DV


  SUBROUTINE ComputeWeightSL3DV (ibjbkdx, wgts, lam, pphi, sig, Done, ibend,flag)

    INTEGER, INTENT(OUT) :: ibjbkdx(ibmax,8,3)
    REAL   , INTENT(OUT) :: wgts(ibmax,8)
    REAL,    INTENT(IN ) :: lam(ibmax)
    REAL,    INTENT(IN ) :: pphi(ibmax)
    REAL,    INTENT(IN ) :: sig(ibmax)
    INTEGER, INTENT(OUT) :: flag(ibmax)
    LOGICAL, INTENT(IN ) :: Done(ibmax)
    INTEGER, INTENT(IN ) :: ibend
    INTEGER              :: ib
    INTEGER              :: i(ibmax,4)
    INTEGER              :: j(ibmax)
    INTEGER              :: k(ibmax)
    INTEGER              :: jl
    INTEGER              :: ju
    INTEGER              :: ill
    INTEGER              :: ilr
    INTEGER              :: iul
    INTEGER              :: iur
    REAL                 :: beta(4)
    REAL                 :: d(4)


    flag = 0 

    ! get position of point relative to grid. 

    CALL GetIndex (i, j, k, lam, pphi, sig, Done, ibend)

    DO ib = 1, ibend
       IF (.NOT. Done(ib)) THEN
          jl = j(ib)
          ju = j(ib)+1

          ! if above last or below first latitude,  use two pairs
          ! from the same latitude

          IF (jl == 2) THEN
             iul = i(ib,3)
             iur = i(ib,3) + 1
             ill = iul + imaxperj(1) / 2
             ilr = iur + imaxperj(1) / 2

             ! normalized distance to left longitude at lower and upper latitude 
             
             d(3) = (lam(ib) - LongitudePerIJ(iul,ju-2)) / DeltaLonPerJ(ju)
             d(1) = d(3) 

             ! normalized distance to lower latitude 

             d(2) = (pphi(ib) - LatitudePerJ(jl)) / DeltaLatPerJ(jl) 
             jl = 3 
             flag(ib) = 1 
          ELSE IF (ju > jmax+2) THEN      
             ill = i(ib,2)
             ilr = i(ib,2) + 1
             iul = ill + imaxperj(jmax)/ 2
             iur = ilr + imaxperj(jmax)/ 2

             ! normalized distance to left longitude at lower and upper latitude 

             d(1) = (lam(ib) - LongitudePerIJ(ill,jl-2)) / DeltaLonPerJ(jl)
             d(3) = d(1) 

             ! normalized distance to lower latitude 

             d(2) = (pphi(ib) - LatitudePerJ(jl)) / DeltaLatPerJ(jl)
             ju = jmax + 2
             flag(ib) = 2 
          ELSE 
             ill = i(ib,2)
             ilr = i(ib,2) + 1
             iul = i(ib,3)
             iur = i(ib,3) + 1

             ! normalized distance to left longitude at lower and upper latitude 

             d(1) = (lam(ib) - LongitudePerIJ(ill,jl-2)) / DeltaLonPerJ(jl)
             d(3) = (lam(ib) - LongitudePerIJ(iul,ju-2)) / DeltaLonPerJ(ju)

             ! normalized distance to lower latitude 

             d(2) = (pphi(ib) - LatitudePerJ(jl)) / DeltaLatPerJ(jl) 
          END IF

          IF (ill > imaxperj(jl-2)+1) ill = ill - imaxperj(jl-2)
          IF (ilr > imaxperj(jl-2)+1) ilr = ilr - imaxperj(jl-2)
          IF (iul > imaxperj(ju-2)+1) iul = iul - imaxperj(ju-2)
          IF (iur > imaxperj(ju-2)+1) iur = iur - imaxperj(ju-2)
          
          ! normalized distance to lower layer

          d(4) = (sig(ib) - cl(k(ib))) / delcl (k(ib))  

          beta(1) = (1. - d(1)) * (1. - d(2))
          beta(2) = d(1) * (1. - d(2))
          beta(3) = d(3) * d(2)
          beta(4) = (1. - d(3)) * d(2)
          
          !weitghs 

          wgts(ib,1) = (1. - d(4)) * beta(1)  
          wgts(ib,2) = (1. - d(4)) * beta(2)  
          wgts(ib,3) = (1. - d(4)) * beta(3)  
          wgts(ib,4) = (1. - d(4)) * beta(4)  
          
          
          wgts(ib,5) = d(4) * beta(1)
          wgts(ib,6) = d(4) * beta(2)
          wgts(ib,7) = d(4) * beta(3)
          wgts(ib,8) = d(4) * beta(4)
          
          ! interpolation points  (ib, jb indexed) 
          
          ibjbkdx(ib,1,1)  = ibperij(ill-1,jl-2)
          ibjbkdx(ib,2,1)  = ibperij(ilr-1,jl-2)
          ibjbkdx(ib,3,1)  = ibperij(iur-1,ju-2)
          ibjbkdx(ib,4,1)  = ibperij(iul-1,ju-2)
          ibjbkdx(ib,5,1)  = ibjbkdx(ib,1,1) 
          ibjbkdx(ib,6,1)  = ibjbkdx(ib,2,1)
          ibjbkdx(ib,7,1)  = ibjbkdx(ib,3,1)
          ibjbkdx(ib,8,1)  = ibjbkdx(ib,4,1)
          
          ibjbkdx(ib,1,2) = k(ib) 
          ibjbkdx(ib,2,2) = k(ib) 
          ibjbkdx(ib,3,2) = k(ib) 
          ibjbkdx(ib,4,2) = k(ib) 
          ibjbkdx(ib,5,2) = k(ib)+1
          ibjbkdx(ib,6,2) = k(ib)+1
          ibjbkdx(ib,7,2) = k(ib)+1
          ibjbkdx(ib,8,2) = k(ib)+1
          
          ibjbkdx(ib,1,3)  = jbperij(ill-1,jl-2)
          ibjbkdx(ib,2,3)  = jbperij(ilr-1,jl-2)
          ibjbkdx(ib,3,3)  = jbperij(iur-1,ju-2)
          ibjbkdx(ib,4,3)  = jbperij(iul-1,ju-2)
          ibjbkdx(ib,5,3)  = ibjbkdx(ib,1,3) 
          ibjbkdx(ib,6,3)  = ibjbkdx(ib,2,3)  
          ibjbkdx(ib,7,3)  = ibjbkdx(ib,3,3)  
          ibjbkdx(ib,8,3)  = ibjbkdx(ib,4,3) 
          
       END IF
    END DO

  END SUBROUTINE ComputeWeightSL3DV

  !######################################################################## 
  ! DEP2D 
  !########################################################################


  SUBROUTINE Dep2D (u, v, dt) 
    REAL, INTENT(IN) :: u(ibmax, jbmax)
    REAL, INTENT(IN) :: v(ibmax, jbmax)
    REAL, INTENT(IN) :: dt 
    LOGICAL          :: Done(ibmax)
    INTEGER          :: i
    INTEGER          :: j
    INTEGER          :: cnt
    INTEGER          :: ib
    INTEGER          :: jb
    INTEGER          :: id(ibmax,4)
    INTEGER          :: jd(ibmax)
    REAL             :: lama
    REAL             :: phia !arrival, middle and departure points
    REAL             :: lamm(ibmax)
    REAL             :: phim(ibmax) !in polar coordinates
    REAL             :: lamd(ibmax)
    REAL             :: phid(ibmax)
    REAL             :: lamm_old(ibmax)
    REAL             :: phim_old(ibmax)
    REAL             :: xa(ibmax)
    REAL             :: ya(ibmax)
    REAL             :: za(ibmax) !arrival, middle and departure points
    REAL             :: xm(ibmax)
    REAL             :: ym(ibmax)
    REAL             :: zm(ibmax) !in cartesian coordinates
    REAL             :: xd
    REAL             :: yd
    REAL             :: zd
    REAL             :: wx
    REAL             :: wy
    REAL             :: wz !wind in cartesian system
    REAL             :: um(ibmax)
    REAL             :: vm(ibmax) !zonal and meridional velocity 
    REAL             :: b  !rescaling factor
    REAL             :: dotprod ! temporary for dot product
    REAL             :: aux1
    REAL             :: aux2  !temporaries for the stop condition



    DO jb = 1, jbmax
       DO ib = 1, ibmaxperJB(jb)

          ! longitude and latitude index 

          i = iperijb(ib,jb) 
          j = jperijb(ib,jb) 

          ! set arrival point. 

          lama = LongitudePerIJ(i+1,j)
          phia = LatitudePerJ(j+2)

          !arrival point in cartesian coordinates 

          xa(ib) = COS(lama)  * COS(phia)
          ya(ib) = SIN(lama)  * COS(phia)
          za(ib) = SIN(phia)

          ! first iteration. Start with middle == arrival point 

          lamm(ib) = lama
          phim(ib) = phia

          !before updating points, save old values

          lamm_old(ib)  = lamm(ib)
          phim_old(ib)  = phim(ib)

          ! first iteration velocity is known 

          um(ib) = u(ib,jb)  
          vm(ib) = v(ib,jb)  

          Done(ib) = .FALSE.
       END DO

       DO cnt = 1, MAXITER

          ! perform implicit euler step from arrival to middle

          DO ib = 1, ibmaxperJB(jb)

             IF (.NOT. Done(ib)) THEN
                wx = (- um(ib) * SIN(lamm(ib)) - vm(ib) * COS(lamm(ib)) * SIN(phim(ib))) / er
                wy = (  um(ib) * COS(lamm(ib)) - vm(ib) * SIN(lamm(ib)) * SIN(phim(ib))) / er 
                wz = (  vm(ib) * COS(phim(ib))) / er
                b = 1.0 / SQRT(1.0 + dt**2 * (wx**2 + wy**2 + wz**2) - &
                     2 * dt * (wx * xa(ib) + wy * ya(ib) + wz * za(ib)))
                xm(ib) = b * (xa(ib) - dt * wx)
                ym(ib) = b * (ya(ib) - dt * wy)
                zm(ib) = b * (za(ib) - dt * wz)
                lamm(ib) = ATAN2(ym(ib), xm(ib))
                IF (lamm(ib) < 0) lamm(ib) = lamm(ib) +  2 * PI
                phim(ib) = ASIN(zm(ib))
                aux1 = ABS(lamm(ib) - lamm_old(ib)) /( 2 * PI) 
                aux2 = ABS(phim(ib) - phim_old(ib)) / PI 
                Done(ib) = aux1 <= EPSTOL .AND. aux2 <= EPSTOL
             END IF

             ! before next iteration, save old values
             ! and update velocity 

             IF (.NOT. Done(ib)) THEN
                lamm_old(ib)  = lamm(ib)
                phim_old(ib)  = phim(ib)
             END IF


          END DO

          CALL LinearInterpolation (um, vm, u, v, lamm, phim, Done, ibmaxperJB(jb))

       END DO


       cnt = COUNT(.NOT. Done(1:ibmaxperJB(jb)))
       
!       IF (cnt > 0) THEN
!          PRINT * , "Max iterations exceeded at ", cnt," points "
!       END IF


       ! find departure point from middle and arrival. points are over a great circle

       DO ib = 1, ibmaxperJB(jb)
          dotprod = xa(ib) * xm(ib) + ya(ib) * ym(ib) + za(ib) * zm(ib)
          xd = 2 * dotprod * xm(ib) - xa(ib)
          yd = 2 * dotprod * ym(ib) - ya(ib)
          zd = 2 * dotprod * zm(ib) - za(ib)
          lamd(ib) = ATAN2(yd, xd)
          IF (lamd(ib) < 0) lamd(ib) = lamd(ib) + 2 * PI 
          phid(ib) = ASIN(zd)
          
          deplnplam(ib, jb) = lamd(ib)
          deplnpphi(ib, jb) = phid(ib)
       END DO

       CALL GetIndex (id, jd, lamd, phid, ibmaxperjb(jb))

       DO ib = 1, ibmaxperJB(jb)
          i_deplnp(ib, 1, jb) = id(ib,1)
          i_deplnp(ib, 2, jb) = id(ib,2)
          i_deplnp(ib, 3, jb) = id(ib,3)
          i_deplnp(ib, 4, jb) = id(ib,4)
          j_deplnp(ib, jb) = jd(ib)
       END DO
    END DO
  END SUBROUTINE Dep2D


  SUBROUTINE TangentPlane (gyu_past, gyv_past)
    REAL, INTENT (INOUT) :: gyu_past (ibmax,kmax,jbmax)
    REAL, INTENT (INOUT) :: gyv_past (ibmax,kmax,jbmax)
    REAL                 :: seno_alfasl
    REAL                 :: seno_gamasl
    REAL                 :: seno_phi_mid
    REAL                 :: cos_alfasl
    REAL                 :: cos_gamasl
    REAL                 :: cos_phi_mid
    REAL                 :: aux1
    REAL                 :: aux2
    REAL                 :: x_mais
    REAL                 :: x_menos
    REAL                 :: y_mais
    REAL                 :: y_menos
    REAL                 :: xx
    REAL                 :: A_uu
    REAL                 :: A_uv
    REAL                 :: A_vu
    REAL                 :: A_vv
    REAL                 :: gyu_temp
    REAL                 :: gyv_temp 
    INTEGER              :: ib
    INTEGER              :: jb
    INTEGER              :: k
    INTEGER              :: i
    INTEGER              :: j

    DO jb  =  1, jbmax
       DO k   =  1, kmax             
          DO ib  =  1, ibmaxperjb(jb)

             seno_alfasl     = SIN(alfasl(ib,k,jb))
             cos_alfasl      = COS(alfasl(ib,k,jb))

             seno_gamasl     = SIN(gamasl(ib,k,jb))
             cos_gamasl      = COS(gamasl(ib,k,jb))  

             seno_phi_mid    = SIN(midphi(ib,k,jb))
             cos_phi_mid     = COS(midphi(ib,k,jb))

             aux1 = (1-cos_alfasl) *  seno_gamasl * cos_phi_mid
             aux2  =  seno_alfasl  *  seno_phi_mid

             x_mais = (aux1 + aux2) * cos_gamasl
             y_mais = cos_phi_mid - (aux1 + aux2) * seno_gamasl

             ! usamos  alfasl(-) igual a  ( - alfasl ) 

             x_menos = (aux1 - aux2) * cos_gamasl
             y_menos = cos_phi_mid - (aux1 - aux2) * seno_gamasl 

             i = iperijb(ib,jb)
             j = jperijb(ib,jb)

             xx = (x_menos *x_menos + y_menos * y_menos ) 
             A_uu = (x_mais * x_menos + y_mais * y_menos)/xx
             A_uv =  (-y_mais * x_menos + x_mais * y_menos )/xx


             gyu_temp      =                                             &
                  A_uu * gyu_past(ib,k,jb)  +  A_uv * gyv_past(ib,k,jb)    

             A_vv =  A_uu
             A_vu = -A_uv

             gyv_temp      =                                             &
                  A_vu * gyu_past(ib,k,jb)  +  A_vv * gyv_past(ib,k,jb)

             gyu_past(ib,k,jb) = gyu_temp
             gyv_past(ib,k,jb) = gyv_temp 
          END DO
       END DO
    END DO

  END SUBROUTINE TangentPlane


  SUBROUTINE CreateInterpolation(DeltaLatPerJ,  DeltaLonPerJ, LongitudePerIJ,&
       &                         LatitudePerJ)
    REAL, DIMENSION(jmax+3),      INTENT(IN) :: DeltaLatPerJ
    REAL, DIMENSION(jmax+4),      INTENT(IN) :: DeltaLonPerJ
    REAL, DIMENSION(imax+3,jmax), INTENT(IN) :: LongitudePerIJ
    REAL, DIMENSION(jmax+4),      INTENT(IN) :: LatitudePerJ
    

    fac  = 3.*(1. - 10.*EPSILON(1.0))

    ALLOCATE(KeyI(ibMax,4,kMax,jbMax))
    ALLOCATE(KeyJ(ibMax,kMax,jbMax))
    ALLOCATE(KeyK(ibMax,kMax,jbMax))
    ALLOCATE(ClampK(ibMax,kMax,jbMax))

    ALLOCATE(xl(ibMax,4,kMax,jbMax))
    ALLOCATE(xr(ibMax,4,kMax,jbMax))
    ALLOCATE(wgt1x(ibMax,2,kMax,jbMax))
    ALLOCATE(wgt2x(ibMax,2,kMax,jbMax))
    ALLOCATE(wgt3x(ibMax,2,kMax,jbMax))
    ALLOCATE(wgt4x(ibMax,2,kMax,jbMax))

    ALLOCATE(hl(ibMax,4,kMax,jbMax))
    ALLOCATE(hr(ibMax,4,kMax,jbMax))
    ALLOCATE(dhl(ibMax,4,kMax,jbMax))
    ALLOCATE(dhr(ibMax,4,kMax,jbMax))

    ALLOCATE(ys(ibMax,kMax,jbMax))
    ALLOCATE(yn(ibMax,kMax,jbMax))
    ALLOCATE(wgt1y(ibMax,kMax,jbMax))
    ALLOCATE(wgt2y(ibMax,kMax,jbMax))
    ALLOCATE(wgt3y(ibMax,kMax,jbMax))
    ALLOCATE(wgt4y(ibMax,kMax,jbMax))

    ALLOCATE(hs(ibMax,kMax,jbMax))
    ALLOCATE(hn(ibMax,kMax,jbMax))
    ALLOCATE(dhs(ibMax,kMax,jbMax))
    ALLOCATE(dhn(ibMax,kMax,jbMax))

    ALLOCATE(rdphi(ibMax,kMax,jbMax))
    ALLOCATE(wgt1z(ibMax,kMax,jbMax))
    ALLOCATE(wgt2z(ibMax,kMax,jbMax))
    ALLOCATE(wgt3z(ibMax,kMax,jbMax))
    ALLOCATE(wgt4z(ibMax,kMax,jbMax))

    ALLOCATE(hb(ibMax,kMax,jbMax))
    ALLOCATE(ht(ibMax,kMax,jbMax))
    ALLOCATE(dhb(ibMax,kMax,jbMax))
    ALLOCATE(dht(ibMax,kMax,jbMax))
    ALLOCATE(rdz(ibMax,kMax,jbMax))

    ALLOCATE(wiz(4,2,kMax))
    ALLOCATE(wdz(4,2,kMax))
    ALLOCATE(lbasdy(4,2,jMax+4))
    ALLOCATE(z(kMax))
    ALLOCATE(lbasiy(4,2,jMax+4))
    ALLOCATE(dz(kMax-1))
    ALLOCATE(dphi(jMax+4))
    ALLOCATE(phi(jMax+4))
    ALLOCATE(lam(iMax+3,jMax))
    ALLOCATE(dx(jMax))
    ALLOCATE(rdx(jMax))
    ALLOCATE(rdx6(jMax))

    ALLOCATE(StencilIB(ibMax,12,kMax,jbMax))
    ALLOCATE(StencilJB(ibMax,12,kMax,jbMax))

!@@@ change phi, dphi, lam, dx, everywhere to these new names ... 

    phi = LatitudePerJ
    dphi(1:jmax+3) = DeltaLatPerJ
    dx = DeltaLonPerJ(3:jmax+2) 
    lam = LongitudePerIJ 

    rdx = 1.0 / dx
    rdx6 = 1.0 / (6.0 * dx)
    
    z = cl
    dz = delcl
!@@@
    lbasiy = 0.0
    lbasdy = 0.0
    wiz = 0.0
    wdz = 0.0
    CALL basiy(phi, lbasiy)
    CALL basdy(phi, lbasdy)
    CALL basiz(kMax, z, wiz)
    CALL basdz(kMax, z, wdz)

  END SUBROUTINE CreateInterpolation

  SUBROUTINE ComputeWeights2D(LamDep, PhiDep, IDep, JDep)

    REAL, DIMENSION(:,:), INTENT(IN) :: LamDep, PhiDep
    INTEGER, DIMENSION(:,:,:), INTENT(IN) :: IDep
    INTEGER, DIMENSION(:,:), INTENT(IN) :: JDep

    REAL, DIMENSION(ibMax) :: SigDep
    INTEGER, DIMENSION(ibMax) :: KDep

    INTEGER :: jb

    Is3D = .FALSE.

    KDep = 1

    xl = 0.0
    xr = 0.0
    hl = 0.0
    hr = 0.0
    dhl = 0.0
    dhr = 0.0
    wgt1x = 0.0
    wgt2x = 0.0
    wgt3x = 0.0
    wgt4x = 0.0
    ys = 0.0
    yn = 0.0
    hs = 0.0
    hn = 0.0
    dhs = 0.0
    dhn = 0.0
    wgt1y = 0.0
    wgt2y = 0.0
    wgt3y = 0.0
    wgt4y = 0.0
    ht = 0.0
    hb = 0.0
    dht = 0.0
    dhb = 0.0
    wgt1z = 0.0
    wgt2z = 0.0
    wgt3z = 0.0
    wgt4z = 0.0
    rdphi = 0.0
    rdz = 0.0

    DO jb = 1, jbMax
    
      CALL ComputeWeightsPerLine(LamDep(:,jb), SigDep, PhiDep(:,jb), &
     IDep(:,:,jb), KDep, JDep(:,jb), &
     KeyI(:,:,1,jb), KeyK(:,1,jb), KeyJ(:,1,jb), &
     ClampK(:,1,jb), &
     StencilIB(:,:,1,jb), StencilJB(:,:,1,jb), &
     xl(:,:,1,jb), xr(:,:,1,jb), &
     hl(:,:,1,jb), hr(:,:,1,jb), &
     dhl(:,:,1,jb), dhr(:,:,1,jb), &
     wgt1x(:,:,1,jb), wgt2x(:,:,1,jb), &
     wgt3x(:,:,1,jb), wgt4x(:,:,1,jb), &
     ys(:,1,jb), yn(:,1,jb), &
     hs(:,1,jb), hn(:,1,jb), &
     dhs(:,1,jb), dhn(:,1,jb), &
     wgt1y(:,1,jb), wgt2y(:,1,jb), &
     wgt3y(:,1,jb), wgt4y(:,1,jb), &
     ht(:,1,jb), hb(:,1,jb), &
     dht(:,1,jb), dhb(:,1,jb), &
     wgt1z(:,1,jb), wgt2z(:,1,jb), &
     wgt3z(:,1,jb), wgt4z(:,1,jb), &
     rdphi(:,1,jb), rdz(:,1,jb), &
     ibMaxPerJB(jb))
    END DO

  END SUBROUTINE ComputeWeights2D


  SUBROUTINE ComputeWeights3D(LamDep, SigDep, PhiDep, IDep, KDep, JDep)

    REAL, DIMENSION(:,:,:), INTENT(IN) :: LamDep, SigDep, PhiDep
    INTEGER, DIMENSION(:,:,:,:), INTENT(IN) :: IDep
    INTEGER, DIMENSION(:,:,:), INTENT(IN) :: KDep, JDep

    INTEGER :: k, jb

    Is3D = .TRUE.

    DO jb = 1, jbMax
      DO k = 1, kMax
      
 CALL ComputeWeightsPerLine(LamDep(:,k,jb), SigDep(:,k,jb), PhiDep(:,k,jb), &
       IDep(:,:,k,jb), KDep(:,k,jb), JDep(:,k,jb), &
       KeyI(:,:,k,jb), KeyK(:,k,jb), KeyJ(:,k,jb), &
       ClampK(:,k,jb), &
       StencilIB(:,:,k,jb), StencilJB(:,:,k,jb), &
       xl(:,:,k,jb), xr(:,:,k,jb), &
       hl(:,:,k,jb), hr(:,:,k,jb), &
       dhl(:,:,k,jb), dhr(:,:,k,jb), &
       wgt1x(:,:,k,jb), wgt2x(:,:,k,jb), &
       wgt3x(:,:,k,jb), wgt4x(:,:,k,jb), &
       ys(:,k,jb), yn(:,k,jb), &
       hs(:,k,jb), hn(:,k,jb), &
       dhs(:,k,jb), dhn(:,k,jb), &
       wgt1y(:,k,jb), wgt2y(:,k,jb), &
       wgt3y(:,k,jb), wgt4y(:,k,jb), &
       ht(:,k,jb), hb(:,k,jb), &
       dht(:,k,jb), dhb(:,k,jb), &
       wgt1z(:,k,jb), wgt2z(:,k,jb), &
       wgt3z(:,k,jb), wgt4z(:,k,jb), &
       rdphi(:,k,jb), rdz(:,k,jb), &
       ibMaxPerJB(jb))
      END DO
    END DO

  END SUBROUTINE ComputeWeights3D

  SUBROUTINE ComputeWeightsPerLine(LamDep, SigDep, PhiDep, &
       IDep, KDep, JDep, &
       KeyI, KeyK, KeyJ, &
       ClampK, &
       StencilIB, StencilJB, &
       xl, xr, &
       hl, hr, &
       dhl, dhr, &
       wgt1x, wgt2x, &
       wgt3x, wgt4x, &
       ys, yn, &
       hs, hn, &
       dhs, dhn, &
       wgt1y, wgt2y, &
       wgt3y, wgt4y, &
       ht, hb, &
       dht, dhb, &
       wgt1z, wgt2z, &
       wgt3z, wgt4z, &
       rdphi, rdz, &
       ibLim)

    REAL, DIMENSION(:), INTENT(IN) :: LamDep, SigDep, PhiDep
    INTEGER, DIMENSION(:,:), INTENT(IN) :: IDep
    INTEGER, DIMENSION(:), INTENT(IN) :: KDep, JDep
    INTEGER, DIMENSION(:,:), INTENT(OUT) :: KeyI
    INTEGER, DIMENSION(:), INTENT(OUT) :: KeyK, KeyJ, ClampK
    INTEGER, DIMENSION(:,:), INTENT(OUT) :: StencilIB, StencilJB

    REAL, DIMENSION(:,:), INTENT(OUT) :: xl, xr, hl, hr, dhl, dhr
    REAL, DIMENSION(:,:), INTENT(OUT) :: wgt1x, wgt2x, wgt3x, wgt4x
    REAL, DIMENSION(:), INTENT(OUT) :: ys, yn, hs, hn, dhs, dhn
    REAL, DIMENSION(:), INTENT(OUT) :: wgt1y, wgt2y, wgt3y, wgt4y
    REAL, DIMENSION(:), INTENT(OUT) :: ht, hb, dht, dhb
    REAL, DIMENSION(:), INTENT(OUT) :: wgt1z, wgt2z, wgt3z, wgt4z
    REAL, DIMENSION(:), INTENT(OUT) :: rdphi, rdz
    INTEGER, INTENT(IN) :: ibLim

!---------------------------Local workspace-----------------------------

  integer jdpval            ! |
  integer kdpval            ! |
  integer kdimm2            ! |

  real dyj                  ! |
  real tmp1                 ! |
  real tmp2                 ! |
  real tmp3                 ! |
  real tmp4                 ! | -- tmp variables
  real dzk                  ! |
  real zt                   ! |
  real zb                   ! |
  real denom1               ! |
  real denom2               ! |
  real denom3               ! |
  real denom4               ! |
  real coef12               ! |
  real coef34               ! |

!-----------------------------------------------------------------------
  LOGICAL :: LimHorDeriv, LimVerDeriv
  INTEGER, PARAMETER :: RightPoint(4) = (/ 2, 5, 9 ,12 /)
  INTEGER :: i, j, l, jj, ib, iib, jjb, i1, i2, i3, i4
  LimHorDeriv = .TRUE.
  LimVerDeriv = .TRUE.

  denom1 = -1./6.
  denom2 =  0.5
  denom3 = -0.5
  denom4 =  1./6.

  KeyI = IDep
  KeyJ = JDep
  KeyK = KDep

! HORIZONTAL weights

! Compute weights for x-direction

  DO ib = 1, ibLim

    j = KeyJ(ib)

    j = j - 1
    jj = j
    IF (j == -1) jj = 2
    IF (j == 0) jj = 1
    i = KeyI(ib,1)
    xl(ib,1) = ( lam(i+IOffset+1,jj) - LamDep(ib) )/dx(jj)
    xr(ib,1) = 1. - xl(ib,1)
    i1 = i
    IF (i1 <= 0) i1 = i1 + iMaxPerJ(jj)
    i2 = i + 1
    IF (j <= 0) THEN
      i1 = i1 + iMaxPerJ(jj) / 2
      i2 = i2 + iMaxPerJ(jj) / 2
    END IF
    IF (i1 > iMaxPerJ(jj)) i1 = i1 - iMaxPerJ(jj)
    IF (i2 > iMaxPerJ(jj)) i2 = i2 - iMaxPerJ(jj)
    StencilIB(ib,1) = ibPerIJ(i1,jj)
    StencilIB(ib,2) = ibPerIJ(i2,jj)
    StencilJB(ib,1) = jbPerIJ(i1,jj)
    StencilJB(ib,2) = jbPerIJ(i2,jj)

    j = j + 1
    jj = j
    IF (jj == 0) jj = 1
    i = KeyI(ib,2)
    xl(ib,2) = ( lam(i+IOffset+1,jj) - LamDep(ib) )/dx(jj)
    xr(ib,2) = 1. - xl(ib,2)
    i1 = i - 1
    IF (i1 <= 0) i1 = i1 + iMaxPerJ(jj)
    i2 = i
    IF (i2 <= 0) i2 = i2 + iMaxPerJ(jj)
    i3 = i + 1
    i4 = i + 2
    IF (j == 0) THEN
      i1 = i1 + iMaxPerJ(jj) / 2
      i2 = i2 + iMaxPerJ(jj) / 2
      i3 = i3 + iMaxPerJ(jj) / 2
      i4 = i4 + iMaxPerJ(jj) / 2
    END IF
    IF (i1 > iMaxPerJ(jj)) i1 = i1 - iMaxPerJ(jj)
    IF (i2 > iMaxPerJ(jj)) i2 = i2 - iMaxPerJ(jj)
    IF (i3 > iMaxPerJ(jj)) i3 = i3 - iMaxPerJ(jj)
    IF (i4 > iMaxPerJ(jj)) i4 = i4 - iMaxPerJ(jj)

    StencilIB(ib,3) = ibPerIJ(i1,jj)
    StencilIB(ib,4) = ibPerIJ(i2,jj)
    StencilIB(ib,5) = ibPerIJ(i3,jj)
    StencilIB(ib,6) = ibPerIJ(i4,jj)
    StencilJB(ib,3) = jbPerIJ(i1,jj)
    StencilJB(ib,4) = jbPerIJ(i2,jj)
    StencilJB(ib,5) = jbPerIJ(i3,jj)
    StencilJB(ib,6) = jbPerIJ(i4,jj)

    j = j + 1
    jj = j
    IF (j == jMax + 1) jj = jMax
    i = KeyI(ib,3)
    xl(ib,3) = ( lam(i+IOffset+1,jj) - LamDep(ib) )/dx(jj)
    xr(ib,3) = 1. - xl(ib,3)
    i1 = i - 1
    IF (i1 <= 0) i1 = i1 + iMaxPerJ(jj)
    i2 = i
    IF (i2 <= 0) i2 = i2 + iMaxPerJ(jj)
    i3 = i + 1
    i4 = i + 2
    IF (j == jMax + 1) THEN
      i1 = i1 + iMaxPerJ(jj) / 2
      i2 = i2 + iMaxPerJ(jj) / 2
      i3 = i3 + iMaxPerJ(jj) / 2
      i4 = i4 + iMaxPerJ(jj) / 2
    END IF
    IF (i1 > iMaxPerJ(jj)) i1 = i1 - iMaxPerJ(jj)
    IF (i2 > iMaxPerJ(jj)) i2 = i2 - iMaxPerJ(jj)
    IF (i3 > iMaxPerJ(jj)) i3 = i3 - iMaxPerJ(jj)
    IF (i4 > iMaxPerJ(jj)) i4 = i4 - iMaxPerJ(jj)

    StencilIB(ib,7 ) = ibPerIJ(i1,jj)
    StencilIB(ib,8 ) = ibPerIJ(i2,jj)
    StencilIB(ib,9 ) = ibPerIJ(i3,jj)
    StencilIB(ib,10) = ibPerIJ(i4,jj)
    StencilJB(ib,7 ) = jbPerIJ(i1,jj)
    StencilJB(ib,8 ) = jbPerIJ(i2,jj)
    StencilJB(ib,9 ) = jbPerIJ(i3,jj)
    StencilJB(ib,10) = jbPerIJ(i4,jj)

    j = j + 1
    jj = j
    IF (j == jMax + 1) jj = jMax
    IF (j == jMax + 2) jj = jMax - 1
    i = KeyI(ib,4)
    xl(ib,4) = ( lam(i+IOffset+1,jj) - LamDep(ib) )/dx(jj)
    xr(ib,4) = 1. - xl(ib,4)
    i1 = i
    IF (i1 <= 0) i1 = i1 + iMaxPerJ(jj)
    i2 = i + 1
    IF (i2 <= 0) i2 = i2 + iMaxPerJ(jj)
    IF (j >= jMax + 1) THEN
      i1 = i1 + iMaxPerJ(jj) / 2
      i2 = i2 + iMaxPerJ(jj) / 2
      i3 = i3 + iMaxPerJ(jj) / 2
      i4 = i4 + iMaxPerJ(jj) / 2
    END IF
    IF (i1 > iMaxPerJ(jj)) i1 = i1 - iMaxPerJ(jj)
    IF (i2 > iMaxPerJ(jj)) i2 = i2 - iMaxPerJ(jj)

    StencilIB(ib,11) = ibPerIJ(i1,jj)
    StencilIB(ib,12) = ibPerIJ(i2,jj)
    StencilJB(ib,11) = jbPerIJ(i1,jj)
    StencilJB(ib,12) = jbPerIJ(i2,jj)

  END DO

  IF (LimHorDeriv) THEN
    DO l = 1, 4
      DO ib = 1, ibLim

 iib = StencilIB(ib,RightPoint(l))
 jjb = StencilJB(ib,RightPoint(l))
 j = jPerIJB(iib,jjb)
 hl (ib,l) = ( 3.0 - 2.0*xl(ib,l) )*xl(ib,l)**2
 hr (ib,l) = ( 3.0 - 2.0*xr(ib,l) )*xr(ib,l)**2
 dhl(ib,l) = -dx(j) *( xl(ib,l) - 1. )*xl(ib,l)**2
 dhr(ib,l) =  dx(j) *( xr(ib,l) - 1. )*xr(ib,l)**2

      END DO
    END DO
  END IF

  DO l = 1, 2
    DO ib = 1, ibLim
      tmp1     =  xr(ib,l+1) + 1.
      tmp4     =  xr(ib,l+1) - 2.
      coef12   = -xl(ib,l+1)*tmp4
      coef34   =  xr(ib,l+1)*tmp1
      wgt1x(ib,l) =  denom1*coef12*xr(ib,l+1)
      wgt2x(ib,l) =  denom2*coef12*tmp1
      wgt3x(ib,l) =  denom3*coef34*tmp4
      wgt4x(ib,l) = -denom4*coef34*xl(ib,l+1)
    END DO
  END DO

! Compute weights for y-direction

    DO ib = 1, ibLim
      jdpval = KeyJ(ib) + JOffset
      dyj    = dphi(jdpval)
      ys(ib) = ( phi(jdpval+1) - PhiDep(ib) )/dyj
      yn(ib) = 1. - ys(ib)
      IF (LimHorDeriv) THEN
 rdphi(ib) = 1./dyj
 hs   (ib) = ( 3.0 - 2.0*ys(ib) )*ys(ib)**2
 hn   (ib) = ( 3.0 - 2.0*yn(ib) )*yn(ib)**2
 dhs  (ib) = -dyj*( ys(ib) - 1. )*ys(ib)**2
 dhn  (ib) =  dyj*( yn(ib) - 1. )*yn(ib)**2
      END IF
      tmp1     = PhiDep(ib) - lbasiy(1,1,jdpval)
      tmp2     = PhiDep(ib) - lbasiy(2,1,jdpval)
      tmp3     = PhiDep(ib) - lbasiy(3,1,jdpval)
      tmp4     = PhiDep(ib) - lbasiy(4,1,jdpval)
      coef12   = tmp3*tmp4   
      coef34   = tmp1*tmp2   
      wgt1y(ib) = coef12*tmp2*lbasiy(1,2,jdpval)
      wgt2y(ib) = coef12*tmp1*lbasiy(2,2,jdpval)
      wgt3y(ib) = coef34*tmp4*lbasiy(3,2,jdpval)
      wgt4y(ib) = coef34*tmp3*lbasiy(4,2,jdpval)
  END DO

! VERTICAL weights

  IF (Is3D) then

! Limit kdp to between "2" and "kdim-2" when computing weights and
! derivatives.

    kdimm2 = kMax - 2
    ClampK = min0(kdimm2,max0(2,KeyK))

    DO ib = 1, ibLim
      kdpval = ClampK(ib)
      dzk    = dz(KeyK(ib))
      rdz(ib) = 1./dzk
      zt     = (z(KeyK(ib)+1) - SigDep(ib) )/dzk
      zb     = 1. - zt
      ht (ib) = ( 3.0 - 2.0*zt )*zt**2
      hb (ib) = ( 3.0 - 2.0*zb )*zb**2
      dht(ib) = -dzk*( zt - 1. )*zt**2
      dhb(ib) =  dzk*( zb - 1. )*zb**2
      tmp1     = SigDep(ib) -  wiz(1,1,kdpval)
      tmp2     = SigDep(ib) -  wiz(2,1,kdpval)
      tmp3     = SigDep(ib) -  wiz(3,1,kdpval)
      tmp4     = SigDep(ib) -  wiz(4,1,kdpval)
      coef12   = tmp3*tmp4   
      coef34   = tmp1*tmp2   
      wgt1z(ib) = coef12*tmp2*wiz(1,2,kdpval)
      wgt2z(ib) = coef12*tmp1*wiz(2,2,kdpval)
      wgt3z(ib) = coef34*tmp4*wiz(3,2,kdpval)
      wgt4z(ib) = coef34*tmp3*wiz(4,2,kdpval)
    END DO
  END IF

  RETURN

  END SUBROUTINE ComputeWeightsPerLine

  SUBROUTINE Interpolate3D(fIn, fOut, LimHorDeriv, LimVerDeriv)

    REAL, DIMENSION(:,:,:), INTENT(IN) :: fIn
    REAL, DIMENSION(:,:,:), INTENT(OUT) :: fOut
    LOGICAL, INTENT(IN) :: LimHorDeriv, LimVerDeriv

    INTEGER :: j, k, kk, ib, jb
!-----------------------------------------------------------------------

! Interpolate field to departure points using Hermite or Lagrange
! Cubic interpolation

!---------------------------Local workspace-----------------------------

  INTEGER jdpval             ! |
  INTEGER kdpval             ! |
  INTEGER kdimm1             ! |
  INTEGER kdimm2             ! |
  INTEGER kdimm3             ! |

  REAL tmp1                  ! derivative factor
  REAL tmp2                  ! abs(tmp1)
  REAL deli                  ! linear derivative
  REAL fxl                   ! left  derivative estimate
  REAL fxr                   ! right derivative estimate

  REAL tmptop                     ! | -- work arrays
  REAL tmpbot                     ! |
  REAL fintx(ibMax,4,4,kMax,jbMax)   ! |
  REAL finty(ibMax,4,kMax,jbMax)        ! |
  REAL fbot(ibMax,4,kMax,jbMax)         ! |
  REAL ftop(ibMax,4,kMax,jbMax)         ! |

!-----------------------------------------------------------------------

  fintx = 0.0
  finty = 0.0
  fbot = 0.0
  ftop = 0.0

  kdimm1 = kMax - 1
  kdimm2 = kMax - 2
  kdimm3 = kMax - 3

!-----------------------------------------------------------------------
!    10XX loops: Hermite  cubic/linear interpolation in the horizontal
!-----------------------------------------------------------------------

  IF (LimHorDeriv) THEN

! PART 1:  x-interpolation

! Loop over fields.
! ..x interpolation at each height needed for z interpolation.
! ...x interpolation at each latitude needed for y interpolation.

    DO jb = 1, jbMax
      DO k = 1, kMax
 DO ib = 1, ibMaxPerJB(jb)

! Height level 1:  Linear interpolation on inner two latitudes only

!CC         fintx(i,1,1) = not used
   fintx(ib,2,1,k,jb) = &
     fIn(StencilIB(ib,4,k,jb), ClampK(ib,k,jb)-1, StencilJB(ib,4,k,jb)) * &
     xl(ib,2,k,jb) + &
     fIn(StencilIB(ib,5,k,jb), ClampK(ib,k,jb)-1, StencilJB(ib,5,k,jb)) * &
     xr(ib,2,k,jb)

   fintx(ib,3,1,k,jb) = &
     fIn(StencilIB(ib,8,k,jb), ClampK(ib,k,jb)-1, StencilJB(ib,8,k,jb)) * &
     xl(ib,3,k,jb) + &
     fIn(StencilIB(ib,9,k,jb), ClampK(ib,k,jb)-1, StencilJB(ib,9,k,jb)) * &
     xr(ib,3,k,jb)
!CC         fintx(i,4,1) = not used

! Height level 2

!   Latitude 1:  Linear interpolation

   fintx(ib,1,2,k,jb) = &
     fIn(StencilIB(ib,1,k,jb), ClampK(ib,k,jb), StencilJB(ib,1,k,jb)) * &
     xl(ib,1,k,jb) + &
     fIn(StencilIB(ib,2,k,jb), ClampK(ib,k,jb), StencilJB(ib,2,k,jb)) * &
     xr(ib,1,k,jb)

!   Latitude 2:  Cubic interpolation

   j = jPerIJB(StencilIB(ib,3,k,jb),StencilJB(ib,3,k,jb))
   fxl = (   - 2.* &
     fIn(StencilIB(ib,3,k,jb), ClampK(ib,k,jb), StencilJB(ib,3,k,jb)) &
      - 3.* &
     fIn(StencilIB(ib,4,k,jb), ClampK(ib,k,jb), StencilJB(ib,4,k,jb)) &
      + 6.* &
     fIn(StencilIB(ib,5,k,jb), ClampK(ib,k,jb), StencilJB(ib,5,k,jb)) &
      -     &
     fIn(StencilIB(ib,6,k,jb), ClampK(ib,k,jb), StencilJB(ib,6,k,jb)) )* &
     rdx6(j) 

   fxr = (        &
     fIn(StencilIB(ib,3,k,jb), ClampK(ib,k,jb), StencilJB(ib,3,k,jb)) &
      - 6.* &
     fIn(StencilIB(ib,4,k,jb), ClampK(ib,k,jb), StencilJB(ib,4,k,jb)) &
      + 3.* &
     fIn(StencilIB(ib,5,k,jb), ClampK(ib,k,jb), StencilJB(ib,5,k,jb)) &
      + 2.* &
     fIn(StencilIB(ib,6,k,jb), ClampK(ib,k,jb), StencilJB(ib,6,k,jb)) )* &
     rdx6(j)

   deli = ( &
     fIn(StencilIB(ib,5,k,jb), ClampK(ib,k,jb), StencilJB(ib,5,k,jb)) - &
     fIn(StencilIB(ib,4,k,jb), ClampK(ib,k,jb), StencilJB(ib,4,k,jb)) ) * rdx(j)

   tmp1 = fac*deli
   tmp2 = abs(tmp1)

   IF( deli*fxl   .le. 0.0  ) fxl = 0.
   IF( deli*fxr   .le. 0.0  ) fxr = 0.
   IF( abs( fxl ) .gt. tmp2 ) fxl = tmp1
   IF( abs( fxr ) .gt. tmp2 ) fxr = tmp1

   fintx(ib,2,2,k,jb) = &
     fIn(StencilIB(ib,4,k,jb), ClampK(ib,k,jb), StencilJB(ib,4,k,jb)) * &
     hl(ib,2,k,jb) + &
     fIn(StencilIB(ib,5,k,jb), ClampK(ib,k,jb), StencilJB(ib,5,k,jb)) * &
     hr(ib,2,k,jb) + &
     fxl * dhl(ib,2,k,jb) + &
     fxr * dhr(ib,2,k,jb)

!   Latitude 3:  Cubic interpolation

   j = jPerIJB(StencilIB(ib,7,k,jb), StencilJB(ib,7,k,jb))
   fxl = (   - 2.* &
     fIn(StencilIB(ib,7 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,7 ,k,jb)) &
      - 3.* &
     fIn(StencilIB(ib,8 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,8 ,k,jb)) &
      + 6.* &
     fIn(StencilIB(ib,9 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,9 ,k,jb)) &
      -     &
     fIn(StencilIB(ib,10,k,jb), ClampK(ib,k,jb), StencilJB(ib,10,k,jb)) )* &
     rdx6(j)

   fxr = (        &
     fIn(StencilIB(ib,7 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,7 ,k,jb)) &
      - 6.* &
     fIn(StencilIB(ib,8 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,8 ,k,jb)) &
      + 3.* &
     fIn(StencilIB(ib,9 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,9 ,k,jb)) &
      + 2.* &
     fIn(StencilIB(ib,10,k,jb), ClampK(ib,k,jb), StencilJB(ib,10,k,jb)) )* &
     rdx6(j)

   deli = ( &
     fIn(StencilIB(ib,9,k,jb), ClampK(ib,k,jb), StencilJB(ib,9,k,jb)) - &
     fIn(StencilIB(ib,8,k,jb), ClampK(ib,k,jb), StencilJB(ib,8,k,jb)) ) * rdx(j)

   tmp1 = fac*deli
   tmp2 = abs(tmp1)

   IF( deli*fxl   .le. 0.0  ) fxl = 0.
   IF( deli*fxr   .le. 0.0  ) fxr = 0.
   IF( abs( fxl ) .gt. tmp2 ) fxl = tmp1
   IF( abs( fxr ) .gt. tmp2 ) fxr = tmp1

   fintx(ib,3,2,k,jb) = &
     fIn(StencilIB(ib,8,k,jb), ClampK(ib,k,jb), StencilJB(ib,8,k,jb)) * &
     hl(ib,3,k,jb) + &
     fIn(StencilIB(ib,9,k,jb), ClampK(ib,k,jb), StencilJB(ib,9,k,jb)) * &
     hr(ib,3,k,jb) + &
     fxl * dhl(ib,3,k,jb) + &
     fxr * dhr(ib,3,k,jb)

!   Latitude 4:  Linear interpolation

   fintx(ib,4,2,k,jb) = &
     fIn(StencilIB(ib,11,k,jb), ClampK(ib,k,jb), StencilJB(ib,11,k,jb)) * &
     xl(ib,4,k,jb) + &
     fIn(StencilIB(ib,12,k,jb), ClampK(ib,k,jb), StencilJB(ib,12,k,jb)) * &
     xr(ib,4,k,jb)

! Height level 3

!   Latitude 1:  Linear interpolation

   fintx(ib,1,3,k,jb) = &
     fIn(StencilIB(ib,1,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,1,k,jb)) * &
     xl(ib,1,k,jb) + &
     fIn(StencilIB(ib,2,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,2,k,jb)) * &
     xr(ib,1,k,jb)

!   Latitude 2:  Cubic interpolation

   j = jPerIJB(StencilIB(ib,3,k,jb),StencilJB(ib,3,k,jb))
   fxl = (   - 2.* &
     fIn(StencilIB(ib,3,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,3,k,jb)) &
      - 3.* &
     fIn(StencilIB(ib,4,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,4,k,jb)) &
      + 6.* &
     fIn(StencilIB(ib,5,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,5,k,jb)) &
      -     &
     fIn(StencilIB(ib,6,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,6,k,jb)) )* &
     rdx6(j)

   fxr = (        &
     fIn(StencilIB(ib,3,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,3,k,jb)) &
      - 6.* &
     fIn(StencilIB(ib,4,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,4,k,jb)) &
      + 3.* &
     fIn(StencilIB(ib,5,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,5,k,jb)) &
      + 2.* &
     fIn(StencilIB(ib,6,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,6,k,jb)) )* &
     rdx6(j)

   deli = ( &
     fIn(StencilIB(ib,5,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,5,k,jb)) - &
     fIn(StencilIB(ib,4,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,4,k,jb)) ) * rdx(j)

   tmp1 = fac*deli
   tmp2 = abs(tmp1)

   IF( deli*fxl   .le. 0.0  ) fxl = 0.
   IF( deli*fxr   .le. 0.0  ) fxr = 0.
   IF( abs( fxl ) .gt. tmp2 ) fxl = tmp1
   IF( abs( fxr ) .gt. tmp2 ) fxr = tmp1

   fintx(ib,2,3,k,jb) = &
     fIn(StencilIB(ib,4,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,4,k,jb)) * &
     hl(ib,2,k,jb) + &
     fIn(StencilIB(ib,5,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,5,k,jb)) * &
     hr(ib,2,k,jb) + &
     fxl * dhl(ib,2,k,jb) + &
     fxr * dhr(ib,2,k,jb)

!   Latitude 3:  Cubic interpolation

   j = jPerIJB(StencilIB(ib,7,k,jb), StencilJB(ib,7,k,jb))
   fxl = (   - 2.* &
     fIn(StencilIB(ib,7 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,7 ,k,jb)) &
      - 3.* &
     fIn(StencilIB(ib,8 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,8 ,k,jb)) &
      + 6.* &
     fIn(StencilIB(ib,9 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,9 ,k,jb)) &
      -     &
     fIn(StencilIB(ib,10,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,10,k,jb)) )* &
     rdx6(j)

   fxr = (        &
     fIn(StencilIB(ib,7 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,7 ,k,jb)) &
      - 6.* &
     fIn(StencilIB(ib,8 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,8 ,k,jb)) &
      + 3.* &
     fIn(StencilIB(ib,9 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,9 ,k,jb)) &
      + 2.* &
     fIn(StencilIB(ib,10,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,10,k,jb)) )* &
     rdx6(j)

   deli = ( &
     fIn(StencilIB(ib,9,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,9,k,jb)) - &
     fIn(StencilIB(ib,8,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,8,k,jb)) ) * rdx(j)

   tmp1 = fac*deli
   tmp2 = abs(tmp1)

   IF( deli*fxl   .le. 0.0  ) fxl = 0.
   IF( deli*fxr   .le. 0.0  ) fxr = 0.
   IF( abs( fxl ) .gt. tmp2 ) fxl = tmp1
   IF( abs( fxr ) .gt. tmp2 ) fxr = tmp1

   fintx(ib,3,3,k,jb) = &
     fIn(StencilIB(ib,8,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,8,k,jb)) * &
     hl(ib,3,k,jb) + &
     fIn(StencilIB(ib,9,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,9,k,jb)) * &
     hr(ib,3,k,jb) + &
     fxl * dhl(ib,3,k,jb) + &
     fxr * dhr(ib,3,k,jb)

!   Latitude 4:  Linear interpolation

   fintx(ib,4,3,k,jb) = &
     fIn(StencilIB(ib,11,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,11,k,jb)) * &
     xl(ib,4,k,jb) + &
     fIn(StencilIB(ib,12,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,12,k,jb)) * &
     xr(ib,4,k,jb)

! Height level 4:  Linear interpolation on inner two latitudes only

!CC         fintx(i,1,4) = not used
   fintx(ib,2,4,k,jb) = &
     fIn(StencilIB(ib,4,k,jb), ClampK(ib,k,jb)+2, StencilJB(ib,4,k,jb)) * &
     xl(ib,2,k,jb) + &
     fIn(StencilIB(ib,5,k,jb), ClampK(ib,k,jb)+2, StencilJB(ib,5,k,jb)) * &
     xr(ib,2,k,jb)

   fintx(ib,3,4,k,jb) = &
     fIn(StencilIB(ib,8,k,jb), ClampK(ib,k,jb)+2, StencilJB(ib,8,k,jb)) * &
     xl(ib,3,k,jb) + &
     fIn(StencilIB(ib,9,k,jb), ClampK(ib,k,jb)+2, StencilJB(ib,9,k,jb)) * &
     xr(ib,3,k,jb)
!CC         fintx(i,4,4) = not used
 END DO
      END DO
    END DO

! The following loop computes x-derivatives for those cases when the
! departure point lies in either the top or bottom interval of the 
! model grid.  In this special case, data are shifted up or down to
! keep the departure point in the middle interval of the 4-point
! stencil.  Therefore, some derivatives that were computed above will 
! be over-written.

    DO jb = 1, jbMax
      DO k = 1, kMax
 DO ib = 1, ibMaxPerJB(jb)

! TOP interval

   j = jPerIJB(ib,jb)
   IF (KeyK(ib,k,jb) == 1) THEN

! shift levels 4 and 2 data to levels 1 and 3, respectively

     fintx(ib,2,1,k,jb) = fintx(ib,2,4,k,jb)
     fintx(ib,3,1,k,jb) = fintx(ib,3,4,k,jb)

     fintx(ib,1,3,k,jb) = fintx(ib,1,2,k,jb)
     fintx(ib,2,3,k,jb) = fintx(ib,2,2,k,jb)
     fintx(ib,3,3,k,jb) = fintx(ib,3,2,k,jb)
     fintx(ib,4,3,k,jb) = fintx(ib,4,2,k,jb)

! Height level 1 (placed in level 2 of stencil):

!   Latitude 1:  Linear interpolation

     fintx(ib,1,2,k,jb) = &
       fIn(StencilIB(ib,1,k,jb), 1, StencilJB(ib,1,k,jb)) * &
       xl(ib,1,k,jb) + &
       fIn(StencilIB(ib,2,k,jb), 1, StencilJB(ib,2,k,jb)) * &
       xr(ib,1,k,jb)

!   Latitude 2:  Cubic interpolation

     j = jPerIJB(StencilIB(ib,3,k,jb), StencilJB(ib,3,k,jb))
     fxl = (   - 2.* &
       fIn(StencilIB(ib,3,k,jb), 1, StencilJB(ib,3,k,jb)) &
        - 3.* &
       fIn(StencilIB(ib,4,k,jb), 1, StencilJB(ib,4,k,jb)) &
        + 6.* &
       fIn(StencilIB(ib,5,k,jb), 1, StencilJB(ib,5,k,jb)) &
        -     &
       fIn(StencilIB(ib,6,k,jb), 1, StencilJB(ib,6,k,jb)) )* &
       rdx6(j)

     fxr = (        &
       fIn(StencilIB(ib,3,k,jb), 1, StencilJB(ib,3,k,jb)) &
        - 6.* &
       fIn(StencilIB(ib,4,k,jb), 1, StencilJB(ib,4,k,jb)) &
        + 3.* &
       fIn(StencilIB(ib,5,k,jb), 1, StencilJB(ib,5,k,jb)) &
        + 2.* &
       fIn(StencilIB(ib,6,k,jb), 1, StencilJB(ib,6,k,jb)) )* &
       rdx6(j)

     deli = ( &
       fIn(StencilIB(ib,5,k,jb), 1, StencilJB(ib,5,k,jb)) - &
       fIn(StencilIB(ib,4,k,jb), 1, StencilJB(ib,4,k,jb)) ) * rdx(j)

     tmp1 = fac*deli
     tmp2 = abs( tmp1 )
     IF( deli*fxl   .le. 0.0  ) fxl = 0.
     IF( deli*fxr   .le. 0.0  ) fxr = 0.
     IF( abs( fxl ) .gt. tmp2 ) fxl = tmp1
     IF( abs( fxr ) .gt. tmp2 ) fxr = tmp1

     fintx(ib,2,2,k,jb) = &
       fIn(StencilIB(ib,4,k,jb), 1, StencilJB(ib,4,k,jb)) * &
       hl(ib,2,k,jb) + &
       fIn(StencilIB(ib,5,k,jb), 1, StencilJB(ib,5,k,jb)) * &
       hr(ib,2,k,jb) + &
       fxl * dhl(ib,2,k,jb) + &
       fxr * dhr(ib,2,k,jb)

!   Latitude 3:  Cubic interpolation

     j = jPerIJB(StencilIB(ib,7,k,jb), StencilJB(ib,7,k,jb))
     fxl = (   - 2.* &
       fIn(StencilIB(ib,7 ,k,jb), 1, StencilJB(ib,7 ,k,jb)) &
        - 3.* &
       fIn(StencilIB(ib,8 ,k,jb), 1, StencilJB(ib,8 ,k,jb)) &
        + 6.* &
       fIn(StencilIB(ib,9 ,k,jb), 1, StencilJB(ib,9 ,k,jb)) &
        -     &
       fIn(StencilIB(ib,10,k,jb), 1, StencilJB(ib,10,k,jb)) )* &
       rdx6(j)

     fxr = (        &
       fIn(StencilIB(ib,7 ,k,jb), 1, StencilJB(ib,7 ,k,jb)) &
        - 6.* &
       fIn(StencilIB(ib,8 ,k,jb), 1, StencilJB(ib,8 ,k,jb)) &
        + 3.* &
       fIn(StencilIB(ib,9 ,k,jb), 1, StencilJB(ib,9 ,k,jb)) &
        + 2.* &
       fIn(StencilIB(ib,10,k,jb), 1, StencilJB(ib,10,k,jb)) )* &
       rdx6(j)

     deli = ( &
       fIn(StencilIB(ib,9,k,jb), 1, StencilJB(ib,9,k,jb)) - &
       fIn(StencilIB(ib,8,k,jb), 1, StencilJB(ib,8,k,jb)) ) * rdx(j)

     tmp1 = fac*deli
     tmp2 = abs( tmp1 )
     IF( deli*fxl   .le. 0.0  ) fxl = 0.
     IF( deli*fxr   .le. 0.0  ) fxr = 0.
     IF( abs( fxl ) .gt. tmp2 ) fxl = tmp1
     IF( abs( fxr ) .gt. tmp2 ) fxr = tmp1

     fintx(ib,3,2,k,jb) = &
       fIn(StencilIB(ib,8,k,jb), 1, StencilJB(ib,8,k,jb)) * &
       hl(ib,3,k,jb) + &
       fIn(StencilIB(ib,9,k,jb), 1, StencilJB(ib,9,k,jb)) * &
       hr(ib,3,k,jb) + &
       fxl * dhl(ib,3,k,jb) + &
       fxr * dhr(ib,3,k,jb)

!   Latitude 4:  Linear interpolation

     fintx(ib,4,2,k,jb) = &
       fIn(StencilIB(ib,11,k,jb), 1, StencilJB(ib,11,k,jb)) * &
       xl(ib,4,k,jb) + &
       fIn(StencilIB(ib,12,k,jb), 1, StencilJB(ib,12,k,jb)) * &
       xr(ib,4,k,jb)

! Height level 3 (placed in level 4 of stencil):
!  Linear interpolation on inner two latitudes only

!CC           fintx(i,1,4) = not used
     fintx(ib,2,4,k,jb) = &
       fIn(StencilIB(ib,4,k,jb), 3, StencilJB(ib,4,k,jb)) * &
       xl(ib,2,k,jb) + &
       fIn(StencilIB(ib,5,k,jb), 3, StencilJB(ib,5,k,jb)) * &
       xr(ib,2,k,jb)
     fintx(ib,3,4,k,jb) = &
       fIn(StencilIB(ib,8,k,jb), 3, StencilJB(ib,8,k,jb)) * &
       xl(ib,3,k,jb) + &
       fIn(StencilIB(ib,9,k,jb), 3, StencilJB(ib,9,k,jb)) * &
       xr(ib,3,k,jb)
!CC           fintx(i,4,4) = not used

! BOT interval

   ELSE IF (KeyK(ib,k,jb) == kdimm1) THEN

! shift levels 1 and 3 data to levels 4 and 2, respectively

     fintx(ib,2,4,k,jb) = fintx(ib,2,1,k,jb)
     fintx(ib,3,4,k,jb) = fintx(ib,3,1,k,jb)

     fintx(ib,1,2,k,jb) = fintx(ib,1,3,k,jb)
     fintx(ib,2,2,k,jb) = fintx(ib,2,3,k,jb)
     fintx(ib,3,2,k,jb) = fintx(ib,3,3,k,jb)
     fintx(ib,4,2,k,jb) = fintx(ib,4,3,k,jb)

! Height level 2 (placed in level 1 of stencil):
!  Linear interpolation on inner two latitudes only

!CC           fintx(i,1,1) =  not used
     fintx(ib,2,1,k,jb) = &
       fIn(StencilIB(ib,4,k,jb), kdimm2, StencilJB(ib,4,k,jb)) * &
       xl(ib,2,k,jb) + &
       fIn(StencilIB(ib,5,k,jb), kdimm2, StencilJB(ib,5,k,jb)) * &
       xr(ib,2,k,jb)
     fintx(ib,3,1,k,jb) = &
       fIn(StencilIB(ib,8,k,jb), kdimm2, StencilJB(ib,8,k,jb)) * &
       xl(ib,3,k,jb) + &
       fIn(StencilIB(ib,9,k,jb), kdimm2, StencilJB(ib,9,k,jb)) * &
       xr(ib,3,k,jb)
!CC           fintx(i,4,1) =  not used

! Height level 4 (placed in level 3 of stencil):

!   Latitude 1:  Linear interpolation

     fintx(ib,1,3,k,jb) = &
       fIn(StencilIB(ib,1,k,jb), kMax, StencilJB(ib,1,k,jb)) * &
       xl(ib,1,k,jb) + &
       fIn(StencilIB(ib,2,k,jb), kMax, StencilJB(ib,2,k,jb)) * &
       xr(ib,1,k,jb)

!   Latitude 2:  Cubic interpolation

     j = jPerIJB(StencilIB(ib,3,k,jb), StencilJB(ib,3,k,jb))
     fxl = (   - 2.* &
       fIn(StencilIB(ib,3,k,jb), kMax, StencilJB(ib,3,k,jb)) &
        - 3.* &
       fIn(StencilIB(ib,4,k,jb), kMax, StencilJB(ib,4,k,jb)) &
        + 6.* &
       fIn(StencilIB(ib,5,k,jb), kMax, StencilJB(ib,5,k,jb)) &
        -     &
       fIn(StencilIB(ib,6,k,jb), kMax, StencilJB(ib,6,k,jb)) )* &
       rdx6(j)

     fxr = (        &
       fIn(StencilIB(ib,3,k,jb), kMax, StencilJB(ib,3,k,jb)) &
        - 6.* &
       fIn(StencilIB(ib,4,k,jb), kMax, StencilJB(ib,4,k,jb)) &
        + 3.* &
       fIn(StencilIB(ib,5,k,jb), kMax, StencilJB(ib,5,k,jb)) &
        + 2.* &
       fIn(StencilIB(ib,6,k,jb), kMax, StencilJB(ib,6,k,jb)) )* &
       rdx6(j)

     deli = ( &
       fIn(StencilIB(ib,5,k,jb), kMax, StencilJB(ib,5,k,jb)) - &
       fIn(StencilIB(ib,4,k,jb), kMax, StencilJB(ib,4,k,jb)) ) * rdx(j)

     tmp1 = fac*deli
     tmp2 = abs( tmp1 )
     IF( deli*fxl   .le. 0.0  ) fxl = 0.
     IF( deli*fxr   .le. 0.0  ) fxr = 0.
     IF( abs( fxl ) .gt. tmp2 ) fxl = tmp1
     IF( abs( fxr ) .gt. tmp2 ) fxr = tmp1

     fintx(ib,2,3,k,jb) = &
       fIn(StencilIB(ib,4,k,jb), kMax, StencilJB(ib,4,k,jb)) * &
       hl(ib,2,k,jb) + &
       fIn(StencilIB(ib,5,k,jb), kMax, StencilJB(ib,5,k,jb)) * &
       hr(ib,2,k,jb) + &
       fxl * dhl(ib,2,k,jb) + &
       fxr * dhr(ib,2,k,jb)


!   Latitude 3:  Cubic interpolation

     j = jPerIJB(StencilIB(ib,7,k,jb), StencilJB(ib,7,k,jb))
     fxl = (   - 2.* &
       fIn(StencilIB(ib,7 ,k,jb), kMax, StencilJB(ib,7 ,k,jb)) &
        - 3.* &
       fIn(StencilIB(ib,8 ,k,jb), kMax, StencilJB(ib,8 ,k,jb)) &
        + 6.* &
       fIn(StencilIB(ib,9 ,k,jb), kMax, StencilJB(ib,9 ,k,jb)) &
        -     &
       fIn(StencilIB(ib,10,k,jb), kMax, StencilJB(ib,10,k,jb)) )* &
       rdx6(j)

     fxr = (        &
       fIn(StencilIB(ib,7 ,k,jb), kMax, StencilJB(ib,7 ,k,jb)) &
        - 6.* &
       fIn(StencilIB(ib,8 ,k,jb), kMax, StencilJB(ib,8 ,k,jb)) &
        + 3.* &
       fIn(StencilIB(ib,9 ,k,jb), kMax, StencilJB(ib,9 ,k,jb)) &
        + 2.* &
       fIn(StencilIB(ib,10,k,jb), kMax, StencilJB(ib,10,k,jb)) )* &
       rdx6(j)

     deli = ( &
       fIn(StencilIB(ib,9,k,jb), kMax, StencilJB(ib,9,k,jb)) - &
       fIn(StencilIB(ib,8,k,jb), kMax, StencilJB(ib,8,k,jb)) ) * rdx(j)

     tmp1 = fac*deli
     tmp2 = abs( tmp1 )
     IF( deli*fxl   .le. 0.0  ) fxl = 0.
     IF( deli*fxr   .le. 0.0  ) fxr = 0.
     IF( abs( fxl ) .gt. tmp2 ) fxl = tmp1
     IF( abs( fxr ) .gt. tmp2 ) fxr = tmp1

     fintx(ib,3,3,k,jb) = &
       fIn(StencilIB(ib,8,k,jb), kMax, StencilJB(ib,8,k,jb)) * &
       hl(ib,3,k,jb) + &
       fIn(StencilIB(ib,9,k,jb), kMax, StencilJB(ib,9,k,jb)) * &
       hr(ib,3,k,jb) + &
       fxl * dhl(ib,3,k,jb) + &
       fxr * dhr(ib,3,k,jb)

!   Latitude 4:  Linear interpolation

     fintx(ib,4,3,k,jb) = &
       fIn(StencilIB(ib,11,k,jb), kMax, StencilJB(ib,11,k,jb)) * &
       xl(ib,4,k,jb) + &
       fIn(StencilIB(ib,12,k,jb), kMax, StencilJB(ib,12,k,jb)) * &
       xr(ib,4,k,jb)

   END IF
 END DO
      END DO
    END DO

! PART 2:  y-derivatives

! Loop over departure latitudes

    DO jb = 1, jbMax
      DO k = 1, kMax
 DO ib = 1, ibMaxPerJB(jb)
   jdpval = KeyJ(ib,k,jb) + JOffset

! y derivatives at the inner height levels (kk = 2,3) needed for
! z-interpolation

   DO kk  = 2,3
     fbot(ib,kk,k,jb) = &
       lbasdy(1,1,jdpval)*fintx(ib,1,kk,k,jb) + &
       lbasdy(2,1,jdpval)*fintx(ib,2,kk,k,jb) + &
       lbasdy(3,1,jdpval)*fintx(ib,3,kk,k,jb) + &
       lbasdy(4,1,jdpval)*fintx(ib,4,kk,k,jb)
     ftop(ib,kk,k,jb) = &
       lbasdy(1,2,jdpval)*fintx(ib,1,kk,k,jb) + &
       lbasdy(2,2,jdpval)*fintx(ib,2,kk,k,jb) + &
       lbasdy(3,2,jdpval)*fintx(ib,3,kk,k,jb) + &
       lbasdy(4,2,jdpval)*fintx(ib,4,kk,k,jb)
   END DO
 END DO
      END DO
    END DO

! Apply SCM0 limiter to derivative estimates.

    DO kk  = 2,3
      DO jb = 1, jbMax
 DO k = 1, kMax
   DO ib = 1, ibMaxPerJB(jb)
     deli       = ( fintx(ib,3,kk,k,jb) - fintx(ib,2,kk,k,jb) )*rdphi(ib,k,jb)
     tmp1 = fac*deli
     tmp2 = abs( tmp1 )
     IF( deli*fbot(ib,kk,k,jb)   .le. 0.0  ) fbot(ib,kk,k,jb) = 0.
     IF( deli*ftop(ib,kk,k,jb)   .le. 0.0  ) ftop(ib,kk,k,jb) = 0.
     IF( abs( fbot(ib,kk,k,jb) ) .gt. tmp2 ) fbot(ib,kk,k,jb) = tmp1
     IF( abs( ftop(ib,kk,k,jb) ) .gt. tmp2 ) ftop(ib,kk,k,jb) = tmp1
   END DO
 END DO
      END DO
    END DO

! PART 3:  y-interpolants

    DO jb = 1, jbMax
      DO k = 1, kMax
 DO ib = 1, ibMaxPerJB(jb)
   finty(ib,1,k,jb) = &
     fintx(ib,2,1,k,jb)*ys(ib,k,jb) + &
     fintx(ib,3,1,k,jb)*yn(ib,k,jb)
   finty(ib,2,k,jb) = &
     fintx(ib,2,2,k,jb)*hs(ib,k,jb) + &
     fbot (ib,2  ,k,jb)*dhs(ib,k,jb) + &
     fintx(ib,3,2,k,jb)*hn (ib,k,jb) + &
     ftop (ib,2  ,k,jb)*dhn(ib,k,jb)
   finty(ib,3,k,jb) = &
     fintx(ib,2,3,k,jb)*hs (ib,k,jb) + &
     fbot (ib,3  ,k,jb)*dhs(ib,k,jb) + &
     fintx(ib,3,3,k,jb)*hn (ib,k,jb) + &
     ftop (ib,3  ,k,jb)*dhn(ib,k,jb)
   finty(ib,4,k,jb) = &
     fintx(ib,2,4,k,jb)*ys(ib,k,jb) + &
     fintx(ib,3,4,k,jb)*yn(ib,k,jb)
 END DO
      END DO
    END DO
  END IF

!-----------------------------------------------------------------------
!    20XX loops: Lagrange cubic/linear interpolation in the horizontal
!-----------------------------------------------------------------------

  IF(.NOT. LimHorDeriv) THEN

! PART 1:  X-INTERPOLATION

! Loop over fields.
! ..x interpolation at each height needed for z interpolation.
! ...x interpolation at each latitude needed for y interpolation.

    DO jb = 1, jbMax
      DO k = 1, kMax
 DO ib = 1, ibMaxPerJB(jb)

! Height level 1:  Linear interpolation on inner two latitudes only

!CC         fintx(ib,k,jb,1,1) = not used
   fintx(ib,2,1,k,jb) = &
     fIn(StencilIB(ib,4,k,jb), ClampK(ib,k,jb)-1, StencilJB(ib,4,k,jb)) * &
     xl(ib,2,k,jb) + &
     fIn(StencilIB(ib,5,k,jb), ClampK(ib,k,jb)-1, StencilJB(ib,5,k,jb)) * &
     xr(ib,2,k,jb)
   fintx(ib,3,1,k,jb) = &
     fIn(StencilIB(ib,8,k,jb), ClampK(ib,k,jb)-1, StencilJB(ib,8,k,jb)) * &
     xl(ib,3,k,jb) + &
     fIn(StencilIB(ib,9,k,jb), ClampK(ib,k,jb)-1, StencilJB(ib,9,k,jb)) * &
     xr(ib,3,k,jb)
!CC         fintx(ib,k,jb,4,1) = not used

! Height level 2:  Linear interpolation on outer two latitudes;
!                  Cubic  interpolation on inner two latitudes.

   fintx(ib,1,2,k,jb) = &
     fIn(StencilIB(ib,1 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,1 ,k,jb)) * &
     xl(ib,1,k,jb) + &
     fIn(StencilIB(ib,2 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,2 ,k,jb)) * &
     xr(ib,1,k,jb)
   fintx(ib,2,2,k,jb) = &
     fIn(StencilIB(ib,3 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,3 ,k,jb)) * &
     wgt1x(ib,1,k,jb) + &
     fIn(StencilIB(ib,4 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,4 ,k,jb)) * &
     wgt2x(ib,1,k,jb) + &
     fIn(StencilIB(ib,5 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,5 ,k,jb)) * &
     wgt3x(ib,1,k,jb) + &
     fIn(StencilIB(ib,6 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,6 ,k,jb)) * &
     wgt4x(ib,1,k,jb)
   fintx(ib,3,2,k,jb) = &
     fIn(StencilIB(ib,7 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,7 ,k,jb)) * &
     wgt1x(ib,2,k,jb) + &
     fIn(StencilIB(ib,8 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,8 ,k,jb)) * &
     wgt2x(ib,2,k,jb) + &
     fIn(StencilIB(ib,9 ,k,jb), ClampK(ib,k,jb), StencilJB(ib,9 ,k,jb)) * &
     wgt3x(ib,2,k,jb) + &
     fIn(StencilIB(ib,10,k,jb), ClampK(ib,k,jb), StencilJB(ib,10,k,jb)) * &
     wgt4x(ib,2,k,jb)
   fintx(ib,4,2,k,jb) = &
     fIn(StencilIB(ib,11,k,jb), ClampK(ib,k,jb), StencilJB(ib,11,k,jb)) * &
     xl(ib,4,k,jb) + &
     fIn(StencilIB(ib,12,k,jb), ClampK(ib,k,jb), StencilJB(ib,12,k,jb)) * &
     xr(ib,4,k,jb)

! Height level 3:  Linear interpolation on outer two latitudes;
!                  Cubic  interpolation on inner two latitudes.

   fintx(ib,1,3,k,jb) = &
     fIn(StencilIB(ib,1 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,1 ,k,jb)) * &
     xl(ib,1,k,jb) + &
     fIn(StencilIB(ib,2 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,2 ,k,jb)) * &
     xr(ib,1,k,jb)
   fintx(ib,2,3,k,jb) = &
     fIn(StencilIB(ib,3 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,3 ,k,jb)) * &
     wgt1x(ib,1,k,jb) + &
     fIn(StencilIB(ib,4 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,4 ,k,jb)) * &
     wgt2x(ib,1,k,jb) + &
     fIn(StencilIB(ib,5 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,5 ,k,jb)) * &
     wgt3x(ib,1,k,jb) + &
     fIn(StencilIB(ib,6 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,6 ,k,jb)) * &
     wgt4x(ib,1,k,jb)
   fintx(ib,3,3,k,jb) = &
     fIn(StencilIB(ib,7 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,7 ,k,jb)) * &
     wgt1x(ib,2,k,jb) + &
     fIn(StencilIB(ib,8 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,8 ,k,jb)) * &
     wgt2x(ib,2,k,jb) + &
     fIn(StencilIB(ib,9 ,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,9 ,k,jb)) * &
     wgt3x(ib,2,k,jb) + &
     fIn(StencilIB(ib,10,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,10,k,jb)) * &
     wgt4x(ib,2,k,jb)
   fintx(ib,4,3,k,jb) = &
     fIn(StencilIB(ib,11,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,11,k,jb)) * &
     xl(ib,4,k,jb) + &
     fIn(StencilIB(ib,12,k,jb), ClampK(ib,k,jb)+1, StencilJB(ib,12,k,jb)) * &
     xr(ib,4,k,jb)

! Height level 4:  Linear interpolation on inner two latitudes only

!CC         fintx(i,1,4) = not used
   fintx(ib,2,4,k,jb) = &
     fIn(StencilIB(ib,4,k,jb), ClampK(ib,k,jb)+2, StencilJB(ib,4,k,jb)) * &
     xl(ib,2,k,jb) + &
     fIn(StencilIB(ib,5,k,jb), ClampK(ib,k,jb)+2, StencilJB(ib,5,k,jb)) * &
     xr(ib,2,k,jb)
   fintx(ib,3,4,k,jb) = &
     fIn(StencilIB(ib,8,k,jb), ClampK(ib,k,jb)+2, StencilJB(ib,8,k,jb)) * &
     xl(ib,3,k,jb) + &
     fIn(StencilIB(ib,9,k,jb), ClampK(ib,k,jb)+2, StencilJB(ib,9,k,jb)) * &
     xr(ib,3,k,jb)
!CC         fintx(i,4,4) = not used
 END DO
      END DO
    END DO

! The following loop computes x-derivatives for those cases when the
! departure point lies in either the top or bottom interval of the 
! model grid.  In this special case, data are shifted up or down to
! keep the departure point in the middle interval of the 4-point
! stencil.  Therefore, some derivatives that were computed above will 
! be over-written.

    DO jb = 1, jbMax
      DO k = 1, kMax
 DO ib = 1, ibMaxPerJB(jb)

! TOP interval

   IF (KeyK(ib,k,jb) == 1) THEN

! shift levels 4 and 2 data to levels 1 and 3, respectively

     fintx(ib,2,1,k,jb) = fintx(ib,2,4,k,jb)
     fintx(ib,3,1,k,jb) = fintx(ib,3,4,k,jb)

     fintx(ib,1,3,k,jb) = fintx(ib,1,2,k,jb)
     fintx(ib,2,3,k,jb) = fintx(ib,2,2,k,jb)
     fintx(ib,3,3,k,jb) = fintx(ib,3,2,k,jb)
     fintx(ib,4,3,k,jb) = fintx(ib,4,2,k,jb)

! Height level 1 (placed in level 2 of stencil):
!  Linear interpolation on outer two latitudes;
!  Cubic  interpolation on inner two latitudes.

     fintx(ib,1,2,k,jb) = &
       fIn(StencilIB(ib,1 ,k,jb), 1, StencilJB(ib,1 ,k,jb)) * &
       xl(ib,1,k,jb) + &
       fIn(StencilIB(ib,2 ,k,jb), 1, StencilJB(ib,2 ,k,jb)) * &
       xr(ib,1,k,jb)
     fintx(ib,2,2,k,jb) = &
       fIn(StencilIB(ib,3 ,k,jb), 1, StencilJB(ib,3 ,k,jb)) * &
       wgt1x(ib,1,k,jb) + &
       fIn(StencilIB(ib,4 ,k,jb), 1, StencilJB(ib,4 ,k,jb)) * &
       wgt2x(ib,1,k,jb) + &
       fIn(StencilIB(ib,5 ,k,jb), 1, StencilJB(ib,5 ,k,jb)) * &
       wgt3x(ib,1,k,jb) + &
       fIn(StencilIB(ib,6 ,k,jb), 1, StencilJB(ib,6 ,k,jb)) * &
       wgt4x(ib,1,k,jb)
     fintx(ib,3,2,k,jb) = &
       fIn(StencilIB(ib,7 ,k,jb), 1, StencilJB(ib,7 ,k,jb)) * &
       wgt1x(ib,2,k,jb) + &
       fIn(StencilIB(ib,8 ,k,jb), 1, StencilJB(ib,8 ,k,jb)) * &
       wgt2x(ib,2,k,jb) + &
       fIn(StencilIB(ib,9 ,k,jb), 1, StencilJB(ib,9 ,k,jb)) * &
       wgt3x(ib,2,k,jb) + &
       fIn(StencilIB(ib,10,k,jb), 1, StencilJB(ib,10,k,jb)) * &
       wgt4x(ib,2,k,jb)
     fintx(ib,4,2,k,jb) = &
       fIn(StencilIB(ib,11,k,jb), 1, StencilJB(ib,11,k,jb)) * &
       xl(ib,4,k,jb) + &
       fIn(StencilIB(ib,12,k,jb), 1, StencilJB(ib,12,k,jb)) * &
       xr(ib,4,k,jb)

! Height level 3 (placed in level 4 of stencil):
!  Linear interpolation on inner two latitudes only

!CC           fintx(i,1,4) = not used
     fintx(ib,2,4,k,jb) = &
       fIn(StencilIB(ib,4,k,jb), 3, StencilJB(ib,4,k,jb)) * &
       xl(ib,2,k,jb) + &
       fIn(StencilIB(ib,5,k,jb), 3, StencilJB(ib,5,k,jb)) * &
       xr(ib,2,k,jb)
     fintx(ib,3,4,k,jb) = &
       fIn(StencilIB(ib,8,k,jb), 3, StencilJB(ib,8,k,jb)) * &
       xl(ib,3,k,jb) + &
       fIn(StencilIB(ib,9,k,jb), 3, StencilJB(ib,9,k,jb)) * &
       xr(ib,3,k,jb)
!CC           fintx(i,4,4) = not used

! BOT interval

   ELSE IF (KeyK(ib,k,jb) == kdimm1) THEN

! shift levels 1 and 3 data to levels 4 and 2, respectively

     fintx(ib,2,4,k,jb) = fintx(ib,2,1,k,jb)
     fintx(ib,3,4,k,jb) = fintx(ib,3,1,k,jb)

     fintx(ib,1,2,k,jb) = fintx(ib,1,3,k,jb)
     fintx(ib,2,2,k,jb) = fintx(ib,2,3,k,jb)
     fintx(ib,3,2,k,jb) = fintx(ib,3,3,k,jb)
     fintx(ib,4,2,k,jb) = fintx(ib,4,3,k,jb)

! Height level 2 (placed in level 1 of stencil):
!  Linear interpolation on inner two latitudes only

!CC           fintx(i,1,1) = not used
     fintx(ib,2,1,k,jb) = &
       fIn(StencilIB(ib,4,k,jb), kdimm2, StencilJB(ib,4,k,jb)) * &
       xl(ib,2,k,jb) + &
       fIn(StencilIB(ib,5,k,jb), kdimm2, StencilJB(ib,5,k,jb)) * &
       xr(ib,2,k,jb)
     fintx(ib,3,1,k,jb) = &
       fIn(StencilIB(ib,8,k,jb), kdimm2, StencilJB(ib,8,k,jb)) * &
       xl(ib,3,k,jb) + &
       fIn(StencilIB(ib,9,k,jb), kdimm2, StencilJB(ib,9,k,jb)) * &
       xr(ib,3,k,jb)
!CC           fintx(i,4,1) = not used

! Height level 4 (placed in level 3 of stencil):
!  Linear interpolation on outer two latitudes;
!  Cubic  interpolation on inner two latitudes.

     fintx(ib,1,3,k,jb) = &
       fIn(StencilIB(ib,1 ,k,jb), kMax, StencilJB(ib,1 ,k,jb)) * &
       xl(ib,1,k,jb) + &
       fIn(StencilIB(ib,2 ,k,jb), kMax, StencilJB(ib,2 ,k,jb)) * &
       xr(ib,1,k,jb)
     fintx(ib,2,3,k,jb) = &
       fIn(StencilIB(ib,3 ,k,jb), kMax, StencilJB(ib,3 ,k,jb)) * &
       wgt1x(ib,1,k,jb) + &
       fIn(StencilIB(ib,4 ,k,jb), kMax, StencilJB(ib,4 ,k,jb)) * &
       wgt2x(ib,1,k,jb) + &
       fIn(StencilIB(ib,5 ,k,jb), kMax, StencilJB(ib,5 ,k,jb)) * &
       wgt3x(ib,1,k,jb) + &
       fIn(StencilIB(ib,6 ,k,jb), kMax, StencilJB(ib,6 ,k,jb)) * &
       wgt4x(ib,1,k,jb)
     fintx(ib,3,3,k,jb) = &
       fIn(StencilIB(ib,7 ,k,jb), kMax, StencilJB(ib,7 ,k,jb)) * &
       wgt1x(ib,2,k,jb) + &
       fIn(StencilIB(ib,8 ,k,jb), kMax, StencilJB(ib,8 ,k,jb)) * &
       wgt2x(ib,2,k,jb) + &
       fIn(StencilIB(ib,9 ,k,jb), kMax, StencilJB(ib,9 ,k,jb)) * &
       wgt3x(ib,2,k,jb) + &
       fIn(StencilIB(ib,10,k,jb), kMax, StencilJB(ib,10,k,jb)) * &
       wgt4x(ib,2,k,jb)
     fintx(ib,4,3,k,jb) = &
       fIn(StencilIB(ib,11,k,jb), kMax, StencilJB(ib,11,k,jb)) * &
       xl(ib,4,k,jb) + &
       fIn(StencilIB(ib,12,k,jb), kMax, StencilJB(ib,12,k,jb)) * &
       xr(ib,4,k,jb)

   END IF
 END DO
      END DO
    END DO

! PART 2:  Y-INTERPOLATION

! Linear on outside of stencil; Lagrange cubic on inside.

    DO jb = 1, jbMax
      DO k = 1, kMax
 DO ib = 1, ibMaxPerJB(jb)
   finty(ib,1,k,jb) = &
     fintx(ib,2,1,k,jb) * &
     ys(ib,k,jb) + &
     fintx(ib,3,1,k,jb) * &
     yn(ib,k,jb)
   finty(ib,2,k,jb) = &
     fintx(ib,1,2,k,jb) * &
     wgt1y(ib,k,jb) + &
     fintx(ib,2,2,k,jb) * &
     wgt2y(ib,k,jb) + &
     fintx(ib,3,2,k,jb) * &
     wgt3y(ib,k,jb) + &
     fintx(ib,4,2,k,jb) * &
     wgt4y(ib,k,jb)
   finty(ib,3,k,jb) = &
     fintx(ib,1,3,k,jb) * &
     wgt1y(ib,k,jb) + &
     fintx(ib,2,3,k,jb) * &
     wgt2y(ib,k,jb) + &
     fintx(ib,3,3,k,jb) * &
     wgt3y(ib,k,jb) + &
     fintx(ib,4,3,k,jb) * &
     wgt4y(ib,k,jb)
   finty(ib,4,k,jb) = &
     fintx(ib,2,4,k,jb) * &
     ys(ib,k,jb) + &
     fintx(ib,3,4,k,jb) * &
     yn(ib,k,jb)
 END DO
      END DO
    END DO
  END IF

!-----------------------------------------------------------------------
!    30XX loops: Hermite  cubic/linear interpolation in the vertical
!-----------------------------------------------------------------------

    IF (LimVerDeriv) THEN
      DO jb = 1, jbMax
 DO k = 1, kMax
   DO ib = 1, ibMaxPerJB(jb)
     kdpval = KeyK(ib,k,jb)
     IF (kdpval .eq. 1) THEN
       ftop(ib,1,k,jb) = 0.
       fbot(ib,1,k,jb) = &
  wdz(1,1,      2) * finty(ib,2,k,jb) + &
  wdz(2,1,      2) * finty(ib,3,k,jb) + &
  wdz(3,1,      2) * finty(ib,4,k,jb) + &
  wdz(4,1,      2) * finty(ib,1,k,jb)
     ELSEIF (kdpval .eq. kdimm1) THEN
       ftop(ib,1,k,jb) = &
  wdz(1,2,kdimm2 ) * finty(ib,4,k,jb) + &
  wdz(2,2,kdimm2 ) * finty(ib,1,k,jb) + &
  wdz(3,2,kdimm2 ) * finty(ib,2,k,jb) + &
  wdz(4,2,kdimm2 ) * finty(ib,3,k,jb)
       fbot(ib,1,k,jb) = 0.
     ELSE
       ftop(ib,1,k,jb) = &
  wdz(1,1,kdpval ) * finty(ib,1,k,jb) + &
  wdz(2,1,kdpval ) * finty(ib,2,k,jb) + &
  wdz(3,1,kdpval ) * finty(ib,3,k,jb) + &
  wdz(4,1,kdpval ) * finty(ib,4,k,jb)
       fbot(ib,1,k,jb) = &
  wdz(1,2,kdpval ) * finty(ib,1,k,jb) + &
  wdz(2,2,kdpval ) * finty(ib,2,k,jb) + &
  wdz(3,2,kdpval ) * finty(ib,3,k,jb) + &
  wdz(4,2,kdpval ) * finty(ib,4,k,jb)
     END IF
   END DO
 END DO
      END DO

  ! Apply SCM0 limiter to derivative estimates.

      DO jb = 1, jbMax
 DO k = 1, kMax
   DO ib = 1, ibMaxPerJB(jb)
     deli = ( finty(ib,3,k,jb) - finty(ib,2,k,jb) )*rdz(ib,k,jb)
     tmp1 = fac*deli
     tmp2 = abs( tmp1 )
     IF( deli*fbot(ib,1,k,jb)   .le. 0.0  ) fbot(ib,1,k,jb) = 0.
     IF( deli*ftop(ib,1,k,jb)   .le. 0.0  ) ftop(ib,1,k,jb) = 0.
     IF( abs( fbot(ib,1,k,jb) ) .gt. tmp2 ) fbot(ib,1,k,jb) = tmp1
     IF( abs( ftop(ib,1,k,jb) ) .gt. tmp2 ) ftop(ib,1,k,jb) = tmp1
     fOut(ib,k,jb) = &
       finty(ib,2,k,jb) * ht(ib,k,jb) + &
       ftop (ib,1,k,jb) * dht(ib,k,jb) + &
       finty(ib,3,k,jb) * hb(ib,k,jb) + &
       fbot (ib,1,k,jb) * dhb(ib,k,jb)
   END DO
 END DO
      END DO
    END IF

!-----------------------------------------------------------------------
!    40XX loops: Lagrange cubic/linear interpolation in the vertical
!-----------------------------------------------------------------------

    IF (.NOT. LimVerDeriv) THEN
      DO jb = 1, jbMax
 DO k = 1, kMax
   DO ib = 1, ibMaxPerJB(jb)
     fOut(ib,k,jb) = &
       finty(ib,1,k,jb) * wgt1z(ib,k,jb) + &
       finty(ib,2,k,jb) * wgt2z(ib,k,jb) + &
       finty(ib,3,k,jb) * wgt3z(ib,k,jb) + &
       finty(ib,4,k,jb) * wgt4z(ib,k,jb)
   END DO
 END DO
      END DO
    END IF

! IF the departure point is in either the top or bottom interval of the
! model grid:  THEN
! Perform Hermite cubic interpolation.  The data are shifted up or down
! such that the departure point sits in the middle interval of the
! 4 point stencil (the shift originally took place in routine "LAGXIN").
! Therefore the derivative weights must be applied appropriately to
! account for this shift.  The following overwrites some results from
! the previous loop.

    DO jb = 1, jbMax
      DO k = 1, kMax
 DO ib = 1, ibMaxPerJB(jb)
   IF (KeyK(ib,k,jb) == 1) THEN
  !CCCC         tmptop = 0.0
     tmpbot = wdz(1,1,     2) * finty(ib,2,k,jb) &
     + wdz(2,1,     2) * finty(ib,3,k,jb) &
     + wdz(3,1,     2) * finty(ib,4,k,jb) &
     + wdz(4,1,     2) * finty(ib,1,k,jb)
     fOut(ib,k,jb) = finty(ib,2,k,jb) * ht(ib,k,jb) &
  !CCCC                + tmptop    *dht(ib,k,jb) 
     + finty(ib,3,k,jb) * hb(ib,k,jb) &
     + tmpbot * dhb(ib,k,jb)
   ELSE IF (KeyK(ib,k,jb) == kdimm1) THEN
     tmptop = wdz(1,2,kdimm2) * finty(ib,4,k,jb) &
     + wdz(2,2,kdimm2) * finty(ib,1,k,jb) &
     + wdz(3,2,kdimm2) * finty(ib,2,k,jb) &
     + wdz(4,2,kdimm2) * finty(ib,3,k,jb)
  !CCCC         tmpbot = 0.0
     fOut(ib,k,jb) = finty(ib,2,k,jb) * ht(ib,k,jb) &
     + tmptop * dht(ib,k,jb) &
     + finty(ib,3,k,jb) * hb(ib,k,jb) 
  !CCCC                + tmpbot    *dhb(ib,k,jb)
   END IF
 END DO
      END DO
    END DO

  END SUBROUTINE Interpolate3D

  SUBROUTINE Interpolate2D(fIn, fOut, LimHorDeriv)

    REAL, DIMENSION(:,:), INTENT(IN) :: fIn
    REAL, DIMENSION(:,:), INTENT(OUT) :: fOut
    LOGICAL, INTENT(IN) :: LimHorDeriv

    INTEGER :: j, ib, jb
!-----------------------------------------------------------------------

! Interpolate field to departure points using Hermite or Lagrange
! Cubic interpolation

!---------------------------Local workspace-----------------------------

  INTEGER jdpval             ! |

  REAL tmp1		     ! derivative factor
  REAL tmp2		     ! abs(tmp1)
  REAL deli		     ! linear derivative
  REAL fxl		     ! left  derivative estimate
  REAL fxr		     ! right derivative estimate

  REAL f1			  ! |
  REAL f2			  ! |
  REAL f3			  ! |
  REAL f4			  ! |
  REAL fintx(ibMax,4,4,jbMax)	  ! |
  REAL finty(ibMax,4,jbMax)	  ! |
  REAL fbot(ibMax,4,jbMax)	  ! |
  REAL ftop(ibMax,4,jbMax)	  ! |

!-----------------------------------------------------------------------
!    50XX loops: an optimized Lagrange cubic/linear algorithm in the
!-----------------------------------------------------------------------

    fintx = 0.0
    finty = 0.0
    fbot = 0.0
    ftop = 0.0

    IF (LimHorDeriv) THEN

! PART 1:  x-interpolation

      DO jb = 1, jbMax
 DO ib = 1, ibMaxPerJB(jb)

!   Latitude 1:  Linear interpolation

 fintx(ib,1,2,jb) = &
   fIn(StencilIB(ib,1,1,jb), StencilJB(ib,1,1,jb)) * &
   xl(ib,1,1,jb) + &
   fIn(StencilIB(ib,2,1,jb), StencilJB(ib,2,1,jb)) * &
   xr(ib,1,1,jb)

!   Latitude 2:  Cubic interpolation

 j = jPerIJB(StencilIB(ib,3,1,jb), StencilJB(ib,3,1,jb))
 fxl = (   - 2.* &
   fIn(StencilIB(ib,3,1,jb), StencilJB(ib,3,1,jb)) &
    - 3.* &
   fIn(StencilIB(ib,4,1,jb), StencilJB(ib,4,1,jb)) &
    + 6.* &
   fIn(StencilIB(ib,5,1,jb), StencilJB(ib,5,1,jb)) &
    -     &
   fIn(StencilIB(ib,6,1,jb), StencilJB(ib,6,1,jb)) )* &
   rdx6(j)

 fxr = (        &
   fIn(StencilIB(ib,3,1,jb), StencilJB(ib,3,1,jb)) &
    - 6.* &
   fIn(StencilIB(ib,4,1,jb), StencilJB(ib,4,1,jb)) &
    + 3.* &
   fIn(StencilIB(ib,5,1,jb), StencilJB(ib,5,1,jb)) &
    + 2.* &
   fIn(StencilIB(ib,6,1,jb), StencilJB(ib,6,1,jb)) )* &
   rdx6(j)

 deli = ( &
   fIn(StencilIB(ib,5,1,jb), StencilJB(ib,5,1,jb)) - &
   fIn(StencilIB(ib,4,1,jb), StencilJB(ib,4,1,jb)) ) * rdx(j)

 tmp1 = fac*deli
 tmp2 = abs( tmp1 )
 IF( deli*fxl   .le. 0.0  ) fxl = 0.
 IF( deli*fxr   .le. 0.0  ) fxr = 0.
 IF( abs( fxl ) .gt. tmp2 ) fxl = tmp1
 IF( abs( fxr ) .gt. tmp2 ) fxr = tmp1

 fintx(ib,2,2,jb) = &
   fIn(StencilIB(ib,4,1,jb), StencilJB(ib,4,1,jb)) * &
   hl(ib,2,1,jb) + &
   fIn(StencilIB(ib,5,1,jb), StencilJB(ib,5,1,jb)) * &
   hr(ib,2,1,jb) + &
   fxl * dhl(ib,2,1,jb) + &
   fxr * dhr(ib,2,1,jb)

!   Latitude 3:  Cubic interpolation

 j = jPerIJB(StencilIB(ib,7,1,jb), StencilJB(ib,7,1,jb))
 fxl = (   - 2.* &
   fIn(StencilIB(ib,7 ,1,jb), StencilJB(ib,7 ,1,jb)) &
    - 3.* &
   fIn(StencilIB(ib,8 ,1,jb), StencilJB(ib,8 ,1,jb)) &
    + 6.* &
   fIn(StencilIB(ib,9 ,1,jb), StencilJB(ib,9 ,1,jb)) &
    -     &
   fIn(StencilIB(ib,10,1,jb), StencilJB(ib,10,1,jb)) )* &
   rdx6(j)

 fxr = (        &
   fIn(StencilIB(ib,7 ,1,jb), StencilJB(ib,7 ,1,jb)) &
    - 6.* &
   fIn(StencilIB(ib,8 ,1,jb), StencilJB(ib,8 ,1,jb)) &
    + 3.* &
   fIn(StencilIB(ib,9 ,1,jb), StencilJB(ib,9 ,1,jb)) &
    + 2.* &
   fIn(StencilIB(ib,10,1,jb), StencilJB(ib,10,1,jb)) )* &
   rdx6(j)

 deli = ( &
   fIn(StencilIB(ib,9,1,jb), StencilJB(ib,9,1,jb)) - &
   fIn(StencilIB(ib,8,1,jb), StencilJB(ib,8,1,jb)) ) * rdx(j)

 tmp1 = fac*deli
 tmp2 = abs( tmp1 )
 IF( deli*fxl   .le. 0.0  ) fxl = 0.
 IF( deli*fxr   .le. 0.0  ) fxr = 0.
 IF( abs( fxl ) .gt. tmp2 ) fxl = tmp1
 IF( abs( fxr ) .gt. tmp2 ) fxr = tmp1

 fintx(ib,3,2,jb) = &
   fIn(StencilIB(ib,8,1,jb), StencilJB(ib,8,1,jb)) * &
   hl(ib,3,1,jb) + &
   fIn(StencilIB(ib,9,1,jb), StencilJB(ib,9,1,jb)) * &
   hr(ib,3,1,jb) + &
   fxl * dhl(ib,3,1,jb) + &
   fxr * dhr(ib,3,1,jb)

!   Latitude 4:  Linear interpolation

 fintx(ib,4,2,jb) = &
   fIn(StencilIB(ib,11,1,jb), StencilJB(ib,11,1,jb)) * &
   xl(ib,4,1,jb) + &
   fIn(StencilIB(ib,12,1,jb), StencilJB(ib,12,1,jb)) * &
   xr(ib,4,1,jb)

 END DO
      END DO

! PART 2:  y-derivatives

! Loop over departure latitudes

      DO jb = 1, jbMax
 DO ib = 1, ibMaxPerJB(jb)
   jdpval = KeyJ(ib,1,jb) + JOffset

   fbot(ib,2,jb) = &
     lbasdy(1,1,jdpval)*fintx(ib,1,2,jb) + &
     lbasdy(2,1,jdpval)*fintx(ib,2,2,jb) + &
     lbasdy(3,1,jdpval)*fintx(ib,3,2,jb) + &
     lbasdy(4,1,jdpval)*fintx(ib,4,2,jb)
   ftop(ib,2,jb) = &
     lbasdy(1,2,jdpval)*fintx(ib,1,2,jb) + &
     lbasdy(2,2,jdpval)*fintx(ib,2,2,jb) + &
     lbasdy(3,2,jdpval)*fintx(ib,3,2,jb) + &
     lbasdy(4,2,jdpval)*fintx(ib,4,2,jb)

! Apply SCM0 limiter to derivative estimates.

   deli = ( fintx(ib,3,2,jb) - fintx(ib,2,2,jb) )*rdphi(ib,1,jb)
   tmp1 = fac*deli
   tmp2 = abs( tmp1 )
   IF (deli*fbot(ib,2,jb) .le. 0.0 ) fbot(ib,2,jb) = 0.
   IF (deli*ftop(ib,2,jb) .le. 0.0 ) ftop(ib,2,jb) = 0.
   IF (abs(fbot(ib,2,jb)) .gt. tmp2) fbot(ib,2,jb) = tmp1
   IF (abs(ftop(ib,2,jb)) .gt. tmp2) ftop(ib,2,jb) = tmp1
 END DO
      END DO

! PART 3:  y-interpolants

      DO jb = 1, jbMax
 DO ib = 1, ibMaxPerJB(jb)
   fOut(ib,jb) = &
     fintx(ib,2,2,jb) * hs (ib,1,jb) + &
     fbot (ib,2  ,jb) * dhs(ib,1,jb) + &
     fintx(ib,3,2,jb) * hn (ib,1,jb) + &
     ftop (ib,2  ,jb) * dhn(ib,1,jb)
 END DO
      END DO
    END IF

    IF(.NOT. LimHorDeriv) THEN

!-----------------------------------------------------------------------
!    60XX loops: Hermite cubic/linear interpolation in the horizontal
!-----------------------------------------------------------------------

    DO jb = 1, jbMax
      DO ib = 1, ibMaxPerJB(jb)

! x-interpolants for the 4 latitudes

 f1 = &
   fIn(StencilIB(ib,1 ,1,jb), StencilJB(ib,1 ,1,jb)) * &
   xl(ib,1,1,jb) + &
   fIn(StencilIB(ib,2 ,1,jb), StencilJB(ib,2 ,1,jb)) * &
   xr(ib,1,1,jb)
 f2 = &
   fIn(StencilIB(ib,3 ,1,jb), StencilJB(ib,3 ,1,jb)) * &
   wgt1x(ib,1,1,jb) + &
   fIn(StencilIB(ib,4 ,1,jb), StencilJB(ib,4 ,1,jb)) * &
   wgt2x(ib,1,1,jb) + &
   fIn(StencilIB(ib,5 ,1,jb), StencilJB(ib,5 ,1,jb)) * &
   wgt3x(ib,1,1,jb) + &
   fIn(StencilIB(ib,6 ,1,jb), StencilJB(ib,6 ,1,jb)) * &
   wgt4x(ib,1,1,jb)
 f3 = &
   fIn(StencilIB(ib,7 ,1,jb), StencilJB(ib,7 ,1,jb)) * &
   wgt1x(ib,2,1,jb) + &
   fIn(StencilIB(ib,8 ,1,jb), StencilJB(ib,8 ,1,jb)) * &
   wgt2x(ib,2,1,jb) + &
   fIn(StencilIB(ib,9 ,1,jb), StencilJB(ib,9 ,1,jb)) * &
   wgt3x(ib,2,1,jb) + &
   fIn(StencilIB(ib,10,1,jb), StencilJB(ib,10,1,jb)) * &
   wgt4x(ib,2,1,jb)
 f4 = &
   fIn(StencilIB(ib,11,1,jb), StencilJB(ib,11,1,jb)) * &
   xl(ib,4,1,jb) + &
   fIn(StencilIB(ib,12,1,jb), StencilJB(ib,12,1,jb)) * &
   xr(ib,4,1,jb)

! y-interpolant

 fOut(ib,jb) = &
   f1 * wgt1y(ib,1,jb) + &
   f2 * wgt2y(ib,1,jb) + &
   f3 * wgt3y(ib,1,jb) + &
   f4 * wgt4y(ib,1,jb)
      END DO
    END DO
  END IF

  END SUBROUTINE Interpolate2D

  FUNCTION BaseI(Lon, j) RESULT (i)
    REAL, INTENT(IN) :: Lon
    INTEGER, INTENT(IN) :: j
    INTEGER :: i

    i = FLOOR((iMaxPerJ(j) * Lon) / (2.0 * pai)) + 1
  END FUNCTION BaseI

  SUBROUTINE BaseIJ(Lat, Lon, i, j)
    REAL, INTENT(IN) :: Lat, Lon
    INTEGER, INTENT(OUT) :: i, j

    j = FLOOR(jMax * (Lat + pai / 2.0) / pai)
    DO
      j = j + 1
      if (j > jMax) EXIT
      if (phi(j) > Lat) EXIT
    END DO
    DO
      j = j - 1
      if (j < 1) EXIT
      if (phi(j-1) < Lat) EXIT
    END DO
    i = BaseI(Lon,j)
  END SUBROUTINE BaseIJ

  SUBROUTINE BaseK(Sig, k)
    REAL, INTENT(IN) :: Sig
    INTEGER, INTENT(OUT) :: k
    DO k = 1, kMax - 1
      if (z(k+1) > Sig) EXIT
    END DO
    IF (k == kMax) k = kMax - 1
  END SUBROUTINE BaseK


  subroutine lcbas(grd     ,bas1    ,bas2    )

    implicit none
    !-----------------------------------------------------------------------

    ! Evaluate the partial Lagrangian cubic basis functions (denominator
    ! only ) for the grid points and gather grid values

    !------------------------------Arguments--------------------------------

    ! Input arguments

    real grd(4)               ! grid stencil

    ! Output arguments

    real bas1(4)              ! grid values on stencil
    real bas2(4)              ! lagrangian basis functions

    !---------------------------Local variables-----------------------------

    real x0mx1                ! |
    real x0mx2                ! |
    real x0mx3                ! |- grid value differences used in
    real x1mx2                ! |  weights
    real x1mx3                ! |
    real x2mx3                ! |

    !-----------------------------------------------------------------------

    x0mx1   = grd(1) - grd(2)
    x0mx2   = grd(1) - grd(3)
    x0mx3   = grd(1) - grd(4)
    x1mx2   = grd(2) - grd(3)
    x1mx3   = grd(2) - grd(4)
    x2mx3   = grd(3) - grd(4)

    bas1(1) = grd(1)
    bas1(2) = grd(2)
    bas1(3) = grd(3)
    bas1(4) = grd(4)

    bas2(1) =  1./ ( x0mx1 * x0mx2 * x0mx3 )
    bas2(2) = -1./ ( x0mx1 * x1mx2 * x1mx3 )
    bas2(3) =  1./ ( x0mx2 * x1mx2 * x2mx3 )
    bas2(4) = -1./ ( x0mx3 * x1mx3 * x2mx3 )

    return
  end subroutine lcbas

  subroutine basiy(phi, lbasiy)

    implicit none
   real,    intent(in)  :: phi(jmax+4)   ! latitude coordinates of model grid
   real,    intent(out) :: lbasiy(4,2,jmax+4) ! derivative estimate weights
    !-----------------------------------------------------------------------

    ! Compute weights used in Lagrange cubic polynomial interpolation in 
    ! the central interval of a four point stencil.  Done for each interval
    ! in the unequally spaced latitude grid.

    !------------------------------Parameters-------------------------------
    !-----------------------------------------------------------------------
    integer jfirst            ! index of first latitude interval
    integer jlast             ! index of last  latitude interval

    !---------------------------Local variables-----------------------------

    integer jj                ! index

    jfirst = 2
    jlast  = jmax+2

    do jj = jfirst,jlast
      call lcbas( phi(jj-1),lbasiy(1,1,jj),lbasiy(1,2,jj) )
    end do

    return
  end subroutine basiy

  subroutine basiz(pkdim ,sig ,lbasiz)

    implicit none

    integer, intent(in)  :: pkdim     ! vertical dimension
    real,    intent(in)  :: sig(pkdim)    ! sigma levels
    real,    intent(out) :: lbasiz(4,2,pkdim)! vertical interpolation weights

    !-----------------------------------------------------------------------
    !
    ! Compute weights used in Lagrange cubic polynomial interpolation in
    ! the central interval of a four point stencil.  Done for each interval
    ! in the unequally spaced height grid.
    !
    !-----------------------------------------------------------------------
    !
    !
    !-----------------------------------------------------------------------

    integer kk                ! index

    do kk = 2,pkdim-2
      call lcbas( sig(kk-1), lbasiz(1,1,kk), lbasiz(1,2,kk) )
    end do

  end subroutine basiz

  subroutine basdz(pkdim ,sig ,lbasdz)
   integer, intent(in)  :: pkdim      ! vertical dimension
   real,    intent(in)  :: sig(pkdim)  ! sigma levels (actually a generic vert.
           ! coordinate)
   real,    intent(out) :: lbasdz(4,2,pkdim) ! vertical interpolation weights

  !-----------------------------------------------------------------------
  !
  ! Compute weights for the calculation of derivative estimates at two
  ! center points of the four point stencil for each interval in the
  ! unequally spaced vertical grid (as defined by the array sig).
  ! Estimates are from differentiating a Lagrange cubic polynomial
  ! through the four point stencil.
  !
  !-----------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------
  !
  !  pkdim   Number of grid points in vertical grid.
  !  sig     Sigma values in the vertical grid.
  !  lbasdz  Weights for derivative estimates based on Lagrange cubic
  !          polynomial on the unequally spaced vertical grid.
  !          If grid interval j is surrounded by a 4 point stencil,
  !          then the derivative at the "top" of the interval (smaller
  !          sigma value) uses the weights lbasdz(1,1,j),lbasdz(2,1,j),
  !          lbasdz(3,1,j), and lbasdz(4,1,j).  The derivative at the
  !          "bottom" of the interval uses lbasdz(1,2,j), lbasdz(2,2,j),
  !          lbasdz(3,2,j), and lbasdz(4,2,j).  (Recall the vertical
  !          level indices increase from the top of the atmosphere
  !          towards the bottom.)
  !
  !-----------------------------------------------------------------------

   integer kk                ! index

   do kk = 2,pkdim-2
    call lcdbas(sig(kk-1), lbasdz(1,1,kk), lbasdz(1,2,kk))
   end do

  end subroutine basdz

  subroutine basdy(phi ,lbasdy)
   real,    intent(in)  :: phi(jmax+4) ! latitude coordinates of model grid
   real,    intent(out) :: lbasdy(4,2,jmax+4) ! derivative estimate weights

  !-----------------------------------------------------------------------
  !
  ! Compute weights for the calculation of derivative estimates at the two
  ! center points of the four point stencil for each interval in the
  ! unequally spaced latitude grid. Estimates are from differentiating
  ! a Lagrange cubic polynomial through the four point stencil.
  !
  !-----------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------

   integer :: jfirst, &    ! index of first latitude interval
   jlast,  &    ! index of last  latitude interval
   jj         ! index
   
   jfirst = 2
   jlast  = jmax+2


  !-----------------------------------------------------------------------
  !
  !  phi     Latitude values in the extended grid.
  !  lbasdy  Weights for derivative estimates based on Lagrange cubic
  !          polynomial on the unequally spaced latitude grid.
  !          If grid interval j (in extended grid) is surrounded by
  !          a 4 point stencil, then the derivative at the "bottom"
  !          of the interval uses the weights lbasdy(1,1,j),
  !          lbasdy(2,1,j), lbasdy(3,1,j), and lbasdy(4,1,j).
  !          The derivative at the "top" of the interval
  !          uses lbasdy(1,2,j), lbasdy(2,2,j), lbasdy(3,2,j),
  !          and lbasdy(4,2,j).
  !
  !
  !-----------------------------------------------------------------------

   do jj = jfirst,jlast
    call lcdbas(phi(jj-1), lbasdy(1,1,jj), lbasdy(1,2,jj))
   end do

  end subroutine basdy


  subroutine lcdbas(grd ,dbas2 ,dbas3)
   real, intent(in)  :: grd(4)   ! grid stencil
   real, intent(out) :: dbas2(4) ! derivatives at grid point 2
   real, intent(out) :: dbas3(4) ! derivatives at grid point 3

  !-----------------------------------------------------------------------
  !
  ! Calculate weights used to evaluate derivative estimates at the
  ! inner grid points of a four point stencil based on Lagrange
  ! cubic polynomial through four unequally spaced points.
  !
  !-----------------------------------------------------------------------
  !
  !  grd    Coordinate values of four points in stencil.
  !  dbas2  Derivatives of the four basis functions at grid point 2.
  !  dbas3  Derivatives of the four basis functions at grid point 3.
  !
  !---------------------------Local variables-----------------------------

   real x1                   !  |
   real x2                   !  |- grid values
   real x3                   !  |
   real x4                   !  |
   real x1mx2                !  |
   real x1mx3                !  |
   real x1mx4                !  |- differences of grid values
   real x2mx3                !  |
   real x2mx4                !  |
   real x3mx4                !  |
  !
  !-----------------------------------------------------------------------
  !

   x1 = grd(1)
   x2 = grd(2)
   x3 = grd(3)
   x4 = grd(4)
   x1mx2 = x1 - x2
   x1mx3 = x1 - x3
   x1mx4 = x1 - x4
   x2mx3 = x2 - x3
   x2mx4 = x2 - x4
   x3mx4 = x3 - x4

   dbas2(1) =   x2mx3 * x2mx4 / ( x1mx2 * x1mx3 * x1mx4 )
   dbas2(2) =   -1./x1mx2 + 1./x2mx3 + 1./x2mx4
   dbas2(3) = - x1mx2 * x2mx4 / ( x1mx3 * x2mx3 * x3mx4 )
   dbas2(4) =   x1mx2 * x2mx3 / ( x1mx4 * x2mx4 * x3mx4 )

   dbas3(1) = - x2mx3 * x3mx4 / ( x1mx2 * x1mx3 * x1mx4 )
   dbas3(2) =   x1mx3 * x3mx4 / ( x1mx2 * x2mx3 * x2mx4 )
   dbas3(3) =   -1./x1mx3 - 1./x2mx3 + 1./x3mx4
   dbas3(4) = - x1mx3 * x2mx3 / ( x1mx4 * x2mx4 * x3mx4 )

  end subroutine lcdbas







  SUBROUTINE InitSL (imaxperj, ibmax, jbmax, imax, jmax, kmax, colrad) 
    INTEGER, INTENT (IN) :: imax, jmax, kmax, ibmax, jbmax
    INTEGER, INTENT (IN) :: imaxperj(jmax)
    REAL,    INTENT (IN) :: colrad (jmax)

    ! local variables 
    INTEGER :: i, j


    ALLOCATE(DeltaLonPerJ(jmax+4)) 
    ALLOCATE(LongitudePerIJ(imax+3, jmax)) 
    ALLOCATE(LatitudePerJ (jmax+4))
    ALLOCATE(DeltaLatPerJ (jmax+3))
    ALLOCATE(coslat(jmax))
    ALLOCATE(deplam(ibmax,kmax,jbmax))
    ALLOCATE(depphi(ibmax,kmax,jbmax))
    ALLOCATE(depsig(ibmax,kmax,jbmax))
    ALLOCATE(deplnplam(ibmax,jbmax))
    ALLOCATE(deplnpphi(ibmax,jbmax))
    ALLOCATE(i_dep(ibmax,4,kmax,jbmax))
    ALLOCATE(j_dep(ibmax,kmax,jbmax))
    ALLOCATE(k_dep(ibmax,kmax,jbmax))
    ALLOCATE(i_deplnp(ibmax,4,jbmax))
    ALLOCATE(j_deplnp(ibmax,jbmax))


    ! defines if momentum eqns are discretized in vector form (true) or not(false)

    tanplane = .TRUE.

    IF (tanplane) THEN
       WRITE(*,"('Tangent Plane operating')")
    ELSE
       WRITE(*,"('Tangent Plane not operating')")
    END IF

    ! need extra storage space in case we use tanplane strategy.

    IF (tanplane) THEN
       ALLOCATE(midphi(ibmax, kmax, jbmax))
       ALLOCATE(alfasl(ibmax,kmax,jbmax))
       ALLOCATE(gamasl(ibmax,kmax,jbmax))
    END IF

    ! defines if momentum eqns are discretized in vector form (true) or not(false)

    ! if departure point falls below first model layer or above last model
    !    layer, project on first or last model layer (true), 
    ! else if it falls below first model level or above last model level, 
    !    project, on first or last model layer (false) 
    project_on_layer =.FALSE.
    IF (project_on_layer) THEN
       WRITE(*,"('Project departure points on layer at top and bottom of atmosphere')")
    ELSE
       WRITE(*,"('Project departure points on level at top and bottom of atmosphere')")
    END IF

    !defines whether we recompute middle (true) for "projected" departures  
    !      or not (false) 
    recompute =.FALSE.
    IF (recompute) THEN
       WRITE(*,"('Recompute middle for projected departures')")
    ELSE
       WRITE(*,"('Do not recompute middle for projected departures')")
    END IF

    ! longitude distribution is not uniform  

    LongitudePerIJ = 0
    DeltaLonPerJ = 0
    DO j = 1, jmax
       DeltaLonPerJ(j+2) = 2*PI/imaxperj(j) 
       DO i = 1, imaxperj(j)+3 
          LongitudePerIJ(i,j)  = (i-2) * DeltaLonPerJ(j+2) 
       END DO
    END DO

    DeltaLonPerJ(1) = DeltaLonPerJ(4)
    DeltaLonPerJ(2) = DeltaLonPerJ(3)
    DeltaLonPerJ(jmax+3) = DeltaLonPerJ(jmax+2)
    DeltaLonPerJ(jmax+4) = DeltaLonPerJ(jmax+1)

    !
    ! change grid points to latitude [-pi/2:pi/2]
    ! positions in the grid are offset by 2 !!! 

    LatitudePerJ(3: jmax+2) = colrad(1:jmax)  - PI/2  

    ! Computes latitude spacing and extends latitude grid for cubic  interpolation

    DO j = 3,  jmax+1
       DeltaLatPerJ(j) = LatitudePerJ(j+1) - LatitudePerJ(j)
    END DO

    ! Recall that the points are symmetric with respect to the equator, 
    ! and we'll set the distance from the last (or first) point to the south
    ! (or north) pole to be half the distance to the next extended grid point. 

    DeltaLatPerJ(jmax+2) =  &
         PI - (LatitudePerJ(jmax+2) - LatitudePerJ(3))

    DeltaLatPerJ(jmax+3) = DeltaLatPerJ(jmax+1)

    DeltaLatPerJ(2)      = DeltaLatPerJ(jmax+2)
    DeltaLatPerJ(1)      = DeltaLatPerJ(jmax+1)

    LatitudePerJ(jmax+3) = LatitudePerJ(jmax+2) + DeltaLatPerJ(jmax+2)
    LatitudePerJ(jmax+4) = LatitudePerJ(jmax+3) + DeltaLatPerJ(jmax+3)
    LatitudePerJ(2)      = LatitudePerJ(3)      - DeltaLatPerJ(2)
    LatitudePerJ(1)      = LatitudePerJ(2)      - DeltaLatPerJ(1)

    ! define  cosine of latitude. 

    COSLAT(1:jmax) = COS(LatitudePerJ(3:jmax+2))


    ! initialize module interpolation 

    CALL CreateInterpolation(DeltaLatPerJ,  DeltaLonPerJ, LongitudePerIJ, &
         LatitudePerJ)

  END SUBROUTINE InitSL

END MODULE SemiLagrangian
