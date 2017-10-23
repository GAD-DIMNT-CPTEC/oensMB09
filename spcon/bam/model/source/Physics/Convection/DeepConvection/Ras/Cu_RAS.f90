!
!  $Author: pkubota $
!  $Date: 2010/04/20 20:18:04 $
!  $Revision: 1.5 $
!
MODULE ModConRas
  USE PhysicalFunctions,Only : fpvs2es5,fpvs

  IMPLICIT NONE

  !                 |Init_Cu_RelAraSch
  !                 |
  !   gwater2-------|
  !                 |
  !                 |RunCu_RelAraSch-------|ras--------|qsat
  !                 |             |           |
  !                 |             |           |cloud-------|acritn
  !                 |             |                        |
  !                 |             |                        |rncl
  !                 |             |rnevp------|qstar9
  !                 |
  !                 |shllcl-------|mstad2
  !                 |
 ! Selecting Kinds
  INTEGER, PARAMETER :: r4 = SELECTED_REAL_KIND(6)  ! Kind for 32-bits Real Numbers
  INTEGER, PARAMETER :: i4 = SELECTED_INT_KIND(9)   ! Kind for 32-bits Integer Numbers
  INTEGER, PARAMETER :: r8 = SELECTED_REAL_KIND(15) ! Kind for 64-bits Real Numbers
  INTEGER, PARAMETER :: i8 = SELECTED_INT_KIND(14)  ! Kind for 64-bits Integer Numbers
  INTEGER, PARAMETER :: r16 = SELECTED_REAL_KIND(15)! Kind for 128-bits Real Numbers


!  --- ...  Geophysics/Astronomy constants

  REAL(kind=r8),PARAMETER:: con_g      =9.80665e+0_r8     ! gravity           (m/s2)

!  --- ...  Thermodynamics constants

  REAL(kind=r8),PARAMETER:: con_cp     =1.0046e+3_r8      ! spec heat air @p    (J/kg/K)
  REAL(kind=r8),PARAMETER:: con_hvap   =2.5000e+6_r8      ! lat heat H2O cond   (J/kg)
  REAL(kind=r8),PARAMETER:: con_rv     =4.6150e+2_r8      ! gas constant H2O    (J/kg/K)
  REAL(kind=r8),PARAMETER:: con_rd     =2.8705e+2_r8      ! gas constant air    (J/kg/K)
  REAL(kind=r8),PARAMETER:: con_t0c    =2.7315e+2_r8      ! temp at 0C          (K)
  REAL(kind=r8),PARAMETER:: con_cvap   =1.8460e+3_r8      ! spec heat H2O gas   (J/kg/K)
  REAL(kind=r8),PARAMETER:: con_cliq   =4.1855e+3_r8      ! spec heat H2O liq   (J/kg/K)

!  Secondary constants

  REAL(kind=r8),PARAMETER:: con_fvirt  =con_rv/con_rd-1.0_r8
  REAL(kind=r8),PARAMETER:: con_eps    =con_rd/con_rv
  REAL(kind=r8),PARAMETER:: con_epsm1  =con_rd/con_rv-1.0_r8
  INTEGER    :: jcap
  PUBLIC :: Init_Cu_RelAraSch
  PUBLIC :: RunCu_RelAraSch
!  PUBLIC :: shllcl

CONTAINS

  SUBROUTINE Init_Cu_RelAraSch(trunc)
   IMPLICIT NONE
   INTEGER , INTENT(IN   ) :: trunc

    jcap=trunc
  END SUBROUTINE Init_Cu_RelAraSch


  SUBROUTINE RunCu_RelAraSch(nCols, KMax,dt,mask2,terr,&
       t2,t3,  q2,ql2,ql3,qi2,qi3, q3,    u2,    v2, omg, prsi_i,prsl_i,phii_i,phil_i, &
       dudt,dvdt,kbot,  ktop,  kuo,raincv ,dtdt ,dqdt ,dqldt,dqidt       )
    IMPLICIT NONE
    INTEGER      , INTENT(IN   ) :: nCols
    INTEGER      , INTENT(IN   ) :: KMax
    REAL(KIND=r8), INTENT(IN   ) :: dt
    INTEGER      , INTENT(IN   ) :: mask2(nCols)! sea -land mask 
    REAL(KIND=r8), INTENT(IN   ) :: terr(nCols)
    REAL(KIND=r8), INTENT(IN   ) :: prsi_i (1:nCols,1:kMax+1)   !interface level pressure Pa
    REAL(KIND=r8), INTENT(IN   ) :: prsl_i (1:nCols,1:kMax  )   !mean  level pressure Pa
    REAL(KIND=r8), INTENT(IN   ) :: phii_i (1:nCols,1:kMax+1)  
    REAL(KIND=r8), INTENT(IN   ) :: phil_i (1:nCols,1:kMax  )  
    REAL(KINd=r8), INTENT(OUT  ) :: dudt(nCols,kMax)
    REAL(KIND=r8), INTENT(OUT  ) :: dvdt(nCols,kMax)

    REAL(KINd=r8), INTENT(OUT  )  :: dtdt (nCols,kMax)
    REAL(KIND=r8), INTENT(OUT  )  :: dqdt (nCols,kMax)
    REAL(KIND=r8), INTENT(OUT  )  :: dqldt(nCols,kMax)
    REAL(KIND=r8), INTENT(OUT  )  :: dqidt(nCols,kMax)

    REAL(KIND=r8), INTENT(IN   ) :: t2 (nCols,kMax)!     tgrs     - real, layer mean temperature ( k )      ix,levs !
    REAL(KIND=r8), INTENT(INOUT) :: t3 (nCols,kMax)!     tgrs     - real, layer mean temperature ( k )      ix,levs !
    REAL(KIND=r8), INTENT(IN   ) :: q2 (nCols,kMax)!     tgrs     - real, layer mean specific humidty ( kg/kg )      ix,levs !
    REAL(KIND=r8), INTENT(INOUT) :: q3 (nCols,kMax)!     tgrs     - real, layer mean specific humidty ( kg/kg )      ix,levs !
    REAL(KIND=r8), INTENT(IN   ) :: ql2 (nCols,kMax)!     tgrs     - real, layer mean cloud liquid water ( kg/kg  )      ix,levs !
    REAL(KIND=r8), INTENT(INOUT) :: ql3 (nCols,kMax)!     tgrs     - real, layer mean cloud liquid water ( kg/kg  )      ix,levs !
    REAL(KIND=r8), INTENT(IN   ) :: qi2 (nCols,kMax)!     tgrs     - real, layer mean cloud ice water  ( kg/kg  )      ix,levs !
    REAL(KIND=r8), INTENT(INOUT) :: qi3 (nCols,kMax)!     tgrs     - real, layer mean cloud ice water ( kg/kg  )      ix,levs !
    REAL(KIND=r8), INTENT(IN   ) :: u2 (nCols,kMax)!     tgrs     - real, layer mean zonal wind ( m/s )      ix,levs !
    REAL(KIND=r8), INTENT(IN   ) :: v2 (nCols,kMax)!     tgrs     - real, layer mean meridional wind ( m/s )      ix,levs !
    REAL(KIND=r8), INTENT(IN   ) :: omg(nCols,kMax)
    INTEGER      , INTENT(INOUT) :: kbot(nCols)
    INTEGER      , INTENT(INOUT) :: ktop(nCols)
    INTEGER      , INTENT(INOUT) :: kuo(nCols)
    REAL(kind=r8), INTENT(OUT  ) :: raincv(nCols)

    REAL(KIND=r8) :: tgrs (nCols,kMax)!     tgrs     - real, layer mean temperature ( k )ix,levs !
    REAL(KIND=r8) :: qgrs (nCols,kMax)!     qgrs     - real, layer mean tracer concentration     ix,levs,ntrac!
    REAL(KIND=r8) :: qliq (nCols,kMax)!     qgrs     - real, layer mean tracer concentrationix,levs,ntrac!
    REAL(KIND=r8) :: qice (nCols,kMax)!     qgrs     - real, layer mean tracer concentrationix,levs,ntrac!

    REAL(KIND=r8) :: ugrs (nCols,kMax)!     ugrs,vgrs- real, u/v component of layer wind ix,levs !
    REAL(KIND=r8) :: vgrs (nCols,kMax)
    REAL(KIND=r8) :: VVEL (nCols,kMax)!     vvel     - real, layer mean vertical velocity (Pa/s)      ix,levs !
    REAL(kind=r8) :: del  (nCols,kMax)  !pa
    REAL(kind=r8) :: pgr  (nCols)  !pa
    REAL(KIND=r8) :: prsi (nCols,kMax+1)!     prsi     - real, pressure at layer interfaces             ix,levs+1
    REAL(KIND=r8) :: prsl (nCols,kMax)!     prsl     - real, mean layer presure                       ix,levs !
    REAL(KIND=r8) :: phii (nCols,kMax+1)
    REAL(KIND=r8) :: phil (nCols,kMax)
    REAL(KIND=r8) :: clw  (nCols,kMax,2)
    REAL(KIND=r8) :: slmsk(nCols)
    REAL(KIND=r8) :: cld1d(nCols)
    REAL(kind=r8) :: ud_mf(nCols,kMax)
    REAL(kind=r8) :: dd_mf(nCols,kMax)
    REAL(kind=r8) :: dt_mf(nCols,kMax)
    REAL(kind=r8) :: dt_in
    INTEGER       :: ntcw=3
    INTEGER, PARAMETER ::   num_p3d=3
    INTEGER, PARAMETER ::   ncld =20 !     ncld     - integer, number of cloud species                  1    !

    INTEGER :: i,k
    !
    dt_in=2*dt
    DO k=1,kMax
       DO i=1,nCols
          dudt(i,k)=0.0_r8
          dvdt(i,k)=0.0_r8
          dtdt (i,k)=0.0_r8
          dqdt (i,k)=0.0_r8
          dqldt(i,k)=0.0_r8
          dqidt(i,k)=0.0_r8
          clw(i,k,1)=0.0_r8
          clw(i,k,2)=0.0_r8
          ud_mf(i,k)=0.0_r8
          dd_mf(i,k)=0.0_r8
          dt_mf(i,k)=0.0_r8
       ENDDO
    ENDDO

    DO i=1,nCols
       kuo (i)=0
       kbot(i)=1
       ktop(i)=1
       raincv(i)=0

    END DO

    DO i=1,nCols
       pgr(i) = PRSI_i(i,1)
       prsi(i,kMax+1) =prsi_i(i,kMax+1)
       phii(i,kMax+1) =(phii_i(i,kMax+1)+terr(i))*con_g
    END DO
    DO k=1,kMax
       DO i=1,nCols
          del(i,k) = PRSI_i(i,k) - PRSI_i(i,k+1)  !Pa
          tgrs (i,k)= t3(i,k) ! layer mean temperature ( k )        K
          ugrs (i,k)= u2 (i,k)
          vgrs (i,k)= v2 (i,k)
          qliq (i,k)= ql3(i,k)
          qice (i,k)= qi3(i,k)
          VVEL (i,k)= omg(i,k)
          prsi (i,k)= prsi_i(i,k)
          prsl (i,k)= prsl_i(i,k)
          phii (i,k)=(phii_i(i,k)+terr(i))*con_g
          phil (i,k)=(phil_i(i,k)+terr(i))*con_g
          dtdt (i,k) =0.0_r8
          dqdt (i,k) =0.0_r8
          dqldt(i,k) =0.0_r8
          dqidt(i,k) =0.0_r8
          dudt (i,k) =0.0_r8
          dvdt (i,k) =0.0_r8
       ENDDO
    ENDDO
    DO k=1,kMax 
       DO i=1, nCols
          qgrs (i,k)= q3(i,k)!qgrs     - real, layer mean tracer concentration     ix,levs,ntrac!
       END DO
    END DO

    !      mask2(i)=0 ! land
    !      mask2(i)=1 ! water/ocean
    DO i=1, nCols
       IF(mask2(i) == 0)THEN
          !land 
          !        slmsk    - real, sea/land/ice mask (=0/1/2)                  im   !
          slmsk(i) = 1
       ELSE
          !        slmsk    - real, sea/land/ice mask (=0/1/2)                  im   !

          slmsk(i) = 0
       END IF
    END DO
    !  --- ...  calling convective parameterization


    IF ( num_p3d == 3 ) THEN    ! call brad ferrier's microphysics
       !  --- ...  algorithm to separate different hydrometeor species

       DO k = 1, kMax
          DO i = 1, nCols
             clw(i,k,1)  = qice(i,k)
             clw(i,k,2)  = qliq(i,k)
             !  --- ...  array to track fraction of "cloud" in the form of ice
          ENDDO
       ENDDO
    ELSE   ! if_num_p3d

       DO k = 1, kMax
          DO i = 1, nCols
             clw(i,k,1) = qgrs(i,k)
          ENDDO
       ENDDO

    ENDIF  ! end if_num_p3d
    !    prepare input, erase output
    !
    DO i=1,nCols
       kuo (i)=0
       kbot(i)=1
       ktop(i)=1
       RAINCV(i)=0.0_r8
    END DO
    !PRINT*,qgrs
    CALL sascnvn(nCols   ,& !integer      , INTENT(IN   ) :: im
         nCols  ,& !integer      , INTENT(IN   ) :: ix
         KMax   ,& !integer      , INTENT(IN   ) :: km
         jcap   ,& !integer      , INTENT(IN   ) :: jcap
         dt_in  ,&   !real(kind=r8), INTENT(IN   ) :: delt	! physics time step in second
         del    ,& !real(kind=r8), INTENT(IN   ) :: delp  (ix,km)  ! delta pressure Pa
         prsl   ,& !real(kind=r8), INTENT(IN   ) :: prslp (ix,km)  ! Pa
         pgr    ,& !real(kind=r8), INTENT(IN   ) :: psp   (im) ! Pessure Pa
         phil   ,& !real(kind=r8), INTENT(IN   ) :: phil  (ix,km)
         clw    ,& !real(kind=r8), INTENT(INOUT) :: ql    (ix,km,2)
         qgrs   ,& !real(kind=r8), INTENT(INOUT) :: q1    (ix,km)
         tgrs   ,& !real(kind=r8), INTENT(INOUT) :: t1    (ix,km)
         ugrs   ,& !real(kind=r8), INTENT(INOUT) :: u1    (ix,km)
         vgrs   ,& !real(kind=r8), INTENT(INOUT) :: v1    (ix,km)
         cld1d  ,& !real(kind=r8), INTENT(OUT  ) :: cldwrk(im)
         raincv ,& !real(kind=r8), INTENT(OUT  ) :: rn    (im)
         kbot   ,& !integer      , INTENT(OUT  ) :: kbot  (im)
         ktop   ,& !integer      , INTENT(OUT  ) :: ktop  (im)
         kuo    ,& !integer      , INTENT(OUT  ) :: kcnv  (im) 
         slmsk  ,& !real(kind=r8), INTENT(IN   ) :: slimsk(im)
         VVEL   ,& !real(kind=r8), INTENT(IN   ) :: dot   (ix,km)! vvel  - real, layer mean vertical velocity (Pa/s)   ix,levs !
         ncld   ,& !integer      , INTENT(IN   ) :: ncloud
         ud_mf  ,& !real(kind=r8), INTENT(OUT  ) :: ud_mf(im,km)
         dd_mf  ,& !real(kind=r8), INTENT(OUT  ) :: dd_mf(im,km)
         dt_mf   )  !real(kind=r8), INTENT(OUT  ) :: dt_mf(im,km)
!!!!!!!!!!!!!hchuang code change 03/03/08 [r1L] add SAS modification of mass flux
!!!!!!!!!!!!!!!!    &                vvel,rann,ncld)
       DO k = 1, kMax
          DO i = 1, nCols
              qice(i,k)= clw(i,k,1)
              qliq(i,k)= clw(i,k,2)  
             !  --- ...  array to track fraction of "cloud" in the form of ice
          ENDDO
       ENDDO

    DO k=1,kMax
       DO i=1, nCols
          IF(RAINCV(i) > 0.0_r8)THEN
            ! PRINT*,tgrs (i,k) ,t3(i,k) , qgrs (i,k), q3 (i,k),  qliq (i,k),ql3(i,k),ugrs(i,k), u2(i,k)
          END IF
       END DO
    END DO   

    DO i = 1, nCols
       raincv(i) =raincv(i)*0.5_r8 ! meters
       IF(RAINCV(i) > 0.0_r8)kuo(i)=1
    ENDDO

    DO k=1,kMax
       DO i=1, nCols
          IF(RAINCV(i) > 0.0_r8)THEN
             dtdt (i,k)=(tgrs (i,k)-t3(i,k))/(dt_in)
             t3(i,k) = tgrs (i,k) !t3 (i,k) + (tgrs (i,k  ) - t2  (i,k))/(2*dt)! layer mean temperature ( k )K

             dqdt (i,k)=(qgrs (i,k)-q3 (i,k))/(dt_in)
             q3 (i,k)= qgrs (i,k)!q3 (i,k) + (qgrs (i,k,1) - q2  (i,k))/(2*dt)

             dqldt(i,k)=(qliq (i,k)-ql3(i,k))/(dt_in)
             ql3(i,k)= qliq (i,k)!ql3(i,k) + (clw  (i,k,2) - qliq(i,k))/(2*dt)

             dqidt(i,k)=(qice (i,k)-qi3(i,k))/(dt_in)
             qi3(i,k)= qice (i,k)!qi3(i,k) + (clw  (i,k,1) - qice(i,k))/(2*dt)
             IF(RAINCV(i) <= 0.300_r8)THEN
                dudt(i,k) =(ugrs(i,k) - u2(i,k))/(dt_in)
                dvdt(i,k) =(vgrs(i,k) - v2(i,k))/(dt_in) 
             END IF     
          END IF
       END DO
    END DO

  END SUBROUTINE RunCu_RelAraSch

  SUBROUTINE sascnvn(im        ,&!integer      , INTENT(IN   ) :: im
                     ix        ,&!integer      , INTENT(IN   ) :: ix
                     km        ,&!integer      , INTENT(IN   ) :: km
                     jcap      ,&!integer      , INTENT(IN   ) :: jcap
                     delt      ,&!real(kind=r8), INTENT(IN   ) :: delt	    ! physics time step in second
                     delp      ,&!real(kind=r8), INTENT(IN   ) :: delp  (ix,km)  ! delta pressure Pa
                     prslp     ,&!real(kind=r8), INTENT(IN   ) :: prslp (ix,km)  ! Pa
                     psp       ,&!real(kind=r8), INTENT(IN   ) :: psp   (im) ! Pessure Pa
                     phil      ,&!real(kind=r8), INTENT(IN   ) :: phil  (ix,km)
                     ql        ,&!real(kind=r8), INTENT(INOUT) :: ql    (ix,km,2)
                     q1        ,&!real(kind=r8), INTENT(INOUT) :: q1    (ix,km)
                     t1        ,&!real(kind=r8), INTENT(INOUT) :: t1    (ix,km)
                     u1        ,&!real(kind=r8), INTENT(INOUT) :: u1    (ix,km)
                     v1        ,&!real(kind=r8), INTENT(INOUT) :: v1    (ix,km)
                     cldwrk    ,&!real(kind=r8), INTENT(OUT  ) :: cldwrk(im)
                     rn        ,&!real(kind=r8), INTENT(OUT  ) :: rn    (im)
                     kbot      ,&!integer      , INTENT(OUT  ) :: kbot  (im)
                     ktop      ,&!integer      , INTENT(OUT  ) :: ktop  (im)
                     kcnv      ,&!integer      , INTENT(OUT  ) :: kcnv  (im) 
                     slimsk    ,&!real(kind=r8), INTENT(IN   ) :: slimsk(im)
                     dot       ,&!real(kind=r8), INTENT(IN   ) :: dot   (ix,km)!     vvel     - real, layer mean vertical velocity (Pa/s)      ix,levs !
                     ncloud    ,&!integer      , INTENT(IN   ) :: ncloud
                     ud_mf     ,&!real(kind=r8), INTENT(OUT  ) :: ud_mf(im,km)
                     dd_mf     ,&!real(kind=r8), INTENT(OUT  ) :: dd_mf(im,km)
                     dt_mf      )!real(kind=r8), INTENT(OUT  ) :: dt_mf(im,km)


!      use machine , only : r8
!      use funcphys , only : fpvs
!      use physcons, con_g => con_g, con_cp => con_cp, con_hvap => con_hvap
!     &,             con_rv => con_rv, con_fvirt => con_fvirt, con_t0c => con_t0c
!     &,             con_cvap => con_cvap, con_cliq => con_cliq
!     &,             con_eps => con_eps, con_epsm1 => con_epsm1
      IMPLICIT NONE
!
      INTEGER      , INTENT(IN   ) :: im
      INTEGER      , INTENT(IN   ) :: ix
      INTEGER      , INTENT(IN   ) :: km
      INTEGER      , INTENT(IN   ) :: jcap
      REAL(kind=r8), INTENT(IN   ) :: delt       ! physics time step in second
      REAL(kind=r8), INTENT(IN   ) :: delp  (ix,km)  ! delta pressure Pa
      REAL(kind=r8), INTENT(IN   ) :: prslp (ix,km)  ! Pa
      REAL(kind=r8), INTENT(IN   ) :: psp   (im) ! Pessure Pa
      REAL(kind=r8), INTENT(IN   ) :: phil  (ix,km)
      REAL(kind=r8), INTENT(INOUT) :: ql    (ix,km,2)
      REAL(kind=r8), INTENT(INOUT) :: q1    (ix,km)
      REAL(kind=r8), INTENT(INOUT) :: t1    (ix,km)
      REAL(kind=r8), INTENT(INOUT) :: u1    (ix,km)
      REAL(kind=r8), INTENT(INOUT) :: v1    (ix,km)
      REAL(kind=r8), INTENT(OUT  ) :: cldwrk(im)
      REAL(kind=r8), INTENT(OUT  ) :: rn    (im)
      INTEGER      , INTENT(OUT  ) :: kbot  (im)
      INTEGER      , INTENT(OUT  ) :: ktop  (im)
      INTEGER      , INTENT(OUT  ) :: kcnv  (im) 
      REAL(kind=r8), INTENT(IN   ) :: slimsk(im)
      REAL(kind=r8), INTENT(IN   ) :: dot   (ix,km)!     vvel     - real, layer mean vertical velocity (Pa/s)      ix,levs !
      INTEGER      , INTENT(IN   ) :: ncloud

! hchuang code change mass flux output

      REAL(kind=r8), INTENT(OUT  ) :: ud_mf(im,km)
      REAL(kind=r8), INTENT(OUT  ) :: dd_mf(im,km)
      REAL(kind=r8), INTENT(OUT  ) :: dt_mf(im,km)
!
!LOCAL VARIABLE
!
      INTEGER :: i
      INTEGER :: j
      INTEGER :: indx
      INTEGER :: jmn
      INTEGER :: k
      INTEGER :: kk
      INTEGER :: latd
      INTEGER :: lond
      INTEGER :: km1
!
      REAL(kind=r8) :: ps    (im)     !cb
      REAL(kind=r8) :: del   (ix,km)
      REAL(kind=r8) :: prsl  (ix,km)
      REAL(kind=r8) :: clam
      REAL(kind=r8) :: cxlamu
      REAL(kind=r8) :: xlamde
      REAL(kind=r8) :: xlamdd
! 
      REAL(kind=r8) :: adw
      REAL(kind=r8) :: aup
      REAL(kind=r8) :: aafac
      REAL(kind=r8) :: beta
      REAL(kind=r8) :: betal
      REAL(kind=r8) :: betas
      REAL(kind=r8) :: dellat
      REAL(kind=r8) :: desdt
      REAL(kind=r8) :: deta
      REAL(kind=r8) :: detad
      REAL(kind=r8) :: dg
      REAL(kind=r8) :: dh
      REAL(kind=r8) :: dhh
      REAL(kind=r8) :: dlnsig
      REAL(kind=r8) :: dp
      REAL(kind=r8) :: dq
      REAL(kind=r8) :: dqsdp
      REAL(kind=r8) :: dqsdt
      REAL(kind=r8) :: dt
      REAL(kind=r8) :: dt2
      REAL(kind=r8) :: dtmax
      REAL(kind=r8) :: dtmin
      REAL(kind=r8) :: dv1h
      REAL(kind=r8) :: dv1q
      REAL(kind=r8) :: dv2h
      REAL(kind=r8) :: dv2q
      REAL(kind=r8) :: dv1u
      REAL(kind=r8) :: dv1v
      REAL(kind=r8) :: dv2u
      REAL(kind=r8) :: dv2v
      REAL(kind=r8) :: dv3q
      REAL(kind=r8) :: dv3h
      REAL(kind=r8) :: dv3u
      REAL(kind=r8) :: dv3v
      REAL(kind=r8) :: dz
      REAL(kind=r8) :: dz1
      REAL(kind=r8) :: e1
      REAL(kind=r8) :: edtmax
      REAL(kind=r8) :: edtmaxl
      REAL(kind=r8) :: edtmaxs
      REAL(kind=r8) :: es
      REAL(kind=r8) :: etah
      REAL(kind=r8) :: evef
      REAL(kind=r8) :: evfact
      REAL(kind=r8) :: evfactl
      REAL(kind=r8) :: factor
      REAL(kind=r8) :: fjcap
      REAL(kind=r8) :: fkm
      REAL(kind=r8) :: gamma
      REAL(kind=r8) :: pprime
      REAL(kind=r8) :: qlk
      REAL(kind=r8) :: qrch
      REAL(kind=r8) :: qs
      REAL(kind=r8) :: rain
      REAL(kind=r8) :: rfact
      REAL(kind=r8) :: shear
      REAL(kind=r8) :: tem1
      REAL(kind=r8) :: tem2
      REAL(kind=r8) :: val
      REAL(kind=r8) :: val1
      REAL(kind=r8) :: val2
      REAL(kind=r8) :: w1
      REAL(kind=r8) :: w1l
      REAL(kind=r8) :: w1s
      REAL(kind=r8) :: w2
      REAL(kind=r8) :: w2l
      REAL(kind=r8) :: w2s
      REAL(kind=r8) :: w3
      REAL(kind=r8) :: w3l
      REAL(kind=r8) :: w3s
      REAL(kind=r8) :: w4
      REAL(kind=r8) :: w4l
      REAL(kind=r8) :: w4s
      REAL(kind=r8) :: xdby
      REAL(kind=r8) :: xpw
      REAL(kind=r8) :: xpwd
      REAL(kind=r8) :: xqrch
      REAL(kind=r8) :: mbdt
      REAL(kind=r8) :: tem
      REAL(kind=r8) :: ptem
      REAL(kind=r8) :: ptem1
      REAL(kind=r8) :: pgcon
!
      INTEGER       :: kb(im)
      INTEGER       :: kbcon(im)
      INTEGER       :: kbcon1(im)
      INTEGER       :: ktcon(im)
      INTEGER       :: ktcon1(im)
      INTEGER       :: jmin(im)
      INTEGER       :: lmin(im)
      INTEGER       :: kbmax(im)
      INTEGER       :: kbm(im)
      INTEGER       :: kmax(im)
!
      REAL(kind=r8) :: aa1(im)
      REAL(kind=r8) :: acrt(im)
      REAL(kind=r8) :: acrtfct(im)
      REAL(kind=r8) :: delhbar(im)
      REAL(kind=r8) :: delq(im)
      REAL(kind=r8) :: delq2(im)
      REAL(kind=r8) :: delqbar(im)
      REAL(kind=r8) :: delqev(im)
      REAL(kind=r8) :: deltbar(im)
      REAL(kind=r8) :: deltv(im)
      REAL(kind=r8) :: dtconv(im)
      REAL(kind=r8) :: edt(im)
      REAL(kind=r8) :: edto(im)
      REAL(kind=r8) :: edtx(im)
      REAL(kind=r8) :: fld(im)
      REAL(kind=r8) :: hcdo(im,km)
      REAL(kind=r8) :: hmax(im)
      REAL(kind=r8) :: hmin(im)
      REAL(kind=r8) :: ucdo(im,km)
      REAL(kind=r8) :: vcdo(im,km)
      REAL(kind=r8) :: aa2(im)
      REAL(kind=r8) :: pbcdif(im)
      REAL(kind=r8) :: pdot(im)
      REAL(kind=r8) :: po(im,km)
      REAL(kind=r8) :: pwavo(im)
      REAL(kind=r8) :: pwevo(im)
      REAL(kind=r8) :: xlamud(im)
      REAL(kind=r8) :: qcdo(im,km)
      REAL(kind=r8) :: qcond(im)
      REAL(kind=r8) :: qevap(im)
      REAL(kind=r8) :: rntot(im)
      REAL(kind=r8) :: vshear(im)
      REAL(kind=r8) :: xaa0(im)
      REAL(kind=r8) :: xk(im)
      REAL(kind=r8) :: xlamd(im)
      REAL(kind=r8) :: xmb(im)
      REAL(kind=r8) :: xmbmax(im)
      REAL(kind=r8) :: xpwav(im)
      REAL(kind=r8) :: xpwev(im)
      REAL(kind=r8) :: delubar(im)
      REAL(kind=r8) :: delvbar(im)
      REAL(kind=r8) :: cincr
!j
!  physical parameters
!      parameter(g=
!  --- ...  Geophysics/Astronomy constants

      REAL(kind=r8), PARAMETER :: cpoel=con_cp/con_hvap
      REAL(kind=r8), PARAMETER :: elocp=con_hvap/con_cp
      
      REAL(kind=r8), PARAMETER :: el2orc=con_hvap*con_hvap/(con_rv*con_cp)
      REAL(kind=r8), PARAMETER :: terr=0.0_r8
      REAL(kind=r8), PARAMETER :: c0=.002_r8
      REAL(kind=r8), PARAMETER :: c1=.002_r8
      REAL(kind=r8), PARAMETER :: delta=con_fvirt
      REAL(kind=r8), PARAMETER :: fact1=(con_cvap-con_cliq)/con_rv
      REAL(kind=r8), PARAMETER :: fact2=con_hvap/con_rv-fact1*con_t0c
      REAL(kind=r8), PARAMETER :: cthk=150.0_r8
      REAL(kind=r8), PARAMETER :: cincrmax=180.0_r8
      REAL(kind=r8), PARAMETER :: cincrmin=120.0_r8
      REAL(kind=r8), PARAMETER :: dthk=25.0_r8
!  local variables and arrays
      REAL(kind=r8) :: pfld(im,km)
      REAL(kind=r8) :: to(im,km)
      REAL(kind=r8) :: qo(im,km)
      REAL(kind=r8) :: uo(im,km)
      REAL(kind=r8) :: vo(im,km)
      REAL(kind=r8) :: qeso(im,km)
!  cloud water
      REAL(kind=r8) :: qlko_ktcon(im)
      REAL(kind=r8) :: dellal(im,km)
      REAL(kind=r8) :: tvo(im,km)
      REAL(kind=r8) :: dbyo(im,km)
      REAL(kind=r8) :: zo(im,km)
      REAL(kind=r8) :: xlamue(im,km)
      REAL(kind=r8) :: fent1(im,km)
      REAL(kind=r8) :: fent2(im,km)
      REAL(kind=r8) :: frh(im,km)
      REAL(kind=r8) :: heo(im,km)
      REAL(kind=r8) :: heso(im,km)
      REAL(kind=r8) :: qrcd(im,km)
      REAL(kind=r8) :: dellah(im,km)
      REAL(kind=r8) :: dellaq(im,km)
      REAL(kind=r8) :: dellau(im,km)
      REAL(kind=r8) :: dellav(im,km)
      REAL(kind=r8) :: hcko(im,km)
      REAL(kind=r8) :: ucko(im,km)
      REAL(kind=r8) :: vcko(im,km)
      REAL(kind=r8) :: qcko(im,km)
      REAL(kind=r8) :: eta(im,km)
      REAL(kind=r8) :: etad(im,km)
      REAL(kind=r8) :: zi(im,km)
      REAL(kind=r8) :: qrcdo(im,km)
      REAL(kind=r8) :: pwo(im,km)
      REAL(kind=r8) :: pwdo(im,km)
      REAL(kind=r8) :: tx1(im)
      REAL(kind=r8) :: sumx(im)
!
      LOGICAL :: totflg
      LOGICAL :: cnvflg(im)
      LOGICAL :: flg(im)
!
      REAL(kind=r8)  :: acrit(15)
!     save pcrit, acritt
      REAL(KIND=r8), PARAMETER :: pcrit(1:15)=(/850.0_r8,800.0_r8,750.0_r8,700.0_r8,650.0_r8,&
                                                600.0_r8,550.0_r8,500.0_r8,450.0_r8,400.0_r8,&
                                                350.0_r8,300.0_r8,250.0_r8,200.0_r8,150.0_r8/)
      REAL(KIND=r8), PARAMETER :: acritt(1:15)=(/.0633_r8,.0445_r8,.0553_r8, .0664_r8, .0750_r8,&
                                                 .1082_r8,.1521_r8,.2216_r8, .3151_r8, .3677_r8,&
                                                 .4100_r8,.5255_r8,.7663_r8,1.1686_r8,1.6851_r8/)
!     gdas derived acrit
!     data acritt/.203,.515,.521,.566,.625,.665,.659,.688,
!    &            .743,.813,.886,.947,1.138,1.377,1.896/
      REAL(kind=r8), PARAMETER :: tf=233.16_r8
      REAL(kind=r8), PARAMETER :: tcr=263.16_r8
      REAL(kind=r8), PARAMETER :: tcrf=1.0_r8/(tcr-tf)
      !parameter (tf=233.16, tcr=263.16, tcrf=1.0/(tcr-tf))
!
!-----------------------------------------------------------------------
      cldwrk =0.0_r8
      ud_mf=0.0_r8
      dd_mf=0.0_r8
      dt_mf=0.0_r8
      ps    =0.0_r8    !cb
      del   =0.0_r8
      prsl  =0.0_r8
      clam =0.0_r8
      cxlamu =0.0_r8
      xlamde =0.0_r8
      xlamdd =0.0_r8
! 
      adw =0.0_r8
      aup =0.0_r8
      aafac =0.0_r8
      beta =0.0_r8
      betal =0.0_r8
      betas =0.0_r8
      dellat =0.0_r8
      desdt =0.0_r8
      deta =0.0_r8
      detad =0.0_r8
      dg =0.0_r8
      dh =0.0_r8
      dhh =0.0_r8
      dlnsig =0.0_r8
      dp =0.0_r8
      dq =0.0_r8
      dqsdp =0.0_r8
      dqsdt =0.0_r8
      dt =0.0_r8
      dt2 =0.0_r8
      dtmax =0.0_r8
      dtmin =0.0_r8
      dv1h =0.0_r8
      dv1q =0.0_r8
      dv2h =0.0_r8
      dv2q =0.0_r8
      dv1u =0.0_r8
      dv1v =0.0_r8
      dv2u =0.0_r8
      dv2v =0.0_r8
      dv3q =0.0_r8
      dv3h =0.0_r8
      dv3u =0.0_r8
      dv3v =0.0_r8
      dz =0.0_r8
      dz1 =0.0_r8
      e1 =0.0_r8
      edtmax =0.0_r8
      edtmaxl =0.0_r8
      edtmaxs =0.0_r8
      es =0.0_r8
      etah =0.0_r8
      evef =0.0_r8
      evfact =0.0_r8
      evfactl =0.0_r8
      factor =0.0_r8
      fjcap =0.0_r8
      fkm =0.0_r8
      gamma =0.0_r8
      pprime =0.0_r8
      qlk =0.0_r8
      qrch =0.0_r8
      qs =0.0_r8
      rain =0.0_r8
      rfact =0.0_r8
      shear =0.0_r8
      tem1 =0.0_r8
      tem2 =0.0_r8
      val =0.0_r8
      val1 =0.0_r8
      val2 =0.0_r8
      w1 =0.0_r8
      w1l =0.0_r8
      w1s =0.0_r8
      w2 =0.0_r8
      w2l =0.0_r8
      w2s =0.0_r8
      w3 =0.0_r8
      w3l =0.0_r8
      w3s =0.0_r8
      w4 =0.0_r8
      w4l =0.0_r8
      w4s =0.0_r8
      xdby =0.0_r8
      xpw =0.0_r8
      xpwd =0.0_r8
      xqrch =0.0_r8
      mbdt =0.0_r8
      tem =0.0_r8
      ptem =0.0_r8
      ptem1 =0.0_r8
      pgcon =0.0_r8
      kb =0
      kbcon =0
      kbcon1 =0
      ktcon =0
      ktcon1 =0
      jmin =0
      lmin =0
      kbmax =0
      kbm =0
      kmax =0
      aa1=0.0_r8
      acrt=0.0_r8
      acrtfct=0.0_r8
      delhbar=0.0_r8
      delq=0.0_r8
      delq2=0.0_r8
  delqbar=0.0_r8
  delqev=0.0_r8
  deltbar=0.0_r8
  deltv=0.0_r8
  dtconv=0.0_r8
  edt=0.0_r8
  edto=0.0_r8
  edtx=0.0_r8
  fld=0.0_r8
  hcdo=0.0_r8
  hmax=0.0_r8
  hmin=0.0_r8
  ucdo=0.0_r8
  vcdo=0.0_r8
  aa2=0.0_r8
  pbcdif=0.0_r8
  pdot=0.0_r8
  po=0.0_r8
  pwavo=0.0_r8
  pwevo=0.0_r8
  xlamud=0.0_r8
  qcdo=0.0_r8
  qcond=0.0_r8
  qevap=0.0_r8
  rntot=0.0_r8
  vshear=0.0_r8
  xaa0=0.0_r8
  xk=0.0_r8
  xlamd=0.0_r8
  xmb=0.0_r8
  xmbmax=0.0_r8
  xpwav=0.0_r8
  xpwev=0.0_r8
  delubar=0.0_r8
  delvbar=0.0_r8
  cincr=0.0_r8

 pfld=0.0_r8
 to=0.0_r8
 qo=0.0_r8
 uo=0.0_r8
 vo=0.0_r8
 qeso=0.0_r8

 qlko_ktcon=0.0_r8
 dellal=0.0_r8
 tvo=0.0_r8
 dbyo=0.0_r8
 zo=0.0_r8
 xlamue=0.0_r8
 fent1=0.0_r8
 fent2=0.0_r8
 frh=0.0_r8
 heo=0.0_r8
 heso=0.0_r8
 qrcd=0.0_r8
 dellah=0.0_r8
 dellaq=0.0_r8
 dellau=0.0_r8
 dellav=0.0_r8
 hcko=0.0_r8
 ucko=0.0_r8
 vcko=0.0_r8
 qcko=0.0_r8
 eta=0.0_r8; etad=0.0_r8; zi=0.0_r8; qrcdo=0.0_r8; pwo=0.0_r8; pwdo=0.0_r8; tx1=0.0_r8; sumx=0.0_r8
!
!************************************************************************
!     convert input Pa terms to Cb terms  -- Moorthi
      ps   = psp   * 0.001_r8
      prsl = prslp * 0.001_r8
      del  = delp  * 0.001_r8



!************************************************************************
!
!
      km1 = km - 1
!
!  initialize arrays
!
      DO i=1,im
        kcnv(i)   =0
        cnvflg(i) = .TRUE.
        rn(i)=0.0_r8
        kbot(i)=km+1
        ktop(i)=0
        kbcon(i)=km
        ktcon(i)=1
        dtconv(i) = 3600.0_r8
        cldwrk(i) = 0.0_r8
        pdot(i) = 0.0_r8
        pbcdif(i)= 0.0_r8
        lmin(i) = 1
        jmin(i) = 1
        qlko_ktcon(i) = 0.0_r8
        edt(i)  = 0.0_r8
        edto(i) = 0.0_r8
        edtx(i) = 0.0_r8
        acrt(i) = 0.0_r8
        acrtfct(i) = 1.0_r8
        aa1(i)  = 0.0_r8
        aa2(i)  = 0.0_r8
        xaa0(i) = 0.0_r8
        pwavo(i)= 0.0_r8
        pwevo(i)= 0.0_r8
        xpwav(i)= 0.0_r8
        xpwev(i)= 0.0_r8
        vshear(i) = 0.0_r8
      ENDDO
! hchuang code change
      DO k = 1, km
        DO i = 1, im
          ud_mf(i,k) = 0.0_r8
          dd_mf(i,k) = 0.0_r8
          dt_mf(i,k) = 0.0_r8
        ENDDO
      ENDDO
!
      DO k = 1, 15
        acrit(k) = acritt(k) * (975.0_r8 - pcrit(k))
      ENDDO
      dt2 = delt
      val   =         1200.0_r8
      dtmin = MAX(dt2, val )
      val   =         3600.0_r8
      dtmax = MAX(dt2, val )
!  model tunable parameters are all here
      mbdt    = 10.0_r8
      edtmaxl = .3_r8
      edtmaxs = .3_r8
      clam    = .1_r8
      aafac   = .1_r8
!     betal   = .15_r8
!     betas   = .15_r8
      betal   = .05_r8
      betas   = .05_r8
!     evef    = 0.07_r8
      evfact  = 0.3_r8
      evfactl = 0.3_r8
!
      cxlamu  = 1.0e-4_r8
      xlamde  = 1.0e-4_r8
      xlamdd  = 1.0e-4_r8
!
      IF(jcap <= 299)THEN
         pgcon   = 0.55_r8    ! Zhang & Wu (2003,JAS)
      ELSE
         !pk pgcon   = 0.7_r8     ! Gregory et al. (1997, QJRMS)
         pgcon   = 0.55_r8         ! kubota et al. (2016, CPTEC)
      END IF
      fjcap   = (float(jcap) / 126.0_r8) ** 2
      val     =           1.0_r8
      fjcap   = MAX(fjcap,val)
      fkm     = (float(km) / 28.0) ** 2
      fkm     = MAX(fkm,val)

      w1l     = -8e-3_r8 
      w2l     = -4.e-2_r8
      w3l     = -5.e-3_r8 
      w4l     = -5.e-4_r8

      w1s     = -2.e-4_r8
      w2s     = -2.e-3_r8
      w3s     = -1.e-3_r8
      w4s     = -2.e-5_r8
!
!  define top layer for search of the downdraft originating layer
!  and the maximum thetae for updraft
!
      DO i=1,im
        kbmax(i) = km
        kbm(i)   = km
        kmax(i)  = km
        tx1(i)   = 1.0_r8 / ps(i)
      ENDDO
!     
      DO k = 1, km
        DO i=1,im
          IF (prsl(i,k)*tx1(i) .GT. 0.04_r8) kmax(i)  = k + 1
          IF (prsl(i,k)*tx1(i) .GT. 0.45_r8) kbmax(i) = k + 1
          IF (prsl(i,k)*tx1(i) .GT. 0.70_r8) kbm(i)   = k + 1
        ENDDO
      ENDDO
      DO i=1,im
      !  PRINT*,'1' ,kmax(i),kbmax(i),kbm(i)
        kmax(i)  = MIN(km      ,kmax(i))
        kbmax(i) = MIN(kbmax(i),kmax(i))
        kbm(i)   = MIN(kbm(i)  ,kmax(i))
       ! PRINT*,'2' ,kmax(i),kbmax(i),kbm(i)
      ENDDO
!
!  hydrostatic height assume zero terr and initially assume
!    updraft entrainment rate as an inverse function of height 
!
      DO k = 1, km
        DO i=1,im
          zo(i,k) = phil(i,k) / con_g
        ENDDO
      ENDDO
      DO k = 1, km1
        DO i=1,im
          zi(i,k) = 0.5_r8*(zo(i,k)+zo(i,k+1))
          xlamue(i,k) = clam / zi(i,k)
        ENDDO
      ENDDO
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   convert surface pressure to mb from cb
!
      DO k = 1, km
        DO i = 1, im
          IF (k .LE. kmax(i)) THEN
            pfld(i,k) = prsl(i,k) * 10.0_r8
            eta(i,k)  = 1.0_r8
            fent1(i,k)= 1.0_r8
            fent2(i,k)= 1.0_r8
            frh(i,k)  = 0.0_r8
            hcko(i,k) = 0.0_r8
            qcko(i,k) = 0.0_r8
            ucko(i,k) = 0.0_r8
            vcko(i,k) = 0.0_r8
            etad(i,k) = 1.0_r8
            hcdo(i,k) = 0.0_r8
            qcdo(i,k) = 0.0_r8
            ucdo(i,k) = 0.0_r8
            vcdo(i,k) = 0.0_r8
            qrcd(i,k) = 0.0_r8
            qrcdo(i,k)= 0.0_r8
            dbyo(i,k) = 0.0_r8
            pwo(i,k)  = 0.0_r8
            pwdo(i,k) = 0.0_r8
            dellal(i,k) = 0.0_r8
            to(i,k)   = t1(i,k)
            qo(i,k)   = q1(i,k)
            uo(i,k)   = u1(i,k)
            vo(i,k)   = v1(i,k)
!           uo(i,k)   = u1(i,k) * rcs(i)
!           vo(i,k)   = v1(i,k) * rcs(i)
          ENDIF
        ENDDO
      ENDDO
!
!  column variables
!  p is pressure of the layer (mb)
!  t is temperature at t-dt (k)..tn
!  q is mixing ratio at t-dt (kg/kg)..qn
!  to is temperature at t+dt (k)... this is after advection and turbulan
!  qo is mixing ratio at t+dt (kg/kg)..q1
!
      DO k = 1, km
        DO i=1,im
          IF (k .LE. kmax(i)) THEN
            qeso(i,k) = 0.01_r8 * fpvs2es5(to(i,k))      ! fpvs is in pa
           !PK qeso(i,k) = 0.01_r8 * fpvs(to(i,k))      ! fpvs is in pa
            qeso(i,k) = con_eps * qeso(i,k) / (pfld(i,k) + con_epsm1*qeso(i,k))
            val1      =             1.e-8_r8
            qeso(i,k) = MAX(qeso(i,k), val1)
            val2      =           1.e-10_r8
            qo(i,k)   = MAX(qo(i,k), val2 )
!           qo(i,k)   = min(qo(i,k),qeso(i,k))
!           tvo(i,k)  = to(i,k) + delta * to(i,k) * qo(i,k)
          ENDIF
        ENDDO
      ENDDO
!
!  compute moist static energy
!
      DO k = 1, km
        DO i=1,im
          IF (k .LE. kmax(i)) THEN
!           tem       = con_g * zo(i,k) + con_cp * to(i,k)
            tem       = phil(i,k) + con_cp * to(i,k)
            heo(i,k)  = tem  + con_hvap * qo(i,k)
            heso(i,k) = tem  + con_hvap * qeso(i,k)
!           heo(i,k)  = min(heo(i,k),heso(i,k))
          ENDIF
        ENDDO
      ENDDO
!
!  determine level with largest moist static energy
!  this is the level where updraft starts
!
      DO i=1,im
        hmax(i) = heo(i,1)
        kb(i)   = 1
      ENDDO
      DO k = 2, km
        DO i=1,im
          IF (k .LE. kbm(i)) THEN
            IF(heo(i,k).GT.hmax(i)) THEN
              kb(i)   = k
              hmax(i) = heo(i,k)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
      DO k = 1, km1
        DO i=1,im
          IF (k .LE. kmax(i)-1) THEN
            dz      = 0.5_r8 * (zo(i,k+1) - zo(i,k))
            dp      = 0.5_r8 * (pfld(i,k+1) - pfld(i,k))
            es      = 0.01_r8 * fpvs2es5(to(i,k+1))      ! fpvs is in pa
!PK            es      = 0.01_r8 * fpvs(to(i,k+1))      ! fpvs is in pa
            pprime  = pfld(i,k+1) + con_epsm1 * es
            qs      = con_eps * es / pprime
            dqsdp   = - qs / pprime
            desdt   = es * (fact1 / to(i,k+1) + fact2 / (to(i,k+1)**2))
            dqsdt   = qs * pfld(i,k+1) * desdt / (es * pprime)
            gamma   = el2orc * qeso(i,k+1) / (to(i,k+1)**2)
            dt      = (con_g * dz + con_hvap * dqsdp * dp) / (con_cp * (1.0_r8 + gamma))
            dq      = dqsdt * dt + dqsdp * dp
            to(i,k) = to(i,k+1) + dt
            qo(i,k) = qo(i,k+1) + dq
            po(i,k) = 0.5_r8 * (pfld(i,k) + pfld(i,k+1))
          ENDIF
        ENDDO
      ENDDO
!
      DO k = 1, km1
        DO i=1,im
          IF (k .LE. kmax(i)-1) THEN
            qeso(i,k) = 0.01_r8 * fpvs2es5(to(i,k))      ! fpvs is in pa
!PK            qeso(i,k) = 0.01_r8 * fpvs(to(i,k))      ! fpvs is in pa

            qeso(i,k) = con_eps * qeso(i,k) / (po(i,k) + con_epsm1*qeso(i,k))
            val1      =             1.e-8_r8
            qeso(i,k) = MAX(qeso(i,k), val1)
            val2      =           1.e-10_r8
            qo(i,k)   = MAX(qo(i,k), val2 )
!           qo(i,k)   = min(qo(i,k),qeso(i,k))
            frh(i,k)  = 1.0_r8 - MIN(qo(i,k)/qeso(i,k), 1.0_r8)
            heo(i,k)  = 0.5_r8 * con_g * (zo(i,k) + zo(i,k+1)) +    &
                        con_cp * to(i,k) + con_hvap * qo(i,k)
            heso(i,k) = 0.5_r8 * con_g * (zo(i,k) + zo(i,k+1)) +    &
                        con_cp * to(i,k) + con_hvap * qeso(i,k)
            uo(i,k)   = 0.5_r8 * (uo(i,k) + uo(i,k+1))
            vo(i,k)   = 0.5_r8 * (vo(i,k) + vo(i,k+1))
          ENDIF
        ENDDO
      ENDDO
!
!  look for the level of free convection as cloud base
!
      DO i=1,im
        flg(i)   = .TRUE.
        kbcon(i) = kmax(i)
      ENDDO
      DO k = 1, km1
        DO i=1,im
          IF (flg(i).AND.k.LE.kbmax(i)) THEN
            !  PRINT*,k,kb(i),heo(i,kb(i)),heso(i,k)

            IF(k.GT.kb(i).AND.heo(i,kb(i)).GT.heso(i,k)) THEN
              kbcon(i) = k
              flg(i)   = .FALSE.
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
      DO i=1,im
        IF(kbcon(i).EQ.kmax(i)) cnvflg(i) = .FALSE.
      ENDDO
!!
      totflg = .TRUE.
      DO i=1,im
        totflg = totflg .AND. (.NOT. cnvflg(i))
! PRINT*,totflg,cnvflg(i)
      ENDDO
      IF(totflg) RETURN
!!
!
!  determine critical convective inhibition
!  as a function of vertical velocity at cloud base.
!
      DO i=1,im
        IF(cnvflg(i)) THEN
!         pdot(i)  = 10.0_r8* dot(i,kbcon(i))
!PK          pdot(i)  = 0.01_r8 * dot(i,kbcon(i)) ! Now dot is in Pa/s
          pdot(i)  =  dot(i,kbcon(i)) ! Now dot is in Pa/s

        ENDIF
      ENDDO
      DO i=1,im
        IF(cnvflg(i)) THEN
          IF(slimsk(i).EQ.1.0_r8) THEN
         !ocean/seaice
            w1 = w1l
            w2 = w2l
            w3 = w3l
            w4 = w4l
          ELSE
            w1 = w1s
            w2 = w2s
            w3 = w3s
            w4 = w4s
          ENDIF
          IF(pdot(i).LE.w4) THEN
            tem =   (pdot(i) - w4) / (w3 - w4)
          ELSEIF(pdot(i).GE.-w4) THEN
            tem = - (pdot(i) + w4) / (w4 - w3)
          ELSE
            tem = 0.05_r8
          ENDIF
          val1    =             -1.0_r8
          tem = MAX(tem,val1)
          val2    =             1.0_r8
          tem = MIN(tem,val2)
          tem = 1.0_r8 - tem
          tem1= 0.5_r8*(cincrmax-cincrmin)
          cincr = cincrmax - tem * tem1
          pbcdif(i) = pfld(i,kb(i)) - pfld(i,kbcon(i))
          IF(pbcdif(i).GT.cincr) THEN
             cnvflg(i) = .FALSE.
          ENDIF
        ENDIF
      ENDDO
!!
      totflg = .TRUE.
      DO i=1,im
        totflg = totflg .AND. (.NOT. cnvflg(i))
      ENDDO
      IF(totflg) RETURN
!!
!
!  assume that updraft entrainment rate above cloud base is
!    same as that at cloud base
!
      DO k = 2, km1
        DO i=1,im
          IF(cnvflg(i).AND.  &
            (k.GT.kbcon(i).AND.k.LT.kmax(i))) THEN
              xlamue(i,k) = xlamue(i,kbcon(i))
          ENDIF
        ENDDO
      ENDDO
!
!  assume the detrainment rate for the updrafts to be same as
!  the entrainment rate at cloud base
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          xlamud(i) = xlamue(i,kbcon(i))
        ENDIF
      ENDDO
!
!  functions rapidly decreasing with height, mimicking a cloud ensemble
!    (Bechtold et al., 2008)
!
      DO k = 2, km1
        DO i=1,im
          IF(cnvflg(i).AND.   &
            (k.GT.kbcon(i).AND.k.LT.kmax(i))) THEN
              tem = qeso(i,k)/qeso(i,kbcon(i))
              fent1(i,k) = tem**2
              fent2(i,k) = tem**3
          ENDIF
        ENDDO
      ENDDO
!
!  final entrainment rate as the sum of turbulent part and organized entrainment
!    depending on the environmental relative humidity
!    (Bechtold et al., 2008)
!
      DO k = 2, km1
        DO i=1,im
          IF(cnvflg(i).AND.  &
            (k.GE.kbcon(i).AND.k.LT.kmax(i))) THEN
              tem = cxlamu * frh(i,k) * fent2(i,k)
              xlamue(i,k) = xlamue(i,k)*fent1(i,k) + tem
          ENDIF
        ENDDO
      ENDDO
!
!  determine updraft mass flux for the subcloud layers
!
      DO k = km1, 1, -1
        DO i = 1, im
          IF (cnvflg(i)) THEN
            IF(k.LT.kbcon(i).AND.k.GE.kb(i)) THEN
              dz       = zi(i,k+1) - zi(i,k)
              ptem     = 0.5_r8*(xlamue(i,k)+xlamue(i,k+1))-xlamud(i)
              eta(i,k) = eta(i,k+1) / (1.0_r8 + ptem * dz)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!  compute mass flux above cloud base
!
      DO k = 2, km1
        DO i = 1, im
         IF(cnvflg(i))THEN
           IF(k.GT.kbcon(i).AND.k.LT.kmax(i)) THEN
              dz       = zi(i,k) - zi(i,k-1)
              ptem     = 0.5_r8*(xlamue(i,k)+xlamue(i,k-1))-xlamud(i)
              eta(i,k) = eta(i,k-1) * (1 + ptem * dz)
           ENDIF
         ENDIF
        ENDDO
      ENDDO
!
!  compute updraft cloud properties
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          indx         = kb(i)
          hcko(i,indx) = heo(i,indx)
          ucko(i,indx) = uo(i,indx)
          vcko(i,indx) = vo(i,indx)
          pwavo(i)     = 0.0_r8
        ENDIF
      ENDDO
!
!  cloud property is modified by the entrainment process
!
      DO k = 2, km1
        DO i = 1, im
          IF (cnvflg(i)) THEN
            IF(k.GT.kb(i).AND.k.LT.kmax(i)) THEN
              dz   = zi(i,k) - zi(i,k-1)
              tem  = 0.5_r8 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.5_r8 * xlamud(i) * dz
              factor = 1.0_r8 + tem - tem1
              ptem = 0.5_r8 * tem + pgcon
              ptem1= 0.5_r8 * tem - pgcon
              hcko(i,k) = ((1.0_r8-tem1)*hcko(i,k-1)+tem*0.5_r8*   &
                          (heo(i,k)+heo(i,k-1)))/factor
              ucko(i,k) = ((1.0_r8-tem1)*ucko(i,k-1)+ptem*uo(i,k)  &
                          +ptem1*uo(i,k-1))/factor
              vcko(i,k) = ((1.0_r8-tem1)*vcko(i,k-1)+ptem*vo(i,k)  &
                          +ptem1*vo(i,k-1))/factor
              dbyo(i,k) = hcko(i,k) - heso(i,k)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!   taking account into convection inhibition due to existence of
!    dry layers below cloud base
!
      DO i=1,im
        flg(i) = cnvflg(i)
        kbcon1(i) = kmax(i)
      ENDDO
      DO k = 2, km1
      DO i=1,im
        IF (flg(i).AND.k.LT.kmax(i)) THEN
          IF(k.GE.kbcon(i).AND.dbyo(i,k).GT.0.0_r8) THEN
            kbcon1(i) = k
            flg(i)    = .FALSE.
          ENDIF
        ENDIF
      ENDDO
      ENDDO
      DO i=1,im
        IF(cnvflg(i)) THEN
          IF(kbcon1(i).EQ.kmax(i)) cnvflg(i) = .FALSE.
        ENDIF
      ENDDO
      DO i=1,im
        IF(cnvflg(i)) THEN
          tem = pfld(i,kbcon(i)) - pfld(i,kbcon1(i))
          IF(tem.GT.dthk) THEN
             cnvflg(i) = .FALSE.
          ENDIF
        ENDIF
      ENDDO
!!
      totflg = .TRUE.
      DO i = 1, im
        totflg = totflg .AND. (.NOT. cnvflg(i))
      ENDDO
      IF(totflg) RETURN
!!
!
!  determine first guess cloud top as the level of zero buoyancy
!
      DO i = 1, im
        flg(i) = cnvflg(i)
        ktcon(i) = 1
      ENDDO
      DO k = 2, km1
      DO i = 1, im
        IF (flg(i).AND.k .LT. kmax(i)) THEN
          IF(k.GT.kbcon1(i).AND.dbyo(i,k).LT.0.0_r8) THEN
             ktcon(i) = k
             flg(i)   = .FALSE.
          ENDIF
        ENDIF
      ENDDO
      ENDDO
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          tem = pfld(i,kbcon(i))-pfld(i,ktcon(i))
          IF(tem.LT.cthk) cnvflg(i) = .FALSE.
        ENDIF
      ENDDO
!!
      totflg = .TRUE.
      DO i = 1, im
        totflg = totflg .AND. (.NOT. cnvflg(i))
      ENDDO
      IF(totflg) RETURN
!!
!
!  search for downdraft originating level above theta-e minimum
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
           hmin(i) = heo(i,kbcon1(i))
           lmin(i) = kbmax(i)
           jmin(i) = kbmax(i)
        ENDIF
      ENDDO
      DO k = 2, km1
        DO i = 1, im
          IF (cnvflg(i) .AND. k .LE. kbmax(i)) THEN
            IF(k.GT.kbcon1(i).AND.heo(i,k).LT.hmin(i)) THEN
               lmin(i) = k + 1
               hmin(i) = heo(i,k)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!  make sure that jmin(i) is within the cloud
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          jmin(i) = MIN(lmin(i),ktcon(i)-1)
          jmin(i) = MAX(jmin(i),kbcon1(i)+1)
          IF(jmin(i).GE.ktcon(i)) cnvflg(i) = .FALSE.
        ENDIF
      ENDDO
!
!  specify upper limit of mass flux at cloud base
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
!         xmbmax(i) = .1
!
          k = kbcon(i)
          dp = 1000.0_r8 * del(i,k)
          xmbmax(i) = dp / (con_g * dt2)
!
!         tem = dp / (con_g * dt2)
!         xmbmax(i) = min(tem, xmbmax(i))
        ENDIF
      ENDDO
!
!  compute cloud moisture property and precipitation
!
      DO i = 1, im
        IF (cnvflg(i)) THEN
          aa1(i) = 0.0_r8
          qcko(i,kb(i)) = qo(i,kb(i))
!         rhbar(i) = 0.0_r8
        ENDIF
      ENDDO
      DO k = 2, km1
        DO i = 1, im
          IF (cnvflg(i)) THEN
            IF(k.GT.kb(i).AND.k.LT.ktcon(i)) THEN
              dz    = zi(i,k) - zi(i,k-1)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              qrch = qeso(i,k)                                              &
                  + gamma * dbyo(i,k) / (con_hvap * (1.0_r8 + gamma))
!j
              tem  = 0.5_r8 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.5_r8 * xlamud(i) * dz
              factor = 1.0_r8 + tem - tem1
              qcko(i,k) = ((1.0_r8-tem1)*qcko(i,k-1)+tem*0.5_r8*                   &
                           (qo(i,k)+qo(i,k-1)))/factor
!j
              dq = eta(i,k) * (qcko(i,k) - qrch)
!
!             rhbar(i) = rhbar(i) + qo(i,k) / qeso(i,k)
!
!  check if there is excess moisture to release latent heat
!
              IF(k.GE.kbcon(i).AND.dq.GT.0.0_r8) THEN
                etah = 0.5_r8 * (eta(i,k) + eta(i,k-1))
                IF(ncloud.GT.0.0_r8.AND.k.GT.jmin(i)) THEN
                  dp = 1000.0_r8 * del(i,k)
                  qlk = dq / (eta(i,k) + etah * (c0 + c1) * dz)
                  dellal(i,k) = etah * c1 * dz * qlk * con_g / dp
                ELSE
                  qlk = dq / (eta(i,k) + etah * c0 * dz)
                ENDIF
                aa1(i) = aa1(i) - dz * con_g * qlk
                qcko(i,k) = qlk + qrch
                pwo(i,k) = etah * c0 * dz * qlk
                pwavo(i) = pwavo(i) + pwo(i,k)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!     do i = 1, im
!       if(cnvflg(i)) then
!         indx = ktcon(i) - kb(i) - 1
!         rhbar(i) = rhbar(i) / float(indx)
!       endif
!     enddo
!
!  calculate cloud work function
!
      DO k = 2, km1
        DO i = 1, im
          IF (cnvflg(i)) THEN
            IF(k.GE.kbcon(i).AND.k.LT.ktcon(i)) THEN
              dz1 = zo(i,k+1) - zo(i,k)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              rfact =  1.0_r8 + delta * con_cp * gamma         &
                       * to(i,k) / con_hvap
              aa1(i) = aa1(i) +                            &
                       dz1 * (con_g / (con_cp * to(i,k)))  &
                       * dbyo(i,k) / (1.0_r8 + gamma)          &
                       * rfact
              val = 0.0_r8
              aa1(i)=aa1(i)+                               &
                       dz1 * con_g * delta *               &
                       MAX(val,(qeso(i,k) - qo(i,k)))
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      DO i = 1, im
        IF(cnvflg(i).AND.aa1(i).LE.0.0_r8) cnvflg(i) = .FALSE.
      ENDDO
!!
      totflg = .TRUE.
      DO i=1,im
        totflg = totflg .AND. (.NOT. cnvflg(i))
      ENDDO
      IF(totflg) RETURN
!!
!
!  estimate the onvective overshooting as the level 
!    where the [aafac * cloud work function] becomes zero,
!    which is the final cloud top
!
      DO i = 1, im
        IF (cnvflg(i)) THEN
          aa2(i) = aafac * aa1(i)
        ENDIF
      ENDDO
!
      DO i = 1, im
        flg(i) = cnvflg(i)
        ktcon1(i) = kmax(i) - 1
      ENDDO
      DO k = 2, km1
        DO i = 1, im
          IF (flg(i)) THEN
            IF(k.GE.ktcon(i).AND.k.LT.kmax(i)) THEN
              dz1 = zo(i,k+1) - zo(i,k)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              rfact =  1.0_r8 + delta * con_cp * gamma            &
                       * to(i,k) / con_hvap
              aa2(i) = aa2(i) +                               & 
                       dz1 * (con_g / (con_cp * to(i,k)))     &
                       * dbyo(i,k) / (1.0_r8 + gamma)             & 
                       * rfact
              IF(aa2(i).LT.0.0_r8) THEN
                ktcon1(i) = k
                flg(i) = .FALSE.
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!  compute cloud moisture property, detraining cloud water 
!    and precipitation in overshooting layers 
!
      DO k = 2, km1
        DO i = 1, im
          IF (cnvflg(i)) THEN
            IF(k.GE.ktcon(i).AND.k.LT.ktcon1(i)) THEN
              dz    = zi(i,k) - zi(i,k-1)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              qrch = qeso(i,k)                                        &
                   + gamma * dbyo(i,k) / (con_hvap * (1.0_r8 + gamma))
!j
              tem  = 0.5_r8 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.5_r8 * xlamud(i) * dz
              factor = 1.0_r8 + tem - tem1
              qcko(i,k) = ((1.0_r8-tem1)*qcko(i,k-1)+tem*0.5_r8*              &
                           (qo(i,k)+qo(i,k-1)))/factor
!j
              dq = eta(i,k) * (qcko(i,k) - qrch)
!
!  check if there is excess moisture to release latent heat
!
              IF(dq.GT.0.0_r8) THEN
                etah = 0.5_r8 * (eta(i,k) + eta(i,k-1))
                IF(ncloud.GT.0.0_r8) THEN
                  dp = 1000.0_r8 * del(i,k)
                  qlk = dq / (eta(i,k) + etah * (c0 + c1) * dz)
                  dellal(i,k) = etah * c1 * dz * qlk * con_g / dp
                ELSE
                  qlk = dq / (eta(i,k) + etah * c0 * dz)
                ENDIF
                qcko(i,k) = qlk + qrch
                pwo(i,k) = etah * c0 * dz * qlk
                pwavo(i) = pwavo(i) + pwo(i,k)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
! exchange ktcon with ktcon1
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          kk = ktcon(i)
          ktcon(i) = ktcon1(i)
          ktcon1(i) = kk
        ENDIF
      ENDDO
!
!  this section is ready for cloud water
!
      IF(ncloud.GT.0) THEN
!
!  compute liquid and vapor separation at cloud top
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          k = ktcon(i) - 1
          gamma = el2orc * qeso(i,k) / (to(i,k)**2)
          qrch = qeso(i,k)                                        &
               + gamma * dbyo(i,k) / (con_hvap * (1.0_r8 + gamma))
          dq = qcko(i,k) - qrch
!
!  check if there is excess moisture to release latent heat
!
          IF(dq.GT.0.0_r8) THEN
            qlko_ktcon(i) = dq
            qcko(i,k) = qrch
          ENDIF
        ENDIF
      ENDDO
      ENDIF
!
!cccc if(lat.eq.latd.and.lon.eq.lond.and.cnvflg(i)) then
!cccc   print *, ' aa1(i) before dwndrft =', aa1(i)
!cccc endif
!
!------- downdraft calculations
!
!--- compute precipitation efficiency in terms of windshear
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          vshear(i) = 0.0_r8
        ENDIF
      ENDDO
      DO k = 2, km
        DO i = 1, im
          IF (cnvflg(i)) THEN
            IF(k.GT.kb(i).AND.k.LE.ktcon(i)) THEN
              shear= SQRT((uo(i,k)-uo(i,k-1)) ** 2       &
                        + (vo(i,k)-vo(i,k-1)) ** 2)
              vshear(i) = vshear(i) + shear
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      DO i = 1, im
        IF(cnvflg(i)) THEN
          vshear(i) = 1.e3_r8 * vshear(i) / (zi(i,ktcon(i))-zi(i,kb(i)))
          e1=1.591_r8-0.639_r8*vshear(i)                             &
             +0.0953_r8*(vshear(i)**2)-0.00496_r8*(vshear(i)**3)
          edt(i)=1.-e1
          val =         .9_r8
          edt(i) = MIN(edt(i),val)
          val =         .0_r8
          edt(i) = MAX(edt(i),val)
          edto(i)=edt(i)
          edtx(i)=edt(i)
        ENDIF
      ENDDO
!
!  determine detrainment rate between 1 and kbcon
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          sumx(i) = 0._r8
        ENDIF
      ENDDO
      DO k = 1, km1
      DO i = 1, im
        IF(cnvflg(i).AND.k.GE.1.AND.k.LT.kbcon(i)) THEN
          dz = zi(i,k+1) - zi(i,k)
          sumx(i) = sumx(i) + dz
        ENDIF
      ENDDO
      ENDDO
      DO i = 1, im
        beta = betas
        IF(slimsk(i).EQ.1.0_r8) beta = betal
        IF(cnvflg(i)) THEN
          dz  = (sumx(i)+zi(i,1))/float(kbcon(i))
          tem = 1.0_r8/float(kbcon(i))
          xlamd(i) = (1.0_r8-beta**tem)/dz
        ENDIF
      ENDDO
!
!  determine downdraft mass flux
!
      DO k = km1, 1, -1
        DO i = 1, im
          IF (cnvflg(i) .AND. k .LE. kmax(i)-1) THEN
           IF(k.LT.jmin(i).AND.k.GE.kbcon(i)) THEN
              dz        = zi(i,k+1) - zi(i,k)
              ptem      = xlamdd - xlamde
              etad(i,k) = etad(i,k+1) * (1.0_r8 - ptem * dz)
           ELSE IF(k.LT.kbcon(i)) THEN
              dz        = zi(i,k+1) - zi(i,k)
              ptem      = xlamd(i) + xlamdd - xlamde
              etad(i,k) = etad(i,k+1) * (1.0_r8 - ptem * dz)
           ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!--- downdraft moisture properties
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          jmn = jmin(i)
          hcdo(i,jmn) = heo(i,jmn)
          qcdo(i,jmn) = qo(i,jmn)
          qrcdo(i,jmn)= qeso(i,jmn)
          ucdo(i,jmn) = uo(i,jmn)
          vcdo(i,jmn) = vo(i,jmn)
          pwevo(i) = 0.0_r8
        ENDIF
      ENDDO
!j
      DO k = km1, 1, -1
        DO i = 1, im
          IF (cnvflg(i) .AND. k.LT.jmin(i)) THEN
              dz = zi(i,k+1) - zi(i,k)
              IF(k.GE.kbcon(i)) THEN
                 tem  = xlamde * dz
                 tem1 = 0.5_r8 * xlamdd * dz
              ELSE
                 tem  = xlamde * dz
                 tem1 = 0.5_r8 * (xlamd(i)+xlamdd) * dz
              ENDIF
              factor = 1.0_r8 + tem - tem1
              ptem = 0.5_r8 * tem - pgcon
              ptem1= 0.5_r8 * tem + pgcon
              hcdo(i,k) = ((1.0_r8-tem1)*hcdo(i,k+1)+tem*0.5_r8*                &
                          (heo(i,k)+heo(i,k+1)))/factor
              ucdo(i,k) = ((1.0_r8-tem1)*ucdo(i,k+1)+ptem*uo(i,k+1)           &
                          +ptem1*uo(i,k))/factor
              vcdo(i,k) = ((1.0_r8-tem1)*vcdo(i,k+1)+ptem*vo(i,k+1)           &
                          +ptem1*vo(i,k))/factor
              dbyo(i,k) = hcdo(i,k) - heso(i,k)
          ENDIF
        ENDDO
      ENDDO
!
      DO k = km1, 1, -1
        DO i = 1, im
          IF (cnvflg(i).AND.k.LT.jmin(i)) THEN
              gamma      = el2orc * qeso(i,k) / (to(i,k)**2)
              qrcdo(i,k) = qeso(i,k)+                                 &
                      (1.0_r8/con_hvap)*(gamma/(1.0_r8+gamma))*dbyo(i,k)
!             detad      = etad(i,k+1) - etad(i,k)
!j
              dz = zi(i,k+1) - zi(i,k)
              IF(k.GE.kbcon(i)) THEN
                 tem  = xlamde * dz
                 tem1 = 0.5_r8 * xlamdd * dz
              ELSE
                 tem  = xlamde * dz
                 tem1 = 0.5_r8 * (xlamd(i)+xlamdd) * dz
              ENDIF
              factor = 1.0_r8 + tem - tem1
              qcdo(i,k) = ((1.0_r8-tem1)*qcdo(i,k+1)+tem*0.5_r8*   &
                           (qo(i,k)+qo(i,k+1)))/factor
!j
!             pwdo(i,k)  = etad(i,k+1) * qcdo(i,k+1) -
!    &                     etad(i,k) * qrcdo(i,k)
!             pwdo(i,k)  = pwdo(i,k) - detad *
!    &                    .5 * (qrcdo(i,k) + qrcdo(i,k+1))
!j
              pwdo(i,k)  = etad(i,k+1) * (qcdo(i,k) - qrcdo(i,k))
              qcdo(i,k)  = qrcdo(i,k)
              pwevo(i)   = pwevo(i) + pwdo(i,k)
          ENDIF
        ENDDO
      ENDDO
!
!--- final downdraft strength dependent on precip
!--- efficiency (edt), normalized condensate (pwav), and
!--- evaporate (pwev)
!
      DO i = 1, im
        edtmax = edtmaxl
        IF(slimsk(i).EQ.0.0_r8) edtmax = edtmaxs
        IF(cnvflg(i)) THEN
          IF(pwevo(i).LT.0.0_r8) THEN
            edto(i) = -edto(i) * pwavo(i) / pwevo(i)
            edto(i) = MIN(edto(i),edtmax)
          ELSE
            edto(i) = 0.0_r8
          ENDIF
        ENDIF
      ENDDO
!
!--- downdraft cloudwork functions
!
      DO k = km1, 1, -1
        DO i = 1, im
          IF (cnvflg(i) .AND. k .LT. jmin(i)) THEN
              gamma = el2orc * qeso(i,k) / to(i,k)**2
              dhh=hcdo(i,k)
              dt=to(i,k)
              dg=gamma
              dh=heso(i,k)
              dz=-1.0_r8*(zo(i,k+1)-zo(i,k))
              aa1(i)=aa1(i)+edto(i)*dz*(con_g/(con_cp*dt))*((dhh-dh)/(1.0_r8+dg))  &
                     *(1.0_r8+delta*con_cp*dg*dt/con_hvap)
              val=0.0_r8
              aa1(i)=aa1(i)+edto(i)*                            &
              dz*con_g*delta*MAX(val,(qeso(i,k)-qo(i,k)))
          ENDIF
        ENDDO
      ENDDO
      DO i = 1, im
        IF(cnvflg(i).AND.aa1(i).LE.0.0_r8) THEN
           cnvflg(i) = .FALSE.
        ENDIF
      ENDDO
!!
      totflg = .TRUE.
      DO i=1,im
        totflg = totflg .AND. (.NOT. cnvflg(i))
      ENDDO
      IF(totflg) RETURN
!!
!
!--- what would the change be, that a cloud with unit mass
!--- will do to the environment?
!
      DO k = 1, km
        DO i = 1, im
          IF(cnvflg(i) .AND. k .LE. kmax(i)) THEN
            dellah(i,k) = 0.0_r8
            dellaq(i,k) = 0.0_r8
            dellau(i,k) = 0.0_r8
            dellav(i,k) = 0.0_r8
          ENDIF
        ENDDO
      ENDDO
      DO i = 1, im
        IF(cnvflg(i)) THEN
          dp = 1000.0_r8 * del(i,1)
          dellah(i,1) = edto(i) * etad(i,1) * (hcdo(i,1)   &
                         - heo(i,1)) * con_g / dp
          dellaq(i,1) = edto(i) * etad(i,1) * (qcdo(i,1)   &
                         - qo(i,1)) * con_g / dp
          dellau(i,1) = edto(i) * etad(i,1) * (ucdo(i,1)   &
                         - uo(i,1)) * con_g / dp
          dellav(i,1) = edto(i) * etad(i,1) * (vcdo(i,1)   &
                         - vo(i,1)) * con_g / dp
        ENDIF
      ENDDO
!
!--- changed due to subsidence and entrainment
!
      DO k = 2, km1
        DO i = 1, im
          IF (cnvflg(i).AND.k.LT.ktcon(i)) THEN
              aup = 1.
              IF(k.LE.kb(i)) aup = 0.0_r8
              adw = 1.0_r8
              IF(k.GT.jmin(i)) adw = 0.0_r8
              dp = 1000.0_r8 * del(i,k)
              dz = zi(i,k) - zi(i,k-1)
!
              dv1h = heo(i,k)
              dv2h = 0.5_r8 * (heo(i,k) + heo(i,k-1))
              dv3h = heo(i,k-1)
              dv1q = qo(i,k)
              dv2q = 0.5_r8 * (qo(i,k) + qo(i,k-1))
              dv3q = qo(i,k-1)
              dv1u = uo(i,k)
              dv2u = 0.5_r8 * (uo(i,k) + uo(i,k-1))
              dv3u = uo(i,k-1)
              dv1v = vo(i,k)
              dv2v = 0.5_r8 * (vo(i,k) + vo(i,k-1))
              dv3v = vo(i,k-1)
!
              tem  = 0.5_r8 * (xlamue(i,k)+xlamue(i,k-1))
              tem1 = xlamud(i)
!
              IF(k.LE.kbcon(i)) THEN
                ptem  = xlamde
                ptem1 = xlamd(i)+xlamdd
              ELSE
                ptem  = xlamde
                ptem1 = xlamdd
              ENDIF
!j
              dellah(i,k) = dellah(i,k) +                                 &
           ((aup*eta(i,k)-adw*edto(i)*etad(i,k))*dv1h                     &
          - (aup*eta(i,k-1)-adw*edto(i)*etad(i,k-1))*dv3h                 &
          - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2h*dz       &
          +  aup*tem1*eta(i,k-1)*0.5_r8*(hcko(i,k)+hcko(i,k-1))*dz            &
          +  adw*edto(i)*ptem1*etad(i,k)*0.5_r8*(hcdo(i,k)+hcdo(i,k-1))*dz    &
               ) *con_g/dp
!j
              dellaq(i,k) = dellaq(i,k) +                                 &
           ((aup*eta(i,k)-adw*edto(i)*etad(i,k))*dv1q                     &
          - (aup*eta(i,k-1)-adw*edto(i)*etad(i,k-1))*dv3q                 &
          - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2q*dz       &
          +  aup*tem1*eta(i,k-1)*0.5_r8*(qcko(i,k)+qcko(i,k-1))*dz            &
          +  adw*edto(i)*ptem1*etad(i,k)*0.5_r8*(qrcdo(i,k)+qrcdo(i,k-1))*dz  &
               ) *con_g/dp
!j
              dellau(i,k) = dellau(i,k) +                                 &
           ((aup*eta(i,k)-adw*edto(i)*etad(i,k))*dv1u                     &
          - (aup*eta(i,k-1)-adw*edto(i)*etad(i,k-1))*dv3u                 &
          - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2u*dz       &
          +  aup*tem1*eta(i,k-1)*0.5_r8*(ucko(i,k)+ucko(i,k-1))*dz            &
          +  adw*edto(i)*ptem1*etad(i,k)*0.5_r8*(ucdo(i,k)+ucdo(i,k-1))*dz    &
          -  pgcon*(aup*eta(i,k-1)-adw*edto(i)*etad(i,k))*(dv1u-dv3u)     &
               ) *con_g/dp
!j
              dellav(i,k) = dellav(i,k) +                                 &
           ((aup*eta(i,k)-adw*edto(i)*etad(i,k))*dv1v                     &
          - (aup*eta(i,k-1)-adw*edto(i)*etad(i,k-1))*dv3v                 &
          - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2v*dz       &
          +  aup*tem1*eta(i,k-1)*0.5_r8*(vcko(i,k)+vcko(i,k-1))*dz            &
          +  adw*edto(i)*ptem1*etad(i,k)*0.5_r8*(vcdo(i,k)+vcdo(i,k-1))*dz    & 
          -  pgcon*(aup*eta(i,k-1)-adw*edto(i)*etad(i,k))*(dv1v-dv3v)     &
               ) *con_g/dp
!j
          ENDIF
        ENDDO
      ENDDO
!
!------- cloud top
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          indx = ktcon(i)
          dp = 1000.0_r8 * del(i,indx)
          dv1h = heo(i,indx-1)
          dellah(i,indx) = eta(i,indx-1) *                          &
                           (hcko(i,indx-1) - dv1h) * con_g / dp
          dv1q = qo(i,indx-1)
          dellaq(i,indx) = eta(i,indx-1) *                          &
                           (qcko(i,indx-1) - dv1q) * con_g / dp
          dv1u = uo(i,indx-1)
          dellau(i,indx) = eta(i,indx-1) *                          &
                           (ucko(i,indx-1) - dv1u) * con_g / dp
          dv1v = vo(i,indx-1)
          dellav(i,indx) = eta(i,indx-1) *                          &
                           (vcko(i,indx-1) - dv1v) * con_g / dp
!
!  cloud water
!
          dellal(i,indx) = eta(i,indx-1) *                          &
                           qlko_ktcon(i) * con_g / dp
        ENDIF
      ENDDO
!
!------- final changed variable per unit mass flux
!
      DO k = 1, km
        DO i = 1, im
          IF (cnvflg(i).AND.k .LE. kmax(i)) THEN
            IF(k.GT.ktcon(i)) THEN
              qo(i,k) = q1(i,k)
              to(i,k) = t1(i,k)
            ENDIF
            IF(k.LE.ktcon(i)) THEN
              qo(i,k) = dellaq(i,k) * mbdt + q1(i,k)
              dellat = (dellah(i,k) - con_hvap * dellaq(i,k)) / con_cp
              to(i,k) = dellat * mbdt + t1(i,k)
              val   =           1.e-10_r8
              qo(i,k) = MAX(qo(i,k), val  )
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!--- the above changed environment is now used to calulate the
!--- effect the arbitrary cloud (with unit mass flux)
!--- would have on the stability,
!--- which then is used to calculate the real mass flux,
!--- necessary to keep this change in balance with the large-scale
!--- destabilization.
!
!--- environmental conditions again, first heights
!
      DO k = 1, km
        DO i = 1, im
          IF(cnvflg(i) .AND. k .LE. kmax(i)) THEN
            qeso(i,k) = 0.01_r8 * fpvs2es5(to(i,k))      ! fpvs is in pa
!PK            qeso(i,k) = 0.01_r8 * fpvs(to(i,k))      ! fpvs is in pa

            qeso(i,k) = con_eps * qeso(i,k) / (pfld(i,k)+con_epsm1*qeso(i,k))
            val       =             1.e-8_r8
            qeso(i,k) = MAX(qeso(i,k), val )
!           tvo(i,k)  = to(i,k) + delta * to(i,k) * qo(i,k)
          ENDIF
        ENDDO
      ENDDO
!
!--- moist static energy
!
      DO k = 1, km1
        DO i = 1, im
          IF(cnvflg(i) .AND. k .LE. kmax(i)-1) THEN
            dz = 0.5_r8 * (zo(i,k+1) - zo(i,k))
            dp = 0.5_r8 * (pfld(i,k+1) - pfld(i,k))
            es = 0.01_r8 * fpvs2es5(to(i,k+1))      ! fpvs is in pa
!PK            es = 0.01_r8 * fpvs(to(i,k+1))      ! fpvs is in pa

            pprime = pfld(i,k+1) + con_epsm1 * es
            qs = con_eps * es / pprime
            dqsdp = - qs / pprime
            desdt = es * (fact1 / to(i,k+1) + fact2 / (to(i,k+1)**2))
            dqsdt = qs * pfld(i,k+1) * desdt / (es * pprime)
            gamma = el2orc * qeso(i,k+1) / (to(i,k+1)**2)
            dt = (con_g * dz + con_hvap * dqsdp * dp) / (con_cp * (1.0_r8 + gamma))
            dq = dqsdt * dt + dqsdp * dp
            to(i,k) = to(i,k+1) + dt
            qo(i,k) = qo(i,k+1) + dq
            po(i,k) = 0.5_r8 * (pfld(i,k) + pfld(i,k+1))
          ENDIF
        ENDDO
      ENDDO
      DO k = 1, km1
        DO i = 1, im
          IF(cnvflg(i) .AND. k .LE. kmax(i)-1) THEN
            qeso(i,k) = 0.01_r8 * fpvs2es5(to(i,k))      ! fpvs is in pa
!PK            qeso(i,k) = 0.01_r8 * fpvs(to(i,k))      ! fpvs is in pa

            qeso(i,k) = con_eps * qeso(i,k) / (po(i,k) + con_epsm1 * qeso(i,k))
            val1      =             1.e-8_r8
            qeso(i,k) = MAX(qeso(i,k), val1)
            val2      =           1.e-10_r8
            qo(i,k)   = MAX(qo(i,k), val2 )
!           qo(i,k)   = min(qo(i,k),qeso(i,k))
            heo(i,k)   = 0.5_r8 * con_g * (zo(i,k) + zo(i,k+1)) +  &
                          con_cp * to(i,k) + con_hvap * qo(i,k)
            heso(i,k) = 0.5_r8 * con_g * (zo(i,k) + zo(i,k+1)) +      &
                        con_cp * to(i,k) + con_hvap * qeso(i,k)
          ENDIF
        ENDDO
      ENDDO
      DO i = 1, im
        IF(cnvflg(i)) THEN
          k = kmax(i)
          heo(i,k) = con_g * zo(i,k) + con_cp * to(i,k) + con_hvap * qo(i,k)
          heso(i,k) = con_g * zo(i,k) + con_cp * to(i,k) + con_hvap * qeso(i,k)
!         heo(i,k) = min(heo(i,k),heso(i,k))
        ENDIF
      ENDDO
!
!**************************** static control
!
!------- moisture and cloud work functions
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          xaa0(i) = 0.0_r8
          xpwav(i) = 0.0_r8
        ENDIF
      ENDDO
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          indx = kb(i)
          hcko(i,indx) = heo(i,indx)
          qcko(i,indx) = qo(i,indx)
        ENDIF
      ENDDO
      DO k = 2, km1
        DO i = 1, im
          IF (cnvflg(i)) THEN
            IF(k.GT.kb(i).AND.k.LE.ktcon(i)) THEN
              dz = zi(i,k) - zi(i,k-1)
              tem  = 0.5_r8 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.5_r8 * xlamud(i) * dz
              factor = 1.0_r8 + tem - tem1
              hcko(i,k) = ((1.0_r8-tem1)*hcko(i,k-1)+tem*0.5_r8*   &
                           (heo(i,k)+heo(i,k-1)))/factor
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      DO k = 2, km1
        DO i = 1, im
          IF (cnvflg(i)) THEN
            IF(k.GT.kb(i).AND.k.LT.ktcon(i)) THEN
              dz = zi(i,k) - zi(i,k-1)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              xdby = hcko(i,k) - heso(i,k)
              xqrch = qeso(i,k)                     &
                    + gamma * xdby / (con_hvap * (1.0_r8 + gamma))
!j
              tem  = 0.5_r8 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.5_r8 * xlamud(i) * dz
              factor = 1.0_r8 + tem - tem1
              qcko(i,k) = ((1.0_r8-tem1)*qcko(i,k-1)+tem*0.5_r8*       &
                           (qo(i,k)+qo(i,k-1)))/factor
!j
              dq = eta(i,k) * (qcko(i,k) - xqrch)
!
              IF(k.GE.kbcon(i).AND.dq.GT.0.0_r8) THEN
                etah = 0.5_r8 * (eta(i,k) + eta(i,k-1))
                IF(ncloud.GT.0.0_r8.AND.k.GT.jmin(i)) THEN
                  qlk = dq / (eta(i,k) + etah * (c0 + c1) * dz)
                ELSE
                  qlk = dq / (eta(i,k) + etah * c0 * dz)
                ENDIF
                IF(k.LT.ktcon1(i)) THEN
                  xaa0(i) = xaa0(i) - dz * con_g * qlk
                ENDIF
                qcko(i,k) = qlk + xqrch
                xpw = etah * c0 * dz * qlk
                xpwav(i) = xpwav(i) + xpw
              ENDIF
            ENDIF
            IF(k.GE.kbcon(i).AND.k.LT.ktcon1(i)) THEN
              dz1 = zo(i,k+1) - zo(i,k)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              rfact =  1.0_r8 + delta * con_cp * gamma             &
                       * to(i,k) / con_hvap
              xaa0(i) = xaa0(i)                                & 
                      + dz1 * (con_g / (con_cp * to(i,k)))     &
                      * xdby / (1.0_r8 + gamma)                    &
                      * rfact
              val=0.0_r8
              xaa0(i)=xaa0(i)+                                &
                       dz1 * con_g * delta *                  &
                       MAX(val,(qeso(i,k) - qo(i,k)))
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!------- downdraft calculations
!
!--- downdraft moisture properties
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          jmn = jmin(i)
          hcdo(i,jmn) = heo(i,jmn)
          qcdo(i,jmn) = qo(i,jmn)
          qrcd(i,jmn) = qeso(i,jmn)
          xpwev(i) = 0.0_r8
        ENDIF
      ENDDO
!j
      DO k = km1, 1, -1
        DO i = 1, im
          IF (cnvflg(i) .AND. k.LT.jmin(i)) THEN
              dz = zi(i,k+1) - zi(i,k)
              IF(k.GE.kbcon(i)) THEN
                 tem  = xlamde * dz
                 tem1 = 0.5_r8 * xlamdd * dz
              ELSE
                 tem  = xlamde * dz
                 tem1 = 0.5_r8 * (xlamd(i)+xlamdd) * dz
              ENDIF
              factor = 1.0_r8 + tem - tem1
              hcdo(i,k) = ((1.0_r8-tem1)*hcdo(i,k+1)+tem*0.5_r8*   &
                           (heo(i,k)+heo(i,k+1)))/factor
          ENDIF
        ENDDO
      ENDDO
!j
      DO k = km1, 1, -1
        DO i = 1, im
          IF (cnvflg(i) .AND. k .LT. jmin(i)) THEN
              dq = qeso(i,k)
              dt = to(i,k)
              gamma    = el2orc * dq / dt**2
              dh       = hcdo(i,k) - heso(i,k)
              qrcd(i,k)=dq+(1.0_r8/con_hvap)*(gamma/(1.0_r8+gamma))*dh
!             detad    = etad(i,k+1) - etad(i,k)
!j
              dz = zi(i,k+1) - zi(i,k)
              IF(k.GE.kbcon(i)) THEN
                 tem  = xlamde * dz
                 tem1 = 0.5_r8 * xlamdd * dz
              ELSE
                 tem  = xlamde * dz
                 tem1 = 0.5_r8 * (xlamd(i)+xlamdd) * dz
              ENDIF
              factor = 1.0_r8 + tem - tem1
              qcdo(i,k) = ((1.0_r8-tem1)*qcdo(i,k+1)+tem*0.5_r8*   &
                           (qo(i,k)+qo(i,k+1)))/factor
!j
!             xpwd     = etad(i,k+1) * qcdo(i,k+1) -
!    &                   etad(i,k) * qrcd(i,k)
!             xpwd     = xpwd - detad *
!    &                 .5 * (qrcd(i,k) + qrcd(i,k+1))
!j
              xpwd     = etad(i,k+1) * (qcdo(i,k) - qrcd(i,k))
              qcdo(i,k)= qrcd(i,k)
              xpwev(i) = xpwev(i) + xpwd
          ENDIF
        ENDDO
      ENDDO
!
      DO i = 1, im
        edtmax = edtmaxl
        IF(slimsk(i).EQ.0.0_r8) edtmax = edtmaxs
        IF(cnvflg(i)) THEN
          IF(xpwev(i).GE.0.0_r8) THEN
            edtx(i) = 0.0_r8
          ELSE
            edtx(i) = -edtx(i) * xpwav(i) / xpwev(i)
            edtx(i) = MIN(edtx(i),edtmax)
          ENDIF
        ENDIF
      ENDDO
!
!
!--- downdraft cloudwork functions
!
!
      DO k = km1, 1, -1
        DO i = 1, im
          IF (cnvflg(i) .AND. k.LT.jmin(i)) THEN
              gamma = el2orc * qeso(i,k) / to(i,k)**2
              dhh=hcdo(i,k)
              dt= to(i,k)
              dg= gamma
              dh= heso(i,k)
              dz=-1.0_r8*(zo(i,k+1)-zo(i,k))
              xaa0(i)=xaa0(i)+edtx(i)*dz*(con_g/(con_cp*dt))*((dhh-dh)/(1.0_r8+dg)) &
                      *(1.0_r8+delta*con_cp*dg*dt/con_hvap)
              val=0.0_r8
              xaa0(i)=xaa0(i)+edtx(i)*              &
              dz*con_g*delta*MAX(val,(qeso(i,k)-qo(i,k)))
          ENDIF
        ENDDO
      ENDDO
!
!  calculate critical cloud work function
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
          IF(pfld(i,ktcon(i)).LT.pcrit(15))THEN
            acrt(i)=acrit(15)*(975.0_r8-pfld(i,ktcon(i)))         &
                    /(975.0_r8-pcrit(15))
          ELSE IF(pfld(i,ktcon(i)).GT.pcrit(1))THEN
            acrt(i)=acrit(1)
          ELSE
            k =  INT((850.0_r8 - pfld(i,ktcon(i)))/50.0_r8) + 2
            k = MIN(k,15)
            k = MAX(k,2)
            acrt(i)=acrit(k)+(acrit(k-1)-acrit(k))*             &
                 (pfld(i,ktcon(i))-pcrit(k))/(pcrit(k-1)-pcrit(k))
          ENDIF
        ENDIF
      ENDDO
      DO i = 1, im
        IF(cnvflg(i)) THEN
          IF(slimsk(i).EQ.1.0_r8) THEN
            w1 = w1l
            w2 = w2l
            w3 = w3l
            w4 = w4l
          ELSE
            w1 = w1s
            w2 = w2s
            w3 = w3s
            w4 = w4s
          ENDIF
!
!  modify critical cloud workfunction by cloud base vertical velocity
!
          IF(pdot(i).LE.w4) THEN
            acrtfct(i) = (pdot(i) - w4) / (w3 - w4)
          ELSEIF(pdot(i).GE.-w4) THEN
            acrtfct(i) = - (pdot(i) + w4) / (w4 - w3)
          ELSE
            acrtfct(i) = 0.05_r8
          ENDIF
          val1    =             -1.0_r8
          acrtfct(i) = MAX(acrtfct(i),val1)
          val2    =             1.0_r8
          acrtfct(i) = MIN(acrtfct(i),val2)
          acrtfct(i) = 1.0_r8 - acrtfct(i)
!
!  modify acrtfct(i) by colume mean rh if rhbar(i) is greater than 80 percent
!
!         if(rhbar(i).ge..8) then
!           acrtfct(i) = acrtfct(i) * (.9 - min(rhbar(i),.9)) * 10.
!         endif
!
!  modify adjustment time scale by cloud base vertical velocity
!
          dtconv(i) = dt2 + MAX((1800.0_r8 - dt2),0.0_r8) *        &
                      (pdot(i) - w2) / (w1 - w2)
!         dtconv(i) = max(dtconv(i), dt2)
!         dtconv(i) = 1800._r8 * (pdot(i) - w2) / (w1 - w2)
          dtconv(i) = MAX(dtconv(i),dtmin)
          dtconv(i) = MIN(dtconv(i),dtmax)
!
        ENDIF
      ENDDO
!
!--- large scale forcing
!
      DO i= 1, im
        IF(cnvflg(i)) THEN
          fld(i)=(aa1(i)-acrt(i)* acrtfct(i))/dtconv(i)
          IF(fld(i).LE.0.0_r8) cnvflg(i) = .FALSE.
        ENDIF
        IF(cnvflg(i)) THEN
!         xaa0(i) = max(xaa0(i),0.0_r8)
          xk(i) = (xaa0(i) - aa1(i)) / mbdt
          IF(xk(i).GE.0.0_r8) cnvflg(i) = .FALSE.
        ENDIF
!
!--- kernel, cloud base mass flux
!
        IF(cnvflg(i)) THEN
          xmb(i) = -fld(i) / xk(i)
          xmb(i) = MIN(xmb(i),xmbmax(i))
        ENDIF
      ENDDO
!!
      totflg = .TRUE.
      DO i=1,im
        totflg = totflg .AND. (.NOT. cnvflg(i))
      ENDDO
      IF(totflg) RETURN
!!
!
!  restore to,qo,uo,vo to t1,q1,u1,v1 in case convection stops
!
      DO k = 1, km
        DO i = 1, im
          IF (cnvflg(i) .AND. k .LE. kmax(i)) THEN
            to(i,k) = t1(i,k)
            qo(i,k) = q1(i,k)
            uo(i,k) = u1(i,k)
            vo(i,k) = v1(i,k)
            qeso(i,k) = 0.01_r8 * fpvs2es5(t1(i,k))      ! fpvs is in pa
!PK            qeso(i,k) = 0.01_r8 * fpvs(t1(i,k))      ! fpvs is in pa

            qeso(i,k) = con_eps * qeso(i,k) / (pfld(i,k) + con_epsm1*qeso(i,k))
            val     =             1.e-8_r8
            qeso(i,k) = MAX(qeso(i,k), val )
          ENDIF
        ENDDO
      ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!--- feedback: simply the changes from the cloud with unit mass flux
!---           multiplied by  the mass flux necessary to keep the
!---           equilibrium with the larger-scale.
!
      DO i = 1, im
        delhbar(i) = 0.0_r8
        delqbar(i) = 0.0_r8
        deltbar(i) = 0.0_r8
        delubar(i) = 0.0_r8
        delvbar(i) = 0.0_r8
        qcond(i) = 0.0_r8
      ENDDO
      DO k = 1, km
        DO i = 1, im
          IF (cnvflg(i) .AND. k .LE. kmax(i)) THEN
            IF(k.LE.ktcon(i)) THEN
              dellat = (dellah(i,k) - con_hvap * dellaq(i,k)) / con_cp
              t1(i,k) = t1(i,k) + dellat * xmb(i) * dt2
              q1(i,k) = q1(i,k) + dellaq(i,k) * xmb(i) * dt2
!             tem = 1./rcs(i)
!             u1(i,k) = u1(i,k) + dellau(i,k) * xmb(i) * dt2 * tem
!             v1(i,k) = v1(i,k) + dellav(i,k) * xmb(i) * dt2 * tem
              u1(i,k) = u1(i,k) + dellau(i,k) * xmb(i) * dt2
              v1(i,k) = v1(i,k) + dellav(i,k) * xmb(i) * dt2
              dp = 1000.0_r8 * del(i,k)
              delhbar(i) = delhbar(i) + dellah(i,k)*xmb(i)*dp/con_g
              delqbar(i) = delqbar(i) + dellaq(i,k)*xmb(i)*dp/con_g
              deltbar(i) = deltbar(i) + dellat*xmb(i)*dp/con_g
              delubar(i) = delubar(i) + dellau(i,k)*xmb(i)*dp/con_g
              delvbar(i) = delvbar(i) + dellav(i,k)*xmb(i)*dp/con_g
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      DO k = 1, km
        DO i = 1, im
          IF (cnvflg(i) .AND. k .LE. kmax(i)) THEN
            IF(k.LE.ktcon(i)) THEN
              qeso(i,k) = 0.01_r8 * fpvs2es5(t1(i,k))      ! fpvs is in pa
!PK              qeso(i,k) = 0.01_r8 * fpvs(t1(i,k))      ! fpvs is in pa

              qeso(i,k) = con_eps * qeso(i,k)/(pfld(i,k) + con_epsm1*qeso(i,k))
              val     =             1.e-8_r8
              qeso(i,k) = MAX(qeso(i,k), val )
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
      DO i = 1, im
        rntot(i) = 0.0_r8
        delqev(i) = 0.0_r8
        delq2(i) = 0.0_r8
        flg(i) = cnvflg(i)
      ENDDO
      DO k = km, 1, -1
        DO i = 1, im
          IF (cnvflg(i) .AND. k .LE. kmax(i)) THEN
            IF(k.LT.ktcon(i)) THEN
              aup = 1.0_r8
              IF(k.LE.kb(i)) aup = 0.0_r8
              adw = 1.0_r8
              IF(k.GE.jmin(i)) adw = 0.0_r8
              rain =  aup * pwo(i,k) + adw * edto(i) * pwdo(i,k)
              rntot(i) = rntot(i) + rain * xmb(i) * 0.001_r8 * dt2
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      DO k = km, 1, -1
        DO i = 1, im
          IF (k .LE. kmax(i)) THEN
            deltv(i) = 0.0_r8
            delq(i) = 0.0_r8
            qevap(i) = 0.0_r8
            IF(cnvflg(i).AND.k.LT.ktcon(i)) THEN
              aup = 1.
              IF(k.LE.kb(i)) aup = 0.0_r8
              adw = 1.0_r8
              IF(k.GE.jmin(i)) adw = 0.0_r8
              rain =  aup * pwo(i,k) + adw * edto(i) * pwdo(i,k)
              rn(i) = rn(i) + rain * xmb(i) * 0.001_r8 * dt2
            ENDIF
            IF(flg(i).AND.k.LT.ktcon(i)) THEN
              evef = edt(i) * evfact
              IF(slimsk(i).EQ.1.0_r8) evef=edt(i) * evfactl
!             if(slimsk(i).eq.1.) evef=.07
!             if(slimsk(i).ne.1.) evef = 0.
              qcond(i) = evef * (q1(i,k) - qeso(i,k))            &
                       / (1.0_r8 + el2orc * qeso(i,k) / t1(i,k)**2)
              dp = 1000.0_r8 * del(i,k)
              IF(rn(i).GT.0.0_r8.AND.qcond(i).LT.0.0_r8) THEN
                qevap(i) = -qcond(i) * (1.0_r8-EXP(-0.32_r8*SQRT(dt2*rn(i))))
                qevap(i) = MIN(qevap(i), rn(i)*1000.0_r8*con_g/dp)
                delq2(i) = delqev(i) + 0.001_r8 * qevap(i) * dp / con_g
              ENDIF
              IF(rn(i).GT.0.0_r8.AND.qcond(i).LT.0.0_r8.AND.delq2(i).GT.rntot(i)) THEN
                qevap(i) = 1000.0_r8* con_g * (rntot(i) - delqev(i)) / dp
                flg(i) = .FALSE.
              ENDIF
              IF(rn(i).GT.0.0_r8.AND.qevap(i).GT.0.0_r8) THEN
                q1(i,k) = q1(i,k) + qevap(i)
                t1(i,k) = t1(i,k) - elocp * qevap(i)
                rn(i) = rn(i) - 0.001_r8 * qevap(i) * dp / con_g
                deltv(i) = - elocp*qevap(i)/dt2
                delq(i) =  + qevap(i)/dt2
                delqev(i) = delqev(i) + 0.001_r8*dp*qevap(i)/con_g
              ENDIF
              dellaq(i,k) = dellaq(i,k) + delq(i) / xmb(i)
              delqbar(i) = delqbar(i) + delq(i)*dp/con_g
              deltbar(i) = deltbar(i) + deltv(i)*dp/con_g
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!j
!     do i = 1, im
!     if(me.eq.31.and.cnvflg(i)) then
!     if(cnvflg(i)) then
!       print *, ' deep delhbar, delqbar, deltbar = ',
!    &             delhbar(i),con_hvap*delqbar(i),cp*deltbar(i)
!       print *, ' deep delubar, delvbar = ',delubar(i),delvbar(i)
!       print *, ' precip =', con_hvap*rn(i)*1000./dt2
!       print*,'pdif= ',pfld(i,kbcon(i))-pfld(i,ktcon(i))
!     endif
!     enddo
!
!  precipitation rate converted to actual precip
!  in unit of m instead of kg
!
      DO i = 1, im
        IF(cnvflg(i)) THEN
!
!  in the event of upper level rain evaporation and lower level downdraft
!    moistening, rn can become negative, in this case, we back out of the
!    heating and the moistening
!
          IF(rn(i).LT.0.0_r8.AND..NOT.flg(i)) rn(i) = 0.0_r8
          IF(rn(i).LE.0.0_r8) THEN
            rn(i) = 0.0_r8
          ELSE
            ktop(i) = ktcon(i)
            kbot(i) = kbcon(i)
            kcnv(i) = 1
            cldwrk(i) = aa1(i)
          ENDIF
        ENDIF
      ENDDO
!
!  cloud water
!
      IF (ncloud.GT.0) THEN
!
      DO k = 1, km
        DO i = 1, im
          IF (cnvflg(i) .AND. rn(i).GT.0.0_r8) THEN
            IF (k.GT.kb(i).AND.k.LE.ktcon(i)) THEN
              tem  = dellal(i,k) * xmb(i) * dt2
              tem1 = MAX(0.0_r8, MIN(1.0_r8, (tcr-t1(i,k))*tcrf))
              IF (ql(i,k,2) .GT. -999.0_r8) THEN
                ql(i,k,1) = ql(i,k,1) + tem * tem1            ! ice
                ql(i,k,2) = ql(i,k,2) + tem *(1.0_r8-tem1)       ! water
              ELSE
                ql(i,k,1) = ql(i,k,1) + tem
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
      ENDIF
!
      DO k = 1, km
        DO i = 1, im
          IF(cnvflg(i).AND.rn(i).LE.0.0_r8) THEN
            IF (k .LE. kmax(i)) THEN
              t1(i,k) = to(i,k)
              q1(i,k) = qo(i,k)
              u1(i,k) = uo(i,k)
              v1(i,k) = vo(i,k)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
! hchuang code change
!
      DO k = 1, km
        DO i = 1, im
          IF(cnvflg(i).AND.rn(i).GT.0.0_r8) THEN
            IF(k.GE.kb(i) .AND. k.LT.ktop(i)) THEN
              ud_mf(i,k) = eta(i,k) * xmb(i) * dt2
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      DO i = 1, im
        IF(cnvflg(i).AND.rn(i).GT.0.0_r8) THEN
           k = ktop(i)-1
           dt_mf(i,k) = ud_mf(i,k)
        ENDIF
      ENDDO
      DO k = 1, km
        DO i = 1, im
          IF(cnvflg(i).AND.rn(i).GT.0.0_r8) THEN
            IF(k.GE.1 .AND. k.LE.jmin(i)) THEN
              dd_mf(i,k) = edto(i) * etad(i,k) * xmb(i) * dt2
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!!
      RETURN
      END SUBROUTINE sascnvn
END MODULE ModConRas
