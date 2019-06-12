!
!  $Author: pkubota $
!  $Date: 2011/04/07 16:00:31 $
!  $Revision: 1.15 $
!
MODULE SpecDynamics

  USE Utils, ONLY : &
       Iminv, &
       Epslon                ! intent(in)

  USE Parallelism, ONLY : &
       mygroup_four, & ! intent(in)
       maxnodes,     & ! intent(in)
       myid

  USE Communications, ONLY : &
       Exchange_si,  & ! intent(in)
       SpectoSi,     & ! intent(in)
       SitoSpec

  USE Constants,    ONLY : &
       eriv,         & ! intent(in)
       er,           & ! intent(in)
       ersqiv,       & ! intent(in) 
       cp,           & ! intent(in)
       gasr,         & ! intent(in)
       tbar,         & ! intent(in)
       raa,          & ! intent(in)
       tov,          & ! intent(in)
       rk,           & ! intent(in)
       i8,           & ! intent(in)
       r8              ! intent(in)

  USE Diagnostics, ONLY : &
       dodia,             & ! intent(in)
       upspec,            & ! intent(in)
       nDiag_hdidif,      & ! intent(in)
       nDiag_hhedif,      & ! intent(in)
       nDiag_hvodif,      & ! intent(in)
       nDiag_hmodif         ! intent(in)

  USE FieldsDynamics, ONLY : &
       qqp,          & ! intent(inout)
       qdivp,        & ! intent(inout)
       qrotp,        & ! intent(inout)
       qtmpp,        & ! intent(inout)
       qlnpp,        & ! intent(inout)
       qlnpl,        & ! intent(inout)
       qgzs,         & ! intent(in)
       qdivt,        & ! intent(in)
       qtmpt,        & ! intent(in)
       qlnpt,        & ! intent(in)
       qdivt_si,     & ! intent(out)
       qtmpt_si,     & ! intent(out)
       qlnpt_si        ! intent(out)

  USE Options,    ONLY : &
       vcrit,            & ! intent(in)
       alpha,            & ! intent(in)
       ndord,            & ! intent(in)
       iqdif,            & ! intent(in)
       nfprt,            & ! intent(in)
       nfctrl,           & ! intent(in)
       MasCon,           & ! intent(in)
       MasCon_ps,        & ! intent(in)
       rhdifd,           & ! intent(in)
       rhdift              ! intent(in)

  USE Sizes,  ONLY : &
       mMax,         & ! intent(in)
       nMax,         & ! intent(in)
       nExtMax,      & ! intent(in)
       mnMax,        & ! intent(in)
       mnMax_si,     & ! intent(in)
       mnExtMax,     & ! intent(in)
       mnExtMap,     & ! intent(in)
       mnMap,        & ! intent(in)
       nMap,         & ! intent(in)
       mMap,         & ! intent(in)
       lm2m,         & ! intent(in)
       kMax,         & ! intent(in)
       kMaxloc,      & ! intent(in)
       myfirstlev,   & ! intent(in)
       mylastlev,    & ! intent(in)
       mymMax,       & ! intent(in)
       mymnMax,      & ! intent(in)
       mymnExtMax,   & ! intent(in)
       mymnExtMap,   & ! intent(in)
       mymnMap,      & ! intent(in)
       mynMap,       & ! intent(in)
       mynMap_si,    & ! intent(in)
       mymextMap,    & ! intent(in)
       mynextMap,    & ! intent(in)
       mymnmap_si,   & ! intent(in)
       nsends_si,    & ! intent(in)
       nrecs_si,     & ! intent(in)
       maps_si,      & ! intent(in)
       mapr_si,      & ! intent(in)
       mysends_si,   & ! intent(in)
       myrecs_si,    & ! intent(in)
       inibr_si,     & ! intent(in)
       inibs_si,     & ! intent(in)
       mymMap,       & ! intent(in)
       haveM1,       & ! intent(in)
       havesurf,     & ! intent(in)
       nodehasm,     & ! intent(in)
       ngroups_four, & ! intent(in)
       nlevperg_four,& ! intent(in)
       rpi,          & ! intent(in)
       del,          & ! intent(in)
       ci              ! intent(in) 

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: InitDztouv
  PUBLIC :: dztouv
  PUBLIC :: InitFiltDiss
  PUBLIC :: FiltDiss
  PUBLIC :: InitGozrim
  PUBLIC :: Gozrim
  PUBLIC :: InitImplDifu
  PUBLIC :: ImplDifu
  PUBLIC :: InitSemiImpl
  PUBLIC :: bmcm
  PUBLIC :: SemiImpl
  PUBLIC :: SemiImpl_si
  PUBLIC :: InitUvtodz
  PUBLIC :: uvtodz

  PUBLIC :: dk
  PUBLIC :: tk
  PUBLIC :: hm
  PUBLIC :: tm
  PUBLIC :: sv
  PUBLIC :: am
  PUBLIC :: bm
  PUBLIC :: cm
  PUBLIC :: p1
  PUBLIC :: p2
  PUBLIC :: h1
  PUBLIC :: h2
  PUBLIC :: hmt
  PUBLIC :: snnp1
  PUBLIC :: snnp1_si

  INTERFACE Gozrim
     MODULE PROCEDURE Gozrim1D, Gozrim2D
  END INTERFACE


  !  Module exports two routines:
  !     InitDZtoUV:        Should be invoked once, before any other module 
  !                        routine; sets up local constants and mappings;
  !     DZtoUV:            Velocity fields from Divergence and Vorticity; 
  !                        use values computed by InitDZtoUV

  !  Module require values from modules Sizes, AssocLegFunc and Constants

  REAL(KIND=r8), ALLOCATABLE :: alfa_dz(:)        ! er*Epslon(m,n)/n  for m<n<=nExtMax; 0 otherwise
  REAL(KIND=r8), ALLOCATABLE :: alfa_dzNp1(:)     ! alfa_dz(m,n+1) 
  REAL(KIND=r8), ALLOCATABLE :: beta_dz(:)        ! m*er/(n*(n+1)) for m/=0 and m<=n<=nMax;
  !                                          er/(n+1)     for m=0;
  REAL(KIND=r8), ALLOCATABLE :: alfa_gz(:)        ! -eriv*(n-1)*Epslon(m,n)  for m<n<=nExtMax;
  !                                         0 otherwise
  REAL(KIND=r8), ALLOCATABLE :: beta_gz(:)        ! eriv*(n+2)*Epslon(m,n+1) for m<=n<nMax;
  !                                         0 otherwise



  ! Observe, in the relation to be computed, that u and v are defined for 
  ! 1<=mn<=mnExtMax, while Div and Vor for 1<=mn<=mnMax. Consequently, a 
  ! mapping from 1:mnExtMax to 1:mnMax
  ! has to be computed.

  ! This mapping will have faults, since there is no Div or Vor at (m,nExtMax).

  ! Furthermore, the relation requires mappings from (m,nExt) to (m,n), 
  ! (m,n+1) and (m,n-1)

  ! Mapping function mnp1_dz(1:2*mnExtMax) gives index of (m,nExt) on (m,n+1). 
  ! It is faulty on (*,nMax:nExtMax). Since it is only used by the last term,
  ! faulty values have to be multipied by 0 (on alfa_dzNp1)

  ! Mapping function mnm1_dz(1:2*mnExtMax) gives index of (m,nExt) on (m,n-1). 
  ! It is faulty on (m,m) for all m. Since it is only used by the second term,
  ! faulty values have to be multipied by 0 (on alfa_dz)

  ! Mapping function mnir_dz(1:2*mnExtMax) gives index of (m,nExt) on (m,n-1) and
  ! multiplies by i (trading imaginary by real and correcting sign). It is
  ! faulty on (m,nExtMax). To correct the fault, beta_dz(m,nExtMax) is set to 0.

  INTEGER, ALLOCATABLE :: mnir_dz(:)
  INTEGER, ALLOCATABLE :: mnm1_dz(:)
  INTEGER, ALLOCATABLE :: mnp1_dz(:)
  INTEGER, ALLOCATABLE :: mnm1_gz(:)
  INTEGER, ALLOCATABLE :: mnp1_gz(:)
  INTEGER(KIND=i8), ALLOCATABLE :: snnp1(:)
  INTEGER(KIND=i8), ALLOCATABLE :: snnp1_si(:)


  INTEGER :: ndho
  REAL(KIND=r8) :: dk
  REAL(KIND=r8) :: tk
  LOGICAL :: diffuseQ
  REAL(KIND=r8), ALLOCATABLE :: ct(:)
  REAL(KIND=r8), ALLOCATABLE :: cq(:)

  REAL(KIND=r8), ALLOCATABLE :: sv(:)
  REAL(KIND=r8), ALLOCATABLE :: p1(:)
  REAL(KIND=r8), ALLOCATABLE :: p2(:)
  REAL(KIND=r8), ALLOCATABLE :: h1(:)
  REAL(KIND=r8), ALLOCATABLE :: h2(:)
  REAL(KIND=r8), ALLOCATABLE :: am(:,:)
  REAL(KIND=r8), ALLOCATABLE :: bm(:,:)
  REAL(KIND=r8), ALLOCATABLE :: cm(:,:)
  REAL(KIND=r8), ALLOCATABLE :: dm(:,:,:)
  REAL(KIND=r8), ALLOCATABLE :: hm(:,:)
  REAL(KIND=r8), ALLOCATABLE :: hmt(:,:)
  REAL(KIND=r8), ALLOCATABLE :: tm(:,:)
  REAL(KIND=r8), ALLOCATABLE :: workImplDifu(:)

  ! DIVERGENCE AND VORTICITY FROM VELOCITY FIELDS
  !
  ! Implements the following relations:
  !
  !    m              m        m       m        m         m   m       m   m
  ! Div   =CMPLX(-Alfa * Imag(U ), Alfa * Real(U )) + Beta * V  - Gama * V
  !    n              n        n       n        n        n+1 n+1      n  n-1
  !
  !    m              m        m       m        m         m   m       m   m
  ! Vor   =CMPLX(-Alfa * Imag(V ), Alfa * Real(V )) + Beta * U  - Gama * U
  !    n              n        n       n        n        n+1 n+1      n  n-1
  !
  ! for 0<=m<=mMax, m<=n<=nMax, where
  !
  !  m   m
  ! U = V = 0 for n < m
  !  n   n

  !  Module exports two routines:
  !     InitUvtodz:        Should be invoked once, before any other module 
  !                        routine; sets up local constants and mappings;
  !     Uvtodz:            Divergence and Vorticity from Velocity fields; 
  !                        use values computed by InitUvtodz

  !  Module require values from modules Sizes, AssocLegFunc and Constants

  REAL(KIND=r8),    ALLOCATABLE :: alfa_uv(:)        ! eriv*m 
  !                                           for m<=n<=nMax
  REAL(KIND=r8),    ALLOCATABLE :: beta_uv(:)        ! eriv*n*Epslon(m,n+1) 
  !                                           for m<=n<=nMax;
  REAL(KIND=r8),    ALLOCATABLE :: gama_uv(:)        ! eriv*(n+1)*Epslon(m,n) 
  !                                           for m<=n<=nMax;

  ! Observe, in the relation to be computed, that u and v are defined for 
  ! 1<=mn<=mnExtMax, while Div and Vor for 1<=mn<=mnMax. Consequently, a 
  ! mapping from 1:mnMax to 1:mnExtMax has to be computed.

  ! In fact, the relation requires 3 mappings:
  !    1) (m,n) to (m,nExt)    implemented by index array mnir(mn);
  !    2) (m,n) to (m,nExt+1)  implemented by index array mnp1(mn);
  !    3) (m,n) to (m,nExt-1)  implemented by index array mnm1(mn);
  ! for m<=n<=nMax

  ! Mapping functions (1) and (2) are easily computed; mapping function (3)
  ! will be faulty for n=m; since it is used only at the last term,
  ! faulty values have to be multipied by 0 (on gama_uv(m,m))

  ! Mapping function mnir(1:2*mnMax) gives index of (m,n) on (m,nExt) and
  ! multiplies by i (trading immaginary by real). Other mappings keep in
  ! place the real and immaginary components.

  INTEGER, ALLOCATABLE :: mnir_uv(:)
  INTEGER, ALLOCATABLE :: mnm1_uv(:)
  INTEGER, ALLOCATABLE :: mnp1_uv(:)

  INTEGER :: kGlob
  REAL(KIND=r8)    :: alphaGlob
  REAL(KIND=r8)    :: betaGlob
  INTEGER, ALLOCATABLE :: ncrit(:)

CONTAINS


  ! InitDZtoUV: Mapping Functions and Local Constants


  SUBROUTINE InitDZtoUV()
    INTEGER :: m, mglob, n, mn, mn2, indir, indnp1, indnm1
    REAL(KIND=r8) :: aux

    ! mapping mnir_dz

    ALLOCATE(mnir_dz(2*mymnExtMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,indir)
    DO m = 1, mymMax
       mglob = lm2m(m)
       DO n = mglob, nMax
          mn = mymnExtMap(m,n)
          indir = mymnMap(m,n)
          mnir_dz(2*mn-1) = 2*indir
          mnir_dz(2*mn  ) = 2*indir-1
       END DO
       mn = mymnExtMap(m,nExtMax)
       mnir_dz(2*mn-1) = 1     ! faulty mapping # 1
       mnir_dz(2*mn  ) = 1     ! faulty mapping # 1
    END DO
    !$OMP END PARALLEL DO

    ! mapping mnm1_dz

    ALLOCATE(mnm1_dz  (2*mymnExtMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,indnm1)
    DO m = 1, mymMax
       mglob = lm2m(m)
       mn = mymnExtMap(m,mglob)
       indnm1 = mymnMap(m,mglob)       
       mnm1_dz(2*mn-1) = 1    ! faulty mapping # 2
       mnm1_dz(2*mn  ) = 1    ! faulty mapping # 2
       DO n = mglob+1, nExtMax
          mn = mymnExtMap(m,n)
          indnm1 = mymnMap(m,n-1)
          mnm1_dz(2*mn-1) = 2*indnm1-1
          mnm1_dz(2*mn  ) = 2*indnm1  
       END DO
    END DO
    !$OMP END PARALLEL DO

    ! mapping mnp1_dz

    ALLOCATE(mnp1_dz  (2*mymnExtMax))
    mnp1_dz = 0
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,indnp1)
    DO m = 1, mymMax
       mglob = lm2m(m)
       DO n = mglob, nMax-1
          mn = mymnExtMap(m,n)
          indnp1 = mymnMap(m,n+1)
          mnp1_dz(2*mn-1) = 2*indnp1-1
          mnp1_dz(2*mn  ) = 2*indnp1  
       END DO
       DO n = nMax, nExtMax
          mn = mymnExtMap(m,n)
          mnp1_dz(2*mn-1) = 1 ! faulty mapping # 3
          mnp1_dz(2*mn  ) = 1 ! faulty mapping # 3
       END DO
    END DO
    !$OMP END PARALLEL DO

    ! constant beta_dz

    ALLOCATE(beta_dz(2*mymnExtMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,aux)
    DO m = 1, mymMax
       mglob = lm2m(m)
       aux = er/REAL(mglob,r8)
       mn = mymnExtMap(m,mglob)
       beta_dz(2*mn-1) = aux
       beta_dz(2*mn  ) = -aux
       DO n = mglob+1, nMax
          aux = REAL(mglob-1,r8)*er/REAL((n-1)*n,r8)
          mn = mymnExtMap(m,n)
          beta_dz(2*mn-1) = aux
          beta_dz(2*mn  ) = -aux
       END DO
       mn = mymnExtMap(m,nExtMax)
       beta_dz(2*mn-1) = 0.0_r8           ! corrects faulty mapping # 1
       beta_dz(2*mn  ) = 0.0_r8           ! corrects faulty mapping # 1
    END DO
    !$OMP END PARALLEL DO

    ! constant alfa_dz

    ALLOCATE(alfa_dz    (2*mymnExtMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,aux)
    DO m = 1, mymMax
       mglob = lm2m(m)
       mn = mymnExtMap(m,mglob)
       alfa_dz(2*mn-1) = 0.0_r8           ! corrects faulty mapping # 2
       alfa_dz(2*mn  ) = 0.0_r8           ! corrects faulty mapping # 2
       DO n = mglob+1, nExtMax
          mn = mymnExtMap(m,n)
          aux = er * Epslon(mn) / REAL(n-1,r8)
          alfa_dz(2*mn-1) = aux
          alfa_dz(2*mn  ) = aux
       END DO
    END DO
    !$OMP END PARALLEL DO

    ! constant alfa_dz mapped to n-1

    ALLOCATE(alfa_dzNp1 (2*mymnExtMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,mn2,aux)
    DO m = 1, mymMax
       mglob = lm2m(m)
       DO n = mglob, nMax - 1
          mn  = mymnExtMap(m,n)
          mn2 = mymnExtMap(m,n+1)
          aux  = er * Epslon(mn2) / REAL(n,r8)
          alfa_dzNp1(2*mn-1) = aux
          alfa_dzNp1(2*mn  ) = aux
       END DO
       DO n = nMax, nExtMax
          mn = mymnExtMap(m,n)
          alfa_dzNp1(2*mn-1) = 0.0_r8     ! corrects faulty mapping # 3
          alfa_dzNp1(2*mn  ) = 0.0_r8     ! corrects faulty mapping # 3
       END DO
    END DO
    !$OMP END PARALLEL DO

  END SUBROUTINE InitDZtoUV


  ! DZtoUV: Velocity fields from divergence and vorticity
  !
  ! Implements the following relations:
  !
  !  m            m          m       m         m         m     m       m     m
  ! U  =CMPLX(Beta * Imag(Div), -Beta *Real(Div))  - alfa * Vor  + alfa * Vor
  !  n            n          n       n         n         n    n-1     n+1   n+1
  !
  !  m            m          m       m         m         m     m       m     m
  ! V  =CMPLX(Beta * Imag(Vor), -Beta *Real(Vor))  + alfa * Div  - alfa * Div
  !  n            n          n       n         n         n    n-1     n+1   n+1
  !
  ! for 0<=m<=mMax, m<=n<=nExtMax, where
  !
  !    m     m
  ! Div = Vor = 0 for n > nMax or n < m
  !    n     n



  SUBROUTINE DZtoUV(qdivp, qrotp, qup, qvp, mnRIExtFirst, mnRIExtLast)
    REAL(KIND=r8),    INTENT(IN ) :: qdivp(2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(IN ) :: qrotp(2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(OUT) :: qup(2*mymnExtMax,kMaxloc)
    REAL(KIND=r8),    INTENT(OUT) :: qvp(2*mymnExtMax,kMaxloc)
    INTEGER, INTENT(IN ) :: mnRIExtFirst
    INTEGER, INTENT(IN ) :: mnRIExtLast
    INTEGER :: mn, k   

    DO k = 1, kMaxloc
       DO mn = mnRIExtFirst, mnRIExtLast
          qup(mn,k) =                          - &
               alfa_dz   (mn) * qrotp(mnm1_dz(mn),k) + &
               alfa_dzNp1(mn) * qrotp(mnp1_dz(mn),k) + &
               beta_dz   (mn) * qdivp(mnir_dz(mn),k) 
          qvp(mn,k) =                          + &
               alfa_dz   (mn) * qdivp(mnm1_dz(mn),k) - &
               alfa_dzNp1(mn) * qdivp(mnp1_dz(mn),k) + &
               beta_dz   (mn) * qrotp(mnir_dz(mn),k) 
       END DO
    END DO
  END SUBROUTINE DZtoUV


  ! InitFiltDiss


  SUBROUTINE InitFiltDiss()

    ALLOCATE(ncrit(kMax))
    alphaGlob = (alpha / 6.37_r8) * 1.e-06_r8
    betaGlob  = 1034.6_r8 * 62.0_r8 * vcrit
  END SUBROUTINE InitFiltDiss

  !
  !     ftrdis : this routine applies an extra dissipation to spectral
  !              fields, depending on maximum velocity at a given level
  !              and on wave-number .

  SUBROUTINE FiltDiss(dt, vmax, kFirst, kLast, mnRIFirst, mnRILast)
    REAL(KIND=r8),    INTENT(IN) :: dt
    REAL(KIND=r8),    INTENT(IN) :: vmax(kMaxloc)
    INTEGER, INTENT(IN) :: kFirst
    INTEGER, INTENT(IN) :: kLast
    INTEGER, INTENT(IN) :: mnRIFirst
    INTEGER, INTENT(IN) :: mnRILast
    !
    REAL(KIND=r8)    :: alpha0
    REAL(KIND=r8)    :: beta
    REAL(KIND=r8)    :: DumpFactor
    INTEGER :: k
    INTEGER :: n
    INTEGER :: m
    INTEGER :: mglob
    INTEGER :: mn

    alpha0 = alphaGlob * dt
    beta   = betaGlob / dt

    DO k = kFirst, kLast
       IF (vmax(k) <= beta/nMax ) THEN
          ncrit(k) = nMax
       ELSE
          ncrit(k) = INT(beta / vmax(k))
       ENDIF
    END DO
    !$OMP BARRIER


    DO k = 1,kMaxloc
       !CDIR NODEP
       DO mn = mnRIFirst, mnRILast
          n = mynMap((mn+1)/2)
          m = mymMap((mn+1)/2)
          mglob = lm2m(m)
          IF (n >= ncrit(k) + 2 .AND. mglob <= n) THEN
             DumpFactor = 1.0_r8 / (1.0_r8+alpha0*vmax(k)*(n-1-ncrit(k)))
             qdivp(mn,k) = DumpFactor * qdivp(mn,k)
             qtmpp(mn,k) = DumpFactor * qtmpp(mn,k)
             qrotp(mn,k) = DumpFactor * qrotp(mn,k)
             qqp  (mn,k) = DumpFactor * qqp(mn,k)
          END IF
       END DO
    END DO
    !$OMP BARRIER
  END SUBROUTINE FiltDiss



  ! InitGozrim: Mapping Functions and Local Constants


  SUBROUTINE InitGozrim()
    INTEGER :: m, mglob, n, mn, mn2, indnp1, indnm1

    ! mapping mnm1_gz

    ALLOCATE(mnm1_gz  (2*mymnExtMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,indnm1)
    DO m = 1, mymMax
       mglob = lm2m(m)
       mn = mymnExtMap(m,mglob)
       indnm1 = mymnMap(m,mglob)       
       mnm1_gz(2*mn-1) = 1    ! faulty mapping # 1
       mnm1_gz(2*mn  ) = 1    ! faulty mapping # 1
       DO n = mglob+1, nExtMax
          mn = mymnExtMap(m,n)
          indnm1 = mymnMap(m,n-1)
          mnm1_gz(2*mn-1) = 2*indnm1-1
          mnm1_gz(2*mn  ) = 2*indnm1  
       END DO
    END DO
    !$OMP END PARALLEL DO

    ! mapping mnp1_gz

    ALLOCATE(mnp1_gz  (2*mymnExtMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,indnp1)
    DO m = 1, mymMax
       mglob = lm2m(m)
       DO n = mglob, nMax-1
          mn = mymnExtMap(m,n)
          indnp1 = mymnMap(m,n+1)
          mnp1_gz(2*mn-1) = 2*indnp1-1
          mnp1_gz(2*mn  ) = 2*indnp1  
       END DO
       DO n = nMax, nExtMax
          mn = mymnExtMap(m,n)
          mnp1_gz(2*mn-1) = 1 ! faulty mapping # 2
          mnp1_gz(2*mn  ) = 1 ! faulty mapping # 2
       END DO
    END DO
    !$OMP END PARALLEL DO

    ! constant alfa_gz

    ALLOCATE(alfa_gz    (2*mymnExtMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn)
    DO m = 1, mymMax
       mglob = lm2m(m)
       mn = mymnExtMap(m,mglob)
       alfa_gz(2*mn-1) = 0.0_r8           ! corrects faulty mapping # 1
       alfa_gz(2*mn  ) = 0.0_r8           ! corrects faulty mapping # 1
       DO n = mglob+1, nExtMax
          mn = mymnExtMap(m,n)
          alfa_gz(2*mn-1) = -REAL(n-2,r8)*Epslon(mn)
          alfa_gz(2*mn  ) = -REAL(n-2,r8)*Epslon(mn)
       END DO
    END DO
    !$OMP END PARALLEL DO

    ! constant beta_gz

    ALLOCATE(beta_gz(2*mymnExtMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,mn2)
    DO m = 1, mymMax
       mglob = lm2m(m)
       DO n = mglob, nMax-1
          mn = mymnExtMap(m,n)
          mn2 = mymnExtMap(m,n+1)
          beta_gz(2*mn-1) = REAL(n+1,r8)*Epslon(mn2)
          beta_gz(2*mn  ) = REAL(n+1,r8)*Epslon(mn2)
       END DO
       DO n = nMax, nExtMax
          mn = mymnExtMap(m,n)
          beta_gz(2*mn-1) = 0.0_r8        ! corrects faulty mapping # 2
          beta_gz(2*mn  ) = 0.0_r8        ! corrects faulty mapping # 2
       END DO
    END DO
    !$OMP END PARALLEL DO

  END SUBROUTINE InitGozrim


  ! Gozrim: Meridional derivative of spectral field
  ! MERIDIONAL DERIVATIVE OF SPECTRAL FIELD
  !
  ! Derivative (with respect to phi) of a spectral field, computed
  ! by the following relation:
  !
  !  m       m   m       m   m
  ! G  = alfa * F  + beta * F
  !  n       n  n-1      n  n+1
  !
  ! for 0<=m<=mMax, m<=n<=nExtMax, 
  ! where F is the spectral field, G its derivative, alfa and beta are
  ! constants and
  !
  !  m     
  ! F  = 0 for n<m or n>nMax
  !  n     


  SUBROUTINE Gozrim1D(q, qder, mnRIExtFirst, mnRIExtLast)
    REAL(KIND=r8),    INTENT(IN ) :: q(2*mymnMax)
    REAL(KIND=r8),    INTENT(OUT) :: qder(2*mymnExtMax)
    INTEGER, INTENT(IN ) :: mnRIExtFirst 
    INTEGER, INTENT(IN ) :: mnRIExtLast
    INTEGER :: mn

    DO mn = mnRIExtFirst, mnRIExtLast
       qder(mn) = &
            beta_gz(mn) * q(mnp1_gz(mn)) + &
            alfa_gz(mn) * q(mnm1_gz(mn)) 
       qder(mn) = eriv*qder(mn)
    END DO
  END SUBROUTINE Gozrim1D





  SUBROUTINE Gozrim2D(q, qder, mnRIExtFirst, mnRIExtLast)
    REAL(KIND=r8),    INTENT(IN)  :: q(2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(OUT) :: qder(2*mymnExtMax,kMaxloc)
    INTEGER, INTENT(IN ) :: mnRIExtFirst 
    INTEGER, INTENT(IN ) :: mnRIExtLast
    INTEGER :: mn, k

    DO k = 1, kMaxloc
       DO mn = mnRIExtFirst, mnRIExtLast
          qder(mn,k) = &
               beta_gz(mn) * q(mnp1_gz(mn),k) + &
               alfa_gz(mn) * q(mnm1_gz(mn),k) 
          qder(mn,k) = eriv*qder(mn,k)
       END DO
    END DO
  END SUBROUTINE Gozrim2D


  SUBROUTINE InitImplDifu(ct_in, cq_in, dk_in, tk_in)
    REAL(KIND=r8),    INTENT(IN) :: ct_in(:)
    REAL(KIND=r8),    INTENT(IN) :: cq_in(:)
    REAL(KIND=r8),    INTENT(INOUT) :: dk_in
    REAL(KIND=r8),    INTENT(INOUT) :: tk_in
    INTEGER :: mn, m, n, mng
    INTEGER :: mglob
    !
    !     horizontal diffusion coefficients
    !     revised arbitrary even order horizontal diffusion
    !     earth's radius removed algebraically to avoid exponent limit
    !     problems
    !
    IF(TRIM(iqdif) == 'YES') THEN
       diffuseQ = .TRUE.
    ELSE
       diffuseQ = .FALSE.
    END IF
    ndho = ndord/2
    ! fnn1=(nMax-1)*nMax
    ! dk=(1.0_r8/fnn1)**ndho/(rhdifd*3600.0_r8)
    ! tk=(1.0_r8/fnn1)**ndho/(rhdift*3600.0_r8)
    !fnn1=(REAL(nMax,kind=r8)-1.0_r8)*REAL(nMax,kind=r8)
    !dk_in=((1.0_r8/fnn1)**ndho/(rhdifd*dt))*( (er**2)**ndho)
    !tk_in=((1.0_r8/fnn1)**ndho/(rhdift*dt))*( (er**2)**ndho)
    dk = dk_in/(er**2)**ndho
    tk = tk_in/(er**2)**ndho
    IF (nfctrl(2) >= 1) THEN
       WRITE (nfprt, '(/,1P,2(A,1PG12.5),A,I2)') &
            ' dk_in = ', dk_in, ' tk_in = ', tk_in, ' ndord = ', ndord
       WRITE (nfprt, '(1P,2(A,1PG12.5),/)') ' dk = ', dk, ' tk = ', tk
    ENDIF

    ALLOCATE (ct(kMaxloc))
    ct = ct_in(myfirstlev:mylastlev)
    ALLOCATE (cq(kMaxloc))
    cq = cq_in(myfirstlev:mylastlev)
    ALLOCATE (snnp1(2*mymnMax))
    ALLOCATE (snnp1_si(2*MNMax_si))
    DO m = 1, mymMax
       mglob = lm2m(m)
       DO n = mglob, nMax
          mn = mymnMap(m,n)
          snnp1(2*mn-1) = (n-1)*n
          snnp1(2*mn  ) = (n-1)*n
       END DO
    END DO
    DO mn=1,MNMax_si
       mng = mymnmap_si(mn)
       n = nMap(mng)
       snnp1_si(2*mn) = n*(n-1)
       snnp1_si(2*mn-1) = snnp1_si(2*mn)
    ENDDO
    !
    ! dependence on earth's radius removed
    !
    ALLOCATE(workImplDifu(2*mymnMax))
    DO mn = 1, 2*mymnMax
       workImplDifu(mn) = 2.0_r8*(snnp1(mn)**ndho)
    END DO
  END SUBROUTINE InitImplDifu

  ! impdif :  the diffusion operator (a power of the laplacian) is integrated
  ! in a fully implicit scheme (backward euler)
  ! The effect is a selective damping of the actualized variables 
  ! in spectral space.

  SUBROUTINE ImplDifu(dt, mnRIFirst, mnRILast)
    REAL(KIND=r8),    INTENT(IN) :: dt
    INTEGER, INTENT(IN) :: mnRIFirst
    INTEGER, INTENT(IN) :: mnRILast
    INTEGER :: mn, k
    REAL(KIND=r8) :: work(mnRIFirst:mnRILast)
    REAL(KIND=r8), ALLOCATABLE, DIMENSION(:,:) :: holda, holdb, holdc, hold
    !
    ! diffusion coefficient for divergence
    !
    IF (dodia(nDiag_hdidif) .OR. dodia(nDiag_hhedif) .OR. &
         dodia(nDiag_hvodif) .OR. dodia(nDiag_hmodif)) THEN
       ALLOCATE (hold (mnRIFirst:mnRILast,kMaxloc))
       IF (dodia(nDiag_hdidif)) THEN
          ALLOCATE (holda (mnRIFirst:mnRILast,kMaxloc))
          holda=qdivp(mnRIFirst:mnRILast,:)
       END IF
    END IF
    DO k = 1, kMaxloc
       DO mn = mnRIFirst, mnRILast
          qdivp(mn,k) = qdivp(mn,k)/(1.0_r8+dt*dk*workImplDifu(mn))
       END DO
    END DO
    IF (dodia(nDiag_hdidif)) THEN
       hold=qdivp(mnRIFirst:mnRILast,:)-holda
       CALL upspec (hold, mnRIFirst, mnRILast, nDiag_hdidif)
       DEALLOCATE (holda)
    END IF
    !
    ! diffusion coefficient for remaining fields
    !
    DO mn = mnRIFirst, mnRILast
       work(mn) = dt*tk*workImplDifu(mn)
    END DO
    !
    ! damp temp and vorticity 
    !
    IF (dodia(nDiag_hhedif)) THEN
       ALLOCATE (holda (mnRIFirst:mnRILast,kMaxloc))
       holda=qtmpp(mnRIFirst:mnRILast,:)
    END IF
    IF (dodia(nDiag_hvodif)) THEN
       ALLOCATE (holdb (mnRIFirst:mnRILast,kMaxloc))
       holdb=qrotp(mnRIFirst:mnRILast,:)
    END IF
    IF (dodia(nDiag_hmodif)) THEN
       ALLOCATE (holdc (mnRIFirst:mnRILast,kMaxloc))
       holdc=qqp(mnRIFirst:mnRILast,:)
    END IF
    IF (diffuseQ) THEN
       DO k = 1, kMaxloc
          DO mn = mnRIFirst, mnRILast
             qtmpp(mn,k) = (qtmpp(mn,k)+work(mn)*ct(k)*qlnpp(mn))/(1.0_r8+work(mn))
             qrotp(mn,k) = qrotp(mn,k)/(1.0_r8+work(mn))
             qqp  (mn,k) = (qqp  (mn,k)+work(mn)*cq(k)*qlnpp(mn))/(1.0_r8+work(mn))
          END DO
       END DO
    ELSE
       DO k = 1, kMaxloc
          DO mn = mnRIFirst, mnRILast
             qtmpp(mn,k) = (qtmpp(mn,k)+work(mn)*ct(k)*qlnpp(mn))/(1.0_r8+work(mn))
             qrotp(mn,k) = qrotp(mn,k)/(1.0_r8+work(mn))
          END DO
       END DO
    ENDIF
    IF (dodia(nDiag_hhedif)) THEN
       hold=qtmpp(mnRIFirst:mnRILast,:)-holda
       CALL upspec (hold, mnRIFirst, mnRILast, nDiag_hhedif)
       DEALLOCATE (holda)
    END IF
    IF (dodia(nDiag_hvodif)) THEN
       hold=qrotp(mnRIFirst:mnRILast,:)-holdb
       CALL upspec (hold, mnRIFirst, mnRILast, nDiag_hvodif)
       DEALLOCATE (holdb)
    END IF
    IF (dodia(nDiag_hmodif)) THEN
       hold=qqp(mnRIFirst:mnRILast,:)-holdc
       CALL upspec (hold, mnRIFirst, mnRILast, nDiag_hmodif)
       DEALLOCATE (holdc)
    END IF
    IF (ALLOCATED (hold)) THEN
       DEALLOCATE (hold)
    END IF
  END SUBROUTINE ImplDifu








  SUBROUTINE InitSemiImpl()
    INTEGER :: i, j, k
    REAL(KIND=r8)    :: det
    INTEGER :: lll(kMax), mmm(kMax)
    INTEGER, ALLOCATABLE :: ini(:), recs(:)


    ALLOCATE(sv(kmax))
    ALLOCATE(p1(kmax))
    ALLOCATE(p2(kmax))
    ALLOCATE(h1(kmax))
    ALLOCATE(h2(kmax))
    ALLOCATE(am(kmax,kmax))
    ALLOCATE(bm(kmax,kmax))
    ALLOCATE(cm(kmax,kmax))
    ALLOCATE(hm(kmax,kmax))
    ALLOCATE(hmt(kmax,kmax))
    ALLOCATE(tm(kmax,kmax))

    hm = 0.0_r8
    tm = 0.0_r8
    !cdir novector
    DO k=1, kmax-1
       hm(k,k) = 1.0_r8
       tm(k,k) = 0.5_r8*cp*(rpi(k)-1.0_r8)
    END DO
    DO k=1, kmax-1
       hm(k,k+1) = -1.0_r8
       tm(k,k+1) = 0.5_r8*cp*(1.0_r8-1.0_r8/rpi(k))
    END DO
    DO k=1, kmax
       hm(kmax,k) = del(k)
       tm(kmax,k) = gasr*del(k)
    END DO
    CALL iminv (hm,  kmax, det, lll, mmm)
    DO i=1, kmax
       DO j=1, kmax
          am(i,j) = 0.0_r8
          DO k=1, kmax
             am(i,j) = am(i,j) + hm(i,k)*tm(k,j)
          END DO
       END DO
    END DO

    tm = am
    hm = am
    hmt = TRANSPOSE(hm)
    am = am *ersqiv
    CALL iminv(tm, kmax, det, lll, mmm)

    !cdir novector
    DO k=1, kmax
       sv(k) = -del(k)
    END DO
    DO k=1, kmax-1
       p1(k) = 1.0_r8 / rpi(k)
       p2(k+1) = rpi(k)
    END DO
    p1(kmax) = 0.0_r8
    p2( 1  ) = 0.0_r8

    ALLOCATE (dm(kMax,kMax,nMax))
    dm =0.0_r8

    ALLOCATE (ini(0:maxnodes))
    ALLOCATE (recs(0:maxnodes))

    CALL Exchange_si(inibs_si,ini,nsends_si,mysends_si)

    nrecs_si = 0
    mapr_si = -1
    DO k=0,maxnodes-1
       IF (ini(k).ne.0) THEN
          ini(nrecs_si) = ini(k)
          nrecs_si = nrecs_si + 1
          recs(nrecs_si) = k
          mapr_si(k) = nrecs_si
       END IF
    END DO

    ALLOCATE (myrecs_si(nrecs_si))
    ALLOCATE (inibr_si(1:nrecs_si+1))
   
    inibr_si(1) = 1
    DO k=1,nrecs_si
       inibr_si(k+1)=inibr_si(k)+ini(k-1)
       myrecs_si(k) = recs(k)
    END DO

    DEALLOCATE(ini)
    DEALLOCATE(recs)

  END SUBROUTINE InitSemiImpl


  SUBROUTINE bmcm(dt, slagr)
    LOGICAL, INTENT(IN) :: slagr
    REAL(KIND=r8),    INTENT(IN) :: dt
    !
    !  local variables
    !
    REAL(KIND=r8), DIMENSION(kmax) ::  x1, x2
    INTEGER :: i, j, k, n, nn, lll(kMax), mmm(kMax)
    REAL(KIND=r8) :: temp, det
    REAL(KIND=r8)    :: rim(kMax,kMax)
    !
    IF (.NOT. slagr) THEN
       DO k=1,kmax-1
          h1(k) = p1(k) * tov(k+1) - tov(k)
       END DO
       h1(kmax)=0.0_r8 
       h2(   1)=0.0_r8 
       DO k=2, kmax
          h2(k) = tov(k) - p2(k) * tov(k-1)
       END DO
    ELSE
       DO k=1,kmax-1
          h1(k) = tov(k+1) * ( p1(k) - 1.0_r8 )
       END DO
       h1(kmax)=0.0_r8 
       h2(   1)=0.0_r8 
       DO k=2, kmax
          h2(k) = tov(k-1) * ( 1.0_r8 - p2(k) )
       END DO
    END IF
    DO k=1, kmax
       x1(k) = rk*tov(k)+0.5_r8*(ci(k+1)*h1(k)+ci(k)*h2(k))/del(k)
       x2(k) = 0.5_r8*(h1(k)+h2(k)) / del(k)
    END DO
    DO j=1, kmax
       DO k=1, kmax
          bm(k,j) = -x1(k)*del(j)
       END DO
    END DO
    DO k=1, kmax
       DO j=1, k
          bm(k,j) = bm(k,j) + x2(k)*del(j)
       END DO
    END DO
    DO k=1, kmax
       bm(k,k) = bm(k,k) - 0.5_r8*h2(k)
    END DO
    !$OMP PARALLEL DO PRIVATE(j,k)
    DO i=1, kmax
       DO j=1, kmax
          cm(i,j) = 0.0_r8 
          DO k=1, kmax
             cm(i,j) = cm(i,j) + am(i,k) * bm(k,j)
          END DO
          cm(i,j) = (cm(i,j)+raa*tov(i)*sv(j))*dt*dt
       END DO
    END DO
    !$OMP END PARALLEL DO

    rim=0.0_r8
    DO k=1,kmax
       rim(k,k)=1.0_r8
    END DO
    !$OMP PARALLEL DO PRIVATE(n,temp,det,lll,mmm)
    DO nn=1,nMax
       n=nn-1
       temp = n*(n+1)
       dm(:,:,nn)=rim-temp*cm
       CALL iminv (dm(1,1,nn), kmax, det, lll, mmm)
    END DO
    !$OMP END PARALLEL DO

  END SUBROUTINE bmcm


  SUBROUTINE SemiImpl_si(dt, slagr, mnfirst, mnlast, mnfirst_si, mnlast_si)
    REAL(KIND=r8),    INTENT(IN) :: dt
    LOGICAL, INTENT(IN) :: slagr
    INTEGER, INTENT(IN) :: mnfirst
    INTEGER, INTENT(IN) :: mnlast
    INTEGER, INTENT(IN) :: mnfirst_si
    INTEGER, INTENT(IN) :: mnlast_si
    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: n
    INTEGER :: mn
    INTEGER :: startMasCon
    REAL(KIND=r8)    :: tor
    REAL(KIND=r8)    :: aux(2*mnMax_si,kMax)

    !
    !  Transfer fields to semi-implicit partition
    !
    !$OMP BARRIER
    !$OMP SINGLE
    CALL SpectoSi(inibs_si,inibr_si,nsends_si,nrecs_si,mysends_si,myrecs_si,&
                  maps_si,mapr_si,kmax,kmaxloc,nlevperg_four,qtmpt,qdivt,&
                  qtmpt_si,qdivt_si,qs1=qlnpt,qs1_si=qlnpt_si)
    !$OMP END SINGLE
    DO k=1, kmax
       DO mn = mnfirst_si,mnlast_si
          aux(mn,k)=0.0_r8
       END DO
    END DO
    DO j=1, kmax
       DO k=1, kmax
!TO!cdir novector
          DO mn = mnfirst_si,mnlast_si
             aux(mn,j)=aux(mn,j)+am(j,k)*qtmpt_si(mn,k)
          END DO
       END DO
    END DO
    DO k=1, kmax
       tor=gasr*tov(k)/(er*er)
       DO mn = mnfirst_si,mnlast_si
          aux(mn,k)=aux(mn,k)+tor*qlnpt_si(mn)
          aux(mn,k)=aux(mn,k)*dt*snnp1_si(mn)
       END DO
    END DO
    DO k=1, kmax
       DO mn = mnfirst_si,mnlast_si
          aux(mn,k)=aux(mn,k)+qdivt_si(mn,k)
       END DO
    END DO
    DO mn = mnfirst_si,mnlast_si
       n = mynMap_si((mn+1)/2)
       DO k=1,kmax
          qdivt_si(mn,k)=0.0_r8
       END DO
       DO k=1, kmax
!TO!cdir novector
          DO i=1, kmax
             qdivt_si(mn,i)=qdivt_si(mn,i)+dm(i,k,n)*aux(mn,k)
          END DO
       END DO
    END DO

    DO k=1, kmax
       DO mn = mnfirst_si,mnlast_si
          aux(mn,k)=0.0_r8
       END DO
    END DO
    DO j=1, kmax
       DO k=1, kmax
!TO!cdir novector
          DO mn = mnfirst_si,mnlast_si
             aux(mn,j)=aux(mn,j)+bm(j,k)*qdivt_si(mn,k)
          END DO
       END DO
    END DO
    DO k=1, kmax
       DO mn = mnfirst_si,mnlast_si
          qtmpt_si(mn,k)=qtmpt_si(mn,k)+dt*aux(mn,k)
       END DO
    END DO
    DO mn = mnfirst_si,mnlast_si
       aux(mn,1)=0.0_r8
    END DO
    DO k=1, kmax
!TO!cdir novector
       DO mn = mnfirst_si,mnlast_si
          aux(mn,1)=aux(mn,1)+sv(k)*qdivt_si(mn,k)
       END DO
    END DO
    DO mn = mnfirst_si,mnlast_si
       qlnpt_si(mn)=qlnpt_si(mn)+dt*aux(mn,1)
    END DO

    !
    !   Transfer back to normal spectral partition
    !
    !$OMP BARRIER
    !$OMP SINGLE
    CALL SitoSpec(inibr_si,inibs_si,nrecs_si,nsends_si,myrecs_si,mysends_si,&
                  mapr_si,maps_si,kmax,kmaxloc,nlevperg_four,.true.,qtmpp,qdivp,&
                  qtmpt_si,qdivt_si,qs1=qlnpt,qs1_si=qlnpt_si)
    !$OMP END SINGLE

    ! Implementa Conservacao de ln(ps)
       startMasCon = mnfirst
       IF (MasCon.and..not.MasCon_ps) THEN
          IF (haveM1) THEN
             startMasCon = max(3,mnfirst)
          END IF
       END IF
       IF (slagr) THEN
          DO mn = startMasCon,mnlast
             qlnpl(mn)=qlnpt(mn)
             qlnpp(mn)=qlnpl(mn)-qgzs(mn)/(gasr*tbar)
          END DO
       ELSE
          DO mn = startMasCon,mnlast
             qlnpp(mn)=qlnpt(mn)
          END DO
       ENDIF
!   ENDIF
  END SUBROUTINE SemiImpl_si


  SUBROUTINE SemiImpl(dt, slagr, mnRIFirst, mnRILast)
    REAL(KIND=r8),    INTENT(IN) :: dt
    LOGICAL, INTENT(IN) :: slagr
    INTEGER, INTENT(IN) :: mnRIFirst
    INTEGER, INTENT(IN) :: mnRILast
    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: n
    INTEGER :: mn
    INTEGER :: startMasCon
    REAL(KIND=r8)    :: tor
    REAL(KIND=r8)    :: aux(2*mymnMax,kMax)

    DO k=1, kmax
       DO mn = mnRIFirst, mnRILast
          aux(mn,k)=0.0_r8
       END DO
    END DO
    DO j=1, kmax
       DO k=1, kmax
!TO!cdir novector
          DO mn = mnRIFirst, mnRILast
             aux(mn,j)=aux(mn,j)+am(j,k)*qtmpt(mn,k)
          END DO
       END DO
    END DO
    DO k=1, kmax
       tor=gasr*tov(k)/(er*er)
       DO mn = mnRIFirst, mnRILast
          aux(mn,k)=aux(mn,k)+tor*qlnpt(mn)
          aux(mn,k)=aux(mn,k)*dt*snnp1(mn)
       END DO
    END DO
    DO k=1, kmax
       DO mn = mnRIFirst, mnRILast
          aux(mn,k)=aux(mn,k)+qdivt(mn,k)
       END DO
    END DO
    DO mn = mnRIFirst, mnRILast
       n = mynMap((mn+1)/2)
       DO k=1,kmax
          qdivp(mn,k)=0.0_r8
       END DO
       DO k=1, kmax
!TO!cdir novector
          DO i=1, kmax
             qdivp(mn,i)=qdivp(mn,i)+dm(i,k,n)*aux(mn,k)
          END DO
       END DO
    END DO

    DO k=1, kmax
       DO mn = mnRIFirst, mnRILast
          aux(mn,k)=0.0_r8
       END DO
    END DO
    DO j=1, kmax
       DO k=1, kmax
!TO!cdir novector
          DO mn = mnRIFirst, mnRILast
             aux(mn,j)=aux(mn,j)+bm(j,k)*qdivp(mn,k)
          END DO
       END DO
    END DO
    DO k=1, kmax
       DO mn = mnRIFirst, mnRILast
          qtmpp(mn,k)=qtmpt(mn,k)+dt*aux(mn,k)
       END DO
    END DO
    DO mn = mnRIFirst, mnRILast
       aux(mn,1)=0.0_r8
    END DO
    DO k=1, kmax
!TO!cdir novector
       DO mn = mnRIFirst, mnRILast
          aux(mn,1)=aux(mn,1)+sv(k)*qdivp(mn,k)
       END DO
    END DO
    ! Implementa Conservacao de ln(ps)
    startMasCon = mnRIFirst
    IF (MasCon.and..not.MasCon_ps) THEN
       IF (mnRIFirst == 1 .AND. haveM1) THEN
          startMasCon = 3
       END IF
    END IF
    IF (slagr) THEN
       DO mn = startMasCon, mnRILast
          qlnpl(mn)=qlnpt(mn)+dt*aux(mn,1)
          qlnpp(mn)=qlnpl(mn)-qgzs(mn)/(gasr*tbar)
       END DO
    ELSE
       DO mn = startMasCon, mnRILast
          qlnpp(mn)=qlnpt(mn)+dt*aux(mn,1)
       END DO
    ENDIF
  END SUBROUTINE SemiImpl

  ! InitUvtodz: Mapping Functions and Local Constants


  SUBROUTINE InitUvtodz()
    INTEGER :: m, mglob, n, mn, mnExt

    ! mapping mnir_uv

    ALLOCATE(mnir_uv(2*mymnMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,mnExt)
    DO m = 1, mymMax
       mglob = lm2m(m)
       DO n = mglob, nMax
          mn    = mymnMap(m,n)
          mnExt = mymnExtMap(m,n)
          mnir_uv(2*mn-1) = 2*mnExt
          mnir_uv(2*mn  ) = 2*mnExt-1
       END DO
    END DO
    !$OMP END PARALLEL DO

    ! mapping mnm1_uv

    ALLOCATE(mnm1_uv(2*mymnMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,mnExt)
    DO m = 1, mymMax
       mglob = lm2m(m)
       mn    = mymnMap(m,mglob)
       mnExt = mymnExtMap(m,mglob)
       mnm1_uv(2*mn-1) = 2*mnExt-1   ! faulty mapping
       mnm1_uv(2*mn  ) = 2*mnExt     ! faulty mapping
       DO n = mglob+1, nMax
          mn    = mymnMap(m,n)
          mnExt = mymnExtMap(m,n-1)
          mnm1_uv(2*mn-1) = 2*mnExt-1
          mnm1_uv(2*mn  ) = 2*mnExt
       END DO
    END DO
    !$OMP END PARALLEL DO

    ! mapping mnp1_uv

    ALLOCATE(mnp1_uv(2*mymnMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,mnExt)
    DO m = 1, mymMax
       mglob = lm2m(m)
       DO n = mglob, nMax
          mn    = mymnMap(m,n)
          mnExt = mymnExtMap(m,n+1)
          mnp1_uv(2*mn-1) = 2*mnExt-1
          mnp1_uv(2*mn  ) = 2*mnExt
       END DO
    END DO
    !$OMP END PARALLEL DO

    ! constant alfa_uv

    ALLOCATE(alfa_uv(2*mymnMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn)
    DO m = 1, mymMax
       mglob = lm2m(m)
       DO n = mglob, nMax
          mn = mymnMap(m,n)
          alfa_uv(2*mn-1) = -REAL(mglob-1,r8)
          alfa_uv(2*mn  ) = REAL(mglob-1,r8)
       END DO
    END DO
    !$OMP END PARALLEL DO

    ! constant beta_uv

    ALLOCATE(beta_uv(2*mymnMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,mnExt)
    DO m = 1, mymMax
       mglob = lm2m(m)
       DO n = mglob, nMax
          mn    = mymnMap(m,n)
          mnExt = mymnExtMap(m,n+1)
          beta_uv(2*mn-1) = REAL(n-1,r8)*Epslon(mnExt)
          beta_uv(2*mn  ) = REAL(n-1,r8)*Epslon(mnExt)
       END DO
    END DO
    !$OMP END PARALLEL DO

    ! constant gama_uv

    ALLOCATE(gama_uv(2*mymnExtMax))
    !$OMP PARALLEL DO PRIVATE(mglob,n,mn,mnExt)
    DO m = 1, mymMax
       mglob = lm2m(m)
       mn = mymnMap(m,mglob)
       gama_uv(2*mn-1) = 0.0_r8     ! corrects faulty mapping
       gama_uv(2*mn  ) = 0.0_r8     ! corrects faulty mapping
       DO n = mglob+1, nMax
          mn    = mymnMap(m,n)
          mnExt = mymnExtMap(m,n)
          gama_uv(2*mn-1) = REAL(n,r8) * Epslon(mnExt)
          gama_uv(2*mn  ) = REAL(n,r8) * Epslon(mnExt)
       END DO
    END DO
    !$OMP END PARALLEL DO

  END SUBROUTINE InitUvtodz


  ! Uvtodz: Divergence and Vorticity from Velocity fields 


  SUBROUTINE Uvtodz(qup, qvp, qdivt, qrott, mnRIFirst, mnRILast)
    REAL(KIND=r8),    INTENT(IN ) :: qup(2*mymnExtMax,kMaxloc)
    REAL(KIND=r8),    INTENT(IN ) :: qvp(2*mymnExtMax,kMaxloc)
    REAL(KIND=r8),    INTENT(OUT) :: qdivt(2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(OUT) :: qrott(2*mymnMax,kMaxloc)
    INTEGER, INTENT(IN ) :: mnRIFirst
    INTEGER, INTENT(IN ) :: mnRILast
    INTEGER :: mn, k

    DO k = 1, kMaxloc
       DO mn = mnRIFirst, mnRILast
          qdivt(mn,k) = &
               alfa_uv(mn) * qup(mnir_uv(mn),k) + &
               beta_uv(mn) * qvp(mnp1_uv(mn),k) - &
               gama_uv(mn) * qvp(mnm1_uv(mn),k)
          qdivt(mn,k) = eriv * qdivt(mn,k)
          qrott(mn,k) = &
               alfa_uv(mn) * qvp(mnir_uv(mn),k) - &
               beta_uv(mn) * qup(mnp1_uv(mn),k) + &
               gama_uv(mn) * qup(mnm1_uv(mn),k)
          qrott(mn,k) = eriv * qrott(mn,k)
       END DO
    END DO
  END SUBROUTINE Uvtodz
END MODULE SpecDynamics
