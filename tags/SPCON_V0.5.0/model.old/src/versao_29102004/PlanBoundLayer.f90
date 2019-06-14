!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE PlanBoundLayer
  USE Constants, ONLY :     &
       cp,            &
       grav,          &
       gasr
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: InitPlanBoundLayer
  PUBLIC :: ympbl0
  PUBLIC :: ympbl1
  PUBLIC :: InitGwdd
  PUBLIC :: Gwdd

  REAL,    PARAMETER :: eps   =   0.608
  REAL,    PARAMETER :: shrmin=   1.0e-5
  REAL,    PARAMETER :: facl  =   0.1
  INTEGER, PARAMETER :: nitr  =   2
  REAL,    PARAMETER :: gkm0  =   1.00
  REAL,    PARAMETER :: gkh0  =   0.10
  REAL,    PARAMETER :: gkm1  = 300.0
  REAL,    PARAMETER :: gkh1  = 300.0
  REAL,    PARAMETER :: vk0   =   0.4
  INTEGER, PARAMETER :: kmean =   1
  REAL,    PARAMETER :: a1    =   0.92
  REAL,    PARAMETER :: a2    =   0.74
  REAL,    PARAMETER :: b1    =  16.6
  REAL,    PARAMETER :: b2    =  10.1
  REAL,    PARAMETER :: c1    =   0.08
  REAL,    PARAMETER :: deltx =   0.0


  REAL, PARAMETER :: x        = 1.0
  REAL, PARAMETER :: xx       = x*x
  REAL, PARAMETER :: g        = 1.0
  REAL, PARAMETER :: gravi    = 9.81
  REAL, PARAMETER :: agrav    = 1.0/gravi
  REAL, PARAMETER :: rgas     = 287.0
  REAL, PARAMETER :: akwnmb   = 2.5e-05
  REAL, PARAMETER :: lstar    = 1.0/akwnmb
  REAL, PARAMETER :: gocp     = gravi/1005.0
  INTEGER         :: nbase
  INTEGER         :: nbasep1
  INTEGER         :: nbasem1
  INTEGER         :: nthin
  INTEGER         :: nthinp


  REAL               :: alfa
  REAL               :: beta
  REAL               :: gama
  REAL               :: dela
  REAL               :: r1
  REAL               :: r2
  REAL               :: r3
  REAL               :: r4
  REAL               :: s1
  REAL               :: s2
  REAL               :: rfc
  REAL, ALLOCATABLE  :: sigkiv(:)
  REAL, ALLOCATABLE  :: sigr(:)
  REAL, ALLOCATABLE  :: sigriv(:)
  REAL, ALLOCATABLE  :: a0(:)
  REAL, ALLOCATABLE  :: b0(:)
  REAL, ALLOCATABLE  :: con0(:)
  REAL, ALLOCATABLE  :: con1(:)
  REAL, ALLOCATABLE  :: con2(:)
  REAL, ALLOCATABLE  :: t0(:)
  REAL, ALLOCATABLE  :: t1(:)
  REAL               :: c0

CONTAINS


  SUBROUTINE InitPlanBoundLayer(kmax, sig, delsig, sigml, nfprt, nferr)
    INTEGER, INTENT(IN) :: kmax
    REAL,    INTENT(IN) :: sig(kmax)
    REAL,    INTENT(IN) :: delsig(kmax)
    REAL,    INTENT(IN) :: sigml(kmax)
    INTEGER, INTENT(IN   ) :: nfprt
    INTEGER, INTENT(IN   ) :: nferr
    INTEGER  :: k
    REAL     :: gam1
    REAL     :: gam2
    REAL     :: gbyr
    REAL     :: akappa
    REAL     :: sigk(kmax)
    ALLOCATE(sigkiv(kmax))
    ALLOCATE(sigr  (kmax))
    ALLOCATE(sigriv(kmax))
    ALLOCATE(a0    (kmax))
    ALLOCATE(b0    (kmax))
    ALLOCATE(con0  (kmax))
    ALLOCATE(con1  (kmax))
    ALLOCATE(con2  (kmax))
    ALLOCATE(t0    (kmax))
    ALLOCATE(t1    (kmax))
    gam1=1./3.-2.*a1/b1
    gam2=(b2+6.*a1)/b1
    alfa=b1*(gam1-c1)+3.*(a2+2.*a1)
    beta=b1*(gam1-c1)
    gama=a2/a1*(b1*(gam1+gam2)-3.*a1)
    dela=a2/a1* b1* gam1
    r1  =0.5*gama/alfa
    r2  =    beta/gama
    r3  =2.*(2.*alfa*dela-gama*beta)/(gama*gama)
    r4  =r2*r2
    s1  =3.*a2* gam1
    s2  =3.*a2*(gam1+gam2)
    !     
    !     critical flux richardson number
    !     
    rfc =s1/s2
    akappa=gasr/cp
    !
    DO k = 1, kmax
       WRITE(*,*)sig(k),akappa,delsig(k),gasr,cp
       sigk  (k)=sig(k)**akappa
       sigkiv(k)=1./sigk(k)
       con0  (k)=gasr*delsig(k)/(grav*sig(k))
    END DO
    a0    (kmax)=0.
    b0    (   1)=0.
    sigr  (kmax)=0.
    sigriv(   1)=0.
    gbyr        =(grav/gasr)**2
    DO k = 1, kmax-1
       con1  (   k)=grav*sigml(k+1)/(gasr*(sig(k)-sig(k+1)))
       con2  (   k)=grav*con1(k)
       con1  (   k)=con1(k)*con1(k)
       t0    (   k)=(sig(k+1)-sigml(k+1))/(sig(k+1)-sig(k))
       t1    (   k)=(sigml(k+1)-sig(k  ))/(sig(k+1)-sig(k))
       sigr  (   k)=sigk(k)*sigkiv(k+1)
       sigriv( k+1)=sigk(k+1)*sigkiv(k)
       a0(k)=gbyr*sigml(k+1)**2/(delsig(k  )*(sig(k)-sig(k+1)))
       b0(k+1)=gbyr*sigml(k+1)**2/(delsig(k+1)*(sig(k)-sig(k+1)))
    END DO
    c0=grav/(gasr*delsig(1))
    CALL InitGwdd(sigml, kmax, nfprt, nferr)
  END SUBROUTINE InitPlanBoundLayer

  SUBROUTINE InitGwdd(si, kmax, nfprt, nferr)
    INTEGER, INTENT(IN   ) :: kmax
    REAL,    INTENT(IN   ) :: si(kmax+1)
    INTEGER, INTENT(IN   ) :: nfprt
    INTEGER, INTENT(IN   ) :: nferr
    !
    !
    ! InitGwdd   : module initialization
    !
    !=========================================================================
    !
    !  kmax......Number of sigma levels  
    !  si........si(l)=1.0-ci(l).  
    !  nfprt.....standard print out unit
    !            0 no print, 1 less detail, 2 more detail, 3 most detail  
    !  nferr.....error print out unit
    !            0 no print, 1 less detail, 2 more detail, 3 most detail  
    !=========================================================================
    !
    !     nthin is the number of low layers strapped together
    !
    DO nthin=1,(kmax+1)
       IF(si(nthin).GT.0.025)EXIT
    END DO
    nthinp=nthin+1
    !     
    !     nbase is determined so that the lower third of the atmosphere
    !     (approximately) is used for the base layer
    !     
    DO nbase=1,(kmax+1)
       IF(si(nbase).LT.0.6667) EXIT
    END DO
    IF(nbase.LE.1)THEN
       WRITE(nfprt,9976)nbase,si
       WRITE(nferr,9976)nbase,si
       STOP 9976
    END IF
    IF(0.6667-si(nbase).GT.si(nbase-1)-0.6667)nbase=nbase-1
    IF(nbase.LE.1)THEN
       WRITE(nfprt,9976)nbase,si
       WRITE(nferr,9976)nbase,si
       STOP 9976
    END IF
    nbasep1=nbase+1
    nbasem1=nbase-1
9976 FORMAT(' MODEL TOO COARSE FOR MULTILAYER BASE LAYER.'/, &
         ' CHANGE THE MODEL VERTICAL STRUCTURE OR RUN WITH IGWD SET TO NO.'/, &
         ' NBASE=',I5,' SI='/(' ',5G16.8/))
  END SUBROUTINE InitGwdd





  SUBROUTINE gauss0(g,d,ncols,kmax,nmax)
    !
    !-----------------------------------------------------------------------
    ! g.........gmtx
    ! d.........d can be (gmu,gmq or gmt)
    !           gmt    temperature related matrix
    !                 gmt(i,k,1)*d(gt(i,k-1))/dt+gmt(i,k,2)*d(gt(i,k))/dt=gmt(i,k,3)
    !                 gmt(i,1,1)=0.
    !                 gmt(*,*,1)...dimensionless
    !                 gmt(*,*,2)...dimensionless
    !                 gmt(*,*,3)...deg/sec
    !           gmq    specific humidity related matrix
    !                 gmq(i,k,1)*d(gq(i,k-1))/dt+gmq(i,k,2)*d(gq(i,k))/dt=gmq(i,k,3)
    !                 gmq(i,1,1)=0.
    !                 gmq(*,*,1)...dimensionless
    !                 gmq(*,*,2)...dimensionless
    !                 gmq(*,*,3)...kg/kg/sec
    !           gmu    wind related matrix
    !                 gmu(i,k,1)*d(gu(i,k-1))/dt+gmu(i,k,2)*d(gu(i,k))/dt=gmu(i,k,3)
    !                 gmu(i,k,1)*d(gv(i,k-1))/dt+gmu(i,k,2)*d(gv(i,k))/dt=gmu(i,k,4)
    !                 gmu(i,1,1)=0.
    !                 gmu(*,*,1)...dimensionless
    !                 gmu(*,*,2)...dimensionless
    !                 gmu(*,*,3)...m/sec**2
    !                 gmu(*,*,4)...m/sec**2
    ! ncols.....Number of grid points on a gaussian latitude circle
    ! kmax......Number of grid points at vertcal 
    ! nmax
    !-----------------------------------------------------------------------
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: kmax
    INTEGER, INTENT(in   ) :: nmax
    REAL,    INTENT(inout) :: g(ncols,kmax,nmax)
    REAL,    INTENT(out  ) :: d(ncols,kmax,nmax-1)
    INTEGER :: i 
    INTEGER :: k
    !cdir unroll=2
    DO k = kmax-1, 1, -1
       DO i = 1, ncols
          g(i,k,3)=g(i,k,3)/g(i,k+1,2)
          g(i,k,2)=g(i,k,2)-g(i,k,3)*g(i,k+1,1)
          g(i,k,4)=g(i,k,4)-g(i,k,3)*g(i,k+1,4)
       END DO
       IF (nmax == 5) THEN
          DO i = 1, ncols
             g(i,k,5)=g(i,k,5)-g(i,k,3)*g(i,k+1,5)
          END DO
       END IF
    END DO
    DO k = 1, kmax
       DO i = 1, ncols
          d(i,k,1)=g(i,k,1)
          d(i,k,2)=g(i,k,2)
          d(i,k,3)=g(i,k,4)
       END DO
    END DO
    IF (nmax == 5) THEN
       DO k = 1, kmax
          DO i = 1, ncols
             d(i,k,4)=g(i,k,5)
          END DO
       END DO
    END IF
  END SUBROUTINE gauss0





  SUBROUTINE gauss1(g,ncols,kmax,ndim)
    !
    !!-----------------------------------------------------------------------
    ! ncols....number of grid points on a gaussian latitude circle
    ! kmax.....kpbl or kqpbl number of  vertical grid points..
    !          kpbl number of layers pbl process is included( for u v,t )..
    !          kqpbl  number of layers pbl process is included( for q     )
    ! ndim.....dimension of  g idim=3 or idim=4
    ! g........gmt, gmq, gmu (temperature related matrix, 
    !                         specific humidity related matrix,
    !                         wind related matrix
    !!-----------------------------------------------------------------------
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: kmax
    INTEGER, INTENT(in   ) :: ndim
    REAL,    INTENT(inout) :: g(ncols,kmax,ndim)
    INTEGER :: i 
    INTEGER :: k

    !cdir unroll=2
    DO k=2,kmax
       DO i=1,ncols
          g(i,k,3)=(g(i,k,3)-g(i,k,1)*g(i,k-1,3))/g(i,k,2)
       END DO
    END DO
    IF (ndim == 4) THEN
       !cdir unroll=2

       DO  k=2,kmax
          DO i=1,ncols
             g(i,k,4)=(g(i,k,4)-g(i,k,1)*g(i,k-1,4))/g(i,k,2)
          END DO
       END DO
    END IF
  END SUBROUTINE gauss1





  SUBROUTINE ympbl0 ( &
       gu    ,gv    ,gt    ,gq    ,delsig,ncols , &
       kmax  ,delt  ,csqiv ,gmt   ,gmq   ,gmu   ,gl0  )
    !
    !
    ! ympbl0 :performs momentum, water vapour and sensible heat diffusion
    !         on planetary boundary layer.
    !-----------------------------------------------------------------------
    !
    !           input values
    !-----------------------------------------------------------------------
    !..imx.......Number of grid points on a gaussian latitude circle (ncols+ 2)
    !..ncols......Number of grid points on a gaussian latitude circle
    !..kmax......Number of grid points at vertcal  
    !..cp........Specific heat of air           (j/kg/k) 
    !..gasr......Gas constant of dry air        (j/kg/k) 
    !..grav......gravity constant               (m/s**2) 
    !..gu,gv,gt,gq is at time level t-dt
    !
    !..gu    (zonal      velocity)*sin(colat)
    !..gv    (meridional velocity)*sin(colat)
    !..gt    temperature
    !..gq    specific humidity
    !..fsen  sensible heat flux in w/m**2
    !..flat  latent   heat fulx in w/m**2
    !..fmom  momentum flux      in n/m**2
    !
    !..delsig     k=2  ****gu,gv,gt,gq,gyu,gyv,gtd,gqd,sig*** } delsig(2)
    !             k=3/2----sigml,ric,rf,km,kh,b,l -----------
    !             k=1  ****gu,gv,gt,gq,gyu,gyv,gtd,gqd,sig*** } delsig(1)
    !             k=1/2----sigml ----------------------------
    !..delt   time interval
    !..gl0    maximum mixing length l0 in blackerdar's formula
    !                                  l=k0*z/(1+k0*z/l0)
    !..csqiv  1./sin(colat)**2
    !..ncols   number of grid points on a gaussian latitude circle
    !..kpbl   number of layers pbl process is included( for u v,t )
    !..kqpbl  number of layers pbl process is included( for q     )
    !-----------------------------------------------------------------------
    !           work arrays
    !-----------------------------------------------------------------------
    !..gwrk
    !..gld
    !..gln
    !-----------------------------------------------------------------------
    !           output values
    !-----------------------------------------------------------------------
    !..gmt    temperature related matrix
    !         gmt(i,k,1)*d(gt(i,k-1))/dt+gmt(i,k,2)*d(gt(i,k))/dt=gmt(i,k,3)
    !         gmt(i,1,1)=0.
    !     gmt(*,*,1)...dimensionless
    !     gmt(*,*,2)...dimensionless
    !     gmt(*,*,3)...deg/sec
    !..gmq    specific humidity related matrix
    !         gmq(i,k,1)*d(gq(i,k-1))/dt+gmq(i,k,2)*d(gq(i,k))/dt=gmq(i,k,3)
    !         gmq(i,1,1)=0.
    !     gmq(*,*,1)...dimensionless
    !     gmq(*,*,2)...dimensionless
    !     gmq(*,*,3)...kg/kg/sec
    !..gmu    wind related matrix
    !         gmu(i,k,1)*d(gu(i,k-1))/dt+gmu(i,k,2)*d(gu(i,k))/dt=gmu(i,k,3)
    !         gmu(i,k,1)*d(gv(i,k-1))/dt+gmu(i,k,2)*d(gv(i,k))/dt=gmu(i,k,4)
    !         gmu(i,1,1)=0.
    !     gmu(*,*,1)...dimensionless
    !     gmu(*,*,2)...dimensionless
    !     gmu(*,*,3)...m/sec**2
    !     gmu(*,*,4)...m/sec**2
    !..gl0    maximum mixing length l0 in blackerdar's formula
    !         this is retained as a first guess for next time step
    !
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: kmax
    REAL,    INTENT(in   ) :: gu(ncols,kmax)
    REAL,    INTENT(in   ) :: gv(ncols,kmax)
    REAL,    INTENT(in   ) :: gt(ncols,kmax)
    REAL,    INTENT(in   ) :: gq(ncols,kmax)
    REAL,    INTENT(in   ) :: delsig(kmax)
    REAL,    INTENT(in   ) :: delt
    REAL,    INTENT(in   ) :: csqiv(ncols)
    REAL,    INTENT(out  ) :: gmt(ncols,kmax,3)
    REAL,    INTENT(out  ) :: gmq(ncols,kmax,3)
    REAL,    INTENT(out  ) :: gmu(ncols,kmax,4)
    REAL,    INTENT(inout) :: gl0(ncols)

    REAL :: a(kmax)
    REAL :: b(kmax)
    REAL :: gwrk(ncols,kmax,8)
    REAL :: gmtx(ncols,kmax,5)
    REAL :: gld (ncols)
    REAL :: gln (ncols)
    !     
    !     eps   ; virtual temperature correction factor
    !     shrmin; squre of minimum wind shear, this is in order to avoid
    !     large richardson number   (1./sec**2)
    !     facl  ; appears in l0 computation
    !     nitr  ; number of iteration computing l0
    !     gkm0  ; minimum value of eddy diffusion coefficient for momentm
    !     (m/sec**2)
    !     gkh0  ; minimum value of eddy diffusion coefficient for sensible heat
    !     (m/sec**2)
    !     gkm1  ; maximum value of eddy diffusion coefficient for momentm
    !     (m/sec**2)
    !     gkh1  ; maximum value of eddy diffusion coefficient for sensible heat
    !     (m/sec**2)
    !     vk0   ; von-karman constant
    !

    INTEGER :: k 
    INTEGER :: i 
    INTEGER :: itr 
    INTEGER :: icnt
    REAL    :: concos 
    REAL    :: s1ms2g 
    REAL    :: x 
    REAL    :: y 
    REAL    :: fac 
    REAL    :: rfx

    !     
    !     ichk(kmax) is flag to vectorize.
    !     
    INTEGER :: ichk(kmax)
    REAL    :: c
    REAL    :: twodt
    REAL    :: twodti
    !     
    !     constants concerning mixing length scaling
    !     (mellor & yamada '82 rev.geophys.space sci. p851-874)
    !     
    IF (delt == deltx) THEN
       WRITE (*, "(' ERROR - delt == 0  in ympbl0' )")
       STOP "**(ymbpl0)"
    END IF
    twodt =2.*delt
    twodti=1./twodt
    DO k = 1, kmax
       a(k)=twodt*a0(k)
       b(k)=twodt*b0(k)
    END DO
    c=twodt*c0
    !     
    !     gwrk(3)   virtual potential temperature
    !     gwrk(7)   temperature at the interface of two adjacent layers
    !     
    DO k = 1, kmax-1
       DO i = 1, ncols
          gwrk(i,k,7)=t0(k)*gt(i,k)+t1(k)*gt(i,k+1)
       END DO
    END DO

    DO k = 1, kmax
       DO i = 1, ncols
          gwrk(i,k,3)=sigkiv(k)*gt(i,k)*(1.+eps*gq(i,k))
       END DO
    END DO
    !     
    !     gwrk(2)   stability
    !     gwrk(6)   square of vertical wind shear
    !     
    !cdir unroll=2
    DO k = 1, kmax-1
       DO i = 1, ncols
          concos=con1(k)*csqiv(i)
          gwrk(i,k,2)=con2(k)*(gwrk(i,k+1,3)-gwrk(i,k,3)) &
               /((t0(k)*gwrk(i,k,3)+t1(k)*gwrk(i,k+1,3))*gwrk(i,k,7))
          gwrk(i,k,7)=1./(gwrk(i,k,7)*gwrk(i,k,7))
          gwrk(i,k,6)=concos*gwrk(i,k,7) &
               *((gu(i,k)-gu(i,k+1))**2+(gv(i,k)-gv(i,k+1))**2)
          gwrk(i,k,6)=MAX(shrmin,gwrk(i,k,6))
       END DO
    END DO
    !     
    !     gwrk(4)        richardson number
    !     gwrk(5)   flux richardson number
    !     gwrk(8)   flux richardson number
    !     
    DO k = 1,(kmax-1)
       DO i = 1, ncols
          gwrk(i,k,4)=gwrk(i,k,2)/gwrk(i,k,6)
          gwrk(i,k,5)= r1*(gwrk(i,k,4)+r2 &
               -SQRT(gwrk(i,k,4)*(gwrk(i,k,4)-r3)+r4))
          gwrk(i,k,5)=MIN(rfc,gwrk(i,k,5))
          gwrk(i,k,8)=        gwrk(i,k,5)
          !     
          !     gwrk(3)   shbar
          !     gwrk(4)   smbar
          !     
          !     eliminate negative value for s1-s2*gwrk(i,1,5):
          !     gwrk(i,1,5) is s1/s2 under some circumstances
          !     which makes this expression zero.  machine roundoff
          !     can produce an unphysical negative value in this case.
          !     it is used as sqrt argument in later loop.
          !     
          s1ms2g=s1-s2*gwrk(i,k,5)
          IF (ABS(s1ms2g) < 1.0e-10) s1ms2g=0.0
          gwrk(i,k,3)=s1ms2g/(1.0-gwrk(i,k,5))
          !     
          !     gwrk(i,1,3)=(s1-s2*gwrk(i,1,5))/(1.0-gwrk(i,1,5))
          !     end of  negative sqrt argument trap
          !     
          gwrk(i,k,4)=gwrk(i,k,3)*(beta-alfa*gwrk(i,k,5))/ &
               (dela-gama*gwrk(i,k,5))
          !     
          !     gwrk(5)   sqrt(w) or b/l
          !     gwrk(4)   km/l**2
          !     gwrk(3)   kh/l**2
          !     
          gwrk(i,k,5)=SQRT(b1*gwrk(i,k,4)*(1.-gwrk(i,k,5))*gwrk(i,k,6))
          gwrk(i,k,4)=gwrk(i,k,5)*gwrk(i,k,4)
          gwrk(i,k,3)=gwrk(i,k,5)*gwrk(i,k,3)
       END DO
    END DO
    !     
    !     gwrk(1)   height at the layer interface
    !     
    k=1
    DO i = 1, ncols
       gwrk(i,k,1)=con0(k)*gt(i,k)
    END DO

    DO k = 2, kmax
       DO i = 1, ncols
          gwrk(i,k,1)=gwrk(i,k-1,1)+con0(k)*gt(i,k)
       END DO
    END DO

    DO itr = 1, nitr
       !     
       !     gwrk(2)   mixing length
       !     gwrk(2)   b     :b**2 is eddy enegy
       !     
       gln = 0.0
       gld = 0.0
       gwrk(:,1,2) = 0.0
       !cdir unroll=2
       DO k = 1, kmax-1
          DO i = 1, ncols
             gwrk(i,k+1,2)=vk0*gl0(i)*gwrk(i,k,1) &
                  /(gl0(i)+vk0*gwrk(i,k,1))
             gwrk(i,k+1,2)=gwrk(i,k+1,2)*gwrk(i,k,5)
          END DO
       END DO
       k=1
       DO i = 1, ncols
          gwrk(i,k,2)=     1.e-3
       END DO
       k=1
       DO i = 1, ncols
          x=0.5*delsig(k)*(gwrk(i,k,2)+gwrk(i,k+1,2))
          y=x*0.5*gwrk(i,k,1)
          gln(i)=gln(i)+y
          gld(i)=gld(i)+x
       END DO
       k=kmax
       DO i = 1, ncols
          x=0.5*delsig(k)*gwrk(i,k,2)
          y=x*0.5*(gwrk(i,k,1)+gwrk(i,k-1,1))
          gln(i)=gln(i)+y
          gld(i)=gld(i)+x
       END DO
       IF (kmax > 2) THEN
          DO k = 2, kmax-1
             DO i = 1, ncols
                x=0.5*delsig(k)*(gwrk(i,k,2)+gwrk(i,k+1,2))
                y=x*0.5*(gwrk(i,k,1)+gwrk(i,k-1,1))
                gln(i)=gln(i)+y
                gld(i)=gld(i)+x
             END DO
          END DO
       END IF
       DO i = 1, ncols
          gl0(i)=facl*gln(i)/gld(i)
       END DO
       !     
       !     iteration that determines mixing length
       !     
    END DO
    !
    !     gwrk(5)   mixing length
    !     
    DO k = 1, kmax-1
       DO i=1,ncols
          gwrk(i,k,5)=vk0*gl0(i)*gwrk(i,k,1)/(gl0(i)+vk0*gwrk(i,k,1))
       END DO
    END DO
    !     
    !     gwrk(1)   km
    !     gwrk(2)   kh
    !     
    IF (kmean == 0) THEN
       DO k = 1, kmax-1
          DO i = 1, ncols
             gwrk(i,k,1)=MIN(gkm1, &
                  MAX(gkm0,gwrk(i,k,5)**2*gwrk(i,k,4)))
             gwrk(i,k,2)=MIN(gkh1, &
                  MAX(gkh0,gwrk(i,k,5)**2*gwrk(i,k,3)))
          END DO
       END DO
    ELSE
       DO k = 1, kmax-1
          DO i = 1, ncols
             gmtx(i,k,1)=gwrk(i,k,5)**2*gwrk(i,k,4)
             gmtx(i,k,2)=gwrk(i,k,5)**2*gwrk(i,k,3)
          END DO
       END DO
       fac=0.25
       IF (kmax >= 4) THEN
          DO k = 2, kmax-2
             DO i = 1, ncols
                gwrk(i,k,1)=fac* &
                     (gmtx(i,k-1,1)+2.0*gmtx(i,k,1)+gmtx(i,k+1,1))
                gwrk(i,k,2)=fac* &
                     (gmtx(i,k-1,2)+2.0*gmtx(i,k,2)+gmtx(i,k+1,2))
             END DO
          END DO
       END IF
       DO i = 1, ncols
          gwrk(i,     1,1)=0.5*(gmtx(i,     1,1)+gmtx(i,     2,1))
          gwrk(i,     1,2)=0.5*(gmtx(i,     1,2)+gmtx(i,     2,2))
          gwrk(i,kmax-1,1)=0.5*(gmtx(i,kmax-1,1)+gmtx(i,kmax-2,1))
          gwrk(i,kmax-1,2)=0.5*(gmtx(i,kmax-1,2)+gmtx(i,kmax-2,2))
       END DO
       DO k = 1, kmax-1
          DO i = 1, ncols
             gwrk(i,k,1)=MIN(gkm1,MAX(gkm0,gwrk(i,k,1)))
             gwrk(i,k,2)=MIN(gkh1,MAX(gkh0,gwrk(i,k,2)))
          END DO
       END DO
       rfx=rfc-0.001
       IF (kmax >= 3) THEN
          DO i = 1, ncols
             icnt=0
             DO k = kmax-1, 2, -1
                IF (gwrk(i,k,8) > rfx .AND. gwrk(i,k-1,8) <= rfx) THEN
                   icnt=icnt+1
                   ichk(icnt)=k
                END IF
             END DO
             IF (icnt /= 0) THEN
                gwrk(i,ichk(1),1)=gkm0
                gwrk(i,ichk(1),2)=gkh0
             END IF
          END DO
       END IF
    END IF
    !     
    !     momentum diffusion
    !     
    DO k = 1, kmax-1
       DO i = 1, ncols
          gwrk(i,k  ,1)=gwrk(i,k,1)*gwrk(i,k,7)
          gwrk(i,k  ,3)=a(k  )*gwrk(i,k,1)
          gwrk(i,k+1,4)=b(k+1)*gwrk(i,k,1)
          !     
          !     gwrk(1)   difference of pseudo v wind ( km is destroyed )
          !     gwrk(5)   difference of pseudo u wind ( b  is destroyed )
          !     
          gwrk(i,k  ,5)=gu(i,k)-gu(i,k+1)
          gwrk(i,k  ,1)=gv(i,k)-gv(i,k+1)
       END DO
    END DO
    DO k = 1, kmax
       IF (k == 1) THEN
          DO i = 1, ncols
             gmtx(i,k,1)=0.
             gmtx(i,k,2)=1.+gwrk(i,k,3)
             gmtx(i,k,3)=  -gwrk(i,k,3)
             gmtx(i,k,4)=-twodti*gwrk(i,k,3)*gwrk(i,k,5)
             gmtx(i,k,5)=-twodti*gwrk(i,k,3)*gwrk(i,k,1)
          END DO
       ELSE IF (k == kmax) THEN
          DO i = 1, ncols
             gmtx(i,k,1)=  -gwrk(i,k,4)
             gmtx(i,k,2)=1.+gwrk(i,k,4)
             gmtx(i,k,3)=0.
             gmtx(i,k,4)=twodti*gwrk(i,k,4)*gwrk(i,k-1,5)
             gmtx(i,k,5)=twodti*gwrk(i,k,4)*gwrk(i,k-1,1)
          END DO
       ELSE IF (kmax > 2) THEN
          DO i = 1, ncols
             gmtx(i,k,1)=  -gwrk(i,k,4)
             gmtx(i,k,2)=1.+gwrk(i,k,3)+gwrk(i,k,4)
             gmtx(i,k,3)=  -gwrk(i,k,3)
             gmtx(i,k,4)=(gwrk(i,k,4)*gwrk(i,k-1,5)- &
                  gwrk(i,k,3)*gwrk(i,k,5))*twodti
             gmtx(i,k,5)=(gwrk(i,k,4)*gwrk(i,k-1,1)- &
                  gwrk(i,k,3)*gwrk(i,k,1))*twodti
          END DO
       END IF
    END DO
    CALL gauss0(gmtx,gmu,ncols,kmax,5)
    !     
    !     water vapour diffusion
    !     
    DO k = 1, kmax-1
       DO i = 1, ncols
          gwrk(i,k  ,2)=gwrk(i,k,2)*gwrk(i,k,7)
          gwrk(i,k  ,3)=a(k  )*gwrk(i,k,2)
          gwrk(i,k+1,4)=b(k+1)*gwrk(i,k,2)
       END DO
    END DO
    !     
    !     above part is common to sensible heat diffuion
    !     
    !     gwrk(1)   difference of specific humidity
    !     
    DO k = 1, kmax-1
       DO i = 1, ncols
          gwrk(i,k,1)=gq(i,k)-gq(i,k+1) 
       END DO
    END DO
    DO k = 1, kmax
       IF (k == 1) THEN
          DO i = 1, ncols
             gmtx(i,k,1)=0.
             gmtx(i,k,2)=1.+gwrk(i,k,3)
             gmtx(i,k,3)=  -gwrk(i,k,3)
             gmtx(i,k,4)=-twodti*gwrk(i,k,3)*gwrk(i,k  ,1)
          END DO
       ELSE IF (k == kmax) THEN
          DO i = 1, ncols
             gmtx(i,k,1)=  -gwrk(i,k,4)
             gmtx(i,k,2)=1.+gwrk(i,k,4)
             gmtx(i,k,3)=0.
             gmtx(i,k,4)= twodti*gwrk(i,k,4)*gwrk(i,k-1,1)
          END DO
       ELSE IF (kmax > 2) THEN
          DO i = 1, ncols
             gmtx(i,k,1)=  -gwrk(i,k,4)
             gmtx(i,k,2)=1.+gwrk(i,k,3)+gwrk(i,k,4)
             gmtx(i,k,3)=  -gwrk(i,k,3)
             gmtx(i,k,4)=(gwrk(i,k,4)*gwrk(i,k-1,1)- &
                  gwrk(i,k,3)*gwrk(i,k,1))*twodti
          END DO
       END IF
    END DO
    CALL gauss0(gmtx,gmq,ncols,kmax,4)
    !     
    !     sensible heat diffusion
    !     
    DO k = 1, kmax
       IF (k == 1) THEN
          DO i = 1, ncols
             gmtx(i,k,1)=0.
             gmtx(i,k,2)=1.+gwrk(i,k,3)
             gmtx(i,k,3)=-sigr(k)*gwrk(i,k,3)
             gmtx(i,k,4)=-gwrk(i,k,3)*(gt(i,k)- &
                  sigr(k)*gt(i,k+1))*twodti
          END DO
       ELSE IF (k == kmax) THEN
          DO i = 1, ncols
             gmtx(i,k,1)=-sigriv(k)*gwrk(i,k,4)
             gmtx(i,k,2)=1.+gwrk(i,k,4)
             gmtx(i,k,3)=0.
             gmtx(i,k,4)=twodti*gwrk(i,k,4)* &
                  (sigriv(k)*gt(i,k-1)-gt(i,k))
          END DO
       ELSE IF (kmax > 2) THEN
          DO i = 1, ncols
             gmtx(i,k,1)=-sigriv(k)*gwrk(i,k,4)
             gmtx(i,k,2)=1.+gwrk(i,k,3)+gwrk(i,k,4)
             gmtx(i,k,3)=-sigr  (k)*gwrk(i,k,3)
             gmtx(i,k,4)=(gwrk(i,k,4)*(sigriv(k)*gt(i,k-1)-gt(i,k)) &
                  -gwrk(i,k,3)*(gt(i,k)-sigr(k)*gt(i,k+1)))*twodti
          END DO
       END IF
    END DO
    CALL gauss0(gmtx,gmt,ncols,kmax,4)
  END SUBROUTINE ympbl0






  SUBROUTINE ympbl1(gmt,gmq,gmu,ncols,kpbl,kqpbl)
    !
    !
    ! ympbl1 :does second half part of gauss elimination
    !         and obtains time tendency for t,q,u,v
    !
    !-----------------------------------------------------------------------
    !           input values
    !-----------------------------------------------------------------------
    !..ncols   number of grid points on a gaussian latitude circle
    !..kpbl   number of layers pbl process is included( for u v,t )
    !..kqpbl  number of layers pbl process is included( for q     )
    !..gmt    temperature related matrix
    !         for k.ge.2
    !         gmt(i,k,1)*d(gt(i,k-1))/dt+gmt(i,k,2)*d(gt(i,k))/dt=gmt(i,k,3)
    !         for k.eq.1,the solution is already obtained in sib.
    !         gmt(i,1,3)=d(gt(i,1))/dt
    !..gmq    specific humidity related matrix
    !         for k.ge.2
    !         gmq(i,k,1)*d(gq(i,k-1))/dt+gmq(i,k,2)*d(gq(i,k))/dt=gmq(i,k,3)
    !         for k.eq.1,the solution is already obtained in sib.
    !         gmq(i,1,3)=d(gq(i,1))/dt
    !..gmu    wind related matrix
    !         for k.ge.2
    !         gmu(i,k,1)*d(gu(i,k-1))/dt+gmu(i,k,2)*d(gu(i,k))/dt=gmu(i,k,3)
    !         gmu(i,k,1)*d(gv(i,k-1))/dt+gmu(i,k,2)*d(gv(i,k))/dt=gmu(i,k,3)
    !         for k.eq.1, the solution is already obtained in sib.
    !         gmu(i,1,3)=d(gu(i,1))/dt
    !         gmu(i,1,4)=d(gv(i,1))/dt
    !-----------------------------------------------------------------------
    !           output values
    !-----------------------------------------------------------------------
    !         gmt(i,k,3)=d(gt(i,k))/dt
    !         gmq(i,k,3)=d(gq(i,k))/dt
    !         gmu(i,k,3)=d(gu(i,k))/dt
    !         gmu(i,k,4)=d(gv(i,k))/dt
    !         where  da/dt=(a(t+dt)-a(t-dt))/(2*dt)
    !
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: kpbl
    INTEGER, INTENT(in   ) :: kqpbl
    REAL,    INTENT(inout) :: gmt(ncols,kpbl)
    REAL,    INTENT(inout) :: gmq(ncols,kqpbl)
    REAL,    INTENT(inout) :: gmu(ncols,kpbl)
    CALL gauss1(gmt,ncols,kpbl ,3)
    CALL gauss1(gmq,ncols,kqpbl,3)
    CALL gauss1(gmu,ncols,kpbl ,4)
  END SUBROUTINE ympbl1


  SUBROUTINE Gwdd(psfc, u, v, t, chug, chvg, xdrag, ydrag, &
       nfprt, var, varcut, si, sl, del, ncols, kmax)
    !
    !
    ! gwdd   :change on gwdd by cptec on 29 july 1994 to improve
    !         vectorization performance on gwdd: dr. j.p. bonatti
    !==========================================================================  
    !  ncols......Number of grid points on a gaussian latitude circle  
    !  jmax......Number of gaussian latitudes  
    !  kmax......Number of sigma levels  
    !  latco.....latitude
    !  xdrag.....gravity wave drag surface zonal stress
    !  ydrag.....gravity wave drag surface meridional stress
    !  t.........Temperature         
    !  u.........(zonal      velocity)*sin(colat)     
    !  v.........(meridional velocity)*sin(colat)      
    !  chug......gravity wave drag zonal momentum change 
    !  chvg......gravity wave drag meridional momentum change
    !  psfc......surface pressure   
    !  si........si(l)=1.0-ci(l).  
    !  ci........sigma value at each level.      
    !  sl........sigma value at midpoint of
    !                                         each layer : (k=287/1005)
    !
    !                                                                     1
    !                                             +-                   + ---
    !                                             !     k+1         k+1!  k
    !                                             !si(l)   - si(l+1)   !
    !                                     sl(l) = !--------------------!
    !                                             !(k+1) (si(l)-si(l+1)!
    !                                             +-                  -+    
    !  del.......sigma spacing for each layer computed in routine "setsig".   
    !  varcut....cut off height variance in m**2 for gravity wave drag
    !  var.......Surface height variance   
    !  nfprt.....standard print out unit
    !            0 no print, 1 less detail, 2 more detail, 3 most detail  
    !==========================================================================
    IMPLICIT NONE
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: kmax

    REAL,    INTENT(out  ) :: xdrag(ncols)
    REAL,    INTENT(out  ) :: ydrag(ncols)

    REAL,    INTENT(in   ) :: t    (ncols,kmax)
    REAL,    INTENT(in   ) :: u    (ncols,kmax)
    REAL,    INTENT(in   ) :: v    (ncols,kmax)

    REAL,    INTENT(out  ) :: chug (ncols,kmax)
    REAL,    INTENT(out  ) :: chvg (ncols,kmax)
    REAL,    INTENT(inout) :: psfc (ncols)


    REAL,    INTENT(in   ) ::  si  (kmax+1)      
    REAL,    INTENT(in   ) ::  sl  (kmax)        
    REAL,    INTENT(in   ) ::  del (kmax)        


    REAL,    INTENT(in   ) :: varcut             
    REAL,    INTENT(inout) :: var   (ncols)  

    INTEGER, INTENT(in   ) :: nfprt        


    REAL    :: xtens (ncols,kmax+1)
    REAL    :: ytens (ncols,kmax+1)
    REAL    :: ro    (ncols,kmax)
    REAL    :: pp    (ncols,kmax)
    REAL    :: tensio(ncols,kmax+1)  
    REAL    :: dz    (ncols,kmax)
    REAL    :: ppp   (ncols,kmax+1)
    REAL    :: dragsf(ncols)    
    REAL    :: nbar  (ncols)
    REAL    :: bv    (ncols,kmax)
    REAL    :: robar (ncols)
    REAL    :: ubar  (ncols)
    REAL    :: vbar  (ncols)  
    INTEGER :: icrilv(ncols)
    REAL    :: speeds(ncols)
    REAL    :: ang   (ncols)
    REAL    :: coef  (ncols)



    INTEGER :: k   
    INTEGER :: i   
    REAL    :: vai1    
    REAL    :: cte   
    REAL    :: frsf    
    REAL    :: gstar   
    REAL    :: roave   
    REAL    :: velco   
    REAL    :: fro2    
    REAL    :: delve2  
    REAL    :: richsn  
    REAL    :: crifro  
    REAL    :: crif2   
    INTEGER :: icount  
    REAL    :: vsqua   
    REAL    :: vaisd   
    REAL    :: dsigma  
    !
    !     surface pressure and constrain the variance
    !
    DO i=1,ncols
       icrilv(i)=0
       psfc(i)=EXP(psfc(i))*10.0
       var(i) =MIN(varcut,var(i))
       var(i) =MAX(0.0,var(i))
       coef(i)=gravi/psfc(i)*.01
    END DO
    !
    !     compute pressure at every level
    !
    DO k=1,(kmax+1)
       DO i=1,ncols
          ppp(i,k)=si(k)*psfc(i)
       END DO
    END DO
    !
    !     compute pressure at every layer, density
    !     at every layer
    !
    DO k=1,kmax
       DO i=1,ncols
          pp(i,k)=sl(k)*psfc(i)
          ro(i,k)=pp(i,k)/(rgas*t(i,k))
       END DO
    END DO
    !
    !     compute dz at every level from 2 to kmax
    !
    DO k=2,kmax
       DO i=1,ncols
          dz(i,k)=0.5*agrav*(1.0/ro(i,k-1)+1.0/ro(i,k))* &
               (pp(i,k-1)-pp(i,k))
       END DO
    END DO
    !     
    !     vaisala frequency
    !
    DO k=2,nbase
       DO i=1,ncols
          vai1=MAX(0.0,(t(i,k)-t(i,k-1))/dz(i,k)+gocp)
          bv(i,k)=SQRT(vai1*2.0*gravi/(t(i,k)+t(i,k-1)))
       END DO
    END DO
    !
    !     end of input and elementary computations
    !
    !     surface and base layer stress                  
    !
    !     base layer stress is defined in terms of a vertical ave.
    !
    DO i=1,ncols
       robar(i)=0.0
       ubar(i)=0.0
       vbar(i)=0.0
       nbar(i)=0.0
    END DO
    !
    !     mass weighted veritcal average of density, velocity
    !
    DO k=1,nbasem1
       DO i=1,ncols
          robar(i)=robar(i)+ro(i,k)*(ppp(i,k)-ppp(i,k+1))
          ubar(i)=ubar(i)+u(i,k)*(ppp(i,k)-ppp(i,k+1))
          vbar(i)=vbar(i)+v(i,k)*(ppp(i,k)-ppp(i,k+1))
       END DO
    END DO
    !
    !     vertical mass weighted average of the brunt-vaisiala freq.
    !
    DO k=2,nbase
       DO i=1,ncols
          nbar(i)=nbar(i)+bv(i,k)*(pp(i,k-1)-pp(i,k))
       END DO
    END DO
    DO i=1,ncols
       cte=1.0/(ppp(i,1)-ppp(i,nbase))
       robar(i)=robar(i)*cte*100.0
       ubar(i)=ubar(i)*cte
       vbar(i)=vbar(i)*cte
       nbar(i)=nbar(i)/(pp(i,1)-pp(i,nbase))
    END DO
    !
    !     end vertical average
    !     definition of surface wind vector
    !
    DO i=1,ncols
       speeds(i)=SQRT(ubar(i)*ubar(i)+vbar(i)*vbar(i))
       IF(speeds(i).EQ.0.0)THEN
          WRITE(nfprt,222)
       ENDIF
       ang(i)=ATAN2(vbar(i),ubar(i))
       !
       !     stress at the surface level lev=1
       !
       frsf=nbar(i)*SQRT(var(i))/speeds(i)
       IF(speeds(i).EQ.0.0)THEN
          tensio(i,1)=0.0
       ELSEIF(nbar(i).NE.0.0)THEN
          !
          !     use non linear weighting function
          !
          gstar=g*frsf*frsf/(frsf*frsf+xx)
          tensio(i,1)=gstar*(robar(i)*speeds(i)*speeds(i)*speeds(i))/ &
               (nbar(i)*lstar)
       ELSE
          tensio(i,1)=0.0
       ENDIF
       xtens(i,1)=COS(ang(i))*tensio(i,1)
       ytens(i,1)=SIN(ang(i))*tensio(i,1)
       !
       !     save surface values
       !     
       dragsf(i)=tensio(i,1)
       xdrag(i)=xtens(i,1)
       ydrag(i)=ytens(i,1)
    END DO
    IF(nthin.GT.1)THEN
       DO k=1,nthin
          DO i=1,ncols
             tensio(i,k)=tensio(i,1)
             xtens(i,k)=xtens(i,1)
             ytens(i,k)=ytens(i,1)
          END DO
       END DO
    ENDIF
    !
    !     scalar product of lower wind vector and surface wind
    !
    DO k=nthinp,nbase
       DO i=1,ncols
          roave=50.0*(ro(i,k-1)+ro(i,k))
          !
          !     *100 to convert to newton/m2
          !     velocity component paralell to surface velocity
          !
          velco=0.5*((u(i,k-1)+u(i,k))*ubar(i)+ &
               (v(i,k-1)+v(i,k))*vbar(i))/speeds(i)
          !
          !     tau doesn't change in the base layer because of a
          !     critical level i.e. velco < 0.0
          !
          IF(velco.LE.0.0)THEN
             tensio(i,k)=tensio(i,k-1)
             !
             !     froude number squared
             !
          ELSE
             fro2=bv(i,k)/(akwnmb*roave*velco*velco*velco)* &
                  tensio(i,k-1)
             !     
             !     denominator of richardson number
             !
             delve2=(u(i,k)-u(i,k-1))*(u(i,k)-u(i,k-1))+ &
                  (v(i,k)-v(i,k-1))*(v(i,k)-v(i,k-1))
             !     
             !     richardson number
             !     
             IF(delve2.NE.0.0)THEN
                richsn=dz(i,k)*bv(i,k)*dz(i,k)*bv(i,k)/delve2
             ELSE
                richsn=99999.0
             ENDIF
             !     
             !     tau in the base layer does not change because of the
             !     richardson criterion
             !     
             IF(richsn.LE.0.25)THEN
                tensio(i,k)=tensio(i,k-1)
                !     
                !     tau in the base layer does change if the local froude
                !     excedes the critical froude number... the so called
                !     froude number reduction.
                !
             ELSE
                crifro=1.0-0.25/richsn
                crif2=crifro*crifro
                IF(k.EQ.2)crif2= MIN (0.7,crif2)
                IF(fro2.GT.crif2)THEN
                   tensio(i,k)=crif2/fro2*tensio(i,k-1)
                ELSE
                   tensio(i,k)=tensio(i,k-1)
                ENDIF
             ENDIF
          ENDIF
          xtens(i,k)=tensio(i,k)*COS(ang(i))
          ytens(i,k)=tensio(i,k)*SIN(ang(i))
       END DO
    END DO
    !
    !     stress from base level to top level
    !
    icount=0
    DO k=nbasep1,(kmax+1)
       DO i=1,ncols
          !     
          !     the stress is always initialized to zero
          !     
          tensio(i,k)=0.0
          IF(icrilv(i).NE.1.AND.k.NE.(kmax+1))THEN
             roave=50.0*(ro(i,k-1)+ro(i,k))
             !     
             !     *100 to convert to newton/m2
             !     vaisala frequency
             !     
             vai1=(t(i,k)-t(i,k-1))/dz(i,k)+gocp
             vsqua=vai1*2.0*gravi/(t(i,k)+t(i,k-1))
             !     
             !     velocity component paralell to surface velocity
             !     scalar product of upper and surface wind vector
             !     
             velco=0.5*((u(i,k-1)+u(i,k))*ubar(i)+ &
                  (v(i,k-1)+v(i,k))*vbar(i))/speeds(i)
             !     
             !     froude number squared
             !     fro2=vaisd/(akwnmb*roave*velco*velco*velco)*tensio(i,k-1)
             !     denominator of richardson number
             !     
             delve2=(u(i,k)-u(i,k-1))*(u(i,k)-u(i,k-1))+ &
                  (v(i,k)-v(i,k-1))*(v(i,k)-v(i,k-1))
             !     
             !     richardson number
             !     
             IF(delve2.NE.0.0)THEN
                richsn=dz(i,k)*dz(i,k)*vsqua/delve2
             ELSE
                richsn=99999.0
                icount=icount+1
             ENDIF
             IF(vai1.GE.0.0.AND.velco.GE.0.0.AND.richsn.GT.0.25) THEN
                vaisd=SQRT(vsqua)
                fro2=vaisd/(akwnmb*roave*velco*velco*velco)* &
                     tensio(i,k-1)
                !     
                !     critical froude number
                !     
                crifro=1.0-0.25/richsn
                crif2=crifro*crifro
                !     
                !     end critical froude number
                !     
                IF(fro2.GE.crif2)THEN
                   tensio(i,k)=crif2/fro2*tensio(i,k-1)
                ELSE
                   tensio(i,k)=tensio(i,k-1)
                ENDIF
                xtens(i,k)=tensio(i,k)*COS(ang(i))
                ytens(i,k)=tensio(i,k)*SIN(ang(i))
             ELSE
                icrilv(i)   = 1
                tensio(i,k) = 0.0
                xtens(i,k)  = 0.0
                ytens(i,k)  = 0.0
             ENDIF
          ELSE
             tensio(i,k) = 0.0
             xtens(i,k)  = 0.0
             ytens(i,k)  = 0.0
          ENDIF
       END DO
    END DO
    !     
    !     end stress
    !     
    !     momentum change for free atmosphere
    !     
    DO k=nthinp,kmax
       DO i=1,ncols
          chug(i,k)=-coef(i)/del(k)*(xtens(i,k+1)-xtens(i,k))
          chvg(i,k)=-coef(i)/del(k)*(ytens(i,k+1)-ytens(i,k))
       END DO
    END DO
    !     
    !     momentum change near the surface
    !     if lowest layer is very thin, it is strapped to next layer
    !     
    dsigma=si(nthinp)-si(1)
    DO i=1,ncols
       chug(i,1)=coef(i)/dsigma*(xtens(i,nthinp)-xtens(i,1))
       chvg(i,1)=coef(i)/dsigma*(ytens(i,nthinp)-ytens(i,1))
    END DO
    IF(nthin.GT.1)THEN
       DO k=2,nthin
          DO i=1,ncols
             chug(i,k)=chug(i,1)
             chvg(i,k)=chvg(i,1)
          END DO
       END DO
    ENDIF
222 FORMAT('SPEEDS EQ ZERO IN GWD')
  END SUBROUTINE Gwdd
END MODULE PlanBoundLayer
