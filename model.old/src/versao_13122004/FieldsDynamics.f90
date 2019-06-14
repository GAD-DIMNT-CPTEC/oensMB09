!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE FieldsDynamics
  IMPLICIT NONE


  ! Spectral fields: 13 2D and 7 1D


  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:) :: qtmpp
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:) :: qtmpt
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:) :: qrotp
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:) :: qrott
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:) :: qdivp
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:) :: qdivt
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:) :: qqp
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:) :: qqt
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:) :: qup
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:) :: qvp
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:) :: qtphi
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:) :: qqphi
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:) :: qdiaten
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ) :: qlnpp
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ) :: qlnpt
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ) :: qgzslap
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ) :: qgzs
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ) :: qplam
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ) :: qpphi
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ) :: qgzsphi


  ! Gaussian fields: 28 3D and 12 2D


  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgyu
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgyv
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtd
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgqd
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgu
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgv
!IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII  
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgw ! vertical velocity @ layer 
!IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII    
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgdiv
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgrot
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgq
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtmp
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgum
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgvm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgumm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgvmm  
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: omg  
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtmpm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtmpmm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgqm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgqmm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgdivm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgdivmm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgyum
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgyvm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtdm
!IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgqdm ! tendencia de q no departure
!IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtphi
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgqphi
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fguphi
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgvphi
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtphim
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtphimm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtlam
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgqlam
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgulam
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgvlam
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtlamm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtlammm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgvdlnp
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fglnps
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgps
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fglnpm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fglnpmm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgpphi
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgplam
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgplamm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgplammm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgpphim
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgpphimm
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgzslam
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgzsphi
  REAL, ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgvdlnpm
CONTAINS
  SUBROUTINE InitFields(ibMax, kMax, jbMax, mnMax, mnExtMax)
    INTEGER, INTENT(IN) :: ibMax
    INTEGER, INTENT(IN) :: kMax
    INTEGER, INTENT(IN) :: jbMax
    INTEGER, INTENT(IN) :: mnMax
    INTEGER, INTENT(IN) :: mnExtMax

    ALLOCATE (qtmpp(2*mnMax,kMax))
    qtmpp=0.0
    ALLOCATE (qtmpt(2*mnMax,kMax))
    qtmpt=0.0
    ALLOCATE (qrotp(2*mnMax,kMax))
    qrotp=0.0
    ALLOCATE (qrott(2*mnMax,kMax))
    qrott=0.0
    ALLOCATE (qdivp(2*mnMax,kMax))
    qdivp=0.0
    ALLOCATE (qdivt(2*mnMax,kMax))
    qdivt=0.0
    ALLOCATE (qqp(2*mnMax,kMax))
    qqp=0.0
    ALLOCATE (qqt(2*mnMax,kMax))
    qqt=0.0
    ALLOCATE (qdiaten(2*mnMax,kMax))
    qdiaten=0.0
    ALLOCATE (qup(2*mnExtMax,kMax))
    qup=0.0
    ALLOCATE (qvp(2*mnExtMax,kMax))
    qvp=0.0
    ALLOCATE (qtphi(2*mnExtMax,kMax))
    qtphi=0.0
    ALLOCATE (qqphi(2*mnExtMax,kMax))
    qqphi=0.0
    ALLOCATE (qlnpp(2*mnMax))
    qlnpp=0.0
    ALLOCATE (qlnpt(2*mnMax))
    qlnpt=0.0
    ALLOCATE (qgzslap(2*mnMax))
    qgzslap=0.0
    ALLOCATE (qgzs(2*mnMax))
    qgzs=0.0
    ALLOCATE (qplam(2*mnMax))
    qplam=0.0
    ALLOCATE (qpphi(2*mnExtMax))
    qpphi=0.0
    ALLOCATE (qgzsphi(2*mnExtMax))
    qgzsphi=0.0

    ALLOCATE (fgyu(ibMax,kMax,jbMax))
    fgyu=0.0
    ALLOCATE (fgyv(ibMax,kMax,jbMax))
    fgyv=0.0
    ALLOCATE (fgtd(ibMax,kMax,jbMax))
    fgtd=0.0
    ALLOCATE (fgqd(ibMax,kMax,jbMax))
    fgqd=0.0
    ALLOCATE (fgu(ibMax,kMax,jbMax))
    fgu=0.0
    ALLOCATE (fgv(ibMax,kMax,jbMax))
    fgv=0.0
!IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
    ALLOCATE (fgw(ibMax,kMax,jbMax)) ! vertical velocity @ layer 
    fgw=0.0
!IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
    ALLOCATE (fgdiv(ibMax,kMax,jbMax))
    fgdiv=0.0
    ALLOCATE (fgrot(ibMax,kMax,jbMax))
    fgrot=0.0
    ALLOCATE (fgq(ibMax,kMax,jbMax))
    fgq=0.0
    ALLOCATE (fgtmp(ibMax,kMax,jbMax))
    fgtmp=0.0
    ALLOCATE (fgum(ibMax,kMax,jbMax))
    fgum=0.0
    ALLOCATE (fgvm(ibMax,kMax,jbMax))
    fgvm=0.0
    ALLOCATE (fgumm(ibMax,kMax,jbMax))
    fgumm=0.0
    ALLOCATE (fgvmm(ibMax,kMax,jbMax))
    fgvmm=0.0    
    ALLOCATE (omg(ibMax,kMax,jbMax))
    omg=0.0
    ALLOCATE (fgtmpm(ibMax,kMax,jbMax))
    fgtmpm=0.0
    ALLOCATE (fgtmpmm(ibMax,kMax,jbMax))
    fgtmpmm=0.0
    ALLOCATE (fgqm(ibMax,kMax,jbMax))
    fgqm=0.0 
    ALLOCATE (fgqmm(ibMax,kMax,jbMax))
    fgqmm=0.0
    ALLOCATE (fgdivm(ibMax,kMax,jbMax))
    fgdivm=0.0
    ALLOCATE (fgdivmm(ibMax,kMax,jbMax))
    fgdivmm=0.0
    ALLOCATE (fgyum(ibMax,kMax,jbMax))
    fgyum=0.0
    ALLOCATE (fgyvm(ibMax,kMax,jbMax))
    fgyvm=0.0
    ALLOCATE (fgtdm(ibMax,kMax,jbMax))
    fgtdm=0.0
!IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
    ALLOCATE (fgqdm(ibMax,kMax,jbMax))
    fgqdm=0.0
!IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
    ALLOCATE (fgtphi(ibMax,kMax,jbMax))
    fgtphi=0.0
    ALLOCATE (fgqphi(ibMax,kMax,jbMax))
    fgqphi=0.0
    ALLOCATE (fguphi(ibMax,kMax,jbMax))
    fguphi=0.0
    ALLOCATE (fgvphi(ibMax,kMax,jbMax))
    fgvphi=0.0
    ALLOCATE (fgtphim(ibMax,kMax,jbMax))
    fgtphim=0.0
    ALLOCATE (fgtphimm(ibMax,kMax,jbMax))
    fgtphimm=0.0
    ALLOCATE (fgtlam(ibMax,kMax,jbMax))
    fgtlam=0.0
    ALLOCATE (fgqlam(ibMax,kMax,jbMax))
    fgqlam=0.0
    ALLOCATE (fgulam(ibMax,kMax,jbMax))
    fgulam=0.0
    ALLOCATE (fgvlam(ibMax,kMax,jbMax))
    fgvlam=0.0
    ALLOCATE (fgtlamm(ibMax,kMax,jbMax))
    fgtlamm=0.0
    ALLOCATE (fgtlammm(ibMax,kMax,jbMax))
    fgtlammm=0.0
    ALLOCATE (fgvdlnp(ibMax,     jbMax))
    fgvdlnp=0.0
    ALLOCATE (fglnps(ibMax,     jbMax))
    fglnps=0.0
    ALLOCATE (fgps(ibMax,     jbMax))
    fgps=0.0
    ALLOCATE (fglnpm(ibMax,     jbMax))
    fglnpm=0.0
    ALLOCATE (fglnpmm(ibMax,     jbMax))
    fglnpmm=0.0
    ALLOCATE (fgpphi(ibMax,     jbMax))
    fgpphi=0.0
    ALLOCATE (fgplam(ibMax,     jbMax))
    fgplam=0.0
    ALLOCATE (fgplamm(ibMax,     jbMax))
    fgplamm=0.0
    ALLOCATE (fgplammm(ibMax,     jbMax))
    fgplammm=0.0
    ALLOCATE (fgpphim(ibMax,     jbMax))
    fgpphim=0.0
    ALLOCATE (fgpphimm(ibMax,     jbMax))
    fgpphimm=0.0
    ALLOCATE (fgzslam(ibMax,     jbMax))
    fgzslam=0.0
    ALLOCATE (fgzsphi(ibMax,     jbMax))
    fgzsphi=0.0
    ALLOCATE (fgvdlnpm(ibMax,     jbMax))
    fgvdlnpm=0.0
  END SUBROUTINE InitFields
END MODULE FieldsDynamics
