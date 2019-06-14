!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE FieldsDynamics
  IMPLICIT NONE


  ! Spectral fields: 13 2D and 7 1D


  REAL, ALLOCATABLE, DIMENSION(:,:) :: qtmpp
  REAL, ALLOCATABLE, DIMENSION(:,:) :: qtmpt
  REAL, ALLOCATABLE, DIMENSION(:,:) :: qrotp
  REAL, ALLOCATABLE, DIMENSION(:,:) :: qrott
  REAL, ALLOCATABLE, DIMENSION(:,:) :: qdivp
  REAL, ALLOCATABLE, DIMENSION(:,:) :: qdivt
  REAL, ALLOCATABLE, DIMENSION(:,:) :: qqp
  REAL, ALLOCATABLE, DIMENSION(:,:) :: qqt
  REAL, ALLOCATABLE, DIMENSION(:,:) :: qup
  REAL, ALLOCATABLE, DIMENSION(:,:) :: qvp
  REAL, ALLOCATABLE, DIMENSION(:,:) :: qtphi
  REAL, ALLOCATABLE, DIMENSION(:,:) :: qqphi
  REAL, ALLOCATABLE, DIMENSION(:,:) :: qdiaten
  REAL, ALLOCATABLE, DIMENSION(:  ) :: qlnpp
  REAL, ALLOCATABLE, DIMENSION(:  ) :: qlnpt
  REAL, ALLOCATABLE, DIMENSION(:  ) :: qgzslap
  REAL, ALLOCATABLE, DIMENSION(:  ) :: qgzs
  REAL, ALLOCATABLE, DIMENSION(:  ) :: qplam
  REAL, ALLOCATABLE, DIMENSION(:  ) :: qpphi
  REAL, ALLOCATABLE, DIMENSION(:  ) :: qgzsphi


  ! Gaussian fields: 28 3D and 12 2D


  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgyu
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgyv
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgtd
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgqd
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgu
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgv
!IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII  
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgw ! vertical velocity @ layer 
!IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII    
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgdiv
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgrot
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgq
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgtmp
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgum
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgvm
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgumm
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgvmm  
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: omg  
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgtmpm
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgtmpmm
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgqm
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgqmm
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgdivm
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgdivmm
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgyum
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgyvm
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgtdm
!IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgqdm ! tendencia de q no departure
!IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgtphi
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgqphi
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fguphi
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgvphi
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgtphim
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgtphimm
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgtlam
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgqlam
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgulam
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgvlam
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgtlamm
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: fgtlammm
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fgvdlnp
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fglnps
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fgps
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fglnpm
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fglnpmm
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fgpphi
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fgplam
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fgplamm
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fgplammm
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fgpphim
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fgpphimm
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fgzslam
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fgzsphi
  REAL, ALLOCATABLE, DIMENSION(:  ,:) :: fgvdlnpm
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
