!
!  $Author: pkubota $
!  $Date: 2007/06/15 21:28:17 $
!  $Revision: 1.6 $
!
MODULE FieldsDynamics

  USE Constants, Only: r8
  IMPLICIT NONE


  ! Spectral fields: 13 2D and 7 1D


  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:) :: qtmpp
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:) :: qtmpt
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:) :: qrotp
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:) :: qrott
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:) :: qdivp
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:) :: qdivt
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:) :: qqp
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:) :: qqt
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:) :: qup
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:) :: qvp
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:) :: qtphi
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:) :: qqphi
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:) :: qdiaten
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ) :: qlnpp
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ) :: qlnpl
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ) :: qlnpt
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ) :: qgzslap
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ) :: qgzs
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ) :: qplam
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ) :: qpphi
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ) :: qgzsphi


  ! Gaussian fields: 28 + 2 * nscalars 3D and 12 2D


  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:,:,:) :: fgpass_scalars

  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgyu
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgyv
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtd
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgqd
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgu
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgv
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgw 
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgdiv
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgrot
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgq
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtmp
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgum
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgvm
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgumm
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgvmm  
!snf
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgumm2
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgvmm2
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: omg2
  
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtmpm
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtmpmm1
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtmpmm2

  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgqm
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgqmm
!snf
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgqmm2
  
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgdivm
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgyum
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgyvm
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtdm
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgqdm 
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtphi
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgqphi
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fguphi
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgvphi
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtphim
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtlam
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgqlam
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgulam
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgvlam
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: fgtlamm
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgumean
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgvmean
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgvdlnp
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fglnps
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgps
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fglnpm
!snf
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgps2
  
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgpphi
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgplam
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgplamm
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgpphim
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgzs
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgzslam
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgzsphi
  REAL(KIND=r8), ALLOCATABLE, TARGET, DIMENSION(:  ,:) :: fgvdlnpm
CONTAINS
  SUBROUTINE InitFields(ibMax, kMax, jbMax, jbminus, jbplus, mnMax, mnExtMax, &
                        nscalars)
    INTEGER, INTENT(IN) :: ibMax
    INTEGER, INTENT(IN) :: kMax
    INTEGER, INTENT(IN) :: jbMax
    INTEGER, INTENT(IN) :: jbminus
    INTEGER, INTENT(IN) :: jbplus
    INTEGER, INTENT(IN) :: mnMax
    INTEGER, INTENT(IN) :: mnExtMax
    INTEGER, INTENT(IN) :: nscalars

    ALLOCATE (qtmpp(2*mnMax,kMax))
    qtmpp=0.0_r8
    ALLOCATE (qtmpt(2*mnMax,kMax))
    qtmpt=0.0_r8
    ALLOCATE (qrotp(2*mnMax,kMax))
    qrotp=0.0_r8
    ALLOCATE (qrott(2*mnMax,kMax))
    qrott=0.0_r8
    ALLOCATE (qdivp(2*mnMax,kMax))
    qdivp=0.0_r8
    ALLOCATE (qdivt(2*mnMax,kMax))
    qdivt=0.0_r8
    ALLOCATE (qqp(2*mnMax,kMax))
    qqp=0.0_r8
    ALLOCATE (qqt(2*mnMax,kMax))
    qqt=0.0_r8
    ALLOCATE (qdiaten(2*mnMax,kMax))
    qdiaten=0.0_r8
    ALLOCATE (qup(2*mnExtMax,kMax))
    qup=0.0_r8
    ALLOCATE (qvp(2*mnExtMax,kMax))
    qvp=0.0_r8
    ALLOCATE (qtphi(2*mnExtMax,kMax))
    qtphi=0.0_r8
    ALLOCATE (qqphi(2*mnExtMax,kMax))
    qqphi=0.0_r8
    ALLOCATE (qlnpp(2*mnMax))
    qlnpp=0.0_r8
    ALLOCATE (qlnpl(2*mnMax))
    qlnpl=0.0_r8
    ALLOCATE (qlnpt(2*mnMax))
    qlnpt=0.0_r8
    ALLOCATE (qgzslap(2*mnMax))
    qgzslap=0.0_r8
    ALLOCATE (qgzs(2*mnMax))
    qgzs=0.0_r8
    ALLOCATE (qplam(2*mnMax))
    qplam=0.0_r8
    ALLOCATE (qpphi(2*mnExtMax))
    qpphi=0.0_r8
    ALLOCATE (qgzsphi(2*mnExtMax))
    qgzsphi=0.0_r8

    IF (nscalars .gt. 0) THEN
       ALLOCATE (fgpass_scalars(ibMax,kMax,jbminus:jbplus,nscalars,3))
       fgpass_scalars=0.0_r8
    ENDIF

    ALLOCATE (fgyu(ibMax,kMax,jbMax))
    fgyu=0.0_r8
    ALLOCATE (fgyv(ibMax,kMax,jbMax))
    fgyv=0.0_r8
    ALLOCATE (fgtd(ibMax,kMax,jbMax))
    fgtd=0.0_r8
    ALLOCATE (fgqd(ibMax,kMax,jbMax))
    fgqd=0.0_r8
    ALLOCATE (fgu(ibMax,kMax,jbminus:jbplus))
    fgu=0.0_r8
    ALLOCATE (fgv(ibMax,kMax,jbminus:jbplus))
    fgv=0.0_r8
    ALLOCATE (fgw(ibMax,kMax,jbminus:jbplus))
    fgw=0.0_r8
    ALLOCATE (fgdiv(ibMax,kMax,jbMax))
    fgdiv=0.0_r8
    ALLOCATE (fgrot(ibMax,kMax,jbMax))
    fgrot=0.0_r8
    ALLOCATE (fgq(ibMax,kMax,jbMax))
    fgq=0.0_r8
    ALLOCATE (fgtmp(ibMax,kMax,jbMax))
    fgtmp=0.0_r8
    ALLOCATE (fgum(ibMax,kMax,jbMax))
    fgum=0.0_r8
    ALLOCATE (fgvm(ibMax,kMax,jbMax))
    fgvm=0.0_r8
    ALLOCATE (fgumm(ibMax,kMax,jbMax))
    fgumm=0.0_r8
    ALLOCATE (fgvmm(ibMax,kMax,jbMax))
    fgvmm=0.0_r8    
!snf
    ALLOCATE (fgumm2(ibMax,kMax,jbMax))
    fgumm2=0.0_r8
    ALLOCATE (fgvmm2(ibMax,kMax,jbMax))
    fgvmm2=0.0_r8
    ALLOCATE (omg2(ibMax,kMax,jbMax))
    omg2=0.0_r8
!--
    ALLOCATE (fgtmpm(ibMax,kMax,jbMax))
    fgtmpm=0.0_r8
!snf
    ALLOCATE (fgtmpmm1(ibMax,kMax,jbMax))
    fgtmpmm1=0.0_r8
    ALLOCATE (fgtmpmm2(ibMax,kMax,jbMax))
    fgtmpmm2=0.0_r8
!
    ALLOCATE (fgqm(ibMax,kMax,jbMax))
    fgqm=0.0_r8 
    ALLOCATE (fgqmm(ibMax,kMax,jbMax))
    fgqmm=0.0_r8
!snf---
   ALLOCATE (fgqmm2(ibMax,kMax,jbMax))
    fgqmm2=0.0_r8
!---
    ALLOCATE (fgdivm(ibMax,kMax,jbMax))
    fgdivm=0.0_r8
    ALLOCATE (fgyum(ibMax,kMax,jbminus:jbplus))
    fgyum=0.0_r8
    ALLOCATE (fgyvm(ibMax,kMax,jbminus:jbplus))
    fgyvm=0.0_r8
    ALLOCATE (fgtdm(ibMax,kMax,jbminus:jbplus))
    fgtdm=0.0_r8
    ALLOCATE (fgqdm(ibMax,kMax,jbminus:jbplus))
    fgqdm=0.0_r8
    ALLOCATE (fgtphi(ibMax,kMax,jbMax))
    fgtphi=0.0_r8
    ALLOCATE (fgqphi(ibMax,kMax,jbMax))
    fgqphi=0.0_r8
    ALLOCATE (fguphi(ibMax,kMax,jbMax))
    fguphi=0.0_r8
    ALLOCATE (fgvphi(ibMax,kMax,jbMax))
    fgvphi=0.0_r8
    ALLOCATE (fgtphim(ibMax,kMax,jbMax))
    fgtphim=0.0_r8
    ALLOCATE (fgtlam(ibMax,kMax,jbMax))
    fgtlam=0.0_r8
    ALLOCATE (fgqlam(ibMax,kMax,jbMax))
    fgqlam=0.0_r8
    ALLOCATE (fgulam(ibMax,kMax,jbMax))
    fgulam=0.0_r8
    ALLOCATE (fgvlam(ibMax,kMax,jbMax))
    fgvlam=0.0_r8
    ALLOCATE (fgtlamm(ibMax,kMax,jbMax))
    fgtlamm=0.0_r8
    ALLOCATE (fgumean(ibMax,     jbminus:jbplus))
    fgumean=0.0_r8
    ALLOCATE (fgvmean(ibMax,     jbminus:jbplus))
    fgvmean=0.0_r8
    ALLOCATE (fgvdlnp(ibMax,     jbMax))
    fgvdlnp=0.0_r8
    ALLOCATE (fglnps(ibMax,     jbMax))
    fglnps=0.0_r8
    ALLOCATE (fgps(ibMax,     jbMax))
    fgps=0.0_r8
    ALLOCATE (fglnpm(ibMax,     jbMax))
    fglnpm=0.0_r8
!snf
    ALLOCATE (fgps2(ibMax,     jbMax))
    fgps2=0.0_r8
!
    ALLOCATE (fgpphi(ibMax,     jbMax))
    fgpphi=0.0_r8
    ALLOCATE (fgplam(ibMax,     jbMax))
    fgplam=0.0_r8
    ALLOCATE (fgplamm(ibMax,     jbMax))
    fgplamm=0.0_r8
    ALLOCATE (fgpphim(ibMax,     jbMax))
    fgpphim=0.0_r8
    ALLOCATE (fgzs(ibMax,     jbMax))
    fgzs=0.0_r8
    ALLOCATE (fgzslam(ibMax,     jbMax))
    fgzslam=0.0_r8
    ALLOCATE (fgzsphi(ibMax,     jbMax))
    fgzsphi=0.0_r8
    ALLOCATE (fgvdlnpm(ibMax,     jbminus:jbplus))
    fgvdlnpm=0.0_r8
  END SUBROUTINE InitFields
END MODULE FieldsDynamics
