!
!  $Author: tomita $
!  $Date: 2007/08/01 20:09:58 $
!  $Revision: 1.1.1.1 $
!
MODULE InputArrays

  USE Constants, ONLY: i4, r4, r8
  USE Sizes,     ONLY: ibMax, jbMax, lmax, kMax, lMaxloc, kmaxloc, &
                       imax, jMax, jMaxHalf, jMinPerM, iMaxPerJ, &
                       mMax, nExtMax, mnMax, mnExtMax, mnExtMap, &
                       mymnMax, mymnExtMax

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: GetArrays

  REAL (KIND=r8), DIMENSION (:), ALLOCATABLE, PUBLIC :: &
                  qlnpp, qplam, qpphi, qgzs, wd

  REAL (KIND=r8), DIMENSION (:,:), ALLOCATABLE, PUBLIC :: &
                  qup, qvp, qrotp, qdivp, qtmpp, qqp, &
                  qug, qvg, qrotg, qdivg, wk, wl, wlphi, qa

  REAL (KIND=r8), DIMENSION (:,:), ALLOCATABLE, PUBLIC :: &
                  psmb, pscb, lnpscb, top, lsmk, fgplam, fgpphi

  REAL (KIND=r8), DIMENSION (:,:,:), ALLOCATABLE, PUBLIC :: &
                  og, ga_l, fgu, fgv, fgdiv, fgq, fgtmp, fgomega, fgdivq, ga


CONTAINS


SUBROUTINE GetArrays

  IMPLICIT NONE

  ALLOCATE (qlnpp(2*mymnmax))
  ALLOCATE (qpphi(2*mymnextmax))
  ALLOCATE (qplam(2*mymnmax))
  ALLOCATE (qgzs (2*mymnmax))
  ALLOCATE (wd(2*mymnmax))
  ALLOCATE (qup(2*mymnextmax,kmaxloc))
  ALLOCATE (qvp(2*mymnextmax,kmaxloc))
  ALLOCATE (qdivp(2*mymnmax,kmaxloc))
  ALLOCATE (qrotp(2*mymnmax,kmaxloc))
  ALLOCATE (qtmpp(2*mymnmax,kmaxloc))
  ALLOCATE (qqp(2*mymnmax,kmaxloc))
  ALLOCATE (qug(2*mymnextmax,lmaxloc))
  ALLOCATE (qvg(2*mymnextmax,lmaxloc))
  ALLOCATE (qdivg(2*mymnmax,lmaxloc))
  ALLOCATE (qrotg(2*mymnmax,lmaxloc))
  ALLOCATE (wk(2*mymnmax,lmaxloc))
  ALLOCATE (wl(2*mymnmax,lmaxloc))
  ALLOCATE (wlphi(2*mymnextmax,lmaxloc))
  ALLOCATE (psmb(ibmax,jbmax))
  ALLOCATE (pscb(ibmax,jbmax))
  ALLOCATE (lnpscb(ibmax,jbmax))
  ALLOCATE (top (ibmax,jbmax))
  ALLOCATE (lsmk(ibmax,jbmax))
  ALLOCATE (fgplam(ibmax,jbmax))
  ALLOCATE (fgpphi(ibmax,jbmax))
  ALLOCATE (og(ibmax,lmax,jbmax))
  ALLOCATE (ga_l(ibmax,kmax,jbmax))
  ALLOCATE (fgu (ibmax,kmax,jbmax))
  ALLOCATE (fgv (ibmax,kmax,jbmax))
  ALLOCATE (fgq (ibmax,kmax,jbmax))
  ALLOCATE (fgdiv (ibmax,kmax,jbmax))
  ALLOCATE (fgtmp (ibmax,kmax,jbmax))
  ALLOCATE (fgomega(ibmax,kmax,jbmax))
  ALLOCATE (fgdivq(ibmax,lmax,jbmax))
  ALLOCATE (qa (imax,jmax))
  ALLOCATE (ga (imax,jmax,kmax))

END SUBROUTINE GetArrays

END MODULE InputArrays
