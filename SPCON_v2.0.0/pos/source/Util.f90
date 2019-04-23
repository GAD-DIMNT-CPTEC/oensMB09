!
!  $Author: pkubota $
!  $Date: 2006/10/30 18:41:45 $
!  $Revision: 1.2 $
!
MODULE Util

  USE Constants, ONLY : r8

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: scase, uvtodz


CONTAINS


  SUBROUTINE scase (kflo, Ldim, nfe, iclcd, gausin)

    USE Constants, ONLY : Ndv, CvLHEv
    USE PrblSize, ONLY : Imax, Jmax

    IMPLICIT NONE

    INTEGER, PARAMETER :: Ndi=150
    INTEGER, PARAMETER :: Ndp=Ndi+Ndv

    INTEGER, INTENT(IN) :: kflo
    INTEGER, INTENT(IN) :: Ldim
    INTEGER, INTENT(IN) :: nfe(Ndp)
    INTEGER, INTENT(IN) :: iclcd(Ndv)
    
    REAL (KIND=r8),    INTENT(INOUT) :: gausin(Imax,Jmax,Ldim)

    INTEGER :: l, j, i

    IF (iclcd(nfe(kflo)) == 1) THEN
       DO l=1,Ldim
          DO j=1,Jmax
             DO i=1,Imax
                gausin(i,j,l)=ABS(gausin(i,j,l))
             END DO
          END DO
       END DO
    ELSE IF (iclcd(nfe(kflo)) == 2) THEN
       DO l=1,Ldim
          DO j=1,Jmax
             DO i=1,Imax
                gausin(i,j,l)=gausin(i,j,l)/CvLHEv
             END DO
          END DO
       END DO
    ELSE
    END IF

  END SUBROUTINE scase
    

  SUBROUTINE uvtodz (u, v, qrot, qdiv)

    USE PrblSize, ONLY : Imax, Jmax, Lmax, Mnwv2, Imx, Jmaxhf
    USE GaussRep, ONLY : ColRad
    USE FFT, ONLY : DirFFT
    USE LegTrans, ONLY : DivRot

    IMPLICIT NONE

    REAL (KIND=r8), INTENT(IN)  :: u(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(IN)  :: v(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(OUT) :: qrot(Mnwv2,Lmax)
    REAL (KIND=r8), INTENT(OUT) :: qdiv(Mnwv2,Lmax)

    INTEGER :: j, i, k

    REAL (KIND=r8) :: CosLat(Jmax)
    REAL (KIND=r8), DIMENSION (Imx,Jmax,Lmax) :: u1, uf, v1, vf

    !cdir nodep
    DO j=1,Jmaxhf
       CosLat(j)=SIN(ColRad(j))
       CosLat(Jmax+1-j)=CosLat(j)
    END DO

    DO k=1,Lmax
       DO j=1,Jmax
          DO i=1,Imax
             u1(i,j,k)=u(i,j,k)*CosLat(j)
             v1(i,j,k)=v(i,j,k)*CosLat(j)
          END DO
       END DO
    END DO

    CALL DirFFT (u1, uf)
    CALL DirFFT (v1, vf)
    CALL DivRot (uf, vf, qdiv, qrot)

  END SUBROUTINE uvtodz


END MODULE Util
