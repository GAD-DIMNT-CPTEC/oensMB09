!
!  $Author: pkubota $
!  $Date: 2006/10/30 18:41:33 $
!  $Revision: 1.2 $
!
MODULE TransSpectralGrid

  USE Constants, ONLY : r8
  USE PrblSize, ONLY : Imax, Imx, Jmax, Jmaxhf, Mnwv2
  USE FFT, ONLY : DirFFT, InvFFT
  USE LegTrans, ONLY : Four2Spec, Spec2Four

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: dectrg, rectrg, rectrd


CONTAINS


  SUBROUTINE dectrg (Ldim, f, qf)

    IMPLICIT NONE

    INTEGER, INTENT(IN) ::  Ldim

    REAL (KIND=r8), INTENT(IN) :: f(Imax,Jmax,Ldim)
    REAL (KIND=r8), INTENT(OUT) :: qf(Mnwv2,Ldim)

    REAL (KIND=r8), DIMENSION (Imx,Jmax,Ldim) :: Four

    CALL DirFFT (f, Four)
    CALL Four2Spec (Four, qf)

  END SUBROUTINE dectrg


  SUBROUTINE rectrg (Mnwvd, Ldim, qf, f)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: Mnwvd
    INTEGER, INTENT(IN) :: Ldim

    REAL (KIND=r8), INTENT(IN) :: qf(Mnwvd,Ldim)
    REAL (KIND=r8), INTENT(OUT) :: f(Imax,Jmax,Ldim)

    REAL (KIND=r8), DIMENSION (Imx,Jmax,Ldim) :: workf

    CALL Spec2Four (qf, workf)
    CALL InvFFT (workf, f)

  END SUBROUTINE rectrg


  SUBROUTINE rectrd (Ldim, qf, f)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: Ldim

    REAL (KIND=r8), INTENT(IN) :: qf(Mnwv2,Ldim)
    REAL (KIND=r8), INTENT(OUT) :: f(Imax,Jmax,Ldim)

    INTEGER :: k, j, i
    LOGICAL :: Der
    REAL (KIND=r8), DIMENSION (Imx,Jmax,Ldim) :: Four

    Der=.TRUE.
    CALL Spec2Four (qf, Four, Der)
    CALL InvFFT (Four, f)
    DO k=1,Ldim
       DO j=Jmaxhf+1,Jmax
          DO i=1,Imax
             f(i,j,k)=-f(i,j,k)
          END DO
       END DO
    END DO

  END SUBROUTINE rectrd


END MODULE TransSpectralGrid
