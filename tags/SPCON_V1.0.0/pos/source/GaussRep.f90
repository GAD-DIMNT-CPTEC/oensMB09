!
!  $Author: pkubota $
!  $Date: 2006/10/30 18:37:57 $
!  $Revision: 1.2 $
!
MODULE GaussRep

  USE Constants, ONLY : r8
  USE PrblSize, ONLY : Jmax, Jmaxhf

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: CreateGaussRep, gLat, ColRad, rCs2, Wgt

  REAL (KIND=r8), ALLOCATABLE, DIMENSION(:) :: gLat, ColRad, rCs2, Wgt


CONTAINS


  SUBROUTINE CreateGaussRep()

    IMPLICIT NONE

    INTEGER :: j
    REAL (KIND=r8) :: rd

    ALLOCATE (gLat(Jmax))
    ALLOCATE (ColRad(Jmaxhf))
    ALLOCATE (rCs2(Jmaxhf))
    ALLOCATE (Wgt(Jmaxhf))

    CALL gLats ()
    rd=45.0_r8/ATAN(1.0_r8)
    DO j=1,Jmaxhf
       gLat(j)=90.0_r8-ColRad(j)*rd
       gLat(Jmax-j+1)=-gLat(j)
    END DO

  END SUBROUTINE CreateGaussRep


  SUBROUTINE gLats()

    IMPLICIT NONE

    ! Calculates Gaussian Latitudes and Gaussian Weights 
    ! for Use in Grid-Spectral and Spectral-Grid Transforms

    INTEGER :: j
    REAL (KIND=r8) :: eps
    REAL (KIND=r8) :: scale, dgColIn, gCol, dgCol, p2, p1

    eps=EPSILON(1.0_r8)*100.0_r8
    scale=2.0_r8/(REAL(Jmax,r8)*REAL(Jmax,r8))
    dgColIn=ATAN(1.0_r8)/REAL(Jmax,r8)
    gCol=0.0_r8
    DO j=1,Jmaxhf
       dgCol=dgColIn
       DO
          CALL LegendrePolynomial (Jmax, gCol, p2)
          DO
             p1=p2
             gCol=gCol+dgCol
             CALL LegendrePolynomial (Jmax, gCol, p2)
             IF (SIGN(1.0_r8,p1) /= SIGN(1.0_r8,p2)) EXIT
          END DO
          IF (dgCol <= eps) EXIT
          gCol=gCol-dgCol
          dgCol=dgCol*0.25_r8
       END DO
       ColRad(j)=gCol
       CALL LegendrePolynomial (Jmax-1, gCol, p1)
       Wgt(j)=scale*(1.0_r8-COS(gCol)*COS(gCol))/(p1*p1)
       rCs2(j)=1.0_r8/(SIN(gCol)*SIN(gCol))
    END DO

  END SUBROUTINE gLats


  SUBROUTINE LegendrePolynomial (N, Colatitude, Pln)

    IMPLICIT NONE

    ! Calculates the Value of the Ordinary Legendre 
    ! Function of Given Order at a Specified Colatitude.  
    ! Used to Determine Gaussian Latitudes.

    INTEGER, INTENT(IN ) :: N
    REAL (KIND=r8), INTENT(IN ) :: Colatitude
    REAL (KIND=r8), INTENT(OUT) :: Pln

    INTEGER :: i
    REAL (KIND=r8) :: x, y1, y2, y3, g

    x=COS(Colatitude)
    y1=1.0_r8
    y2=x
    DO i=2,N
       g=x*y2
       y3=g-y1+g-(g-y1)/REAL(i,r8)
       y1=y2
       y2=y3
    END DO
    Pln=y3

  END SUBROUTINE LegendrePolynomial


END MODULE GaussRep
