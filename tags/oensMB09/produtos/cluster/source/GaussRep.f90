  MODULE GaussRep

!  USE PrblSize, ONLY : jmax, jmaxhf

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: CreateGaussRep, glat, colrad, rcs2, wgt
  REAL, ALLOCATABLE, DIMENSION(:) :: glat, colrad, rcs2, wgt
CONTAINS

  SUBROUTINE CreateGaussRep(jmax,jmaxhf)

    INTEGER, INTENT(IN ) :: jmax,jmaxhf

    INTEGER :: j
    REAL    :: rd
   
    ALLOCATE (glat(jmax))
    ALLOCATE (colrad(jmaxhf))
    ALLOCATE (rcs2(jmaxhf))
    ALLOCATE (wgt(jmaxhf))

    CALL glats(jmax,jmaxhf)
    rd=45.0/ATAN(1.0)
    DO j=1,jmaxhf 
       glat(j)=90.0-colrad(j)*rd
       glat(jmax-j+1)=-glat(j)
    END DO
  END SUBROUTINE CreateGaussRep

  SUBROUTINE glats(jmax,jmaxhf) 
    ! 
    !     glats: calculates gaussian latitudes and 
    !            gaussian weights for use in grid-spectral 
    !            and spectral-grid transforms.
    ! 

    INTEGER, INTENT(IN ) :: jmax,jmaxhf

    INTEGER :: j
    REAL :: eps
    REAL :: scale,dradz,rad,drad,p2,p1

    eps = EPSILON(1.0) * 100.
    scale=2.0/(float(jmax)*float(jmax))
    dradz=ATAN(1.0)/90.0
    rad=0.0
    DO j=1,jmaxhf
       drad=dradz
       DO
          CALL poly(jmax,rad,p2)
          DO
             p1=p2
             rad=rad+drad
             CALL poly(jmax,rad,p2)
             IF (SIGN(1.0,p1) /= SIGN(1.0,p2)) EXIT
          END DO
          IF (drad <= eps) EXIT
          rad=rad-drad
          drad=drad*0.25
       END DO
       colrad(j)=rad
       CALL poly(jmax-1,rad,p1)
       wgt(j)=scale*(1.0-COS(rad)*COS(rad))/(p1*p1)
       rcs2(j)=1.0/(SIN(rad)*SIN(rad))
    END DO
  END SUBROUTINE glats


  SUBROUTINE poly(n,rad,p)
    !
    ! poly : calculates the value of the ordinary legendre function
    !        of given order at a specified latitude.  used to
    !        determine gaussian latitudes.
    !
    INTEGER, INTENT(IN ) :: n
    REAL,    INTENT(IN ) :: rad
    REAL,    INTENT(OUT) :: p

    INTEGER :: i
    REAL    :: x, y1, y2, y3, g

    x = COS(rad)
    y1 = 1.0e0
    y2=x
    DO i=2,n
       g=x*y2
       y3=g-y1+g-(g-y1)/ float(i)
       y1=y2
       y2=y3
    END DO
    p=y3
  END SUBROUTINE poly
END MODULE GaussRep
