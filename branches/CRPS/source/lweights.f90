!************************************************************
!
! Subroutine to evaluate the latitudinal weights
!
!************************************************************

!  SUBROUTINE lweights(jmax,glat,latweight)
!  IMPLICIT NONE
!  INTEGER,               INTENT(IN)  :: jmax
!  REAL, DIMENSION(jmax), INTENT(IN)  :: glat
!  REAL, DIMENSION(jmax), INTENT(OUT) :: latweight
!  INTEGER  :: j
!  REAL     :: pi,latr
!  pi=4.0*ATAN(1.0)
!  DO j=1,jmax
!     latr=(pi*glat(j))/180.0
!     latweight(j)=cos(latr)
!     IF (latweight(j) .LT. 0.0) latweight(j)=0.0
!     WRITE(*,*) 'lat(',j,'):',glat(j),latweight(j)
!  END DO
!  WRITE(*,*) 'Latitudinal weights:'
!  WRITE(*,'(8E13.5)') (latweight(j),j=1,jmax)
!  RETURN
!  END SUBROUTINE lweights

  SUBROUTINE lweights(jmax,latweight)
  IMPLICIT NONE
  INTEGER,               INTENT(IN)  :: jmax
  REAL, DIMENSION(jmax), INTENT(OUT) :: latweight
  INTEGER  :: j
  REAL     :: pi,dlon,lat,latr
  pi=4.0*ATAN(1.0)
  lat=90.0
  dlon=2.5
  DO j=1,jmax
     latr=(pi*lat)/180.0
     latweight(j)=cos(latr)
     IF (latweight(j) .LT. 0.0) latweight(j)=0.0
     WRITE(*,*) 'lat(',j,'):',lat,latweight(j)
     lat=lat-dlon
  END DO
  WRITE(*,*)' '
  WRITE(*,*) 'Latitudinal weights:'
  WRITE(*,'(8E13.5)') (latweight(j),j=1,jmax)
  RETURN
  END SUBROUTINE lweights

