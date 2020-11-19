! Este modulo contem subrotinas a funcoes relacioadas com o calculo
! de latitudes Gaussianas a partir de um numero (par) de latitudes sobre os hemisferios 
! (subrotina gauss2lats, original por Tom Holt (NCAR)
! https://www.mathworks.com/matlabcentral/fileexchange/2586-gauss2lats?requestedDomain=www.mathworks.com)
!
! A subrotina latitudes foi adaptada do programa do CRPS do ensemble global;
! A subrotina llij_latlon foi adaptada do map_utils do SCAMTEC.
!
! Observacoes:
! O criterio de parada da subrotina gauss2lats (parametro xlim) deve ser utilizado com cuidado.
!
! carlos.bastarz@inpe.br (Outubro de 2015; Junho de 2017; Novembro de 2020)

module constants

  implicit none

  real, parameter :: pi=4.0*atan(1.0)

end module constants

module m_gauss2lats

  use constants

  implicit none

  contains

  subroutine latitudes(jmax, lats, latweights)
  
    use constants, only : pi

    implicit none
  
    integer :: j
    integer, intent(in) :: jmax ! >= 6 - cfb: número de pontos de latitudes + 2

    real :: lat, latr
    real, dimension(jmax), intent(out) :: latweights, lats

    call gauss2lats(jmax, lats)

    open(80, file='wght.txt', form='formatted', action='write', access='sequential', status='replace')

    do j=1, jmax
  
      lat=lats(j)
  
      latr=(pi / 180.0) * lat
      latweights(j)=cos(latr)
  
      if (latweights(j) .lt. 0.0) latweights(j) = 0.0

!      write(*,'(I3,F8.3,E13.5)') j, lat, latweights(j) 
      write(80,'(I3,F8.3,E13.5)') j, lat, latweights(j) 
  
    end do
  
    close(80)

  end subroutine latitudes
  
  subroutine gauss2lats(nlat, xlats)
 
    use constants, only : pi

    implicit none
  
    integer, intent(in) :: nlat
    integer :: i, nzero
  
    real :: acon, fi, fi1, a, b, c, d, ylat, delta
    real, parameter :: xlim=1.0e-6 ! Convervence criterion for iteration of cos latitude
    real, dimension(:), allocatable :: cosc, gwt, sinc, colat, wos2
    real, dimension(:), allocatable :: dlat, g, gm, gp, gt
    real, dimension(nlat/2) :: xlat, xlatm
    real, dimension(nlat), intent(out) :: xlats
 
    acon=180.0/pi
  
    allocate(cosc(nlat+1))
    allocate(sinc(nlat+1))
    allocate(colat(nlat+1))
  
    allocate(gwt(nlat))
    allocate(wos2(nlat))
 
    ! Initialize arrays
    cosc=0.0
    gwt=0.0
    sinc=0.0
    colat=0.0
    wos2=0.0
  
    ! The number of zeros between pole and Equator
    nzero=nlat/2
  
    allocate(dlat(nzero))
  
    ! Set the first guess for cos(colat)
    do i=1, nzero
      cosc(i)=sin((i-0.5)*pi/nlat+pi*0.5)
    end do
  
    ! Constants for determining the derivative of the polynomial
    fi=nlat
    fi1=fi+1.0
    a=fi*fi1/sqrt(4.0*fi1*fi1-1.0)
    b=fi1*fi/sqrt(4.0*fi*fi-1.0)
  
    allocate(g(nlat))
    allocate(gm(nlat-1))
    allocate(gt(nlat+1))
    allocate(gp(nlat+1))
  
    ! Loop over latitudes, iterating the search for each root
    do i=1, nzero
      ! Determine the value of the ordinary Legendre polynomial for the current guess root
      g=fgord(nlat,cosc(i))

      ! Determine the derivative of the polynomial at this point
      gm=fgord(nlat-1,cosc(i))

      gp=fgord(nlat+1,cosc(i)) 

      gt=(cosc(i)*cosc(i)-1.0)/(a*gp-b*gm)

      ! Update the estimate of the root
      delta=g(i)*gt(i)
      cosc(i)=cosc(i)-delta

      ! If convergence criterion has not been met, keep trying
      do while(abs(delta).gt.xlim)
        g=fgord(nlat,cosc(i))
        gm=fgord(nlat-1,cosc(i))
        gp=fgord(nlat+1,cosc(i))
        gt=(cosc(i)*cosc(i)-1)/(a*gp-b*gm)
        delta=g(i)*gt(i)
        cosc(i)=cosc(i)-delta
      end do

      ! Determine the Gaussian weights
      c=2.0*(1.0-cosc(i)*cosc(i))
      d=fgord(nlat-1,cosc(i))
      d=d*d*fi*fi
      gwt(i)=c*(fi-0.5)/d
    end do

    ! Determine the colatitudes and sin(colat) and weights over sin
    do i=1,nzero
      colat(i)=acos(cosc(i))
      sinc(i)=sin(colat(i))
      wos2(i)=gwt(i)/(sinc(i)*sinc(i))
    end do
    
    ! If nlat is odd, set values at the Equator
    if (mod(nlat,2).ne.0) then
      i=nzero+1
      cosc(i)=0.0
      c=2.0
      d=fgord(nlat-1,cosc(i))
      d=d*d*fi*fi
      gwt(i)=c*(fi-0.5)/d
      colat(i)=pi*0.5
      sinc(i)=1.0
      wos2(i)=gwt(i)
    end if
   
    ! Determine the southern hemisphere values by symmetry
  !  do i=nlat-nzero+1,nlat
  !    cosc(i)=-cosc(nlat+1-i)
  !    gwt(i)=gwt(nlat+1-i)
  !    colat(i)=pi-colat(nlat+1-i)
  !    sinc(i)=sinc(nlat+1-i)
  !    wos2(i)=wos2(nlat+1-i)
  !  end do
  
    ylat=90.0
  
    ! Calculate latitudes and latitude spacing
    do i=1,nzero
      xlat(i)=acos(sinc(i))*acon
      dlat(i)=xlat(i)-ylat
      ylat=xlat(i)
    end do

    ! Reverse latitudes for south hemisphere and assembly full array 
    xlatm=-1.0*xlat
    xlats=0.0
  
    do i=1,nzero
      xlats(i)=xlatm(i)
    end do
  
    xlat=xlat(nzero:1:-1)
  
    do i=nzero+1,nlat
      xlats(i)=xlat(i-nzero)
    end do

    deallocate(cosc)
    deallocate(gwt)
    deallocate(sinc)
    deallocate(colat)
    deallocate(wos2)
  
    deallocate(g)
    deallocate(gm)
    deallocate(gt)
    deallocate(gp)
    deallocate(dlat)
  
    contains

    real function fgord(n,x) result(gord)
   
      integer :: i, k, n
      real :: x, ang, c1, s1, c4, a, b, fn, colat, fk
 
      ! Determine the colatitude
      colat=acos(x)
      
      c1=sqrt(2.0)
      
      do i=1,n
        c1=c1*sqrt(1.0-1.0/(4*i*i))
      end do
      
      fn=n
      ang=fn*colat
      s1=0.0
      c4=1.0
      a=-1.0
      b=0.0
      
      do k=0,n,2
        if (k.eq.n) then
          c4=0.5*c4
        end if
        s1=s1+c4*cos(ang)
        a=a+2.0
        b=b+1.0
        fk=k
        ang=colat*(fn-fk-2.0)
        c4=(a*(fn-b+1.0)/(b*(fn+fn-a)))*c4
      end do
    
      gord=s1*c1
    
      return
    
    end function fgord

  end subroutine gauss2lats
  
  subroutine subdomain(imax, jmax, inarray, sdibeg, sdiend, sdjbeg, sdjend, outarray)
  
    implicit none
  
    integer :: i, j, nx, ny, nxy
    integer, intent(in) :: imax
    integer, intent(in) :: jmax
    integer, intent(in) :: sdibeg ! beginning i index (1,2,...,etc) of the required subdomain
    integer, intent(in) :: sdiend ! ending i index (1,2,...,etc) of the required subdomain
    integer, intent(in) :: sdjbeg ! beginning j index (1,2,...,etc) of the required subdomain
    integer, intent(in) :: sdjend ! ending j index (1,2,...,etc) of the required subdomain
 
    real, intent(in), dimension(imax,jmax) :: inarray
    real, intent(out), dimension(:) :: outarray
  
    logical, dimension(imax,jmax) :: mask

    ! set up a mask 
    ! outside the limits defined the mask is .false., inside is .true.
    mask=.FALSE.

!    open(100, file='mask.txt', form='formatted', action='write', access='sequential', status='replace')

    do ny=sdjbeg, sdjend 
      do nx=sdibeg, sdiend 
        mask(nx,ny)=.TRUE.
      end do
    end do 

!    do j=1, jmax
!      do i=1, imax
!        write(100, '(L3)') mask(i,j)
!      end do
!    end do

!    close(100)

!    open(110, file='outarray.txt', form='formatted', action='write', access='sequential', status='replace')
!    open(120, file='inarray.txt', form='formatted', action='write', access='sequential', status='replace')

    ! le os valores dos pesos de inarray apenas nos pontos em que mask(i,j)=T
    nxy=0

    do j=1, jmax
      do i=1, imax
        if (mask(i,j)) then
          nxy = nxy + 1
          outarray(nxy) = inarray(i,j)
!          write(110,'(F5.3)') outarray(nx)
        end if
!        write(120,'(F5.3)') inarray(i,j)
      end do
    end do

!    close(120)
!    close(110)

  end subroutine subdomain

  subroutine llij_latlon(imax, jmax, lat, lon, i, j)
 
    ! compute the i/j location of a lat/lon on a latlon grid.
    ! map_utils.f90 (SCAMTEC)
 
    implicit none
  
    integer :: nx
    integer :: imax, jmax
  
    real :: deltalat, deltalon, lon360
    real :: dlon, dlat
  
    real, parameter :: lat1=-90.0
    real, parameter :: lon1=0.0

    real, intent(in) :: lat, lon
    real, intent(out) :: i, j  

    logical, parameter :: cyclic=.FALSE. ! se -180<=lon<=180
  
    nx = imax
  
    dlat = 180.0 / jmax
    dlon = 360.0 / imax
  
    ! compute deltalat and deltalon as the difference between the input 
    ! lat/lon and the origin lat/lon
    deltalat = lat - lat1
  
    ! to account for issues around the dateline, convert the incoming
    ! longitudes to be 0->360.
    if (lon .lt. 0.0) then 
      lon360 = lon + 360.0 
    else 
      lon360 = lon
    endif    
  
    deltalon = lon360 - lon1      
  
    if (deltalon .lt. 0.0) deltalon = deltalon + 360.0
  
    ! compute i/j
    i = (deltalon / dlon) + 1.0
    j = (deltalat / dlat) + 1.0
    
    ! corrections for longitudinal wrapping
    ! note that 0 is not a valid index.  for cyclic wrapping
    ! in fortran indexing, 0.5 <= i < nx+0.5
    if (cyclic) then
      if (i .ge. float(nx) + 0.5) i = i - float(nx)
    endif
  
!    print *, 'lon, i=', lon, i
!    print *, 'lat, j=', lat, j
  
  end subroutine llij_latlon

end module m_gauss2lats
