! Este programa calcula as latitudes Gaussianas a partir do 
! de um numero (par) de latitudes sobre os hemisferios.
! Programa original por Tom Holt (NCAR)
! https://www.mathworks.com/matlabcentral/fileexchange/2586-gauss2lats?requestedDomain=www.mathworks.com
!
! Observacoes:
! O criterio de parada (parametro xlim) deve ser utilizado com cuidado.
!
! Carlos Frederico Bastarz (Outubro de 2015; Junho de 2017)

program latitudes

  implicit none

  integer :: j
  real :: lat, latr, pi

  integer, parameter :: jmax=452 ! >= 6
  real, dimension(jmax) :: lats, latweight

  pi=4.0d0*datan(1.0d0)

  call gauss2lats(jmax,lats)

  do j=1, jmax

    lat=lats(j)

    latr=(pi*lat)/180.0
    latweight(j)=cos(latr)

    if (latweight(j) .lt. 0.0) latweight(j) = 0.0

  end do

  write(*,"(8F11.6)") (lats)

end program latitudes

subroutine gauss2lats(nlat,xlats)

  implicit none

  integer, intent(in) :: nlat

  integer :: i, nzero

  real :: acon, fi, fi1, a, b, c, d, ylat, gord, delta, pi

  real, parameter :: xlim=1.0e-6 ! Convervence criterion for iteration of cos latitude

  real, dimension(:), allocatable :: cosc, gwt, sinc, colat, wos2

  real, dimension(:), allocatable :: dlat, g, gm, gp, gt

  real, dimension(nlat/2) :: xlat, xlatm

  real, dimension(nlat) :: xlats

  pi=4.0d0*datan(1.0d0)

  acon=180.0d0/pi

  allocate(cosc(nlat+1))
  allocate(sinc(nlat+1))
  allocate(colat(nlat+1))

  allocate(gwt(nlat))
  allocate(wos2(nlat))

  ! Initialize arrays
  cosc=0.0d0
  gwt=0.0d0
  sinc=0.0d0
  colat=0.0d0
  wos2=0.0d0

  ! The number of zeros between pole and Equator
  nzero=nlat/2

  allocate(dlat(nzero))

  ! Set the first guess for cos(colat)
  do i=1, nzero
    cosc(i)=sin((i-0.5d0)*pi/nlat+pi*0.5d0)
  end do

  ! Constants for determining the derivative of the polynomial
  fi=nlat
  fi1=fi+1.0d0
  a=fi*fi1/sqrt(4.0d0*fi1*fi1-1.0d0)
  b=fi1*fi/sqrt(4.0d0*fi*fi-1.0d0)

  allocate(g(nlat))
  allocate(gm(nlat-1))
  allocate(gt(nlat+1))
  allocate(gp(nlat+1))

  ! Loop over latitudes, iterating the search for each root
  do i=1, nzero
    ! Determine the value of the ordinary Legendre polynomial for the current guess root
    g=gord(nlat,cosc(i))
    ! Determine the derivative of the polynomial at this point
    gm=gord(nlat-1,cosc(i))
    gp=gord(nlat+1,cosc(i)) 
    gt=(cosc(i)*cosc(i)-1.0d0)/(a*gp-b*gm)
    ! Update the estimate of the root
    delta=g(i)*gt(i)
    cosc(i)=cosc(i)-delta
    ! If convergence criterion has not been met, keep trying
    do while(abs(delta).gt.xlim)
      g=gord(nlat,cosc(i))
      gm=gord(nlat-1,cosc(i))
      gp=gord(nlat+1,cosc(i))
      gt=(cosc(i)*cosc(i)-1)/(a*gp-b*gm)
      delta=g(i)*gt(i)
      cosc(i)=cosc(i)-delta
    end do
    ! Determine the Gaussian weights
    c=2.0d0*(1.0d0-cosc(i)*cosc(i))
    d=gord(nlat-1,cosc(i))
    d=d*d*fi*fi
    gwt(i)=c*(fi-0.5d0)/d
  end do

  ! Determine the colatitudes and sin(colat) and weights over sin
  do i=1,nzero
    colat(i)=acos(cosc(i))
    sinc(i)=sin(colat(i))
    wos2(i)=gwt(i)/(sinc(i)*sinc(i))
  end do
  
  ! If nlat is odd, set values at the equator
  if (mod(nlat,2).ne.0) then
    i=nzero+1
    cosc(i)=0.0d0
    c=2.0d0
    d=gord(nlat-1,cosc(i))
    d=d*d*fi*fi
    gwt(i)=c*(fi-0.5d0)/d
    colat(i)=pi*0.5d0
    sinc(i)=1.0d0
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

  ylat=90.0d0

  ! Calculate latitudes and latitude spacing
  do i=1,nzero
    xlat(i)=acos(sinc(i))*acon
    dlat(i)=xlat(i)-ylat
    ylat=xlat(i)
  end do

!  print *, 'xlat=', xlat

  ! Reverse latitudes for south hemisphere and assembly full array 
  xlatm=-1.0d0*xlat

!  print *, 'xlatm=', xlatm

  xlats=0.0d0

  do i=1,nzero
    xlats(i)=xlatm(i)
  end do

  xlat=xlat(nzero:1:-1)

  do i=nzero+1,nlat
    xlats(i)=xlat(i-nzero)
  end do

!  print *, 'xlats=', xlats

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

end subroutine gauss2lats

real function gord(n,x)

  ! Determine the colatitude
  colat=acos(x)
  
  c1=sqrt(2.0d0)
  
  do i=1,n
    c1=c1*sqrt(1.0d0-1.0d0/(4*i*i))
  end do
  
  fn=n
  ang=fn*colat
  s1=0.0d0
  c4=1.0d0
  a=-1.0d0
  b=0.0d0
  
  do k=0,n,2
    if (k.eq.n) then
      c4=0.5d0*c4
    end if
    s1=s1+c4*cos(ang)
    a=a+2.0d0
    b=b+1.0d0
    fk=k
    ang=colat*(fn-fk-2.0d0)
    c4=(a*(fn-b+1.0d0)/(b*(fn+fn-a)))*c4
  end do

  gord=s1*c1

  return

end function gord
