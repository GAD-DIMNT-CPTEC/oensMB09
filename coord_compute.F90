module coord_compute

   implicit none
   private
 
   real, parameter :: pi  = 3.14159265358979
   real, parameter :: dpr = 180.0/pi
   real, parameter :: eps = 1.0e-7

   public :: compute_earth_coord
   public :: compute_grid_coord


   character(len=64),parameter :: myname='Coord_Compute'
   contains
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! !ROUTINE: compute_earth_coord
!  \label{compute_earth_coord}
!
!
! !DESCRIPTION: This subroutine computes the earth coordinates (lat/lon values) 
!               of the specified domain. This routine is based on the grid
!               decoding routines in the ipolates interoplation package. 
!  
!  The input options include :
!  The current code recognizes the following projections:\\
!             (gridDesc(1)=000) equidistant cylindrical \\
!             (gridDesc(1)=001) mercator cylindrical \\
!             (gridDesc(1)=003) lambert conformal conical \\
!             (gridDesc(1)=004) gaussian cylindrical \\
!             (gridDesc(1)=005) polar stereographic azimuthal \\
!
!
! !INTERFACE:
!
subroutine compute_earth_coord(gridDesc, Udef, gen_xypts, &
                               xpts, ypts, rlon, rlat,    &
                               iret                       &
                               )

!
! !INPUT PARAMETERS:
!

  real, dimension(:), intent(in   ) :: gridDesc  ! grid description parameters
  real,               intent(in   ) :: Udef      ! value to set invalid output data
  logical,            intent(in   ) :: gen_xypts ! logical that specifies whether this
                                                 ! routine should generate xpts and ypts


!
! !OUTOUT PARAMETERS:
!

  real, dimension(:), intent(inout) :: xpts ! grid x point coordinates
  real, dimension(:), intent(inout) :: ypts ! grid y point coordinates
  real, dimension(:), intent(inout) :: rlat ! latitudes in degrees
  real, dimension(:), intent(inout) :: rlon ! longitudes in degrees
  integer, optional,  intent(  out) :: iret ! return code (0-success)

!
!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
!
! !REMARKS:
!
!    - gen_xypts: specifies whether this routine should generate
!                 xpts and ypts.  Example: This routine is called 
!                 twice when computing weights and neighbours for 
!                 the conservative intepolation scheme.  The first 
!                 call must generate xpts and ypts.  The second call
!                 should not.
!
! !SEE ALSO:
!
!   - EarthCoord_latlon_( ) - computes the earth coordinates of a latlon grid
!   - EarthCoord_merc_( ) - computes the earth coordinates of a mercator grid
!   - EarthCoord_lambert_( ) - computes the earth coordinates of a lambert conformal grid
!   - EarthCoord_gauss_( ) - computes the earth coordinates of a gaussian cylindrical grid
!   - EarthCoord_polar_( ) - computes the earth coordinates of a polar stereographic grid
!
!EOP
!-------------------------------------------------------------------------!
!BOC
  character(len=64), parameter :: myname_=trim(myname)//' :: compute_earth_coord( )'
  real    :: minimal
  integer :: im,jm,n
  integer :: npts, nij
  integer :: i, j
  integer :: nret

  if(present(iret)) iret = 0

  npts = size(xpts)

  if ( gen_xypts ) then

     im  = int(gridDesc(2))
     jm  = int(gridDesc(3))
     nij = im*jm
     
     if(nij.le.npts) then
        do n=1,nij
           j       = (n-1)/im+1
           i       = n-im*(j-1)
           xpts(n) = i
           ypts(n) = j
        enddo
        ! if nij < npts fill with Udef
        do n=nij+1,npts
           xpts(n) = Udef
           ypts(n) = Udef
        enddo
     else
        do n = 1,npts
           xpts(n) = Udef
           ypts(n) = Udef
        enddo
     endif
  endif

  select case (int(gridDesc(1)))

     case ( 0 ) ! Equidistant cylindrical
        call EarthCoord_latlon_(gridDesc, xpts, ypts, Udef,&
                                rlon, rlat, nret           &
                                )
        if(present(iret)) iret = nret
!     case ( 1 ) ! Mercator
!        call EarthCoord_merc_(gridDesc,npts,fill,xpts,ypts,&
!                                      rlon,rlat,nret)
!        if(present(iret)) iret = nret
!     case ( 3 ) ! Lambert Conformal
!        call EarthCoord_lambert_(gridDesc,npts,fill,xpts,ypts,&
!                                         rlon,rlat,nret)
!        if(present(iret)) iret = nret
     case ( 4 ) ! gaussian cylindrical
        call EarthCoord_gauss_(gridDesc, xpts, ypts, Udef,&
                               rlon, rlat, nret           &
                               )
        if(present(iret)) iret = nret
!     case ( 5 ) ! Polar Stereographic 
!        call EarthCoord_polar_(gridDesc,npts,fill,xpts,ypts,&
!                                    rlon,rlat,nret)
!        if(present(iret)) iret = nret
     case default 
        print*, 'Unrecognized Projection .... '
        print*, 'Program stopping ..'
        stop

  end select


end subroutine compute_earth_coord
!EOC
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! !ROUTINE: compute_earth_coord
!
!
! !DESCRIPTION: This subroutine computes the grid coordinates (cartesian) 
!               of the specified domain. This routine is based on the grid
!               decoding routines in the ipolates interoplation package. 
!  
!  The input options include :
!
!  The current code recognizes the following projections:\\
!             (gridDesc(1)=000) equidistant cylindrical \\
!             (gridDesc(1)=001) mercator cylindrical \\
!             (gridDesc(1)=003) lambert conformal conical \\
!             (gridDesc(1)=004) gaussian cylindrical \\
!             (gridDesc(1)=005) polar stereographic azimuthal \\
!
!
! !INTERFACE:
!
subroutine compute_grid_coord(gridDesc, rlon, rlat, Udef, &
                              xpts, ypts, iret            &
                             )

!
! !INPUT PARAMETERS:
!

  real, dimension(:), intent(in   ) :: gridDesc ! grid description parameters
  real,               intent(in   ) :: Udef     ! value to set invalid output data
  real, dimension(:), intent(in   ) :: rlat     ! latitudes in degrees
  real, dimension(:), intent(in   ) :: rlon     ! longitudes in degrees


!
! !OUTOUT PARAMETERS:
!

  real, dimension(:), intent(inout) :: xpts ! grid x point coordinates
  real, dimension(:), intent(inout) :: ypts ! grid y point coordinates
  integer, optional,  intent(  out) :: iret ! return code (0-success)

!
!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
!
! !REMARKS:
!
!
! !SEE ALSO:
!
!   - GridCoord_latlon_( ) - computes the grid coordinates of a latlon grid
!   - GridCoord_merc_( ) - computes the grid coordinates of a mercator grid
!   - GridCoord_lambert_( ) - computes the grid coordinates of a lambert conformal grid
!   - GridCoord_gauss_( ) - computes the grid coordinates of a gaussian cylindrical grid
!   - GridCoord_polar_( ) - computes the grid coordinates of a polar stereographic grid
!
!EOP
!-------------------------------------------------------------------------!
!BOC
  character(len=64), parameter :: myname_=trim(myname)//' :: compute_grid_coord( )'
  integer :: nret

  if(present(iret)) iret = 0

  select case (int(gridDesc(1)))

     case ( 0 ) ! Equidistant cylindrical
        call GridCoord_latlon_(gridDesc, rlon, rlat, Udef,&
                               xpts, ypts, nret           &
                              )
        if(present(iret)) iret = nret
!     case ( 1 ) ! Mercator
!        call GridCoord_merc_(gridDesc, rlon, rlat, Udef,&
!                             xpts, ypts, nret           &
!                            )
!        if(present(iret)) iret = nret
!     case ( 3 ) ! Lambert Conformal
!        call GridCoord_lambert_(gridDesc, rlon, rlat, Udef,&
!                                xpts, ypts, nret           &
!                               )
!        if(present(iret)) iret = nret
     case ( 4 ) ! gaussian cylindrical
        call GridCoord_gauss_(gridDesc, rlon, rlat, Udef,&
                              xpts, ypts, nret           &
                             )
        if(present(iret)) iret = nret
!     case ( 5 ) ! Polar Stereographic 
!        call GridCoord_polar_(gridDesc, rlon, rlat, Udef,&
!                              xpts, ypts, nret           &
!                             )
!        if(present(iret)) iret = nret
     case default 
        print*, 'Unrecognized Projection .... '
        print*, 'Program stopping ..'
        stop

  end select
end subroutine compute_grid_coord
!EOC
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! 
! !ROUTINE: EarthCoord_latlon_( )
!
!
! !DESCRIPTION: This subroutine computes the earth coordinates of 
!               the specified domain for an equidistant cylindrical projection.
!               This routine is based on the grid decoding routines in the NCEP
!               interoplation package. 
!
!
! !INTERFACE:
!
subroutine EarthCoord_latlon_(gridDesc, xpts, ypts, Udef, & 
                              rlon, rlat, iret            &
                              )
!
! !INPUT PARAMETERS:
!

  real, dimension(:), intent(in   ) :: gridDesc  ! grid description parameters
  real, dimension(:), intent(in   ) :: xpts      ! grid x point coordinates
  real, dimension(:), intent(in   ) :: ypts      ! grid y point coordinates  
  real,               intent(in   ) :: Udef      ! value to set invalid output data

!
! !OUTPUT PARAMETERS:
!

  real, dimension(:), intent(inout) :: rlon      ! longitudes in degrees
  real, dimension(:), intent(inout) :: rlat      ! latitudes in degrees 
  integer, optional,  intent(  out) :: iret      ! return code ( .ge. 0 - success)

!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
!
!EOP
!-------------------------------------------------------------------------!
!BOC

  character(len=64), parameter :: myname_=trim(myname)//' :: EarthCoord_latlon_( )'

  real    :: rlat1, rlon1
  real    :: rlat2, rlon2
  real    :: dlon, dlat
  real    :: xmin, xmax
  real    :: ymin, ymax
  integer :: im, jm, n
  integer :: npts


  if( present(iret) ) iret = 0
  
  npts = size(rlon)
  
  if ( gridDesc(1) .ne. 0 ) then

     do n = 1,npts
        rlon(n) = Udef
        rlat(n) = Udef
     enddo

     if( present(iret) ) iret = -1
     return

  endif
  
  !                                      rlat2,rlon2
  !       +----------------------------------o 
  !     L | \ d                              |
  !     A | | l                              |
  !     T | | a                              |
  !     I | / t                              |
  !     T |                                  |
  !     U |                                  |
  !     D |     dlon                         |
  !     E |    /---\                         |
  !       o----------------------------------+
  ! rlat1,rlon1    L O N G I T U D E 
  !
  !

  im    = int(gridDesc(2))
  jm    = int(gridDesc(3))
  rlat1 = gridDesc(4)
  rlon1 = gridDesc(5)
  rlat2 = gridDesc(7)
  rlon2 = gridDesc(8)

  !normalize lon to 0 ... 359
  rlon1 = mod((rlon1+3600.0),360.0)
  rlon2 = mod((rlon2+3600.0),360.0)

  if(rlat1 .gt. rlat2) then 
     dlat = -gridDesc(10)
  else
     dlat = gridDesc(10)
  endif

  if(rlon1 .gt. rlon2) then 
     dlon = -gridDesc(9)
  else
     dlon = gridDesc(9)
  endif

  xmin = 0
  xmax = im + 1
  if(im.eq.nint(360.0/abs(dlon))) xmax = im + 2
  ymin = 0
  ymax = jm + 1


! translate grid coordinates to earth coordinates
  if( present(iret) ) iret = 0
  do n = 1,npts

     if(xpts(n).ge.xmin.and.xpts(n).le.xmax.and. & 
          ypts(n).ge.ymin.and.ypts(n).le.ymax) then

        rlon(n) = rlon1 + dlon*(xpts(n)-1)

        if(rlon(n).lt.0) then 
           rlon(n) = 360.0 + rlon(n)
        endif

        rlat(n) = rlat1 + dlat*(ypts(n)-1)

        if(present(iret) ) iret = iret + 1

     else

        rlon(n) = Udef
        rlat(n) = Udef

     endif

  enddo

end subroutine EarthCoord_latlon_
!EOC
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! 
! !ROUTINE: EarthCoord_gauss_( )
!
! !DESCRIPTION: This subroutine computes the earth coordinates of 
!               the specified domain for a gaussian cylindrical projection.
!               This routine is based on the grid decoding routines
!               in the NCEP interoplation package. 
!
! !INTERFACE:
!
subroutine EarthCoord_gauss_(gridDesc,xpts,ypts, Udef,&
                             rlon,rlat,iret           &
                             )

!
! !INPUT PARAMETERS:
!

  real, dimension(:), intent(in   ) :: gridDesc  ! grid description parameters
  real, dimension(:), intent(in   ) :: xpts      ! grid x point coordinates
  real, dimension(:), intent(in   ) :: ypts      ! grid y point coordinates  
  real,               intent(in   ) :: Udef      ! value to set invalid output data

!
! !OUTPUT PARAMATERS:
!

  real, dimension(:), intent(inout) :: rlon      ! longitudes in degrees
  real, dimension(:), intent(inout) :: rlat      ! latitudes in degrees 
  integer, optional,  intent(  out) :: iret      ! return code ( .ge. 0 - success)

!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
! !SEE ALSO:
!
!  - gausslat: Computes latitude values in gaussian
!
!-------------------------------------------------------------------------!
!BOC

  character(len=64), parameter :: myname_=trim(myname)//' :: EarthCoord_gauss_( )'

  integer, parameter :: jgmax = 2000

  real    :: rlata, rlatb  
  real    :: rlat1, rlon1
  real    :: rlat2, rlon2
  real    :: xmin, xmax
  real    :: ymin, ymax
  real    :: hi, wb
  real    :: dlon
  real    :: alat(0:jgmax+1),blat(jgmax)
  real    :: minimal
  integer :: im,jm, jg, j, ja, n
  integer :: iscan,jscan,nscan
  integer :: jh, j1, j2
  integer :: npts

!-------------------------------------------------------------------------!

  npts = size(rlon)

  if(gridDesc(1).ne.4.or.gridDesc(10)*2.gt.jgmax) then

     if(present(iret) )iret = -1
     
     do n=1,npts
        rlon(n) = Udef
        rlat(n) = Udef
     enddo

     return

  endif

  im    = int(gridDesc(2))
  jm    = int(gridDesc(3))
  npts  = im * jm
  rlat1 = gridDesc(4)
  rlon1 = gridDesc(5)
  rlat2 = gridDesc(7)
  rlon2 = gridDesc(8)
  jg    = int(gridDesc(10)*2)
  iscan = mod(nint(gridDesc(11))/128,2)
  jscan = mod(nint(gridDesc(11))/64,2)
  nscan = mod(nint(gridDesc(11))/32,2)
  hi    = (-1.0)**iscan
  jh    = (-1  )**jscan
  dlon  = hi*(mod(hi*(rlon2-rlon1)-1+3600,360.)+1)/(im-1)

  !normalize lon to 0 ... 359
  rlon1 = mod((rlon1+3600.0),360.0)
  rlon2 = mod((rlon2+3600.0),360.0)

  call gausslat(alat(1:1),blat)

  do ja=1,jg
     alat(ja) = dpr*asin(alat(ja))
  enddo

  alat(0)    = 180.-alat(1)
  alat(jg+1) = -alat(0)

  j1 = 1
  do while(j1.lt.jg.and.rlat1.lt.(alat(j1)+alat(j1+1))/2)
     j1=j1+1
  enddo

  j2   = j1+jh*(jm-1)
  xmin = 0
  xmax = im+1
  if(im.eq.nint(360/abs(dlon))) xmax=im+2
  ymin = 0.5
  ymax = jm+0.5

! translate grid coordinates to earth coordinates

  if(present(iret)) iret = 0
  do n = 1,npts
     if(xpts(n).ge.xmin.and.xpts(n).le.xmax.and. & 
          ypts(n).ge.ymin.and.ypts(n).le.ymax) then

        j       = min(int(ypts(n)),jm)
        wb      = ypts(n)-j

        rlon(n) = mod(rlon1+dlon*(xpts(n)-1)+3600,360.)
        rlata   = alat(j1+jh*(j-1))
        rlatb   = alat(j1+jh*j)
        rlat(n) = rlata+wb*(rlatb-rlata)

        if(present(iret)) iret = iret+1

     else

        rlon(n) = Udef
        rlat(n) = Udef

     endif
     
  enddo



end subroutine EarthCoord_gauss_
!EOC
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! 
! !ROUTINE: GridCoord_latlon_( )
!
! !DESCRIPTION: This subroutine computes the grid coordinates of 
!               the specified domain for an equidistant cylindrical rojection.
!               This routine is based on the grid decoding routines in the
!               NCEP interoplation package. 
!
! !INTERFACE:
!
subroutine GridCoord_latlon_(gridDesc, rlon, rlat, Udef, & 
                             xpts, ypts, iret            &
                            )

!
! !INPUT PARAMETERS:
!

  real, dimension(:), intent(in   ) :: gridDesc  ! grid description parameters
  real,               intent(in   ) :: Udef      ! value to set invalid output data
  real, dimension(:), intent(in   ) :: rlon      ! longitudes in degrees
  real, dimension(:), intent(in   ) :: rlat      ! latitudes in degrees 

!
! !OUTPUT PARAMETERS:
!

  real, dimension(:), intent(inout) :: xpts      ! grid x point coordinates
  real, dimension(:), intent(inout) :: ypts      ! grid y point coordinates  
  integer, optional,  intent(  out) :: iret      ! return code ( .ge. 0 - success)

!
!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
!
!EOP
!-------------------------------------------------------------------------!
!BOC

  character(len=64), parameter :: myname_=trim(myname)//' :: GridCoord_latlon_( )'

  real    :: rlat1, rlon1
  real    :: rlat2, rlon2
  real    :: dlon, dlat
  real    :: xmin, xmax
  real    :: ymin, ymax
  integer :: im, jm, n
  integer :: npts

  if( present(iret) ) iret = 0

  npts = size(rlon)

  if ( gridDesc(1) .ne. 0 ) then


     do n = 1,npts
        xpts(n) = UDef
        ypts(n) = UDef
     enddo

     if( present(iret) ) iret = -1
     return

  endif


  im    = int(gridDesc(2))
  jm    = int(gridDesc(3))
  rlat1 = gridDesc(4)
  rlon1 = gridDesc(5)
  rlat2 = gridDesc(7)
  rlon2 = gridDesc(8)

  !normalize lon to 0 ... 359
  rlon1 = mod((rlon1+3600.0),360.0)
  rlon2 = mod((rlon2+3600.0),360.0)
  
  if(rlat1.gt.rlat2) then 
     dlat = -gridDesc(10)
  else
     dlat = gridDesc(10)
  endif

  if(rlon1.gt.rlon2) then 
     dlon = -gridDesc(9)
  else
     dlon = gridDesc(9)
  endif

  xmin = 0
  xmax = im+1
  if(im.eq.nint(360/abs(dlon))) xmax = im+2
  ymin = 0
  ymax = jm+1

  if( present(iret) ) iret = 0
  do n=1,npts
     if(abs(rlon(n)).le.360.and.abs(rlat(n)).le.90) then

!        if(rlon(n).gt.180) then 
!           xpts(n) = 1 + (rlon(n)-360-rlon1)/dlon
!        else
           xpts(n) = 1 + (rlon(n)-rlon1)/dlon
!        endif

        ypts(n) = 1 + (rlat(n)-rlat1)/dlat

        if(xpts(n).ge.xmin.and.xpts(n).le.xmax.and. & 
             ypts(n).ge.ymin.and.ypts(n).le.ymax) then


           if(present(iret))iret = iret + 1

        else

           xpts(n) = Udef
           ypts(n) = Udef
        endif

     else
        xpts(n) = Udef
        ypts(n) = Udef
     endif

  enddo
end subroutine GridCoord_latlon_
!EOC
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! 
! !ROUTINE: compute_grid_coord_gauss
!
! !DESCRIPTION: This subroutine computes the grid coordinates of 
!               the specified domain for a gaussian cylindrical projection.
!               This routine is based on the grid decoding routines
!               in the NCEP interoplation package.
!
! !INTERFACE:
!
subroutine GridCoord_gauss_(gridDesc, rlon, rlat, Udef, & 
                            xpts, ypts, iret            &
                           )

!
! !INPUT PARAMETERS:
!

  real, dimension(:), intent(in   ) :: gridDesc  ! grid description parameters
  real,               intent(in   ) :: Udef      ! value to set invalid output data
  real, dimension(:), intent(in   ) :: rlon      ! longitudes in degrees
  real, dimension(:), intent(in   ) :: rlat      ! latitudes in degrees 

!
! !OUTPUT PARAMETERS:
!

  real, dimension(:), intent(inout) :: xpts      ! grid x point coordinates
  real, dimension(:), intent(inout) :: ypts      ! grid y point coordinates  
  integer, optional,  intent(  out) :: iret      ! return code ( .ge. 0 - success)

!
!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic.
!
! !SEE ALSO:
!
!  - gausslat: Compute latitude values in gaussian
!
!EOP
!-------------------------------------------------------------------------!
!BOC

  character(len=64), parameter :: myname_=trim(myname)//' :: GridCoord_gauss_( )'

  integer, parameter :: jgmax = 2000

  real    :: rlat1, rlon1
  real    :: rlat2, rlon2
  real    :: xmin, xmax
  real    :: ymin, ymax
  real    :: hi, wb
  real    :: dlon
  real    :: yptsa, yptsb
  real    :: alat(0:jgmax+1),blat(jgmax)
  integer :: im,jm, jg, ja, n
  integer :: iscan,jscan,nscan
  integer :: jh, j1, j2
  integer :: npts
  
!-------------------------------------------------------------------------!


  if( present(iret) ) iret = 0

  npts = size(rlon)
  
  if(gridDesc(1).ne.4.or.gridDesc(10)*2.gt.jgmax) then

     if(present(iret) )iret = -1
     
     do n=1,npts
        xpts(n) = Udef
        ypts(n) = Udef
     enddo

     return

  endif

  im    = int(gridDesc(2))
  jm    = int(gridDesc(3))
  rlat1 = gridDesc(4)
  rlon1 = gridDesc(5)
  rlat2 = gridDesc(7)
  rlon2 = gridDesc(8)
  jg    = int(gridDesc(10)*2)
  iscan = mod(nint(gridDesc(11))/128,2)
  jscan = mod(nint(gridDesc(11))/64,2)
  nscan = mod(nint(gridDesc(11))/32,2)
  hi    = (-1.)**iscan
  jh    = (-1)**jscan
  dlon  = hi*(mod(hi*(rlon2-rlon1)-1+3600,360.)+1)/(im-1)

  !normalize lon to 0 ... 359
  rlon1 = mod((rlon1+3600.0),360.0)
  rlon2 = mod((rlon2+3600.0),360.0)


  call gausslat(alat(1:1),blat)

  do ja=1,jg
     alat(ja)=dpr*asin(alat(ja))
  enddo

  alat(0)    = 180.-alat(1)
  alat(jg+1) = -alat(0)

  j1 = 1
  do while(j1.lt.jg.and.rlat1.lt.(alat(j1)+alat(j1+1))/2)
     j1 = j1 + 1
  enddo

  j2   = j1+jh*(jm-1)
  xmin = 0
  xmax = im+1
  if(im.eq.nint(360/abs(dlon))) xmax=im+2
  ymin = 0.5
  ymax = jm+0.5


! translate grid coordinates to earth coordinates

  if(present(iret)) iret = 0
  if(abs(dlon-gridDesc(9)).gt.0.01) then
     print*, 'problem with the domain calculations : gdswiz04'
     stop
  endif

  do n=1,npts
     xpts(n) = Udef
     ypts(n) = Udef
     if(abs(rlon(n)).le.360.and.abs(rlat(n)).le.90) then
        xpts(n) = 1+hi*mod(hi*(rlon(n)-rlon1)+3600,360.)/dlon
        ja      = min(int((jg+1)/180.*(90-rlat(n))),jg)

        if(rlat(n).gt.alat(ja)) ja=max(ja-2,0)
        if(rlat(n).lt.alat(ja+1)) ja=min(ja+2,jg)
        if(rlat(n).gt.alat(ja)) ja=ja-1
        if(rlat(n).lt.alat(ja+1)) ja=ja+1

        yptsa   = 1+jh*(ja-j1)
        yptsb   = 1+jh*(ja+1-j1)
        wb      = (alat(ja)-rlat(n))/(alat(ja)-alat(ja+1))
        ypts(n) = yptsa + wb*(yptsb-yptsa)
        
        if(xpts(n).ge.xmin.and.xpts(n).le.xmax.and. & 
             ypts(n).ge.ymin.and.ypts(n).le.ymax) then
           if(present(iret)) iret = iret+1
        else
           xpts(n) = Udef
           ypts(n) = Udef
        endif
     endif
  enddo

end subroutine GridCoord_gauss_
!EOC
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! 
! !ROUTINE : gausslat
!
! !DESCRIPTION:  This subroutine computes gaussian latitudes.
!                Computes cosines of colatitude and gaussian weights
!                on the gaussian latitudes.  the gaussian latitudes are at
!                the zeroes of the legendre polynomial of the given order.
!
! !INTERFACE:
subroutine gausslat(slat,wlat)

!
! !INPUT/OUTPUT PARAMETERS:
!
  real, dimension(:), intent(inout) :: slat ! cosines of colatitude
  real, dimension(:), intent(inout) :: wlat ! gaussian weights



! !REVISION HISTORY:
!   04-16-92 Mark Iredell; Initial Specification
!   10-20-97 Mark Iredell; Increased precision
!   05-14-02 Urzula Jambor; Reduced limit of eps from e-12 to e-7
!
!EOP
!-------------------------------------------------------------------------!
!BOC
  character(len=64), parameter :: myname_=trim(myname)//' :: gausslat( )'

  integer, parameter :: jz = 50
  real,    parameter :: c  = (1.-(2./pi)**2)*0.25
  real, parameter :: bz(1:50) = [                                                             &
                                   2.4048255577,  5.5200781103,                               & 
                                   8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679, &
                                  21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684, & 
                                  33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132, & 
                                  46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550, & 
                                  58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299, & 
                                  71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711, & 
                                  84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819, & 
                                  96.6052679510, 99.7468198587, 102.888374254, 106.029930916, & 
                                 109.1714896490, 112.313050280, 115.454612653, 118.596176630, & 
                                 121.7377420880, 124.879308913, 128.020877005, 131.162446275, & 
                                 134.3040166380, 137.445588020, 140.587160352, 143.728733573, & 
                                 146.8703076250, 150.011882457, 153.153458019, 156.295034268  &
                                ]
  integer :: jmax                                
  integer :: jh, jhe, n, j
  real    :: spmax, sp, r
  real, allocatable, dimension(:) :: pk
  real, allocatable, dimension(:) :: pkm1
  real, allocatable, dimension(:) :: pkm2
  

  jmax = max(size(slat),size(wlat))

  allocate(pk(jmax/2))
  allocate(pkm1(jmax/2))
  allocate(pkm2(jmax/2))

  jh  = jmax/2
  jhe = (jmax+1)/2
  r   = 1./sqrt((jmax+0.5)**2+c)

  do j=1,min(jh,jz)
     slat(j)=cos(bz(j)*r)
  enddo

  do j=jz+1,jh
     slat(j)=cos((bz(jz)+(j-jz)*pi)*r)
  enddo

  spmax=1.
  do while(spmax.gt.eps)
     spmax=0.

     do j=1,jh
        pkm1(j) = 1.
        pk(j)   = slat(j)
     enddo

     do n=2,jmax
        do j=1,jh
           pkm2(j) = pkm1(j)
           pkm1(j) = pk(j)
           pk(j)   = ((2*n-1)*slat(j)*pkm1(j)-(n-1)*pkm2(j))/n
        enddo
     enddo

     do j=1,jh
        sp      = pk(j)*(1.-slat(j)**2)/(jmax*(pkm1(j)-slat(j)*pk(j)))
        slat(j) = slat(j)-sp
        spmax   = max(spmax,abs(sp))
     enddo
  enddo

  do j=1,jh
     wlat(j)        = (2.*(1.-slat(j)**2))/(jmax*pkm1(j))**2
     slat(jmax+1-j) = -slat(j)
     wlat(jmax+1-j) = wlat(j)
  enddo

  if(jhe.gt.jh) then

     slat(jhe) = 0.
     wlat(jhe) = 2./jmax**2

     do n=2,jmax,2
        wlat(jhe) = wlat(jhe)*n**2/(n-1)**2
     enddo

  endif

end subroutine gausslat
!EOC
!-------------------------------------------------------------------------!

end module coord_compute

