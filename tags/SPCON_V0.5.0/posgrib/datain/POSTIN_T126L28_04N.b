!namelist
!############################### Change Log ##################################
! 1.0.0.0
!
!  : alexalm $
!  : 2007/05/28 18:29:34 $
!  : 1.1.1.1 $
!
!
!#############################################################################
!
 &PosInput
  nffrs=-1,            ! value to indicated if model use or not initialization
                       !    or to indicate if it is a cold or warm start run:
                       !    nffrs=-1 - for runs with normal mode initialization
                       !    nffrs=0  - for runs without normal mode initialization
                       !    nffrs=1  - for warm start runs
  nfbeg=-1,          ! number of the first forecasted file to be post-processed
  nfend=65,       ! number of forecasted files to be post-processed
  nmand=-1,            ! number of pressure levels listed below in format 10f8.2
                       !    nmand=-1 means the use of default 18-levels
  RegIntIn=.FALSE.,    ! flag to interpolate outputs on regular grid (.TRUE.)
                       !    .FALSE. to get outputs on Gaussian grid
  Linear=.FALSE.,      ! Flag to Gaussian grid type Linear (.TRUE.) or 
                       !                            Quadratic (.FALSE.)
  trunc ='T126',       ! horizontal truncation = Txxx
  lev   ='L28',        ! vertical layers = Lxx
  labeli='2007101500', ! initial forecasting label (yyyymmddhh)
  labelf='2007103000', ! final forecasting label (yyyymmddhh)
  prefx ='04N',        ! preffix for input and output files
  req   ='',          ! flag to select requested field file (p, s, c, e or nothing)
                       !   p - use file rfd.pnt
                       !   s - use file rfd.sfc
                       !   c - use file rfd.clm
                       !   e - use file rfd.eta
                       !     - use file rfd
                       !   these files are spected to be in the directory:
                       !   /pos/datain
                       !   at the same directory there is a dft file for
                       !   derived fields (this should be included in the
                       !   code in future)
!   roperm='/gfs/dk19/io_dop/users/alex/oenspro'
  datain='/gfs/dk19/io_dop/users/alex/oenspro/model/dataout/T126L28', ! main dataout directory
  datalib='/gfs/home3/io_dop/users/alex/oenspro/posgrib/datain',   ! main dataout directory
  dataout='/gfs/dk19/io_dop/users/alex/oenspro/pos/dataout',  ! main dataout directory
                                 !   should be the same for model and post-processing
  Binary=.TRUE.,       ! output binary format ! False -> Grib (Default)
  postclim=.FALSE.,    ! The Option "postclim" only is functional by grib files
                       ! If postclim=TRUE, post-processing all files as anl.,
                       ! should be used for climate running
                       ! If postclim=FALSE,it differentiates anl. of the forecast
 /
 1000.00  925.00  850.00  700.00  500.00  400.00  300.00  250.00  200.00  150.00
  100.00   70.00   50.00   30.00   20.00   10.00    3.00