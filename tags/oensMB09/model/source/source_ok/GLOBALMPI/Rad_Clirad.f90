!
!  $Author: pkubota $
!  $Date: 2011/05/27 23:15:12 $
!  $Revision: 1.3 $
!
MODULE Rad_Clirad

  USE Constants, ONLY :  &
       pai,              & ! constant pi=3.1415926e0
       tf,               & ! Temperatura de congelamento (K)=273.16e0
       r8,i8

  USE Options, ONLY : &
       co2val, &              ! co2val is wgne standard value in ppm
       nfprt

  !
  ! cliradintf---
  !             |
  !             |
  !             |
  !             |
  !             |
  !             |
  !             |
  !             |
  !             | cloudy*--| soradcld ----| soluvcld ---| cldscale
  !                                       |             | deledd
  !                                       |             | cldflx
  !                                       |
  !                                       | solircld ---| cldscale
  !                                       |             | deledd
  !                                       |             | cldflx
  !                                       | rflx

  USE Options, ONLY :       &
       asolc, &
       asolm

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: cliradsw

CONTAINS

  !-----------------------------------------------------------------------
  !
  ! Subroutine: CLIRADSW (Clirad Interface)
  !
  ! This subroutine replaces SWRAD in the global model. It uses nearly the 
  ! same input and output and calls the main routines of Chou and Suarez
  ! radiation code.
  !
  ! ACRONYMS: 
  !   CDLGP...CLOUDY DAYTIME LATITUDE GRID POINTS
  !   DLGP....DAYTIME LATITUDE GRID POINTS
  !   LGP.....LATITUDE GRID POINTS
  !   NSOL....NUMBER OF DAYTIME LATITUDE GRID POINTS
  !   NCLD....NUMBER OF CLOUDY DLGP
  !
  ! Authors: Tatiana Tarasova & Henrique Barbosa
  !
  !-----------------------------------------------------------------------

  SUBROUTINE cliradsw(&
       ! Model Info and flags
       ncols , kmax  , nls   , noz   , &
       icld  , inalb , s0    , cosz  , &
       ! Atmospheric fields
       pl20  , dpl   , tl    , ql    , &
       o3l   , gps   , imask , &
       ! SURFACE:  albedo
       alvdf , alndf , alvdr , alndr , &
       ! SW Radiation fields 
       swinc ,                         &
       radvbc, radvdc, radnbc, radndc, &
       radvbl, radvdl, radnbl, radndl, &
       dswclr, dswtop, ssclr , ss    , &
       aslclr, asl   ,                 &
       ! Cloud field
       cld   , clu   , fice  , &
       rei   , rel   , taud              )

    IMPLICIT NONE
  !==========================================================================
  !
  ! ___________________________
  ! MODEL INFORMATION AND FLAGS
  !
  ! ncols.....Number of grid points on a gaussian latitude circle
  ! kmax......Number of sigma levels
  ! nls.......Number of layers in the stratosphere.
  ! noz.......
  ! icld......Select three types of cloud emissivity/optical depth
  !           =1 : old cloud emissivity (optical depth) setting
  !                ccu :  0.05 *dp
  !                css :  0.025*dp       for ice cloud t<253.0
  !                       0.05 *dp       for ice cloud t>253.0
  !           =2 : new cloud emissivity (optical depth) setting
  !                ccu : (0.16)*dp
  !                css :  0.0                              t<-82.5
  !                      (2.0e-6*(t-tcrit)**2)*dp    -82.5<t<-10.0
  !                      (6.949e-3*(t-273)+.08)*dp   -10.0<t<  0.0
  !                      (0.08)*dp                   -10.0<t<  0.0
  !           =3    : ccm3 based cloud emissivity
  ! inalb.....Select two types of surface albedo
  !           =1 : input two  types surface albedo (2 diffused)
  !                direct beam albedos are calculated by the subr.
  !           =2 : input four types surfc albedo (2 diff,2 direct)
  ! s0........Solar constant  at proper sun-earth distance
  ! cosz......Cosine of solar zenith angle
  !
  ! __________________
  ! ATMOSPHERIC FIELDS
  !
  !    flip arrays (k=1 means top of atmosphere)
  !
  ! pl20......Flip array of pressure at bottom of layers (mb)
  ! dpl.......Flip array of pressure difference bettween levels
  ! tl........Flip array of temperature (K)
  ! ql........Flip array of specific humidity (g/g)
  ! o3l.......Flip array of ozone mixing ratio (g/g)
  ! gps.......Surface pressure (mb)
  ! imask.....Sea/Land mask
  !
  ! ________________
  ! SURFACE:  albedo
  !
  ! alvdf.....Visible diffuse surface albedo
  ! alndf.....Near-ir diffuse surface albedo
  ! alvdr.....Visible beam surface albedo
  ! alndr.....Near-ir beam surface albedo
  !
  ! ________________________________________
  ! SW Radiation fields 
  !
  ! swinc.....Incident SW at top 
  ! radvbc....Down Sfc SW flux visible beam    (all-sky)
  ! radvdc....Down Sfc SW flux visible diffuse (all-sky)
  ! radnbc....Down Sfc SW flux Near-IR beam    (all-sky)
  ! radndc....Down Sfc SW flux Near-IR diffuse (all-sky)
  ! radvbl....Down Sfc SW flux visible beam    (clear)  
  ! radvdl....Down Sfc SW flux visible diffuse (clear)  
  ! radnbl....Down Sfc SW flux Near-IR beam    (clear)  
  ! radndl....Down Sfc SW flux Near-IR diffuse (clear)  
  ! dswclr....Net SW flux at TOA (clear)   = Abs by Atmos + Sfc
  ! dswtop....Net SW flux at TOA (all-sky) = Abs by Atmos + Sfc
  ! ssclr.....Net SW flux at SFC (clear)   = Abs by Sfc
  ! ss........Net SW flux at SFC (all-sky) = Abs by Sfc
  ! asl.......Heating rate due to shortwave         (K/s)
  ! aslclr....Heating rate due to shortwave (clear) (K/s)
  !
  ! ___________
  ! Cloud field
  !
  ! cld.......Supersaturation cloud fraction
  ! clu.......Convective cloud fraction     
  ! fice......Fraction of cloud water in the form of ice
  ! rei.......Ice particle Effective Radius (microns)
  ! rel.......Liquid particle Effective Radius (microns)
  ! taud......Shortwave cloud optical depth
  !
  !==========================================================================

    ! Model Info and flags
    INTEGER      ,    INTENT(in   ) :: ncols
    INTEGER      ,    INTENT(in   ) :: kmax
    INTEGER      ,    INTENT(in   ) :: nls
    LOGICAL      ,    INTENT(in   ) :: noz
    INTEGER      ,    INTENT(in   ) :: icld
    INTEGER      ,    INTENT(in   ) :: inalb
    REAL(KIND=r8),    INTENT(in   ) :: s0
    REAL(KIND=r8),    INTENT(in   ) :: cosz   (ncols)

    ! Atmospheric fields
    REAL(KIND=r8),    INTENT(in   ) :: pl20   (ncols,kmax)
    REAL(KIND=r8),    INTENT(in   ) :: dpl    (ncols,kmax)
    REAL(KIND=r8),    INTENT(in   ) :: tl     (ncols,kmax)
    REAL(KIND=r8),    INTENT(in   ) :: ql     (ncols,kmax)
    REAL(KIND=r8),    INTENT(in   ) :: o3l    (ncols,kmax)
    REAL(KIND=r8),    INTENT(in   ) :: gps    (ncols)
    INTEGER(KIND=i8), INTENT(IN   ) :: imask  (ncols) 

    ! SURFACE:  albedo
    REAL(KIND=r8),    INTENT(in   ) :: alvdf  (ncols)
    REAL(KIND=r8),    INTENT(in   ) :: alndf  (ncols)
    REAL(KIND=r8),    INTENT(inout) :: alvdr  (ncols)
    REAL(KIND=r8),    INTENT(inout) :: alndr  (ncols)
    ! SW Radiation fields 
    REAL(KIND=r8),    INTENT(out) :: swinc  (ncols)
    REAL(KIND=r8),    INTENT(out) :: radvbc (ncols)
    REAL(KIND=r8),    INTENT(out) :: radvdc (ncols)
    REAL(KIND=r8),    INTENT(out) :: radnbc (ncols)
    REAL(KIND=r8),    INTENT(out) :: radndc (ncols)
    REAL(KIND=r8),    INTENT(out) :: radvbl (ncols)
    REAL(KIND=r8),    INTENT(out) :: radvdl (ncols)
    REAL(KIND=r8),    INTENT(out) :: radnbl (ncols)
    REAL(KIND=r8),    INTENT(out) :: radndl (ncols)
    REAL(KIND=r8),    INTENT(out) :: aslclr (ncols,kmax)
    REAL(KIND=r8),    INTENT(out) :: asl    (ncols,kmax)
    REAL(KIND=r8),    INTENT(out) :: ss     (ncols)
    REAL(KIND=r8),    INTENT(out) :: ssclr  (ncols)
    REAL(KIND=r8),    INTENT(out) :: dswtop (ncols)
    REAL(KIND=r8),    INTENT(out) :: dswclr (ncols)

    ! Cloud field and Microphysics
    REAL(KIND=r8),    INTENT(in   ) :: cld    (ncols,kmax)
    REAL(KIND=r8),    INTENT(in   ) :: clu    (ncols,kmax)
    REAL(KIND=r8),    INTENT(in   ) :: fice   (ncols,kmax)
    REAL(KIND=r8),    INTENT(in   ) :: rei    (ncols,kmax)
    REAL(KIND=r8),    INTENT(in   ) :: rel    (ncols,kmax) 
    REAL(KIND=r8),    INTENT(in   ) :: taud   (ncols,kmax)

    ! local  variables
    INTEGER i,k
    INTEGER :: nsol ! NUMBER OF SOLAR LATITUDE GRID POINTS (COSZ>0.01)

    REAL(KIND=r8):: scosz (ncols) ! DOWNWARD FLUX AT TOP IN DLGP
    REAL(KIND=r8):: cmu   (ncols) ! COSINE OF SOLAR ZENITH ANGLE IN DLGP
    INTEGER  :: dmask (ncols) ! sib-mask in DLGP
    !
    !     Downward ground fluxes in dlgp
    !
    REAL(KIND=r8):: rvbl  (ncols) ! VISIBLE, BEAM, CLEAR
    REAL(KIND=r8):: rvdl  (ncols) ! VISIBLE, DIFFUSE, CLEAR
    REAL(KIND=r8):: rnbl  (ncols) ! NearIR, BEAM, CLEAR
    REAL(KIND=r8):: rndl  (ncols) ! NearIR, DIFFUSE, CLEAR
    REAL(KIND=r8):: rvbc  (ncols) ! VISIBLE, BEAM, CLOUDY
    REAL(KIND=r8):: rvdc  (ncols) ! VISIBLE, DIFFUSE, CLOUDY
    REAL(KIND=r8):: rnbc  (ncols) ! NearIR, BEAM, CLOUDY
    REAL(KIND=r8):: rndc  (ncols) ! NearIR, DIFFUSE, CLOUDY
    REAL(KIND=r8):: sl    (ncols) ! NET GROUND CLEAR FLUX IN DLGP
    REAL(KIND=r8):: sc    (ncols) ! NET GROUND CLOUDY FLUX IN DLGP
    !
    !     Other fluxes
    !
    REAL(KIND=r8):: dsclr (ncols)        ! ABSORPTION OF CLEAR ATMOSPHERE AND GROUND
    REAL(KIND=r8):: dscld (ncols)        ! DOWNWARD FLUX AT TOP IN DLGP
    REAL(KIND=r8):: aclr  (ncols,kmax+1) ! ABSORPTION IN CLEAR ATM.
    REAL(KIND=r8):: acld  (ncols,kmax+1) ! HEATING RATE (CLOUDY)
    !
    !     Albedo in DLGP
    !
    REAL(KIND=r8):: agv   (ncols)        ! GROUND VISIBLE DIFFUSE ALBEDO IN DLGP
    REAL(KIND=r8):: agn   (ncols)        ! GROUND NEAR IR DIFFUSE ALBEDO IN DLGP
    REAL(KIND=r8):: rsurfv(ncols)        ! GROUND VISIBLE BEAM ALBEDO IN DLGP
    REAL(KIND=r8):: rsurfn(ncols)        ! GROUND NEAR IR BEAM ALBEDO IN DLGP
    !
    !     Atmospheric state in DLGP
    !
    REAL(KIND=r8):: ps (ncols)        ! surface pressure in DLGP (mb)
    REAL(KIND=r8):: ta (ncols,kmax+1) ! LAYER TEMPERATURE IN DLGP
    REAL(KIND=r8):: wa (ncols,kmax+1) ! LAYER SPECIFIC HUMIDITY IN DLGP
    REAL(KIND=r8):: oa (ncols,kmax+1) ! LAYER OZONE MIXING RATIO IN DLGP
    REAL(KIND=r8):: pu (ncols,kmax+2) ! PRESSURE AT BOTTOM OF LAYER IN DLGP
    REAL(KIND=r8):: dp (ncols,kmax+1) ! PRESSURE DIFFERENCE IN DLGP
    !
    !     Clouds in DLGP
    !
    REAL(KIND=r8):: css   (ncols,kmax+1) ! LARGE SCALE CLOUD AMOUNT IN DLGP
    REAL(KIND=r8):: ccu   (ncols,kmax+1) ! CUMULUS CLOUD AMOUNT IN DLGP
    REAL(KIND=r8):: e0 (ncols,kmax+1) ! Cloud optical depth
    REAL(KIND=r8):: frcice(ncols,kmax+1) ! Fraction of cloud water in the form of ice
    REAL(KIND=r8):: radliq(ncols,kmax+1) ! Ice particle Effective Radius (microns)
    REAL(KIND=r8):: radice(ncols,kmax+1) ! Liquid particle Effective Radius (microns)

    REAL(KIND=r8):: tauk (ncols,kmax+1) ! total optical depth with k=1 at top
    !
    !     Logical and working vectors
    !
    INTEGER  :: litx  (ncols) ! NUMBERS OF DLGP IN ALL LAYERS
    INTEGER  :: listim(ncols) ! =1,2,3...NCOLS*(KMAX+1)
    LOGICAL  :: bitx  (ncols) ! TRUE IN SOLAR LATITUDE GRID POINTS
    LOGICAL  :: bitc  (ncols,kmax+1) ! Working logical

    ! Initialize local vectors and output variables

    scosz=0.0_r8
    cmu=0.0_r8
    listim=0
    bitx=.FALSE.

    ps=0.0_r8
    ta=0.0_r8
    wa=0.0_r8
    oa=0.0_r8
    pu=0.0_r8

    rsurfv=0.0_r8
    rsurfn=0.0_r8
    agv=0.0_r8
    agn=0.0_r8

    ccu=0.0_r8
    css=0.0_r8
    dp=0.0_r8
    litx=0
    dmask=0

    ! Subroutine starts here

    !
    ! Set array listim = i, WHEN I=1,ncols*(kmax+2)
    !
    !FORALL (I=1:ncols) listim(I)=I
    DO i=1,ncols
        listim(i)=i
    END DO
    !
    ! set bits for daytime grid points
    ! BITX=.TRUE. IF COSZ>DUM(1)....0.01
    !
    !bitx(1:ncols)=cosz(1:ncols).ge.0.01e0_r8
    nsol=0
    DO i=1,ncols
       IF(cosz(i).ge.0.01e0_r8)THEN
           nsol=nsol+1
           bitx(i)=.TRUE.
       END IF
    END DO
    !
    ! Calculate nsol = number of daytime latitude grid points
    !
    !nsol=COUNT(bitx(1:ncols))
    !
    ! Set zero to all latitude grids surface fluxes
    !
    swinc  = 0.0_r8
    ss     = 0.0_r8
    ssclr  = 0.0_r8
    dswtop = 0.0_r8
    dswclr = 0.0_r8
    radvbl = 0.0_r8
    radvdl = 0.0_r8
    radnbl = 0.0_r8
    radndl = 0.0_r8
    radvbc = 0.0_r8
    radvdc = 0.0_r8
    radnbc = 0.0_r8
    radndc = 0.0_r8
    asl    = 0.0_r8
    aslclr = 0.0_r8
    !
    ! If there are no daytime points then
    !
    IF(nsol.eq.0) RETURN
    !
    ! Set integer array litx (nsol*(kmax+1))
    ! numbers of latitude daytime grid points at first level
    !
    !litx(1:nsol) = PACK(listim(1:ncols), bitx(1:ncols))
    nsol=0
    DO  i=1,ncols
       IF(bitx(i))THEN
          nsol=nsol+1
          litx(nsol) = listim(i)
       END IF  
    END DO
    !
    !  Transform  two-size  input arrays:
    !        pl20(ncols,kmax),dpl,tl,ql,cld,clu
    !  in two-size arrays:
    !        pu(nsol,kmax+2),dp(nsol,kmax+1),ta,wa,css,ccu
    !  in daytime latitude grid points at all levels
    !
    
    DO k=2,kMax+1
       DO i=1,nsol
          pu (i,k+1)=pl20(litx(i),k-1)
          dp (i,k)  =dpl (litx(i),k-1)
          ta (i,k)  =tl  (litx(i),k-1)
          wa (i,k)  =max (0.1e-22_r8,ql(litx(i),k-1))
          oa (I,k)  =max (0.1e-9_r8,o3l(litx(i),k-1))
          css(i,k)  =cld (litx(i),k-1)
          ccu(i,k)  =clu (litx(i),k-1)
          ps (i)    =gps (litx(i))
       END DO
    END DO

    !
    ! Set some parameters at first, second
    !
    !#TO Correcao de hmjb
    DO i=1,nsol
       pu(i,2) = PU(I,3)/2.0_r8  ! 0.5_r8
       pu(i,1) = PU(I,2)/10.0_r8 ! 0.05_r8
       dp(i,1) = pu(i,2)-pu(i,1) ! pressure differense
       ta(i,1) = ta(i,2)       ! temperature
       wa(i,1) = wa(i,2)       ! specific humidity
       oa(i,1) = oa(i,2)       ! ozone

    ENDDO

    ! if no ozone
    IF (noz) oa=0.0_r8

    !
    ! Set some parameters at stratospheric levels
    !
    css(1:nsol,1:nls+1)=0.0_r8
    ccu(1:nsol,1:nls+1)=0.0_r8
    wa (1:nsol,1:nls  )=3.0e-6_r8

    !
    ! The same transformation as mentioned above for:
    !   visible surface albedo....alvdf to agv
    !   nearir surface albedo.....alndf to agn
    !   cosine of solar zenith angle..cosz to cmu
    !
    FORALL (I=1:nsol,litx(I).le.ncols)
       agv  (I) = alvdf (litx(I))
       agn  (I) = alndf (litx(I))
       cmu  (I) = cosz  (litx(I))
       dmask(I) = imask (litx(i))
    ENDFORALL
    !
    ! If direct beam albedos are given then
    ! alvdr transform to rsurfv(nsol) and alndr to rsurfn(nsol)
    ! in daytime grid points
    !
    !hmjb inalb=2 is hardcoded in physics!!!
    IF (inalb .eq. 2) THEN
       FORALL (I=1:nsol,litx(I).le.ncols)
          rsurfv(I)=alvdr(litx(I))
          rsurfn(I)=alndr(litx(I))
       ENDFORALL
    ELSE
       !
       ! If direct beam albedos are not given then do the reverse
       ! calculate direct beam surface albedo
       !
       rvbl(1:nsol)=acos(cmu(1:nsol)) ! rvbl... solar zenith angle
       DO i=1,nsol
          rvdc(i)  =  -18.0_r8 * (0.5_r8 * pai - rvbl(i)) / pai
          rvbc(i)  =  exp(rvdc(i))
       ENDDO
       DO i=1,nsol
          rvdc(i)  = (agv(i) - 0.054313_r8) / 0.945687_r8
          rndc(i)  = (agn(i) - 0.054313_r8) / 0.945687_r8
          rsurfv(i) = rvdc(i)+(1.0-rvdc(i))*rvbc(i)
          rsurfn(i) = rndc(i)+(1.0-rndc(i))*rvbc(i)
       ENDDO
       DO i=1,ncols
          alvdr(i) = 0.0_r8
          alndr(i) = 0.0_r8
       ENDDO
       FORALL (I=1:nsol) alvdr(litx(I))=rsurfv(I)
       FORALL (I=1:nsol) alndr(litx(I))=rsurfn(I)
    ENDIF
    !
    ! CMU.......COSINE OF SOLAR ZENITH ANGLE AT DLGP
    !
    DO i=1,nsol
       scosz(i) = s0 * cmu(i)  ! DOWNWARD SOLAR FLUX AT TOP
    ENDDO
    !
    ! Transform scosz(nsol) to swinc(ncols) at all lgp
    !
    FORALL(I=1:nsol) swinc(litx(I))=scosz(I)
    !
    ! Calculate solar fluxes
    ! Calls cloudy to calculate:
    !   dsclr,sl,aclr,rvbl,rvdl,rnbl,rndl
    !   dscld,sc,acld,rvbc,rvdc,rnbc,rndc
    ! The values are packed at the begining of the arrays.
    ! Instead of occupying 1..ncols, they cover only the range 1..nsol
    !

    ! Set cloud amount as Maximum cloudiness
    css=max(ccu,css)

    IF (icld.eq.1) THEN

       e0  (1:nsol,1:kmax+1) = 0.05_r8
       bitc(1:nsol,1:kmax+1) = (ta(1:nsol,1:kmax+1).lt.253.0_r8).and.(ccu(1:nsol,1:kmax+1).eq.0.0_r8)

       ! IF BITC=.TRUE. EO=0.025_r8
       WHERE (bitc(1:nsol,1:kmax+1)) e0(1:nsol,1:kmax+1)=0.025_r8

       ! the extra cloud fraction idea from ncar is not used with clirad
       ! because clirad properly acounts for combination between different
       ! layers of clouds
       !WHERE(css.gt.0.0_r8) e0=e0*dp*css
       WHERE(css(1:nsol,1:kmax+1).gt.0.0_r8) e0(1:nsol,1:kmax+1)=e0(1:nsol,1:kmax+1)*dp(1:nsol,1:kmax+1)

    ELSE IF(icld.eq.4) THEN

       ! Prepare tau, fice, rel e rei
       DO k=1,kmax
          DO i=1,nsol
             tauk  (i,k+1) = taud(litx(i),k)
             frcice(i,k+1) = fice(litx(i),k)
             radice(i,k+1) = rei (litx(i),k)
             radliq(i,k+1) = rel (litx(i),k)
          END DO
       END DO
       tauk  (1:nsol,1)=0.0_r8
       frcice(1:nsol,1)=frcice(1:nsol,2)
       radice(1:nsol,1)=radice(1:nsol,2)
       radliq(1:nsol,1)=radliq(1:nsol,2)

       ! the extra cloud fraction idea from ncar is not used with clirad
       !e0(1:nsol,1:kmax+1) = sqrt(css(1:nsol,1:kmax+1))*tauk(1:nsol,1:kmax+1)
       e0(1:nsol,1:kmax+1) = tauk(1:nsol,1:kmax+1)
    ELSE
       WRITE(nfprt,*) 'error! icld must be 1 or 4 with Clirad-sw-m '
       STOP
    ENDIF

    !
    ! Call subroutine cloudy to calculate all-sky fluxes
    !

    CALL cloudy( &
         s0,    co2val*1.0E-6_r8,   nsol,   kmax+1,   &
         pu(1:nsol,1:kmax+2), ta(1:nsol,1:kmax+1), wa(1:nsol,1:kmax+1), oa(1:nsol,1:kmax+1), &
         cmu(1:nsol),  rsurfv(1:nsol), agv(1:nsol), rsurfn(1:nsol), agn(1:nsol),   &
         dscld(1:nsol), sc(1:nsol),  acld(1:nsol,1:kmax+1), rvbc(1:nsol), rvdc(1:nsol), rnbc(1:nsol), rndc(1:nsol),  &
         dsclr(1:nsol), sl(1:nsol),  aclr(1:nsol,1:kmax+1), rvbl(1:nsol), rvdl(1:nsol), rnbl(1:nsol), rndl(1:nsol),  &
         e0(1:nsol,1:kmax+1), css(1:nsol,1:kmax+1),  ps(1:nsol),   &
         dmask(1:nsol), frcice(1:nsol,1:kmax+1), radliq(1:nsol,1:kmax+1), radice(1:nsol,1:kmax+1),icld)
    !
    ! SET SOLAR FLUXES IN ALL GRID POINTS
    ! All values are nsol-packed and need to be unpacked
    ! This is done by copying values from positions (1:nsol) to
    ! positions litx(1:nsol).
    !
    FORALL(I=1:nsol)
       ! clear
       ssclr (litx(I))=sl   (I)
       dswclr(litx(I))=dsclr(I)
       radvbl(litx(I))=rvbl (I)
       radvdl(litx(I))=rvdl (I)
       radnbl(litx(I))=rnbl (I)
       radndl(litx(I))=rndl (I)

       ! cloudy
       ss    (litx(I))=sc   (I)
       dswtop(litx(I))=dscld(I)
       radvbc(litx(I))=rvbc (I)
       radvdc(litx(I))=rvdc (I)
       radnbc(litx(I))=rnbc (I)
       radndc(litx(I))=rndc (I)
    ENDFORALL

    DO k=1,kmax
       DO i=1,nsol
          aslclr(litx(i),k)=aclr(i,k+1)
          asl   (litx(i),k)=acld(i,k+1)
       END DO
    END DO

    !
    ! Calculation of solar heating rate in k/s
    !
    DO k=1,kmax
       DO i=1,ncols
          IF(aslclr(i,k).lt.1.0e-22_r8) aslclr(i,k) = 0.0_r8
          IF(asl   (i,k).lt.1.0e-22_r8) asl   (i,k) = 0.0_r8

          aslclr   (i,k) = aslclr(i,k) * 1.1574e-5_r8
          asl      (i,k) = asl   (i,k) * 1.1574e-5_r8
       ENDDO
    ENDDO

  END SUBROUTINE cliradsw

  !
  ! Subroutine: CLOUDY
  !
  ! $Author: pkubota $
  ! Modifications: H.Barbosa 2005
  !
  ! Description:
  !
  !NEW! continental aerosol model is included
  !NEW! the k-distributions of Tarasova and Fomin (2000)
  !NEW! 28 layers
  !
  !this  is  the source  program  for  computing  solar fluxes  due  to
  !absorption  by water  vapor, ozone,  co2, o2,  clouds,  and aerosols
  !anddue to scattering by clouds, aerosols, and gases.
  !
  !this is a vectorized code.   it computes fluxes simultaneously for m
  !soundings.
  !
  !the meaning, units and DIMENSION  of the input and output parameters
  !are given in the SUBROUTINE sorad.
  !
  ! Inputs:
  !setsw (global)         Clirad
  !
  !ncld       ncld
  !ncols
  !kmax
  !
  !puu(ncols*(kmax+2))....level pressures mb       pl
  !taa(ncols*(kmax+1))....layer temp in K       ta
  !waa(ncols*(kmax+1))....layer specific humidity g/g      wa
  !oaa(ncols*(kmax+1))....layer ozone concentration g/g    oa
  !tauc(ncols*(kmax+1))...cloud optical depth
  !css(ncols*(kmax+1))....cloud amount       fcld
  !cmu(ncols).............cosine solar zenith angle        cosz
  !rsurfv(ncols)..........Vis Beam Albedo       rsuvbm
  !agv(ncols).............Vis Diffuse Albedo       rsuvdf
  !rsurfn(ncols)..........Nir Beam Albedo       rsirbm
  !agn(ncols).............Nir Diffuse Albedo       rsirdf
  ! psc   =  surface pressure   (mb)
  ! Outputs:
  !  dscld(ncols)    ABSORPTION IN THE CLOUDY ATMOSPHERE AND AT THE GROUND
  !  sc(ncols)    ABSORPTION AT THE GROUND IN CLOUDY CASE
  !acld(ncols*(kmax+1)) HEATING RATE (CLOUDY) in K/sec
  !rvbc(ncols)    VISIBLE BEAM  FLUXES (CLOUDY)
  !rvdc(ncols)    VISIBLE DIFFUSE FLUXES (CLOUDY)
  !rnbc(ncols)    NEAR-IR BEAM  FLUXES (CLOUDY)
  !rndc(ncols)    NEAR-IR DIFFUSE FLUXES  (CLOUDY)
  !
  !
  !
  !
  SUBROUTINE cloudy(s0,rco2,m,np,&
       pl,ta,wa,oa, &
       cosz,rsuvbm, rsuvdf,rsirbm,rsirdf,&
       dscld1,sc1,acld1,rvbc1,rvdc1,rnbc1,rndc1, &
       dsclr1,sl1,aclr1,rvbl1,rvdl1,rnbl1,rndl1, &
       tauc,csscgp, psc, &
       dmask,fice, rel, rei, icld)

    IMPLICIT NONE

    ! Parameters

    ! specify aerosol properties of continental (c) aerosols
    REAL(KIND=r8), PARAMETER, DIMENSION(11) :: tau_c = (/ &
         2.432_r8,2.1_r8,2.1_r8,1.901_r8,1.818_r8,1.76_r8,1.562_r8, &
         1.0_r8,0.568_r8, 0.281_r8,0.129_r8 /)

    REAL(KIND=r8), PARAMETER, DIMENSION(11) :: ssa_c = (/ &
         0.653_r8,0.78_r8,0.78_r8,0.858_r8,0.886_r8,0.892_r8,0.903_r8, &
         0.891_r8,0.836_r8,0.765_r8,0.701_r8 /)

    REAL(KIND=r8), PARAMETER, DIMENSION(11) :: asym_c = (/ &
         0.726_r8,0.686_r8,0.686_r8,0.666_r8,0.658_r8,0.656_r8,0.65_r8, &
         0.637_r8,0.632_r8,0.66_r8,0.776_r8 /)

    ! specify aerosol properties of maritime (m) aerosols

    REAL(KIND=r8), PARAMETER, DIMENSION(11) :: tau_m = (/ &
         1.33_r8,1.241_r8,1.241_r8,1.192_r8,1.173_r8,1.161_r8,1.117_r8, &
         1.00_r8,0.906_r8,0.799_r8,0.603_r8 /)

    REAL(KIND=r8), PARAMETER, DIMENSION(11) :: ssa_m = (/ &
         0.84_r8,0.921_r8,0.921_r8,0.962_r8,0.977_r8,0.98_r8,0.987_r8, &
         0.989_r8,0.987_r8,0.986_r8,0.861_r8 /)

    REAL(KIND=r8), PARAMETER, DIMENSION(11) :: asym_m = (/ &
         0.774_r8,0.757_r8,0.757_r8,0.746_r8,0.745_r8,0.744_r8,0.743_r8, &
         0.743_r8,0.756_r8,0.772_r8,0.802_r8 /)

    ! input variables
    INTEGER, INTENT(IN) :: m, np
    INTEGER, INTENT(IN) :: dmask (m) !sib-mask in DLGP
    INTEGER, INTENT(IN) :: icld ! new cloud microphysics

    REAL(KIND=r8), INTENT(IN) :: s0, rco2
    ! pl,ta,wa and oa are, respectively, the level pressure (mb), layer
    ! temperature (k), layer specific humidity (g/g), and layer ozone
    ! concentration (g/g)
    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np+1) :: pl
    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np) :: ta, wa, oa, tauc, csscgp
    REAL(KIND=r8), INTENT(IN), DIMENSION(m) :: cosz,rsuvbm,rsuvdf,rsirbm,rsirdf
    REAL(KIND=r8), INTENT(IN), DIMENSION(m) :: psc

    REAL(KIND=r8), intent(in), DIMENSION(m,np) :: fice, rel, rei
    ! output variables
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m) :: dscld1,sc1, rvbc1,rvdc1,rnbc1,rndc1
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m,np) :: acld1

    REAL(KIND=r8), INTENT(OUT), DIMENSION(m) :: dsclr1,sl1, rvbl1,rvdl1,rnbl1,rndl1
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m,np) :: aclr1

    ! local  variables
    REAL(KIND=r8), DIMENSION(m,np,11) :: taual,ssaal,asyal
    REAL(KIND=r8), DIMENSION(m,np,3)  :: taucld,reff
    REAL(KIND=r8), DIMENSION(m,np):: fcld,dzint,aotb_c,aotb_m
    REAL(KIND=r8), DIMENSION(m,np+1)  :: flx,flc,flx_d,flx_u,flc_d,flc_u
    REAL(KIND=r8), DIMENSION(m) :: fdiruv,fdifuv,fdirpar,fdifpar,fdirir,fdifir

    REAL(KIND=r8), DIMENSION(m) :: fdiruv_c,fdifuv_c,fdirpar_c,fdifpar_c,fdirir_c,fdifir_c

    !hmjb new indexes for high/mid/low clouds
    INTEGER :: ict,icb

    REAL(KIND=r8) :: topa(m),hzmask(m,np),heat
    INTEGER :: i,k,ib

    ! Initialize local vectors and output variables

    ! subroutine starts here
    dscld1=0.0_r8
    sc1=0.0_r8
    rvbc1=0.0_r8
    rvdc1=0.0_r8
    rnbc1=0.0_r8
    rndc1=0.0_r8
    acld1=0.0_r8

    dsclr1=0.0_r8
    sl1=0.0_r8
    rvbl1=0.0_r8
    rvdl1=0.0_r8
    rnbl1=0.0_r8
    rndl1=0.0_r8
    aclr1=0.0_r8

    ! specify level indices separating high clouds from middle clouds
    ! (ict), and middle clouds from low clouds (icb).  this levels
    ! correspond to 400mb and 700 mb roughly.

    ! CPTEC-GCM works in sigma levels, hence in all columns the same
    ! layer will correspond to 0.4 and 0.7.Therefore, search is
    ! done only in the 1st column
    DO k=1,np
       IF (pl(1,k)/psc(1).le.0.4_r8.and.pl(1,k+1)/psc(1).gt.0.4_r8) ict=k
       IF (pl(1,k)/psc(1).le.0.7_r8.and.pl(1,k+1)/psc(1).gt.0.7_r8) icb=k
    ENDDO

    ! specify cloud optical thickness (taucld), amount (fcld), effective
    ! particle size (reff).cloud ice (index 1), liquid (index 2), and
    ! rain (index 3) are allowed to co-exit in a layer.
    ! cwc is the cloud ice/water concentration. if cldwater=.true.,
    ! taucld is computed from cwc and reff.  if cldwater=.false.,
    ! taucld is an input parameter, and cwc is irrelevent
    !m,np,3
    IF (icld.eq.1) THEN
       DO k=1,np
          DO i=1,m
             taucld(i,k,1)=0.0_r8
             taucld(i,k,2)=tauc(i,k)
             reff  (i,k,1) = 80.0_r8   ! ice particles
             reff  (i,k,2) = 5.25_r8   ! water particles
          END DO
       END DO
    ELSEIF (icld.eq.4) THEN
       DO k=1,np
          DO i=1,m
             taucld(i,k,1)=tauc(i,k)*fice(i,k)
             taucld(i,k,2)=tauc(i,k)*(1.0_r8-fice(i,k))
             reff  (i,k,1) = rei(i,k)   ! ice particles
             reff  (i,k,2) = rel(i,k)   ! water particles
          END DO
       END DO
    ELSE
       WRITE(nfprt,*) 'error! icld must be 1 or 4 with Clirad-sw-m '
       STOP
    ENDIF
    DO k=1,np
       DO i=1,m
          taucld(i,k,3)  = 0.0_r8 ! no droplets
          reff  (i,k,3)  = 0.0_r8 ! no droplets
          fcld  (i,k)    = csscgp(i,k)  ! cloud field
       END DO
    END DO

    ! calculation of background aerosol optical depth profile: aotb(m,np)

    ! calculation of layer depth in km
    DO i=1,m
       DO k=1,np
          dzint(i,k)=0.0660339_r8*(log10(pl(i,k+1))-log10(pl(i,k)))*ta(i,k)
       ENDDO
    ENDDO

    ! calculation of number of layers in 2 km depth (nta)
    !hmjb Now we save, for each (i,k), a number hzmask.
    !     This number is 1, if bottom of layer below 2km
    !     This number is 0, if bottom of layer above 2km
    !  I did this because it is faster to multiply by this
    !  matrix than to do a loop in (i) with two loops in
    !  k inside (1:2km and 2km:ground).

    hzmask=0.0_r8
    DO i=1,m
       k=np+1
       topa(i)=0.0_r8
       DO while (topa(i).lt.2.0_r8)
          k=k-1
          topa(i)=topa(i)+dzint(i,k)
       ENDDO
       hzmask(i,k:np)=1.0_r8
    ENDDO

    ! background aerosol profile with optical depth
    ! extinction coefficient 0.1 or 0.5 km-1 in each layer

    !hmjb The total column aerosol (0.22 or 0.14) should be distributed
    !     over the first 2km. However, we see from the calculation above
    !     that we distribute the aersol from the first layer above 2km
    !     down to the ground. Therefore, we must consider that probably
    !     the height of this region will be more than 2km.
    ! I changed the loop above so that we keep track of the total
    !     height, in each column, of the levels where we will spread the
    !     aerosol. Now the distribution of the aerosol inside each layer.
    !     More than that, if we sum the total aerosol in the full column,
    !     it will add to the stipulated value.
    DO k=1,np
       DO i=1,m
          aotb_c(i,k)=asolc*dzint(i,k)*hzmask(i,k)/2.0_r8
          aotb_m(i,k)=asolm*dzint(i,k)*hzmask(i,k)/2.0_r8
          !need more testing
          !new   aotb_c(i,:)=asolc*dzint(i,:)*hzmask(i,:)/topa(i)
          !new   aotb_m(i,:)=asolm*dzint(i,:)*hzmask(i,:)/topa(i)
       END DO
    END DO

    ! specify aerosol optical thickness (taual), single-scattering
    ! albedo (ssaal), and asymmetry factor (asyal)
    ! nta is top level of aerosol layer over the ground

    DO k=1,np
       DO ib=1,11
          DO i=1,m
             IF (dmask(i).gt.0.and.dmask(i).le.12) THEN
                ! sibmask=1..12 is land with different vegetation types
                taual(i,k,ib) = tau_c(ib)*aotb_c(i,k)*hzmask(i,k)
                ssaal(i,k,ib) = ssa_c(ib)
                asyal(i,k,ib) = asym_c(ib)
             ELSE
                ! sibmask -1 or 0 means ice/water and 13 is permanent ice (greenland and antartic)
                taual(i,k,ib) = tau_m(ib)*aotb_m(i,k)*hzmask(i,k)
                ssaal(i,k,ib) = ssa_m(ib)
                asyal(i,k,ib) = asym_m(ib)
             END IF
          END DO
       END DO
    END DO

    ! compute solar fluxes

    CALL soradcld (m,np,pl,ta,wa,oa,rco2,  &
         taucld,reff,fcld,ict,icb,  &
         taual,ssaal,asyal,  &
         cosz,rsuvbm,rsuvdf,rsirbm,rsirdf,  &
         flx,flc,fdiruv,fdifuv,fdirpar,fdifpar,fdirir,fdifir,  &
         fdiruv_c,fdifuv_c,fdirpar_c,fdifpar_c,fdirir_c,fdifir_c,  &
         flx_d,flx_u,flc_d,flc_u)    ! new

    ! convert the units of flx and flc from fraction to w/m^2
    ! transfer to the global model fluxes

    DO i=1,m
       dscld1(i)=flx(i,1)*s0*cosz(i)
       sc1   (i)=flx(i,np+1)*s0*cosz(i)
       rvbc1 (i)=(fdiruv(i)+fdirpar(i))*s0*cosz(i)
       rvdc1 (i)=(fdifuv(i)+fdifpar(i))*s0*cosz(i)
       rnbc1 (i)=fdirir(i)*s0*cosz(i)
       rndc1 (i)=fdifir(i)*s0*cosz(i)
       !
       dsclr1(i)=flc(i,1)*s0*cosz(i)
       sl1   (i)=flc(i,np+1)*s0*cosz(i)
       rvbl1 (i)=(fdiruv_c(i)+fdirpar_c(i))*s0*cosz(i)
       rvdl1 (i)=(fdifuv_c(i)+fdifpar_c(i))*s0*cosz(i)
       rnbl1 (i)=fdirir_c(i)*s0*cosz(i)
       rndl1 (i)=fdifir_c(i)*s0*cosz(i)
    ENDDO


    ! compute heating rates, c/day

    DO k=1,np
       DO i=1,m
          heat=8.4410_r8*s0*cosz(i)/(pl(i,k+1)-pl(i,k))
          aclr1(i,k)=(flc(i,k)-flc(i,k+1))*heat
          acld1(i,k)=(flx(i,k)-flx(i,k+1))*heat
       ENDDO
    ENDDO

  END SUBROUTINE cloudy


  ! ==============
  !
  !  clirad-sw
  !
  ! ==============
  !
  ! Subroutine: Soradcld
  !
  ! $Author: pkubota $
  ! Modifications: T. Tarasova, 2005
  ! Modifications: H. Barbosa, 2005
  !
  ! Description:
  !
  !following  the nasa  technical  memorandum (nasa/tm-1999-104606,vol.
  !15) of chou and suarez (1999), this routine computes solarfluxes due
  !to absorption by water vapor, ozone, co2, o2, clouds,andaerosols and
  !due to scattering by clouds, aerosols, and gases.
  !
  !this code computes fluxes simultaneously for m soundings.
  !
  !cloud ice, liquid,  and rain particles are allowed  to co-exist in a
  !layer.
  !
  !there is an option of  providing either cloud ice/water mixing ratio
  !(cwc) or optical thickness (taucld).  if the former is provided, set
  !cldwater=.true.,  and taucld  is computed  from  cwc and  reff as  a
  !function  of  spectra band.   otherwise,  set cldwater=.false.,  and
  !specify taucld, independent of spectral band.
  !
  !if  no information  is available  for the  effective  particle size,
  !reff, default values of 10 micron for liquid water and 75 micron for
  !ice may be  used.  the size of raindrops,  reff(3), is irrelevant in
  !this code. it can be set to any values.  for a clear layer, reff can
  !be set to any values except zero.
  !
  !the  maximum-random   assumption  is  appliedfor  treating  cloud
  !overlapping. clouds  are grouped into  high, middle, and  low clouds
  !separated  by  the level  indices  ict  and  icb.  for  detail,  see
  !SUBROUTINE "cldscale".
  !
  !in   a  high   spatial-resolution   atmospheric  model,   fractional
  !cloudcover might be  computed to be either 0 or 1.   in such a case,
  !scaling  of the  cloud optical  thickness isnot  necessary,  and the
  !computation   can  bemade  faster   by   setting  overcast=.true.
  !otherwise, set the option overcast=.false.
  !
  !aerosol optical  thickness, single-scattering albedo,  and asymmetry
  !factor can be specified as functions of height and spectral band.
  !
  ! Inputs: units        size
  !
  !      m: number of soundings n/d      1
  !     np: number of atmospheric layers n/d      1
  !     pl: level pressure  mb      m*(np+1)
  !     ta: layer temperature k      m*np
  !     wa: layer specific humidity gm/gm        m*np
  !     oa: layer ozone concentration gm/gm        m*np
  !    co2: co2 mixing ratio by volume pppv      1
  !  overcast: option for scaling cloud optical thickness n/d      1
  !   "true"  = scaling is not required
  !   "fasle" = scaling is required
  !  cldwater: input option for cloud optical thickness n/d      1
  !   "true"  = taucld is provided
  !   "false" = cwp is provided
  !    cwc: cloud water mixing ratio gm/gm        m*np*3
  !   index 1 for ice particles
  !   index 2 for liquid drops
  !   index 3 for rain drops
  ! taucld: cloud optical thickness n/d      m*np*3
  !   index 1 for ice particles
  !   index 2 for liquid drops
  !   index 3 for rain drops
  !   reff: effective cloud-particle size   micrometer   m*np*3
  !   index 1 for ice paticles
  !   index 2 for liquid drops
  !   index 3 for rain drops
  !   fcld: cloud amount fraction     m*np
  !    ict: level index separating high and middle clouds   n/d      m
  !    icb: level indiex separating middle and low clouds   n/d      m
  !  taual: aerosol optical thickness n/d      m*np*11
  !  ssaal: aerosol single-scattering albedo n/d      m*np*11
  !  asyal: aerosol asymmetry factor n/d      m*np*11
  ! in the uv region :
  !    index  1 for the 0.175-0.225 micron band
  !    index  2 for the 0.225-0.245; 0.260-0.280 micron band
  !    index  3 for the 0.245-0.260 micron band
  !    index  4 for the 0.280-0.295 micron band
  !    index  5 for the 0.295-0.310 micron band
  !    index  6 for the 0.310-0.320 micron band
  !    index  7 for the 0.325-0.400 micron band
  ! in the par region :
  !    index  8 for the 0.400-0.700 micron band
  ! in the infrared region :
  !    index  9 for the 0.700-1.220 micron band
  !    index 10 for the 1.220-2.270 micron band
  !    index 11 for the 2.270-10.00 micron band
  !   cosz: cosine of solar zenith angle       n/d    m
  ! rsuvbm: uv+vis sfc albedo for beam rad for wl<0.7 micron      fraction     m
  ! rsuvdf: uv+vis sfc albedo for diffuse rad  for wl<0.7 micron  fraction     m
  ! rsirbm: ir sfc albedo for beam rad for wl>0.7 micron       fraction     m
  ! rsirdf: ir sfc albedo for diffuse rad         fraction     m
  !
  ! Outputs: (updated parameters)
  !
  !    flx: all-sky   net downward flux       fraction     m*(np+1)
  !    flc: clear-sky net downward flux       fraction     m*(np+1)
  ! fdiruv: all-sky direct  downward uv (.175-.4 micron) flux sfc fraction     m
  ! fdifuv: all-sky diffuse downward uv flux at the surface       fraction     m
  !fdirpar: all-sky direct  downward par (.4-.7 micron) flux sfc  fraction     m
  !fdifpar: all-sky diffuse downward par flux at the surface      fraction     m
  ! fdirir: all-sky direct  downward ir (.7-10 micron) flux sfc   fraction     m
  ! fdifir: all-sky diffuse downward ir flux at the surface ()    fraction     m
  !
  !
  !
  ! NOTES
  !
  ! (1) the unit of output fluxes (flx,flc,etc.) is fraction of the
  !     insolation at the top of the atmosphere.  therefore, fluxes
  !     are the output fluxes multiplied by the extra-terrestrial solar
  !     flux and the cosine of the solar zenith angle.
  ! (2) pl( ,1) is the pressure at the top of the model, and
  !     pl( ,np+1) is the surface pressure.
  ! (3) the pressure levels ict and icb correspond approximately
  !     to 400 and 700 mb.
  !
  !  if coding errors are found, please notify ming-dah chou at
  !  chou@climate.gsfc.nasa.gov
  !
  !
  !
  !
  SUBROUTINE Soradcld (m,np,pl,ta,wa,oa,co2,  &
       taucld,reff,fcld,ict,icb,  &
       taual,ssaal,asyal,  &
       cosz,rsuvbm,rsuvdf,rsirbm,rsirdf,  &
       flx,flc,fdiruv,fdifuv,fdirpar,fdifpar,fdirir,fdifir,  &
       fdiruv_c,fdifuv_c,fdirpar_c,fdifpar_c,fdirir_c,fdifir_c,  &
       flx_d,flx_u,flc_d,flc_u)

    IMPLICIT NONE
    INTEGER i,j


    ! parameters for co2 transmission tables
    INTEGER, PARAMETER :: nu=43
    INTEGER, PARAMETER :: nw=37
    INTEGER, PARAMETER :: nx=62
    INTEGER, PARAMETER :: ny=101

    ! cah is the co2 absorptance in band 10
    REAL(KIND=r8), DIMENSION(nx,ny) :: coa
    INCLUDE "coa.data90"

    ! coa is the co2 absorptance in strong absorption regions of band 11
    REAL(KIND=r8), DIMENSION(nu,nw) :: cah
    INCLUDE "cah.data90"

    ! input variables
    INTEGER, INTENT(IN) :: m,np,ict,icb

    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np,11) :: taual, ssaal, asyal
    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np,3) :: taucld, reff
    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np+1) :: pl
    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np) :: ta, wa, oa, fcld
    REAL(KIND=r8), INTENT(IN), DIMENSION(m) :: cosz, rsuvbm, rsuvdf, rsirbm, rsirdf
    REAL(KIND=r8), INTENT(IN) :: co2

    ! output variables
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m,np+1) :: flx,flc,flx_d,flx_u, flc_d,flc_u
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m) :: fdiruv,fdifuv,fdirpar,fdifpar,fdirir,fdifir
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m) :: fdiruv_c,fdifuv_c,fdirpar_c,fdifpar_c,fdirir_c,fdifir_c

    ! local  variables
    INTEGER :: k,ntop
    INTEGER, DIMENSION(m) :: nctop

    REAL(KIND=r8)    :: x, w1,dw,u1,du
    REAL(KIND=r8), DIMENSION(m) :: snt, cnt
    REAL(KIND=r8), DIMENSION(m,np) :: dp, wh, oh, scal
    REAL(KIND=r8), DIMENSION(m,np+1) :: swu, swh, so2, df

    ! initialize local vectors and output variables
    ! subroutine starts here
    DO i=1,m
       swh(i,1)=0.0_r8
       so2(i,1)=0.0_r8
       snt(i)=1.0_r8/cosz(i) ! snt is the secant of the solar zenith angle
    ENDDO

    DO k=1,np
       DO i=1,m

          ! compute layer thickness. indices for the surface level and
          ! surface layer are np+1 and np, respectively.
          dp(i,k)=pl(i,k+1)-pl(i,k)
          !
          ! compute scaled water vapor amount following eqs. (3.3) and (3.5)
          ! unit is g/cm**2
          !
          scal(i,k)=dp(i,k)*(0.5_r8*(pl(i,k)+pl(i,k+1))/300.0_r8)**0.8_r8
          wh(i,k)=1.02_r8*wa(i,k)*scal(i,k)*(1.0_r8+0.00135_r8*(ta(i,k)-240.0_r8))+1.e-11_r8
          swh(i,k+1)=swh(i,k)+wh(i,k)
          !
          ! compute ozone amount, unit is (cm-atm)stp
          ! the number 466.7 is the unit conversion factor
          ! from g/cm**2 to (cm-atm)stp
          !
          oh(i,k)=1.02_r8*oa(i,k)*dp(i,k)*466.7_r8 +1.0e-11_r8
          !
          ! compute layer cloud water amount (gm/m**2)
          ! the index is 1 for ice crystals, 2 for liquid drops, and
          ! 3 for rain drops
          !
          !     x=1.02*10000.*dp(i,k)
          !     cwp(i,k,1)=x*cwc(i,k,1)
          !     cwp(i,k,2)=x*cwc(i,k,2)
          !     cwp(i,k,3)=x*cwc(i,k,3)
       ENDDO
    ENDDO

    ! initialize fluxes for all-sky (flx), clear-sky (flc), and
    ! flux reduction (df)

    DO k=1,np+1
       DO i=1,m
          flx(i,k)=0.0_r8
          flx_d(i,k)=0.0_r8    ! new
          flx_u(i,k)=0.0_r8    ! new
          flc(i,k)=0.0_r8
          flc_d(i,k)=0.0_r8    ! new
          flc_u(i,k)=0.0_r8    ! new
          df(i,k)=0.0_r8
       ENDDO
    ENDDO

    ! compute solar uv and par fluxes
    CALL soluvcld (m,np,wh,oh,dp,  &
         taucld,reff,ict,icb,fcld,cosz,  &
         taual,ssaal,asyal,rsuvbm,rsuvdf,  &
         flx,flc,fdiruv,fdifuv,fdirpar,fdifpar,  &
         fdiruv_c,fdifuv_c,fdirpar_c,fdifpar_c,  &
         flx_d,flx_u,flc_d,flc_u)  ! new

    ! compute and update solar ir fluxes
    CALL solircld (m,np,wh,dp,  &
         taucld,reff,ict,icb,fcld,cosz,  &
         taual,ssaal,asyal,rsirbm,rsirdf,  &
         flx,flc,fdirir,fdifir,fdirir_c,fdifir_c,  &
         flx_d,flx_u,flc_d,flc_u)  ! new

    ! compute pressure-scaled o2 amount following eq. (3.5) with
    !     f=1. unit is (cm-atm)stp.
    !     the constant 165.22 equals (1000/980)*23.14%*(22400/32)

    DO i=1,m
       cnt(i)=165.22_r8*snt(i)
    ENDDO

    DO k=1,np
       DO i=1,m
          so2(i,k+1)=so2(i,k)+scal(i,k)*cnt(i)
       ENDDO
    ENDDO

    ! compute flux reduction due to oxygen following eq. (3.18)
    !     the constant 0.0633 is the fraction of insolation contained
    !     in the oxygen bands

    DO k=2,np+1
       DO i=1,m
          x=so2(i,k)
          df(i,k)=0.0633_r8*(1.0_r8-exp(-0.000145_r8*sqrt(x)))
       ENDDO
    ENDDO

    ! for solar heating due to co2

    DO i=1,m
       cnt(i)=co2*snt(i)
    ENDDO

    ! scale co2 amounts following eq. (3.5) with f=1.
    !     unit is (cm-atm)stp.
    !     the constant 789 equals (1000/980)*(44/28.97)*(22400/44)

    DO k=1,np
       DO i=1,m
          x=789.0_r8*cnt(i)
          so2(i,k+1)=so2(i,k)+x*scal(i,k)+1.0e-11_r8
       ENDDO
    ENDDO
    ! for co2 absorption in band 10 where absorption due to
    !     water vapor and co2 are both moderate

    u1=-3.0_r8
    du=0.15_r8
    w1=-4.0_r8
    dw=0.15_r8

    ! so2 and swh are the co2 and water vapor amounts integrated
    !     from the top of the atmosphere

    DO k=2,np+1
       DO i=1,m
          swu(i,k)=log10(so2(i,k))
          swh(i,k)=log10(swh(i,k)*snt(i))
       ENDDO
    ENDDO

    ! df is the updated flux reduction given by the second term on the
    !     right-hand-side of eq. (3.24) divided by so

    CALL rflx(m,np,swu,u1,du,nu,swh,w1,dw,nw,cah,df)

    ! for co2 absorption in band 11 where the co2 absorption has
    !     a large impact on the heating of middle atmosphere.

    u1=0.000250_r8
    du=0.000050_r8
    w1=-2.0_r8
    dw=0.05_r8

    DO i=1,m
       swu(i,1)=co2*snt(i)
    ENDDO

    ! co2 mixing ratio is independent of space

    DO k=2,np+1
       DO i=1,m
          swu(i,k)=swu(i,1)
       ENDDO
    ENDDO

    ! swh is the logarithm of pressure

    DO k=2,np+1
       DO i=1,m
          swh(i,k)=log10(pl(i,k))
       ENDDO
    ENDDO

    ! df is the updated flux reduction derived from the table given by
    !     eq. (3.19)

    CALL rflx(m,np,swu,u1,du,nx,swh,w1,dw,ny,coa,df)

    ! adjustment for the effect of o2 and co2 on clear-sky fluxes.
    !     both flc and df are positive quantities

    DO k=1,np+1
       DO i=1,m
          flc(i,k)=flc(i,k)-df(i,k)
          flc_d(i,k)=flc_d(i,k)-df(i,k)   ! new
       ENDDO
    ENDDO

    ! adjustment for the direct downward flux (CLEAR)

    DO i=1,m
       fdirir_c(i)=fdirir_c(i)-df(i,np+1)
       IF (fdirir_c(i) .lt. 0.0_r8) fdirir_c(i)=0.0_r8
    ENDDO

    ! identify top cloud-layer

    DO i=1,m
       nctop(i)=np+1
    ENDDO

    DO k=1,np
       DO i=1,m
          IF (fcld(i,k).gt.0.02_r8 .and. nctop(i).eq.np+1) THEN
             nctop(i)=k
          ENDIF
       ENDDO
    ENDDO

    DO i=1,m
       !hmjb ERROR
       !     ntop=nctop(m)
       ntop=nctop(i)
       !
       ! adjust fluxes above clouds following eq. (6.17)
       DO k=1,ntop
          flx(i,k)=flx(i,k)-df(i,k)
          flx_d(i,k)=flx_d(i,k)-df(i,k)   ! new
       ENDDO
       ! adjust fluxes below cloud top following eq. (6.18)
       IF (ntop.lt.np+1) THEN
          DO k=ntop+1,np+1
             df(i,k)=df(i,k)*(flx(i,k)/flc(i,k))
             flx(i,k)=flx(i,k)-df(i,k)
             flx_d(i,k)=flx_d(i,k)-df(i,k)! new
          ENDDO
       ENDIF
    ENDDO
    ! adjustment for the direct downward flux
    DO i=1,m
       fdirir(i)=fdirir(i)-df(i,np+1)
       IF (fdirir(i) .lt. 0.0_r8) fdirir(i)=0.0_r8
    ENDDO

  END SUBROUTINE Soradcld

  !
  ! Subroutine: SOLIRCLD
  !
  ! $Author: pkubota $
  ! Modifications: T. Tarasova, 2005
  ! Modifications: H. Barbosa, 2005
  !
  ! Description:
  !  compute solar flux in the infrared region. the spectrum is divided
  !into three bands:
  !
  !       band   wavenumber(/cm)  wavelength (micron)
  !       1( 9)14280-8200   0.70-1.22
  !       2(10) 8200-4400   1.22-2.27
  !       3(11) 4400-1000   2.27-10.0
  !
  ! Inputs: units        size
  !
  !      m: number of soundings n/d      1
  !     np: number of atmospheric layers n/d      1
  !     wh: layer scaled-water vapor content gm/cm^2      m*np
  !  overcast: option for scaling cloud optical thickness n/d      1
  !   "true"  = scaling is not required
  !   "fasle" = scaling is required
  !  cldwater: input option for cloud optical thickness n/d      1
  !   "true"  = taucld is provided
  !   "false" = cwp is provided
  !    cwp: cloud water amount gm/m**2      m*np*3
  !   index 1 for ice particles
  !   index 2 for liquid drops
  !   index 3 for rain drops
  ! taucld: cloud optical thickness n/d      m*np*3
  !   index 1 for ice particles
  !   index 2 for liquid drops
  !   index 3 for rain drops
  !   reff: effective cloud-particle size   micrometer   m*np*3
  !   index 1 for ice paticles
  !   index 2 for liquid drops
  !   index 3 for rain drops
  !    ict: level index separating high and middle clouds   n/d      m
  !    icb: level indiex separating middle and low clouds   n/d      m
  !   fcld: cloud amount fraction     m*np
  !  taual: aerosol optical thickness n/d      m*np*11
  !  ssaal: aerosol single-scattering albedo n/d      m*np*11
  !  asyal: aerosol asymmetry factor n/d      m*np*11
  ! rsirbm: near ir surface albedo for beam radiation fraction     m
  ! rsirdf: near ir surface albedo for diffuse radiation fraction     m
  !
  ! Outputs: (updated parameters)
  !
  !    flx: all-sky   net downward flux    fraction  m*(np+1)
  !    flc: clear-sky net downward flux    fraction  m*(np+1)
  ! fdirir: all-sky direct  downward ir flux at the surface    fraction  m
  ! fdifir: all-sky diffuse downward ir flux at the surface    fraction  m
  !
  ! Local Variables
  !
  !  tauclb: scaled cloud optical thickness for beam radiation    n/d   m*np
  !  tauclf: scaled cloud optical thickness for diffuse radiation n/d   m*np
  !
  !
  !
  SUBROUTINE Solircld (m,np,wh,dp,  &
       taucld,reff,ict,icb,fcld,cosz,  &
       taual,ssaal,asyal,  &
       rsirbm,rsirdf,flx,flc,fdirir,fdifir,fdirir_c,fdifir_c,  &
       flx_d,flx_u,flc_d,flc_u)   !  new

    IMPLICIT NONE

    ! parameters
    INTEGER, PARAMETER :: nk=10
    INTEGER, PARAMETER :: nband=3

    ! water vapor absorption coefficient for 10 k-intervals. unit: cm^2/gm (table 2)
    REAL(KIND=r8), PARAMETER, DIMENSION(nk) :: xk2 = (/    &
         0.0010_r8, 0.0133_r8, 0.0422_r8, 0.1334_r8, 0.4217_r8, &
         1.334_r8,  5.623_r8,  31.62_r8,  177.8_r8,  1000.0_r8 /)

    ! water vapor k-distribution function,
    ! the sum of hk is 0.52926. unit: fraction (table 2)
    ! --- new coefficients (tarasova and fomin, 2000)
    REAL(KIND=r8), PARAMETER, DIMENSION(nband,nk) :: hk2 = RESHAPE( &
         SOURCE = (/ &
         0.19310_r8, 0.06924_r8, 0.00310_r8, 0.05716_r8, 0.01960_r8, 0.00637_r8, &
         0.02088_r8, 0.00795_r8, 0.00526_r8, 0.02407_r8, 0.01716_r8, 0.00641_r8, &
         0.01403_r8, 0.01118_r8, 0.00542_r8, 0.00582_r8, 0.01377_r8, 0.00312_r8, &
         0.00246_r8, 0.02008_r8, 0.00368_r8, 0.00163_r8, 0.00265_r8, 0.00346_r8, &
         0.00101_r8, 0.00282_r8, 0.00555_r8, 0.00041_r8, 0.00092_r8, 0.00098_r8 /) , &
         SHAPE = (/nband , nk/) )

    ! ry is the extinction coefficient for rayleigh scattering. unit: /mb (table 3)
    REAL(KIND=r8), PARAMETER, DIMENSION(nband) :: ry2 = (/ 0.0000156_r8, 0.0000018_r8, 0.000000_r8 /)

    ! coefficients for computing the extinction coefficients of
    ! ice, water, and rain particles (table 4)
    REAL(KIND=r8), PARAMETER, DIMENSION(nband,2) :: aib = RESHAPE( &
         SHAPE = (/ nband, 2 /), SOURCE = (/ &
         0.000333_r8, 0.000333_r8, 0.000333_r8, 2.52_r8,    2.52_r8,2.52_r8 /) )
    REAL(KIND=r8), PARAMETER, DIMENSION(nband,2) :: awb = RESHAPE( &
         SHAPE = (/ nband, 2 /), SOURCE = (/ &
         -0.0101_r8, -0.0166_r8, -0.0339_r8, 1.72_r8,    1.85_r8,2.16_r8 /) )
    REAL(KIND=r8), PARAMETER, DIMENSION(nband,2) :: arb = RESHAPE( &
         SHAPE = (/ nband, 2 /), SOURCE = (/ &
         0.00307_r8, 0.00307_r8, 0.00307_r8, 0.0_r8    , 0.0_r8    , 0.0_r8  /) )

    ! coefficients for computing the single-scattering co-albedo of
    !     ice, water, and rain particles (table 5)
    REAL(KIND=r8), PARAMETER, DIMENSION(nband,3) :: aia  = RESHAPE( &
         SHAPE = (/ nband, 3 /), SOURCE = (/ &
         -0.00000260_r8,  0.00215346_r8,  0.08938331_r8, &
         0.00000746_r8,  0.00073709_r8,  0.00299387_r8, &
         0.00000000_r8, -0.00000134_r8, -0.00001038_r8 /) )
    REAL(KIND=r8), PARAMETER, DIMENSION(nband,3) :: awa = RESHAPE( &
         SHAPE = (/ nband, 3 /), SOURCE = (/ &
         0.00000007_r8,-0.00019934_r8, 0.01209318_r8, &
         0.00000845_r8, 0.00088757_r8, 0.01784739_r8, &
         -0.00000004_r8,-0.00000650_r8,-0.00036910_r8 /) )
    REAL(KIND=r8), PARAMETER, DIMENSION(nband,3) :: ara = RESHAPE( &
         SHAPE = (/ nband, 3 /), SOURCE = (/ &
         0.029_r8,  0.342_r8,    0.466_r8, &
         0.0000_r8,  0.000_r8,    0.000_r8, &
         0.0000_r8,  0.000_r8,    0.000_r8 /) )

    ! coefficients for computing the asymmetry factor of
    !     ice, water, and rain particles (table 6)

    REAL(KIND=r8), PARAMETER, DIMENSION(nband,3) :: aig = RESHAPE(  &
         SHAPE = (/ nband, 3 /), SOURCE = (/ &
         0.74935228_r8, 0.76098937_r8, 0.84090400_r8, &
         0.00119715_r8, 0.00141864_r8, 0.00126222_r8, &
         -0.00000367_r8,-0.00000396_r8,-0.00000385_r8 /) )

    REAL(KIND=r8), PARAMETER, DIMENSION(nband,3) :: awg = RESHAPE( &
         SHAPE = (/ nband, 3 /), SOURCE = (/ &
         0.79375035_r8, 0.74513197_r8, 0.83530748_r8, &
         0.00832441_r8, 0.01370071_r8, 0.00257181_r8, &
         -0.00023263_r8,-0.00038203_r8, 0.00005519_r8 /) )

    REAL(KIND=r8), PARAMETER, DIMENSION(nband,3) :: arg = RESHAPE( &
         SHAPE = (/ nband, 3 /), SOURCE = (/ &
         0.891_r8,  0.948_r8,    0.971_r8, &
         0.0000_r8,  0.000_r8,    0.000_r8, &
         0.0000_r8,  0.000_r8,    0.000_r8 /) )

    ! input variables
    INTEGER, INTENT(IN) :: m,np,ict,icb
    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np,11) :: taual, ssaal, asyal
    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np,3) :: taucld, reff
    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np) :: fcld, wh, dp
    REAL(KIND=r8), INTENT(IN), DIMENSION(m) :: rsirbm, rsirdf, cosz

    ! output variables
    REAL(KIND=r8), INTENT(INOUT), DIMENSION(m,np+1) :: flx,flc,flx_d,flx_u, flc_d,flc_u
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m) :: fdirir,fdifir,fdirir_c,fdifir_c

    ! local  variables
    INTEGER :: i,k,ib,ik,iv
    INTEGER :: ih1,ih2,im1,im2,is1,is2
    REAL(KIND=r8)    :: taurs,tauwv
    REAL(KIND=r8)    :: taux,reff1,reff2,w1,w2,w3,g1,g2,g3

    REAL(KIND=r8), DIMENSION(m) :: dsm, fsdir, fsdif, fsdir_c, fsdif_c, asyclt, ssaclt
    REAL(KIND=r8), DIMENSION(m,3) :: cc
    REAL(KIND=r8), DIMENSION(m,np) :: tauclb,tauclf,asycl,tautof,ssatof,asytof,ssacl
    REAL(KIND=r8), DIMENSION(m,np) :: tausto,ssatau,asysto,tautob,ssatob,asytob
    REAL(KIND=r8), DIMENSION(m,np) :: dum,rrt,ttt,tdt,rst,tst
    REAL(KIND=r8), DIMENSION(m,np+1) :: fall,fclr,fall_d,fall_u,fclr_d,fclr_u
    REAL(KIND=r8), DIMENSION(m,np+1,2) :: rr,tt,td,rs,ts

    ! Initialize local vectors
    ! Subroutine starts here
    !
    ! initialize surface fluxes, reflectances, and transmittances.
    ! the reflectance and transmittance of the clear and cloudy portions
    ! of a layer are denoted by 1 and 2, respectively.
    ! cc is the maximum cloud cover in each of the high, middle, and low
    ! cloud groups.
    ! 1/dsm=1/cos(53)=1.66

    dsm=0.602_r8
    fdirir=0.0_r8
    fdifir=0.0_r8
    fdirir_c=0.0_r8
    fdifir_c=0.0_r8
    rr(1:m,np+1,1)=rsirbm(1:m)
    rr(1:m,np+1,2)=rsirbm(1:m)
    rs(1:m,np+1,1)=rsirdf(1:m)
    rs(1:m,np+1,2)=rsirdf(1:m)
    td=0.0_r8
    tt=0.0_r8
    ts=0.0_r8
    cc=0.0_r8

    ! integration over spectral bands
    DO ib=1,nband
       iv=ib+8
       ! scale cloud optical thickness in each layer from taucld (with
       !     cloud amount fcld) to tauclb and tauclf (with cloud amount cc).
       !     tauclb is the scaled optical thickness for beam radiation and
       !     tauclf is for diffuse radiation.
       CALL cldscale(m,np,cosz,fcld,taucld,ict,icb,cc,tauclb,tauclf)
       ! compute cloud single scattering albedo and asymmetry factor
       !     for a mixture of ice and liquid particles.
       !     eqs.(4.6)-(4.8), (6.2)-(6.4)
       DO k=1,np
          DO i=1,m
             ssaclt(i)=0.99999_r8
             asyclt(i)=1.0_r8

             taux=taucld(i,k,1)+taucld(i,k,2)+taucld(i,k,3)
             IF (taux.gt.0.02_r8 .and. fcld(i,k).gt.0.01_r8) THEN
                reff1=min(reff(i,k,1),130.0_r8)
                reff2=min(reff(i,k,2),20.0_r8)

                w1=(1.0_r8-(aia(ib,1)+(aia(ib,2)+ &
                     aia(ib,3)*reff1)*reff1))*taucld(i,k,1)
                w2=(1.0_r8-(awa(ib,1)+(awa(ib,2)+ &
                     awa(ib,3)*reff2)*reff2))*taucld(i,k,2)
                w3=(1.0_r8- ara(ib,1))*taucld(i,k,3)
                ssaclt(i)=(w1+w2+w3)/taux

                g1=(aig(ib,1)+(aig(ib,2)+aig(ib,3)*reff1)*reff1)*w1
                g2=(awg(ib,1)+(awg(ib,2)+awg(ib,3)*reff2)*reff2)*w2
                g3= arg(ib,1)*w3
                asyclt(i)=(g1+g2+g3)/(w1+w2+w3)
             ENDIF
          ENDDO

          DO i=1,m
             ssacl(i,k)=ssaclt(i)
          ENDDO
          DO i=1,m
             asycl(i,k)=asyclt(i)
          ENDDO
       ENDDO

       ! integration over the k-distribution function
       DO ik=1,nk
          DO k=1,np
             DO i=1,m
                taurs=ry2(ib)*dp(i,k)
                tauwv=xk2(ik)*wh(i,k)
                ! compute clear-sky optical thickness, single scattering albedo,
                !and asymmetry factor. eqs.(6.2)-(6.4)
                tausto(i,k)=taurs+tauwv+taual(i,k,iv)+1.0e-8_r8
                ssatau(i,k)=ssaal(i,k,iv)*taual(i,k,iv)+taurs
                asysto(i,k)=asyal(i,k,iv)*ssaal(i,k,iv)*taual(i,k,iv)

                ! compute reflectance and transmittance of the clear portion of a layer
                tautob(i,k)=tausto(i,k)
                ssatob(i,k)=ssatau(i,k)/tautob(i,k)+1.0e-8_r8
                ssatob(i,k)=min(ssatob(i,k),0.999999_r8)
                asytob(i,k)=asysto(i,k)/(ssatob(i,k)*tautob(i,k))
             ENDDO
          ENDDO

          ! for direct incident radiation

          CALL deledd (m,np,tautob,ssatob,asytob,cosz,rrt,ttt,tdt)

          ! diffuse incident radiation is approximated by beam radiation with
          !  an incident angle of 53 degrees, eqs. (6.5) and (6.6)

          CALL deledd (m,np,tautob,ssatob,asytob,dsm,rst,tst,dum)

          DO k=1,np
             DO i=1,m
                rr(i,k,1)=rrt(i,k)
                tt(i,k,1)=ttt(i,k)
                td(i,k,1)=tdt(i,k)
                rs(i,k,1)=rst(i,k)
                ts(i,k,1)=tst(i,k)
             ENDDO
          ENDDO

          ! compute reflectance and transmittance of the cloudy portion of a layer
          DO k=1,np
             DO i=1,m
                ! for direct incident radiation. eqs.(6.2)-(6.4)
                tautob(i,k)=tausto(i,k)+tauclb(i,k)
                ssatob(i,k)=(ssatau(i,k)+ssacl(i,k)*tauclb(i,k)) &
                     /tautob(i,k)+1.0e-8_r8
                ssatob(i,k)=min(ssatob(i,k),0.999999_r8)
                asytob(i,k)=(asysto(i,k)+asycl(i,k)*ssacl(i,k)*tauclb(i,k)) &
                     /(ssatob(i,k)*tautob(i,k))

                ! for diffuse incident radiation
                tautof(i,k)=tausto(i,k)+tauclf(i,k)
                ssatof(i,k)=(ssatau(i,k)+ssacl(i,k)*tauclf(i,k)) &
                     /tautof(i,k)+1.0e-8_r8
                ssatof(i,k)=min(ssatof(i,k),0.999999_r8)
                asytof(i,k)=(asysto(i,k)+asycl(i,k)*ssacl(i,k)*tauclf(i,k)) &
                     /(ssatof(i,k)*tautof(i,k))
             ENDDO
          ENDDO

          ! for direct incident radiation

          CALL deledd (m,np,tautob,ssatob,asytob,cosz,rrt,ttt,tdt)

          ! diffuse incident radiation is approximated by beam radiation with
          !  an incident angle of 53 degrees, eqs.(6.5) and (6.6)

          CALL deledd (m,np,tautof,ssatof,asytof,dsm,rst,tst,dum)

          DO k=1,np
             DO i=1,m
                rr(i,k,2)=rrt(i,k)
                tt(i,k,2)=ttt(i,k)
                td(i,k,2)=tdt(i,k)
                rs(i,k,2)=rst(i,k)
                ts(i,k,2)=tst(i,k)
             ENDDO
          ENDDO
          ! flux calculations

          ! initialize clear-sky flux (fclr), all-sky flux (fall),
          !  and surface downward fluxes (fsdir and fsdif)
          !hmjb they are initialized inside cldfx()
          !     fclr   = 0.0_r8
          !     fall   = 0.0_r8
          !     fclr_d = 0.0_r8
          !     fclr_u = 0.0_r8
          !     fall_d = 0.0_r8
          !     fall_u = 0.0_r8
          !
          !     fsdir   = 0.0_r8
          !     fsdif   = 0.0_r8
          !     fsdir_c = 0.0_r8
          !     fsdif_c = 0.0_r8

          ! for clear- and all-sky fluxes
          !  the all-sky flux, fall is the summation inside the brackets
          !  of eq. (7.11)

          ih1=1
          ih2=2
          im1=1
          im2=2
          is1=1
          is2=2

          CALL cldflx (m,np,ict,icb,ih1,ih2,im1,im2,is1,is2,  &
               cc,rr,tt,td,rs,ts,fclr,fall,fsdir,fsdif, fsdir_c,fsdif_c, &
               fclr_d,fclr_u,fall_d,fall_u)! new

          ! flux integration following eq. (6.1)

          DO k=1,np+1
             DO i=1,m
                flx_d(i,k)=flx_d(i,k)+fall_d(i,k)*hk2(ib,ik) !new
                flx_u(i,k)=flx_u(i,k)+fall_u(i,k)*hk2(ib,ik) !new
                flx(i,k) = flx(i,k)+fall(i,k)*hk2(ib,ik)
             ENDDO

             DO i=1,m
                flc(i,k) = flc(i,k)+fclr(i,k)*hk2(ib,ik)
                flc_d(i,k)=flc_d(i,k)+fclr_d(i,k)*hk2(ib,ik) !new
                flc_u(i,k)=flc_u(i,k)+fclr_u(i,k)*hk2(ib,ik) !new
             ENDDO
          ENDDO

          ! compute downward surface fluxes in the ir region

          DO i=1,m
             fdirir(i) = fdirir(i)+fsdir(i)*hk2(ib,ik)
             fdifir(i) = fdifir(i)+fsdif(i)*hk2(ib,ik)
             fdirir_c(i) = fdirir_c(i)+fsdir_c(i)*hk2(ib,ik)
             fdifir_c(i) = fdifir_c(i)+fsdif_c(i)*hk2(ib,ik)
          ENDDO

       ENDDO ! integration over the k-distribution function
    ENDDO ! integration over spectral bands

  END SUBROUTINE solircld

  !
  ! Subroutine: Soluvcld
  !
  ! $Author: pkubota $
  ! Modifications: T. Tarasova, 2005
  ! Modifications: H. Barbosa
  !
  ! Description:
  !  compute solar fluxes in the uv+par region. the spectrum is
  !  grouped into 8 bands:
  !
  !   band     micrometer
  !
  !    uv-c    1.     .175 - .225
  !    2.     .225 - .245
  !   .260 - .280
  !    3.     .245 - .260
  !
  !    uv-b    4.     .280 - .295
  !    5.     .295 - .310
  !    6.     .310 - .320
  !
  !    uv-a    7.     .320 - .400
  !
  !    par     8.     .400 - .700
  !
  ! Inputs: units        size
  !
  !      m: number of soundings n/d      1
  !     np: number of atmospheric layers n/d      1
  !     wh: layer scaled-water vapor content gm/cm^2      m*np
  !     oh: layer ozone content (cm-atm)stp  m*np
  !     dp: layer pressure thickness mb      m*np
  !  overcast: option for scaling cloud optical thickness n/d      1
  !   "true"  = scaling is not required
  !   "fasle" = scaling is required
  !  cldwater: input option for cloud optical thickness n/d      1
  !   "true"  = taucld is provided
  !   "false" = cwp is provided
  !    cwp: cloud water amount gm/m**2      m*np*3
  !   index 1 for ice particles
  !   index 2 for liquid drops
  !   index 3 for rain drops
  ! taucld: cloud optical thickness n/d      m*np*3
  !   index 1 for ice particles
  !   index 2 for liquid drops
  !   index 3 for rain drops
  !   reff: effective cloud-particle size   micrometer   m*np*3
  !   index 1 for ice paticles
  !   index 2 for liquid drops
  !   index 3 for rain drops
  !    ict: level index separating high and middle clouds   n/d      m
  !    icb: level indiex separating middle and low clouds   n/d      m
  !   fcld: cloud amount fraction     m*np
  !   cosz: cosine of solar zenith angle n/d      m
  !  taual: aerosol optical thickness n/d      m*np*11
  !  ssaal: aerosol single-scattering albedo n/d      m*np*11
  !  asyal: aerosol asymmetry factor n/d      m*np*11
  ! rsuvbm: uv+par surface albedo for beam radiation fraction     m
  ! rsuvdf: uv+par surface albedo for diffuse radiation fraction     m
  !
  ! Outputs: (updated parameters)
  !
  !    flx: all-sky   net downward flux    fraction  m*(np+1)
  !    flc: clear-sky net downward flux    fraction  m*(np+1)
  ! fdiruv: all-sky direct  downward uv  flux at the surface   fraction  m
  ! fdifuv: all-sky diffuse downward uv  flux at the surface   fraction  m
  !fdirpar: all-sky direct  downward par flux at the surface   fraction  m
  !fdifpar: all-sky diffuse downward par flux at the surface   fraction  m
  !
  ! Local Variables
  !
  !  tauclb: scaled cloud optical thickness for beam radiation    n/d   m*np
  !  tauclf: scaled cloud optical thickness for diffuse radiation n/d   m*np
  !
  !
  !
  !
  SUBROUTINE Soluvcld (m,np,wh,oh,dp,  &
       taucld,reff,ict,icb,fcld,cosz,  &
       taual,ssaal,asyal,rsuvbm,rsuvdf,  &
       flx,flc,fdiruv,fdifuv,fdirpar,fdifpar,  &
       fdiruv_c,fdifuv_c,fdirpar_c,fdifpar_c,  &
       flx_d,flx_u,flc_d,flc_u)   !new

    IMPLICIT NONE
    INTEGER I

    ! parameters
    INTEGER, PARAMETER :: nband=8

    ! hk is the fractional extra-terrestrial solar flux in each
    ! of the 8 bands. the sum of hk is 0.47074. (table 3)
    REAL(KIND=r8), PARAMETER, DIMENSION(nband) :: hk3 = (/ &
         0.00057_r8, 0.00367_r8, 0.00083_r8, 0.00417_r8, 0.00600_r8, &
         0.00556_r8, 0.05913_r8, 0.39081_r8 /)

    ! zk is the ozone absorption coefficient. unit: /(cm-atm)stp (table 3)
    REAL(KIND=r8), PARAMETER, DIMENSION(nband) :: zk3 = (/ &
         30.47_r8, 187.2_r8, 301.9_r8, 42.83_r8, 7.09_r8, 1.25_r8, 0.0345_r8, 0.0572_r8 /)

    ! wk is the water vapor absorption coefficient. unit: cm**2/g (table 3)
    REAL(KIND=r8), PARAMETER, DIMENSION(nband) :: wk3 = (/ (0.0_r8,I=1,7) , 0.00075_r8 /)

    ! ry is the extinction coefficient for rayleigh scattering. unit: /mb. (table 3)
    REAL(KIND=r8), PARAMETER, DIMENSION(nband) :: ry3 = (/ &
         0.00604_r8, 0.00170_r8, 0.00222_r8, 0.00132_r8, 0.00107_r8, 0.00091_r8, &
         0.00055_r8, 0.00012_r8 /)

    ! coefficients for computing the extinction coefficients of ice,
    ! water, and rain particles, independent of spectral band. (table 4)
    REAL(KIND=r8), PARAMETER, DIMENSION(2) :: aib3 = (/  3.33e-4_r8, 2.52_r8 /)
    REAL(KIND=r8), PARAMETER, DIMENSION(2) :: awb3 = (/ -6.59e-3_r8, 1.65_r8 /)
    REAL(KIND=r8), PARAMETER, DIMENSION(2) :: arb3 = (/  3.07e-3_r8, 0.00_r8 /)

    ! coefficients for computing the asymmetry factor of ice, water,
    ! and rain particles, independent of spectral band. (table 6)

    REAL(KIND=r8), PARAMETER, DIMENSION(3) :: aig3 = (/ 0.74625_r8, 0.0010541_r8, -0.00000264_r8 /)
    REAL(KIND=r8), PARAMETER, DIMENSION(3) :: awg3 = (/ 0.82562_r8, 0.0052900_r8, -0.00014866_r8 /)
    REAL(KIND=r8), PARAMETER, DIMENSION(3) :: arg3 = (/ 0.883_r8,   0.0_r8,      0.0_r8        /)

    ! input variables
    INTEGER, INTENT(IN) :: m,np,ict,icb
    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np,11) :: taual, ssaal, asyal
    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np,3) :: taucld, reff
    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np) :: fcld, wh, oh, dp
    REAL(KIND=r8), INTENT(IN), DIMENSION(m) :: rsuvbm, rsuvdf, cosz

    ! output variables
    REAL(KIND=r8), INTENT(INOUT), DIMENSION(m,np+1) :: flx,flc,flx_d,flx_u, flc_d,flc_u
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m) :: fdiruv,fdifuv,fdirpar,fdifpar
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m) :: fdiruv_c,fdifuv_c,fdirpar_c,fdifpar_c

    ! local  variables
    INTEGER :: k,ib
    INTEGER :: ih1,ih2,im1,im2,is1,is2
    REAL(KIND=r8)    :: taurs,tauoz,tauwv
    REAL(KIND=r8)    :: taux,reff1,reff2,g1,g2,g3

    REAL(KIND=r8), DIMENSION(m) :: dsm, fsdir, fsdif, fsdir_c, fsdif_c, asyclt
    REAL(KIND=r8), DIMENSION(m,3) :: cc
    REAL(KIND=r8), DIMENSION(m,np) :: tauclb,tauclf,asycl,tautof,ssatof,asytof
    REAL(KIND=r8), DIMENSION(m,np) :: tausto,ssatau,asysto,tautob,ssatob,asytob
    REAL(KIND=r8), DIMENSION(m,np) :: dum,rrt,ttt,tdt,rst,tst
    REAL(KIND=r8), DIMENSION(m,np+1) :: fall,fclr,fall_d,fall_u,fclr_d,fclr_u
    REAL(KIND=r8), DIMENSION(m,np+1,2) :: rr,tt,td,rs,ts

    ! initialize local vectors
    ! subroutine starts here

    !  initialize fdiruv, fdifuv, surface reflectances and transmittances.
    !  the reflectance and transmittance of the clear and cloudy portions
    !  of a layer are denoted by 1 and 2, respectively.
    !  cc is the maximum cloud cover in each of the high, middle, and low
    !  cloud groups.
    ! 1/dsm=1/cos(53) = 1.66

    dsm=0.602_r8
    fdiruv=0.0_r8
    fdifuv=0.0_r8
    fdirpar=0.0_r8
    fdifpar=0.0_r8
    fdiruv_c=0.0_r8
    fdifuv_c=0.0_r8
    fdirpar_c=0.0_r8
    fdifpar_c=0.0_r8
    rr(1:m,np+1,1)=rsuvbm(1:m)
    rr(1:m,np+1,2)=rsuvbm(1:m)
    rs(1:m,np+1,1)=rsuvdf(1:m)
    rs(1:m,np+1,2)=rsuvdf(1:m)
    td=0.0_r8
    tt=0.0_r8
    ts=0.0_r8
    cc=0.0_r8

    ! scale cloud optical thickness in each layer from taucld (with
    ! cloud amount fcld) to tauclb and tauclf (with cloud amount cc).
    ! tauclb is the scaled optical thickness for beam radiation and
    ! tauclf is for diffuse radiation (see section 7).

    CALL cldscale(m,np,cosz,fcld,taucld,ict,icb,cc,tauclb,tauclf)

    ! cloud asymmetry factor for a mixture of liquid and ice particles.
    ! unit of reff is micrometers. eqs. (4.8) and (6.4)

    DO k=1,np
       DO i=1,m

          asyclt(i)=1.0_r8
          taux=taucld(i,k,1)+taucld(i,k,2)+taucld(i,k,3)
          IF (taux.gt.0.02_r8 .and. fcld(i,k).gt.0.01_r8) THEN
             reff1=min(reff(i,k,1),130.0_r8)
             reff2=min(reff(i,k,2),20.0_r8)
             g1=(aig3(1)+(aig3(2)+aig3(3)*reff1)*reff1)*taucld(i,k,1)
             g2=(awg3(1)+(awg3(2)+awg3(3)*reff2)*reff2)*taucld(i,k,2)
             g3= arg3(1)*taucld(i,k,3)
             asyclt(i)=(g1+g2+g3)/taux
          ENDIF
       ENDDO

       DO i=1,m
          asycl(i,k)=asyclt(i)
       ENDDO

    ENDDO

    ! integration over spectral bands
    DO ib=1,nband
       DO k=1,np
          DO i=1,m
             ! compute rayleigh, ozone and water vapor optical thicknesses
             taurs=ry3(ib)*dp(i,k)
             tauoz=zk3(ib)*oh(i,k)
             tauwv=wk3(ib)*wh(i,k)

             ! compute clear-sky optical thickness, single scattering albedo,
             ! and asymmetry factor (eqs. 6.2-6.4)
             tausto(i,k)=taurs+tauoz+tauwv+taual(i,k,ib)+1.0e-8_r8
             ssatau(i,k)=ssaal(i,k,ib)*taual(i,k,ib)+taurs
             asysto(i,k)=asyal(i,k,ib)*ssaal(i,k,ib)*taual(i,k,ib)

             ! compute reflectance and transmittance of the clear portion of a layer
             tautob(i,k)=tausto(i,k)
             ssatob(i,k)=ssatau(i,k)/tautob(i,k)+1.0e-8_r8
             ssatob(i,k)=min(ssatob(i,k),0.999999_r8)
             asytob(i,k)=asysto(i,k)/(ssatob(i,k)*tautob(i,k))
          ENDDO
       ENDDO

       ! for direct incident radiation
       CALL deledd (m,np,tautob,ssatob,asytob,cosz,rrt,ttt,tdt)

       ! diffuse incident radiation is approximated by beam radiation with
       ! an incident angle of 53 degrees, eqs. (6.5) and (6.6)
       CALL deledd (m,np,tautob,ssatob,asytob,dsm,rst,tst,dum)

       DO k=1,np
          DO i=1,m
             rr(i,k,1)=rrt(i,k)
             tt(i,k,1)=ttt(i,k)
             td(i,k,1)=tdt(i,k)
             rs(i,k,1)=rst(i,k)
             ts(i,k,1)=tst(i,k)
          ENDDO
       ENDDO

       ! compute reflectance and transmittance of the cloudy portion of a layer
       DO k=1,np
          DO i=1,m

             ! for direct incident radiation
             ! the effective layer optical properties. eqs. (6.2)-(6.4)
             tautob(i,k)=tausto(i,k)+tauclb(i,k)
             ssatob(i,k)=(ssatau(i,k)+tauclb(i,k))/tautob(i,k)+1.0e-8_r8
             ssatob(i,k)=min(ssatob(i,k),0.999999_r8)
             asytob(i,k)=(asysto(i,k)+asycl(i,k)*tauclb(i,k)) &
                  /(ssatob(i,k)*tautob(i,k))

             ! for diffuse incident radiation
             tautof(i,k)=tausto(i,k)+tauclf(i,k)
             ssatof(i,k)=(ssatau(i,k)+tauclf(i,k))/tautof(i,k)+1.0e-8_r8
             ssatof(i,k)=min(ssatof(i,k),0.999999_r8)
             asytof(i,k)=(asysto(i,k)+asycl(i,k)*tauclf(i,k))  &
                  /(ssatof(i,k)*tautof(i,k))
          ENDDO
       ENDDO

       ! for direct incident radiation
       ! note that the cloud optical thickness is scaled differently for direct
       ! and diffuse insolation, eqs. (7.3) and (7.4).

       CALL deledd (m,np,tautob,ssatob,asytob,cosz,rrt,ttt,tdt)

       ! diffuse incident radiation is approximated by beam radiation with
       ! an incident angle of 53 degrees, eqs. (6.5) and (6.6)

       CALL deledd (m,np,tautof,ssatof,asytof,dsm,rst,tst,dum)

       DO k=1,np
          DO i=1,m
             rr(i,k,2)=rrt(i,k)
             tt(i,k,2)=ttt(i,k)
             td(i,k,2)=tdt(i,k)
             rs(i,k,2)=rst(i,k)
             ts(i,k,2)=tst(i,k)
          ENDDO
       ENDDO

       ! flux calculations

       ! initialize clear-sky flux (fclr), all-sky flux (fall),
       ! and surface downward fluxes (fsdir and fsdif)

       !hmjb they are initialized inside cldfx()
       !     fclr   = 0.0_r8
       !     fall   = 0.0_r8
       !     fclr_d = 0.0_r8
       !     fclr_u = 0.0_r8
       !     fall_d = 0.0_r8
       !     fall_u = 0.0_r8
       !
       !     fsdir   = 0.0_r8
       !     fsdif   = 0.0_r8
       !     fsdir_c = 0.0_r8
       !     fsdif_c = 0.0_r8

       ! for clear- and all-sky fluxes
       ! the all-sky flux, fall is the summation inside the brackets
       ! of eq. (7.11)
       ih1=1
       ih2=2
       im1=1
       im2=2
       is1=1
       is2=2

       CALL cldflx (m,np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
            cc,rr,tt,td,rs,ts,fclr,fall,fsdir,fsdif,fsdir_c,fsdif_c, &
            fclr_d,fclr_u,fall_d,fall_u)   ! new

       ! flux integration, eq. (6.1)

       DO k=1,np+1
          DO i=1,m
             flx(i,k)=flx(i,k)+fall(i,k)*hk3(ib)
             flx_d(i,k)=flx_d(i,k)+fall_d(i,k)*hk3(ib) !new
             flx_u(i,k)=flx_u(i,k)+fall_u(i,k)*hk3(ib) !new
          ENDDO

          DO i=1,m
             flc(i,k)=flc(i,k)+fclr(i,k)*hk3(ib)
             flc_d(i,k)=flc_d(i,k)+fclr_d(i,k)*hk3(ib) !new
             flc_u(i,k)=flc_u(i,k)+fclr_u(i,k)*hk3(ib) !new
          ENDDO
       ENDDO

       ! compute direct and diffuse downward surface fluxes in the uv
       ! and par regions
       IF(ib.lt.8) THEN
          DO i=1,m
             fdiruv(i)=fdiruv(i)+fsdir(i)*hk3(ib)
             fdifuv(i)=fdifuv(i)+fsdif(i)*hk3(ib)
             fdiruv_c(i) = fdiruv_c(i)+fsdir_c(i)*hk3(ib)
             fdifuv_c(i) = fdifuv_c(i)+fsdif_c(i)*hk3(ib)
          ENDDO
       ELSE
          DO i=1,m
             fdirpar(i)=fsdir(i)*hk3(ib)
             fdifpar(i)=fsdif(i)*hk3(ib)
             fdirpar_c(i) = fsdir_c(i)*hk3(ib)
             fdifpar_c(i) = fsdif_c(i)*hk3(ib)
          ENDDO
       ENDIF

    ENDDO ! integration over spectral bands

  end SUBROUTINE soluvcld

  !
  ! Subroutine: CLDSCALE
  !
  ! $Author: pkubota $
  !
  ! Description:
  !this SUBROUTINE computes the high, middle, and low cloud
  ! amounts and scales the cloud optical thickness (section 7)
  !
  !to simplify calculations in a cloudy atmosphere, clouds are
  ! grouped into high, middle and low clouds separated by the levels
  ! ict and icb (level 1 is the top of the model atmosphere).
  !
  !within each of the three groups, clouds are assumed maximally
  ! overlapped, and the cloud cover (cc) of a group is the maximum
  ! cloud cover of all the layers in the group.  the optical thickness
  ! (taucld) of a given layer is then scaled to new values (tauclb and
  ! tauclf) so that the layer reflectance corresponding to the cloud
  ! cover cc is the same as the original reflectance with optical
  ! thickness taucld and cloud cover fcld.
  !
  ! Inputs:
  !   m:  number of atmospheric soundings
  !  np:  number of atmospheric layers
  !cosz:  cosine of the solar zenith angle
  !fcld:  fractional cloud cover
  ! taucld:  cloud optical thickness
  ! ict:  index separating high and middle clouds
  ! icb:  index separating middle and low clouds
  !
  ! Outputs:
  !   cc:  fractional cover of high, middle, and low cloud groups
  !  tauclb:  scaled cloud optical thickness for direct  radiation
  !  tauclf:  scaled cloud optical thickness for diffuse radiation
  !
  !
  !
  SUBROUTINE cldscale (m,np,cosz,fcld,taucld,ict,icb,cc,tauclb,tauclf)

    IMPLICIT NONE

    ! parameters
    INTEGER, PARAMETER :: nm=11
    INTEGER, PARAMETER :: nt=9
    INTEGER, PARAMETER :: na=11

    REAL(KIND=r8), PARAMETER :: dm=0.1_r8     ! size of cosz-interval
    REAL(KIND=r8), PARAMETER :: dt=0.30103_r8  ! size of taucld-interval
    REAL(KIND=r8), PARAMETER :: da=0.1_r8      ! size of cloud amount-interval
    REAL(KIND=r8), PARAMETER :: t1=-0.9031_r8

    INTEGER i,j
    ! include the pre-computed table of mcai for scaling the cloud optical
    ! thickness under the assumption that clouds are maximally overlapped

    ! caib is for scaling the cloud optical thickness for direct radiation
    REAL(KIND=r8) :: caib(nm,nt,na)

    ! caif is for scaling the cloud optical thickness for diffuse radiation
    REAL(KIND=r8) :: caif(nt,na)

    INCLUDE "mcai.data90"

    ! input variables
    INTEGER, INTENT(IN) :: m
    INTEGER, INTENT(IN) :: np
    INTEGER, INTENT(IN) :: ict
    INTEGER, INTENT(IN) :: icb

    REAL(KIND=r8), INTENT(IN), DIMENSION(m) :: cosz
    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np) :: fcld
    REAL(KIND=r8), INTENT(IN), DIMENSION(m,np,3) :: taucld

    ! output variables
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m,3)  :: cc
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m,np) :: tauclb
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m,np) :: tauclf

    !  local  variables
    INTEGER :: k,im,it,ia,kk
    REAL(KIND=r8) :: fm,ft,fa,xai,taux

    ! initialize local vectors and output variables

    ! subroutine starts here

    ! clouds within each of the high, middle, and low clouds are assumed
    !     to be maximally overlapped, and the cloud cover (cc) for a group
    !     (high, middle, or low) is the maximum cloud cover of all the layers
    !     within a group

    DO i=1,m
       cc(i,1)=0.0_r8
       cc(i,2)=0.0_r8
       cc(i,3)=0.0_r8
    ENDDO

    DO k=1,ict-1
       DO i=1,m
          cc(i,1)=max(cc(i,1),fcld(i,k))
       ENDDO
    ENDDO

    DO k=ict,icb-1
       DO i=1,m
          cc(i,2)=max(cc(i,2),fcld(i,k))
       ENDDO
    ENDDO

    DO k=icb,np
       DO i=1,m
          cc(i,3)=max(cc(i,3),fcld(i,k))
       ENDDO
    ENDDO
    ! scale the cloud optical thickness.
    !     taucld(i,k,1) is the optical thickness for ice particles
    !     taucld(i,k,2) is the optical thickness for liquid particles
    !     taucld(i,k,3) is the optical thickness for rain drops
    DO k=1,np

       IF(k.lt.ict) THEN
          kk=1
       ELSEIF(k.ge.ict .and. k.lt.icb) THEN
          kk=2
       ELSE
          kk=3
       ENDIF

       DO i=1,m
          !hmjb ta feito no comeco
          tauclb(i,k) = 0.0_r8
          tauclf(i,k) = 0.0_r8
          taux=taucld(i,k,1)+taucld(i,k,2)+taucld(i,k,3)

          IF (taux.gt.0.02_r8 .and. fcld(i,k).gt.0.01_r8) THEN
             ! normalize cloud cover following eq. (7.8)
             fa=fcld(i,k)/cc(i,kk)

             ! table look-up
             taux=min(taux,32.0_r8)

             fm=cosz(i)/dm
             ft=(log10(taux)-t1)/dt
             fa=fa/da

             im=int(fm+1.5_r8)
             it=int(ft+1.5_r8)
             ia=int(fa+1.5_r8)

             im=max(im,2)
             it=max(it,2)
             ia=max(ia,2)

             im=min(im,nm-1)
             it=min(it,nt-1)
             ia=min(ia,na-1)

             fm=fm-float(im-1)
             ft=ft-float(it-1)
             fa=fa-float(ia-1)

             ! scale cloud optical thickness for beam radiation following eq. (7.3)
             !     the scaling factor, xai, is a function of the solar zenith
             !     angle, optical thickness, and cloud cover.

             xai=    (-caib(im-1,it,ia)*(1.0_r8-fm)+  &
                  caib(im+1,it,ia)*(1.0_r8+fm))*fm*0.5_r8+caib(im,it,ia)*(1.0_r8-fm*fm)

             xai=xai+(-caib(im,it-1,ia)*(1.0_r8-ft)+ &
                  caib(im,it+1,ia)*(1.0_r8+ft))*ft*0.5_r8+caib(im,it,ia)*(1.0_r8-ft*ft)

             xai=xai+(-caib(im,it,ia-1)*(1.0_r8-fa)+ &
                  caib(im,it,ia+1)*(1.0_r8+fa))*fa*0.5_r8+caib(im,it,ia)*(1.0_r8-fa*fa)

             xai= xai-2.0_r8*caib(im,it,ia)
             xai=max(xai,0.0_r8)

             tauclb(i,k) = taux*xai
             ! scale cloud optical thickness for diffuse radiation following eq. (7.4)
             !     the scaling factor, xai, is a function of the cloud optical
             !     thickness and cover but not the solar zenith angle.

             xai=    (-caif(it-1,ia)*(1.0_r8-ft)+  &
                  caif(it+1,ia)*(1.0_r8+ft))*ft*0.5_r8+caif(it,ia)*(1.0_r8-ft*ft)

             xai=xai+(-caif(it,ia-1)*(1.0_r8-fa)+  &
                  caif(it,ia+1)*(1.0_r8+fa))*fa*0.5_r8+caif(it,ia)*(1.0_r8-fa*fa)

             xai= xai-caif(it,ia)
             xai=max(xai,0.0_r8)

             tauclf(i,k) = taux*xai
          ENDIF
       ENDDO
    ENDDO
  END SUBROUTINE cldscale

  !
  ! Subroutine: DELEDD
  !
  ! $Author: pkubota $
  !
  ! Description:
  !  uses the delta-eddington approximation to compute the
  !  bulk scattering properties of a single layer
  !  coded following king and harshvardhan (jas, 1986)
  !
  ! Inputs:
  !   m:  number of soundings
  !  np:  number of atmospheric layers
  ! tau:  optical thickness
  ! ssc:  single scattering albedo
  !  g0:  asymmetry factor
  ! cza:  cosine of the zenith angle
  !
  ! Outputs:
  !   rr:  reflection of the direct beam
  !   tt:  total diffuse transmission of the direct beam
  !   td:  direct transmission of the direct beam
  !
  !
  !
  SUBROUTINE deledd(m,np,tau,ssc,g0,cza,rr,tt,td)

    IMPLICIT NONE

    ! parameters
    REAL(KIND=r8), PARAMETER :: zero=0.0_r8
    REAL(KIND=r8), PARAMETER :: one=1.0_r8
    REAL(KIND=r8), PARAMETER :: two=2.0_r8
    REAL(KIND=r8), PARAMETER :: three=3.0_r8
    REAL(KIND=r8), PARAMETER :: four=4.0_r8
    REAL(KIND=r8), PARAMETER :: fourth=0.25_r8
    REAL(KIND=r8), PARAMETER :: seven=7.0_r8
    REAL(KIND=r8), PARAMETER :: thresh=1.0e-8_r8

    ! input variables
    INTEGER, INTENT(IN) :: m
    INTEGER, INTENT(IN) :: np

    REAL(KIND=r8),  INTENT(IN), DIMENSION(m,np) :: tau
    REAL(KIND=r8),  INTENT(IN), DIMENSION(m,np) :: ssc
    REAL(KIND=r8),  INTENT(IN), DIMENSION(m,np) :: g0
    REAL(KIND=r8),  INTENT(IN), DIMENSION(m) :: cza

    ! output variables
    REAL(KIND=r8),  INTENT(OUT), DIMENSION(m,np) :: rr
    REAL(KIND=r8),  INTENT(OUT), DIMENSION(m,np) :: tt
    REAL(KIND=r8),  INTENT(OUT), DIMENSION(m,np) :: td

    ! local  variables
    INTEGER :: i,k
    REAL(KIND=r8) :: zth,ff,xx,taup,sscp,gp,gm1,gm2,gm3,akk,alf1,alf2, &
         temp_all,bll,st7,st8,cll,dll,fll,ell,st1,st2,st3,st4


    ! initialize local vectors and output variables
    ! subroutine starts here
    DO k=1,np
       DO i=1,m
          zth = cza(i)
          !  delta-eddington scaling of single scattering albedo,
          !  optical thickness, and asymmetry factor,
          !  k & h eqs(27-29)

          ff  = g0(i,k)*g0(i,k)
          xx  = one-ff *ssc(i,k)
          taup= tau(i,k)*xx
          sscp= ssc(i,k)*(one-ff)/xx
          gp  = g0(i,k) /(one+g0(i,k))

          !  gamma1, gamma2, and gamma3. see table 2 and eq(26) k & h
          !  ssc and gp are the d-s single scattering
          !  albedo and asymmetry factor.

          xx  =  three*gp
          gm1 =  (seven - sscp*(four+xx))*fourth
          gm2 = -(one   - sscp*(four-xx))*fourth

          !  akk is k as defined in eq(25) of k & h

          akk = sqrt((gm1+gm2)*(gm1-gm2))

          xx  = akk * zth
          st7 = one - xx
          st8 = one + xx
          st3 = st7 * st8

          IF (abs(st3) .lt. thresh) THEN
             zth = zth + 0.001_r8
             xx  = akk * zth
             st7 = one - xx
             st8 = one + xx
             st3 = st7 * st8
          ENDIF

          !  extinction of the direct beam transmission
          td(i,k)  = exp(-taup/zth)

          !  alf1 and alf2 are alpha1 and alpha2 from eqs (23) & (24) of k & h
          gm3  = (two - zth*three*gp)*fourth
          xx   = gm1 - gm2
          alf1 = gm1 - gm3 * xx
          alf2 = gm2 + gm3 * xx

          ! all is last term in eq(21) of k & h
          ! bll is last term in eq(22) of k & h

          xx  = akk * two
          temp_all = (gm3 - alf2 * zth    )*xx*td(i,k)
          bll = (one - gm3 + alf1*zth)*xx

          xx  = akk * gm3
          cll = (alf2 + xx) * st7
          dll = (alf2 - xx) * st8

          xx  = akk * (one-gm3)
          fll = (alf1 + xx) * st8
          ell = (alf1 - xx) * st7

          st2 = exp(-akk*taup)
          st4 = st2 * st2

          st1 =  sscp / ((akk+gm1 + (akk-gm1)*st4) * st3)

          ! rr is r-hat of eq(21) of k & h
          ! tt is diffuse part of t-hat of eq(22) of k & h

          rr(i,k) =( cll-dll*st4      -temp_all*st2)*st1
          tt(i,k) = - ((fll-ell*st4)*td(i,k)-bll*st2)*st1

          rr(i,k) = max(rr(i,k),zero)
          tt(i,k) = max(tt(i,k),zero)

          tt(i,k) = tt(i,k)+td(i,k)
       ENDDO
    ENDDO

  END SUBROUTINE deledd

  !
  ! Subroutine: RFLX
  !
  ! $Author: pkubota $
  !
  ! Description:
  !Computes the reduction of clear-sky downward solar flux
  !due to co2 absorption.
  !
  !
  !
  !
  SUBROUTINE rflx(m,np,swc,u1,du,nu,swh,w1,dw,nw,tbl,df)

    IMPLICIT NONE

    ! input variables
    INTEGER, INTENT(IN) :: m
    INTEGER, INTENT(IN) :: np
    INTEGER, INTENT(IN) :: nu
    INTEGER, INTENT(IN) :: nw

    REAL(KIND=r8),  INTENT(IN) :: u1
    REAL(KIND=r8),  INTENT(IN) :: du
    REAL(KIND=r8),  INTENT(IN) :: w1
    REAL(KIND=r8),  INTENT(IN) :: dw
    REAL(KIND=r8),  INTENT(IN), DIMENSION(m,np+1) :: swc
    REAL(KIND=r8),  INTENT(IN), DIMENSION(m,np+1) :: swh
    REAL(KIND=r8),  INTENT(IN), DIMENSION(nu,nw)  :: tbl

    ! output variables (updated)
    REAL(KIND=r8),  INTENT(INOUT), DIMENSION(m,np+1) :: df

    ! local  variables
    INTEGER :: i,k,ic,iw
    REAL(KIND=r8) :: temp_clog,wlog,dc,dd,x1,x2,y1,y2

    ! subroutine starts here

    ! table look-up for the reduction of clear-sky solar
    x1=u1-0.5_r8*du
    y1=w1-0.5_r8*dw

    DO k= 2, np+1
       DO i= 1, m
          temp_clog=swc(i,k)
          wlog=swh(i,k)
          ic=int( (temp_clog-x1)/du+1.0_r8)
          iw=int( (wlog-y1)/dw+1.0_r8)
          IF(ic.lt.2)ic=2
          IF(iw.lt.2)iw=2
          IF(ic.gt.nu)ic=nu
          IF(iw.gt.nw)iw=nw
          dc=temp_clog-float(ic-2)*du-u1
          dd=wlog-float(iw-2)*dw-w1
          x2=tbl(ic-1,iw-1)+(tbl(ic-1,iw)-tbl(ic-1,iw-1))/dw*dd
          y2=x2+(tbl(ic,iw-1)-tbl(ic-1,iw-1))/du*dc
          df(i,k)=df(i,k)+y2
       ENDDO
    ENDDO

  END SUBROUTINE rflx


  !
  ! Subroutine: CLDFLX
  !
  ! $Author: pkubota $
  !
  ! Description:
  !  compute upward and downward fluxes using a two-stream adding method
  !  following equations (6.9)-(6.16).
  !
  !  clouds are grouped into high, middle, and low clouds which are assumed
  !  randomly overlapped. it involves a maximum of 8 sets of calculations.
  !  in each set of calculations, each atmospheric layer is homogeneous,
  !  either totally filled with clouds or without clouds.
  !
  ! Inputs:
  !    m:  number of soundings
  !   np:  number of atmospheric layers
  !  ict:  the level separating high and middle clouds
  !  icb:  the level separating middle and low clouds
  ! ih1,ih2,
  ! im1,im2,
  ! is1,is2: indices for three group of clouds
  !   cc:  effective cloud covers for high, middle and low clouds
  !   rr:  reflection of a layer illuminated by beam radiation
  !   tt:  total diffuse transmission of a layer illuminated by beam radiation
  !   td:  direct beam transmission
  !   rs:  reflection of a layer illuminated by diffuse radiation
  !   ts:  transmission of a layer illuminated by diffuse radiation
  !
  ! Outputs:
  !  fclr:  clear-sky flux (downward minus upward)
  !  fall:  all-sky flux (downward minus upward)
  !  fsdir: surface direct downward flux
  !  fsdif: surface diffuse downward flux
  !
  !
  !
  !
  SUBROUTINE cldflx (m,np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
       cc,rr,tt,td,rs,ts,fclr,fall,fsdir,fsdif,fsdir_c,fsdif_c, &
       fclr_d,fclr_u,fall_d,fall_u)   ! new

    IMPLICIT NONE

    ! input variables
    INTEGER,  INTENT(IN) :: m,np,ict,icb,ih1,ih2,im1,im2,is1,is2
    REAL(KIND=r8),  INTENT(IN), DIMENSION(m,np+1,2) :: rr,tt,td,rs,ts
    REAL(KIND=r8),  INTENT(IN), DIMENSION(m,3) :: cc

    ! output variables
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m,np+1) :: fclr, fclr_u, fclr_d
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m,np+1) :: fall, fall_u, fall_d
    REAL(KIND=r8), INTENT(OUT), DIMENSION(m) :: fsdir, fsdif, fsdir_c, fsdif_c

    ! local  variables
    REAL(KIND=r8), DIMENSION(m,np+1,2,2) :: tta ! composite total transmittance illuminated by beam radiation
    REAL(KIND=r8), DIMENSION(m,np+1,2,2) :: tda ! composite transmittance illuminated by beam radiation
    REAL(KIND=r8), DIMENSION(m,np+1,2,2) :: rsa ! composite reflectance illuminated from below by diffuse radiation
    REAL(KIND=r8), DIMENSION(m,np+1,2,2) :: rra ! composite reflectance illuminated by beam radiation
    REAL(KIND=r8), DIMENSION(m,np+1,2,2) :: rxa ! the composite reflectance illuminated from above by diffuse radiation

    REAL(KIND=r8), DIMENSION(m,np+1) :: fdn! downward fluxes
    REAL(KIND=r8), DIMENSION(m,np+1) :: fup! upward fluxes
    REAL(KIND=r8), DIMENSION(m,np+1) :: flxdn ! net flux

    REAL(KIND=r8), DIMENSION(m) :: fdndir ! direct  downward flux
    REAL(KIND=r8), DIMENSION(m) :: fdndif ! diffuse downward flux
    REAL(KIND=r8), DIMENSION(m) :: ch, cm, ct

    REAL(KIND=r8) :: fupdif    ! diffuse upward flux
    REAL(KIND=r8) :: denm,xx,yy
    INTEGER :: i,k,ih,im,is

    ! subroutine starts here
    fclr   = 0.0_r8
    fall   = 0.0_r8
    fclr_d = 0.0_r8
    fclr_u = 0.0_r8
    fall_d = 0.0_r8
    fall_u = 0.0_r8

    fsdir   = 0.0_r8
    fsdif   = 0.0_r8
    fsdir_c = 0.0_r8
    fsdif_c = 0.0_r8

    ! compute transmittances and reflectances for a composite of
    !     layers. layers are added one at a time, going down from the top.
    !     tta and rsa are computed from eqs. (6.10) and (6.12)

    ! for high clouds
    !     ih=1 for clear-sky condition, ih=2 for cloudy-sky condition
    DO ih=ih1,ih2

       DO i=1,m
          tda(i,1,ih,1)=td(i,1,ih)
          tta(i,1,ih,1)=tt(i,1,ih)
          rsa(i,1,ih,1)=rs(i,1,ih)
          tda(i,1,ih,2)=td(i,1,ih)
          tta(i,1,ih,2)=tt(i,1,ih)
          rsa(i,1,ih,2)=rs(i,1,ih)
       ENDDO

       DO k=2,ict-1
          DO i=1,m
             denm = ts(i,k,ih)/( 1.0_r8-rsa(i,k-1,ih,1)*rs(i,k,ih))
             tda(i,k,ih,1)= tda(i,k-1,ih,1)*td(i,k,ih)
             tta(i,k,ih,1)= tda(i,k-1,ih,1)*tt(i,k,ih) &
                  +(tda(i,k-1,ih,1)*rsa(i,k-1,ih,1)*rr(i,k,ih) &
                  +tta(i,k-1,ih,1)-tda(i,k-1,ih,1))*denm
             rsa(i,k,ih,1)= rs(i,k,ih)+ts(i,k,ih) &
                  *rsa(i,k-1,ih,1)*denm
             tda(i,k,ih,2)= tda(i,k,ih,1)
             tta(i,k,ih,2)= tta(i,k,ih,1)
             rsa(i,k,ih,2)= rsa(i,k,ih,1)
          ENDDO

       ENDDO

       ! for middle clouds
       !     im=1 for clear-sky condition, im=2 for cloudy-sky condition
       DO im=im1,im2

          DO k=ict,icb-1
             DO i=1,m
                denm = ts(i,k,im)/( 1.0_r8-rsa(i,k-1,ih,im)*rs(i,k,im))
                tda(i,k,ih,im)= tda(i,k-1,ih,im)*td(i,k,im)
                tta(i,k,ih,im)= tda(i,k-1,ih,im)*tt(i,k,im)  &
                     +(tda(i,k-1,ih,im)*rsa(i,k-1,ih,im)*rr(i,k,im)  &
                     +tta(i,k-1,ih,im)-tda(i,k-1,ih,im))*denm
                rsa(i,k,ih,im)= rs(i,k,im)+ts(i,k,im)  &
                     *rsa(i,k-1,ih,im)*denm
             ENDDO
          ENDDO

       ENDDO         ! end im loop
    ENDDO    ! end ih loop

    ! layers are added one at a time, going up from the surface.
    !     rra and rxa are computed from eqs. (6.9) and (6.11)

    ! for the low clouds
    !     is=1 for clear-sky condition, is=2 for cloudy-sky condition
    DO is=is1,is2

       DO i=1,m
          rra(i,np+1,1,is)=rr(i,np+1,is)
          rxa(i,np+1,1,is)=rs(i,np+1,is)
          rra(i,np+1,2,is)=rr(i,np+1,is)
          rxa(i,np+1,2,is)=rs(i,np+1,is)
       ENDDO

       DO k=np,icb,-1
          DO i=1,m
             denm=ts(i,k,is)/( 1.0_r8-rs(i,k,is)*rxa(i,k+1,1,is) )
             rra(i,k,1,is)=rr(i,k,is)+(td(i,k,is)*rra(i,k+1,1,is) &
                  +(tt(i,k,is)-td(i,k,is))*rxa(i,k+1,1,is))*denm
             rxa(i,k,1,is)= rs(i,k,is)+ts(i,k,is)  &
                  *rxa(i,k+1,1,is)*denm
             rra(i,k,2,is)=rra(i,k,1,is)
             rxa(i,k,2,is)=rxa(i,k,1,is)
          ENDDO
       ENDDO

       ! for middle clouds
       DO im=im1,im2

          DO k=icb-1,ict,-1
             DO i=1,m
                denm=ts(i,k,im)/( 1.0_r8-rs(i,k,im)*rxa(i,k+1,im,is) )
                rra(i,k,im,is)= rr(i,k,im)+(td(i,k,im)*rra(i,k+1,im,is) &
                     +(tt(i,k,im)-td(i,k,im))*rxa(i,k+1,im,is))*denm
                rxa(i,k,im,is)= rs(i,k,im)+ts(i,k,im) &
                     *rxa(i,k+1,im,is)*denm
             ENDDO
          ENDDO

       ENDDO       ! end im loop
    ENDDO  ! end is loop

    ! integration over eight sky situations.
    !     ih, im, is denotes high, middle and low cloud groups.
    DO ih=ih1,ih2

       ! clear portion
       IF(ih.eq.1) THEN
          DO i=1,m
             ch(i)=1.0_r8-cc(i,1)
          ENDDO
       ELSE
          ! cloudy portion
          DO i=1,m
             ch(i)=cc(i,1)
          ENDDO
       ENDIF

       DO im=im1,im2
          ! clear portion
          IF(im.eq.1) THEN
             DO i=1,m
                cm(i)=ch(i)*(1.0_r8-cc(i,2))
             ENDDO
          ELSE
             ! cloudy portion
             DO i=1,m
                cm(i)=ch(i)*cc(i,2)
             ENDDO
          ENDIF

          DO is=is1,is2
             ! clear portion
             IF(is.eq.1) THEN
                DO i=1,m
                   ct(i)=cm(i)*(1.0_r8-cc(i,3))
                ENDDO
             ELSE
                ! cloudy portion
                DO i=1,m
                   ct(i)=cm(i)*cc(i,3)
                ENDDO
             ENDIF

             ! add one layer at a time, going down.
             DO k=icb,np
                DO i=1,m
                   denm = ts(i,k,is)/( 1.0_r8-rsa(i,k-1,ih,im)*rs(i,k,is) )
                   tda(i,k,ih,im)= tda(i,k-1,ih,im)*td(i,k,is)
                   tta(i,k,ih,im)=  tda(i,k-1,ih,im)*tt(i,k,is)  &
                        +(tda(i,k-1,ih,im)*rr(i,k,is)  &
                        *rsa(i,k-1,ih,im)+tta(i,k-1,ih,im)-tda(i,k-1,ih,im))*denm
                   rsa(i,k,ih,im)= rs(i,k,is)+ts(i,k,is)  &
                        *rsa(i,k-1,ih,im)*denm
                ENDDO
             ENDDO

             ! add one layer at a time, going up.
             DO k=ict-1,1,-1
                DO i=1,m
                   denm =ts(i,k,ih)/(1.0_r8-rs(i,k,ih)*rxa(i,k+1,im,is))
                   rra(i,k,im,is)= rr(i,k,ih)+(td(i,k,ih)*rra(i,k+1,im,is) &
                        +(tt(i,k,ih)-td(i,k,ih))*rxa(i,k+1,im,is))*denm
                   rxa(i,k,im,is)= rs(i,k,ih)+ts(i,k,ih) &
                        *rxa(i,k+1,im,is)*denm
                ENDDO
             ENDDO

             ! compute fluxes following eq. (6.15) for fupdif and
             !     eq. (6.16) for (fdndir+fdndif)
             DO k=2,np+1
                DO i=1,m
                   denm= 1.0_r8/(1.0_r8-rsa(i,k-1,ih,im)*rxa(i,k,im,is))
                   fdndir(i)= tda(i,k-1,ih,im)
                   xx= tda(i,k-1,ih,im)*rra(i,k,im,is)
                   yy= tta(i,k-1,ih,im)-tda(i,k-1,ih,im)
                   fdndif(i)= (xx*rsa(i,k-1,ih,im)+yy)*denm

                   ! calculation of downward (fdn(i,k)) and upward (fupdif(i,k)) fluxes
                   ! as well as net flux
                   fupdif= (xx+yy*rxa(i,k,im,is))*denm
                   flxdn(i,k)= fdndir(i)+fdndif(i)-fupdif  ! net flux
                   fup(i,k)= (xx+yy*rxa(i,k,im,is))*denm   !new
                   fdn(i,k)= fdndir(i)+fdndif(i)  !new
                ENDDO
             ENDDO

             DO i=1,m
                flxdn(i,1)=1.0_r8-rra(i,1,im,is)
                fdn(i,1)=1.0_r8
                fup(i,1)=rra(i,1,im,is)
             ENDDO

             ! summation of fluxes over all sky situations;
             !     the term in the brackets of eq. (7.11)
             DO k=1,np+1
                DO i=1,m
                   IF(ih.eq.1 .and. im.eq.1 .and. is.eq.1) THEN
                      fclr(i,k)=flxdn(i,k)
                      fclr_d(i,k)=fdn(i,k)
                      fclr_u(i,k)=fup(i,k)
                   ENDIF
                   fall(i,k)=fall(i,k)+flxdn(i,k)*ct(i)
                   fall_d(i,k)=fall_d(i,k)+fdn(i,k)*ct(i)
                   fall_u(i,k)=fall_u(i,k)+fup(i,k)*ct(i)
                ENDDO
             ENDDO

             DO i=1,m
                fsdir(i)=fsdir(i)+fdndir(i)*ct(i)
                fsdif(i)=fsdif(i)+fdndif(i)*ct(i)
                !hmjb
                IF(ih.eq.1 .and. im.eq.1 .and. is.eq.1) THEN
                   fsdir_c(i)=fdndir(i)
                   fsdif_c(i)=fdndif(i)
                ENDIF
             ENDDO

          ENDDO ! end is loop
       ENDDO        ! end im loop
    ENDDO   ! end ih loop

  END SUBROUTINE cldflx
END MODULE Rad_Clirad
