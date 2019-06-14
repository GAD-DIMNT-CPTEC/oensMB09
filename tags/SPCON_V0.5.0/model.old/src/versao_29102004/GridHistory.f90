!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE GridHistory
  USE InputOutput, ONLY: &
       nfprt,            &
       nferr,            &
       ifprt,            &
       cnvray

  USE Options, ONLY: &
       yrl       , &
       monl      , &
       fNameGHLoc, &  
       fNameGHTable

  USE Utils, ONLY: &
      tmstmp2

  USE IOLowLevel, ONLY: & 
      WriteGrdHist      

  IMPLICIT NONE


  PRIVATE

  PUBLIC :: InitGridHistory
  PUBLIC :: StoreGridHistory
  PUBLIC :: StoreMaskedGridHistory
  PUBLIC :: WriteGridHistory
  PUBLIC :: TurnOnGridHistory
  PUBLIC :: TurnOffGridHistory
  PUBLIC :: IsGridHistoryOn

  INTERFACE StoreGridHistory
      MODULE PROCEDURE Store2D, Store1D, Store2DV, Store1DV
  END INTERFACE

  !------------------------------
  ! Grid History Available Fields
  !------------------------------
  !
  ! Each available field is stored at a fully private data structure composed by:
  !
  !  fdesc   -  field name                                (private parameter)
  !  surface -  if surface field or not                   (private parameter)
  !  lygf    -  number of vertical levels                 (private computed)
  !  iugf    -  code of field units                       (private parameter)
  !  tfrmgf  -  ????                        (not used)    (private parameter)
  !  alfd    -  field name for grads output (not used)    (private parameter)
  !
  ! These are arrays addressed by available field number.
  ! There are *ngfld* (private parameter) available fields. Each field
  ! has a mnemonic (public parameter) to identify the field in the
  ! fully private data structure, defined bellow:

  INTEGER              :: iMax
  INTEGER              :: jMax
  INTEGER              :: ibMax
  INTEGER              :: jbMax
  INTEGER, ALLOCATABLE :: ibPerIJ(:,:)
  INTEGER, ALLOCATABLE :: jbPerIJ(:,:)
  INTEGER              :: kMax
  ! # available fields

  INTEGER, PARAMETER :: ngfld=79

  ! public field mnemonic pointer to the data structure

  INTEGER, PUBLIC, PARAMETER :: ngpsf  =  1
  INTEGER, PUBLIC, PARAMETER :: ngtc   =  2
  INTEGER, PUBLIC, PARAMETER :: ngtg   =  3
  INTEGER, PUBLIC, PARAMETER :: ngtd   =  4
  INTEGER, PUBLIC, PARAMETER :: ngw1   =  5
  INTEGER, PUBLIC, PARAMETER :: ngw2   =  6
  INTEGER, PUBLIC, PARAMETER :: ngw3   =  7
  INTEGER, PUBLIC, PARAMETER :: ngcapac=  8
  INTEGER, PUBLIC, PARAMETER :: ngcapag=  9
  INTEGER, PUBLIC, PARAMETER :: ngsn   = 10
  INTEGER, PUBLIC, PARAMETER :: ngsf   = 11
  INTEGER, PUBLIC, PARAMETER :: ngz0   = 12
  INTEGER, PUBLIC, PARAMETER :: ngustr = 13
  INTEGER, PUBLIC, PARAMETER :: ngvstr = 14
  INTEGER, PUBLIC, PARAMETER :: ngshf  = 15
  INTEGER, PUBLIC, PARAMETER :: nglhf  = 16
  INTEGER, PUBLIC, PARAMETER :: ngto   = 17
  INTEGER, PUBLIC, PARAMETER :: ngkuo  = 18
  INTEGER, PUBLIC, PARAMETER :: ngdswt = 19
  INTEGER, PUBLIC, PARAMETER :: ngulwt = 20
  INTEGER, PUBLIC, PARAMETER :: ngdlwb = 21
  INTEGER, PUBLIC, PARAMETER :: ngulwb = 22
  INTEGER, PUBLIC, PARAMETER :: nguswt = 23
  INTEGER, PUBLIC, PARAMETER :: ngsvb  = 24
  INTEGER, PUBLIC, PARAMETER :: ngsvd  = 25
  INTEGER, PUBLIC, PARAMETER :: ngsnb  = 26
  INTEGER, PUBLIC, PARAMETER :: ngsnd  = 27
  INTEGER, PUBLIC, PARAMETER :: ngavb  = 28
  INTEGER, PUBLIC, PARAMETER :: ngavd  = 29
  INTEGER, PUBLIC, PARAMETER :: nganb  = 30
  INTEGER, PUBLIC, PARAMETER :: ngand  = 31
  INTEGER, PUBLIC, PARAMETER :: ngty   = 32
  INTEGER, PUBLIC, PARAMETER :: ngrd1  = 33
  INTEGER, PUBLIC, PARAMETER :: ngrd2  = 34
  INTEGER, PUBLIC, PARAMETER :: ngcsz  = 35
  INTEGER, PUBLIC, PARAMETER :: ngdrg  = 36
  INTEGER, PUBLIC, PARAMETER :: ngrm   = 37
  INTEGER, PUBLIC, PARAMETER :: ngrarh = 38
  INTEGER, PUBLIC, PARAMETER :: ngrb   = 39
  INTEGER, PUBLIC, PARAMETER :: ngrd   = 40
  INTEGER, PUBLIC, PARAMETER :: ngrc   = 41
  INTEGER, PUBLIC, PARAMETER :: ngrg   = 42
  INTEGER, PUBLIC, PARAMETER :: ngrs   = 43
  INTEGER, PUBLIC, PARAMETER :: ngea   = 44
  INTEGER, PUBLIC, PARAMETER :: ngta   = 45
  INTEGER, PUBLIC, PARAMETER :: ngect  = 46
  INTEGER, PUBLIC, PARAMETER :: ngeci  = 47
  INTEGER, PUBLIC, PARAMETER :: ngegt  = 48
  INTEGER, PUBLIC, PARAMETER :: ngegi  = 49
  INTEGER, PUBLIC, PARAMETER :: nglhbs = 50
  INTEGER, PUBLIC, PARAMETER :: nghc   = 51
  INTEGER, PUBLIC, PARAMETER :: nghg   = 52
  INTEGER, PUBLIC, PARAMETER :: ngchf  = 53
  INTEGER, PUBLIC, PARAMETER :: ngghf  = 54
  INTEGER, PUBLIC, PARAMETER :: ngrof  = 55
  INTEGER, PUBLIC, PARAMETER :: ngcnd  = 56
  INTEGER, PUBLIC, PARAMETER :: ngstr  = 57
  INTEGER, PUBLIC, PARAMETER :: ngu    = 58
  INTEGER, PUBLIC, PARAMETER :: ngv    = 59
  INTEGER, PUBLIC, PARAMETER :: ngt    = 60
  INTEGER, PUBLIC, PARAMETER :: ngq    = 61
  INTEGER, PUBLIC, PARAMETER :: ngswh  = 62
  INTEGER, PUBLIC, PARAMETER :: nglwh  = 63
  INTEGER, PUBLIC, PARAMETER :: nglslh = 64
  INTEGER, PUBLIC, PARAMETER :: ngclh  = 65
  INTEGER, PUBLIC, PARAMETER :: ngsclh = 66
  INTEGER, PUBLIC, PARAMETER :: ngdtvd = 67
  INTEGER, PUBLIC, PARAMETER :: nglsmc = 68
  INTEGER, PUBLIC, PARAMETER :: ngcmc  = 69
  INTEGER, PUBLIC, PARAMETER :: ngscmc = 70
  INTEGER, PUBLIC, PARAMETER :: ngdqvd = 71
  INTEGER, PUBLIC, PARAMETER :: ngduvd = 72
  INTEGER, PUBLIC, PARAMETER :: ngdvvd = 73
  INTEGER, PUBLIC, PARAMETER :: ngcld  = 74
  INTEGER, PUBLIC, PARAMETER :: ngctd  = 75
  INTEGER, PUBLIC, PARAMETER :: ngus   = 76
  INTEGER, PUBLIC, PARAMETER :: ngvs   = 77
  INTEGER, PUBLIC, PARAMETER :: ngts   = 78
  INTEGER, PUBLIC, PARAMETER :: ngqs   = 79

  ! field name  

  CHARACTER(LEN=40), PARAMETER :: fdesc(ngfld)= (/ &
       "SURFACE PRESSURE                        ", &
       "CANOPY TEMPERATURE                      ", &
       "GROUND/SURFACE COVER TEMPERATURE        ", &
       "DEEP SOIL TEMPERATURE                   ", &
       "SOIL WETNESS OF SURFACE ZONE            ", &
       "SOIL WETNESS OF ROOT ZONE               ", &
       "SOIL WETNESS OF RECHARGE ZONE           ", &
       "MOISTURE STORE ON CANOPY                ", &
       "MOISTURE STORE ON GROUND COVER          ", &
       "SNOW DEPTH                              ", &
       "SNOWFALL                                ", &
       "ROUGHNESS LENGTH                        ", &
       "SURFACE ZONAL WIND STRESS               ", &
       "SURFACE MERIDIONAL WIND STRESS          ", &
       "SENSIBLE HEAT FLUX FROM SURFACE         ", &
       "LATENT HEAT FLUX FROM SURFACE           ", &
       "TOTAL PRECIPITATION                     ", &
       "CONVECTIVE PRECIPITATION                ", &
       "INCIDENT SHORT WAVE FLUX                ", &
       "OUTGOING LONG WAVE AT TOP               ", &
       "DOWNWARD LONG WAVE AT GROUND            ", &
       "UPWARD LONG WAVE FLUX AT GROUND         ", &
       "UPWARD SHORT WAVE AT TOP                ", &
       "DOWNWARD SHORT WAVE FLUX AT GROUND (VB) ", &
       "DOWNWARD SHORT WAVE FLUX AT GROUND (VD) ", &
       "DOWNWARD SHORT WAVE FLUX AT GROUND (NB) ", &
       "DOWNWARD SHORT WAVE FLUX AT GROUND (ND) ", &
       "VISIBLE BEAM ALBEDO                     ", &
       "VISIBLE DIFFUSE ALBEDO                  ", &
       "NEAR INFRARED BEAM ALBEDO               ", &
       "NEAR INFRARED DIFFUSE ALBEDO            ", &
       "VEGETATION TYPE                         ", &
       "NET RADIATION OF CANOPY                 ", &
       "NET RADIATION OF GROUND SURFACE/COVER   ", &
       "COSINE OF ZENITH ANGLE                  ", &
       "DRAG                                    ", &
       "MOMENTUM FLUX RESISTANCE                ", &
       "CANOPY AIR SPC TO REF. LVL RESISTANCE   ", &
       "CANOPY AIR SPC TO CANOPY RESISTANCE     ", &
       "CANOPY AIR SPC TO GROUND RESISTANCE     ", &
       "CANOPY RESISTANCE                       ", &
       "GROUND COVER RESISTANCE                 ", &
       "BARE SOIL SURFACE RESISTANCE            ", &
       "VAPOR PRESSURE OF CANOPY AIR SPACE      ", &
       "TEMPERATURE OF CANOPY AIR SPACE         ", &
       "TRANSPIRATION FROM CANOPY               ", &
       "INTERCEPTION LOSS FROM CANOPY           ", &
       "TRANSPIRATION FROM GROUND COVER         ", &
       "INTERCEPTION LOSS FROM GROUND COVER     ", &
       "BARE SOIL EVAPORATION                   ", &
       "SENSIBLE HEAT FLUX FROM CANOPY          ", &
       "SENSIBLE HEAT FLUX FROM GROUND          ", &
       "CANOPY HEATING RATE                     ", &
       "GROUND/SURFACE COVER HEATING RATE       ", &
       "RUNOFF                                  ", &
       "HEAT CONDUCTION THROUGH SEA ICE         ", &
       "HEAT STORAGE TENDENCY OVER SEA ICE      ", &
       "ZONAL WIND (U)                          ", &
       "MERIDIONAL WIND (V)                     ", &
       "VIRTUAL TEMPERATURE                     ", &
       "SPECIFIC HUMIDITY                       ", &
       "SHORT WAVE RADIATIVE HEATING            ", &
       "LONG WAVE RADIATIVE HEATING             ", &
       "SUPERSATURATION LATENT HEATING          ", &
       "CONVECTIVE LATENT HEATING               ", &
       "SHALLOW CONVECTIVE HEATING              ", &
       "VERTICAL DIFFUSION HEATING              ", &
       "SUPERSATURATION MOISTURE SOURCE         ", &
       "CONVECTIVE MOISTURE SOURCE              ", &
       "SHALLOW CONVECTIVE MOISTENING           ", &
       "VERTICAL DIFFUSION MOISTENING           ", &
       "VERTICAL DIFFUSION DU/DT                ", &
       "VERTICAL DIFFUSION DV/DT                ", &
       "CLOUD COVER                             ", &
       "VERTICAL DIST TOTAL CLOUD COVER         ", &
       "SURFACE ZONAL WIND (U)                  ", &
       "SURFACE MERIDIONAL WIND (V)             ", &
       "SURFACE VIRTUAL TEMPERATURE             ", &
       "SURFACE SPECIFIC HUMIDITY               "    /)

  ! surface field or not

  LOGICAL, PARAMETER :: surface(ngfld)=    (/ &
       .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , &
       .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , &
       .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , &
       .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , &
       .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , &
       .TRUE. , .TRUE. , .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., &
       .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .TRUE. , .FALSE., .TRUE. , .TRUE. , &
       .TRUE. , .TRUE.                                /)

  ! units

  INTEGER, PARAMETER :: iugf(ngfld)=  (/ &
       131,  40,  40,  40,   0,   0,   0, 110, 110, 110, 120, &
        10, 130, 130, 170, 170, 120, 120, 170, 170, 170, 170, &
       170, 170, 170, 170, 170,   0,   0,   0,   0,   0, 170, &
       170,   0, 200, 190, 190, 190, 190, 190, 190, 190, 131, &
        40, 170, 170, 170, 170, 170, 170, 170, 170, 170, 120, &
       170, 170,  60,  60,  40,   0,  70,  70,  70,  70,  70, &
        70,  50,  50,  50,  50, 100, 100,   0,   0,  60,  60, &
        40,   0              /)  

  ! ???

  CHARACTER(LEN=1), PARAMETER :: tfrmgf(ngfld) =     (/ &
       "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "C", &
       "I", "C", "C", "P", "P", "C", "C", "C", "C", "C", "C", & 
       "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", &
       "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "P", & 
       "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", & 
       "C", "C", "I", "I", "I", "I", "C", "C", "C", "C", "C", & 
       "C", "C", "C", "C", "C", "C", "C", "C", "C", "I", "I", & 
       "I", "I" /)

  ! grads names

  CHARACTER(LEN=4), PARAMETER :: alfd(ngfld) =        (/&
       "PSLC", "TDSL", "TGSC", "TGDP", "USSL", "UZRS", "UZDS", "AUDL", "AUGC", "PNEV", "NEVE", &
       "ZORL", "USST", "VSST", "CSSF", "CLSF", "PREC", "PRCV", "FOCI", "ROLE", "OLIS", "OLES", &
       "ROCE", "CIVB", "CIVD", "CINB", "CIND", "ALVB", "ALVD", "ALIB", "ALID", "TIVG", "SRDL", &
       "SRGC", "CAZL", "DRAG", "RFLM", "RDNR", "RDAD", "RDAG", "RDSL", "RSGC", "RSBS", "PVDL", &
       "TADL", "TRDL", "PIDL", "TRGC", "PIGC", "EVBS", "CSDL", "CSGR", "TAQD", "TAQG", "RNOF", &
       "CAGM", "ACGM", "UVEL", "VVEL", "TEMV", "UMES", "AROC", "AROL", "CLSS", "CLCV", "ACVR", &
       "DVAQ", "FUSS", "FUCV", "UCVR", "DVUM", "DVTU", "DVTV", "CBNV", "VDCC", "UVES", "VVES", &
       "TEVS", "UESS" /)


  ! #  vertical levels

  INTEGER :: lygf(ngfld)   


  !-----------------------------
  ! Grid History Required Fields
  !-----------------------------
  !
  ! Each required field is stored at a fully private data structure composed by:
  !
  ! rdesc  -  field name        (private)
  ! iurgf  -  units of field    (private)
  !
  ! These are arrays addressed by required field number.
  ! There are *ngrfld* (private) required fields:

  ! # required fields

  INTEGER :: ngrfld

  ! grid history required field name

  CHARACTER(LEN=40), ALLOCATABLE :: rdesc(:)  

  ! desirable unit of grid history required field

  INTEGER,           ALLOCATABLE :: iurgf(:)  

  ! it is possible to select all available grid history
  ! fields:
  !
  ! allghf - if all available fields are required    (private)

  LOGICAL :: allghf 

  !--------------------------------------------
  ! Mapping Available Field into Required Field
  !--------------------------------------------
  !
  ! Each available field that is required is mapped into
  ! a required field number. Available fields not required
  ! are mapped to zero.
  !
  ! nghrq - gives the required field number of the
  !         available field number. Zero if the available
  !         field number is not required        (private)

  INTEGER :: nghrq(ngfld)


  !------------------------------------------
  ! Marking Available Field as Required Field
  !------------------------------------------
  !
  ! Each available field has an indicator if it is
  ! a required field. Grid history is computed only
  ! for these required fields.
  !
  ! isreq - if an available field is required    (private)

  LOGICAL :: isreq(ngfld)


  !------------------------------------
  ! Grid points to collect Grid History
  !------------------------------------
  !
  ! Required fields are collected on selected grid points.
  ! Each selected grid point has a name, longitude and
  ! latitude.
  !
  ! pdesc - grid point name                      (private)
  ! iloc  - grid point longitude (1 to iMax)     (private)
  ! jloc  - grid point latitude  (1 to jMax)     (private)
  ! alpt  - grid point coordinates (character)   (private)
  !
  ! There are *ngpts* selected points            (private)

  INTEGER :: ngpts 

  CHARACTER(LEN=40), ALLOCATABLE :: pdesc(:)
  INTEGER,           ALLOCATABLE :: iloc(:)
  INTEGER,           ALLOCATABLE :: jloc(:)
  CHARACTER(LEN=11), ALLOCATABLE :: alpt(:)


  !---------------------
  ! Storing Grid History
  !---------------------
  !
  ! Required fields at selected grid points are copied
  ! into a data structure for a single time step. The
  ! data structure is a rank two array; first dimension
  ! is addressed by the selected grid point number; 
  ! second dimension is addressed by a combination of 
  ! vertical level and required field number.
  !
  ! dignos - rank two array to store grid history  (private)

  REAL, ALLOCATABLE :: dignos(:,:)

  ! second dimension of dignos store all required grid
  ! history fields for a single grid history point (first
  ! dimension). Array locgf, addressed by available grid
  ! history field, points to the address of the second
  ! dimension of dignos where the first vertical of the
  ! available field is stored. If the available field
  ! is not stored, points to zero.
  ! the number of verticals is defined by lygf (see above)

  INTEGER :: locgf(ngfld)                          

  ! total number of verticals in *dignos* 

  INTEGER :: nghsl

  !----------------------------
  ! Turning Grid History On/Off
  !----------------------------
  !
  ! The input flag *grhflg* defines if this run will have
  ! grid history turned on or off. (private)

  LOGICAL :: grhflg

  ! Whenever *grhflg* is turned on, there are timesteps
  ! where Grid History will be collected, and there are
  ! timesteps where grid history will not be collected.
  ! These are controlled by the (private) variable
  ! *grhOn*, that is turned on/off by TurnOnGridHistory
  ! and TurnOffGridHistory. It is inquired by IsGridHistoryOn.
  ! Default is GridHistory turned off.

  LOGICAL :: grhOn

  ! Available Grid History fields are computed for a set of
  ! grid points concurrently. The set of computed grid points
  ! may have some or none selected grid points for Grid History.
  !
  ! Rank two array *dogrh*, indexed by available field and
  ! block of grid point number, indicates if the available field
  ! is required and if the block of computed grid points has
  ! selected grid history points

  LOGICAL, PUBLIC, ALLOCATABLE :: dogrh(:,:)

  ! Mapping grid points into selected grid history points is
  ! done by rank two array *mapgrh*, indexed by grid point
  ! and block number. Array values are zero if the grid point
  ! is not selected and the number of grid point history
  ! (first dimension index of dignos) otherwise.

  INTEGER, ALLOCATABLE :: mapgrh(:,:)

CONTAINS





  SUBROUTINE InitGridHistory (del, nexp, jttl, idate, &
       allghf_in, grhflg_in, igrfu, iptu, ighdr,iMax_in,jMax_in,&
       ibMax_in,jbMax_in,ibPerIJ_in,jbPerIJ_in,kMax_in)
    INTEGER,           INTENT(in) :: iMax_in
    INTEGER,           INTENT(in) :: jMax_in
    INTEGER,           INTENT(in) :: ibMax_in
    INTEGER,           INTENT(in) :: jbMax_in
    INTEGER,           INTENT(in) :: ibPerIJ_in(:,:)
    INTEGER,           INTENT(in) :: jbPerIJ_in(:,:)
    INTEGER,           INTENT(in) :: kMax_in
    REAL,              INTENT(in) :: del(:)
    CHARACTER(LEN= 4), INTENT(in) :: nexp
    CHARACTER(LEN=40), INTENT(in) :: jttl
    INTEGER,           INTENT(in) :: idate(4)
    LOGICAL,           INTENT(in) :: allghf_in
    LOGICAL,           INTENT(in) :: grhflg_in
    INTEGER,           INTENT(in) :: igrfu
    INTEGER,           INTENT(in) :: iptu
    INTEGER,           INTENT(in) :: ighdr

    CHARACTER(len= *), PARAMETER :: h="**(InitGridHistory)**"
    CHARACTER(len=20), PARAMETER :: typgh='GRID POINT HISTORY  '
    CHARACTER(len= 4), PARAMETER :: iacc='SEQU'
    CHARACTER(len= 4), PARAMETER :: idev='TAPE'
    CHARACTER(len=40) :: caux40
    CHARACTER(len=11) :: caux11
    INTEGER :: n
    INTEGER :: k
    INTEGER :: nn
    INTEGER :: ib
    INTEGER :: jb
    INTEGER :: iaux1
    INTEGER :: iaux2
    INTEGER :: ierr
    LOGICAL :: notfound
    
    ALLOCATE (ibPerIJ(iMax_in ,jMax_in ))
    ibPerIJ=-1
    ALLOCATE (jbPerIJ(iMax_in ,jMax_in ))
    jbPerIJ=-1

    ! store input data

    allghf = allghf_in
    grhflg = grhflg_in
    grhOn  = .FALSE.
   
    iMax    = iMax_in  
    jMax    = jMax_in
    ibMax   = ibMax_in
    jbMax   = jbMax_in
    ibPerIJ = ibPerIJ_in
    jbPerIJ = jbPerIJ_in
    kMax    = kMax_in
    ! if grid history not required, fix data structure and return

    IF (.NOT. grhflg) THEN
       ALLOCATE (dogrh(ngfld, jbMax))
       dogrh = .FALSE.
       RETURN
    END IF
       
    ! # vertical levels of available grid history fields

    DO n = 1, ngfld
       IF (surface(n)) THEN
          lygf(n) = 1
       ELSE
          lygf(n) = kMax
       END IF
    END DO

    ! dump available fields

    IF (ifprt(55) >= 1) THEN
       WRITE(nfprt,110)
       DO n = 1, ngfld
          WRITE(nfprt,140) n, fdesc(n), lygf(n), iugf(n), tfrmgf(n), alfd(n)
       END DO
    END IF

    ! # required fields
    OPEN(igrfu, file=TRIM(fNameGHTable),ACTION="read")      

    IF (allghf) THEN
       ngrfld = ngfld
    ELSE IF (igrfu  > 0) THEN
       ngrfld = 0
       DO
          READ (igrfu, 225, IOSTAT=ierr) caux40, iaux1
          IF (ierr == 0) THEN
             ngrfld = ngrfld + 1
          ELSE IF (ierr > 0) THEN
             WRITE (nfprt, "(a,' error reading unit ',i4)") h, igrfu
             WRITE (nferr, "(a,' error reading unit ',i4)") h, igrfu
             STOP h 
          ELSE
             EXIT
          END IF
       END DO
    END IF
    IF (ngrfld == 0 .AND. .NOT. allghf) THEN
       WRITE (nfprt,2135)
       WRITE (nferr,2135)
       STOP h
    END IF

    ! allocate required field data structure

    ALLOCATE (rdesc(ngrfld))
    ALLOCATE (iurgf(ngrfld))

    ! initialize required field data structure

    IF (allghf) THEN
       rdesc = fdesc
       iurgf = iugf
       nghrq = (/ (n, n = 1, ngfld) /)
       isreq = .TRUE.
    ELSE
       nghrq = 0
       isreq = .FALSE.
       REWIND igrfu
       IF (ifprt(55) > 0) WRITE(nfprt,210)
       DO n = 1, ngrfld
          READ (igrfu, 225) rdesc(n), iurgf(n)
          IF (ifprt(55) > 0) WRITE(nfprt,240) n, rdesc(n), iurgf(n)
          notfound = .TRUE.
          DO nn = 1, ngfld
             IF (rdesc(n) == fdesc(nn)) THEN
                nghrq(nn) = n
                isreq(nn) = .TRUE.
                notfound  = .FALSE.
                EXIT
             END IF
          END DO
          IF (notfound) THEN
             WRITE (nfprt, "(a,' required field ',a,' not available')") h, TRIM(rdesc(n))
             WRITE (nferr, "(a,' required field ',a,' not available')") h, TRIM(rdesc(n))
             STOP h
          END IF
       END DO
    END IF

    ! where to store each required field and total # of verticals

    nghsl = 0
    DO n = 1, ngfld
       IF (isreq(n)) THEN
          locgf(n) = nghsl + 1
          nghsl    = nghsl + lygf(n)
       ELSE
          locgf(n) = 0
       END IF
    END DO

    ! # grid points to collect grid history
    OPEN(iptu, file=TRIM(fNameGHLoc),ACTION="read")  
    ngpts = 0
    DO
       READ (iptu, 550, IOSTAT=ierr) caux40, iaux1, iaux2, caux11
       IF (ierr == 0) THEN
          ngpts = ngpts + 1
       ELSE IF (ierr > 0) THEN
          WRITE (nfprt, "(a,' error reading unit ',i4)") h, iptu
          WRITE (nferr, "(a,' error reading unit ',i4)") h, iptu
          STOP h 
       ELSE
          EXIT
       END IF
    END DO

    ! allocate grid point data structure

    ALLOCATE (pdesc(ngpts))
    ALLOCATE (iloc(ngpts))
    ALLOCATE (jloc(ngpts))
    ALLOCATE (alpt(ngpts))

    ! fill grid point data structure

    REWIND iptu
    DO n = 1, ngpts
       READ (iptu, 550) pdesc(n), iloc(n), jloc(n), alpt(n)
    END DO
    IF (ifprt(55) > 0) THEN
       WRITE(nfprt,520)
       DO n = 1, ngpts
          WRITE (nfprt, 570) n, pdesc(n), iloc(n), jloc(n), alpt(n)
       END DO
    END IF

    ! allocate and initialize grid history buffer

    ALLOCATE (dignos(ngpts, nghsl))
    dignos = 0.0

    ! mapping desired grid history points into grid points

    ALLOCATE (mapgrh(ibMax, jbMax))

    mapgrh = 0
    DO n = 1, ngpts
       ib = ibPerIJ(iloc(n),jloc(n))
       jb = jbPerIJ(iloc(n),jloc(n))
       mapgrh(ib,jb) = n
    END DO

    ! allocate and initialize where to do grid history

    ALLOCATE (dogrh(ngfld, jbMax))
    DO jb = 1, jbMax
       IF (ANY(mapgrh(:,jb) /= 0)) THEN
          dogrh(:, jb) = isreq(:)
       ELSE
          dogrh(:, jb) = .FALSE.
       END IF
    END DO

    ! dump 

    WRITE(ighdr,700)typgh
    WRITE(ighdr,720) nexp, iacc, iMax, jMax, kMax, kMax, ngpts, nghsl, nghsl, &
         idate, idev
    WRITE(ighdr,730) jttl
    WRITE(ighdr,740)(del(k),k=1,kMax)
    DO n=1,ngfld
       IF(isreq(n))THEN
          WRITE(ighdr,126) fdesc(n), lygf(n), iurgf(nghrq(n)), alfd(n)
       END IF
    END DO
    DO n=1,ngpts
       WRITE(ighdr,550)pdesc(n),iloc(n),jloc(n),alpt(n)
    END DO
    ENDFILE ighdr
110 FORMAT(' NO.',T10,'AVAILABLE GRID HISTORY FIELD DESCRIPTION', &
         T47,'NUM. OF LAYERS',T66,'UNITS',T75,'TIME FRAME')
125 FORMAT(A40,I5,2X,I5,2X,A1,1X,A4)
126 FORMAT(A40,I5,2X,I5,1X,A4)
140 FORMAT(' ',I4,' ',A40,I5,2X,I5,2X,A1,1X,A4)
210 FORMAT(' NO.',T10,'REQUESTED GRID HISTORY FIELD DESCRIPTION', &
         T55,'REQUESTED UNITS')
225 FORMAT(A40,I5)
240 FORMAT(' ',I4,' ',A40,I5)
520 FORMAT(' NO.',T17,'POINT DESCRIPTION',T51,'I PT.',T59,'J PT.')
550 FORMAT(A40,2I5,1X,A11)
570 FORMAT(' ',I4,' ',A40,I5,I5,1X,A11)
700 FORMAT(A20)
720 FORMAT(A4,1X,A4,11I5,1X,A4)
730 FORMAT(A40)
740 FORMAT(5E16.8)
1000 FORMAT(' I,J POINT LOCATIONS OUT OF RANGE OR OUT OF ORDER')
1635 FORMAT(' REQUESTED FIELD NUMBER',I4,' = ',A40/ &
         ' WAS NOT FOUND IN AVAILABLE GRID POINT HISTORY FIELD TABLE')
2135 FORMAT(' REQUESTED GRID POINT HISTORY FIELD TABLE EMPTY OR', &
         ' NOT FOUND'/' WITH ALLGHF=F')
  END SUBROUTINE InitGridHistory





  SUBROUTINE Store2D (field, fId, jb, cf)
    REAL,    INTENT(IN) :: field(:,:)
    INTEGER, INTENT(IN) :: fId
    INTEGER, INTENT(IN) :: jb
    REAL,    OPTIONAL, INTENT(IN) :: cf

    INTEGER :: dim1
    INTEGER :: dim2
    INTEGER :: kfirst
    INTEGER :: k
    INTEGER :: i
    INTEGER :: imap
    CHARACTER(LEN=*), PARAMETER :: h = "**(StoreGridHistory)**"

    IF (.NOT. IsGridHistoryOn()) THEN
       RETURN
    END IF
    dim1 = SIZE(field,1)
    dim2 = SIZE(field,2)
    IF (fId < 1 .OR. fId > ngfld) THEN
       WRITE (nfprt, "(a, ' fId out of range =', i10)") h, fId
       WRITE (nferr, "(a, ' fId out of range =', i10)") h, fId
       STOP h
    ELSE IF (jb < 1 .OR. jb > jbMax) THEN
       WRITE (nfprt, "(a, ' jb out of range =', i10)") h, jb
       WRITE (nferr, "(a, ' jb out of range =', i10)") h, jb
       STOP h
    ELSE IF (dim1 /= ibMax) THEN
       WRITE (nfprt, "(a, ' field first dimension out of range =', i10)") h, dim1
       WRITE (nferr, "(a, ' field first dimension out of range =', i10)") h, dim1
       STOP h
    ELSE IF (dim2 /= kMax) THEN
       WRITE (nfprt, "(a, ' field second dimension out of range =', i10)") h, dim2
       WRITE (nferr, "(a, ' field second dimension out of range =', i10)") h, dim2
       STOP h
    END IF

    kfirst = locgf(fId)
    IF (PRESENT(cf)) THEN
       DO k = 1, kMax
          DO i = 1, ibMax
             imap = mapgrh(i, jb)
             IF (imap /= 0) THEN
                dignos(imap,k+kfirst-1) = field(i, k) * cf
             END IF
          END DO
       END DO
    ELSE
       DO k = 1, kMax
          DO i = 1, ibMax
             imap = mapgrh(i, jb)
             IF (imap /= 0) THEN
                dignos(imap,k+kfirst-1) = field(i, k)
             END IF
          END DO
       END DO
    END IF
  END SUBROUTINE Store2D






  SUBROUTINE Store1D (field, fId, jb, cf)
    REAL,    INTENT(IN) :: field(:)
    INTEGER, INTENT(IN) :: fId
    INTEGER, INTENT(IN) :: jb
    REAL,    OPTIONAL, INTENT(IN) :: cf

    INTEGER :: dim1
    INTEGER :: kfirst
    INTEGER :: i
    INTEGER :: imap
    CHARACTER(LEN=*), PARAMETER :: h = "**(StoreGridHistory)**"

    IF (.NOT. IsGridHistoryOn()) THEN
       RETURN
    END IF
    dim1 = SIZE(field,1)
    IF (fId < 1 .OR. fId > ngfld) THEN
       WRITE (nfprt, "(a, ' fId out of range =', i10)") h, fId
       WRITE (nferr, "(a, ' fId out of range =', i10)") h, fId
       STOP h
    ELSE IF (jb < 1 .OR. jb > jbMax) THEN
       WRITE (nfprt, "(a, ' jb out of range =', i10)") h, jb
       WRITE (nferr, "(a, ' jb out of range =', i10)") h, jb
       STOP h
    ELSE IF (dim1 /= ibMax) THEN
       WRITE (nfprt, "(a, ' field first dimension out of range =', i10)") h, dim1
       WRITE (nferr, "(a, ' field first dimension out of range =', i10)") h, dim1
       STOP h
    END IF

    kfirst = locgf(fId)
    IF (PRESENT(cf)) THEN
       DO i = 1, ibMax
          imap = mapgrh(i, jb)
          IF (imap /= 0) THEN
             dignos(imap,kfirst) = field(i) * cf
          END IF
       END DO
    ELSE
       DO i = 1, ibMax
          imap = mapgrh(i, jb)
          IF (imap /= 0) THEN
             dignos(imap,kfirst) = field(i)
          END IF
       END DO
    END IF
  END SUBROUTINE Store1D






  SUBROUTINE WriteGridHistory (ighou, ifday, tod, idate)
    INTEGER, INTENT(IN) :: ighou
    INTEGER, INTENT(IN) :: ifday
    REAL,    INTENT(IN) :: tod
    INTEGER, INTENT(IN) :: idate(4)

    INTEGER :: i
    INTEGER :: j
    INTEGER :: m
    INTEGER :: n
    INTEGER :: iqstmp(6)
    INTEGER :: isg
    REAL    :: sg
    REAL    :: stmp(6)
    REAL    :: qwork(ngpts,nghsl)

    IF (.NOT. IsGridHistoryOn()) THEN
       RETURN
    END IF
    CALL tmstmp2 (idate, ifday, tod, iqstmp(3), iqstmp(4), iqstmp(5), iqstmp(6))
    sg = MOD(tod+0.03125,3600.)-0.03125
    isg=sg
    iqstmp(2)=isg/60
    iqstmp(1)=MOD(isg,60)
    DO j=1,6
       stmp(j)=iqstmp(j)
    END DO
    IF(.NOT.allghf)THEN
       m = 1
       DO n = 1, ngfld
          IF (isreq(n)) THEN
             CALL cnvray(dignos(1,m), lygf(n)*ngpts, iugf(n), iurgf(nghrq(n)))
             m=m+lygf(n)
          END IF
       END DO
    END IF
    DO j = 1, nghsl
       DO i = 1, ngpts
          qwork(i,j)=dignos(i,j)
       END DO
    END DO
    CALL WriteGrdHist(ighou,stmp,qwork)
    dignos = 0.0
  END SUBROUTINE WriteGridHistory






  SUBROUTINE StoreMaskedGridHistory (field, imask, fId, jb, cf)
    REAL,    INTENT(IN) :: field(ibMax)
    INTEGER, INTENT(IN) :: imask(ibMax)
    INTEGER, INTENT(IN) :: fId
    INTEGER, INTENT(IN) :: jb
    REAL,    OPTIONAL, INTENT(IN) :: cf

    INTEGER :: i
    INTEGER :: ncount
    REAL :: bfr(ibMax)

    IF (.NOT. IsGridHistoryOn()) THEN
       RETURN
    END IF
    ncount=0
    DO i = 1, ibMax
       IF (imask(i) >= 1) THEN
          ncount=ncount+1
          bfr(i) = field(ncount)
       ELSE
          bfr(i) = 0.0
       END IF
    END DO
    IF (PRESENT(cf)) THEN
       CALL StoreGridHistory (bfr, fId, jb, cf)
    ELSE
       CALL StoreGridHistory (bfr, fId, jb)
    END IF
  END SUBROUTINE StoreMaskedGridHistory





  SUBROUTINE TurnOnGridHistory()
    grhOn = grhflg
  END SUBROUTINE TurnOnGridHistory




  SUBROUTINE TurnOffGridHistory()
    grhOn = .FALSE.
  END SUBROUTINE TurnOffGridHistory



  FUNCTION IsGridHistoryOn()
    LOGICAL :: IsGridHistoryOn
    IsGridHistoryOn = grhflg .AND. grhOn
  END FUNCTION IsGridHistoryOn






  SUBROUTINE Store2DV (field, fId, jb, cf)
    REAL,    INTENT(IN) :: field(:,:)
    INTEGER, INTENT(IN) :: fId
    INTEGER, INTENT(IN) :: jb
    REAL,    INTENT(IN) :: cf(:)

    INTEGER :: dim1
    INTEGER :: dim2
    INTEGER :: dimcf
    INTEGER :: kfirst
    INTEGER :: k
    INTEGER :: i
    INTEGER :: imap
    CHARACTER(LEN=*), PARAMETER :: h = "**(StoreGridHistory)**"

    IF (.NOT. IsGridHistoryOn()) THEN
       RETURN
    END IF
    dim1 = SIZE(field,1)
    dim2 = SIZE(field,2)
    dimcf = SIZE(cf,1)
    IF (fId < 1 .OR. fId > ngfld) THEN
       WRITE (nfprt, "(a, ' fId out of range =', i10)") h, fId
       WRITE (nferr, "(a, ' fId out of range =', i10)") h, fId
       STOP h
    ELSE IF (jb < 1 .OR. jb > jbMax) THEN
       WRITE (nfprt, "(a, ' jb out of range =', i10)") h, jb
       WRITE (nferr, "(a, ' jb out of range =', i10)") h, jb
       STOP h
    ELSE IF (dim1 /= ibMax) THEN
       WRITE (nfprt, "(a, ' field first dimension out of range =', i10)") h, dim1
       WRITE (nferr, "(a, ' field first dimension out of range =', i10)") h, dim1
       STOP h
    ELSE IF (dim2 /= kMax) THEN
       WRITE (nfprt, "(a, ' field second dimension out of range =', i10)") h, dim2
       WRITE (nferr, "(a, ' field second dimension out of range =', i10)") h, dim2
       STOP h
    ELSE IF (dim1 /= dimcf) THEN
       WRITE (nfprt, "(a, ' field first dimension and cf dimension do not match',2i10)") h, dim1, dimcf
       WRITE (nferr, "(a, ' field first dimension and cf dimension do not match',2i10)") h, dim1, dimcf
       STOP h
    END IF

    kfirst = locgf(fId)
    DO k = 1, kMax
       DO i = 1, ibMax
          imap = mapgrh(i, jb)
          IF (imap /= 0) THEN
             dignos(imap,k+kfirst-1) = field(i, k) * cf(i)
          END IF
       END DO
    END DO
  END SUBROUTINE Store2DV






  SUBROUTINE Store1DV (field, fId, jb, cf)
    REAL,    INTENT(IN) :: field(:)
    INTEGER, INTENT(IN) :: fId
    INTEGER, INTENT(IN) :: jb
    REAL,    INTENT(IN) :: cf(:)

    INTEGER :: dim1
    INTEGER :: dimcf
    INTEGER :: kfirst
    INTEGER :: i
    INTEGER :: imap
    CHARACTER(LEN=*), PARAMETER :: h = "**(StoreGridHistory)**"

    IF (.NOT. IsGridHistoryOn()) THEN
       RETURN
    END IF
    dim1 = SIZE(field,1)
    dimcf = SIZE(cf,1)
    IF (fId < 1 .OR. fId > ngfld) THEN
       WRITE (nfprt, "(a, ' fId out of range =', i10)") h, fId
       WRITE (nferr, "(a, ' fId out of range =', i10)") h, fId
       STOP h
    ELSE IF (jb < 1 .OR. jb > jbMax) THEN
       WRITE (nfprt, "(a, ' jb out of range =', i10)") h, jb
       WRITE (nferr, "(a, ' jb out of range =', i10)") h, jb
       STOP h
    ELSE IF (dim1 /= ibMax) THEN
       WRITE (nfprt, "(a, ' field first dimension out of range =', i10)") h, dim1
       WRITE (nferr, "(a, ' field first dimension out of range =', i10)") h, dim1
       STOP h
    ELSE IF (dim1 /= dimcf) THEN
       WRITE (nfprt, "(a, ' field first dimension and cf dimension do not match',2i10)") h, dim1, dimcf
       WRITE (nferr, "(a, ' field first dimension and cf dimension do not match',2i10)") h, dim1, dimcf
       STOP h
    END IF

    kfirst = locgf(fId)
    DO i = 1, ibMax
       imap = mapgrh(i, jb)
       IF (imap /= 0) THEN
          dignos(imap,kfirst) = field(i) * cf(i)
       END IF
    END DO
  END SUBROUTINE Store1DV
END MODULE GridHistory
