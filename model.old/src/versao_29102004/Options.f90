!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE Options
  IMPLICIT NONE  
  PRIVATE
  INTEGER           , PUBLIC           :: trunc
  INTEGER           , PUBLIC           :: vert
  REAL              , PUBLIC           :: dt
  CHARACTER(LEN=200), PUBLIC           :: start
  INTEGER           , PUBLIC           :: IDATEI(4) 
  INTEGER           , PUBLIC           :: IDATEW(4) 
  INTEGER           , PUBLIC           :: IDATEF(4) 
  CHARACTER(LEN=6)  , PUBLIC           :: NMSST
  INTEGER           , PUBLIC           :: DHFCT  
  INTEGER           , PUBLIC           :: DHRES
  INTEGER           , PUBLIC           :: DHDHN
  INTEGER           , PUBLIC           :: NHDHN
  INTEGER           , PUBLIC           :: DHEXT
  INTEGER           , PUBLIC           :: NHEXT
  LOGICAL           , PUBLIC           :: DOGRH
  LOGICAL           , PUBLIC           :: DOPRC
  LOGICAL           , PUBLIC           :: DOSMC
  CHARACTER(LEN=5)  , PUBLIC           :: PREFX
  CHARACTER(LEN=5)  , PUBLIC           :: PREFY
  CHARACTER(LEN=1)  , PUBLIC           :: TABLE
  CHARACTER(LEN=200), PUBLIC           :: path_in
  CHARACTER(LEN=200), PUBLIC           :: dirfNameOutput
  
  INTEGER           , PUBLIC           :: maxtim
  REAL              , PUBLIC           :: cth0  
  REAL              , PUBLIC           :: dct 
  INTEGER           , PUBLIC           :: maxtfm
  REAL              , PUBLIC           :: ctdh0 
  REAL              , PUBLIC           :: dctd  
  INTEGER           , PUBLIC           :: mdxtfm
  REAL              , PUBLIC           :: cteh0 
  REAL              , PUBLIC           :: dcte  
  INTEGER           , PUBLIC           :: mextfm
  INTEGER           , PUBLIC           :: ddelt
  LOGICAL           , PUBLIC           :: intcosz
  REAL              , PUBLIC           :: dk
  REAL              , PUBLIC           :: tk
  LOGICAL           , PUBLIC           :: Model1D
  LOGICAL           , PUBLIC           :: GenRestFiles
  LOGICAL           , PUBLIC           :: rmRestFiles

  CHARACTER(LEN=5 ) , PUBLIC           :: EXTF
  CHARACTER(LEN=5 ) , PUBLIC           :: EXDF
  CHARACTER(LEN=5 ) , PUBLIC           :: EXTH
  CHARACTER(LEN=5 ) , PUBLIC           :: EXDH
  CHARACTER(LEN=5 ) , PUBLIC           :: EXTW
  CHARACTER(LEN=5 ) , PUBLIC           :: EXDW
  CHARACTER(LEN=5 ) , PUBLIC           :: EXTS

  INTEGER           , PUBLIC           :: reststep  
  CHARACTER(LEN=200), PUBLIC           :: fNameInput0
  CHARACTER(LEN=200), PUBLIC           :: fNameInput1
  CHARACTER(LEN=200), PUBLIC           :: fNameNmi
  CHARACTER(LEN=200), PUBLIC           :: fNameSSTAOI
  CHARACTER(LEN=200), PUBLIC           :: fNameSnow
  CHARACTER(LEN=200), PUBLIC           :: fNameSoilms 
  CHARACTER(LEN=200), PUBLIC           :: fNameAlbedo
  CHARACTER(LEN=200), PUBLIC           :: fNameCnftBl
  CHARACTER(LEN=200), PUBLIC           :: fNameCnf2Tb
  CHARACTER(LEN=200), PUBLIC           :: fNameLookTb
  CHARACTER(LEN=200), PUBLIC           :: fNameSibVeg
  CHARACTER(LEN=200), PUBLIC           :: fNameSibAlb
  CHARACTER(LEN=200), PUBLIC           :: fNameDTable
  CHARACTER(LEN=200), PUBLIC           :: fNameGHLoc  
  CHARACTER(LEN=200), PUBLIC           :: fNameGHTable
  CHARACTER(LEN=200), PUBLIC           :: fNameOrgvar
  CHARACTER(LEN=200), PUBLIC           :: fNameSibmsk
  CHARACTER(LEN=200), PUBLIC           :: fNameTg3zrl 
  REAL              , PUBLIC           :: deltOut
  LOGICAL           , PUBLIC           :: slagr
  LOGICAL           , PUBLIC           :: reducedGrid
  LOGICAL           , PUBLIC           :: linearGrid
  LOGICAL           , PUBLIC           :: nlnminit  
  LOGICAL           , PUBLIC           :: diabatic
  LOGICAL           , PUBLIC           :: eigeninit
  LOGICAL           , PUBLIC           :: rsettov


  INTEGER         , PUBLIC             :: maxtid
  INTEGER         , PUBLIC             :: ifilt
  CHARACTER(len=4), PUBLIC             :: irad
  CHARACTER(len=4), PUBLIC             :: iccon
  CHARACTER(len=4), PUBLIC             :: ilcon
  CHARACTER(len=4), PUBLIC             :: idcon
  CHARACTER(len=4), PUBLIC             :: iqadj
  CHARACTER(len=4), PUBLIC             :: ipbl
  CHARACTER(len=4), PUBLIC             :: ievap
  CHARACTER(len=4), PUBLIC             :: isens
  CHARACTER(len=4), PUBLIC             :: idrag
  CHARACTER(len=4), PUBLIC             :: iqdif
  CHARACTER(len=4), PUBLIC             :: ifft
  CHARACTER(len=4), PUBLIC             :: iscon
  CHARACTER(len=4), PUBLIC             :: igwd
  CHARACTER(len=4), PUBLIC             :: isimp
  CHARACTER(len=4), PUBLIC             :: ickcfl
  CHARACTER(len=4), PUBLIC             :: enhdif
  CHARACTER(LEN=4), PUBLIC             :: impdif


  REAL    , PUBLIC                     :: swint
  REAL    , PUBLIC                     :: trint
  REAL    , PUBLIC                     :: yrl
  INTEGER , PUBLIC                     :: idate (4)   
  INTEGER , PUBLIC                     :: idatec(4)    
  INTEGER , PUBLIC                     :: kt    
  INTEGER , PUBLIC                     :: ktm   
  INTEGER , PUBLIC                     :: ktp   
  INTEGER , PUBLIC                     :: jdt 
  INTEGER , PUBLIC                     :: monl  (12)         

  REAL    , PUBLIC                     :: co2val       
  REAL    , PUBLIC                     :: delt        
  REAL    , PUBLIC                     :: filta        
  REAL    , PUBLIC                     :: filtb 
  REAL    , PUBLIC                     :: fint      
  REAL    , PUBLIC                     :: percut       
  REAL    , PUBLIC                     :: sstlag
  REAL    , PUBLIC                     :: taucfl
  REAL    , PUBLIC                     :: ucrit
  REAL    , PUBLIC                     :: varcut             
  INTEGER , PUBLIC                     :: nfin0        
  INTEGER , PUBLIC                     :: nfin1         
  INTEGER , PUBLIC                     :: nfout0       
  INTEGER , PUBLIC                     :: nfout1       
  INTEGER , PUBLIC                     :: nfout2       
  INTEGER , PUBLIC                     :: nfclm0       
  INTEGER , PUBLIC                     :: nfclm1  
  INTEGER , PUBLIC                     :: nftgz0
  INTEGER , PUBLIC                     :: nftgz1
  INTEGER , PUBLIC                     :: nfsibt
  INTEGER , PUBLIC                     :: nfsibd         
  INTEGER , PUBLIC                     :: nfsibi       
  INTEGER , PUBLIC                     :: nfsibo       
  INTEGER , PUBLIC                     :: nfnmi        
  INTEGER , PUBLIC                     :: nfdbh        
  INTEGER , PUBLIC                     :: nfdrct       
  INTEGER , PUBLIC                     :: nfdiag       
  INTEGER , PUBLIC                     :: nf3d        
  INTEGER , PUBLIC                     :: nf2d 
  INTEGER , PUBLIC                     :: initlz                
  INTEGER , PUBLIC                     :: nstep        
  INTEGER , PUBLIC                     :: maxstp       
  INTEGER , PUBLIC                     :: isteps       
  INTEGER , PUBLIC                     :: ndord        
  INTEGER , PUBLIC                     :: nfiles       
  INTEGER , PUBLIC                     :: ifin        
  INTEGER , PUBLIC                     :: nfsst        
  INTEGER , PUBLIC                     :: nfalb        
  INTEGER , PUBLIC                     :: nfsnw        
  INTEGER , PUBLIC                     :: nfslm        
  INTEGER , PUBLIC                     :: nfcnv0       
  INTEGER , PUBLIC                     :: nfcnv1
  INTEGER , PUBLIC                     :: nfvar
  INTEGER , PUBLIC                     :: masci        
  INTEGER , PUBLIC                     :: intsst
  INTEGER , PUBLIC                     :: nfcldr   
  LOGICAL , PUBLIC                     :: ptime        
  LOGICAL , PUBLIC                     :: allghf 
  INTEGER , PUBLIC                     :: igfdu
  INTEGER , PUBLIC                     :: iptu
  INTEGER , PUBLIC                     :: ighdr
  INTEGER , PUBLIC                     :: ighou
  INTEGER , PUBLIC                     :: igrfu
  REAL    , PUBLIC                     :: dfilta       
  REAL    , PUBLIC                     :: dpercu       
  INTEGER , PUBLIC                     :: nfprt 
  INTEGER , PUBLIC                     :: nferr 
  INTEGER , PUBLIC                     :: ifprt (100)

  REAL    , PUBLIC                     :: vcrit       
  REAL    , PUBLIC                     :: alpha 
  REAL    , PUBLIC                     :: ucstr 
  REAL    , PUBLIC                     :: tcflst
  REAL    , PUBLIC                     :: ucupp 
  REAL    , PUBLIC                     :: tcflup
  REAL    , PUBLIC                     :: slupp 
  INTEGER , PUBLIC                     :: ifddp 
  LOGICAL , PUBLIC                     :: doprec
  INTEGER , PUBLIC                     :: nfprc   
  LOGICAL , PUBLIC                     :: dodyn   
  INTEGER , PUBLIC                     :: nfdyn   
  INTEGER , PUBLIC                     :: nfdhn  
  INTEGER , PUBLIC                     :: neprog   
  INTEGER , PUBLIC                     :: nedrct   
  INTEGER , PUBLIC                     :: nefcst
  LOGICAL , PUBLIC                     :: grhflg  


  REAL    , PUBLIC                     :: sthick
  REAL    , PUBLIC                     :: sacum
  REAL    , PUBLIC                     :: acum0
  REAL    , PUBLIC                     :: tbase
  REAL    , PUBLIC                     :: ubase
  REAL    , PUBLIC                     :: vbase
  REAL    , PUBLIC                     :: rbase
  REAL    , PUBLIC                     :: dbase
  REAL    , PUBLIC                     :: pbase
  REAL    , PUBLIC                     :: tfact
  REAL    , PUBLIC                     :: ufact
  REAL    , PUBLIC                     :: vfact
  REAL    , PUBLIC                     :: rfact
  REAL    , PUBLIC                     :: dfact
  REAL    , PUBLIC                     :: pfact
  INTEGER , PUBLIC                     :: mkuo
  INTEGER , PUBLIC                     :: mlrg
  INTEGER , PUBLIC                     :: is
  INTEGER , PUBLIC                     :: ki
  REAL    , PUBLIC                     :: cflric

  LOGICAL , PUBLIC                     :: mxrdcc
  INTEGER , PUBLIC                     :: lcnvl
  INTEGER , PUBLIC                     :: lthncl
  REAL    , PUBLIC                     :: rccmbl
  INTEGER , PUBLIC                     :: icld
  INTEGER , PUBLIC                     :: inalb
  INTEGER , PUBLIC                     :: mxiter

  INTEGER , PUBLIC                     :: istrt
  LOGICAL , PUBLIC                     :: first
  REAL    , PUBLIC                     :: dtc3x 
  REAL    , PUBLIC                     :: epsflt
  INTEGER , PUBLIC                     :: intg  
  INTEGER , PUBLIC                     :: ifalb 
  INTEGER , PUBLIC                     :: ifsst 
  INTEGER , PUBLIC                     :: ifslm 
  INTEGER , PUBLIC                     :: ifsnw 
  INTEGER , PUBLIC                     :: dogwd

  LOGICAL , PUBLIC, ALLOCATABLE        :: cdhl  (:) 
  PUBLIC :: InitResSet
  PUBLIC :: InitNamModel
  PUBLIC :: InitBlkdat
  PUBLIC :: SetTimeOutput


CONTAINS 


  SUBROUTINE InitResSet()

    NAMELIST /MODEL_RES/trunc,vert,dt,IDATEI,IDATEW,IDATEF,NMSST,&
              DHFCT,DHRES,DHDHN,NHDHN,DHEXT,NHEXT,DOGRH,&
              DOPRC,PREFX,PREFY,TABLE,path_in,dirfNameOutput

    READ (*,MODEL_RES)     
   
    grhflg = DOGRH 
    doprec = DOPRC 
    path_in=TRIM(path_in)//'/' 
      
    ddelt=dt
    delt=dt

    IF (ANY(IDATEW /= IDATEF)) THEN
       start='warm'
    ELSE
       start='cold'
    END IF

    IF( TRIM(start) == "warm" )THEN
       CALL SetTimeOutput(IDATEW ,IDATEF, dhfct ,nhdhn ,dhdhn ,nhext ,dhext )
    ELSE
       CALL SetTimeOutput(IDATEI ,IDATEF, dhfct ,nhdhn ,dhdhn ,nhext ,dhext )
    END IF

    reststep=NINT((DHRES*3600)/dt)
    deltOut=dhfct*3600
    idate =IDATEI
    idatec=IDATEW

  END SUBROUTINE InitResSet







  SUBROUTINE InitNamModel() 

    CHARACTER(LEN=200)              :: SSTF
    LOGICAL                         :: lexist
    NAMELIST /MODEL_IN/nlnminit,diabatic,eigeninit, &
                       rsettov,intcosz,Model1D, &
                       GenRestFiles,rmRestFiles
		     
    NAMELIST /PHYSPROC/irad ,iccon,ilcon,iqdif, &
                       iscon,igwd ,isimp,enhdif
		       
    NAMELIST /PHYSCS/mxrdcc,lcnvl ,lthncl ,rccmbl ,swint  , &
		     trint ,icld  ,inalb  ,mxiter ,co2val , &
		     sthick,sacum ,acum0  ,tbase  ,mlrg   , &
                     is    ,ki, cflric

    NAMELIST /COMCON/initlz , nstep  , fint   , intsst , &
                     sstlag , maxstp , isteps , ndord  , &
                     filta  , percut , varcut , ifsst  , &
                     ifsnw  , ifalb  , ifslm  , allghf , &
		     dpercu , vcrit  , alpha  , dodyn  , dk, tk

    READ (*,MODEL_IN)
    slagr	     =.FALSE.  ! slg	    --> .FALSE. 
    reducedGrid      =.FALSE.  ! reduced    --> .FALSE. 
    linearGrid       =.FALSE.  ! linear     --> .FALSE. 
    READ (*,PHYSPROC)
    READ (*,PHYSCS)
    READ (*,COMCON)

    CALL  COLDWARM()

    CALL SETSST (trunc,NMSST,IDATEI,START,path_in,SSTF,ifsst)

    fNameSSTAOI=TRIM(SSTF)
    INQUIRE (FILE=TRIM(fNameSSTAOI),exist=lexist)
    IF(lexist) THEN
       PRINT*,'* The  ', TRIM(fNameSSTAOI) ,' file exist *'
    ELSE
       PRINT*,'* The  ', TRIM(fNameSSTAOI), ' file not exist*'
       STOP
    END IF
    dtc3x  = 0.0
    maxtid=(51*366*86400)/dt
    filtb =(1.0-filta)*0.5
    !     intg=2  time integration of surface physical variable is done
    !     by leap-frog implicit scheme. this conseves enegy and h2o.
    !     intg=1  time integration of surface physical variable is done
    !     by backward implicit scheme.
    !
    intg =2
    IF(intg == 1) THEN
       epsflt=0.0e0
    ELSE
       epsflt=0.5e0 *(1.0e0 -filta)
    END IF
    ALLOCATE(cdhl(0:maxtid))
  END SUBROUTINE InitNamModel







  SUBROUTINE InitBlkdat

    slagr         =.FALSE.  ! slg          --> .FALSE. 
    reducedGrid   =.FALSE.  ! reduced      --> .FALSE. 
    linearGrid    =.FALSE.  ! linear       --> .FALSE. 
    nlnminit      =.TRUE.   ! nlNmiInit    --> .FALSE. 
    diabatic      =.TRUE.   ! diabatic     --> .TRUE.  
    eigeninit     =.FALSE.  ! eigenInit    --> .FALSE. 
    rsettov       =.TRUE.   ! rsettov      --> .FALSE. 
    intcosz       =.TRUE.   ! intcosz      --> .TRUE.
    Model1D       =.FALSE.  ! Model1D      --> .FALSE. ! Flag set to .TRUE. when running 1D model
    GenRestFiles  =.FALSE.  ! GenRestFiles --> .FALSE. ! Flag to generate restart files
                                                       ! Used as .TRUE. when restart is in use
    rmRestFiles   =.FALSE.  ! rmRestFiles  --> .FALSE. ! Flag to remove restart files after
                                                       ! read them when .TRUE.


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  dt time interval,usually =delt,but changes			             !
!  in nlnmi (dt=1.) and at dead start(delt/4,delt/2)		             !
!  oldtim whether input    is in old time style of fhour,idate               !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SWINT     =    1.000000
    TRINT     =    3.000000
    YRL       =  365.2500
    KT        =    0
    KTM       =   -1
    KTP       =    0
    JDT       =    0
    MONL      =   (/31,28,31,30,31,30,31,31,30,31,30,31/)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !     ixxxx=yes  the physical process included
    !     ixxxx=no   the physical process excluded
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    IRAD    = 'YES ' ! irad =yes:radiation
    ICCON   = 'YES ' ! iccon=yes:cumulus convection(kuo)
    ILCON   = 'YES ' ! ilcon=yes:large scale condensation
    IDCON   = 'NO  ' ! idcon=yes:dry convection
    IQADJ   = 'NO  ' ! iqadj=yes:mixing of moisture in dry unstable layers
    IPBL    = 'YES ' ! ipbl =yes:vertical diffusion of momentum,heat & moisture
    IEVAP   = 'YES ' ! ievap=yes:air surface exchange of moisture
    ISENS   = 'YES ' ! isens=yes:air surface exchange of sensible heat
    IDRAG   = 'YES ' ! idrag=yes:drag at the earth's surface
    IQDIF   = 'YES ' ! iqdif=yes:horizontal diffusion of moisture
    IFFT    = 'JMA ' ! ifft =jma:calls jma fft end   ifft =cyb : calls cyb fft
    ISCON   = 'YES ' ! iscon=yes:shallow convection this process follows cumulus 
                     ! convection
    IGWD    = 'YES ' ! igwd =yes:gravity wave 
    ISIMP   = 'NO  ' ! isimp=yes:simplified physics version. 
    ICKCFL  = 'NO  ' ! ickcfl=yes: check and adjust for cfl instability
    ENHDIF  = 'YES ' ! enhdif=yes: enhance diffusion on higher levels )
    IMPDIF  = 'YES ' ! IMPDIF=yes: implicit diffusion  ) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !     files
    !     ifxxx=0    xxx is not processed
    !     ifxxx=1    xxx is set to month=idatec(2) in the first call,
    !                but not processed from the subsequent calls.
    !                ifxxx is set to zero after interpolation
    !     ifxxx=2    xxx is interpolated to current day and time every fint
    !                hours synchronized to 00z regardless of initial time.
    !                interpolation is continuous (every time step) if fint<0.
    !     ifxxx=3    xxx is interpolated to current day and time when ifday=0
    !                and tod=0.0 but not processed otherwise
    !                ( appropriate only when xxx is predicted )
    !
    !                the following are for sst only (fint applies as in
    !                ifxxx=2):
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    nfin0   =18     ! input  file at time level t-dt
    nfin1   =18     ! input  file at time level t
    nfout0  =20     ! output file at time level t-dt
    nfout1  =21     ! output file at time level t
    nfout2  =21     ! output file at t=0 ( normal-mode initialized )
    nfclm0  =10     ! sst,soil moisture etc.  input
    nfclm1  =11     ! sst,soil moisture etc.  output
    nftgz0  =61     ! ground temperature and roughness length input
    nftgz1  =61     ! ground temperature and roughness length output
    nfsibt  =99     ! sib surface vegetation type
    nfsibd  =88     ! sib vegetation parameter
    nfsibi  =77     ! sib prognostic variable input  file
    nfsibo  =66     ! sib prognostic variable output file
    nfnmi   =80     ! normal modes
    nfdbh   =75     ! heating rate used for diabatic nlnmi
    nfcldr  =74     !
    nfdrct  =25     ! directory for diagnostics
    nfdiag  =26     ! diagnostics
    nf3d  =27       ! intermediate 3-d diagnostics
    nf2d  =28       ! intermediate 2-d diagnostics
    nfcnv0  =0      ! initial information on convective clouds for int. radiation
    nfcnv1  =32     ! output information on convective clouds for int. radiation
    nfvar   =33     ! surface height variance
    initlz  =2      ! nitlz=2 diabatic normal mode initialization
                    !      =1 diabatic with no normal mode initialization
                    !      =0 adiabatic with no normal mode initialization
                    !      <0 same as >0 with sib variables read in instead of 
                    !         initialized
    nstep   =1      ! number of steps in getting diabatic heating rate
    ! in diaten if nstep=1,nstep is set equal to 7 in qsmf.    
    masci   =0      ! mass conservation interval in hours 
    fint  =6        ! surface boundary calling interval in hours
    !
    intsst  =7      ! sst data set interval in days (if > 0)
    ! sst data set is in calendar months if < 0.
    sstlag  =3.5    ! starting time of sst data in days before i.c. date 
    ! if intsst > 0.  starting time of sst data in months
    ! before i.c. date if intsst < 0.
    maxstp  =06     ! maxstp*delt is interval (in sec) of history file output
    isteps  =01     ! maxstp*delt*isteps is forecasted time in sec
    ndord   =4      ! order (even) of horizontal diffusion del operator
    nfiles  =1      ! nfiles   if nfiles=1, normal modes are all in one big file.
    ! if nfiles>1, normal modes are in "mods" smaller files.
    ifin  =0      ! a switch controlling the form of the time stamp
    filta  =0.92e0  ! time filter constant
    filtb  =(1.0-filta)*0.5!
    percut =27502.  ! percut   cut off period in sec  in nlnmi
    ! modes are to be read.
    varcut =1.6e5   ! cut off height variance in m**2 for gravity wave drag
    nfsst  =50      ! sst   file
    nfsnw  =51      ! snow   file
    nfalb  =52      ! albedo file
    nfslm  =53      ! soil moisture file
    ifsst  =-1      ! ifsst=4  sst is linearly interpolated from continuous 
    !              direct access data set to current day and 
    !              time.data set is assumed to be spaced every 
    !              intsst days or every calendar month is 
    !              intsst < 0.
    ! ifsst=5  sst is expanded from piecewise cubic 
    !              coefficients in direct access data set to 
    !              current day and time. data set
    !  is assumed to be spaced every intsst days.
    ifsnw  =3       !
    ifalb  =0       !
    ifslm  =3       !
    co2val =345.0   ! co2val is wgne standard value in ppm
    ucrit  =100.0   ! critical velocity (m/s) at which damping kicks in
    ! (for troposphere)
    taucfl =86400.0 ! damping time constant (sec) (for troposphere)
    nfprt=6         !
    nferr=0         !
    ifprt=100*3     !
    ptime=.TRUE.    !
    allghf=.FALSE.  ! it is possible to select all available grid history
                    ! fields:  
                    !
                    ! allghf=.TRUE.-  all available fields are required
    igfdu=41        !  
    iptu=42         !   iptu=42
    ighdr=43        !   ighdr=43
    ighou=91        !   ighou=44
    igrfu=45        !   igrfu=45
    dfilta=0.92     !
    dpercu=27502.   !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !    SHENEIDER/COLA (troposphere), CPTEC (stratosphere):
    !    parameters for controlling cfl instability
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    vcrit  =   85.00 ! critical velocity (m/s) at which damping kicks in
    ! (for troposphere)
    alpha  =    2.50 !
    ucstr  =   85.0  ! ucrit for lower stratosphere
    tcflst =21600.0  ! taucfl for lower stratosphere
    ucupp  =   70.0  ! ucrit for upper stratosphere
    tcflup = 2160.0  ! taucfl for upper stratosphere
    slupp  =    0.020!
    ifddp  =   10    !
    doprec =.FALSE.  ! logical flag to output
    ! instantaneous total precipitation
    ! and convective precipitation
    ! at every time step
    nfprc  =93       ! nfprc  = unit file number to output
    ! instantaneous total precipitation
    ! and convective precipitation
    !
    dodyn  =.FALSE.  ! logical flag to output
    ! first level of divergence, vorticity,
    ! virtual temperature, specific humidity
    ! and log of surface pressure
    ! at every time step
    nfdyn  =94       ! unit file number to output
    ! first level of divergence, vorticity,
    ! virtual temperature, specific humidity
    ! and log of surface pressure
    ! at every time step
    nfdhn  =92       ! nfdhn  : unit file number to output
    !      ustress and vstress at surface
    neprog =81       ! neprog : unit file number to output
    !      extra prognostics
    nedrct =82       ! nedrct : unit file number to output
    !      description of extra prognostics
    nefcst =83       ! nefcst : unit file number to output
    !          files list of extra prognostics
    grhflg = .FALSE. ! grhflg : logical (T or F) to do grid history for
    !          selected points
    STHICK=0.65e0                ! sthick; upper limit for originating air for lcl.
    ! replaces kthick.
    SACUM=0.46e0                 ! sacum; top level for integrated moisture 
    ! convergence test. replaces
    ! kacum
    ACUM0=-2.0e-8                ! acum0; threshold moisture convergence such that 
    ! integrated moisture
    ! convergence > - acum0 for convection to occur.
    TBASE=273.15e00              !
    UBASE=  0.0e00               !
    VBASE=  1.0e03               !
    RBASE= 30.0e00               !
    DBASE=  2.0e07               !
    PBASE= 10.0e00               !
    TFACT=  0.000000000000000E+00!
    UFACT=  0.000000000000000E+00!
    VFACT=  0.000000000000000E+00!
    RFACT=  0.000000000000000E+00!
    DFACT=  0.000000000000000E+00!
    PFACT=  0.000000000000000E+00!
    MKUO=0                       ! mkuo=1 ;output of pre-adjusted & post adjusted 
    ! temp. & s.h. in kuolcl
    MLRG=0                       ! mlrg=1 ;output of pre-adjusted & post adjusted 
    ! temp. & s.h. in lrgscl
    is=1                         ! is  ;start i-point
    ki=1                         ! ki  ; lowest level from which parcels can be 
    ! lifted to find lcl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !   the following are vertical resolution dependent cloud parameters
    !   used in cldgen.  correct settings for these parameters need to be
    !   determined experimentally.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    mxrdcc=.TRUE. ! use maximum random converage for radiative conv. clouds
    lcnvl =2      ! the lowest layer index where non-convective clouds can
    ! occur (ben says this should be 2 or more)
    lthncl=80     ! minimum depth in mb of non-zero low level cloud
    rccmbl=3.0    ! radiative convective cloud minimum base layer index
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !   physical constants for simple physics options
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    icld  =1      !  
    inalb =2      !
    mxiter=200    !

    ! Horizontal Diffusion Coefficients (MRF)
    dk=8.0E+15
    tk=6.0E+15

  END SUBROUTINE InitBlkdat






  SUBROUTINE SetTimeOutput(idate ,idatec,dhfct ,nhdhn ,dhdhn ,nhext ,dhext )
    IMPLICIT NONE
    INTEGER, INTENT(IN     ) :: idate (4) 
    INTEGER, INTENT(IN     ) :: idatec(4)
    INTEGER, INTENT(IN     ) :: dhfct
    INTEGER, INTENT(INOUT  ) :: nhdhn
    INTEGER, INTENT(INOUT  ) :: dhdhn
    INTEGER, INTENT(INOUT  ) :: nhext
    INTEGER, INTENT(INOUT  ) :: dhext
    INTEGER                  :: yi
    INTEGER                  :: mi
    INTEGER                  :: di
    INTEGER                  :: hi
    INTEGER                  :: yf
    INTEGER                  :: mf
    INTEGER                  :: df
    INTEGER                  :: hf
    INTEGER                  :: ntstepmax
    REAL                     :: xday
    REAL                     :: datehr
    REAL                     :: datehf
    INTEGER                  :: nday
    REAL                     :: ybi
    INTEGER                  :: md(12)
    INTEGER                  :: ntstep
    INTEGER                  :: mhfct
    INTEGER                  :: mhdhn
    INTEGER                  :: mhext
    REAL                     :: dh 
    REAL                     :: nts
    REAL                     :: mhf
    REAL                     :: chk

    hi = idate (1)
    di = idate (2)
    mi = idate (3)
    yi = idate (4)  
    hf = idatec(1)
    df = idatec(2)
    mf = idatec(3)
    yf = idatec(4)

    CALL jull(yi,mi,di,hi,xday)
    datehr=yi+(xday/365.25e0)
    CALL jull(yf,mf,df,hf,xday)
    datehf=yf+(xday/365.25e0)
    nday=0
    IF(yi == yf .AND. mi==mf .AND. di==df) THEN
       nday=0
    ELSE
       DO WHILE (datehr < datehf)
          nday=nday+1
          ybi=MOD(yi,4)
          IF ( ybi == 0.0 )THEN
             md =(/31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
          ELSE
             md =(/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
          END IF
          di=di+1
          IF( di > md(mi) )THEN
             di=1
             mi=mi+1
             IF ( mi > 12 ) THEN
                mi=1
                yi=yi+1
             END IF
          END IF
          CALL jull(yi,mi,di,hi,xday)
          datehr=yi+(xday/365.25e0)
       END DO
    END IF
    ntstep=(nday)*86400/dt
    IF(dhfct /= 0 ) THEN
       mhfct=nday*24/dhfct
    ELSE
       mhfct=17
    END IF

    IF( nhdhn == 0 ) THEN
       dhdhn=0
    ELSE IF ( dhdhn /= 0 ) THEN
       mhdhn=nhdhn/dhdhn
    ELSE
       mhdhn=0
       nhdhn=0
    END IF
    IF ( nhext == 0 ) THEN
       dhext=0
       !**(JP)** faltou atribuicao abaixo
       mhext=0
    ELSE IF ( dhext /= 0 ) THEN
       mhext=nhext/dhext
    ELSE
       mhext=0  
       nhext=0
    END IF
    IF ( dhfct /= 0 ) THEN
       IF ( hi /= hf ) THEN
          dh =hf-hi
          nts=dh*3600/dt
          mhf=dh/dhfct
          chk=mhf*dhfct
          IF ( chk /= dh ) THEN
             WRITE(*,*) 'Wrong Request for the Hour in datef =', yf,mf,df,hf
             WRITE(*,*) 'Difference of Hours in datei = ',yi,mi,di,hi, 'and '
             WRITE(*,*) 'datef is Not Compatible With dhfct =' ,dhfct
             STOP
          END IF
          ntstep=ntstep+nts
          mhfct=mhfct+mhf
       END IF
    END IF
    maxtim=ntstep
    maxtfm=mhfct
    cth0 =dhfct
    mdxtfm=mhdhn
    ctdh0=dhdhn
    mextfm=mhext
    cteh0=dhext
    ntstepmax=51*366*86400/dt
    IF( ntstep > ntstepmax ) THEN
       WRITE(*,*) 'nstep = ',ntstep,' is greater than ntstepmax = ',ntstepmax
       STOP
    END IF
    dct=cth0  
    dctd=ctdh0
    dcte=cteh0  
  END SUBROUTINE SetTimeOutput






  SUBROUTINE jull(yi,mi,di,hi,xday)
    IMPLICIT NONE
    INTEGER, INTENT(IN   ) :: yi
    INTEGER, INTENT(IN   ) :: mi
    INTEGER, INTENT(IN   ) :: di
    INTEGER, INTENT(IN   ) :: hi
    REAL   , INTENT(OUT  ) :: xday
    REAL                   :: tod
    REAL                   :: yrl
    INTEGER                :: monl(12)
    INTEGER                :: monday(12)
    INTEGER                :: m
    REAL   , PARAMETER     :: f3600=3.6e3
    tod=0.0
    yrl=365.25e0
    MONL    =   (/31,28,31,30,31,30,31,31,30,31,30,31/) 
    !     
    !     id is now assumed to be the current date and hour
    !  
    monday(1)=0
    DO m=2,12
       monday(m)=monday(m-1)+monl(m-1)
    END DO
    xday=hi*f3600
    xday=xday+MOD(tod,f3600)
    xday=monday(mi)+di+xday/86400.0
    xday=xday-MOD(yi+3,4)*.25
    IF(MOD(yi,4).EQ.0.AND.mi.GT.2)xday=xday+1.0e0
    xday= MOD(xday-1.0,yrl)
  END SUBROUTINE jull


  SUBROUTINE setsst (trunc,SST,idate,START,path,SSTF,ifsst)
    INTEGER          , INTENT(IN   ) :: trunc 
    CHARACTER(LEN=* ), INTENT(INOUT) :: SST
    INTEGER          , INTENT(IN   ) :: idate(4)
    CHARACTER(LEN=* ), INTENT(IN   ) :: START
    CHARACTER(LEN=* ), INTENT(IN   ) :: path
    CHARACTER(LEN=* ), INTENT(OUT  ) :: SSTF
    INTEGER          , INTENT(OUT  ) :: ifsst

    INTEGER           :: LFSST
    INTEGER           :: LWSST 
    CHARACTER(LEN=8)  :: LABELS
    CHARACTER(LEN=10) :: TRC
    LOGICAL           :: lexist

    IF(LEN(TRIM(SST)) /= 6)THEN
       WRITE(*,*)'It is not set file sst :',SST
       STOP
    END IF

    IF(trunc < 100)WRITE(TRC,'(2a1,i2)')'T','0',trunc
    IF(trunc >= 100 .AND. trunc < 1000)WRITE(TRC,'(a1,i3)')'T',trunc
    IF(trunc >= 1000)WRITE(TRC,'(a1,i4)')'T',trunc 

    WRITE(LABELS,'(i4.4,2i2.2)')idate(4),idate(3),idate(2)

    IF ( TRIM(SST) == 'sstwkl' ) THEN
       INQUIRE (FILE=TRIM(path)//TRIM(SST)//LABELS//'.'//TRIM(TRC) ,exist=lexist)
       IF(lexist) THEN
          SSTF=TRIM(path)//TRIM(SST)//LABELS//'.'//TRIM(TRC)
       ELSE
          SST='sstaoi'
          PRINT*,'*****************************************************'
          PRINT*,'* SST changed from weekly running mean (sstwkl) *'
          PRINT*,'*      to climatology (sstaoi)  *'
          PRINT*,'* sstwkl s are unavailable for the last 15 days *'
          PRINT*,'*****************************************************'
          SSTF=TRIM(path)//TRIM(SST)//'.'//TRIM(TRC)
       END IF
    ELSE IF ( TRIM(SST) == 'sstwkd' ) THEN
       SSTF=TRIM(path)//TRIM(SST)//'.'//TRIM(TRC)
    ELSE IF ( TRIM(SST) == 'sstmtd' ) THEN
       SSTF=TRIM(path)//TRIM(SST)//LABELS//'.'//TRIM(TRC)
    ELSE IF ( TRIM(SST) == 'sstanp' ) THEN
       SSTF=TRIM(path)//TRIM(SST)//LABELS//'.'//TRIM(TRC)
    ELSE
       SST='sstaoi'
       SSTF=TRIM(path)//TRIM(SST)//'.'//TRIM(TRC)
    END IF

    IF ( TRIM(SST) == 'sstwkl' ) THEN
       LFSST=-1
    ELSE
       LFSST=2
    END IF

    IF      ( TRIM(SST) == 'sstwkd' ) THEN
       LFSST=4
    ELSE IF ( TRIM(SST) == 'sstmtd' ) THEN
       LFSST=4
    END IF

    IF (TRIM(START)  == 'warm') THEN
       IF ( LFSST == -1 ) THEN
          LWSST=0
       ELSE
          LWSST=LFSST
       END IF
    END IF

    IF ( TRIM(START) /= 'warm')THEN
       ifsst=LFSST
    ELSE
       ifsst=LWSST
    END IF
  END SUBROUTINE setsst






  SUBROUTINE  coldwarm()

    CHARACTER(LEN= 10)                 :: LABELI
    CHARACTER(LEN= 10)                 :: LABELC
    CHARACTER(LEN= 10)                 :: LABELF
    CHARACTER(LEN=  4)                   :: TRC
    CHARACTER(LEN=  4)                   :: LV
    INTEGER                              :: i
    LOGICAL                              :: lexist
    IF(TRIM(START) == 'cold') idatec=idatef
    WRITE(LABELI,'(i4.4,3i2.2)')idate(4),idate(3),idate(2),idate(1)
    WRITE(LABELC,'(i4.4,3i2.2)')idatec(4),idatec(3),idatec(2),idatec(1)
    WRITE(LABELF,'(i4.4,3i2.2)')idatec(4),idatec(3),idatec(2),idatec(1)

    EXTS    ='S.unf'
    DO i=1,4
       idate(i)=0
       idatec(i)=0
    END DO

    IF(trunc < 100)WRITE(TRC,'(a1,i3.3)')'T',trunc
    IF(trunc >= 100 .AND. trunc < 1000)WRITE(TRC,'(a1,i3)')'T',trunc
    IF(trunc >= 1000)WRITE(TRC,'(a1,i4.4)')'T',trunc   
    IF(vert < 100)WRITE(LV,'(a1,i2.2)')'L',vert
    IF(vert >= 100 .AND. vert < 1000)WRITE(LV,'(a1,i3.3)')'L',vert

    IF ( TRIM(START) == 'cold' ) THEN
       first = .TRUE.  
       EXTW='F.unf'
       EXDW='F.dir'
       EXTH='F.unf'
       EXDH='F.dir'
       LABELC=LABELF
       fNameInput0=TRIM(path_in)//'GANL'//TRIM(PREFY)//LABELI//EXTS//'.'//TRIM(TRC)//TRIM(LV)
       fNameInput1=TRIM(path_in)//'GANL'//TRIM(PREFY)//LABELI//EXTS//'.'//TRIM(TRC)//TRIM(LV)
    ELSEIF ( TRIM(START) == 'warm' ) THEN
       EXTW='F.unf'
       EXDW='F.dir'
       EXTH='F.unf'
       EXDH='F.dir'
       fNameInput0=TRIM(dirfNameOutput)//'/'//'GFCT'//TRIM(PREFX)//LABELI//LABELC//EXTW//'.'//TRIM(TRC)//TRIM(LV)//'.outmdt'
       fNameInput1=TRIM(dirfNameOutput)//'/'//'GFCT'//TRIM(PREFX)//LABELI//LABELC//EXTW//'.'//TRIM(TRC)//TRIM(LV)//'.outatt'
       nlnminit = .FALSE.
       diabatic = .FALSE.
       eigeninit= .FALSE.
       rsettov  = .FALSE.
       first    = .FALSE.  
       nfin1 =19
       nfcnv0=31
       initlz=0  
       ifsnw=0
       ifalb=0
       ifslm=0
    END IF
    fNameNmi    = TRIM(path_in)//'NMI'//'.'//TRIM(TRC)//TRIM(LV)
    fNameSnow   = TRIM(path_in)//'snowfd'//LABELI//EXTS//'.'//TRIM(TRC)
    fNameSoilms = TRIM(path_in)//'soilms'//'.'//TRIM(TRC)
    fNameOrgvar = TRIM(path_in)//'orgvar'//'.'//TRIM(TRC)
    fNameSibmsk = TRIM(path_in)//'sibmsk'//'.'//TRIM(TRC)
    fNameTg3zrl = TRIM(path_in)//'tg3zrl'//'.'//TRIM(TRC)
    fNameAlbedo = TRIM(path_in)//'sibalb'
    fNameCnftBl = TRIM(path_in)//'cnftbl'
    fNameCnf2Tb = TRIM(path_in)//'cnf2tb'
    fNameLookTb = TRIM(path_in)//'looktb'
    fNameSibVeg = TRIM(path_in)//'sibveg'
    fNameSibAlb = TRIM(path_in)//'sibalb'  
    fNameGHLoc  = TRIM(path_in)//'ghlocal'//'.'//TRIM(TRC)
    fNameGHTable= TRIM(path_in)//'ghdstable'

    IF(TRIM(TABLE) == 'p') THEN
      fNameDTable=TRIM(path_in)//'desirtable.pnt'
    ELSE IF(TRIM(TABLE) == 'c') THEN
      fNameDTable=TRIM(path_in)//'desirtable.clm'
    ELSE IF(TRIM(TABLE) == 'n') THEN
      fNameDTable=TRIM(path_in)//'desirtable'
    ELSE
      fNameDTable=TRIM(path_in)//'desirtable'
    END IF

    INQUIRE (FILE=TRIM(fNameSibAlb),exist=lexist)
    IF(lexist) THEN
       PRINT*,'* The  ', TRIM(fNameSibAlb) ,' file exist *'
    ELSE
       PRINT*,'* The  ', TRIM(fNameSibAlb), ' file not exist*'
       STOP
    END IF
    INQUIRE (FILE=TRIM(fNameSibVeg),exist=lexist)
    IF(lexist) THEN
       PRINT*,'* The  ', TRIM(fNameSibVeg) ,' file exist *'
    ELSE
       PRINT*,'* The  ', TRIM(fNameSibVeg), ' file not exist*'
       STOP
    END IF
    INQUIRE (FILE=TRIM(fNameLookTb),exist=lexist)
    IF(lexist) THEN
       PRINT*,'* The  ', TRIM(fNameLookTb) ,' file exist *'
    ELSE
       PRINT*,'* The  ', TRIM(fNameLookTb), ' file not exist*'
       STOP
    END IF
    INQUIRE (FILE=TRIM(fNameCnf2Tb),exist=lexist)
    IF(lexist) THEN
       PRINT*,'* The  ', TRIM(fNameCnf2Tb) ,' file exist *'
    ELSE
       PRINT*,'* The  ', TRIM(fNameCnf2Tb), ' file not exist*'
       STOP
    END IF
    INQUIRE (FILE=TRIM(fNameCnftBl),exist=lexist)
    IF(lexist) THEN
       PRINT*,'* The  ', TRIM(fNameCnftBl) ,' file exist *'
    ELSE
       PRINT*,'* The  ', TRIM(fNameCnftBl), ' file not exist*'
       STOP
    END IF
    INQUIRE (FILE=TRIM(fNameAlbedo),exist=lexist)
    IF(lexist) THEN
       PRINT*,'* The  ', TRIM(fNameAlbedo) ,' file exist *'
    ELSE
       PRINT*,'* The  ', TRIM(fNameAlbedo), ' file not exist*'
       STOP
    END IF
    INQUIRE (FILE=TRIM(fNameTg3zrl),exist=lexist)
    IF(lexist) THEN
       PRINT*,'* The  ', TRIM(fNameTg3zrl) ,' file exist *'
    ELSE
       PRINT*,'* The  ', TRIM(fNameTg3zrl), ' file not exist*'
       STOP
    END IF
    INQUIRE (FILE=TRIM(fNameSibmsk),exist=lexist)
    IF(lexist) THEN
       PRINT*,'* The  ', TRIM(fNameSibmsk) ,' file exist *'
    ELSE
       PRINT*,'* The  ', TRIM(fNameSibmsk), ' file not exist*'
       STOP
    END IF
    INQUIRE (FILE=TRIM(fNameOrgvar),exist=lexist)
    IF(lexist) THEN
       PRINT*,'* The  ', TRIM(fNameOrgvar) ,' file exist *'
    ELSE
       PRINT*,'* The  ', TRIM(fNameOrgvar), ' file not exist*'
       STOP
    END IF
    INQUIRE (FILE=TRIM(fNameSoilms),exist=lexist)
    IF(lexist) THEN
       PRINT*,'* The  ', TRIM(fNameSoilms) ,' file exist *'
    ELSE
       PRINT*,'* The  ', TRIM(fNameSoilms), ' file not exist*'
       STOP
    END IF
    INQUIRE (FILE=TRIM(fNameSnow),exist=lexist)
    IF(lexist) THEN
       PRINT*,'* The  ', TRIM(fNameSnow) ,' file exist *'
    ELSE
       PRINT*,'* The  ', TRIM(fNameSnow), ' file not exist*'
       STOP
    END IF
    INQUIRE (FILE=TRIM(fNameNmi),exist=lexist)
    IF(lexist) THEN
       PRINT*,'* The  ', TRIM(fNameNmi) ,' file exist *'
    ELSE
       PRINT*,'* The  ', TRIM(fNameNmi), ' file not exist*'
       !STOP
    END IF
  END SUBROUTINE coldwarm
END MODULE Options
