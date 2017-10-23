!
!  $Author: bonatti $
!  $Date: 2005/05/18 10:00:00 $
!  $Revision: 1.0 $
!
MODULE InputParameters

   IMPLICIT NONE

   PRIVATE

                               ! Selecting Kinds:
                               ! r4 : Kind for 32-bits Real Numbers
                               ! i4 : Kind for 32-bits Integer Numbers
   INTEGER, PARAMETER, PUBLIC :: r4 = SELECTED_REAL_KIND(6)
   INTEGER, PARAMETER, PUBLIC :: i4 = SELECTED_INT_KIND(9)

   INTEGER (KIND=i4), PARAMETER, PUBLIC :: nFmxI=45   , nFmxO=47   

   INTEGER (KIND=i4), PUBLIC :: nPTx, nFmx, MxGHsl, Imax, Jmax

   CHARACTER (LEN=4   ), PUBLIC :: Lev

   CHARACTER (LEN=6   ), PUBLIC :: Trunc

   CHARACTER (LEN=4   ), PUBLIC :: GrdBox

   CHARACTER (LEN=10   ), PUBLIC :: Resol

   CHARACTER (LEN=20   ), PUBLIC :: Title

   CHARACTER (LEN=20   ), PUBLIC :: DateICn

   CHARACTER (LEN=20   ), PUBLIC :: DateFct

   CHARACTER (LEN=555   ), PUBLIC :: TopogInp

   CHARACTER (LEN=555   ), PUBLIC :: GrHDatIn

   CHARACTER (LEN=555   ), PUBLIC :: GrHDatOut

   CHARACTER (LEN=555   ), PUBLIC :: GrHPrfOut

   CHARACTER (LEN=555   ), PUBLIC :: GrHLocOut

   CHARACTER (LEN=555   ), PUBLIC :: GrHIdfOut

   CHARACTER (LEN=555   ), PUBLIC :: GrHCtlOut

   CHARACTER (LEN=555   ), PUBLIC :: DirTopog

   CHARACTER (LEN=555   ), PUBLIC :: DirModel

   CHARACTER (LEN=555   ), PUBLIC :: DirOut

   INTEGER (KIND=i4), PUBLIC :: LMonth(12   )

   INTEGER (KIND=i4), PUBLIC :: uVarc(nFmxO), lVarc(nFmxO)

   CHARACTER (LEN=3   ), PUBLIC :: AMonth(12   )

   CHARACTER (LEN=4   ), PUBLIC :: bVar(nFmxO)

   CHARACTER (LEN=40   ), PUBLIC :: iVar(nFmxI), dVar(nFmxO)

   CHARACTER (LEN=4   ), DIMENSION (:), ALLOCATABLE, PUBLIC :: aVar

   CHARACTER (LEN=11   ), DIMENSION (:), ALLOCATABLE, PUBLIC :: Prx

   CHARACTER (LEN=40   ), DIMENSION (:), ALLOCATABLE, PUBLIC :: cVar, cLoc

   INTEGER (KIND=i4), DIMENSION (:), ALLOCATABLE, PUBLIC :: lVar, uVar, iLoc, jLoc

   LOGICAL (KIND=i4), DIMENSION (:), ALLOCATABLE, PUBLIC :: DoPt

   INTEGER (KIND=i4), PUBLIC :: Mend, Kmax, mUTC

   REAL (KIND=r4), PUBLIC :: DelT, TMean

   CHARACTER (LEN=3   ), PUBLIC :: Preffix

   CHARACTER (LEN=10   ), PUBLIC :: LabelI, LabelF
   
   LOGICAL     , PUBLIC :: Linear

   CHARACTER (LEN=64   ), PUBLIC :: DirMain

   CHARACTER (LEN=555   ), PUBLIC :: DirInPut
   
   CHARACTER (LEN=555   ), PUBLIC :: DirOutPut

   PUBLIC :: InitParameters

   INTEGER (KIND=i4) :: Kdim, Kqdim, n

   INTEGER (KIND=i4) :: iDate(4   )

   REAL (KIND=r4), DIMENSION (:), ALLOCATABLE :: Del

   CHARACTER (LEN=4   ) :: nExp, iacc, idev

   CHARACTER (LEN=40   ) :: Exper

   CHARACTER (LEN=555   ) :: GrHDirIn
  
   INTEGER :: ierr

CONTAINS


SUBROUTINE InitParameters ()

   NAMELIST /InputDim/ Mend, Kmax, mUTC, DelT, TMean, &
                       LabelI, LabelF, Preffix, GrdBox,Linear,DirInPut,DirOutPut, DirMain

   Mend=213            ! Model Spectral Horizontal Resolution
   Kmax=42             ! Number of Vertical Model Layers
   mUTC=0              ! Diference in Hour to Greenwhich (if desired, if no set 0   )
   DelT=360.0_r4       ! Model Time Step in Seconds
   TMean=3600.0_r4     ! Time Interval in Seconds To Average Output (1    Hour)
   LabelI='yyyimidihi' ! Initial Condition Date
   LabelF='yyyfmfdfhf' ! Final Forecast Date
   Preffix='NMC'       ! Preffix of File Names
   GrdBox='   '        ! Preffix Name To Skip Points (Use = 'Prox' to skip "Proxes" Points)
   Linear=.FALSE.
   DirMain='./ '       ! Main Data Directory
   DirInPut='./ ' 
   DirOutPut='./ ' 
   READ  (UNIT=*, NML=InputDim)

   WRITE (UNIT=*, FMT='(/,A)') ' &InputDim'
   WRITE (UNIT=*, FMT='(A,I)') '     Mend = ', Mend
   WRITE (UNIT=*, FMT='(A,I)') '     Kmax = ', Kmax
   WRITE (UNIT=*, FMT='(A,I)') '     mUTC = ', mUTC
   WRITE (UNIT=*, FMT='(A,F)') '     DelT = ', DelT
   WRITE (UNIT=*, FMT='(A,F)') '    TMean = ', TMean
   WRITE (UNIT=*, FMT='(A)')   '   LabelI = '//LabelI
   WRITE (UNIT=*, FMT='(A)')   '   LabelF = '//LabelF
   WRITE (UNIT=*, FMT='(A)')   '  Preffix = '//Preffix
   WRITE (UNIT=*, FMT='(A)')   '   GrdBox = '//GrdBox   
   WRITE (UNIT=*, FMT='(A,L)')   '   Linear = ',Linear
   WRITE (UNIT=*, FMT='(A)')   '  DirInPut = '//TRIM(DirInPut)
   WRITE (UNIT=*, FMT='(A)')   '  DirInPut = '//TRIM(DirInPut)
   WRITE (UNIT=*, FMT='(A)')   '  DirMain = '//TRIM(DirMain)

   WRITE (UNIT=*, FMT='(A,/)') ' /'

   ! Resolution
   IF(Linear)THEN
      Trunc='TL    '
   ELSE
      Trunc='TQ    '   
   END IF   
   WRITE (Trunc(3   :6   ), FMT='(I4.4)') Mend
   Lev='L   '
   WRITE (Lev(2   :4   ), FMT='(I3.3)') Kmax
   Resol=Trunc//Lev

   ! Dates
   DateICn=LabelI//LabelF
   DateFct=LabelI//LabelF

   ! Directories
   DirTopog=TRIM(DirInPut)
   DirModel=TRIM(DirInPut)
   DirOut=TRIM(DirOutPut)

   ! Input Files
   TopogInp='GFGH'//Preffix//DateICn//'F.top'//'.'//TRIM(Resol)
   GrHDirIn='GFGH'//Preffix//DateICn//'F.dir'//'.'//TRIM(Resol)
   GrHDatIn='GFGH'//Preffix//DateICn//'F.unf'//'.'//TRIM(Resol)

   ! Output Files (DirOut)
   GrHDatOut='GFGN'//Preffix//DateFct//'M.grh'//'.'//TRIM(Resol)
   GrHCtlOut='GFGN'//Preffix//DateFct//'M.grh'//'.'//TRIM(Resol)//'.ctl'
   GrHPrfOut='Preffix'//DateFct//'.'//TRIM(Resol)
   GrHLocOut='Localiz'//DateFct//'.'//TRIM(Resol)
   GrHIdfOut='Identif'//DateFct//'.'//TRIM(Resol)

   OPEN (UNIT=10, STATUS='UNKNOWN',FORM='FORMATTED', &
         FILE=TRIM(DirModel)//TRIM(GrhDirIn), IOSTAT=ierr) 
       IF (ierr /= 0) THEN
          WRITE(UNIT=0,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
               TRIM(DirModel)//TRIM(GrhDirIn), ierr
          STOP "**(ERROR)**"
       END IF
WRITE(UNIT=0,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
               TRIM(DirModel)//TRIM(GrhDirIn), ierr
   READ (UNIT=10, FMT='(A20)') Title
   WRITE (UNIT=*, FMT='(1X,A20)') Title
   READ (UNIT=10, FMT='(A4,1X,A4,11I5,1X,A4)') &
                  nExp, iacc, Imax, Jmax, Kdim, Kqdim,  &
                  nPTx, nFmx, MxGHsl, iDate, idev
   WRITE (UNIT=*, FMT='(1X,A4,1X,A4,11I5,1X,A4)') &
                  nExp, iacc, Imax, Jmax, Kdim, Kqdim,  &
                  nPTx, nFmx, MxGHsl, iDate, idev

   ALLOCATE (Del(Kmax))
   ALLOCATE (cVar(nFmx), cLoc(nPTx), aVar(nFmx), Prx(nPTx))
   ALLOCATE (lVar(nFmx), uVar(nFmx), iLoc(nPTx), jLoc(nPTx))

   READ (UNIT=10, FMT='(A40)') Exper
   WRITE (UNIT=*, FMT='(1X,A40)') Exper
   READ (UNIT=10, FMT='(5E16.8)') Del
   WRITE (UNIT=*, FMT='(5E16.8)') Del

   DO n=1   ,nFmx
     READ (UNIT=10, FMT='(A40,I5,2X,I5,1X,A4)') cVar(n), lVar(n), uVar(n), aVar(n)
     WRITE (UNIT=*, FMT='(1X,A40,I5,2X,I5,1X,A4)') cVar(n), lVar(n), uVar(n), aVar(n)
   END DO
   DO n=1   ,nPTx
     !OpenMP READ (UNIT=10, FMT='(A40,2I5,1X,A11)') cLoc(n), iLoc(n), jLoc(n), Prx(n)
      
      READ (UNIT=10, FMT='(A40,1X,A11)') cLoc(n), Prx(n)

     WRITE (UNIT=*, FMT='(I6,2X,A40,1X,A11)') n, cLoc(n), Prx(n)
   END DO
   WRITE (UNIT=*, FMT='(/)')
   CLOSE (UNIT=10)

   ALLOCATE (DoPt(nPTx))
   DoPt=.FALSE.

   LMonth = (/ 31   , 28   , 31   , 30   , 31   , 30   , &
               31   , 31   , 30   , 31   , 30   , 31    /)

   AMonth = (/ 'jan', 'feb', 'mar', 'apr', 'may', 'jun', &
               'jul', 'aug', 'sep', 'oct', 'nov', 'dec' /)

   uVarc = (/  10   , 131   , 131   , 121   ,   1   ,  60   , &
               60   ,  41   ,   1   , 110   , 120   ,  41   , &
               40   ,   0   ,   0   ,   0   , 170   , 170   , &
              120   , 120   , 170   , 170   , 170   , 170   , &
              170   , 170   , 170   ,   0   ,   0   ,  40   , &
                0   ,  60   ,  60   , 170   , 170   ,  40   , &
              170   , 170   , 170   , 170   , 170   , 170   , &
              170   , 150   ,   0   ,   0   ,   0/)

   lVarc = (/   0   ,   0   ,   0   ,   0   ,   0   ,   1   , &
                1   ,   0   ,   0   ,   0   ,   0   ,   0   , &
                1   ,   1   ,   1   ,   1   ,   1   ,   1   , &
                1   ,   1   ,   1   ,   1   ,   1   ,   1   , &
                1   ,   1   ,   1   ,   1   ,   1   ,   1   , &
                1   ,   1   ,   1   ,   1   ,   1   ,   1   , &
                1   ,   1   ,   1   ,   1   ,   1   ,   1   , &
                1   ,   1   ,   1   ,   1   ,   1/)

   bVar = (/ 'topo', 'pslc', 'psnm', 'prec', 'cbnv', 'uves', &
             'vves', 'tems', 'umrs', 'pnev', 'neve', 'tadl', &
             'tgdp', 'ussl', 'uzrs', 'uzds', 'cssf', 'clsf', &
             'prcv', 'rnof', 'foci', 'olis', 'oles', 'civb', &
             'civd', 'cinb', 'cind', 'alvb', 'alvd', 'tp2m', &
             'qq2m', 'us2m', 'vs2m', 'ocis', 'oces', 'tgrd', &
             'trdl', 'pidl', 'trgc', 'pigc', 'evbs', 'csdl', &
             'csgr', 'tsps', 'cllw', 'clmd', 'clhi'/)
             
   iVar = (/ 'SURFACE PRESSURE                        ', &
             'TOTAL PRECIPITATION                     ', &
             'CLOUD COVER                             ', &
             'SURFACE ZONAL WIND (U)                  ', &
             'SURFACE MERIDIONAL WIND (V)             ', &
             'SURFACE VIRTUAL TEMPERATURE             ', &
             'SURFACE SPECIFIC HUMIDITY               ', &
             'SNOW DEPTH                              ', &
             'SNOWFALL                                ', &
             'TEMPERATURE OF CANOPY AIR SPACE         ', &
             'DEEP SOIL TEMPERATURE                   ', &
             'SOIL WETNESS OF SURFACE ZONE            ', &
             'SOIL WETNESS OF ROOT ZONE               ', &
             'SOIL WETNESS OF RECHARGE ZONE           ', &
             'SENSIBLE HEAT FLUX FROM SURFACE         ', &
             'LATENT HEAT FLUX FROM SURFACE           ', &
             'CONVECTIVE PRECIPITATION                ', &
             'RUNOFF                                  ', &
             'INCIDENT SHORT WAVE FLUX                ', &
             'DOWNWARD LONG WAVE AT GROUND            ', &
             'UPWARD LONG WAVE FLUX AT GROUND         ', &
             'DOWNWARD SHORT WAVE FLUX AT GROUND (VB) ', &
             'DOWNWARD SHORT WAVE FLUX AT GROUND (VD) ', &
             'DOWNWARD SHORT WAVE FLUX AT GROUND (NB) ', &
             'DOWNWARD SHORT WAVE FLUX AT GROUND (ND) ', &
             'VISIBLE BEAM ALBEDO                     ', &
             'VISIBLE DIFFUSE ALBEDO                  ', &
             'TEMP AT 2-M FROM SFC                    ', &
             'SPECIFIC HUMID AT 2-M FROM SFC          ', &
             'ZONAL WIND AT 10-M FROM SFC             ', &
             'MERIDIONAL WIND AT 10-M FROM SFC        ', &
             'SHORTWAVE DOWNWARD AT GROUND            ', &
             'SHORTWAVE UPWARD AT BOTTOM              ', &
             'TEMPERATURA DA SUPERFICIE DO SOLO       ', &
             'TRANSPIRATION FROM CANOPY               ', &
             'INTERCEPTION LOSS FROM CANOPY           ', &
             'TRANSPIRATION FROM GROUND COVER         ', &
             'INTERCEPTION LOSS FROM GROUND COVER     ', &
             'BARE SOIL EVAPORATION                   ', &
             'SENSIBLE HEAT FLUX FROM CANOPY          ', &
             'SENSIBLE HEAT FLUX FROM GROUND          ', &
             'TENDENCY SURFACE PRESSURE               ', &
             'FRACTION OF CLOUDS FOR LOW              ', &
             'FRACTION OF CLOUDS FOR MEDIUM           ', &
             'FRACTION OF CLOUDS FOR HIGH             '/)

   dVar = (/ 'TOPOGRAPHY                              ', &
             'SURFACE PRESSURE                        ', &
             'SEA LEVEL PRESSURE                      ', &
             'TOTAL PRECIPITATION                     ', &
             'CLOUD COVER                             ', &
             'SURFACE ZONAL WIND (U)                  ', &
             'SURFACE MERIDIONAL WIND (V)             ', &
             'SURFACE ABSOLUTE TEMPERATURE            ', &
             'SURFACE RELATIVE HUMIDITY               ', &
             'SNOW DEPTH                              ', &
             'SNOWFALL                                ', &
             'TEMPERATURE OF CANOPY AIR SPACE         ', &
             'DEEP SOIL TEMPERATURE                   ', &
             'SOIL WETNESS OF SURFACE ZONE            ', &
             'SOIL WETNESS OF ROOT ZONE               ', &
             'SOIL WETNESS OF RECHARGE ZONE           ', &
             'SENSIBLE HEAT FLUX FROM SURFACE         ', &
             'LATENT HEAT FLUX FROM SURFACE           ', &
             'CONVECTIVE PRECIPITATION                ', &
             'RUNOFF                                  ', &
             'INCIDENT SHORT WAVE FLUX                ', &
             'DOWNWARD LONG WAVE AT GROUND            ', &
             'UPWARD LONG WAVE FLUX AT GROUND         ', &
             'DOWNWARD SHORT WAVE FLUX AT GROUND (VB) ', &
             'DOWNWARD SHORT WAVE FLUX AT GROUND (VD) ', &
             'DOWNWARD SHORT WAVE FLUX AT GROUND (NB) ', &
             'DOWNWARD SHORT WAVE FLUX AT GROUND (ND) ', &
             'VISIBLE BEAM ALBEDO                     ', &
             'VISIBLE DIFFUSE ALBEDO                  ', &
             'TEMP AT 2-M FROM SFC                    ', &
             'SPECIFIC HUMID AT 2-M FROM SFC          ', &
             'ZONAL WIND AT 10-M FROM SFC             ', &
             'MERIDIONAL WIND AT 10-M FROM SFC        ', &
             'SHORTWAVE DOWNWARD AT GROUND            ', &
             'SHORTWAVE UPWARD AT BOTTOM              ', &
             'TEMPERATURA DA SUPERFICIE DO SOLO       ', &
             'TRANSPIRATION FROM CANOPY               ', &
             'INTERCEPTION LOSS FROM CANOPY           ', &
             'TRANSPIRATION FROM GROUND COVER         ', &
             'INTERCEPTION LOSS FROM GROUND COVER     ', &
             'BARE SOIL EVAPORATION                   ', &
             'SENSIBLE HEAT FLUX FROM CANOPY          ', &
             'SENSIBLE HEAT FLUX FROM GROUND          ', &
             'TENDENCY SURFACE PRESSURE               ', &
             'FRACTION OF CLOUDS FOR LOW              ', &
             'FRACTION OF CLOUDS FOR MEDIUM           ', &
             'FRACTION OF CLOUDS FOR HIGH             '/)


END SUBROUTINE InitParameters


END MODULE InputParameters
