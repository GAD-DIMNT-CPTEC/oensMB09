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

   INTEGER (KIND=i4), PARAMETER, PUBLIC :: nFmxI=10_i4, nFmxO=12_i4

   INTEGER (KIND=i4), PUBLIC :: nPTx, nFmx, MxGHsl, Imax, Jmax

   CHARACTER (LEN=4_i4), PUBLIC :: Lev

   CHARACTER (LEN=6_i4), PUBLIC :: Trunc

   CHARACTER (LEN=4_i4), PUBLIC :: GrdBox

   CHARACTER (LEN=10_i4), PUBLIC :: Resol

   CHARACTER (LEN=20_i4), PUBLIC :: Title

   CHARACTER (LEN=20_i4), PUBLIC :: DateICn

   CHARACTER (LEN=20_i4), PUBLIC :: DateFct

   CHARACTER (LEN=255_i4), PUBLIC :: TopogInp

   CHARACTER (LEN=255_i4), PUBLIC :: GrHDatIn

   CHARACTER (LEN=255_i4), PUBLIC :: GrHDatOut

   CHARACTER (LEN=255_i4), PUBLIC :: GrHPrfOut

   CHARACTER (LEN=255_i4), PUBLIC :: GrHLocOut

   CHARACTER (LEN=255_i4), PUBLIC :: GrHIdfOut

   CHARACTER (LEN=255_i4), PUBLIC :: GrHCtlOut

   CHARACTER (LEN=255_i4), PUBLIC :: DirTopog

   CHARACTER (LEN=96_i4), PUBLIC :: DirModel

   CHARACTER (LEN=255_i4), PUBLIC :: DirOut

   INTEGER (KIND=i4), PUBLIC :: LMonth(12_i4)

   INTEGER (KIND=i4), PUBLIC :: uVarc(nFmxO), lVarc(nFmxO)

   CHARACTER (LEN=3_i4), PUBLIC :: AMonth(12_i4)

   CHARACTER (LEN=4_i4), PUBLIC :: bVar(nFmxO)

   CHARACTER (LEN=40_i4), PUBLIC :: iVar(nFmxI), dVar(nFmxO)

   CHARACTER (LEN=4_i4), DIMENSION (:), ALLOCATABLE, PUBLIC :: aVar

   CHARACTER (LEN=11_i4), DIMENSION (:), ALLOCATABLE, PUBLIC :: Prx

   CHARACTER (LEN=40_i4), DIMENSION (:), ALLOCATABLE, PUBLIC :: cVar, cLoc

   INTEGER (KIND=i4), DIMENSION (:), ALLOCATABLE, PUBLIC :: lVar, uVar, iLoc, jLoc

   LOGICAL (KIND=i4), DIMENSION (:), ALLOCATABLE, PUBLIC :: DoPt

   INTEGER (KIND=i4), PUBLIC :: Mend, Kmax, mUTC

   REAL (KIND=r4), PUBLIC :: DelT, TMean

   CHARACTER (LEN=3_i4), PUBLIC :: Preffix

   CHARACTER (LEN=10_i4), PUBLIC :: LabelI, LabelF
   
   LOGICAL     , PUBLIC :: Linear

   CHARACTER (LEN=64_i4), PUBLIC :: DirMain

   CHARACTER (LEN=255_i4), PUBLIC :: DirInPut
   
   CHARACTER (LEN=255_i4), PUBLIC :: DirOutPut

   PUBLIC :: InitParameters

   INTEGER (KIND=i4) :: Kdim, Kqdim, n

   INTEGER (KIND=i4) :: iDate(4_i4)

   REAL (KIND=r4), DIMENSION (:), ALLOCATABLE :: Del

   CHARACTER (LEN=4_i4) :: nExp, iacc, idev

   CHARACTER (LEN=40_i4) :: Exper

   CHARACTER (LEN=255_i4) :: GrHDirIn


CONTAINS


SUBROUTINE InitParameters ()

   NAMELIST /InputDim/ Mend, Kmax, mUTC, DelT, TMean, &
                       LabelI, LabelF, Preffix, GrdBox,Linear,DirInPut,DirOutPut, DirMain

   Mend=213_i4         ! Model Spectral Horizontal Resolution
   Kmax=42_i4          ! Number of Vertical Model Layers
   mUTC=0_i4           ! Diference in Hour to Greenwhich (if desired, if no set 0_i4)
   DelT=360.0_r4       ! Model Time Step in Seconds
   TMean=3600.0_r4     ! Time Interval in Seconds To Average Output (1_i4 Hour)
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
   WRITE (Trunc(3_i4:6_i4), FMT='(I4.4)') Mend
   Lev='L   '
   WRITE (Lev(2_i4:4_i4), FMT='(I3.3)') Kmax
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
   GrHPrfOut='Preffix'//Preffix//DateFct//'.'//TRIM(Resol)
   GrHLocOut='Localiz'//Preffix//DateFct//'.'//TRIM(Resol)
   GrHIdfOut='Identif'//Preffix//DateFct//'.'//TRIM(Resol)

   write (*,*)GrHPrfOut,GrHLocOut,GrHIdfOut,DirOut

   OPEN (UNIT=10, STATUS='UNKNOWN',FORM='FORMATTED', &
         FILE=TRIM(DirModel)//TRIM(GrhDirIn))
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

   DO n=1_i4,nFmx
     READ (UNIT=10, FMT='(A40,I5,2X,I5,1X,A4)') cVar(n), lVar(n), uVar(n), aVar(n)
     WRITE (UNIT=*, FMT='(1X,A40,I5,2X,I5,1X,A4)') cVar(n), lVar(n), uVar(n), aVar(n)
   END DO
   DO n=1_i4,nPTx
     !OpenMP READ (UNIT=10, FMT='(A40,2I5,1X,A11)') cLoc(n), iLoc(n), jLoc(n), Prx(n)
      
      READ (UNIT=10, FMT='(A40,1X,A11)') cLoc(n), Prx(n)

     WRITE (UNIT=*, FMT='(I6,2X,A40,1X,A11)') n, cLoc(n), Prx(n)
   END DO
   WRITE (UNIT=*, FMT='(/)')
   CLOSE (UNIT=10)

   ALLOCATE (DoPt(nPTx))
   DoPt=.FALSE.

   LMonth = (/ 31_i4, 28_i4, 31_i4, 30_i4, 31_i4, 30_i4, &
               31_i4, 31_i4, 30_i4, 31_i4, 30_i4, 31_i4 /)

   AMonth = (/ 'jan', 'feb', 'mar', 'apr', 'may', 'jun', &
               'jul', 'aug', 'sep', 'oct', 'nov', 'dec' /)

   uVarc = (/  10_i4, 131_i4, 131_i4, 121_i4,   1_i4,  60_i4, &
               60_i4,  41_i4,   1_i4, 110_i4, 120_i4,  41_i4/)

   lVarc = (/   0_i4,   0_i4,   0_i4,   0_i4,   0_i4,   1_i4, &
                1_i4,   0_i4,   0_i4,   0_i4,   0_i4,   0_i4/)

   bVar = (/ 'topo', 'pslc', 'psnm', 'prec', 'cbnv', 'uves', &
             'vves', 'tems', 'umrs', 'pnev', 'neve', 'tadl'/)
	     
   iVar = (/ 'SURFACE PRESSURE                        ', &
             'TOTAL PRECIPITATION                     ', &
             'CLOUD COVER                             ', &
             'SURFACE ZONAL WIND (U)                  ', &
             'SURFACE MERIDIONAL WIND (V)             ', &
             'SURFACE VIRTUAL TEMPERATURE             ', &
             'SURFACE SPECIFIC HUMIDITY               ', &
             'SNOW DEPTH                              ', &
             'SNOWFALL                                ', &
             'TEMPERATURE OF CANOPY AIR SPACE         '/)

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
             'TEMPERATURE OF CANOPY AIR SPACE         '/)

END SUBROUTINE InitParameters


END MODULE InputParameters
