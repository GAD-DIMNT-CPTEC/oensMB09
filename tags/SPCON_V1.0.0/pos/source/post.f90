!
!  $Author: pkubota $
!  $Date: 2007/10/10 20:28:03 $
!  $Revision: 1.3 $
!
PROGRAM GLobalModelPostProcessing

  ! Reads Spectral Forecast Coefficients of Topography,
  ! Log of Sfc Pressure, Temperature, Divergence, Vorticity
  ! and Humidity in Sigma Layers. Converts These Values
  ! to Selected Mandatory Pressure Levels.

  USE Constants, ONLY : InitParameters, nfprt, nFFrs, nFBeg, nFEnd,datalib
  USE Conversion, ONLY : CreateConversion
  USE PrblSize, ONLY : CreatePrblSize, Mend1, Kmax
  USE FFT, ONLY : CreateFFT
  USE GaussRep, ONLY : CreateGaussRep
  USE GaussSigma, ONLY : CreateGaussSigma
  USE RegInterp, ONLY : InitAreaInterpolation
  USE SpectralRep, ONLY : CreateSpectralRep
  USE FileAccess, ONLY : InitFiles
  USE LegTrans, ONLY : CreateLegTrans
  USE PostLoop, ONLY : postgl,InitPostLoop
  USE tables, ONLY: tables_readed,Init_tables
  USE Watches

  IMPLICIT NONE

  INTEGER :: nFile

!!MARCELO 1
  LOGICAL, PARAMETER :: instrument=.TRUE.
  INTEGER :: nThreads=0
  INTEGER :: iThread
  INTEGER :: unitDump=100
  TYPE(Watch), ALLOCATABLE :: wt(:)  
!!MARCELO 1

!!MARCELO 2
  IF (instrument) THEN
     ALLOCATE(wt(0:nThreads))
     iThread = 0
     wt(iThread) = CreateWatch(1, 1)
     CALL NameWatch(wt(iThread), 1,"Time     ")
  END IF
!!MARCELO 2

  CALL InitParameters ()
  CALL InitFiles ()
  CALL InitPostLoop()
  CALL CreatePrblSize ()
  CALL CreateSpectralRep ()
  CALL CreateGaussRep ()
  CALL CreateFFT ()
  CALL CreateLegTrans ()
  CALL CreateConversion ()
  CALL CreateGaussSigma ()
  CALL InitAreaInterpolation ()
  !If grib tables not read proceed
  
  IF(.not. tables_readed) CALL Init_tables(datalib)

  WRITE (UNIT=nfprt, FMT='(/,A,I3.3,A,I2.2,/)') &
        ' Post-Processing Resolution: T', Mend1-1, 'L',Kmax

  ! Do Post-Processing for Files nFFrs to nFEnd
  DO nFile=nFFrs,nFEnd
     WRITE (UNIT=nfprt, FMT='(A,I5)') ' nFile = ',nFile
     CALL postgl (nFFrs, nFBeg, nFEnd, nFile)
     WRITE (UNIT=nfprt, FMT='(/,A,/)') ' Advanced Time Loop'
  END DO

!!MARCELO 3
  IF (instrument) THEN
     iThread = 0
     CALL DumpWatch(wt(iThread), unitDump,'TempoPosProcessamento')
     CALL DestroyWatch(wt(iThread))
  END IF
!!MARCELO 3

END PROGRAM GLobalModelPostProcessing
