!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:37 $
!  $Revision: 1.1.1.1 $
!
MODULE Init   ! Version 0 of Nov 25th, 2001



  !  INITIALIZATION PROCEDURE

  !  Initializes all modules required by the computation, at
  !  a precise order. Exports a single procedure, InitAll.
  !  That procedure ought to be invoked before any other
  !  procedure in the computation, since it initializes all
  !  other modules.
  !  See procedure header for arguments description.



  USE Sizes, ONLY : ibMax, jbMax,kMax, &
       jMax, jMaxHalf, jMinPerM, iMaxPerJ, &
       mMax, nExtMax, mnMax, mnExtMax, mnExtMap, &
       RegisterBasicSizes, RegisterOtherSizes, DumpSizes, InitVerSizes
  USE Utils, ONLY: CreateGaussQuad, DumpGaussQuad, &
       GaussPoints, GaussWeights, CreateAssocLegFunc, DumpAssocLegFunc, &
       LegFuncF2STransp, InitTransDiagCol, CreateGridValues
  USE FieldsDynamics, ONLY: InitFields
  USE Constants, ONLY: InitConstants
  USE SpecDynamics, ONLY: InitDZtoUV, InitGozrim, InitImplDifu, &
       InitSemiImpl, InitUvtodz, InitFiltDiss
  USE Transform, ONLY: InitTransform, NextSizeFFT
  IMPLICIT NONE
  PRIVATE
  INTEGER           , PUBLIC           :: nls
  INTEGER           , PUBLIC           :: nlcs
  PUBLIC :: InitAll
  LOGICAL, PARAMETER :: dumpLocal=.FALSE.
  REAL, PARAMETER :: Sigma_Estrat=0.099999
  INTEGER :: k

CONTAINS



  !InitAll: Initializes all modules used by the computation.
  !         There are four mandatory arguments and four
  !         optional arguments. The four mandatory arguments are:
  !
  !         trunc       model truncation
  !         vert        number of model vertical layers
  !         reducedGrid TRUE iff model uses reduced Grid
  !         linearGrid  TRUE iff model uses linear Grid
  !
  !         If no other argument is given, InitAll computes the
  !         maximum number of longitudes and the number of
  !         latitudes required by the
  !         combination of (reduced or gaussian) with
  !         (linear or quadratic) grids. It proceeds by 
  !         computing Gaussian Points and Weights, placing
  !         latitudes as Gaussian Points and computing Associated
  !         Legendre Functions.
  !         In the case of reduced grid, it proceeds by computing 
  !         the number of longitudes per latitude according to
  !         Courtier-Naughton criteria.
  !         In any case, it follows by accomodating the number of
  !         longitudes per latitude to the acceptable FFT sizes and
  !         ends by initializing MultiFFT and  
  !         MultiLegTrans modules.
  !
  !         Parts of the computation can be bypassed if any of
  !         the optional arguments were given. The optional arguments
  !         and their effect are:
  !
  !Obs. do Jairo: os opcionais serao revistos e alterados, pois
  !permitem inconsistencias e contem informacoes duplicadas.
  !
  !         lonPerLat: an integer vector, indexed by latitudes, 
  !         containing the number of longitudes at each latitude.
  !         Its presence bypass the Courtier-Naughton criteria
  !         computation, but not the accomodation to FFT sizes.
  !          
  !         wavesPerLat: an integer vector, indexed by latitudes,
  !         containing the number of Fourier waves at each latitude.
  !         Its presence bypass the accomodation of Courtier-Naughton
  !         results to the possible sizes of FFTs.
  !
  !         gausPt: a real vector containing all Gaussian Points. Its
  !         size is the number of model latitudes. The presence of this
  !         argument bypasses the computation of Gaussian Points.
  !
  !         gausWg: a real vector containing all Gaussian Weights. Its
  !         size is the number of model latitudes. The presence of this
  !         argument bypasses the computation of Gaussian Weights.

  !Obs do Jairo: esta rotina sera aumentada para conter todas as 
  !inicializacoes do modelo. Esta longe de sua forma final.


  SUBROUTINE InitAll (trunc, vert, reducedGrid, linearGrid, &
        ct_in, cq_in, si_in, sl_in, del_in, dk_in, tk_in, &
       lonPerLat, wavesPerLat, gausPt, gausWg)
    INTEGER, INTENT(IN)           :: trunc
    INTEGER, INTENT(IN)           :: vert
    LOGICAL, INTENT(IN)           :: reducedGrid 
    LOGICAL, INTENT(IN)           :: linearGrid 
    REAL,    INTENT(IN)           :: si_in(:)
    REAL,    INTENT(IN)           :: sl_in(:)
    REAL,    INTENT(IN)           :: dk_in
    REAL,    INTENT(IN)           :: tk_in
    INTEGER, INTENT(IN), OPTIONAL :: lonPerLat(:)
    INTEGER, INTENT(IN), OPTIONAL :: wavesPerLat(:)
    REAL,    INTENT(IN), OPTIONAL :: gausPt(:)
    REAL,    INTENT(IN), OPTIONAL :: gausWg(:)
    REAL,    INTENT(OUT)          :: del_in(:)
    REAL,    INTENT(OUT)          :: ct_in(:) 
    REAL,    INTENT(OUT)          :: cq_in(:)

    CHARACTER(LEN=*), PARAMETER :: h="**(InitAll)**"
    CHARACTER(LEN=10) :: c1, c2
    INTEGER :: wavesFactor
    INTEGER :: nLon
    INTEGER :: nLat, left
    INTEGER :: j
    INTEGER, ALLOCATABLE :: mPerLat(:)
    INTEGER, ALLOCATABLE :: iMaxPerLat(:)
    
    IF (dumpLocal) THEN
       WRITE(c1,"(i10)") trunc
       WRITE(c2,"(i10)") vert
       WRITE(*,"(a,' trunc=',a,'; vert=',a)",ADVANCE='NO') &
            h, TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2))
       IF (reducedGrid) THEN
          WRITE(*,"('; Reduced and')",ADVANCE='NO')
       ELSE
          WRITE(*,"('; Gaussian and')",ADVANCE='NO')
       END IF
       IF (linearGrid) THEN
          WRITE(*,"(' Linear Grid')")
       ELSE
          WRITE(*,"(' Quadratic Grid')")
       END IF
    END IF

    ! linear or quadratic grid

    IF (linearGrid) THEN
       wavesFactor = 2
    ELSE
       wavesFactor = 3
    END IF

    ! maximum number of longitudes
    ! for nLat=nLon/2 with even nLat, it suffices nLon multiple of 4

    IF (PRESENT(lonPerLat)) THEN
       nLon = NextSizeFFT(MAXVAL(lonPerLat))
    ELSE 
       nLon = NextSizeFFT(wavesFactor*trunc + 1)
       DO
          left = MOD(nLon,4)
          IF (left == 0) EXIT
          nLon = NextSizeFFT(nLon + 4 - left)
       END DO
    END IF

    ! number of latitudes

    IF (PRESENT(lonPerLat)) THEN
       nLat = SIZE(lonPerLat)
    ELSE
       nLat = nLon/2
    END IF

    ! verify consistency and register basic sizes

    CALL RegisterBasicSizes(trunc, nLat, nLon, vert)
    IF (dumpLocal) THEN
       WRITE(*,"(a,' will DumpSizes just after RegisterBasicSizes')") h
       CALL DumpSizes()
    END IF



    CALL InitVerSizes (si_in, sl_in, del_in)

    nls=0
    DO k=1,kMax
       IF (sl_in(k) <= Sigma_Estrat) nls=nls+1
    END DO
    IF (nls == 0) nls=1
    nlcs=kMax+2

    CALL Init_ct_cq (kMax, si_in, sl_in, ct_in, cq_in)

    ! for now on, jMax, jMaxHalf, spectral sizes and maps 
    ! can be used

    ! Initialize GaussQuad (Gaussian points and weights)

    IF (PRESENT(gausPt) .AND. PRESENT(gausWg)) THEN
       CALL CreateGaussQuad(jMax, gausPt, gausWg)
    ELSE IF (PRESENT(gausPt)) THEN
       CALL CreateGaussQuad(jMax, gausPt        )
    ELSE IF (PRESENT(gausWg)) THEN
       CALL CreateGaussQuad(jMax, weightsGiven=gausWg)
    ELSE
       CALL CreateGaussQuad(jMax)
    END IF
    IF (dumpLocal) THEN
       WRITE(*,"(a,' will DumpGaussQuad just after creation')") h
       CALL DumpGaussQuad()
    END IF
    
    ! Initialize AssocLegFunc (computes associated legendre functions)
    
    CALL CreateAssocLegFunc()

    ! computes mPerLat

    ALLOCATE (mPerLat(jMax))
    IF (PRESENT(wavesPerLat)) THEN
       mPerLat = wavesPerLat
    ELSE IF (reducedGrid) THEN
       CALL VaryMWithLat(LegFuncF2STransp, GaussWeights, mPerLat)
    ELSE
       mPerLat = mMax
    END IF

    ! compute iMaxPerLat

    ALLOCATE(iMaxPerLat(jMax))
    IF (PRESENT(lonPerLat)) THEN
       iMaxPerLat = lonPerLat
    ELSE 
       DO j = 1, jMaxHalf
          iMaxPerLat(j) = NextSizeFFT(wavesFactor*(mPerLat(j)-1) + 1)
       END DO
       iMaxPerLat(jMaxHalf+1:jMax)=iMaxPerLat(jMaxHalf:1:-1)
    END IF
    
    ! finish building sizes
    
    CALL RegisterOtherSizes(iMaxPerLat, mPerLat)
    IF (dumpLocal) THEN
       WRITE(*,"(a,' will DumpSizes just after RegisterOtherSizes')") h
       CALL DumpSizes()
    END IF

    ! for now on, all global constants defined at sizes can be used
    
    ! initialize Fields

    CALL InitFields(ibMax, kMax, jbMax, mnMax, mnExtMax)

    ! initialize Constants
    
    CALL InitConstants(kMax)

    CALL CreateGridValues

    ! initialize SemiImpl

    CALL InitSemiImpl()
    CALL InitImplDifu( ct_in, cq_in, dk_in, tk_in)

    ! initialize dztouv and uvtodz

    CALL InitDZtoUV()
    CALL InitUvtodz()
    CALL InitFiltDiss()

    ! initialize Gozrim

    CALL InitGozrim()

    CALL InitTransDiagCol()


    CALL InitTransform()

  END SUBROUTINE InitAll



  !VaryMWithLat: Implements Courtier-Naughton criteria for computing
  !              wave number per latitude in the reduced grid.
  !              Internal module procedure



  SUBROUTINE VaryMWithLat(Pmn, W, mPerLat)
    REAL,    INTENT(IN ) :: W(jMax)                 ! Gaussian Weights
    REAL,    INTENT(IN ) :: Pmn(jMaxHalf, mnExtMax) ! AssocLegFunc
    INTEGER, INTENT(OUT) :: mPerLat(jMax)           ! Waves per latitude
    REAL    :: Partial(nExtMax, mnExtMax)
    INTEGER :: j, m, n, nPrime, mn, mnPrime
    REAL    :: erroMax
    REAL, PARAMETER :: acceptable=1.0e-12  ! orthogonality acceptable error


    ! At a given latitude j, Partial contains the partial summations from
    ! North Pole up to j that are the contribution of this
    ! set of latitudes to the orthogonality of Associated Legendre Functions
    ! of Legendre Degree nPrime and Legendre Order m with respect to all
    ! Associated Legendre Functions with same Order. While this contribution
    ! is less than a fixed acceptable error, that particular Order is not
    ! required for that latitude. (Courier-Naughton criteria)


    Partial = 0.0
    DO j = 1, jMaxHalf
       DO m = mMax, 1, -1
          DO n = m, nExtMax
             mn = mnExtMap(m,n)
             DO nPrime = n, nExtMax
                mnPrime = mnExtMap(m,nPrime)
                Partial(nPrime,mn) = Partial(nPrime,mn) + &
                     Pmn(j,mn)*Pmn(j,mnPrime)*W(j)
             END DO
          END DO
          erroMax = 0.0
          DO n = m, nExtMax
             mn = mnExtMap(m,n)
             erroMax = Max(erroMax, MAXVAL(ABS(Partial(n:nExtMax,mn))))
          END DO
          IF (erroMax > acceptable) THEN
             mPerLat(j) = MIN(mMax, m+1)
             EXIT
          END IF
       END DO
    END DO
    mPerLat(jMaxHalf+1:jMax)=mPerLat(jMaxHalf:1:-1)
  END SUBROUTINE VaryMWithLat



SUBROUTINE Init_ct_cq (kMax, si_in, sl_in, ct_in, cq_in)

   ! Uses MRF86 18-Level ct and cq (Specified Derivatives of
   ! Global Average T and q WRT LOG(Sigma)) to get Departures
   ! of these Quantities from Values at Lowest Layer. 
   ! These Departures are Linearly Interpolated to kMax-Levels, 
   ! and then Used to Compute the Corresponding Derivatives 
   ! ct and cq for the kMax-Level Model

   IMPLICIT NONE

   INTEGER, INTENT(IN) :: kMax

   REAL, DIMENSION (kMax), INTENT(IN) :: sl_in

   REAL, DIMENSION (kMax+1), INTENT(IN) :: si_in

   REAL, DIMENSION (kMax), INTENT(OUT) :: ct_in, cq_in

   INTEGER, PARAMETER :: kMRF=18, kMRFp=kMRF+1, kMRFm=kMRF-1

   INTEGER :: l, k, kk

   REAL :: at, bt, aq, bq

   LOGICAL :: Interp

   REAL, DIMENSION (kMax) :: algsi

   REAL, DIMENSION (kMax+1) :: glt, glq

   REAL, DIMENSION (kMRF) :: algsiMRF

   REAL, DIMENSION (kMRFp) :: gltMRF, glqMRF

   REAL, PARAMETER :: Eps=0.0001

   ! These Values Are For 18-Layer MRF86 NCEP Vertical Spacing

   REAL, PARAMETER, DIMENSION (kMRF) :: slMRF = (/ &
         0.995004, 0.981488, 0.960480, 0.920400, 0.856317, 0.777224, &
         0.688125, 0.593540, 0.497450, 0.424818, 0.374806, 0.324767, &
         0.274735, 0.224668, 0.174576, 0.124402, 0.073986, 0.020747 /)

   REAL, PARAMETER, DIMENSION (kMRFp) :: siMRF = (/ &
         1.000000, 0.990000, 0.973000, 0.948000, 0.893000, 0.820000, &
         0.735000, 0.642000, 0.546000, 0.450000, 0.400000, 0.350000, &
         0.300000, 0.250000, 0.200000, 0.150000, 0.100000, 0.050000, 0.0 /)

   REAL, PARAMETER, DIMENSION (kMRF) :: ctMRF = (/ &
         38.1, 38.2, 38.7, 39.3, 40.5, 42.5, 44.8, 46.8, 47.1, &
         47.9, 47.6, 43.2, 37.2, 29.9, 23.3,  9.7,  0.0,  0.0 /)

   REAL, PARAMETER, DIMENSION (kMRF) :: cqMRF = (/ &
         0.0270, 0.0290, 0.0290, 0.0270, 0.0190, 0.0110, &
         0.0079, 0.0057, 0.0038, 0.0024, 0.0014, 0.0010, &
         0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000  /)

   IF (kMRF /= kMax) THEN
      Interp=.TRUE.
   ELSE
      Interp=ANY((slMRF-sl_in) > Eps)
   END IF

   IF (Interp) THEN
  
      ! Integrate To Get Global Average T and q Differences From Lowest Layer

      gltMRF(1)=0.0
      DO l=1,kMRFm
         gltMRF(l+1)=gltMRF(l)+ctMRF(l)*(ALOG(siMRF(l+1))-ALOG(siMRF(l)))
      END DO
      gltMRF(kMRFp)=gltMRF(kMRF)
      glqMRF(1)=0.0
      DO l=1,kMRFm
         glqMRF(l+1)=glqMRF(l)+cqMRF(l)*(ALOG(siMRF(l+1))-ALOG(siMRF(l)))
      END DO
      glqMRF(kMRFp)=glqMRF(kMRF)
      DO l=1,kMRF
         algsiMRF(l)=ALOG(siMRF(l))
      END DO
      DO k=1,kMax
         algsi(k)=ALOG(si_in(k))
      END DO
  
    ! Interpolate To Get New Global Average Changes
  
      DO l=1,kMax
         kk=2
         IF (algsi(l) <= algsiMRF(kMRF)) THEN
            kk=kMRF
         ELSE
            DO  k=2,kMRF
              IF (algsi(l) >= algsiMRF(k) .AND. algsi(l) < algsiMRF(k-1)) THEN
                 kk=k
              END IF
            END DO
         END IF
         at=(gltMRF(kk)-gltMRF(kk-1))/(algsiMRF(kk)-algsiMRF(kk-1))
         bt=gltMRF(kk-1)-at*algsiMRF(kk-1)
         glt(l)=bt+at*algsi(l)
         aq=(glqMRF(kk)-glqMRF(kk-1))/(algsiMRF(kk)-algsiMRF(kk-1))
         bq=glqMRF(kk-1)-aq*algsiMRF(kk-1)
         glq(l)=bq+aq*algsi(l)
      END DO
  
      ! Differentiate To Get New Derivatives WRT LOG(Sigma)
      DO k=1,kMax-1
         ct_in(k)=(glt(k+1)-glt(k))/(algsi(k+1)-algsi(k))
         IF (ABS(ct_in(k)) <= Eps) ct_in(k)=0.0
         cq_in(k)=(glq(k+1)-glq(k))/(algsi(k+1)-algsi(k))
         IF (ABS(cq_in(k)) <= Eps) cq_in(k)=0.0
      END DO
      ct_in(kMax)=0.0
      cq_in(kMax)=0.0
  
   ELSE
  
      DO l=1,kMRF
         ct_in(l)=ctMRF(l)
         cq_in(l)=cqMRF(l)
      END DO
  
   END IF

END SUBROUTINE Init_ct_cq

END MODULE Init
