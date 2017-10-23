!
!  $Author: pkubota $
!  $Date: 2011/04/07 17:38:06 $
!  $Revision: 1.10 $
!
MODULE Init   ! Version 0 of Nov 25th, 2001



  !  INITIALIZATION PROCEDURE

  !  Initializes all modules required by the computation, at
  !  a precise order. Exports a single procedure, InitAll.
  !  That procedure ought to be invoked before any other
  !  procedure in the computation, since it initializes all
  !  other modules.
  !  See procedure header for arguments description.

  USE Parallelism, ONLY:   &
       myId,               &
       myId_four,          &
       mygroup_four,       &
       CreateFourierGroup, &
       maxnodes_four,      &
       maxNodes

  USE Sizes, ONLY : ibMax, jbMax,kMax, &
       jMax, jMaxHalf, jMinPerM, iMaxPerJ, &
       mMax, nExtMax, mnMax, mnExtMax, mnExtMap, &
       mymnMax, mymnExtMax, kmaxloc, MNMax_si, jbMax_ext, &
       RegisterBasicSizes, RegisterOtherSizes, DumpSizes, InitVerSizes, &
       ngroups_four,npperg_four,first_proc_four,map_four,nprocmax_four, &
       nrecs_gr, nsends_gr, nrecs_f, nrecs_g, nrecs_diag, nsends_diag, & 
       SpectralDomainDecomp, GridDecomposition, VerticalGroups
  USE Utils, ONLY: CreateGaussQuad, DumpGaussQuad, &
       GaussPoints, GaussWeights, CreateAssocLegFunc, DumpAssocLegFunc, &
       LegFuncS2F, CreateGridValues, DestroyAssocLegFunc, allpolynomials
  USE FieldsDynamics, ONLY: InitFields
  USE Constants, ONLY: InitConstants,r8
  USE SpecDynamics, ONLY: InitDZtoUV, InitGozrim, InitImplDifu, &
       InitSemiImpl, InitUvtodz, InitFiltDiss
  USE Transform, ONLY: InitTransform, NextSizeFFT
  USE Communications, ONLY: Set_Communic_buffer, exchange_hallos, exchange_ftog, &
                            exchange_diag

  USE Options, ONLY: &
       nfprt,        &
       nscalars,     &
       fNameWaves,   &
       vmax_est,     &
       slagr,        &
       slhum,        &
       ibdim_size,   &
       nproc_vert,   &
       microphys,    &
       nClass   ,    &
       nAeros   ,    &
       givenfouriergroups,     &
       SL_twotime_scheme,      &
       SetFileNameGaussPoints, &
       SetFileNameWavesPerLat

  USE IOLowLevel, ONLY : &
       ReadMs,           &
       WriteMs



  IMPLICIT NONE
  PRIVATE
  INTEGER           , PUBLIC           :: nls
  INTEGER           , PUBLIC           :: nlcs
  PUBLIC :: InitAll
  LOGICAL, PARAMETER :: dumpLocal=.FALSE.
  REAL(KIND=r8), PARAMETER :: Sigma_Estrat=0.099999_r8
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


  SUBROUTINE InitAll (trunc, vert, reducedGrid, linearGrid, mgiven, gaussgiven, &
       ct_in, cq_in, si_in, sl_in, del_in, dk_in, tk_in )
    INTEGER, INTENT(IN)           :: trunc
    INTEGER, INTENT(IN)           :: vert
    LOGICAL, INTENT(IN)           :: reducedGrid 
    LOGICAL, INTENT(IN)           :: linearGrid 
    LOGICAL, INTENT(IN)           :: mgiven
    LOGICAL, INTENT(IN)           :: gaussgiven
    REAL(KIND=r8),    INTENT(IN)           :: si_in(:)
    REAL(KIND=r8),    INTENT(IN)           :: sl_in(:)
    REAL(KIND=r8),    INTENT(INOUT)        :: dk_in
    REAL(KIND=r8),    INTENT(INOUT)        :: tk_in
    REAL(KIND=r8),    INTENT(OUT)          :: del_in(:)
    REAL(KIND=r8),    INTENT(OUT)          :: ct_in(:) 
    REAL(KIND=r8),    INTENT(OUT)          :: cq_in(:)
    CHARACTER(LEN=*), PARAMETER :: h="**(InitAll)**"
    CHARACTER(LEN=10) :: c1, c2
    INTEGER :: wavesFactor
    INTEGER :: nLon
    INTEGER :: nLat, left
    INTEGER :: j, nf1, nf2
    INTEGER, ALLOCATABLE :: mPerLat(:)
    INTEGER, ALLOCATABLE :: iMaxPerLat(:)

    IF (dumpLocal) THEN
       WRITE(c1,"(i10)") trunc
       WRITE(c2,"(i10)") vert
       WRITE(nfprt,"(a,' trunc=',a,'; vert=',a)",ADVANCE='NO') &
            h, TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2))
       IF (reducedGrid) THEN
          WRITE(nfprt,"('; Reduced and')",ADVANCE='NO')
       ELSE
          WRITE(nfprt,"('; Gaussian and')",ADVANCE='NO')
       END IF
       IF (linearGrid) THEN
          WRITE(nfprt,"(' Linear Grid')")
       ELSE
          WRITE(nfprt,"(' Quadratic Grid')")
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

    nLon = NextSizeFFT(wavesFactor*trunc + 1)
    DO
       left = MOD(nLon,4)
       IF (left == 0) EXIT
       nLon = NextSizeFFT(nLon + 4 - left)
    END DO

    ! number of latitudes

    nLat = nLon/2

    ! verify consistency and register basic sizes

    CALL RegisterBasicSizes(trunc, nLat, nLon, vert)
    IF (dumpLocal) THEN
       WRITE(nfprt,"(a,' will DumpSizes just after RegisterBasicSizes')") h
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

    ! file name for gauss points and weights

    CALL SetFileNameGaussPoints(jMax)

    ! file name for waves per latitude

    CALL SetFileNameWavesPerLat(trunc, jMax)

    ! Initialize GaussQuad (Gaussian points and weights)

    CALL CreateGaussQuad(jMax, gaussgiven)
    IF (dumpLocal) THEN
       WRITE(nfprt,"(a,' will DumpGaussQuad just after creation')") h
       CALL DumpGaussQuad()
    END IF

    ! Spectral Domain Decomposition

    CALL VerticalGroups(givenfouriergroups,nproc_vert)
    CALL SpectralDomainDecomp()

    ! Initialize AssocLegFunc (computes associated legendre functions)

    allpolynomials = reducedGrid.and..not.mgiven
    CALL CreateAssocLegFunc(allpolynomials)

    ! computes mPerLat

    ALLOCATE (mPerLat(jMax))
    IF (reducedGrid) THEN
       IF (mgiven) THEN
          CALL ReadMs(mPerLat,fNameWaves,jMax)
        ELSE
          CALL VaryMWithLat(LegFuncS2F, GaussWeights, mPerLat)
          IF (myId == 0) THEN
             CALL WriteMs(mPerLat,fNameWaves,jMax)
          END IF
       ENDIF
    ELSE
       mPerLat = mMax
    END IF

    ! compute iMaxPerLat

    ALLOCATE(iMaxPerLat(jMax))
    DO j = 1, jMaxHalf
       iMaxPerLat(j) = NextSizeFFT(wavesFactor*(mPerLat(j)-1) + 1)
    END DO
    iMaxPerLat(jMaxHalf+1:jMax)=iMaxPerLat(jMaxHalf:1:-1)

    ! finish building sizes

    CALL RegisterOtherSizes(iMaxPerLat, mPerLat)
    CALL CreateFourierGroup(mygroup_four,myid_four)
    ibmax = ibdim_size
    CALL GridDecomposition(ibmax,jbmax,jbmax_ext,maxnodes,myid,vmax_est)
    CALL Exchange_ftog(nrecs_f,nrecs_g)
    CALL Exchange_diag()
    nf1 = 1+kmax*(nscalars+4)
    nf2 = 2 + kmax*3
    IF (slhum.and..not.slagr) nf1 = kmax*(nscalars+1)
    IF (microphys) nf1 = nf1 + (2+nClass+nAeros)*kmax
    IF ((slagr.or.slhum).and.maxnodes.gt.1) CALL Exchange_Hallos(nrecs_gr,nsends_gr,nf1,nf2)
    IF (dumpLocal) THEN
       WRITE(nfprt,"(a,' will DumpSizes just after RegisterOtherSizes')") h
       CALL DumpSizes()
    END IF

    ! for now on, all global constants defined at sizes can be used

    ! initialize Fields

    CALL InitFields(ibMax, kMax, jbMax, mymnMax, mymnExtMax, kmaxloc, &
                    MNMax_si, jbMax_ext, nscalars)

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

    CALL Set_Communic_buffer()
    ! initialize Gozrim

    CALL InitGozrim()

    CALL InitTransform()

    ! deallocate legendre functions (already stored in transform,
    ! epslon already used in initializations)

    CALL DestroyAssocLegFunc()

  END SUBROUTINE InitAll



  !VaryMWithLat: Implements Courtier-Naughton criteria for computing
  !              wave number per latitude in the reduced grid.
  !              Internal module procedure



!  SUBROUTINE VaryMWithLat(Pmn, W, mPerLat)
!    REAL(KIND=r8),    INTENT(IN ) :: W(jMax)                 ! Gaussian Weights
!    REAL(KIND=r8),    INTENT(IN ) :: Pmn(jMaxHalf, mnExtMax) ! AssocLegFunc
!    INTEGER, INTENT(OUT) :: mPerLat(jMax)           ! Waves per latitude
!    REAL(KIND=r8)    :: Partial(nExtMax, mnExtMax)
!    INTEGER :: j, m, n, nPrime, mn, mnPrime
!    REAL(KIND=r8)    :: erroMax, wj
!    REAL(KIND=r8), PARAMETER :: acceptable=1.0e-12_r8  ! orthogonality acceptable error
!
!
!    ! At a given latitude j, Partial contains the partial summations from
!    ! North Pole up to j that are the contribution of this
!    ! set of latitudes to the orthogonality of Associated Legendre Functions
!    ! of Legendre Degree nPrime and Legendre Order m with respect to all
!    ! Associated Legendre Functions with same Order. While this contribution
!    ! is less than a fixed acceptable error, that particular Order is not
!    ! required for that latitude. (Courier-Naughton criteria)
!
!
!    Partial = 0.0_r8
!    DO j = 1, jMaxHalf
!       wj = w(j)
!       DO m = mMax, 1, -1
!          DO n = m, nExtMax
!             mn = mnExtMap(m,n)
!             DO nPrime = n, nExtMax
!                mnPrime = mnExtMap(m,nPrime)
!                Partial(nPrime,mn) = Partial(nPrime,mn) + &
!                     Pmn(j,mn)*Pmn(j,mnPrime)*Wj
!             END DO
!          END DO
!          erroMax = 0.0_r8
!          DO n = m, nExtMax
!             mn = mnExtMap(m,n)
!             erroMax = Max(erroMax, MAXVAL(ABS(Partial(n:nExtMax,mn))))
!          END DO
!          IF (erroMax > acceptable) THEN
!             mPerLat(j) = MIN(mMax, m+1)
!             EXIT
!          END IF
!       END DO
!    END DO
!    mPerLat(jMaxHalf+1:jMax)=mPerLat(jMaxHalf:1:-1)
!  END SUBROUTINE VaryMWithLat

  SUBROUTINE VaryMWithLat(Pmn, W, mPerLat)
    REAL(KIND=r8),    INTENT(IN ) :: W(jMax)                 ! Gaussian Weights
    REAL(KIND=r8),    INTENT(IN ) :: Pmn(jMaxHalf, mnExtMax) ! AssocLegFunc
    INTEGER, INTENT(OUT) :: mPerLat(jMax)           ! Waves per latitude
    INTEGER :: i, j, m, n, nPrime, mn, mnPrime
    REAL(KIND=r8)    :: erro
    REAL(KIND=r8), PARAMETER :: acceptable=1.0e-12_r8  ! orthogonality acceptable error


    ! At a given latitude j, Partial contains the partial summations from
    ! North Pole up to j that are the contribution of this
    ! set of latitudes to the orthogonality of Associated Legendre Functions
    ! of Legendre Degree nPrime and Legendre Order m with respect to all
    ! Associated Legendre Functions with same Order. While this contribution
    ! is less than a fixed acceptable error, that particular Order is not
    ! required for that latitude. (Courier-Naughton criteria)

       j = jMaxHalf
loopm: DO m = mMax, 1, -1
          DO n = m, nExtMax
             mn = mnExtMap(m,n)
             DO nPrime = n, nExtMax
                mnPrime = mnExtMap(m,nPrime)
                erro = 0.0_r8
                DO i = 1, j
                   erro = erro + Pmn(i,mn)*Pmn(i,mnPrime)*W(i)
                   IF (abs(erro) > acceptable) THEN
                      mPerLat(i:j) = MIN(mMax, m+1)
                      EXIT
                   END IF
                END DO
                j = i-1
                IF (j == 0) EXIT loopm
             END DO
          END DO
       END DO loopm
    mPerLat(jMaxHalf+1:jMax)=mPerLat(jMaxHalf:1:-1)
  END SUBROUTINE VaryMWithLat


  SUBROUTINE Init_ct_cq (kMax, si_in, sl_in, ct_in, cq_in)

    ! Uses MRF86 18-Level ct and cq (Specified Derivatives of
    ! Global Average T and q WRT LOG(Sigma)) to get Departures
    ! of these Quantities from Values at Lowest Layer. 
    ! These Departures are Linearly Interpolated to kMax-Levels, 
    ! and then Used to Compute the Corresponding Derivatives 
    ! ct and cq for the kMax-Level Model


    INTEGER, INTENT(IN) :: kMax

    REAL(KIND=r8), DIMENSION (kMax), INTENT(IN) :: sl_in

    REAL(KIND=r8), DIMENSION (kMax+1), INTENT(IN) :: si_in

    REAL(KIND=r8), DIMENSION (kMax), INTENT(OUT) :: ct_in, cq_in

    INTEGER, PARAMETER :: kMRF=18, kMRFp=kMRF+1, kMRFm=kMRF-1

    INTEGER :: l, k, kk

    REAL(KIND=r8) :: at, bt, aq, bq

    LOGICAL :: Interp

    REAL(KIND=r8), DIMENSION (kMax) :: algsi

    REAL(KIND=r8), DIMENSION (kMax+1) :: glt, glq

    REAL(KIND=r8), DIMENSION (kMRF) :: algsiMRF

    REAL(KIND=r8), DIMENSION (kMRFp) :: gltMRF, glqMRF

    REAL(KIND=r8), PARAMETER :: Eps=0.0001_r8

    ! These Values Are For 18-Layer MRF86 NCEP Vertical Spacing

    REAL(KIND=r8), PARAMETER, DIMENSION (kMRF) :: slMRF = (/ &
         0.995004_r8, 0.981488_r8, 0.960480_r8, 0.920400_r8, 0.856317_r8, 0.777224_r8, &
         0.688125_r8, 0.593540_r8, 0.497450_r8, 0.424818_r8, 0.374806_r8, 0.324767_r8, &
         0.274735_r8, 0.224668_r8, 0.174576_r8, 0.124402_r8, 0.073986_r8, 0.020747_r8 /)

    REAL(KIND=r8), PARAMETER, DIMENSION (kMRFp) :: siMRF = (/ &
         1.000000_r8, 0.990000_r8, 0.973000_r8, 0.948000_r8, 0.893000_r8, 0.820000_r8, &
         0.735000_r8, 0.642000_r8, 0.546000_r8, 0.450000_r8, 0.400000_r8, 0.350000_r8, &
         0.300000_r8, 0.250000_r8, 0.200000_r8, 0.150000_r8, 0.100000_r8, 0.050000_r8, 0.0_r8 /)

    REAL(KIND=r8), PARAMETER, DIMENSION (kMRF) :: ctMRF = (/ &
         38.1_r8, 38.2_r8, 38.7_r8, 39.3_r8, 40.5_r8, 42.5_r8, 44.8_r8, 46.8_r8, 47.1_r8, &
         47.9_r8, 47.6_r8, 43.2_r8, 37.2_r8, 29.9_r8, 23.3_r8,  9.7_r8,  0.0_r8,  0.0_r8 /)

    REAL(KIND=r8), PARAMETER, DIMENSION (kMRF) :: cqMRF = (/ &
         0.0270_r8, 0.0290_r8, 0.0290_r8, 0.0270_r8, 0.0190_r8, 0.0110_r8, &
         0.0079_r8, 0.0057_r8, 0.0038_r8, 0.0024_r8, 0.0014_r8, 0.0010_r8, &
         0.0000_r8, 0.0000_r8, 0.0000_r8, 0.0000_r8, 0.0000_r8, 0.0000_r8  /)

    IF (kMRF /= kMax) THEN
       Interp=.TRUE.
    ELSE
       Interp=ANY((slMRF-sl_in) > Eps)
    END IF

    IF (Interp) THEN

       ! Integrate To Get Global Average T and q Differences From Lowest Layer

       gltMRF(1)=0.0_r8
       DO l=1,kMRFm
          gltMRF(l+1)=gltMRF(l)+ctMRF(l)*(LOG(siMRF(l+1))-LOG(siMRF(l)))
       END DO
       gltMRF(kMRFp)=gltMRF(kMRF)
       glqMRF(1)=0.0_r8
       DO l=1,kMRFm
          glqMRF(l+1)=glqMRF(l)+cqMRF(l)*(LOG(siMRF(l+1))-LOG(siMRF(l)))
       END DO
       glqMRF(kMRFp)=glqMRF(kMRF)
       DO l=1,kMRF
          algsiMRF(l)=LOG(siMRF(l))
       END DO
       DO k=1,kMax
          algsi(k)=LOG(si_in(k))
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
          IF (ABS(ct_in(k)) <= Eps) ct_in(k)=0.0_r8
          cq_in(k)=(glq(k+1)-glq(k))/(algsi(k+1)-algsi(k))
          IF (ABS(cq_in(k)) <= Eps) cq_in(k)=0.0_r8
       END DO
       ct_in(kMax)=0.0_r8
       cq_in(kMax)=0.0_r8

    ELSE

       DO l=1,kMRF
          ct_in(l)=ctMRF(l)
          cq_in(l)=cqMRF(l)
       END DO

    END IF
    ! 
    ! cq_in = 0 removes the contribution of the term dependent of the
    ! topography in the equation of horizontal diffusion of moisture.
    !
    cq_in=0.0_r8

  END SUBROUTINE Init_ct_cq

END MODULE Init
