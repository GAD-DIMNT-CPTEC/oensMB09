!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:37 $
!  $Revision: 1.1.1.1 $
!
MODULE Transform

  USE Constants, ONLY: &
       eriv

  USE Utils, ONLY: &
       LegFuncS2F, &
       LegFuncF2S, &
       NoBankConflict, &
       DumpMatrix

  USE Sizes, ONLY: &
       mMax,       &
       nMax,       &
       nExtMax,    &
       mnMax,      &
       mnExtMax,   &
       iMax,       &
       jMax,       &
       jMaxHalf,   &
       kMax,       &
       ibMax,      &
       ibMaxPerJB, &
       jbMax,      &
       ibPerIJ,    &
       jPerIJB,    &
       jMinPerM,   &
       iMaxPerJ,   &
       ThreadDecomp

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: InitTransform
  PUBLIC :: CreateSpecToGrid
  PUBLIC :: DepositSpecToGrid
  PUBLIC :: DepositSpecToDelLamGrid
  PUBLIC :: DepositSpecToGridAndDelLamGrid
  PUBLIC :: DoSpecToGrid
  PUBLIC :: DestroySpecToGrid
  PUBLIC :: CreateGridToSpec
  PUBLIC :: DepositGridToSpec
  PUBLIC :: DoGridToSpec
  PUBLIC :: DestroyGridToSpec
  PUBLIC :: NextSizeFFT

  ! Domain Decomposition 
  !
  ! There are maxNodes MPI processes, numbered from 0 to maxNodes - 1
  !
  ! The number of this MPI process is myId, 0 <= myId <= maxNodes - 1

  INTEGER :: maxNodes
  INTEGER :: myId


  ! Spectral Decomposition
  !
  ! Spectral coefficients are spreaded across processes.
  ! The set of values of m is partitioned among processes.
  ! Consequently, all values of n for some values of m are stored
  ! at each process. Each value of m is stored at a single process.
  !
  ! Array nodeHasM(m) has which MPI process stores m, 1 <= m <= mMax
  !
  ! Variable myMMax (0 <= myMMax <= mMax) has how many m's are stored at this node.
  !
  ! Array lm2m(l), l=1,...,myMMax has the m's stored at this node.


  INTEGER, ALLOCATABLE :: nodeHasM(:)
  INTEGER, ALLOCATABLE :: lm2m(:)

  INTEGER              :: myMMax
  INTEGER              :: myMNMax
  INTEGER              :: myMNExtMax
  INTEGER              :: mGlob      ! counter of m for all threads in this MPI process

  INTEGER, ALLOCATABLE :: myMMap(:)
  INTEGER, ALLOCATABLE :: myNMap(:)
  INTEGER, ALLOCATABLE :: myMNMap(:,:)
  INTEGER, ALLOCATABLE :: myMExtMap(:)
  INTEGER, ALLOCATABLE :: myNExtMap(:)
  INTEGER, ALLOCATABLE :: myMNExtMap(:,:)


  ! Grid Point Decomposition
  !
  ! Blocks of surface points are spreaded across processes.
  ! Each process has all longitudes of a set of latitudes (block)
  !
  ! Array nodeHasJB(jb) has which MPI process stores block jb, 1 <= jb <= jbMax
  !
  ! Variable myJBMax (0 <= myJBMax <= jbMax) has how many jb's are stored at this node.
  !
  ! Array ljb2jb(l), l=1,...,myJBMax has the jb's stored at this node.


  INTEGER, ALLOCATABLE :: nodeHasJB(:)
  INTEGER, ALLOCATABLE :: ljb2jb(:)

  INTEGER              :: myJBMax

  ! variable myJMax has how many latitudes stored at this proc
  ! first stored latitude at each local block is jFirstLJB(myJBMax)
  ! last stored latitude at each local block is jLastLJB(myJBMax)

  INTEGER              :: myJMax
  INTEGER, ALLOCATABLE :: jFirstLJB(:)
  INTEGER, ALLOCATABLE :: jLastLJB(:)

  ! Legendre and Fourier Transform
  ! 
  ! Internal representation of Spectral Fields
  !
  ! Array Spe(dlmn, dv), using Spe(myMNExtMax, 2*nVertSpec)
  ! dlmn and dv avoid memory bank conflict
  !
  ! second dimension of Spe maps real and imaginary verticals 
  ! of all fields to be transformed to an 1D structure 
  ! in array element order of:(Vertical, Field, RealImaginary).
  ! 
  ! first dimension of Spe maps (lm,n) into an 1D structure st
  ! n goes faster than lm; for each lm, all n are represented.
  ! lm are stored sequentially.
  ! For each lm, first store all n st n+m is even, followed by
  ! all n st n+m is odd.
  !
  !  lm2m(1)
  ! S(n,v)
  !  even
  !
  !  lm2m(1)
  ! S(n,v)
  !  odd
  !
  !  lm2m(2)
  ! S(n,v)
  !  even
  !
  !  lm2m(2)
  ! S(n,v)
  !  odd
  !
  ! .......
  !
  !  lm2m(myMMax)
  ! S(n,v)
  !  even
  !
  !  lm2m(myMMax)
  ! S(n,v)
  !  odd
  !
  ! PAD
  !
  ! nEven(lm) has the number of spectral coeficients (real or imag) for m+n even
  ! nOdd(lm) has the number of spectral coeficients (real or imag) for m+n odd
  !
  ! dnEven(lm) is the dimensioning of nEven(lm) to avoid bank conflicts
  ! dnOdd(lm) is the dimensioning of nEven(lm) to avoid bank conflicts
  !
  ! firstNEven(lm) points to the first row of even n's for lm
  ! firstNOdd(lm) points to the first row of odd n's for lm
  ! 
  ! lmnExtMap(lmn), lmn = 1,...,myMNExtMax maps the external representation
  ! of Spectral Extended Fields to the transform internal representation,
  ! for a single vertical
  !
  ! lmnMap(lmn), lmn = 1,...,myMNMax maps the external representation
  ! of Spectral Regular Fields to the transform internal representation,
  ! for a single vertical
  !
  ! lastv stores the last used vertical of the real part of S; 
  ! first free vertical (of the real part) is lastv+1
  !
  ! nVertSpec stores the distance (in verticals) from the real to the imaginary part
  !
  ! first vertical (of the real part) of each stored field is thisv


  ! Fourier Output of Matrix Multiplications
  !
  ! Array FEO(dv, djhf, dlm), using (2*nVertSpec, jMaxHf, 2*lm)
  !
  ! first dimension of FEO maps real and imaginary verticals 
  ! of all fields to an 1D structure 
  ! in array element order of:(Vertical, Field, RealImaginary),
  ! identical to the second dimension of Spe.
  ! 
  ! second dimension of FEO contains latitudes, from 1 to
  ! jMaxHf on even fields, and from jMax to jMaxHf+1 on odd
  ! fields. Not all latitudes are used; for each m, latitudes
  ! jMinPerM(m) to jMaxHf on even fields and jMax-jMinPerM(m)+1 to
  ! jMaxHf on odd fields. Remaining latitudes are null.
  !
  ! third dimension are even/odd values of m, on the order
  ! defined by lm2m, both values for each m.

  ! Fourier Fields
  !
  ! Array Fou(dvdlj, dmp1), using (nVertGrid*myJMax, 2*mMax+1)
  ! dvdlj and di avoid memory bank conflict. nVertGrid is the
  ! total number of verticals stored (sum over all fields,
  ! including lambda derivatives in Spectral to Grids transforms)
  !
  ! first dimension of Fou maps verticals of all fields 
  ! and latitudes stored by this process into an 1D structure 
  ! in array element order of:(Vertical, Field, latitude).
  ! 
  ! Second dimension of Fou contains real and imaginary
  ! fourier coefficients, in this order. A surplus value is
  ! required for the FFT (null values).

  ! Internal representation of Gridsian Fields
  !
  ! Array Grid(dvdlj, di), using Grid(nVertGrid*myJMax, iMax)
  ! dvdlj and di avoid memory bank conflict. nVertGrid is the
  ! total number of verticals stored (sum over all fields,
  ! including lambda derivatives in Spectral to Grids transforms)
  !
  ! first dimension of Grid maps verticals of all fields 
  ! and latitudes stored by this process into an 1D structure 
  ! in array element order of:(Vertical, Field, latitude).
  ! 
  ! Second dimension of Grid is longitude.


  INTERFACE DepositSpecToGrid
     MODULE PROCEDURE Deposit1D, Deposit2D
  END INTERFACE

  INTERFACE DepositSpecToDelLamGrid
     MODULE PROCEDURE DepDL1D, DepDL2D
  END INTERFACE

  INTERFACE DepositSpecToGridAndDelLamGrid
     MODULE PROCEDURE DepDLG1D, DepDLG2D
  END INTERFACE

  INTERFACE DestroySpecToGrid
     MODULE PROCEDURE Destroy
  END INTERFACE

  INTERFACE DepositGridToSpec
     MODULE PROCEDURE Deposit1D, Deposit2D
  END INTERFACE

  INTERFACE DestroyGridToSpec
     MODULE PROCEDURE Destroy
  END INTERFACE

  REAL, ALLOCATABLE :: Spec(:,:)
  REAL, ALLOCATABLE :: Four(:,:)

  TYPE p1d
     REAL, POINTER :: p(:)
  END TYPE p1d

  TYPE p2d
     REAL, POINTER :: p(:,:)
  END TYPE p2d

  TYPE p3d
     REAL, POINTER :: p(:,:,:)
  END TYPE p3d

  INTEGER                :: nSpecFields        ! total # Spectral Fields in transform
  INTEGER                :: usedSpecFields     ! # Spectral Fields currently deposited
  INTEGER                :: lastUsedSpecVert   ! at internal array Spec
  LOGICAL,   ALLOCATABLE :: surfSpec(:)        ! TRUE iff Surface Spectral Field
  INTEGER,   ALLOCATABLE :: prevSpec(:)        ! prior to first real vertical of this field at all internal arrays
  TYPE(p1d), ALLOCATABLE :: Spec1d(:)          ! points to Surface Spectral Field
  TYPE(p2d), ALLOCATABLE :: Spec2d(:)          ! points to Full Spectral Field

  INTEGER                :: nGridFields        ! total # Grid Fields in transform
  INTEGER                :: usedGridFields     ! # Grid Fields currently deposited
  INTEGER                :: lastUsedGridVert   ! at internal array Grid
  LOGICAL,   ALLOCATABLE :: surfGrid(:)        ! TRUE iff Surface Grid Field
  INTEGER,   ALLOCATABLE :: prevGrid(:)        ! prior to first real vertical of this field at all internal arrays
  TYPE(p2d), ALLOCATABLE :: Grid2d(:)          ! points to Surface Grid Field
  TYPE(p3d), ALLOCATABLE :: Grid3d(:)          ! points to Full Grid Field
  LOGICAL,   ALLOCATABLE :: fieldForDelLam(:)  ! TRUE iff this field position stores Lambda Derivative

  LOGICAL                :: willDelLam
  INTEGER                :: usedDelLamFields   ! last position at Grid array used for Lambda Der. fields
  INTEGER                :: lastUsedDelLamVert ! at Grid array
  INTEGER,   ALLOCATABLE :: prevVertDelLamSource(:)  ! source of Lambda Derivative


  INTEGER              :: dlmn
  INTEGER, ALLOCATABLE :: nEven(:)
  INTEGER, ALLOCATABLE :: dnEven(:)
  INTEGER, ALLOCATABLE :: firstNEven(:)
  INTEGER, ALLOCATABLE :: nOdd(:)
  INTEGER, ALLOCATABLE :: dnOdd(:)
  INTEGER, ALLOCATABLE :: firstNOdd(:)
  INTEGER, ALLOCATABLE :: lmnExtMap(:)
  INTEGER, ALLOCATABLE :: lmnMap(:)
  INTEGER, ALLOCATABLE :: lmnZero(:)


  INTEGER              :: nVertSpec
  INTEGER              :: nVertGrid
  INTEGER              :: dv
  INTEGER              :: dvdlj
  INTEGER              :: di
  INTEGER              :: dip1
  INTEGER, ALLOCATABLE :: previousJ(:)

  INTEGER              :: djh
  INTEGER              :: dvjh

  !  HIDDEN DATA, FFT SIZE INDEPENDENT:
  !  nBase=SIZE(Base)
  !  Base are the bases for factorization of n; base 4 should come
  !       before base 2 to improve FFT efficiency
  !  Permutation defines order of bases when computing FFTs
  !  Trigonometric constants required for computing FFTs



  INTEGER, PARAMETER   :: nBase=4
  INTEGER, PARAMETER   :: Base(nBase) = (/ 4, 2, 3, 5 /)
  INTEGER, PARAMETER   :: Permutation(nBase) = (/ 2, 3, 1, 4 /)
  REAL                 :: sin60
  REAL                 :: sin36
  REAL                 :: sin72
  REAL                 :: cos36
  REAL                 :: cos72


  INTEGER              :: nFactors
  INTEGER, POINTER     :: Factors(:)
  INTEGER              :: nTrigs
  REAL,    POINTER     :: Trigs(:)
  REAL,    ALLOCATABLE :: LS2F(:,:)
  REAL,    ALLOCATABLE :: LF2S(:,:)



  ! Hiden Data, OpenMP parallelism

  LOGICAL, PARAMETER :: dumpLocal=.FALSE.
CONTAINS



  SUBROUTINE InitTransform()
    INTEGER :: m
    INTEGER :: n
    INTEGER :: mn
    INTEGER :: mBase
    INTEGER :: mMid
    INTEGER :: diag
    INTEGER :: ele
    INTEGER :: lm
    INTEGER :: lmn
    INTEGER :: prev
    INTEGER :: ljb
    INTEGER :: j
    INTEGER :: jb
    INTEGER :: jbLast
    INTEGER :: jbThis
    INTEGER :: proc
    REAL    :: frac
    CHARACTER(LEN=*), PARAMETER :: h="**(InitTransform)**"


    maxNodes = 1
    myId = 0

    ALLOCATE(nodeHasM(mMax))
    DO mBase = 1, mMax, 2*maxNodes
       mMid = mBase+maxNodes-1
       DO m = mBase, MIN(mMid, mMax)
          nodeHasM(m) = m - mBase
       END DO
       DO m = mMid+1, MIN(mMid+maxNodes, mMax)
          nodeHasM(m) = maxNodes + mMid - m
       END DO
    END DO

    myMMax = COUNT(nodeHasM == myId)

    ALLOCATE(lm2m(myMMax))


    lm = 0
    myMNMax = 0
    myMNExtMax = 0
    DO m = 1, mMax
       IF (nodeHasM(m) == myId) THEN
          lm = lm + 1
          lm2m(lm) = m
          myMNMax      = myMNMax    + mMax - m + 1
          myMNExtMax   = myMNExtMax + mMax - m + 2
       END IF
    END DO


    ALLOCATE (myMExtMap(myMNExtMax))
    ALLOCATE (myNExtMap(myMNExtMax))
    ALLOCATE (myMNExtMap(myMMax,nExtMax))
    myMExtMap = -1  ! flag mapping error
    myNExtMap = -1  ! flag mapping error
    myMNExtMap = -1 ! flag mapping error
    mn = 0
    DO diag = 1, nExtMax ! diagonal
       lm = 0
       DO ele = 1, MIN(mMax-diag+2, mMax)
          IF (nodeHasM(ele) == myId) THEN
             lm = lm + 1
             n = ele+diag-1
             mn = mn + 1
             myMNExtMap(lm,n) = mn
             myMExtMap(mn)    = lm
             myNExtMap(mn)    = n
          END IF
       END DO
    END DO


    ALLOCATE (myMNMap(myMMax,nMax))
    ALLOCATE (myMMap(myMNMax))
    ALLOCATE (myNMap(myMNMax))
    myMNMap = -1  ! flag mapping error
    myMMap = -1   ! flag mapping error
    myNMap = -1   ! flag mapping error
    mn = 0
    DO diag = 1, nMax 
       lm = 0
       DO ele = 1, mMax-diag+1
          IF (nodeHasM(ele) == myId) THEN
             lm = lm + 1
             n = ele+diag-1
             mn = mn + 1
             myMNMap(lm,n) = mn
             myMMap(mn)    = lm
             myNMap(mn)    = n
          END IF
       END DO
    END DO

    ALLOCATE(nodeHasJB(jbMax))
    frac = REAL(jbMax)/REAL(maxNodes)
    jbLast = 0
    DO proc = 0, maxNodes-1
       jbThis = INT (REAL(proc+1)*frac)
       nodeHasJB(jbLast+1:jbThis) = proc
       jbLast = jbThis
    END DO

    myJBMax = COUNT(nodeHasJB == myId)
    ALLOCATE(ljb2jb(myJBMax))
    ALLOCATE(jFirstLJB(myJBMax))
    ALLOCATE(jLastLJB(myJBMax))
    ljb = 0
    myJMax = 0
    DO jb = 1, jbMax
       IF (nodeHasJB(jb) == myId) THEN
          ljb = ljb + 1
          ljb2jb(ljb) = jb
          JLastLJB (ljb) = jPerIJB(ibMaxPerJB(jb),jb) 
          JFirstLJB(ljb) = jPerIJB(1,jb)
       END IF
    END DO

    myJMax = SUM(jLastLJB(:)-jFirstLJB(:)+1)

    IF (dumpLocal) THEN
!       CALL DumpMySize()
    END IF

    ! fim de MySize

    ALLOCATE(nEven(myMMax))
    ALLOCATE(nOdd(myMMax))
    DO lm = 1, myMMax
       m = lm2m(lm)
       nEven(lm) = (mMax - m + 3)/2
       nOdd(lm)  = (mMax - m + 2)/2
    END DO

    IF (dumpLocal) THEN
       PRINT *, h, "iMax, jMax, kMax=",iMax,jmax,kMax
!       PRINT *, h, " nEven=", nEven
!       PRINT *, h, " nOdd=", nOdd
    END IF

    ALLOCATE(dnEven(myMMax))
    ALLOCATE(dnOdd(myMMax))
    dnEven = NoBankConflict(nEven)
    dnOdd  = NoBankConflict(nOdd)

    ALLOCATE(firstNEven(myMMax))
    ALLOCATE(firstNOdd(myMMax))
    firstNEven(1) = 1
    firstNOdd(1)  = nEven(1) + 1
    DO lm = 2, myMMax
       firstNEven(lm) = firstNOdd(lm-1) + nOdd(lm-1)
       firstNOdd (lm) = firstNEven(lm)  + nEven(lm)
    END DO

    dlmn = SUM(nEven(1:myMMax)) + SUM(nOdd(1:myMMax-1)) + dnOdd(myMMax)
    dlmn = NoBankConflict(dlmn)

!    IF (dumpLocal) THEN
!       PRINT *, h, " firstNEven=", firstNEven
!       PRINT *, h, " firstNOdd=", firstNOdd
!       PRINT *, h, " dlmn=", dlmn
!    END IF

    ALLOCATE(lmnExtMap(myMNExtMax))
    DO lmn = 1, myMNExtMax
       lm = myMExtMap(lmn)
       n  = myNExtMap(lmn)
       m  = lm2m(lm)
       IF (MOD(m+n,2) == 0) THEN
          lmnExtMap(lmn) = firstNEven(lm) + (n - m)/2
       ELSE
          lmnExtMap(lmn) = firstNOdd(lm) + (n - m - 1)/2
       END IF
    END DO

    ALLOCATE(lmnMap(myMNMax))
    DO lmn = 1, myMNMax
       lm = myMMap(lmn)
       n  = myNMap(lmn)
       m  = lm2m(lm)
       IF (MOD(m+n,2) == 0) THEN
          lmnMap(lmn) = firstNEven(lm) + (n - m)/2
       ELSE
          lmnMap(lmn) = firstNOdd(lm) + (n - m - 1)/2
       END IF
    END DO

    ALLOCATE(lmnZero(myMMax))
    DO lm = 1, myMMax
       lmnZero(lm) = lmnExtMap(myMNExtMap(lm,mMax+1))
    END DO

!    IF (dumpLocal) THEN
!       PRINT *, h, " lmnExtMap=", lmnExtMap
!       PRINT *, h, " lmnMap=", lmnMap
!       PRINT *, h, " lmnZero=", lmnZero
!    END IF

    di = NoBankConflict(iMax)
    dip1 = NoBankConflict(iMax+1)

    ALLOCATE(previousJ(jMax))
    previousJ=0
    prev = 0
    DO ljb = 1, myJBMax
       DO j = jFirstLJB(ljb), jLastLJB(ljb)
          previousJ(j) = prev + j - jFirstLJB(ljb)
       END DO
       prev = prev + jLastLJB(ljb) - jFirstLJB(ljb) + 1
    END DO

!    IF (dumpLocal) THEN
!       PRINT *, h, " di=", di
!       PRINT *, h, " previousJ=", previousJ
!    END IF

    djh = NoBankConflict(jMaxHalf)

    ALLOCATE(LS2F(djh, dlmn))

    DO lmn = 1, myMNExtMax
       DO j = 1, jMaxHalf
          LS2F(j,lmnExtMap(lmn)) = LegFuncS2F(j,lmn)
       END DO
    END DO
    LS2F(jMaxHalf+1:djh,                 :) = 0.0
    LS2F(:,              myMNExtMax+1:dlmn) = 0.0

    ALLOCATE(LF2S(dlmn, djh))

    DO j = 1, jMaxHalf
       DO lmn = 1, myMNExtMax
          LF2S(lmnExtMap(lmn),j) = LegFuncF2S(lmn,j)
       END DO
    END DO
    LF2S(:,                 jMaxHalf+1:djh) = 0.0
    LF2S(myMNExtMax+1:dlmn,           :   ) = 0.0

    CALL CreateFFT(iMaxPerJ(1), Factors, Trigs) ! hardwired para grade gausseana

  END SUBROUTINE InitTransform





  SUBROUTINE DumpMySize()
    CHARACTER(LEN=*), PARAMETER :: h="**(DumpMySize)**"
    CHARACTER(LEN=10) :: c0, c1, c2

    WRITE(c0,"(i10)") maxNodes
    WRITE(c1,"(i10)") myId

    PRINT *, h//" There are "//TRIM(ADJUSTL(c0))//&
         &" MPI processes; this is #"//TRIM(ADJUSTL(c1))

    WRITE(c0,"(i10)") myMMax
    WRITE(c1,"(i10)") myMNMax
    WRITE(c2,"(i10)") myMNExtMax

    PRINT *, h//" This process stores "//&
         TRIM(ADJUSTL(c0))//" Fourier wave numbers with "//&
         TRIM(ADJUSTL(c1))//" spectral coef on regular grids and "//&
         TRIM(ADJUSTL(c2))//" on extended grids"

    CALL DumpMatrix ("nodeHasM", nodeHasM)
    CALL DumpMatrix ("lm2m", lm2m)
    CALL DumpMatrix ("myMMap", myMMap)
    CALL DumpMatrix ("myNMap", myNMap)
    CALL DumpMatrix ("myMNMap", myMNMap)
    CALL DumpMatrix ("myMExtMap", myMExtMap)
    CALL DumpMatrix ("myNExtMap", myNExtMap)
    CALL DumpMatrix ("myMNExtMap", myMNExtMap)

    WRITE(c0,"(i10)") myJBMax
    PRINT *, h//" This process stores "//TRIM(ADJUSTL(c0))//" latitude blocks "
    CALL DumpMatrix ("nodeHasJB",nodeHasJB)
    CALL DumpMatrix ("ljb2jb", ljb2jb)

    WRITE(c0,"(i10)") myJMax
    PRINT *, h//" This process stores "//TRIM(ADJUSTL(c0))//" latitudes "
    CALL DumpMatrix ("jFirstLJB",jFirstLJB)
    CALL DumpMatrix ("jLastLJB",jLastLJB)
  END SUBROUTINE DumpMySize





  SUBROUTINE CreateSpecToGrid(nFullSpec, nSurfSpec, nFullGrid, nSurfGrid)
    INTEGER, INTENT(IN) :: nFullSpec
    INTEGER, INTENT(IN) :: nSurfSpec
    INTEGER, INTENT(IN) :: nFullGrid
    INTEGER, INTENT(IN) :: nSurfGrid
    CHARACTER(LEN=*), PARAMETER :: h="**(CreateSpecToGrid)**"

    nSpecFields = nFullSpec + nSurfSpec
    usedSpecFields = 0
    lastUsedSpecVert = 0
    ALLOCATE(surfSpec(nSpecFields))
    ALLOCATE(prevSpec(nSpecFields))
    ALLOCATE(Spec1d(nSpecFields))
    ALLOCATE(Spec2d(nSpecFields))
    nVertSpec = nFullSpec*kMax + nSurfSpec

    nGridFields = nFullGrid + nSurfGrid
    usedGridFields = 0
    lastUsedGridVert = 0
    ALLOCATE(surfGrid(nGridFields))
    ALLOCATE(prevGrid(nGridFields))
    ALLOCATE(Grid2d(nGridFields))
    ALLOCATE(Grid3d(nGridFields))
    ALLOCATE(fieldForDelLam(nGridFields))
    nVertGrid = nFullGrid*kMax + nSurfGrid

    willDelLam = .FALSE.
    usedDelLamFields = nSpecFields
    lastUsedDelLamVert = nVertSpec
    ALLOCATE(prevVertDelLamSource(nGridFields))

    dv = NoBankConflict(2*nVertSpec)
    dvjh = NoBankConflict(nVertSpec*jMaxHalf)
    dvdlj = NoBankConflict(nVertGrid*myJMax)

    ALLOCATE (Spec(dlmn, dv))
    ALLOCATE (Four(dvdlj, dip1))

    IF (dumpLocal) THEN
       WRITE(*,"(a,' Spec: n, used, lastVert, nVert=',4i5)") &
            h, nSpecFields, usedSpecFields, lastUsedSpecVert, nVertSpec
       WRITE(*,"(a,' Grid: n, used, lastVert, nVert=',4i5)") &
            h, nGridFields, usedGridFields, lastUsedGridVert, nVertGrid
       WRITE(*,"(a,' DelLam: used, lastVert=',2i5)") &
            h, usedDelLamFields, lastUsedDelLamVert
    END IF
  END SUBROUTINE CreateSpecToGrid



  SUBROUTINE Deposit1D (ArgSpec, ArgGrid, ArgDelLam)
    REAL, TARGET, INTENT(IN) :: ArgSpec(:)
    REAL, TARGET, OPTIONAL, INTENT(IN) :: ArgGrid(:,:)
    REAL, TARGET, OPTIONAL, INTENT(IN) :: ArgDelLam(:,:)
    CHARACTER(LEN=*), PARAMETER :: h="**(Deposit1D)**"
    LOGICAL :: getGrid, getDelLam
    INTEGER :: lusf
    INTEGER :: pusv
    INTEGER :: lusv

    INTEGER :: lugf
    INTEGER :: pugv
    INTEGER :: lugv

    INTEGER :: ludlf
    INTEGER :: pudlv
    INTEGER :: ludlv

    ! Grid field required?
    ! DelLam field required?

    getGrid   = PRESENT(ArgGrid)
    getDelLam = PRESENT(ArgDelLam)

    ! critical section to update data structure counters 

!$OMP CRITICAL(ControlDeposit)
    lusf = usedSpecFields + 1
    usedSpecFields = lusf
    pusv = lastUsedSpecVert 
    lusv = pusv + 1
    lastUsedSpecVert = lusv
    IF (getGrid) THEN
       lugf = usedGridFields+1
       usedGridFields = lugf
       pugv = lastUsedGridVert
       lugv = pugv + 1
       lastUsedGridVert = lugv
       IF (getDelLam) THEN
          ludlf = usedDelLamFields + 1
          usedDelLamFields = ludlf
          pudlv = lastUsedDelLamVert 
          ludlv = pudlv + 1
          lastUsedDelLamVert = ludlv
       END IF
    ELSE IF (getDelLam) THEN
       lugf = usedGridFields+1
       usedGridFields = lugf
       pugv = lastUsedGridVert
       lugv = pugv + 1
       lastUsedGridVert = lugv
    ELSE
       WRITE(*,"(a, ' no Grid or DelLam field required')") h
       STOP h
    END IF
!$OMP END CRITICAL(ControlDeposit)

    ! deposit spectral field

    IF (lusf <= nSpecFields) THEN
       surfSpec(lusf) = .TRUE.
       prevSpec(lusf) = pusv
       NULLIFY(Spec2d(lusf)%p)
       Spec1d(lusf)%p => ArgSpec
    ELSE
       WRITE(*,"(a, ' too many spectral fields')") h
       WRITE(*,"(a, ' used=',i5,'; declared=',i5)") h, lusf, nSpecFields
       STOP h
    END IF

    IF (dumpLocal) THEN
       WRITE(*,"(a,' usedSpecFields, lastUsedSpecVert, surfSpec, prevSpec=', 2i5,l2,i5)") &
            h, lusf, lusv, surfSpec(lusf), prevSpec(lusf)
    END IF

    IF (getGrid) THEN

       ! ArgGrid Field is required; 
       ! Store ArgGrid Field info at first available position of the Grid array

       IF (lugf <= nSpecFields) THEN
          surfGrid(lugf) = .TRUE.
          prevGrid(lugf) = pugv
          NULLIFY(Grid3d(lugf)%p)
          Grid2d(lugf)%p => ArgGrid
          fieldForDelLam(lugf) = .FALSE.
          prevVertDelLamSource(lugf) = -1
       ELSE
          WRITE(*,"(a, ' too many grid fields')") h
          WRITE(*,"(a, ' used=',i5,'; declared=',i5)") h, lugf, nSpecFields
          STOP h
       END IF

       IF (dumpLocal) THEN
          WRITE(*,"(a,' usedGridFields, lastUsedGridVert, surfGrid, prevGrid=', 2i5,l2,i5)") &
               h, lugf, lugv, surfGrid(lugf), prevGrid(lugf)
          WRITE(*,"(a,' fieldForDelLam, prevVertDelLamSource=', l2,i5)") &
               h, fieldForDelLam(lugf), prevVertDelLamSource(lugf)
       END IF

       IF (getDelLam) THEN

          ! ArgGrid and ArgDelLam are required; 
          ! Store ArgDelLam info at first available position of the Grid array
          ! Mark field for DelLam

          willDelLam = .TRUE.
          IF (ludlf <= nGridFields) THEN
             surfGrid(ludlf) = .TRUE.
             prevGrid(ludlf) = pudlv
             NULLIFY(Grid3d(ludlf)%p)
             Grid2d(ludlf)%p => ArgDelLam
             fieldForDelLam(ludlf) = .TRUE.
             prevVertDelLamSource(ludlf) = prevGrid(lugf)
          ELSE
             WRITE(*,"(a, ' too many grid fields including DelLam')") h
             WRITE(*,"(a, ' used=',i5,'; declared=',i5)") h, ludlf, nGridFields
             STOP h
          END IF

          IF (dumpLocal) THEN
             WRITE(*,"(a,' usedDelLamFields, lastUsedDelLamVert, surfGrid, prevGrid=', 2i5,l2,i5)") &
                  h, ludlf, ludlv, surfGrid(ludlf), prevGrid(ludlf)
             WRITE(*,"(a,' fieldForDelLam, prevVertDelLamSource=', l2,i5)") &
                  h, fieldForDelLam(ludlf), prevVertDelLamSource(ludlf)
          END IF
       END IF

    ELSE IF (getDelLam) THEN

       ! ArgDelLam Field is required; Grid is not
       ! Store ArgDelLam info at first available position of the Grid array
       ! Mark field for DelLam

       willDelLam = .TRUE.
       IF (lugf <= nSpecFields) THEN
          surfGrid(lugf) = .TRUE.
          prevGrid(lugf) = pugv
          NULLIFY(Grid3d(lugf)%p)
          Grid2d(lugf)%p => ArgDelLam
          fieldForDelLam(lugf) = .TRUE.
          prevVertDelLamSource(lugf) = prevGrid(lugf)
       ELSE
          WRITE(*,"(a, ' too many grid fields')") h
          WRITE(*,"(a, ' used=',i5,'; declared=',i5)") h, lugf, nSpecFields
          STOP h
       END IF

       IF (dumpLocal) THEN
          WRITE(*,"(a,' usedGridFields, lastUsedGridVert, surfGrid, prevGrid=', 2i5,l2,i5)") &
               h, lugf, lugv, surfGrid(lugf), prevGrid(lugf)
          WRITE(*,"(a,' fieldForDelLam, prevVertDelLamSource=', l2,i5)") &
               h, fieldForDelLam(lugf), prevVertDelLamSource(lugf)
       END IF
    END IF
  END SUBROUTINE Deposit1D



  SUBROUTINE Deposit2D (ArgSpec, ArgGrid, ArgDelLam)
    REAL, TARGET, INTENT(IN) :: ArgSpec(:,:)
    REAL, TARGET, OPTIONAL, INTENT(IN) :: ArgGrid(:,:,:)
    REAL, TARGET, OPTIONAL, INTENT(IN) :: ArgDelLam(:,:,:)
    CHARACTER(LEN=*), PARAMETER :: h="**(Deposit2D)**"
    LOGICAL :: getGrid, getDelLam
    INTEGER :: lusf
    INTEGER :: pusv
    INTEGER :: lusv

    INTEGER :: lugf
    INTEGER :: pugv
    INTEGER :: lugv

    INTEGER :: ludlf
    INTEGER :: pudlv
    INTEGER :: ludlv

    ! Grid field required?
    ! DelLam field required?

    getGrid   = PRESENT(ArgGrid)
    getDelLam = PRESENT(ArgDelLam)

    ! critical section to update data structure counters 

!$OMP CRITICAL(ControlDeposit)
    lusf = usedSpecFields + 1
    usedSpecFields = lusf
    pusv = lastUsedSpecVert 
    lusv = pusv + kMax
    lastUsedSpecVert = lusv
    IF (getGrid) THEN
       lugf = usedGridFields+1
       usedGridFields = lugf
       pugv = lastUsedGridVert
       lugv = pugv + kMax
       lastUsedGridVert = lugv
       IF (getDelLam) THEN
          ludlf = usedDelLamFields + 1
          usedDelLamFields = ludlf
          pudlv = lastUsedDelLamVert 
          ludlv = pudlv + kMax
          lastUsedDelLamVert = ludlv
       END IF
    ELSE IF (getDelLam) THEN
       lugf = usedGridFields+1
       usedGridFields = lugf
       pugv = lastUsedGridVert
       lugv = pugv + kMax
       lastUsedGridVert = lugv
    ELSE
       WRITE(*,"(a, ' no Grid or DelLam field required')") h
       STOP h
    END IF
!$OMP END CRITICAL(ControlDeposit)

    ! deposit spectral field

    IF (lusf <= nSpecFields) THEN
       surfSpec(lusf) = .FALSE.
       prevSpec(lusf) = pusv
       NULLIFY(Spec1d(lusf)%p)
       Spec2d(lusf)%p => ArgSpec
    ELSE
       WRITE(*,"(a, ' too many spectral fields')") h
       WRITE(*,"(a, ' used=',i5,'; declared=',i5)") h, lusf, nSpecFields
       STOP h
    END IF

    IF (dumpLocal) THEN
       WRITE(*,"(a,' usedSpecFields, lastUsedSpecVert, surfSpec, prevSpec=', 2i5,l2,i5)") &
            h, lusf, lusv, surfSpec(lusf), prevSpec(lusf)
    END IF

    IF (getGrid) THEN

       ! ArgGrid Field is required; 
       ! Store ArgGrid Field info at first available position of the Grid array

       IF (lugf <= nSpecFields) THEN
          surfGrid(lugf) = .FALSE.
          prevGrid(lugf) = pugv
          NULLIFY(Grid2d(lugf)%p)
          Grid3d(lugf)%p => ArgGrid
          fieldForDelLam(lugf) = .FALSE.
          prevVertDelLamSource(lugf) = -1
       ELSE
          WRITE(*,"(a, ' too many grid fields')") h
          WRITE(*,"(a, ' used=',i5,'; declared=',i5)") h, lugf, nSpecFields
          STOP h
       END IF

       IF (dumpLocal) THEN
          WRITE(*,"(a,' usedGridFields, lastUsedGridVert, surfGrid, prevGrid=', 2i5,l2,i5)") &
               h, lugf, lugv, surfGrid(lugf), prevGrid(lugf)
          WRITE(*,"(a,' fieldForDelLam, prevVertDelLamSource=', l2,i5)") &
               h, fieldForDelLam(lugf), prevVertDelLamSource(lugf)
       END IF

       IF (getDelLam) THEN

          ! ArgGrid and ArgDelLam are required; 
          ! Store ArgDelLam info at first available position of the Grid array
          ! Mark field for DelLam

          willDelLam = .TRUE.
          IF (ludlf <= nGridFields) THEN
             surfGrid(ludlf) = .FALSE.
             prevGrid(ludlf) = pudlv
             NULLIFY(Grid2d(ludlf)%p)
             Grid3d(ludlf)%p => ArgDelLam
             fieldForDelLam(ludlf) = .TRUE.
             prevVertDelLamSource(ludlf) = prevGrid(lugf)
          ELSE
             WRITE(*,"(a, ' too many grid fields including DelLam')") h
             WRITE(*,"(a, ' used=',i5,'; declared=',i5)") h, ludlf, nGridFields
             STOP h
          END IF

          IF (dumpLocal) THEN
             WRITE(*,"(a,' usedDelLamFields, lastUsedDelLamVert, surfGrid, prevGrid=', 2i5,l2,i5)") &
                  h, ludlf, ludlv, surfGrid(ludlf), prevGrid(ludlf)
             WRITE(*,"(a,' fieldForDelLam, prevVertDelLamSource=', l2,i5)") &
                  h, fieldForDelLam(ludlf), prevVertDelLamSource(ludlf)
          END IF
       END IF

    ELSE IF (getDelLam) THEN

       ! ArgDelLam Field is required; Grid is not
       ! Store ArgDelLam info at first available position of the Grid array
       ! Mark field for DelLam

       willDelLam = .TRUE.
       IF (lugf <= nSpecFields) THEN
          surfGrid(lugf) = .FALSE.
          prevGrid(lugf) = pugv
          NULLIFY(Grid2d(lugf)%p)
          Grid3d(lugf)%p => ArgDelLam
          fieldForDelLam(lugf) = .TRUE.
          prevVertDelLamSource(lugf) = prevGrid(lugf)
       ELSE
          WRITE(*,"(a, ' too many grid fields')") h
          WRITE(*,"(a, ' used=',i5,'; declared=',i5)") h, lugf, nSpecFields
          STOP h
       END IF

       IF (dumpLocal) THEN
          WRITE(*,"(a,' usedGridFields, lastUsedGridVert, surfGrid, prevGrid=', 2i5,l2,i5)") &
               h, lugf, lugv, surfGrid(lugf), prevGrid(lugf)
          WRITE(*,"(a,' fieldForDelLam, prevVertDelLamSource=', l2,i5)") &
               h, fieldForDelLam(lugf), prevVertDelLamSource(lugf)
       END IF
    END IF
  END SUBROUTINE Deposit2D




  SUBROUTINE DepDLG1D (ArgSpec, ArgGrid, ArgDelLam)
    REAL, TARGET, INTENT(IN) :: ArgSpec(:)
    REAL, TARGET, INTENT(IN) :: ArgGrid(:,:)
    REAL, TARGET, INTENT(IN) :: ArgDelLam(:,:)
    CALL Deposit1D (ArgSpec, ArgGrid, ArgDelLam)
  END SUBROUTINE DepDLG1D





  SUBROUTINE DepDLG2D (ArgSpec, ArgGrid, ArgDelLam)
    REAL, TARGET, INTENT(IN) :: ArgSpec(:,:)
    REAL, TARGET, INTENT(IN) :: ArgGrid(:,:,:)
    REAL, TARGET, INTENT(IN) :: ArgDelLam(:,:,:)
    CALL Deposit2D (ArgSpec, ArgGrid, ArgDelLam)
  END SUBROUTINE DepDLG2D





  SUBROUTINE DepDL1D (ArgSpec, ArgDelLam)
    REAL, TARGET, INTENT(IN) :: ArgSpec(:)
    REAL, TARGET, INTENT(IN) :: ArgDelLam(:,:)
    CALL Deposit1D (ArgSpec, ArgDelLam=ArgDelLam)
  END SUBROUTINE DepDL1D





  SUBROUTINE DepDL2D (ArgSpec, ArgDelLam)
    REAL, TARGET, INTENT(IN) :: ArgSpec(:,:)
    REAL, TARGET, INTENT(IN) :: ArgDelLam(:,:,:)
    CALL Deposit2D (ArgSpec, ArgDelLam=ArgDelLam)
  END SUBROUTINE DepDL2D





  SUBROUTINE DoSpecToGrid()
    CHARACTER(LEN=*), PARAMETER :: h="**(DoSpecToGrid)**"
    INTEGER :: mnFirst
    INTEGER :: mnLast
    INTEGER :: mnExtFirst
    INTEGER :: mnExtLast
    INTEGER :: mFirst
    INTEGER :: mLast
    INTEGER :: jbFirst
    INTEGER :: jbLast
    INTEGER :: FFTfirst 
    INTEGER :: FFTLast
    INTEGER :: FFTSize
    !$ INTEGER, EXTERNAL :: OMP_GET_THREAD_NUM


    ! all fields were deposited?

    IF (usedSpecFields /= nSpecFields) THEN
       WRITE(*,"(a, ' not all spectral fields were deposited')") h
       STOP h
    ELSE IF (usedGridFields /= nSpecFields) THEN
       WRITE(*,"(a, ' not all gauss fields were deposited')") h
       STOP h
    ELSE IF (usedDelLamFields /= nGridFields) THEN
       WRITE(*,"(a, ' not all gauss and dellam fields were deposited')") h
       STOP h
    END IF

    ! start OMP parallelism

    CALL ThreadDecomp(1, mnMax, mnFirst, mnLast, "DoSpecToGrid")
    CALL ThreadDecomp(1, mnExtMax, mnExtFirst, mnExtLast, "DoSpecToGrid")
    CALL ThreadDecomp(1, mMax, mFirst, mLast, "DoSpecToGrid")
    CALL ThreadDecomp(1, nVertGrid*jMax, FFTfirst , FFTLast, "DoSpecToGrid")
    CALL ThreadDecomp(1, jbMax, jbFirst, jbLast, "DoSpecToGrid")

    ! ingest spectral fields

    CALL DepositSpec(mnFirst, mnLast, mnExtFirst, mnExtLast, mFirst, mLast)
    !$OMP BARRIER

    ! Fourier from Spectral

    CALL SpecToFour()
    !$OMP BARRIER

    ! DelLam where required

    IF (willDelLam) THEN
       CALL DelLam(mFirst, mLast)
       !$OMP BARRIER
    END IF

    ! FFT Fourier to Grid

    FFTSize  = FFTLast - FFTFirst + 1
    CALL InvFFTTrans (Four(FFTfirst ,1), dvdlj, dip1, iMaxPerJ(1), FFTSize, &
         Trigs, nTrigs, Factors, nFactors)
    !$OMP BARRIER

    ! Withdraw Grid Fields

    CALL WithdrawGrid(jbFirst, jbLast)
  END SUBROUTINE DoSpecToGrid






  SUBROUTINE DepositSpec(mnFirst, mnLast, mnExtFirst, mnExtLast, mFirst, mLast)
    INTEGER, INTENT(IN) :: mnFirst
    INTEGER, INTENT(IN) :: mnLast
    INTEGER, INTENT(IN) :: mnExtFirst
    INTEGER, INTENT(IN) :: mnExtLast
    INTEGER, INTENT(IN) :: mFirst
    INTEGER, INTENT(IN) :: mLast
    INTEGER :: is
    INTEGER :: imn
    INTEGER :: iv
    INTEGER :: lastv
    REAL, POINTER :: s1(:)
    REAL, POINTER :: s2(:,:)

    DO is = 1, nSpecFields

       IF (surfSpec(is)) THEN
          s1 => Spec1d(is)%p
          lastv = prevSpec(is) + 1
          IF (SIZE(s1,1) == 2*myMNExtMax) THEN
!CDIR NODEP
             DO imn = mnExtFirst, mnExtLast
                Spec(lmnExtMap(imn), lastv)           = s1(2*imn-1)
                Spec(lmnExtMap(imn), lastv+nVertSpec) = s1(2*imn  )
             END DO
          ELSE 
!CDIR NODEP
             DO imn = mnFirst, mnLast
                Spec(lmnMap(imn), lastv)           = s1(2*imn-1)
                Spec(lmnMap(imn), lastv+nVertSpec) = s1(2*imn  )
             END DO
!CDIR NODEP
             DO imn = mFirst, mLast
                Spec(lmnZero(imn), lastv)           = 0.0
                Spec(lmnZero(imn), lastv+nVertSpec) = 0.0
             END DO
          END IF

       ELSE
          s2 => Spec2d(is)%p
          lastv = prevSpec(is)
          IF (SIZE(s2,1) == 2*myMNExtMax) THEN
             DO iv = 1, kMax
!CDIR NODEP
                DO imn = mnExtFirst, mnExtlast
                   Spec(lmnExtMap(imn), iv+lastv)           = s2(2*imn-1, iv)
                   Spec(lmnExtMap(imn), iv+lastv+nVertSpec) = s2(2*imn  , iv)
                END DO
             END DO
          ELSE 
             DO iv = 1, kMax
!CDIR NODEP
                DO imn = mnFirst, mnLast
                   Spec(lmnMap(imn), iv+lastv)           = s2(2*imn-1, iv)
                   Spec(lmnMap(imn), iv+lastv+nVertSpec) = s2(2*imn  , iv)
                END DO
             END DO
             DO iv = 1, kMax
!CDIR NODEP
                DO imn = mFirst, mLast
                   Spec(lmnZero(imn), iv+lastv)           = 0.0
                   Spec(lmnZero(imn), iv+lastv+nVertSpec) = 0.0
                END DO
             END DO
          END IF
       END IF
    END DO

    DO imn = myMNExtMax+1, dlmn
       Spec(imn,:) = 0.0
    END DO
    DO iv = 2*nVertSpec+1, dv
       Spec(:,iv) = 0.0
    END DO
  END SUBROUTINE DepositSpec





  SUBROUTINE SpecToFour()

    INTEGER :: lm
    INTEGER :: m
    INTEGER :: i
    INTEGER :: j
    INTEGER :: v
    INTEGER :: nullFourRowFirst
    INTEGER :: nullFourRowLast
    INTEGER :: nullFourColFirst
    INTEGER :: nullFourColLast
    REAL :: FoEv(dv, djh)
    REAL :: FoOd(dv, djh)


    !$OMP SINGLE
    mGlob = 0
    !$OMP END SINGLE
    DO
       !$OMP CRITICAL(lmcrit)
       mGlob = mGlob + 1
       lm = mGlob
       !$OMP END CRITICAL(lmcrit)
       IF (lm > myMMax) EXIT
    
       m = lm2m(lm)

       ! Spectral to Fourier Even/Odd

       FoEv = 0.0 
       CALL mmt (LS2F(jMinPerM(m),FirstNEven(lm)), djh, &
            Spec(FirstNEven(lm),1), dlmn, &
            FoEv(1,jMinPerM(m)), dv, djh, &
            jMaxHalf-jMinPerM(m)+1, 2*nVertSpec, nEven(lm))
       FoOd = 0.0
       CALL mmt (LS2F(jMinPerM(m),FirstNOdd(lm)), djh, &
            Spec(FirstNOdd(lm),1), dlmn, &
            FoOd(1,jMinPerM(m)), dv, djh, &
            jMaxHalf-jMinPerM(m)+1, 2*nVertSpec, nOdd(lm))

       ! Fourier Even/Odd to Full Fourier

       DO j = 1, jMaxHalf
!CDIR NODEP
          DO v = 1, nVertSpec
             Four((j   -1)*nVertGrid + v, 2*m-1) = FoEv(v,           j) + FoOd(v,           j)
             Four((j   -1)*nVertGrid + v, 2*m  ) = FoEv(v+nVertSpec, j) + FoOd(v+nVertSpec, j)
             Four((jMax-j)*nVertGrid + v, 2*m-1) = FoEv(v,           j) - FoOd(v,           j)
             Four((jMax-j)*nVertGrid + v, 2*m  ) = FoEv(v+nVertSpec, j) - FoOd(v+nVertSpec, j)
          END DO
       END DO
    END DO

    ! Nullify remaining Fourier

    CALL ThreadDecomp(2*mMax+1, dip1, nullFourColFirst, nullFourColLast, "SpecToFour")
    DO j = nullFourColFirst, nullFourColLast
       DO i = 1, dvdlj
          Four(i,j) = 0.0
       END DO
    END DO
    CALL ThreadDecomp(1, dip1, nullFourRowFirst, nullFourRowLast, "SpecToFour")
    DO i = nVertGrid*myJMax+1, dvdlj
       DO j = nullFourRowFirst, nullFourRowLast
          Four(i,j) = 0.0
       END DO
    END DO
  END SUBROUTINE SpecToFour





  SUBROUTINE DelLam(mFirst, mLast)
    INTEGER, INTENT(IN) :: mFirst
    INTEGER, INTENT(IN) :: mLast

    INTEGER :: m
    INTEGER :: j
    INTEGER :: v
    REAL    :: consIm
    REAL    :: consRe
    REAL    :: auxRe
    INTEGER :: ig
    INTEGER :: vBaseFrom
    INTEGER :: vBaseTo

    DO m = mFirst, mLast
       consIm =  eriv*REAL(m-1)
       consRe = -consIm


       DO ig = 1, nGridFields
          IF (fieldForDelLam(ig)) THEN
             vBaseFrom = prevVertDelLamSource(ig)
             vBaseTo   = prevGrid(ig)

             IF (surfGrid(ig)) THEN
!CDIR NODEP
                DO j = 1, jMax
                   auxRe = consIm * Four((j-1)*nVertGrid + vBaseFrom + 1, 2*m-1)
                   Four((j-1)*nVertGrid + vBaseTo + 1, 2*m-1) = &
                        consRe * Four((j-1)*nVertGrid + vBaseFrom + 1, 2*m  )
                   Four((j-1)*nVertGrid + vBaseTo + 1, 2*m  ) = auxRe
                END DO
             ELSE
                DO v = 1, kMax
!CDIR NODEP
                   DO j = 1, jMax
                      auxRe = consIm * Four((j-1)*nVertGrid + vBaseFrom + v, 2*m-1)
                      Four((j-1)*nVertGrid + vBaseTo + v, 2*m-1) = &
                           consRe * Four((j-1)*nVertGrid + vBaseFrom + v, 2*m  )
                      Four((j-1)*nVertGrid + vBaseTo + v, 2*m  ) = auxRe
                   END DO
                END DO
             END IF
          END IF
       END DO
    END DO
  END SUBROUTINE DelLam





  SUBROUTINE WithdrawGrid(jbFirst, jbLast)
    INTEGER, INTENT(IN) :: jbFirst
    INTEGER, INTENT(IN) :: jbLast

    INTEGER :: ig
    INTEGER :: ljb
    INTEGER :: j
    INTEGER :: i
    INTEGER :: i0
    INTEGER :: v0
    INTEGER :: v
    REAL, POINTER :: g2(:,:)
    REAL, POINTER :: g3(:,:,:)

    DO ig = 1, nGridFields
       IF (surfGrid(ig)) THEN
          g2 => Grid2d(ig)%p

!CDIR NODEP
          DO ljb = jbFirst, jbLast
             DO j = jFirstLJB(ljb), jLastLJB(ljb)
                i0 = ibPerIJ(1,j) - 1
                v0 = previousJ(j)*nVertGrid + prevGrid(ig)
                DO i = 1, iMaxPerJ(j)
                   g2(i+i0, ljb) = Four(v0+1, i)
                END DO
             END DO
          END DO
       ELSE

          g3 => Grid3d(ig)%p
!CDIR NODEP
          DO ljb = jbFirst, jbLast
             DO j = jFirstLJB(ljb), jLastLJB(ljb)
                i0 = ibPerIJ(1,j) - 1
                v0 = previousJ(j)*nVertGrid + prevGrid(ig)
                DO v = 1, kMax
                   DO i = 1, iMaxPerJ(j)
                      g3(i+i0, v, ljb)= Four(v+v0, i)
                   END DO
                END DO
             END DO
          END DO
       END IF
    END DO

  END SUBROUTINE WithdrawGrid





  SUBROUTINE CreateGridToSpec(nFull, nSurf)
    INTEGER, INTENT(IN) :: nFull
    INTEGER, INTENT(IN) :: nSurf
    CHARACTER(LEN=*), PARAMETER :: h="**(CreateGridToSpec)**"

    nSpecFields = nFull + nSurf
    usedSpecFields = 0
    lastUsedSpecVert = 0
    ALLOCATE(surfSpec(nSpecFields))
    ALLOCATE(prevSpec(nSpecFields))
    ALLOCATE(Spec1d(nSpecFields))
    ALLOCATE(Spec2d(nSpecFields))
    nVertSpec = nFull*kMax + nSurf

    nGridFields = nFull + nSurf
    usedGridFields = 0
    lastUsedGridVert = 0
    ALLOCATE(surfGrid(nGridFields))
    ALLOCATE(prevGrid(nGridFields))
    ALLOCATE(Grid2d(nGridFields))
    ALLOCATE(Grid3d(nGridFields))
    ALLOCATE(fieldForDelLam(nGridFields))
    nVertGrid = nFull*kMax + nSurf

    willDelLam = .FALSE.
    usedDelLamFields = nSpecFields
    lastUsedDelLamVert = nVertSpec
    ALLOCATE(prevVertDelLamSource(nGridFields))

    dv = NoBankConflict(2*nVertSpec)
    dvjh = NoBankConflict(nVertSpec*jMaxHalf)
    dvdlj = NoBankConflict(nVertGrid*myJMax)

    ALLOCATE (Spec(dlmn, dv))
    ALLOCATE (Four(dvdlj, dip1))


    IF (dumpLocal) THEN
       WRITE(*,"(a,' Spec: n, used, lastVert, nVert=',4i5)") &
            h, nSpecFields, usedSpecFields, lastUsedSpecVert, nVertSpec
       WRITE(*,"(a,' Grid: n, used, lastVert, nVert=',4i5)") &
            h, nGridFields, usedGridFields, lastUsedGridVert, nVertGrid
    END IF

  END SUBROUTINE CreateGridToSpec




  SUBROUTINE DoGridToSpec()
    INTEGER :: mnFirst
    INTEGER :: mnLast
    INTEGER :: mnExtFirst
    INTEGER :: mnExtLast
    INTEGER :: jbFirst
    INTEGER :: jbLast
    INTEGER :: FFTfirst 
    INTEGER :: FFTLast
    INTEGER :: FFTSize
    CHARACTER(LEN=*), PARAMETER :: h="**(DoGridToSpec)**"
    !$ INTEGER, EXTERNAL :: OMP_GET_THREAD_NUM

    ! all fields were deposited?

    IF (usedSpecFields /= nSpecFields) THEN
       WRITE(*,"(a, ' not all spectral fields were deposited')") h
       STOP h
    ELSE IF (usedGridFields /= nSpecFields) THEN
       WRITE(*,"(a, ' not all gauss fields were deposited')") h
       STOP h
    ELSE IF (usedDelLamFields /= nGridFields) THEN
       WRITE(*,"(a, ' not all gauss and dellam fields were deposited')") h
       STOP h
    END IF

    ! start OMP parallelism

    CALL ThreadDecomp(1, mnMax, mnFirst, mnLast, "DoSpecToGrid")
    CALL ThreadDecomp(1, mnExtMax, mnExtFirst, mnExtLast, "DoSpecToGrid")
    CALL ThreadDecomp(1, nVertGrid*jMax, FFTfirst , FFTLast, "DoSpecToGrid")
    CALL ThreadDecomp(1, jbMax, jbFirst, jbLast, "DoSpecToGrid")


    ! deposit all grid fields

    CALL DepositGrid(jbFirst, jbLast)
    !$OMP BARRIER

    ! fft

    FFTSize  = FFTLast - FFTfirst  + 1
    CALL DirFFTTrans (Four(FFTfirst ,1), dvdlj, dip1, iMaxPerJ(1), FFTSize, &
         Trigs, nTrigs, Factors, nFactors)
    !$OMP BARRIER

    ! Fourier to Spectral

    CALL FourToSpec()
    !$OMP BARRIER

    ! retrieve Spectral fields

    CALL WithdrawSpectral(mnFirst, mnLast, mnExtFirst, mnExtLast)
  END SUBROUTINE DoGridToSpec





  SUBROUTINE DepositGrid(jbFirst, jbLast)
    INTEGER, INTENT(IN) :: jbFirst
    INTEGER, INTENT(IN) :: jbLast

    INTEGER :: ig
    INTEGER :: ljb
    INTEGER :: j
    INTEGER :: i
    INTEGER :: i0
    INTEGER :: v0
    INTEGER :: v
    REAL, POINTER :: g2(:,:)
    REAL, POINTER :: g3(:,:,:)

    DO ig = 1, nGridFields
       IF (surfGrid(ig)) THEN
          g2 => Grid2d(ig)%p

          DO ljb = jbFirst, jbLast
             DO j = jFirstLJB(ljb), jLastLJB(ljb)
                i0 = ibPerIJ(1,j) - 1
                v0 = previousJ(j)*nVertGrid + prevGrid(ig)
                DO i = 1, iMaxPerJ(j)
                   Four(v0+1, i) = g2(i+i0, ljb)
                END DO
             END DO
          END DO

       ELSE
          g3 => Grid3d(ig)%p
          DO ljb = jbFirst, jbLast
             DO j = jFirstLJB(ljb), jLastLJB(ljb)
                i0 = ibPerIJ(1,j) - 1
                v0 = previousJ(j)*nVertGrid + prevGrid(ig)
                DO v = 1, kMax
                   DO i = 1, iMaxPerJ(j)
                      Four(v+v0, i) = g3(i+i0, v, ljb)
                   END DO
                END DO
             END DO
          END DO
       END IF
    END DO

    ! Nullify remaining Fourier

    DO i = 2*iMax+1, dip1
       Four(:,i) = 0.0
    END DO


    DO j = nVertGrid*myJMax+1, dvdlj
       Four(j,:) = 0.0
    END DO
  END SUBROUTINE DepositGrid




  SUBROUTINE FourToSpec()
    INTEGER :: lm
    INTEGER :: m
    INTEGER :: mn
    INTEGER :: j
    INTEGER :: v
    REAL :: FoEv(djh, dv)
    REAL :: FoOd(djh, dv)

    DO v = 1, dv
       DO j = jMax+1, djh
          FoEv(j,v) = 0.0
          FoOd(j,v) = 0.0
       END DO
    END DO
    DO v = 2*nVertSpec+1, dv
       DO j = 1, djh
          FoEv(j,v) = 0.0
          FoOd(j,v) = 0.0
       END DO
    END DO

    !$OMP SINGLE
    mGlob = 0
    !$OMP END SINGLE
    DO
       !$OMP CRITICAL(lmcrit)
       mGlob = mGlob + 1
       lm = mGlob
       !$OMP END CRITICAL(lmcrit)
       IF (lm > myMMax) EXIT
       m = lm2m(lm)
       DO v = 1, nVertSpec
!CDIR NODEP
          DO j = 1, jMaxHalf
             FoEv(j,v          ) = Four((j-1)*nVertGrid + v, 2*m-1) + Four((jMax-j)*nVertGrid + v, 2*m-1) 
             FoEv(j,v+nVertSpec) = Four((j-1)*nVertGrid + v, 2*m  ) + Four((jMax-j)*nVertGrid + v, 2*m  ) 
             FoOd(j,v          ) = Four((j-1)*nVertGrid + v, 2*m-1) - Four((jMax-j)*nVertGrid + v, 2*m-1) 
             FoOd(j,v+nVertSpec) = Four((j-1)*nVertGrid + v, 2*m  ) - Four((jMax-j)*nVertGrid + v, 2*m  ) 
          END DO
       END DO
       DO j = 1, dv
          DO mn = FirstNEven(lm), FirstNEven(lm) + nEven(lm) - 1
             Spec(mn,j) = 0.0
          END DO
       END DO
       CALL mmd (&
            LF2S(FirstNEven(lm),jMinPerM(m)), dlmn, &
            FoEv(jMinPerM(m),1), djh, &
            Spec(FirstNEven(lm),1), dlmn, dv, &
            nEven(lm), 2*nVertSpec, jMaxHalf-jMinPerM(m)+1)
       DO j = 1, dv
          DO mn = FirstNOdd(lm), FirstNOdd(lm) + nOdd(lm) - 1
             Spec(mn,j) = 0.0
          END DO
       END DO
       CALL mmd (&
            LF2S(FirstNOdd(lm),jMinPerM(m)), dlmn, &
            FoOd(jMinPerM(m),1), djh, &
            Spec(FirstNOdd(lm),1), dlmn, dv, &
            nOdd(lm), 2*nVertSpec, jMaxHalf-jMinPerM(m)+1)
    END DO
  END SUBROUTINE FourToSpec





  SUBROUTINE WithdrawSpectral(mnFirst, mnLast, mnExtFirst, mnExtLast)
    INTEGER, INTENT(IN) :: mnFirst
    INTEGER, INTENT(IN) :: mnLast
    INTEGER, INTENT(IN) :: mnExtFirst
    INTEGER, INTENT(IN) :: mnExtLast

    INTEGER :: is
    INTEGER :: imn
    INTEGER :: iv
    INTEGER :: lastv
    REAL, POINTER :: s1(:)
    REAL, POINTER :: s2(:,:)


    DO is = 1, nSpecFields

       IF (surfSpec(is)) THEN
          s1 => Spec1d(is)%p
          lastv = prevSpec(is) + 1
          IF (SIZE(s1,1) == 2*myMNExtMax) THEN
!CDIR NODEP
             DO imn = mnExtFirst, mnExtLast
                s1(2*imn-1) = Spec(lmnExtMap(imn), lastv)
                s1(2*imn  ) = Spec(lmnExtMap(imn), lastv+nVertSpec)
             END DO
          ELSE 
!CDIR NODEP
             DO imn = mnFirst, mnLast
                s1(2*imn-1) = Spec(lmnMap(imn), lastv)
                s1(2*imn  ) = Spec(lmnMap(imn), lastv+nVertSpec)
             END DO
          END IF
       ELSE
          s2 => Spec2d(is)%p
          lastv = prevSpec(is)
          IF (SIZE(s2,1) == 2*myMNExtMax) THEN
             DO iv = 1, kMax
!CDIR NODEP
                DO imn = mnExtFirst, mnExtlast
                   s2(2*imn-1, iv) = Spec(lmnExtMap(imn), iv+lastv)
                   s2(2*imn  , iv) = Spec(lmnExtMap(imn), iv+lastv+nVertSpec) 
                END DO
             END DO
          ELSE 
             DO iv = 1, kMax
!CDIR NODEP
                DO imn = mnFirst, mnLast
                   s2(2*imn-1, iv) = Spec(lmnMap(imn), iv+lastv)       
                   s2(2*imn  , iv) = Spec(lmnMap(imn), iv+lastv+nVertSpec) 
                END DO
             END DO
          END IF
       END IF
    END DO
  END SUBROUTINE WithdrawSpectral


  SUBROUTINE Destroy()

!TO!$OMP SINGLE
    DEALLOCATE(Spec)
    DEALLOCATE(Four)
    IF (ALLOCATED(surfSpec)) THEN
       DEALLOCATE(surfSpec)
    END IF
    IF (ALLOCATED(prevSpec)) THEN
       DEALLOCATE(prevSpec)
    END IF
    IF (ALLOCATED(Spec1d)) THEN
       DEALLOCATE(Spec1d)
    END IF
    IF (ALLOCATED(Spec2d)) THEN
       DEALLOCATE(Spec2d)
    END IF

    IF (ALLOCATED(surfGrid)) THEN
       DEALLOCATE(surfGrid)
    END IF
    IF (ALLOCATED(prevGrid)) THEN
       DEALLOCATE(prevGrid)
    END IF
    IF (ALLOCATED(Grid2d)) THEN
       DEALLOCATE(Grid2d)
    END IF
    IF (ALLOCATED(Grid3d)) THEN
       DEALLOCATE(Grid3d)
    END IF
    IF (ALLOCATED(fieldForDelLam)) THEN
       DEALLOCATE(fieldForDelLam)
    END IF
    IF (ALLOCATED(prevVertDelLamSource)) THEN
       DEALLOCATE(prevVertDelLamSource)
    END IF
!TO!$OMP END SINGLE
  END SUBROUTINE Destroy






  SUBROUTINE mmd (A, lda, B, ldb, C, ldc, tdc, ni, nj, nk)
    INTEGER, INTENT(IN ) :: ni , nj,  nk
    INTEGER, INTENT(IN ) :: lda, ldb, ldc, tdc
    REAL,    INTENT(IN ) :: A(lda, nk)
    REAL,    INTENT(IN ) :: B(ldb, nj)
    REAL,    INTENT(INOUT) :: C(ldc, tdc)
    INTEGER i, j, k
    DO i = 1, ni
       DO j = 1, nj
          DO k = 1, nk
             C(i,j)=C(i,j)+A(i,k)*B(k,j)
          END DO
       END DO
    END DO
  END SUBROUTINE mmd




  SUBROUTINE mmt (A, lda, B, ldb, C, ldc, tdc, ni, nj, nk)
    INTEGER, INTENT(IN ) :: ni , nj,  nk
    INTEGER, INTENT(IN ) :: lda, ldb, ldc, tdc
    REAL,    INTENT(IN ) :: A(lda, nk)
    REAL,    INTENT(IN ) :: B(ldb, nj)
    REAL,    INTENT(INOUT) :: C(ldc, tdc)
    INTEGER i, j, k
    CHARACTER(LEN=*), PARAMETER :: h="**(mmt)**"
    DO i = 1, ni
       DO j = 1, nj
          DO k = 1, nk
             C(j,i)=C(j,i)+A(i,k)*B(k,j)
          END DO
       END DO
    END DO
  END SUBROUTINE mmt




  !InvFFT: Computes inverse FFT of 'lot' sequences of 'n+1' input real data
  !        as rows of 'fin', dimensioned 'fin(ldin,tdin)'. Input data is
  !        kept unchanged. Input values 'fin(n+2:ldin,:)' and 
  !        'fin(:,lot+1:tdin)' are not visited. 
  !        Outputs 'lot' sequences of 'n' real data
  !        as rows of 'fout', dimensioned 'fout(ldout,tdout)'. Output
  !        values 'fout(n+1:ldout,:)' and 'fout(:,lot+1:tdout)' are set to 0.



  SUBROUTINE InvFFTTrans (fInOut, ldInOut, tdInOut, n, lot, Trigs, nTrigs, Factors, nFactors)
    INTEGER, INTENT(IN ) :: ldInOut, tdInOut
    REAL :: fInOut (ldInOut ,tdInOut)
    INTEGER, INTENT(IN ) :: n
    INTEGER, INTENT(IN ) :: lot
    INTEGER, INTENT(IN ) :: nTrigs
    REAL,    INTENT(IN ) :: Trigs(nTrigs)
    INTEGER, INTENT(IN ) :: nFactors
    INTEGER, INTENT(IN ) :: Factors(nFactors)

    INTEGER :: nh
    INTEGER :: nfax
    INTEGER :: la
    INTEGER :: k
    LOGICAL :: ab2cd
    CHARACTER(LEN=*), PARAMETER :: h="**(InvFFTTrans)**"
    REAL :: a(lot,n/2)
    REAL :: b(lot,n/2)
    REAL :: c(lot,n/2)
    REAL :: d(lot,n/2)


    nfax=Factors(1)
    nh=n/2

    CALL SplitFourTrans (fInOut, a, b, ldInOut, tdInOut, n, nh, lot, Trigs, nTrigs)

    la=1
    ab2cd=.TRUE.
    DO k=1,nfax
       IF (ab2cd) THEN
          CALL OnePass (a, b, c, d, lot, nh, Factors(k+1), la, Trigs, nTrigs)
          ab2cd=.FALSE.
       ELSE
          CALL OnePass (c, d, a, b, lot, nh, Factors(k+1), la, Trigs, nTrigs)
          ab2cd=.TRUE.
       END IF
       la=la*Factors(k+1)
    END DO

    IF (ab2cd) THEN
       CALL JoinGridTrans (a, b, fInOut, ldInOut, tdInOut, nh, lot)
    ELSE
       CALL JoinGridTrans (c, d, fInOut, ldInOut, tdInOut, nh, lot)
    END IF
  END SUBROUTINE InvFFTTrans


  ! Split Fourier Fields


  SUBROUTINE SplitFourTrans (fin, a, b, ldin, tdin, n, nh, lot, Trigs, nTrigs)
    INTEGER, INTENT(IN ) :: ldin
    INTEGER, INTENT(IN ) :: tdin
    INTEGER, INTENT(IN ) :: n
    INTEGER, INTENT(IN ) :: nh
    INTEGER, INTENT(IN ) :: lot
    REAL,    INTENT(IN ) :: fin(ldin, tdin)
    REAL,    INTENT(OUT) :: a  (lot , nh)
    REAL,    INTENT(OUT) :: b  (lot , nh)
    INTEGER, INTENT(IN ) :: nTrigs
    REAL,    INTENT(IN ) :: Trigs(nTrigs)

    INTEGER :: i, j
    REAL    :: c, s

!CDIR NODEP
    DO i = 1, lot
       a(i,1)=fin(i,1)+fin(i,n+1)
       b(i,1)=fin(i,1)-fin(i,n+1)
    END DO

    DO j = 2, (nh+1)/2
       c=Trigs(n+2*j-1)
       s=Trigs(n+2*j  )
!CDIR NODEP
       DO i = 1, lot
          a(i,j     )=   (fin(i,2*j-1)+fin(i,n+3-2*j)) &
               -      (s*(fin(i,2*j-1)-fin(i,n+3-2*j)) &
               +       c*(fin(i,2*j  )+fin(i,n+4-2*j)))
          a(i,nh+2-j)=   (fin(i,2*j-1)+fin(i,n+3-2*j)) &
               +      (s*(fin(i,2*j-1)-fin(i,n+3-2*j)) &
               +       c*(fin(i,2*j  )+fin(i,n+4-2*j)))
          b(i,j     )=(c*(fin(i,2*j-1)-fin(i,n+3-2*j)) &
               -       s*(fin(i,2*j  )+fin(i,n+4-2*j)))&
               +         (fin(i,2*j  )-fin(i,n+4-2*j))
          b(i,nh+2-j)=(c*(fin(i,2*j-1)-fin(i,n+3-2*j)) &
               -       s*(fin(i,2*j  )+fin(i,n+4-2*j)))&
               -         (fin(i,2*j  )-fin(i,n+4-2*j))
       END DO
    END DO
    IF ( (nh>=2) .AND. (MOD(nh,2)==0) ) THEN
!CDIR NODEP
       DO i = 1, lot
          a(i,nh/2+1)= 2.0*fin(i,nh+1)
          b(i,nh/2+1)=-2.0*fin(i,nh+2)
       END DO
    END IF
  END SUBROUTINE SplitFourTrans




  !JoinGrid: Merge fundamental algorithm complex output into 
  !          sequences of real numbers



  SUBROUTINE JoinGridTrans (a, b, fout, ldout, tdout, nh, lot)
    INTEGER, INTENT(IN ) :: ldout, tdout
    INTEGER, INTENT(IN ) :: nh
    INTEGER, INTENT(IN ) :: lot
    REAL,    INTENT(OUT) :: fout(ldout,tdout)
    REAL,    INTENT(IN ) :: a   (lot  ,nh)
    REAL,    INTENT(IN ) :: b   (lot  ,nh)

    INTEGER :: i, j

    DO j = 1, nh
       DO i = 1, lot
          fout(i,2*j-1)=a(i,j)
          fout(i,2*j  )=b(i,j)
       END DO
    END DO

!    fout(:,2*nh+1:tdout)=0.0
!    fout(lot+1:ldout, :)=0.0
  END SUBROUTINE JoinGridTrans


  !OnePass: single pass of fundamental algorithm



  SUBROUTINE OnePass (a, b, c, d, lot, nh, ifac, la, Trigs, nTrigs)
    INTEGER, INTENT(IN ) :: lot
    INTEGER, INTENT(IN ) :: nh         ! = PROD(factor(1:K))
    REAL,    INTENT(IN ) :: a(lot,nh)
    REAL,    INTENT(IN ) :: b(lot,nh)
    REAL,    INTENT(OUT) :: c(lot,nh)
    REAL,    INTENT(OUT) :: d(lot,nh)
    INTEGER, INTENT(IN ) :: ifac       ! = factor(k)
    INTEGER, INTENT(IN ) :: la         ! = PROD(factor(1:k-1))
    INTEGER, INTENT(IN ) :: nTrigs
    REAL,    INTENT(IN ) :: Trigs(nTrigs)

    INTEGER :: m
    INTEGER :: jump
    INTEGER :: i, j, k
    INTEGER :: ia, ja
    INTEGER :: ib, jb, kb
    INTEGER :: ic, jc, kc
    INTEGER :: id, jd, kd
    INTEGER :: ie, je, ke
    REAL    :: c1, s1
    REAL    :: c2, s2
    REAL    :: c3, s3
    REAL    :: c4, s4
    REAL    :: wka, wkb
    REAL    :: wksina, wksinb
    REAL    :: wkaacp, wkbacp
    REAL    :: wkaacm, wkbacm

    m=nh/ifac
    jump=(ifac-1)*la

    ia=  0
    ib=  m
    ic=2*m
    id=3*m
    ie=4*m

    ja=  0
    jb=  la
    jc=2*la
    jd=3*la
    je=4*la

    IF (ifac == 2) THEN
       DO j = 1, la
!CDIR NODEP
          DO i = 1, lot
             c(i,j+ja)=a(i,j+ia)+a(i,j+ib)
             c(i,j+jb)=a(i,j+ia)-a(i,j+ib)
             d(i,j+ja)=b(i,j+ia)+b(i,j+ib)
             d(i,j+jb)=b(i,j+ia)-b(i,j+ib)
          END DO
       END DO
       DO k = la, m-1, la
          kb=k+k
          c1=Trigs(kb+1)
          s1=Trigs(kb+2)
          ja=ja+jump
          jb=jb+jump
          DO j = k+1, k+la
!CDIR NODEP
             DO i = 1, lot
                wka      =a(i,j+ia)-a(i,j+ib)
                c(i,j+ja)=a(i,j+ia)+a(i,j+ib)
                wkb      =b(i,j+ia)-b(i,j+ib)
                d(i,j+ja)=b(i,j+ia)+b(i,j+ib)
                c(i,j+jb)=c1*wka-s1*wkb
                d(i,j+jb)=s1*wka+c1*wkb
             END DO
          END DO
       END DO
    ELSEIF (ifac == 3) THEN
       DO j = 1, la
!CDIR  NODEP
          DO i = 1, lot
             wka      =       a(i,j+ib)+a(i,j+ic)
             wksina   =sin60*(a(i,j+ib)-a(i,j+ic))
             wkb      =       b(i,j+ib)+b(i,j+ic)
             wksinb   =sin60*(b(i,j+ib)-b(i,j+ic))
             c(i,j+ja)=       a(i,j+ia)+wka
             c(i,j+jb)=      (a(i,j+ia)-0.5*wka)-wksinb
             c(i,j+jc)=      (a(i,j+ia)-0.5*wka)+wksinb
             d(i,j+ja)=       b(i,j+ia)+wkb
             d(i,j+jb)=      (b(i,j+ia)-0.5*wkb)+wksina
             d(i,j+jc)=      (b(i,j+ia)-0.5*wkb)-wksina
          END DO
       END DO
       DO k = la, m-1, la
          kb=k+k
          kc=kb+kb
          c1=Trigs(kb+1)
          s1=Trigs(kb+2)
          c2=Trigs(kc+1)
          s2=Trigs(kc+2)
          ja=ja+jump
          jb=jb+jump
          jc=jc+jump
          DO j = k+1, k+la
!CDIR NODEP
             DO i = 1, lot
                wka      =       a(i,j+ib)+a(i,j+ic)
                wksina   =sin60*(a(i,j+ib)-a(i,j+ic))
                wkb      =       b(i,j+ib)+b(i,j+ic)
                wksinb   =sin60*(b(i,j+ib)-b(i,j+ic))
                c(i,j+ja)=       a(i,j+ia)+wka
                d(i,j+ja)=       b(i,j+ia)+wkb
                c(i,j+jb)=c1*  ((a(i,j+ia)-0.5*wka)-wksinb) &
                     -    s1*  ((b(i,j+ia)-0.5*wkb)+wksina)
                d(i,j+jb)=s1*  ((a(i,j+ia)-0.5*wka)-wksinb) &
                     +    c1*  ((b(i,j+ia)-0.5*wkb)+wksina)
                c(i,j+jc)=c2*  ((a(i,j+ia)-0.5*wka)+wksinb) &
                     -    s2*  ((b(i,j+ia)-0.5*wkb)-wksina)
                d(i,j+jc)=s2*  ((a(i,j+ia)-0.5*wka)+wksinb) &
                     +    c2*  ((b(i,j+ia)-0.5*wkb)-wksina)
             END DO
          END DO
       END DO
    ELSEIF (ifac == 4) THEN
       DO j = 1, la
!CDIR NODEP
          DO i = 1, lot
             wkaacp   =        a(i,j+ia)+a(i,j+ic)
             wkaacm   =        a(i,j+ia)-a(i,j+ic)
             wkbacp   =        b(i,j+ia)+b(i,j+ic)
             wkbacm   =        b(i,j+ia)-b(i,j+ic)
             c(i,j+ja)=wkaacp+(a(i,j+ib)+a(i,j+id))
             c(i,j+jc)=wkaacp-(a(i,j+ib)+a(i,j+id))
             d(i,j+jb)=wkbacm+(a(i,j+ib)-a(i,j+id))
             d(i,j+jd)=wkbacm-(a(i,j+ib)-a(i,j+id))
             d(i,j+ja)=wkbacp+(b(i,j+ib)+b(i,j+id))
             d(i,j+jc)=wkbacp-(b(i,j+ib)+b(i,j+id))
             c(i,j+jb)=wkaacm-(b(i,j+ib)-b(i,j+id))
             c(i,j+jd)=wkaacm+(b(i,j+ib)-b(i,j+id))
          END DO
       END DO
       DO k = la, m-1, la
          kb=k+k
          kc=kb+kb
          kd=kc+kb
          c1=Trigs(kb+1)
          s1=Trigs(kb+2)
          c2=Trigs(kc+1)
          s2=Trigs(kc+2)
          c3=Trigs(kd+1)
          s3=Trigs(kd+2)
          ja=ja+jump
          jb=jb+jump
          jc=jc+jump
          jd=jd+jump
          DO j = k+1, k+la
!CDIR NODEP
             DO i = 1, lot
                wkaacp   =            a(i,j+ia)+a(i,j+ic)
                wkbacp   =            b(i,j+ia)+b(i,j+ic)
                wkaacm   =            a(i,j+ia)-a(i,j+ic)
                wkbacm   =            b(i,j+ia)-b(i,j+ic)
                c(i,j+ja)=    wkaacp+(a(i,j+ib)+a(i,j+id))
                d(i,j+ja)=    wkbacp+(b(i,j+ib)+b(i,j+id))
                c(i,j+jc)=c2*(wkaacp-(a(i,j+ib)+a(i,j+id))) &
                     -    s2*(wkbacp-(b(i,j+ib)+b(i,j+id))) 
                d(i,j+jc)=s2*(wkaacp-(a(i,j+ib)+a(i,j+id))) &
                     +    c2*(wkbacp-(b(i,j+ib)+b(i,j+id)))
                c(i,j+jb)=c1*(wkaacm-(b(i,j+ib)-b(i,j+id))) &
                     -    s1*(wkbacm+(a(i,j+ib)-a(i,j+id)))
                d(i,j+jb)=s1*(wkaacm-(b(i,j+ib)-b(i,j+id))) &
                     +    c1*(wkbacm+(a(i,j+ib)-a(i,j+id)))
                c(i,j+jd)=c3*(wkaacm+(b(i,j+ib)-b(i,j+id))) &
                     -    s3*(wkbacm-(a(i,j+ib)-a(i,j+id)))
                d(i,j+jd)=s3*(wkaacm+(b(i,j+ib)-b(i,j+id))) &
                     +    c3*(wkbacm-(a(i,j+ib)-a(i,j+id)))
             END DO
          END DO
       END DO
    ELSEIF (ifac == 5) THEN
       DO j = 1, la
!CDIR NODEP
          DO i = 1, lot
             c(i,j+ja)=a(i,j+ia)+(a(i,j+ib)+a(i,j+ie))+(a(i,j+ic)+a(i,j+id))
             d(i,j+ja)=    b(i,j+ia)&
                  +       (b(i,j+ib)+b(i,j+ie))&
                  +       (b(i,j+ic)+b(i,j+id))
             c(i,j+jb)=   (a(i,j+ia)&
                  + cos72*(a(i,j+ib)+a(i,j+ie))&
                  - cos36*(a(i,j+ic)+a(i,j+id)))&
                  -(sin72*(b(i,j+ib)-b(i,j+ie))&
                  + sin36*(b(i,j+ic)-b(i,j+id)))
             c(i,j+je)=   (a(i,j+ia)&
                  + cos72*(a(i,j+ib)+a(i,j+ie))&
                  - cos36*(a(i,j+ic)+a(i,j+id)))&
                  +(sin72*(b(i,j+ib)-b(i,j+ie))&
                  + sin36*(b(i,j+ic)-b(i,j+id)))
             d(i,j+jb)=   (b(i,j+ia)&
                  + cos72*(b(i,j+ib)+b(i,j+ie))&
                  - cos36*(b(i,j+ic)+b(i,j+id)))&
                  +(sin72*(a(i,j+ib)-a(i,j+ie))&
                  + sin36*(a(i,j+ic)-a(i,j+id)))
             d(i,j+je)=   (b(i,j+ia)&
                  + cos72*(b(i,j+ib)+b(i,j+ie))&
                  - cos36*(b(i,j+ic)+b(i,j+id)))&
                  -(sin72*(a(i,j+ib)-a(i,j+ie))&
                  + sin36*(a(i,j+ic)-a(i,j+id)))
             c(i,j+jc)=   (a(i,j+ia)&
                  - cos36*(a(i,j+ib)+a(i,j+ie))&
                  + cos72*(a(i,j+ic)+a(i,j+id)))&
                  -(sin36*(b(i,j+ib)-b(i,j+ie))&
                  - sin72*(b(i,j+ic)-b(i,j+id)))
             c(i,j+jd)=   (a(i,j+ia)&
                  - cos36*(a(i,j+ib)+a(i,j+ie))&
                  + cos72*(a(i,j+ic)+a(i,j+id)))&
                  +(sin36*(b(i,j+ib)-b(i,j+ie))&
                  - sin72*(b(i,j+ic)-b(i,j+id)))
             d(i,j+jc)=   (b(i,j+ia)&
                  - cos36*(b(i,j+ib)+b(i,j+ie))&
                  + cos72*(b(i,j+ic)+b(i,j+id)))&
                  +(sin36*(a(i,j+ib)-a(i,j+ie))&
                  - sin72*(a(i,j+ic)-a(i,j+id)))
             d(i,j+jd)=   (b(i,j+ia)&
                  - cos36*(b(i,j+ib)+b(i,j+ie))&
                  + cos72*(b(i,j+ic)+b(i,j+id)))&
                  -(sin36*(a(i,j+ib)-a(i,j+ie))&
                  - sin72*(a(i,j+ic)-a(i,j+id)))
          END DO
       END DO
       DO k = la, m-1, la
          kb=k+k
          kc=kb+kb
          kd=kc+kb
          ke=kd+kb
          c1=Trigs(kb+1)
          s1=Trigs(kb+2)
          c2=Trigs(kc+1)
          s2=Trigs(kc+2)
          c3=Trigs(kd+1)
          s3=Trigs(kd+2)
          c4=Trigs(ke+1)
          s4=Trigs(ke+2)
          ja=ja+jump
          jb=jb+jump
          jc=jc+jump
          jd=jd+jump
          je=je+jump
          DO j = k+1, k+la
!CDIR NODEP
             DO i = 1, lot
                c(i,j+ja)=     a(i,j+ia)&
                     +        (a(i,j+ib)+a(i,j+ie))&
                     +        (a(i,j+ic)+a(i,j+id))
                d(i,j+ja)=     b(i,j+ia)&
                     +        (b(i,j+ib)+b(i,j+ie))&
                     +        (b(i,j+ic)+b(i,j+id))
                c(i,j+jb)=c1*((a(i,j+ia)&
                     +  cos72*(a(i,j+ib)+a(i,j+ie))&
                     -  cos36*(a(i,j+ic)+a(i,j+id)))&
                     - (sin72*(b(i,j+ib)-b(i,j+ie))&
                     +  sin36*(b(i,j+ic)-b(i,j+id))))&
                     -    s1*((b(i,j+ia)&
                     +  cos72*(b(i,j+ib)+b(i,j+ie))&
                     -  cos36*(b(i,j+ic)+b(i,j+id)))&
                     + (sin72*(a(i,j+ib)-a(i,j+ie))&
                     +  sin36*(a(i,j+ic)-a(i,j+id))))
                d(i,j+jb)=s1*((a(i,j+ia)&
                     +  cos72*(a(i,j+ib)+a(i,j+ie))&
                     -  cos36*(a(i,j+ic)+a(i,j+id)))&
                     - (sin72*(b(i,j+ib)-b(i,j+ie))&
                     +  sin36*(b(i,j+ic)-b(i,j+id))))&
                     +    c1*((b(i,j+ia)&
                     +  cos72*(b(i,j+ib)+b(i,j+ie))&
                     -  cos36*(b(i,j+ic)+b(i,j+id)))&
                     + (sin72*(a(i,j+ib)-a(i,j+ie)) &
                     +  sin36*(a(i,j+ic)-a(i,j+id))))
                c(i,j+je)=c4*((a(i,j+ia)&
                     +  cos72*(a(i,j+ib)+a(i,j+ie))&
                     -  cos36*(a(i,j+ic)+a(i,j+id)))&
                     + (sin72*(b(i,j+ib)-b(i,j+ie)) &
                     +  sin36*(b(i,j+ic)-b(i,j+id)))) &
                     -    s4*((b(i,j+ia)&
                     +  cos72*(b(i,j+ib)+b(i,j+ie))&
                     -  cos36*(b(i,j+ic)+b(i,j+id)))&
                     - (sin72*(a(i,j+ib)-a(i,j+ie))&
                     +  sin36*(a(i,j+ic)-a(i,j+id))))
                d(i,j+je)=s4*((a(i,j+ia)&
                     +  cos72*(a(i,j+ib)+a(i,j+ie))&
                     -  cos36*(a(i,j+ic)+a(i,j+id)))&
                     + (sin72*(b(i,j+ib)-b(i,j+ie))&
                     +  sin36*(b(i,j+ic)-b(i,j+id))))&
                     +    c4*((b(i,j+ia)&
                     +  cos72*(b(i,j+ib)+b(i,j+ie))&
                     -  cos36*(b(i,j+ic)+b(i,j+id))) &
                     - (sin72*(a(i,j+ib)-a(i,j+ie))&
                     +  sin36*(a(i,j+ic)-a(i,j+id))))
                c(i,j+jc)=c2*((a(i,j+ia)&
                     -  cos36*(a(i,j+ib)+a(i,j+ie))&
                     +  cos72*(a(i,j+ic)+a(i,j+id)))&
                     - (sin36*(b(i,j+ib)-b(i,j+ie))&
                     -  sin72*(b(i,j+ic)-b(i,j+id))))&
                     -    s2*((b(i,j+ia)&
                     -  cos36*(b(i,j+ib)+b(i,j+ie))&
                     +  cos72*(b(i,j+ic)+b(i,j+id)))&
                     + (sin36*(a(i,j+ib)-a(i,j+ie))&
                     -  sin72*(a(i,j+ic)-a(i,j+id)))) 
                d(i,j+jc)=s2*((a(i,j+ia)&
                     -  cos36*(a(i,j+ib)+a(i,j+ie))&
                     +  cos72*(a(i,j+ic)+a(i,j+id)))&
                     - (sin36*(b(i,j+ib)-b(i,j+ie))&
                     -  sin72*(b(i,j+ic)-b(i,j+id))))&
                     +    c2*((b(i,j+ia)&
                     -  cos36*(b(i,j+ib)+b(i,j+ie))&
                     +  cos72*(b(i,j+ic)+b(i,j+id)))&
                     + (sin36*(a(i,j+ib)-a(i,j+ie))&
                     -  sin72*(a(i,j+ic)-a(i,j+id))))
                c(i,j+jd)=c3*((a(i,j+ia)&
                     -  cos36*(a(i,j+ib)+a(i,j+ie))&
                     +  cos72*(a(i,j+ic)+a(i,j+id)))&
                     + (sin36*(b(i,j+ib)-b(i,j+ie))&
                     -  sin72*(b(i,j+ic)-b(i,j+id))))&
                     -     s3*((b(i,j+ia)&
                     -  cos36*(b(i,j+ib)+b(i,j+ie))&
                     +  cos72*(b(i,j+ic)+b(i,j+id)))&
                     - (sin36*(a(i,j+ib)-a(i,j+ie))&
                     -  sin72*(a(i,j+ic)-a(i,j+id))))
                d(i,j+jd)=s3*((a(i,j+ia)&
                     -  cos36*(a(i,j+ib)+a(i,j+ie))&
                     +  cos72*(a(i,j+ic)+a(i,j+id)))&
                     + (sin36*(b(i,j+ib)-b(i,j+ie))&
                     -  sin72*(b(i,j+ic)-b(i,j+id))))&
                     +    c3*((b(i,j+ia)&
                     -  cos36*(b(i,j+ib)+b(i,j+ie))&
                     +  cos72*(b(i,j+ic)+b(i,j+id)))&
                     - (sin36*(a(i,j+ib)-a(i,j+ie))&
                     -  sin72*(a(i,j+ic)-a(i,j+id))))
             END DO
          END DO
       END DO
    ENDIF
  END SUBROUTINE OnePass






  !CreateFFT: Allocates and computes intermediate values used by
  !           the FFT for sequences of size nIn. If size
  !           is not in the form 2 * 2**i * 3**j * 5**k, with at
  !           least one of i,j,k/=0, stops and prints (stdout) the
  !           next possible size.



  SUBROUTINE CreateFFT(nIn, Factors, Trigs)
    INTEGER, INTENT(IN)  :: nIn
    INTEGER, POINTER     :: Factors(:)
    REAL,    POINTER     :: Trigs(:)
    CHARACTER(LEN=15), PARAMETER :: h="**(CreateFFT)**" ! header
    CALL Factorize  (nIn, Factors)
    nFactors = SIZE(Factors)
    CALL TrigFactors(nIn, Trigs)
    nTrigs = SIZE(Trigs)
  END SUBROUTINE CreateFFT



  !DestroyFFT: Dealocates input area, returning NULL pointers



  SUBROUTINE DestroyFFT(Factors, Trigs)
    INTEGER, POINTER :: Factors(:)
    REAL,    POINTER :: Trigs(:)
    CHARACTER(LEN=16), PARAMETER :: h="**(DestroyFFT)**" ! header
    DEALLOCATE(Factors); NULLIFY(Factors)
    DEALLOCATE(Trigs  ); NULLIFY(Trigs  )
  END SUBROUTINE DestroyFFT



  !Factorize: Factorizes nIn/2 in powers of 4, 3, 2, 5, if possible.
  !           Otherwise, stops with error message



  SUBROUTINE Factorize (nIn, Factors)
    INTEGER, INTENT(IN ) :: nIn
    INTEGER, POINTER     :: Factors(:)
    CHARACTER(LEN=15), PARAMETER :: h="**(Factorize)**" ! header
    CHARACTER(LEN=15) :: c ! Character representation of integer
    INTEGER :: Powers(nBase)
    INTEGER :: nOut
    INTEGER :: sumPowers
    INTEGER :: ifac
    INTEGER :: i
    INTEGER :: j
    INTEGER :: left ! portion of nOut/2 yet to be factorized

    nOut= NextSizeFFT(nIn)

    IF (nIn /= nOut) THEN
       WRITE(c,"(i15)") nIn
       WRITE(*,"(a,' FFT size = ',a,' not factorizable ')")&
            h, TRIM(ADJUSTL(c))
       WRITE(c,"(i15)") nOut
       WRITE(*,"(a,' Next factorizable FFT size is ',a)")&
            h, TRIM(ADJUSTL(c))
       STOP
    END IF

    ! Loop over evens, starting from nOut, getting factors of nOut/2

    left = nOut/2
    Powers = 0

    ! factorize nOut/2

    DO i = 1, nBase 
       DO
          IF (MOD(left, Base(i)) == 0) THEN
             Powers(i) = Powers(i) + 1
             left = left / Base(i)
          ELSE
             EXIT
          END IF
       END DO
    END DO

    sumPowers=SUM(Powers)
    ALLOCATE (Factors(sumPowers+1))
    Factors(1)=sumPowers
    ifac = 1
    DO i = 1, nBase
       j = Permutation(i)
       Factors(ifac+1:ifac+Powers(j)) = Base(j)
       ifac = ifac + Powers(j)
    END DO
  END SUBROUTINE Factorize



  !TrigFactors: Sin and Cos required to compute FFT of size nIn



  SUBROUTINE TrigFactors (nIn, Trigs)
    INTEGER, INTENT(IN) :: nIn
    REAL, POINTER       :: Trigs(:)
    INTEGER :: nn, nh, i
    REAL    :: radi, pi, del, angle

    radi=ATAN(1.)/45.
    sin60=SIN(60.*radi)
    sin36=SIN(36.*radi)
    sin72=SIN(72.*radi)
    cos36=COS(36.*radi)
    cos72=COS(72.*radi)

    nn =  nIn  / 2
    nh = (nn+1) / 2
    ALLOCATE (Trigs(2*(nn+nh)))

    pi = 2.0 * ASIN(1.0)
    del = (2 * pi) / float(nn)

    DO i = 1, 2*nn, 2
       angle = 0.5 * REAL(i-1) * del
       Trigs(i  ) = COS(angle)
       Trigs(i+1) = SIN(angle)
    END DO

    del = 0.5 * del
    DO i = 1, 2*nh, 2
       angle = 0.5 * float(i-1) * del
       Trigs(2*nn+i  ) = COS(angle)
       Trigs(2*nn+i+1) = SIN(angle)
    END DO
  END SUBROUTINE TrigFactors



  !NextSizeFFT: Smallest integer >= input in the form 2 * 2**i * 3**j * 5**k



  FUNCTION NextSizeFFT(nIn) RESULT(nOUT)
    INTEGER, INTENT(IN ) :: nIn
    INTEGER              :: nOut
    CHARACTER(LEN=22), PARAMETER :: h="**(NextSizeFFT)**" ! header
    REAL, PARAMETER :: limit = HUGE(nIn)-1   ! Maximum representable integer
    CHARACTER(LEN=15) :: charNIn ! Character representation of nIn
    INTEGER :: i 
    INTEGER :: left ! portion of nOut/2 yet to be factorized

    ! nOut = positive even 

    IF (nIn <= 0) THEN
       WRITE (charNIn,"(i15)") nIn
       WRITE (*,"(a,' Meaningless FFT size='a)")h, TRIM(ADJUSTL(charNIn))
       STOP
    ELSE IF (MOD(nIn,2) == 0) THEN
       nOut = nIn
    ELSE
       nOut = nIn+1
    END IF

    ! Loop over evens, starting from nOut, looking for
    ! next factorizable even/2

    DO
       left = nOut/2

       ! factorize nOut/2

       DO i = 1, nBase 
          DO
             IF (MOD(left, Base(i)) == 0) THEN
                left = left / Base(i)
             ELSE
                EXIT
             END IF
          END DO
       END DO

       IF (left == 1) THEN
          EXIT
       ELSE IF (nOut < limit) THEN
          nOut = nOut + 2
       ELSE
          WRITE (charNIn,"(i15)") nIn
          WRITE (*,"(a,' Next factorizable FFT size > ',a,&
               &' is not representable in this machine')")&
               h, TRIM(ADJUSTL(charNIn))
          STOP
       END IF
    END DO
  END FUNCTION NextSizeFFT



  !DirFFT: Computes direct FFT of 'lot' sequences of 'n' input real data
  !        as rows of 'fin', dimensioned 'fin(ldin,tdin)'. Input data is
  !        kept unchanged. Input values 'fin(n+1:ldin,:)' and 
  !        'fin(:,lot+1:tdin)' are not visited. 
  !        Outputs 'lot' sequences of 'n+1' real data
  !        as rows of 'fout', dimensioned 'fout(ldout,tdout)'. Output
  !        values 'fout(:,lot+1:tdout)' and 'fout(n+2:ldout,:)' are set to 0.



  SUBROUTINE DirFFTTrans (fInOut, ldInOut, tdInOut, n, lot, Trigs, nTrigs, Factors, nFactors)
    INTEGER, INTENT(IN ) :: ldInOut, tdInOut
    REAL,    INTENT(INOUT) :: fInOut (ldInOut ,tdInOut)
    INTEGER, INTENT(IN ) :: n
    INTEGER, INTENT(IN ) :: lot
    INTEGER, INTENT(IN ) :: nTrigs
    REAL,    INTENT(IN ) :: Trigs(nTrigs)
    INTEGER, INTENT(IN ) :: nFactors
    INTEGER, INTENT(IN ) :: Factors(nFactors)

    INTEGER :: nh
    INTEGER :: nfax
    INTEGER :: la
    INTEGER :: k
    LOGICAL :: ab2cd
    CHARACTER(LEN=12), PARAMETER :: h="**(Dir)**"
    REAL :: a(lot,n/2)
    REAL :: b(lot,n/2)
    REAL :: c(lot,n/2)
    REAL :: d(lot,n/2)

    nfax=Factors(1)
    nh=n/2

    CALL SplitGridTrans (fInOut, a, b, ldInOut, tdInOut, nh, lot)

    la=1
    ab2cd=.TRUE.
    DO k=1,nfax
       IF (ab2cd) THEN
          CALL OnePass (a, b, c, d, lot, nh, Factors(k+1), la, Trigs, nTrigs)
          ab2cd=.FALSE.
       ELSE
          CALL OnePass (c, d, a, b, lot, nh, Factors(k+1), la, Trigs, nTrigs)
          ab2cd=.TRUE.
       END IF
       la=la*Factors(k+1)
    END DO

    IF (ab2cd) THEN
       CALL JoinFourTrans (a, b, fInOut, ldInOut, tdInOut, n, nh, lot, Trigs, nTrigs)
    ELSE
       CALL JoinFourTrans (c, d, fInOut, ldInOut, tdInOut, n, nh, lot, Trigs, nTrigs)
    END IF
  END SUBROUTINE DirFFTTrans



  !SplitGrid: Split space domain real input into complex pairs to
  !           feed fundamental algorithm



  SUBROUTINE SplitGridTrans (fin, a, b, ldin, tdin, nh, lot)
    INTEGER, INTENT(IN ) :: ldin
    INTEGER, INTENT(IN ) :: tdin
    INTEGER, INTENT(IN ) :: nh
    INTEGER, INTENT(IN ) :: lot
    REAL,    INTENT(IN ) :: fin(ldin, tdin)
    REAL,    INTENT(OUT) :: a  (lot , nh)
    REAL,    INTENT(OUT) :: b  (lot , nh)

    INTEGER :: i, j

    DO j = 1, nh
       DO i = 1, lot
          a(i,j)=fin(i,2*j-1)
          b(i,j)=fin(i,2*j  )
       END DO
    END DO

  END SUBROUTINE SplitGridTrans



  !JoinFour: Unscramble frequency domain complex output from fundamental
  !          algorithm into real sequences



  SUBROUTINE JoinFourTrans (a, b, fout, ldout, tdout, n, nh, lot, Trigs, nTrigs)
    INTEGER, INTENT(IN ) :: ldout, tdout
    INTEGER, INTENT(IN ) :: n
    INTEGER, INTENT(IN ) :: nh
    INTEGER, INTENT(IN ) :: lot
    REAL,    INTENT(OUT) :: fout(ldout,tdout)
    REAL,    INTENT(IN ) :: a   (lot  ,nh)
    REAL,    INTENT(IN ) :: b   (lot  ,nh)
    INTEGER, INTENT(IN ) :: nTrigs
    REAL,    INTENT(IN ) :: Trigs(nTrigs)

    INTEGER :: i, j
    REAL    :: scale, scalh
    REAL    :: c, s

    scale=1.0/float(n)
    scalh=0.5*scale

    !cdir nodep
    DO i = 1, lot
       fout(i,  1)=scale*(a(i,1)+b(i,1))
       fout(i,n+1)=scale*(a(i,1)-b(i,1))
       fout(i,  2)=0.
    END DO

    DO j = 2, (nh+1)/2
       c=Trigs(n+2*j-1)
       s=Trigs(n+2*j  )
       !cdir nodep
       DO i = 1, lot
          fout(i,  2*j-1)=scalh*(   (a(i,j     )+a(i,nh+2-j)) &
               +                 (c*(b(i,j     )+b(i,nh+2-j))&
               +                  s*(a(i,j     )-a(i,nh+2-j))))
          fout(i,n+3-2*j)=scalh*(   (a(i,j     )+a(i,nh+2-j)) &
               -                 (c*(b(i,j     )+b(i,nh+2-j))&
               +                  s*(a(i,j     )-a(i,nh+2-j))))
          fout(i,    2*j)=scalh*((c*(a(i,j     )-a(i,nh+2-j))&
               -                  s*(b(i,j     )+b(i,nh+2-j)))&
               +                    (b(i,nh+2-j)-b(i,j     )))
          fout(i,n+4-2*j)=scalh*((c*(a(i,j     )-a(i,nh+2-j))&
               -                  s*(b(i,j     )+b(i,nh+2-j)))&
               -                    (b(i,nh+2-j)-b(i,j     )))
       END DO
    END DO

    IF ((nh>=2) .AND. (MOD(nh,2)==0)) THEN
       !cdir nodep
       DO i = 1, lot
          fout(i,nh+1)= scale*a(i,nh/2+1)
          fout(i,nh+2)=-scale*b(i,nh/2+1)
       END DO
    END IF

  END SUBROUTINE JoinFourTrans
END MODULE Transform
