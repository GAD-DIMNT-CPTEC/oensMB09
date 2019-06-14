!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE Sizes   ! Version 0 of Nov 25th, 2001



  ! SINGLE REPOSITORY OF SIZES AND MAPPINGS OF GRID, FOURIER AND SPECTRAL
  ! FIELD REPRESENTATIONS. SHOULD BE USED BY ALL MODEL MODULES THAT REQUIRE
  ! SUCH INFORMATION.
  !
  ! Module exports three procedures:
  ! RegisterBasicSizes: set all repository values for spectral representation.
  !                     It also sets latitudes and maximum number of longitudes
  !                     for grid representation.
  ! RegisterOtherSizes: set remaining repository values.
  ! DumpSizes:          detailed dumping of repository values
  !
  ! Module exports a set of values and arrays, described bellow.
  !
  ! Notation used throughout the code:
  ! m  Legendre Order (also Fourier wave number). Goes from 1 to mMax.
  ! n  Legendre Degree. Goes from m to nMax on regular spectral fields
  !    and from m to nExtMax on extended spectral fields.
  ! mn index to store the spectral triangle (m,n) into a single dimension.
  ! k  vertical index, from 1 to kMax
  ! j  latitude index, from 1 to jMax (full field)
  !    or from 1 to jMaxHalf (hemisphere)
  ! i  longitude index, from 1 to iMax (gaussian grid) or
  !    from 1 to iMaxPerJ(j) (at latitude j of the reduced grid)
  ! ib index of a longitude on a block of longitudes packed in one
  !    dimension of grids;
  ! jb index for which block of longitudes to take, packed in another
  !    dimension of grids

  ! SPECTRAL REPRESENTATION:
  ! A regular spectral field should be dimensioned (2*mnMax,kMax), where
  ! the first dimension accomodates pairs of (real,imaginary) spectral
  ! coefficients for a fixed vertical and the second dimension varies
  ! verticals.
  ! Extended spectral fields should use (2*mnExtMax,kMax).
  ! This module provides mapping functions to map (m,n) into mn:
  ! For a regular spectral field with complex entries, Legendre Order m
  ! and Degree n has real coefficient at position 2*mnMap(m,n)-1 and
  ! imaginary coefficient at position 2*mnMap(m,n). Inverse mappings 
  ! (from mn to m and n) are also provided. Maps for the extended spectral 
  ! field are also provided.

  ! GRID REPRESENTATION:
  ! Since the number of longitudes per latitude may vary with latitude
  ! (on reduced grids), sets of latitudes (with all longitudes) 
  ! are packed together in the first dimension of Grids. 
  ! Near the pole, many latitudes are packed; near the equator, just a few. 
  ! Second dimension is vertical.
  ! Third dimension is the number of packed latitudes required to represent
  ! a full field.
  ! A grid field should be dimensioned (ibMax, kMax, jbMax).
  ! This module provides mapping functions to map latitude j and longitude
  ! i into (ib,jb).
  ! Map ibPerIJ(i,j) gives index of first dimension that stores longitude
  ! i of latitude j. Map jbPerIJ(i,j) gives index of third dimension that
  ! stores longitude i of latitude j. Consequently, the point of longitude
  ! i, latitude j and vertical k is stored at (ibPerIJ(i,j), k, jbPerIJ(i,j)).
  ! Inverse mappings (from (ib,jb) to (i,j)) are also provided.

  ! FOURIER REPRESENTATION:
  ! For the moment, Fourier fields are represented externally to the
  ! transform. That should not happen in the future.
  ! First dimension contains pairs of (real,imaginary) fourier 
  ! coefficients. Second dimension is latitude. Third dimension is
  ! vertical. A full fourier field should be dimensioned 
  ! (iMax+1, jMax, kMax)

  USE Constants, ONLY: &
    rk,  &
    i8,  &
    r8

  IMPLICIT NONE


  
  ! SPECTRAL REPRESENTATION:
  ! mMax is the maximum Legendre Order (also maximum Fourier wave number).
  !      it is set to model truncation + 1 (Legendre Order 0 is m=1)
  ! nMax is the maximum Legendre Degree for regular spectral fields.
  !      it is set to model truncation + 1 (Legendre Degree 1 is n=1)
  ! mnMax is the amount of spectral coeficients per latitude, for 
  !       regular spectral fields. It is the number of points in the
  !       regular triangular spectral plane.
  ! mMap indexed by mn in [1:mnMax]; returns m at this mn for regular
  !      spectral fields;
  ! nMap indexed by mn in [1:mnMax]; returns n at this mn for regular
  !      spectral fields;
  ! mnMap indexed by (m,n); returns mn that stores spectral coefficient
  !       (m,n), when spectral coefficients are real (e.g. Associated 
  !       Legendre Functions). For complex spectral coefficients, 
  !       mn=2*mnMap(m,n) for the imaginary part and mn=2*mnMap(m,n)-1
  !       for the real part.
  ! Remaining variables with Ext extension have the same meaning, but
  ! for the extended spectral space (where nExtMax=trunc+2), that is,
  ! where each Legendre Order has one more Legendre Degree than usual.



  INTEGER(KIND=i8)              :: mMax=-1
  INTEGER(KIND=i8)              :: nMax=-1
  INTEGER(KIND=i8)              :: mnMax=-1
  INTEGER(KIND=i8), ALLOCATABLE :: mMap(:)
  INTEGER(KIND=i8), ALLOCATABLE :: nMap(:)
  INTEGER(KIND=i8), ALLOCATABLE :: mnMap(:,:)
  INTEGER(KIND=i8), ALLOCATABLE :: snnp1(:)
  INTEGER(KIND=i8)              :: nExtMax=-1
  INTEGER(KIND=i8)              :: mnExtMax=-1
  INTEGER(KIND=i8), ALLOCATABLE :: mExtMap(:)
  INTEGER(KIND=i8), ALLOCATABLE :: nExtMap(:)
  INTEGER(KIND=i8), ALLOCATABLE :: mnExtMap(:,:)



  ! LATITUDES REPRESENTATION:
  ! jMax is the number of latitudes for full Fourier and Grid representations.
  ! jMaxHalf is the number of latitudes at each hemisphere.
  ! mMaxPerJ is an array indexed by latitudes that stores the maximum value
  !          of m at each latitude. Consequently, the contribution of 
  !          latitude j for the Legendre Transform should be taken only
  !          up to Legendre Order mMaxPerJ(j). By the same token, 
  !          FFTs at latitude j should be computed only up to 
  !          Fourier wave number mMaxPerJ(j).
  !          For regular grids, mMaxPerJ(j) = mMax for all j.
  ! jMinPerM is the inverse mapping - an array indexed by Legendre Orders (m)
  !          containing the smallest latitude (value of j) that has that
  !          order. Latitudes that contain Legendre Order (and Fourier
  !          wave number) m are jMinPerM(m) <= j <= jMax-jMinPerM(m)+1
  !          For regular grids, jMinPerM(m) = 1 for all m.



  INTEGER(KIND=i8)              :: jMax=-1
  INTEGER(KIND=i8)              :: jMaxHalf=-1
  INTEGER(KIND=i8), ALLOCATABLE :: mMaxPerJ(:)
  INTEGER(KIND=i8), ALLOCATABLE :: jMinPerM(:)



  ! LONGITUDES REPRESENTATION:
  ! iMax is the maximum number of longitudes per latitude;
  !      it is the number of longitudes per latitude for all latitudes
  !      on regular grids;
  !      it is only the maximum number of longitudes per latitude on 
  !      reduced grids; it is the actual number of longitudes per latitude
  !      close to the equator on reduced grids;
  ! ijMax is the number of horizontal grid points at regular or reduced
  !       grids.
  ! iMaxPerJ is the actual number of longitudes per latitude on regular
  !          and reduced grids; latitude j has iMaxPerJ(j) longitudes.



  INTEGER(KIND=i8)              :: iMax=-1
  INTEGER(KIND=i8)              :: ijMax=-1
  INTEGER(KIND=i8)              :: ijMaxGauQua=-1
  INTEGER(KIND=i8), ALLOCATABLE :: iMaxPerJ(:)



  ! GRID REPRESENTATION:
  ! All longitudes of a set of latitudes are packed together in the
  ! first dimension of grids. That decreases the waste of memory when
  ! comparing to store one latitude only, for the case of reduced
  ! grid. 
  ! ibMax is the maximum number of longitudes packed into the first dimension
  !       of grids. The actual number of longitudes vary with the third
  !       dimension of the grid representation.
  ! jbMax is the number of sets of longitudes required to store an
  !       entire field.
  ! ibPerIJ maps longitude i and latitude j into the first dimension
  !         of grid representation. It is indexed ibPerIJ(i,j).
  ! jbPerIJ maps longitude i and latitude j into the third dimension
  !         of grid representation. It is indexed jbPerIJ(i,j).
  ! iPerIJB gives which longitude is stored at first dimension index
  !         i and third dimension index j of Grid representations.
  !         It is indexed iPerIJB(i,j)
  ! jPerIJB gives which latitude is stored at first dimension index
  !         i and third dimension index j of Grid representations.
  !         It is indexed jPerIJB(i,j)
  ! ibMaxPerJB gives how many latitudes are actually stored at third
  !            dimension jb. Since the number of longitudes vary with
  !            latitudes, the amount of space actually used in the first
  !            dimension of grid representations vary with the third
  !            dimension. Array ibMaxPerJB, indexed by jb, accounts for
  !            such variation.


  INTEGER(KIND=i8)              :: ibMax=-1
  INTEGER(KIND=i8)              :: jbMax=-1
  INTEGER(KIND=i8), ALLOCATABLE :: ibPerIJ(:,:)
  INTEGER(KIND=i8), ALLOCATABLE :: jbPerIJ(:,:)
  INTEGER(KIND=i8), ALLOCATABLE :: iPerIJB(:,:)
  INTEGER(KIND=i8), ALLOCATABLE :: jPerIJB(:,:)
  INTEGER(KIND=i8), ALLOCATABLE :: ibMaxPerJB(:)

  INTEGER(KIND=i8)              :: kMax=-1

  REAL(KIND=r8), ALLOCATABLE :: ci(:)      ! 1 - sigma each level (level 1 at surface)
  REAL(KIND=r8), ALLOCATABLE :: si(:)      ! sigma
  REAL(KIND=r8), ALLOCATABLE :: del(:)     ! layer thickness (in sigma)
  REAL(KIND=r8), ALLOCATABLE :: delcl(:)     ! layer thickness (in cl)
  REAL(KIND=r8), ALLOCATABLE :: rdel2(:)
  REAL(KIND=r8), ALLOCATABLE :: sl(:)      ! sigma at layer midpoint
  REAL(KIND=r8), ALLOCATABLE :: cl(:)      ! 1.0 - sl
  REAL(KIND=r8), ALLOCATABLE :: rpi(:)     ! 'pi' ratios at adjacent layers
  LOGICAL, PARAMETER, PRIVATE :: dumpLocal=.FALSE.

CONTAINS







  SUBROUTINE RegisterBasicSizes(trunc, nLat, nLon, vert)
    INTEGER(KIND=i8), INTENT(IN) :: trunc
    INTEGER(KIND=i8), INTENT(IN) :: nLat
    INTEGER(KIND=i8), INTENT(IN) :: nLon
    INTEGER(KIND=i8), INTENT(IN) :: vert
    INTEGER(KIND=i8) :: m, n, mn, diag, ele
    CHARACTER(LEN=*), PARAMETER :: h="**(RegisterBasicSizes)**"

    jMax     = nLat
    jMaxHalf = nLat/2_i8

    iMax     = nLon

    kMax     = vert

    mMax     = trunc + 1_i8
    nMax     = mMax
    nExtMax  = mMax + 1_i8
    mnExtMax = (nExtMax+2_i8)*(nExtMax-1_i8)/2_i8
    ALLOCATE (mExtMap(mnExtMax))
    ALLOCATE (nExtMap(mnExtMax))
    ALLOCATE (mnExtMap(mMax,nExtMax))
    nExtMap = -1_i8  ! flag mapping error
    mExtMap = -1_i8  ! flag mapping error
    mnExtMap = -1_i8 ! flag mapping error
    mn = 0_i8
    DO diag = 1_i8, nExtMax ! diagonal
       DO ele = 1_i8, MIN(mMax-diag+2_i8, mMax)
          m = ele
          n = ele+diag-1_i8
          mn = mn + 1_i8
          mnExtMap(m,n) = mn
          mExtMap(mn)   = m
          nExtMap(mn)   = n
       END DO
    END DO
    mnMax = (mMax * (nMax+1_i8))/2_i8
    ALLOCATE (mnMap(mMax,nMax))
    ALLOCATE (mMap(mnMax))
    ALLOCATE (nMap(mnMax))
    mnMap = -1_i8  ! flag mapping error
    mMap = -1_i8   ! flag mapping error
    nMap = -1_i8   ! flag mapping error
    mn = 0_i8
    DO diag = 1_i8, nMax 
       DO ele = 1_i8, mMax-diag+1_i8
          m = ele
          n = m+diag-1_i8
          mn = mn + 1_i8
          mnMap(m,n) = mn
          mMap(mn)   = m
          nMap(mn)   = n
       END DO
    END DO
    ALLOCATE (snnp1(2_i8*mnMax))
    DO m = 1_i8, mMax
       DO n = m, nMax
          mn = mnMap(m,n)
          snnp1(2_i8*mn-1_i8) = (n-1_i8)*n
          snnp1(2_i8*mn  ) = (n-1_i8)*n
       END DO
    END DO
!    IF (dumpLocal) THEN
!       WRITE(*,"(a,' Dump at the end ')") h
       CALL DumpSizes()
!    END IF

    ijMaxGauQua = iMax*jMax

  END SUBROUTINE RegisterBasicSizes






  SUBROUTINE RegisterOtherSizes(iMaxPerLat, mPerLat)
    INTEGER(KIND=i8), INTENT(IN) :: iMaxPerLat(jMax)
    INTEGER(KIND=i8), INTENT(IN) :: mPerLat(jMax)
    CHARACTER(LEN=*), PARAMETER :: h="**(RegisterOtherSizes)**"
    INTEGER :: MinLatPerBlk
    !$ INTEGER(KIND=i8), EXTERNAL ::  OMP_GET_MAX_THREADS
    INTEGER(KIND=i8) :: i, j, m, ib, jb, cnt, nTrd

    ALLOCATE (mMaxPerJ(jMax))
    mMaxPerJ = mPerLat
    ALLOCATE (iMaxPerJ(jMax))
    iMaxPerJ = iMaxPerLat
    IF (iMax /= MAXVAL(iMaxPerJ)) THEN
       STOP ' imax and imaxperj disagree'
    END IF
    ijMax = SUM(iMaxPerJ)
    ALLOCATE (jMinPerM(mMax))
    jMinPerM = jMaxHalf
    DO j = 1_i8, jMaxHalf
       m = mMaxPerJ(j)
       jMinPerM(1_i8:m) = MIN(j, jMinPerM(1_i8:m))
    END DO

    ! # longitudes per block

    nTrd = 1_i8
    !$ nTrd = OMP_GET_MAX_THREADS()
    MinLatPerBlk = 1_i8
    !MinLatPerBlk = MIN(1024_i8/iMax, CEILING(REAL(jMax)/REAL(nTrd)))
    !MinLatPerBlk = CEILING(REAL(jMax)/REAL(nTrd))
PRINT *, "**(JP)** MinLatPerBlk=",MinLatPerBlk, 1024_i8/iMax, CEILING(REAL(jMax)/REAL(nTrd))
    ibMax = MinLatPerBlk*iMax
    
    ! # blocks

    jbMax = 1_i8
    cnt = 0_i8
    DO j = 1_i8, jMax
       cnt = cnt + iMaxPerJ(j)
       IF (cnt > ibMax) THEN
          jbMax = jbMax + 1_i8
          cnt = iMaxPerJ(j)
       ELSE IF (cnt == ibMax) THEN
          jbMax = jbMax + 1_i8
          cnt = 0_i8
       END IF
    END DO
    IF (cnt == 0_i8) THEN
       jbMax = jbMax - 1_i8
    END IF

    ! maps (i,j) into (ib,jb) and vice-versa

    ALLOCATE (ibPerIJ(iMax ,jMax ))
    ibPerIJ=-1_i8
    ALLOCATE (jbPerIJ(iMax ,jMax ))
    jbPerIJ=-1_i8
    ALLOCATE (iPerIJB(ibMax,jbMax))
    iPerIJB=-1_i8
    ALLOCATE (jPerIJB(ibMax,jbMax))
    jPerIJB=-1_i8
    ALLOCATE (ibMaxPerJB(jbMax))
    ibMaxPerJB=-1_i8

    jb = 1_i8
    ib = 0_i8
    DO j = 1_i8, jMax
       IF (ib + iMaxPerJ(j) > ibMax) THEN
          jb = jb + 1_i8
          ib = 0_i8
       END IF
       ibPerIJ(   1_i8:   iMaxPerJ(j),  j) = (/ (i, i=ib+1_i8,ib+iMaxPerJ(j)) /)
       jbPerIJ(   1_i8:   iMaxPerJ(j),  j) = jb
       iPerIJB(ib+1_i8:ib+iMaxPerJ(j), jb) = (/ (i, i=1_i8,iMaxPerJ(j)) /)
       jPerIJB(ib+1_i8:ib+iMaxPerJ(j), jb) = j
       ib = ib + iMaxPerJ(j)
    END DO

    DO j = 1, jMax
       ibMaxPerJB(jbPerIJ(iMaxPerJ(j),j)) = ibPerIJ(iMaxPerJ(j),j)
    END DO

    ! OpenMP parallelism

    IF (dumpLocal) THEN
       WRITE(*,"(a,' Dump at the end ')") h
       CALL DumpSizes()
    END IF
  END SUBROUTINE RegisterOtherSizes






  SUBROUTINE DumpSizes()
    CHARACTER(LEN=*), PARAMETER :: h="**(DumpSizes)**"
    CHARACTER(LEN=10) :: c1, c2, c3, c4, c5, c6
    LOGICAL :: Mask(jMaxHalf)
    INTEGER(KIND=i8) :: first(1)
    INTEGER(KIND=i8) :: jMaxPerM(mMax)
    INTEGER(KIND=i8) :: i, j, jj, lastjj, jb, m
    INTEGER(KIND=i8) :: firstLat, lastLat, firstM, lastM, firstJ, lastJ, lastIB
    IF (mMax == -1_i8) THEN
       WRITE(*,"(a,' Sizes not created')") h
    ELSE
       WRITE(c1,"(i10)") mMax
       WRITE(c2,"(i10)") nMax
       WRITE(c3,"(i10)") nExtMax
       WRITE(c4,"(i10)") mnExtMax
       WRITE(*,"(a,' mMax=',a,'; nMax=',a,'; nExtMax=',a,'; mnExtMax=',a)") &
            h, TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2)), TRIM(ADJUSTL(c3)), &
            TRIM(ADJUSTL(c4))
       WRITE(c1,"(i10)") jMax
       WRITE(c2,"(i10)") jMaxHalf
       WRITE(*,"(a,' jMax=',a,'; jMaxHalf=',a)") &
            h, TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2))
       WRITE(c1,"(i10)") iMax
       WRITE(c2,"(i10)") kMax
       WRITE(c3,"(i10)") ijMax
       IF (ijMax == -1_i8) THEN
          WRITE(*,"(a,' iMax=',a,'; kMax=',a)") h, &
               TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2))
          WRITE(*,"(a,' Sizes not fully created')") h
       ELSE
          WRITE(*,"(a,' iMax=',a,'; kMax=',a,'; ijMax=',a)") h, &
               TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2)), TRIM(ADJUSTL(c3))
          WRITE(*,"(' latitudes   longitudes   m')")
          DO i = MINVAL(iMaxPerJ), MAXVAL(iMaxPerJ)-1_i8
             Mask = iMaxPerJ(1_i8:jMaxHalf) == i
             IF (ANY(Mask)) THEN
                first = MINLOC(iMaxPerJ(1_i8:jMaxHalf), Mask)
                firstLat = first(1_i8)
                lastLat = firstLat + COUNT(Mask) - 1_i8
                WRITE(*,"(i5,':',i3,5x,i5,6x,'0:',i3)") &
                     firstLat, lastLat, i, mMaxPerJ(lastLat)-1_i8
             END IF
          END DO
          i = MAXVAL(iMaxPerJ)
          first = MINLOC(iMaxPerJ, iMaxPerJ==i)
          firstLat = first(1_i8)
          lastLat = jMax-firstLat+1_i8
          WRITE(*,"(i5,':',i3,5x,i5,6x,'0:',i3)") &
               firstLat, lastLat, i, mMaxPerJ(lastLat)-1_i8
          DO i = MAXVAL(iMaxPerJ)-1_i8, MINVAL(iMaxPerJ), -1_i8
             Mask = iMaxPerJ(jMaxHalf+1_i8:jMax) == i
             IF (ANY(Mask)) THEN
                first = MINLOC(iMaxPerJ(jMaxHalf+1_i8:jMax), Mask)
                firstLat = first(1_i8) + jMaxHalf
                lastLat = firstLat + COUNT(Mask) - 1_i8
                WRITE(*,"(i5,':',i3,5x,i5,6x,'0:',i3)") &
                     firstLat, lastLat, i, mMaxPerJ(lastLat)-1_i8
             END IF
          END DO


          WRITE(*,"('     m       latitudes')")
          jMaxPerM = jMax - jMinPerM + 1_i8

          lastM = 0_i8
          DO
             IF (lastM == mMax) THEN
                EXIT
             ELSE
                firstM=lastM+1_i8
                lastM=firstM
                firstJ=jMinPerM(firstM)
                lastJ=jMaxPerM(firstM)
                DO m = firstM+1_i8, mMax
                   IF ((firstJ == jMinPerM(m)) .AND. &
                        (lastJ == jMaxPerM(m))) THEN
                      lastM = m
                   ELSE
                      EXIT
                   END IF
                END DO
                WRITE(*,"(i5,':',i3,5x,i3,':',i3)") &
                     firstM-1_i8, lastM-1_i8, firstJ, lastJ
             END IF
          END DO

          WRITE(c1,"(i10)") ibMax
          WRITE(c2,"(i10)") jbMax
          WRITE(c3,"(i10)") jbMax*ibMax
          WRITE(*,"(a,' ibMax=',a,'; jbMax=',a,'; ijbMax=',a)") h, &
               TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2)), TRIM(ADJUSTL(c3))
          DO j = 1_i8, jMax
             IF (jbPerIJ(1_i8,j) /= jbPerIJ(iMaxPerJ(j),j)) THEN
                STOP "error in mapping jbPerIJ"
             END IF
             WRITE(c1,"(i10)") 1_i8
             WRITE(c2,"(i10)") iMaxPerJ(j)
             WRITE(c3,"(i10)") j
             WRITE(c4,"(i10)") ibPerIJ(1,j)
             WRITE(c5,"(i10)") ibPerIJ(iMaxPerJ(j),j)
             WRITE(c6,"(i10)") jbPerIJ(1,j)
             WRITE(*,"(a,'(Long,Lat)=(',a,':',a,',',a,') &
                  &mapped into(ib,jb)=(',&
                  &a,':',a,',',a,')')") h, &
                  TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2)), &
                  TRIM(ADJUSTL(c3)), TRIM(ADJUSTL(c4)), &
                  TRIM(ADJUSTL(c5)), TRIM(ADJUSTL(c6))
          END DO


          DO jb = 1, jbMax
             lastIb = ibMaxPerJB(jb)
             WRITE(c1,"(i10)") 1_i8
             WRITE(c2,"(i10)") lastIb
             WRITE(c3,"(i10)") jb
             WRITE(*,"(a,'(ib,jb)=(',a,':',a,',',a,') maps (Long,Lat)=')") &
                  h, TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2)), TRIM(ADJUSTL(c3))
             firstJ = jPerIJB(1_i8,jb)
             lastJ  = jPerIJB(lastIb,jb)
             DO jj = firstJ, lastJ, 5_i8
                lastjj = MIN(jj+4_i8,lastJ)
                DO j = jj, lastjj
                   WRITE(c4,"(i10)") 1_i8
                   WRITE(c5,"(i10)") iMaxPerJ(j)
                   WRITE(c6,"(i10)") j
                   WRITE(*,"('(',a,':',a,',',a,')')",ADVANCE='no') &
                        TRIM(ADJUSTL(c4)), TRIM(ADJUSTL(c5)), TRIM(ADJUSTL(c6))
                   IF (j /= lastjj) THEN
                      WRITE(*,"(',')", ADVANCE='no')
                   ELSE IF (j /= lastj) THEN
                      WRITE(*,"(',')")
                   ELSE
                      WRITE(*,"('')")
                   END IF
                END DO
             END DO
          END DO
       END IF
    END IF
  END SUBROUTINE DumpSizes



  SUBROUTINE ThreadDecomp(firstInd, lastInd, minInd, maxInd, msg)
    INTEGER(KIND=i8), INTENT(IN ) :: firstInd
    INTEGER(KIND=i8), INTENT(IN ) :: lastInd
    INTEGER(KIND=i8), INTENT(OUT) :: minInd
    INTEGER(KIND=i8), INTENT(OUT) :: maxInd
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(KIND=i8) :: chunk
    INTEGER(KIND=i8) :: left
    INTEGER(KIND=i8) :: nTrd
    INTEGER(KIND=i8) :: iTrd
    INTEGER(KIND=i8) :: length
    LOGICAL :: inParallel
    CHARACTER(LEN=*), PARAMETER :: h="**(ThreadDecomp)**"
    !$ INTEGER(KIND=i8), EXTERNAL :: OMP_GET_NUM_THREADS
    !$ INTEGER(KIND=i8), EXTERNAL :: OMP_GET_THREAD_NUM
    !$ LOGICAL, EXTERNAL :: OMP_IN_PARALLEL

    inParallel = .FALSE.
    nTrd=1_i8
    iTrd=0_i8
    !$ inParallel = OMP_IN_PARALLEL()
    IF (inParallel) THEN
       !$ nTrd = OMP_GET_NUM_THREADS()
       !$ iTrd = OMP_GET_THREAD_NUM()
       length = lastInd - firstInd + 1_i8
       chunk = length/nTrd
       left  = length - chunk*nTrd
       IF (iTrd < left) THEN
          minInd =  iTrd   *(chunk+1_i8) + firstInd
          maxInd = (iTrd+1_i8)*(chunk+1_i8) + firstInd - 1_i8
       ELSE
          minInd =  iTrd   *(chunk) + left + firstInd
          maxInd = (iTrd+1_i8)*(chunk) + left + firstInd - 1_i8
       END IF
    ELSE
       minInd = firstInd
       maxInd = lastInd
    END IF

    IF (dumpLocal) THEN
       IF (inParallel) THEN
          WRITE(*,"(a,' thread ',i2,' got [',i8,':',i8,&
               &'] from [',i8,':',i8,'] in parallel region at ',a)") &
               h, iTrd, minInd, maxInd, firstInd, lastInd, msg
       ELSE
          WRITE(*,"(a,' kept domain [',i8,':',i8,&
               &'] since not in parallel region at ',a)") &
               h, minInd, maxInd, msg
       END IF
    END IF
  END SUBROUTINE ThreadDecomp


  SUBROUTINE InitVerSizes (si_in, sl_in, del_in)

    REAL(KIND=r8), INTENT(IN) :: si_in(:)
    REAL(KIND=r8), INTENT(IN) :: sl_in(:)
    REAL(KIND=r8), INTENT(Out) :: del_in(:)

    INTEGER(KIND=i8) :: kMaxsl
    INTEGER(KIND=i8) :: k
    REAL(KIND=r8)    :: rk1, sirk, sirk1, dif

    kMaxsl = SIZE(sl_in)
    IF (kMaxsl /= kMax) STOP ' Error in InitVerSizes: kMaxsl=SIZE(sl) /= kMax '
    ALLOCATE(del(kMax))
    ALLOCATE(delcl(kMax-1_i8))
    ALLOCATE(rdel2(kMax))
    ALLOCATE(ci(kMax+1_i8))
    ALLOCATE(si(kMax+1_i8))
    ALLOCATE(sl(kMax))
    ALLOCATE(cl(kMax))
    ALLOCATE(rpi(kMax-1_i8))

    DO k=1_i8, kMax
       del(k)=si_in(k)-si_in(k+1)
    END DO
    del_in=del
    rdel2 = 0.5_r8/del

    rk1 = rk + 1.0_r8

    !cdir novector
    ci(1_i8) = 0.0_r8
    DO k=1_i8, kMax-1_i8
       ci(k+1)=ci(k)+del(k)
    END DO
    ci(kMax+1)=1.0_r8

    DO k=1_i8, kMax+1_i8
       si(k) = 1.0_r8 - ci(k)
    END DO

    DO k=1_i8, kMax
       sirk =EXP(rk1*LOG(si(k)))
       IF(k.LE.kMax-1_i8) THEN
          sirk1=EXP(rk1*LOG(si(k+1_i8)))
       ELSE
          sirk1=0.0_r8
       END IF
       dif = sirk-sirk1
       dif = dif / (rk1*(si(k)-si(k+1_i8)))
       sl(k) = EXP(LOG(dif)/rk)
       cl(k) = 1.0_r8 - sl(k)
    END DO
    DO k = 1_i8, kMax-1_i8
       delcl(k) = cl(k+1) - cl(k)
    END DO
    !     
    !     compute pi ratios for temp. matrix.
    !
    DO k=1_i8, kMax-1_i8
       rpi(k) = EXP(rk*LOG(sl(k+1)/sl(k)))
    END DO

  END SUBROUTINE InitVerSizes


END MODULE Sizes
