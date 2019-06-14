!
!  $Author: panetta $
!  $Date: 2007/08/12 13:52:18 $
!  $Revision: 1.9 $
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

  USE Options, ONLY: &
       nfprt,        &
       reducedGrid

  IMPLICIT NONE


  ! INTERNAL DATA STRUCTURE:
  ! Provision for MPI computation:
  !
  ! Domain Decomposition:
  ! There are maxNodes MPI processes, numbered from 0 to maxNodes - 1
  ! The number of this MPI process is myId, 0 <= myId <= maxNodes - 1
  ! SPECTRAL REPRESENTATION:
  ! mMax is the maximum Legendre Order (also maximum Fourier wave number).
  !      it is set to model truncation + 1 (Legendre Order 0 is m=1)
  ! nMax is the maximum Legendre Degree for regular spectral fields.
  !      it is set to model truncation + 1 (Legendre Degree 1 is n=1)
  ! mnMax is the amount of spectral coeficients per vertical, for 
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

  ! Spectral coefficients are spread across processes.
  ! The set of values of m is partitioned among processes.
  ! Consequently, all values of n for some values of m are stored
  ! at each process. Each value of m is stored at a single process.
  !
  ! Array nodeHasM(m) has which MPI process stores m, 1 <= m <= mMax
  !
  ! Variable myMMax (0 <= myMMax <= mMax) has how many m's are
  ! stored at this node.
  !
  ! Array lm2m(l), l=1,...,myMMax has the m's stored at this
  ! node. That is,
  ! it maps local m (lm) into global m (m).


  INTEGER              :: mMax=-1
  INTEGER              :: nMax=-1
  INTEGER              :: mnMax=-1
  INTEGER, ALLOCATABLE :: mMap(:)
  INTEGER, ALLOCATABLE :: nMap(:)
  INTEGER, ALLOCATABLE :: mnMap(:,:)
  INTEGER              :: nExtMax=-1
  INTEGER              :: mnExtMax=-1
  INTEGER, ALLOCATABLE :: mExtMap(:)
  INTEGER, ALLOCATABLE :: nExtMap(:)
  INTEGER, ALLOCATABLE :: mnExtMap(:,:)

  INTEGER, ALLOCATABLE :: nodeHasM(:)
  INTEGER, ALLOCATABLE :: lm2m(:)
  INTEGER, ALLOCATABLE :: MMaxinproc(:)
  INTEGER, ALLOCATABLE :: Msinproc(:,:)
  INTEGER, ALLOCATABLE :: Msperproc(:)

  INTEGER              :: myMMax
  INTEGER              :: MMaxlocal
  INTEGER              :: mnMaxlocal
  INTEGER              :: mnextMaxlocal
  INTEGER              :: myMNMax
  INTEGER              :: myMNExtMax

  INTEGER, ALLOCATABLE :: myMMap(:)
  INTEGER, ALLOCATABLE :: myNMap(:)
  INTEGER, ALLOCATABLE :: mnmaxinproc(:)
  INTEGER, ALLOCATABLE :: mnextmaxinproc(:)
  INTEGER, ALLOCATABLE :: myMNMap(:,:)
  INTEGER, ALLOCATABLE :: myMExtMap(:)
  INTEGER, ALLOCATABLE :: myNExtMap(:)
  INTEGER, ALLOCATABLE :: myMNExtMap(:,:)

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

  


  INTEGER              :: jMax=-1
  INTEGER              :: jMaxHalf=-1
  INTEGER, ALLOCATABLE :: mMaxPerJ(:)
  INTEGER, ALLOCATABLE :: jMinPerM(:)
  INTEGER, ALLOCATABLE :: jMaxPerM(:)



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



  INTEGER              :: iMax=-1
  INTEGER              :: ijMax=-1
  INTEGER              :: ijMaxGauQua=-1
  INTEGER, ALLOCATABLE :: iMaxPerJ(:)



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

  ! Grid Point Decomposition
  !
  ! Blocks of surface points are spreaded across processes.
  ! Each process has all longitudes of a set of latitudes (block)
  !
  ! variable myJMax has how many latitudes stored at this proc
  ! first stored latitude at each local block is jFirstLJB(JBMax)
  ! last stored latitude at each local block is jLastLJB(JBMax)

  INTEGER              :: ibMax=-1
  INTEGER              :: jbMax=-1
  INTEGER, ALLOCATABLE :: ibPerIJ(:,:)
  INTEGER, ALLOCATABLE :: jbPerIJ(:,:)
  INTEGER, ALLOCATABLE :: iPerIJB(:,:)
  INTEGER, ALLOCATABLE :: jPerIJB(:,:)
  INTEGER, ALLOCATABLE :: ibMaxPerJB(:)
  INTEGER, ALLOCATABLE :: firstlatinproc(:)
  INTEGER, ALLOCATABLE :: lastlatinproc(:)
  INTEGER, ALLOCATABLE :: nlatsinproc(:)
  INTEGER, ALLOCATABLE :: pointsinproc(:)
  INTEGER, ALLOCATABLE :: nodeHasJ(:)

  INTEGER              :: myfirstlat
  INTEGER              :: mylastlat
  INTEGER              :: jbminus
  INTEGER              :: jbplus

  INTEGER              :: myJMax
  INTEGER              :: JMaxlocal
  INTEGER, ALLOCATABLE :: jFirstLJB(:)
  INTEGER, ALLOCATABLE :: jLastLJB(:)


  INTEGER              :: kMax=-1

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
    INTEGER, INTENT(IN) :: trunc
    INTEGER, INTENT(IN) :: nLat
    INTEGER, INTENT(IN) :: nLon
    INTEGER, INTENT(IN) :: vert
    INTEGER :: m, n, mn
    CHARACTER(LEN=*), PARAMETER :: h="**(RegisterBasicSizes)**"

    jMax     = nLat
    jMaxHalf = nLat/2

    iMax     = nLon

    kMax     = vert

    mMax     = trunc + 1
    nMax     = mMax
    nExtMax  = mMax + 1
    mnExtMax = (nExtMax+2)*(nExtMax-1)/2
    ALLOCATE (mExtMap(mnExtMax))
    ALLOCATE (nExtMap(mnExtMax))
    ALLOCATE (mnExtMap(mMax,nExtMax))
    nExtMap = -1  ! flag mapping error
    mExtMap = -1  ! flag mapping error
    mnExtMap = -1 ! flag mapping error
    mn = 0
    DO m = 1, mMax
       DO n = m, mMax+1
          mn = mn + 1
          mnExtMap(m,n) = mn
          mExtMap(mn)   = m
          nExtMap(mn)   = n
       END DO
    END DO
    mnMax = (mMax * (nMax+1))/2
    ALLOCATE (mnMap(mMax,nMax))
    ALLOCATE (mMap(mnMax))
    ALLOCATE (nMap(mnMax))
    mnMap = -1  ! flag mapping error
    mMap = -1   ! flag mapping error
    nMap = -1   ! flag mapping error
    mn = 0
    DO m = 1, mMax
       DO n = m, mMax
          mn = mn + 1
          mnMap(m,n) = mn
          mMap(mn)   = m
          nMap(mn)   = n
       END DO
    END DO
    IF (dumpLocal) THEN
       WRITE(nfprt,"(a,' Dump at the end ')") h
       CALL DumpSizes()
    END IF

    ijMaxGauQua = iMax*jMax

  END SUBROUTINE RegisterBasicSizes






  SUBROUTINE RegisterOtherSizes(iMaxPerLat, mPerLat, myid, maxnodes, jovlap)
    INTEGER, INTENT(IN) :: iMaxPerLat(jMax)
    INTEGER, INTENT(IN) :: mPerLat(jMax)
    INTEGER, INTENT(IN) :: myid
    INTEGER, INTENT(IN) :: maxnodes
    INTEGER, INTENT(IN) :: jovlap
    CHARACTER(LEN=*), PARAMETER :: h="**(RegisterOtherSizes)**"
    INTEGER :: MinLatPerBlk
    !$ INTEGER, EXTERNAL ::  OMP_GET_MAX_THREADS
    INTEGER :: i, j, k, jk, m, ib, jb, jh, cnt, nTrd, imp2
    INTEGER :: meanl, meanp, jlast, jused, jfirst
    INTEGER :: mfirst(maxnodes), mlast(maxnodes), npoints(maxnodes)
    INTEGER :: mlast1(maxnodes), npoints1(maxnodes)
    INTEGER :: maxpoints, maxpointsold
    INTEGER :: nproc, nlp, nrest
    LOGICAL :: improved, done

    nproc = maxnodes
    ALLOCATE (mMaxPerJ(jMax))
    mMaxPerJ = mPerLat
    ALLOCATE (iMaxPerJ(jMax))
    iMaxPerJ = iMaxPerLat
    IF (iMax < MAXVAL(iMaxPerJ)) THEN
       STOP ' imax and imaxperj disagree'
    END IF
    IF (jmax.lt.nproc) THEN
       STOP "number of latitudes smaller than number of processors"
    END IF
    ijMax = SUM(iMaxPerJ)
    ALLOCATE (pointsinproc(0:maxNodes-1))
    ALLOCATE (firstlatinproc(0:maxNodes-1))
    ALLOCATE (lastlatinproc(0:maxNodes-1))
    ALLOCATE (nlatsinproc(0:maxNodes-1))
    ALLOCATE (nodeHasJ(jMax))

    IF (reducedGrid) THEN
       meanl = ijMax / jMax
       meanp = ijMax / nproc

       mlast = 0
       npoints = 0
       mlast1 = 0
       npoints1 = 0
       jlast = jmax
       !
       !  compute first possible partition
       !
       DO i=nproc,1,-1
          DO j=jlast,1,-1
             IF (npoints(i).lt.meanp) THEN
                npoints(i) = npoints(i) + iMaxPerJ(j)
                IF (mlast(i).eq.0) mlast(i) = j
                jused = j
               ELSE
                EXIT
             ENDIF
          ENDDO
          jlast = jused  - 1
       ENDDO
       maxpointsold = MAXVAL(npoints)
       !
       !  improve the partition while possible
       !
       DO
          jlast = jmax
          DO i=nproc,1,-1
             DO j=jlast,1,-1
                IF (npoints1(i)+iMaxPerJ(j).lt.maxpointsold) THEN
                   npoints1(i) = npoints1(i) + iMaxPerJ(j)
                   IF (mlast1(i).eq.0) mlast1(i) = j
                   jused = j
                  ELSE
                   EXIT
                ENDIF
             ENDDO
             jlast = jused - 1
          ENDDO
          IF (jlast.eq.0) THEN 
             maxpoints = MAXVAL(npoints1)
            ELSE
             EXIT
          ENDIF
          IF (maxpoints.lt.maxpointsold) THEN
             npoints = npoints1
             npoints1 = 0
             mlast = mlast1
             mlast1 = 0
             maxpointsold = maxpoints
            ELSE
             EXIT
          ENDIF
       ENDDO
       !
       !  make the partition more uniform
       !
       DO i=1,nproc
         IF(mlast(i).ge.i) THEN
           jused = i
           EXIT
         ENDIF
       ENDDO
       DO i=1,jused-1
          mlast(i)=i
          npoints(i) = imaxperj(i)
       ENDDO
       npoints(jused) = 0
       DO i=jused,mlast(jused)
          npoints(jused) = npoints(jused) + imaxperj(i)
       ENDDO
       mfirst = 0
       IF (npoints(1).gt.0) mfirst(1)=1   
       DO i=2,nproc
          IF (npoints(i).gt.0) mfirst(i) = mlast(i-1)+1
       ENDDO
       DO
          improved = .false.
          DO i=1,nproc-1
             IF (npoints(i)-npoints(i+1).gt.imaxperj(mlast(i))) THEN
                npoints(i) = npoints(i) - imaxperj(mlast(i))
                npoints(i+1) = npoints(i+1) + imaxperj(mlast(i))
                IF (mlast(i+1).eq.0) mlast(i+1) = mlast(i)
                mlast(i) = mlast(i) - 1
                improved = .true.
              ELSEIF (npoints(i+1)-npoints(i).gt.imaxperj(mfirst(i+1))) THEN
                npoints(i) = npoints(i) + imaxperj(mfirst(i+1))
                npoints(i+1) = npoints(i+1) - imaxperj(mfirst(i+1))
                mlast(i) = mfirst(i+1)
                mfirst(i+1) = mfirst(i+1) + 1
                IF (mfirst(i).eq.0) mfirst(i) = mlast(i)
                improved = .true.
             ENDIF
          ENDDO
          IF (.not.improved) EXIT
       ENDDO

     ELSE
       
       nlp = jmax / nproc
       nrest = jmax - nproc * nlp
       DO i=1,nproc-nrest
          mfirst(i) = (i-1)*nlp + 1
          mlast(i) = mfirst(i) + nlp - 1
          npoints(i) = imaxperj(1)*nlp
       ENDDO
       DO i=nproc-nrest+1,nproc
          mfirst(i) = mlast(i-1) + 1
          mlast(i) = mfirst(i) + nlp
          npoints(i) = imaxperj(1)*(nlp+1)
       ENDDO

    ENDIF
   
    pointsinproc(0:maxnodes-1)=npoints(1:maxnodes)
    myfirstlat = mfirst(myid+1)
    mylastlat = mlast(myid+1)
    jMaxlocal = 0
    DO k=0,maxNodes-1
       firstlatinproc(k) = mfirst(k+1)
       lastlatinproc(k) = mlast(k+1)
       nlatsinproc(k) = mlast(k+1)-mfirst(k+1)+1
       DO j=firstlatinproc(k),lastlatinproc(k)
          nodeHasJ(j) = k
       ENDDO
    ENDDO
    jMaxlocal = MAXVAL(nlatsinproc)
    IF (jovlap.gt.MINVAL(nlatsinproc)) THEN
       STOP "number of latitudes per proc too small for jovlap"
    END IF

    ALLOCATE (jMinPerM(mMax))
    ALLOCATE (jMaxPerM(mMax))
    jMinPerM = jMaxHalf
    DO j = 1, jMaxHalf
       m = mMaxPerJ(j)
       jMinPerM(1:m) = MIN(j, jMinPerM(1:m))
    END DO
    jMaxPerM = jMax - jMinPerM + 1

    ! # longitudes per block

    nTrd = 1
    !$ nTrd = OMP_GET_MAX_THREADS()
    MinLatPerBlk = 1
    !MinLatPerBlk = MIN(1024/iMax, CEILING(REAL(jMax)/REAL(nTrd)))
    !MinLatPerBlk = CEILING(REAL(jMax)/REAL(nTrd))
    ibMax = MinLatPerBlk*iMax

    ! # blocks

    jbMax = 1
    cnt = 0
    DO j = myfirstlat,mylastlat
       IF (cnt + iMaxPerJ(j) > ibMax) THEN
          jbMax = jbMax + 1
          cnt = 0
       END IF
       cnt = cnt + iMaxPerJ(j)
    END DO

    ! maps (i,j) into (ib,jb) and vice-versa

    ALLOCATE (ibPerIJ(-1:iMax+2 ,-1:jMax+2 ))
    ibPerIJ=-1
    ALLOCATE (jbPerIJ(-1:iMax+2 ,-1:jMax+2 ))
    jbPerIJ=-1
    ALLOCATE (iPerIJB(ibMax,jbMax))
    iPerIJB=-1
    ALLOCATE (jPerIJB(ibMax,jbMax))
    jPerIJB=-1
    ALLOCATE (ibMaxPerJB(jbMax))
    ibMaxPerJB=-1

    ! compute local domain first   

    jb = 1
    ib = 0
    DO j = myfirstlat,mylastlat
       IF (ib + iMaxPerJ(j) > ibMax) THEN
          jb = jb + 1
          ib = 0
       END IF
       ibPerIJ(   1:   iMaxPerJ(j),  j) = (/ (i, i=ib+1,ib+iMaxPerJ(j)) /)
       jbPerIJ(   1:   iMaxPerJ(j),  j) = jb
       iPerIJB(ib+1:ib+iMaxPerJ(j), jb) = (/ (i, i=1,iMaxPerJ(j)) /)
       jPerIJB(ib+1:ib+iMaxPerJ(j), jb) = j
       ib = ib + iMaxPerJ(j)
       ibMaxPerJB(jb) = ib
    END DO

    ! compute extensions for overlapping area
    jbminus = 1
    jbplus = jbmax

    IF (jovlap.gt.0) THEN
       jb = 0
       done = .FALSE.
       jlast = myfirstlat-1
       jfirst = jlast + 1
       DO
          IF (done) EXIT
          done = .TRUE.
          ib = 0
          DO jk = jlast,MAX(myfirstlat-jovlap,1),-1
             IF (ib + iMaxPerJ(jk).le. ibMax) THEN
                done = .TRUE.
                jfirst = jk
                ib = ib + iMaxPerJ(jk)
               ELSE
                ib = 0
                DO j=jfirst,jlast
                   ibPerIJ(1:iMaxPerJ(j),  j) = (/ (i, i=ib+1,ib+iMaxPerJ(j)) /)
                   jbPerIJ(1:iMaxPerJ(j),  j) = jb
                   ib = ib + iMaxPerJ(j)
                END DO
                ib = 0
                jb = jb - 1
                jlast = jfirst - 1
                jfirst = jlast
                done = .FALSE.
                EXIT
             END IF
          END DO
       END DO
       ib = 0
       DO j=jfirst,jlast
          ibPerIJ(1:iMaxPerJ(j),  j) = (/ (i, i=ib+1,ib+iMaxPerJ(j)) /)
          jbPerIJ(1:iMaxPerJ(j),  j) = jb
          ib = ib + iMaxPerJ(j)
          jbminus = jb
       END DO
       jb = jbmax + 1
       ib = 0
       jfirst = mylastlat+1
       jlast = jfirst - 1
       DO j=mylastlat+1,MIN(mylastlat+jovlap,jmax)
          IF (ib + iMaxPerJ(j) > ibMax) THEN
             jb = jb + 1
             ib = 0
          END IF
          ibPerIJ(   1:   iMaxPerJ(j),  j) = (/ (i, i=ib+1,ib+iMaxPerJ(j)) /) 
          jbPerIJ(   1:   iMaxPerJ(j),  j) = jb
          ib = ib + iMaxPerJ(j)
       END DO
       jbplus = jb

    ENDIF

    ! Extensions to use in interpolation

    jh = max(2,jovlap)
    IF (-1.ge.myfirstlat-jh) THEN
       jbPerIJ(   1:   iMaxPerJ(2), -1) =  jbPerIJ(1,2)
       imp2 = iMaxPerJ(2) / 2
       ibPerIJ(   1:   imp2, -1) =  ibPerIJ(imp2+1:iMaxPerJ(2),2)
       ibPerIJ(imp2+1:iMaxPerJ(2), -1) =  ibPerIJ(1:imp2,2)
    ENDIF 
          
    IF (0.ge.myfirstlat-jh) THEN
       jbPerIJ(   1:   iMaxPerJ(1),  0) =  jbPerIJ(1,1)
       imp2 = iMaxPerJ(1) / 2
       ibPerIJ(   1:   imp2, 0) =  ibPerIJ(imp2+1:iMaxPerJ(1),1)
       ibPerIJ(imp2+1:iMaxPerJ(1), 0) =  ibPerIJ(1:imp2,1)
    ENDIF 

    IF (jmax+1.le.mylastlat+jh) THEN
       jbPerIJ(   1:   iMaxPerJ(jmax),  jmax+1) =  jbPerIJ(1,jmax)
       imp2 = iMaxPerJ(jmax) / 2
       ibPerIJ(   1:   imp2, jmax+1) =  ibPerIJ(imp2+1:iMaxPerJ(jmax),jmax)
       ibPerIJ(imp2+1:iMaxPerJ(jmax),jmax+1) =  ibPerIJ(1:imp2,jmax)
    ENDIF 
   
    IF (jmax+2.le.mylastlat+jh) THEN
       jbPerIJ(   1:   iMaxPerJ(jmax-1),  jmax+2) =  jbPerIJ(1,jmax-1)
       imp2 = iMaxPerJ(jmax-1) / 2
       ibPerIJ(   1:   imp2, jmax+2) =  ibPerIJ(imp2+1:iMaxPerJ(jmax-1),jmax-1)
       ibPerIJ(imp2+1:iMaxPerJ(jmax-1),jmax+2) =  ibPerIJ(1:imp2,jmax-1)
    ENDIF 


    DO j = MAX(-1,myfirstlat-jovlap),MIN(mylastlat+jovlap,jmax+2)
       jb = j
       if (j.eq.-1) jb =2
       if (j.eq.0) jb =1
       if (j.eq.jmax+1) jb =jmax
       if (j.eq.jmax+2) jb =jmax-1
       ib = iMaxPerJ(jb)
       ibPerIJ(   0,  j) = ibPerIJ(ib,j)
       ibPerIJ(  -1,  j) = ibPerIJ(ib-1,j)
       ibPerIJ(ib+1,j) = ibPerIJ(   1,  j)
       ibPerIJ(ib+2,j) = ibPerIJ(   2,  j)
       jbPerIJ(   0,  j) = jbPerIJ(ib,j)
       jbPerIJ(  -1,  j) = jbPerIJ(ib-1,j)
       jbPerIJ(ib+1,j) = jbPerIJ(   1,  j)
       jbPerIJ(ib+2,j) = jbPerIJ(   2,  j)
    END DO


    ALLOCATE(jFirstLJB(JBMax))
    ALLOCATE(jLastLJB(JBMax))
    DO jb = 1, jbMax
       JLastLJB (jb) = jPerIJB(ibMaxPerJB(jb),jb) 
       JFirstLJB(jb) = jPerIJB(1,jb)
    END DO

    ! Local index bounds:
    ! myJMax: how many j at this process

    myJMax = mylastlat - myfirstlat + 1
    ! OpenMP parallelism

    IF (dumpLocal) THEN
       WRITE(nfprt,"(a,' Dump at the end ')") h
       CALL DumpSizes()
    END IF
  END SUBROUTINE RegisterOtherSizes






  SUBROUTINE DumpSizes()
    CHARACTER(LEN=*), PARAMETER :: h="**(DumpSizes)**"
    CHARACTER(LEN=10) :: c1, c2, c3, c4, c5, c6
    LOGICAL :: Mask(jMaxHalf)
    INTEGER :: first(1)
    INTEGER :: i, j, jj, lastjj, jb, m
    INTEGER :: firstLat, lastLat, firstM, lastM, firstJ, lastJ, lastIB
    IF (mMax == -1) THEN
       WRITE(nfprt,"(a,' Sizes not created')") h
    ELSE
       WRITE(c1,"(i10)") mMax
       WRITE(c2,"(i10)") nMax
       WRITE(c3,"(i10)") nExtMax
       WRITE(c4,"(i10)") mnExtMax
       WRITE(nfprt,"(a,' mMax=',a,'; nMax=',a,'; nExtMax=',a,'; mnExtMax=',a)") &
            h, TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2)), TRIM(ADJUSTL(c3)), &
            TRIM(ADJUSTL(c4))
       WRITE(c1,"(i10)") jMax
       WRITE(c2,"(i10)") jMaxHalf
       WRITE(nfprt,"(a,' jMax=',a,'; jMaxHalf=',a)") &
            h, TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2))
       WRITE(c1,"(i10)") iMax
       WRITE(c2,"(i10)") kMax
       WRITE(c3,"(i10)") ijMax
       IF (ijMax == -1) THEN
          WRITE(nfprt,"(a,' iMax=',a,'; kMax=',a)") h, &
               TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2))
          WRITE(nfprt,"(a,' Sizes not fully created')") h
       ELSE
          WRITE(nfprt,"(a,' iMax=',a,'; kMax=',a,'; ijMax=',a)") h, &
               TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2)), TRIM(ADJUSTL(c3))
          WRITE(nfprt,"(' latitudes   longitudes   m')")
          DO i = MINVAL(iMaxPerJ), MAXVAL(iMaxPerJ)-1
             Mask = iMaxPerJ(1:jMaxHalf) == i
             IF (ANY(Mask)) THEN
                first = MINLOC(iMaxPerJ(1:jMaxHalf), Mask)
                firstLat = first(1)
                lastLat = firstLat + COUNT(Mask) - 1
                WRITE(nfprt,"(i5,':',i3,5x,i5,6x,'0:',i3)") &
                     firstLat, lastLat, i, mMaxPerJ(lastLat)-1
             END IF
          END DO
          i = MAXVAL(iMaxPerJ)
          first = MINLOC(iMaxPerJ, iMaxPerJ==i)
          firstLat = first(1)
          lastLat = jMax-firstLat+1
          WRITE(nfprt,"(i5,':',i3,5x,i5,6x,'0:',i3)") &
               firstLat, lastLat, i, mMaxPerJ(lastLat)-1
          DO i = MAXVAL(iMaxPerJ)-1, MINVAL(iMaxPerJ), -1
             Mask = iMaxPerJ(jMaxHalf+1:jMax) == i
             IF (ANY(Mask)) THEN
                first = MINLOC(iMaxPerJ(jMaxHalf+1:jMax), Mask)
                firstLat = first(1) + jMaxHalf
                lastLat = firstLat + COUNT(Mask) - 1
                WRITE(nfprt,"(i5,':',i3,5x,i5,6x,'0:',i3)") &
                     firstLat, lastLat, i, mMaxPerJ(lastLat)-1
             END IF
          END DO


          WRITE(nfprt,"('     m       latitudes')")

          lastM = 0
          DO
             IF (lastM == mMax) THEN
                EXIT
             ELSE
                firstM=lastM+1
                lastM=firstM
                firstJ=jMinPerM(firstM)
                lastJ=jMaxPerM(firstM)
                DO m = firstM+1, mMax
                   IF ((firstJ == jMinPerM(m)) .AND. &
                        (lastJ == jMaxPerM(m))) THEN
                      lastM = m
                   ELSE
                      EXIT
                   END IF
                END DO
                WRITE(nfprt,"(i5,':',i3,5x,i3,':',i3)") &
                     firstM-1, lastM-1, firstJ, lastJ
             END IF
          END DO

          WRITE(c1,"(i10)") ibMax
          WRITE(c2,"(i10)") jbMax
          WRITE(c3,"(i10)") jbMax*ibMax
          WRITE(nfprt,"(a,' ibMax=',a,'; jbMax=',a,'; ijbMax=',a)") h, &
               TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2)), TRIM(ADJUSTL(c3))
          DO j = 1, jMax
             IF (jbPerIJ(1,j) /= jbPerIJ(iMaxPerJ(j),j)) THEN
                STOP "error in mapping jbPerIJ"
             END IF
             WRITE(c1,"(i10)") 1
             WRITE(c2,"(i10)") iMaxPerJ(j)
             WRITE(c3,"(i10)") j
             WRITE(c4,"(i10)") ibPerIJ(1,j)
             WRITE(c5,"(i10)") ibPerIJ(iMaxPerJ(j),j)
             WRITE(c6,"(i10)") jbPerIJ(1,j)
             WRITE(nfprt,"(a,'(Long,Lat)=(',a,':',a,',',a,') &
                  &mapped into(ib,jb)=(',&
                  &a,':',a,',',a,')')") h, &
                  TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2)), &
                  TRIM(ADJUSTL(c3)), TRIM(ADJUSTL(c4)), &
                  TRIM(ADJUSTL(c5)), TRIM(ADJUSTL(c6))
          END DO


          DO jb = 1, jbMax
             lastIb = ibMaxPerJB(jb)
             WRITE(c1,"(i10)") 1
             WRITE(c2,"(i10)") lastIb
             WRITE(c3,"(i10)") jb
             WRITE(nfprt,"(a,'(ib,jb)=(',a,':',a,',',a,') maps (Long,Lat)=')") &
                  h, TRIM(ADJUSTL(c1)), TRIM(ADJUSTL(c2)), TRIM(ADJUSTL(c3))
             firstJ = jPerIJB(1,jb)
             lastJ  = jPerIJB(lastIb,jb)
             DO jj = firstJ, lastJ, 5
                lastjj = MIN(jj+4,lastJ)
                DO j = jj, lastjj
                   WRITE(c4,"(i10)") 1
                   WRITE(c5,"(i10)") iMaxPerJ(j)
                   WRITE(c6,"(i10)") j
                   WRITE(nfprt,"('(',a,':',a,',',a,')')",ADVANCE='no') &
                        TRIM(ADJUSTL(c4)), TRIM(ADJUSTL(c5)), TRIM(ADJUSTL(c6))
                   IF (j /= lastjj) THEN
                      WRITE(nfprt,"(',')", ADVANCE='no')
                   ELSE IF (j /= lastj) THEN
                      WRITE(nfprt,"(',')")
                   ELSE
                      WRITE(nfprt,"('')")
                   END IF
                END DO
             END DO
          END DO
       END IF
    END IF
  END SUBROUTINE DumpSizes


  SUBROUTINE MPIMappings(myId,maxNodes)
    INTEGER, INTENT(IN) :: myId
    INTEGER, INTENT(IN) :: maxNodes

    INTEGER :: m
    INTEGER :: n
    INTEGER :: mn
    INTEGER :: mBase
    INTEGER :: mMid
    INTEGER :: lm
    INTEGER :: mm
    CHARACTER(LEN=*), PARAMETER :: h="**(MPIMappings)**"

    ! how many MPI processes and who am i (hardwired for a single node)

    MMaxlocal = mMax / maxNodes
    IF (mMax-MMaxlocal*maxNodes.ne.0) MMaxlocal = MMaxlocal + 1


    ! Spectral Domain decomposition: 
    ! Which process has a particular value of m.
    ! Values of nodeHasM for p processes are:
    ! (0, 1, 2, ..., p-1, p-1, ..., 2, 1, 0, 0, 1, 2, ...)
    ! This distribution tries to spread evenly load across
    ! processes, leaving m with smaller loads to the end.
    ! Mapping Local to Global spectral indices:
    ! map local to global index of m (lm2m) 


    ALLOCATE(Msinproc(MMaxlocal,0:maxNodes-1))
    ALLOCATE(Msperproc(0:maxNodes-1))
    ALLOCATE(nodeHasM(mMax))
    ALLOCATE(MMaxinproc(0:maxNodes-1))
    ALLOCATE(mnmaxinproc(0:maxNodes-1))
    ALLOCATE(mnextmaxinproc(0:maxNodes-1))
    mm = 1
    Msperproc = 0
    mnmaxinproc = 0
    mnextmaxinproc = 0
    DO mBase = 1, mMax, 2*maxNodes
       mMid = mBase+maxNodes-1
       DO m = mBase, MIN(mMid, mMax)
          nodeHasM(m) = m - mBase
          Msperproc(nodeHasM(m)) = Msperproc(nodeHasM(m)) + 1
          Msinproc(mm,nodeHasM(m)) = m
          mnmaxinproc(nodeHasM(m)) =  mnmaxinproc(nodeHasM(m)) + mmax - m + 1
          mnextmaxinproc(nodeHasM(m)) =  mnextmaxinproc(nodeHasM(m)) + mmax - m + 2
       END DO
       mm = mm + 1
       DO m = mMid+1, MIN(mMid+maxNodes, mMax)
          nodeHasM(m) = maxNodes + mMid - m
          Msperproc(nodeHasM(m)) = Msperproc(nodeHasM(m)) + 1
          Msinproc(mm,nodeHasM(m)) = m
          mnmaxinproc(nodeHasM(m)) =  mnmaxinproc(nodeHasM(m)) + mmax - m + 1
          mnextmaxinproc(nodeHasM(m)) =  mnextmaxinproc(nodeHasM(m)) + mmax - m + 2
       END DO
       mm = mm + 1
    END DO
    mnmaxlocal = MAXVAL(mnmaxinproc)
    mnextmaxlocal = MAXVAL(mnextmaxinproc)
    myMMax = COUNT(nodeHasM == myId)
    ALLOCATE(lm2m(myMMax))
    lm2m(1:mymmax) = Msinproc(1:mymmax,myid)

    ! Local spectral bounds:
    ! (1) how many values of m are stored at this MPI process
    ! (2) how many (m,n) are stored at this MPI process
    !     for regular and extended spectral representations
    !     (counting Real/Imaginary entries as a single entry)

    myMNMax = 0
    myMNExtMax = 0
    MMaxinproc = 0
    DO m = 1, mMax
       MMaxinproc(nodeHasM(m)) = MMaxinproc(nodeHasM(m)) + 1
       IF (nodeHasM(m) == myId) THEN
          myMNMax      = myMNMax    + mMax - m + 1
          myMNExtMax   = myMNExtMax + mMax - m + 2
       END IF
    END DO

    ! Mapping Local pairs (lm,n) to 1D for Extended Spectral:
    ! (1) myMExtMap(mn): which lm is stored at this position
    ! (2) myNExtMap(mn): which  n is stored at this position
    ! (3) myMNExtMap(lm,n): position storing pair (lm,n)

    ALLOCATE (myMExtMap(myMNExtMax))
    ALLOCATE (myNExtMap(myMNExtMax))
    ALLOCATE (myMNExtMap(myMMax,nExtMax))
    myMExtMap = -1  ! flag mapping error
    myNExtMap = -1  ! flag mapping error
    myMNExtMap = -1 ! flag mapping error
    mn = 0
    DO lm = 1, MymMax
       DO n = lm2m(lm), mMax+1
          mn = mn + 1
          myMNExtMap(lm,n) = mn
          myMExtMap(mn)    = lm
          myNExtMap(mn)    = n
       END DO
    END DO

    ! Mapping Local pairs (lm,n) to 1D for Regular Spectral:
    ! (1) myMMap(mn): which lm is stored at this position
    ! (2) myNMap(mn): which  n is stored at this position
    ! (3) myMNMap(lm,n): position storing pair (lm,n)

    ALLOCATE (myMNMap(myMMax,nMax))
    ALLOCATE (myMMap(myMNMax))
    ALLOCATE (myNMap(myMNMax))
    myMNMap = -1  ! flag mapping error
    myMMap = -1   ! flag mapping error
    myNMap = -1   ! flag mapping error
    mn = 0
    DO lm = 1, MymMax
       DO n = lm2m(lm), mMax
          mn = mn + 1
          myMNMap(lm,n) = mn
          myMMap(mn)    = lm
          myNMap(mn)    = n
       END DO
    END DO


    !   IF (dumpLocal) THEN
    !      CALL DumpMySize()
    !   END IF
  END SUBROUTINE MPIMappings

  SUBROUTINE ThreadDecomp(firstInd, lastInd, minInd, maxInd, msg)
    INTEGER, INTENT(IN ) :: firstInd
    INTEGER, INTENT(IN ) :: lastInd
    INTEGER, INTENT(OUT) :: minInd
    INTEGER, INTENT(OUT) :: maxInd
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER :: chunk
    INTEGER :: left
    INTEGER :: nTrd
    INTEGER :: iTrd
    INTEGER :: length
    LOGICAL :: inParallel
    CHARACTER(LEN=*), PARAMETER :: h="**(ThreadDecomp)**"
    !$ INTEGER, EXTERNAL :: OMP_GET_NUM_THREADS
    !$ INTEGER, EXTERNAL :: OMP_GET_THREAD_NUM
    !$ LOGICAL, EXTERNAL :: OMP_IN_PARALLEL

    inParallel = .FALSE.
    nTrd=1
    iTrd=0
    !$ inParallel = OMP_IN_PARALLEL()
    IF (inParallel) THEN
       !$ nTrd = OMP_GET_NUM_THREADS()
       !$ iTrd = OMP_GET_THREAD_NUM()
       length = lastInd - firstInd + 1
       chunk = length/nTrd
       left  = length - chunk*nTrd
       IF (iTrd < left) THEN
          minInd =  iTrd   *(chunk+1) + firstInd
          maxInd = (iTrd+1)*(chunk+1) + firstInd - 1
       ELSE
          minInd =  iTrd   *(chunk) + left + firstInd
          maxInd = (iTrd+1)*(chunk) + left + firstInd - 1
       END IF
    ELSE
       minInd = firstInd
       maxInd = lastInd
    END IF

    IF (dumpLocal) THEN
       IF (inParallel) THEN
          WRITE(nfprt,"(a,' thread ',i2,' got [',i8,':',i8,&
               &'] from [',i8,':',i8,'] in parallel region at ',a)") &
               h, iTrd, minInd, maxInd, firstInd, lastInd, msg
       ELSE
          WRITE(nfprt,"(a,' kept domain [',i8,':',i8,&
               &'] since not in parallel region at ',a)") &
               h, minInd, maxInd, msg
       END IF
    END IF
  END SUBROUTINE ThreadDecomp


  SUBROUTINE ThreadDecompms(m, myms, nms)
    INTEGER, INTENT(IN ) :: m 
    INTEGER, INTENT(INOUT) :: myms(m)
    INTEGER, INTENT(OUT) :: nms   
    INTEGER :: i, j, k
    INTEGER :: nTrd
    INTEGER :: iTrd
    LOGICAL :: inParallel
    !$ INTEGER, EXTERNAL :: OMP_GET_NUM_THREADS
    !$ INTEGER, EXTERNAL :: OMP_GET_THREAD_NUM
    !$ LOGICAL, EXTERNAL :: OMP_IN_PARALLEL

    inParallel = .FALSE.
    nTrd=1
    iTrd=0
    !$ inParallel = OMP_IN_PARALLEL()
    IF (inParallel) THEN
       !$ nTrd = OMP_GET_NUM_THREADS()
       !$ iTrd = OMP_GET_THREAD_NUM() + 1
       nms = 0
       i = 1
       j = 1
       DO k=1,m
          IF (i.eq.iTrd) THEN
            nms = nms + 1
            myms(nms) = k
          ENDIF
          i = i + j
          IF (i.eq.nTrd+1) THEN
            j = -1
            i = nTrd
          ENDIF
          IF (i.eq.0) THEN
            j = 1
            i = 1
          ENDIF
       ENDDO
    ELSE
       DO k=1,m
          myms(k) = k
       ENDDO
       nms = m
    END IF

  END SUBROUTINE ThreadDecompms


  SUBROUTINE InitVerSizes (si_in, sl_in, del_in)

    REAL(KIND=r8), INTENT(IN) :: si_in(:)
    REAL(KIND=r8), INTENT(IN) :: sl_in(:)
    REAL(KIND=r8), INTENT(Out) :: del_in(:)

    INTEGER :: kMaxsl
    INTEGER :: k
    REAL(KIND=r8)    :: rk1, sirk, sirk1, dif

    kMaxsl = SIZE(sl_in)
    IF (kMaxsl /= kMax) STOP ' Error in InitVerSizes: kMaxsl=SIZE(sl) /= kMax '
    ALLOCATE(del(kMax))
    ALLOCATE(delcl(kMax-1))
    ALLOCATE(rdel2(kMax))
    ALLOCATE(ci(kMax+1))
    ALLOCATE(si(kMax+1))
    ALLOCATE(sl(kMax))
    ALLOCATE(cl(kMax))
    ALLOCATE(rpi(kMax-1))

    DO k=1, kMax
       del(k)=si_in(k)-si_in(k+1)
    END DO
    del_in=del
    rdel2 = 0.5_r8/del

    rk1 = rk + 1.0_r8

    !cdir novector
    ci(1) = 0.0_r8
    DO k=1, kMax-1
       ci(k+1)=ci(k)+del(k)
    END DO
    ci(kMax+1)=1.0_r8

    DO k=1, kMax+1
       si(k) = 1.0_r8 - ci(k)
    END DO

    DO k=1, kMax
       sirk =EXP(rk1*LOG(si(k)))
       IF(k.LE.kMax-1) THEN
          sirk1=EXP(rk1*LOG(si(k+1)))
       ELSE
          sirk1=0.0_r8
       END IF
       dif = sirk-sirk1
       dif = dif / (rk1*(si(k)-si(k+1)))
       sl(k) = EXP(LOG(dif)/rk)
       cl(k) = 1.0_r8 - sl(k)
    END DO
    DO k = 1, kMax-1
       delcl(k) = cl(k+1) - cl(k)
    END DO
    !     
    !     compute pi ratios for temp. matrix.
    !
    DO k=1, kMax-1
       rpi(k) = EXP(rk*LOG(sl(k+1)/sl(k)))
    END DO

  END SUBROUTINE InitVerSizes


END MODULE Sizes
