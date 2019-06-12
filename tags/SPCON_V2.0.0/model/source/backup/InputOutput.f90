!
!  $Author: pkubota $
!  $Date: 2010/10/21 10:46:22 $
!  $Revision: 1.17 $
!
MODULE InputOutput

  USE Parallelism, ONLY: &
       myId,             &
       myId_four,        &
       maxNodes

  USE Constants, ONLY: &
       r4,i4, r8, i8, ngrmx, numx, ncf, ncf2

  USE IOLowLevel, ONLY: &
       ReadHead     , &
       GReadHead    , &
       ReadField    , &
       GReadField   , &
       WriteHead    , &
       GWriteHead   , &
       WriteField   , &
       GWriteField  , &
       ReadGetALB   , &
       ReadGetSST   , &
       ReadGetSLM   , &
       ReadGetSNW   , &
       ReadGetSST2  , &
       ReadOzone

  USE Options, ONLY: &
       nfprt, &
       nfctrl, &
       nfsst, &
       nfsnw, &
       nfalb, &
       nfslm, &
       nfauntbl, &
       nfcnftbl, &
       nfcnf2tb, &
       nflooktb, &
       reducedGrid,&
       labelsi,&
       labelsj,&
       ifco2, ifozone, & !hmjb
       nfco2, nfozone, & !hmjb
       co2val, &             !hmjb for new co2 values
       fNameSnow  , &
       fNameSSTAOI, &
       fNameSoilms, &
       fNameAlbedo, &
       fNameCO2   , &
       fNameOzone

  USE Utils, ONLY: &
       IJtoIBJB      ,&
       AveBoxIJtoIBJB,&
       NearestIJtoIBJB

  USE Sizes, ONLY: &
       mymnmax,    &
       myjmax_d,   &
       imax,       &
       jmax,       &
       kmaxloc,    &
       myfirstlev, &
       mylastlev,  &
       mymmax,     &
       msinproc,   &
       mnmap,      &
       mymnmap


  USE Communications, ONLY: &
       Collect_Spec,        &
       Collect_Grid_Sur,    &
       Collect_Grid_d

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: InitInputOutput
  PUBLIC :: cnvray
  PUBLIC :: scloutsp
  PUBLIC :: scloutgr
  PUBLIC :: WillGetSbc
  PUBLIC :: getsbc
  PUBLIC :: gread
  PUBLIC :: gread4
  PUBLIC :: gwrite
  PUBLIC :: fsbc
  PUBLIC :: aunits

  INTEGER              :: mMax
  INTEGER              :: nMax
  INTEGER              :: mnMax
  INTEGER              :: kMax

  LOGICAL              :: fsbc
  CHARACTER(LEN=100)   :: path

  CHARACTER(LEN=16), ALLOCATABLE :: aunits(:)
  INTEGER,           ALLOCATABLE :: looku (:,:,:)
  REAL(KIND=r8),              ALLOCATABLE :: cnfac (:)
  REAL(KIND=r8),              ALLOCATABLE :: cnfac2(:)

  REAL(KIND=r8),    PARAMETER   :: undef =1.0e53_r8


CONTAINS



  ! InitInputOutput: Initializes module

  SUBROUTINE InitInputOutput ( &
       mMax_in, nMax_in, mnMax_in, kmax_in, &
       path_in, fNameCnfTbl, &
       fNameCnf2Tb, fNameLookTb, fNameUnitTb)

    INTEGER,          INTENT(IN) :: mMax_in
    INTEGER,          INTENT(IN) :: nMax_in
    INTEGER,          INTENT(IN) :: mnMax_in
    INTEGER,          INTENT(IN) :: kmax_in
    CHARACTER(LEN=*), INTENT(IN) :: path_in
    CHARACTER(LEN=*), INTENT(IN) :: fNameCnfTbl
    CHARACTER(LEN=*), INTENT(IN) :: fNameCnf2Tb
    CHARACTER(LEN=*), INTENT(IN) :: fNameLookTb
    CHARACTER(LEN=*), INTENT(IN) :: fNameUnitTb
    INTEGER :: ierr


    OPEN(UNIT=nfauntbl, FILE=TRIM(fNameUnitTb), FORM='formatted',ACCESS='sequential',&
         ACTION='read', STATUS='old', IOSTAT=ierr)
    IF (ierr /= 0) THEN
       WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
            TRIM(fNameUnitTb), ierr
       STOP "**(ERROR)**"
    END IF

    OPEN(UNIT=nfcnftbl, FILE=TRIM(fNameCnfTbl), FORM='formatted',ACCESS='sequential',&
         ACTION='read', STATUS='old', IOSTAT=ierr)
    IF (ierr /= 0) THEN
       WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
            TRIM(fNameCnfTbl), ierr
       STOP "**(ERROR)**"
    END IF

    OPEN(UNIT=nfcnf2tb, FILE=TRIM(fNameCnf2Tb), FORM='formatted',ACCESS='sequential',&
         ACTION='read', STATUS='old', IOSTAT=ierr)
    IF (ierr /= 0) THEN
       WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
            TRIM(fNameCnf2Tb), ierr
       STOP "**(ERROR)**"
    END IF

    OPEN(UNIT=nflooktb, FILE=TRIM(fNameLookTb), FORM='formatted',ACCESS='sequential',&
         ACTION='read', STATUS='old', IOSTAT=ierr)
    IF (ierr /= 0) THEN
       WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
            TRIM(fNameLookTb), ierr
       STOP "**(ERROR)**"
    END IF

    path  = path_in
    mMax  = mMax_in
    nMax  = nMax_in
    mnMax = mnMax_in
    kmax  = kmax_in

    ALLOCATE(aunits(-1:numx))
    REWIND nfauntbl
    READ(UNIT=nfauntbl,FMT="(A16)") aunits
    REWIND nfauntbl
    CLOSE(UNIT=nfauntbl,status='KEEP')

    ALLOCATE(cnfac(ncf))
    REWIND nfcnftbl
    READ(UNIT=nfcnftbl,FMT="(5e16.8)") cnfac
    REWIND nfcnftbl
    CLOSE(UNIT=nfcnftbl,status='KEEP')

    ALLOCATE(cnfac2(ncf2))
    REWIND nfcnf2tb
    READ(UNIT=nfcnf2tb,FMT="(5e16.8)") cnfac2
    REWIND nfcnf2tb
    CLOSE(UNIT=nfcnf2tb,status='KEEP')

    ALLOCATE(looku(0:9,0:9,0:ngrmx))
    REWIND nflooktb
    READ(UNIT=nflooktb,FMT="(20i4)") looku
    REWIND nflooktb
    CLOSE(UNIT=nflooktb,status='KEEP')

  END SUBROUTINE InitInputOutput



  ! cnvray: convert array



  SUBROUTINE cnvray (array, idim, ifr, ito)
    INTEGER, INTENT(IN   ) :: idim
    REAL(KIND=r8),    INTENT(INOUT) :: array(idim)
    INTEGER, INTENT(IN   ) :: ifr
    INTEGER, INTENT(IN   ) :: ito

    CHARACTER(LEN=20) :: c0
    CHARACTER(LEN=20) :: c1
    INTEGER           :: i
    INTEGER           :: icf
    INTEGER           :: igpf
    INTEGER           :: iuf
    INTEGER           :: igpt
    INTEGER           :: iut
    REAL(KIND=r8)              :: cf
    REAL(KIND=r8)              :: cf2

    CHARACTER(LEN=*), PARAMETER :: h="**(cnvray)**"

    ! consistency

    IF (idim.eq.0) RETURN

    IF (ifr <= -1) THEN
       WRITE(c0,"(i20)") ifr
       WRITE(UNIT=nfprt,FMT="(a)") h//" ERROR: ifr ("//TRIM(ADJUSTL(c0))//") <= -1 "
       STOP h
    ELSE IF (ito <= -1) THEN
       WRITE(c0,"(i20)") ito
       WRITE(UNIT=nfprt,FMT="(a)") h//" ERROR: ito ("//TRIM(ADJUSTL(c0))//") <= -1 "
       STOP h
    ELSE IF (idim < 0) THEN
       WRITE(c0,"(i20)") idim
       WRITE(UNIT=nfprt,FMT="(a)") h//" ERROR: idim ("//TRIM(ADJUSTL(c0))//") < 0 "
       STOP h
    ELSE IF (ito /= ifr) THEN
       igpf=ifr/10
       igpt=ito/10
       IF (igpf /= igpt) THEN
          WRITE(c0,"(i20)") igpf
          WRITE(c1,"(i20)") igpt
          WRITE(UNIT=nfprt,FMT="(a)") h//" ERROR: igpf ("//TRIM(ADJUSTL(c0))//&
               &") /= igpt ("//TRIM(ADJUSTL(c1))//")"
          STOP h
       ELSE IF (igpf > ngrmx) THEN
          WRITE(c0,"(i20)") igpf
          WRITE(c1,"(i20)") ngrmx
          WRITE(UNIT=nfprt,FMT="(a)") h//" ERROR: igpf ("//TRIM(ADJUSTL(c0))//&
               &") > ngrmx ("//TRIM(ADJUSTL(c1))//")"
          STOP h
       ELSE

          ! table look-up

          iuf=MOD(ifr,10)
          iut=MOD(ito,10)
          icf=looku(iuf,iut,igpf)

          ! consistency, again

          IF (icf < 1 .OR. icf > ncf) THEN
             WRITE(c0,"(i20)") icf
             WRITE(c1,"(i20)") ncf
             WRITE(UNIT=nfprt,FMT="(a)") h//" ERROR: icf ("//TRIM(ADJUSTL(c0))//&
                  &") < 1 or > ncf ("//TRIM(ADJUSTL(c1))//")"
             STOP h
          END IF

          ! get coeficients

          cf=cnfac(icf)
          IF (icf <= ncf2) THEN
             cf2=cnfac2(icf)
          ELSE
             cf2=0.0_r8
          END IF

          ! convert array

          DO i = 1, idim
             IF (array(i) /= undef) THEN
                array(i)=cf*array(i)+cf2
             END IF
          END DO
       END IF
    END IF
  END SUBROUTINE cnvray


  !
  ! scale, convert to 32 bits and output field
  !
  SUBROUTINE scloutsp(unit, field, levs, levsg, fact1, nufr, nuto)
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(IN) :: levs
    INTEGER, INTENT(IN) :: levsg
    REAL(KIND=r8),    INTENT(IN) :: field(2*mymnmax,levs)
    REAL(KIND=r8),    INTENT(IN) :: fact1
    INTEGER, INTENT(IN) :: nufr
    INTEGER, INTENT(IN) :: nuto
    REAL(KIND=r8) :: fldaux(2*mymnmax,levs), fout(2*mnmax,levsg)

    fldaux = fact1 * field

    CALL cnvray(fldaux,2*levs*mymnmax,nufr,nuto)

    IF (Maxnodes.eq.1) THEN

       CALL WriteField(unit, fldaux)

    ELSE
       CALL Collect_Spec(fldaux, fout, levs, levsg, 0)
       IF (myid.eq.0) CALL WriteField(unit, fout)
    ENDIF



  END SUBROUTINE scloutsp




  SUBROUTINE scloutgr(unit, field, levs, fact1, nufr, nuto)
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(IN) :: levs
    REAL(KIND=r8),    INTENT(IN) :: field(imax*myjmax_d,levs)
    REAL(KIND=r8),    INTENT(IN) :: fact1
    INTEGER, INTENT(IN) :: nufr
    INTEGER, INTENT(IN) :: nuto
    REAL(KIND=r8) :: fldaux(imax*myjmax_d,levs), fout(imax*jmax,levs)

    fldaux = fact1 * field

    CALL cnvray(fldaux,imax*myjmax_d*levs,nufr,nuto)

    IF (Maxnodes.eq.1) THEN

       CALL WriteField(unit, fldaux)

    ELSE
       IF(levs.eq.1) THEN
          CALL Collect_Grid_Sur(fldaux, fout, 0)
       ELSE
          CALL Collect_Grid_d(fldaux, fout, levs, 0)
       ENDIF
       IF(myid.eq.0) CALL WriteField(unit, fout)

    ENDIF



  END SUBROUTINE scloutgr




  LOGICAL FUNCTION WillGetSbc(idate, tod, fint)
    INTEGER, INTENT(IN) :: idate(4)
    REAL(KIND=r8),    INTENT(IN) :: tod
    REAL(KIND=r8),    INTENT(IN) :: fint
    REAL(KIND=r8)                :: fhr

    WillGetSbc = .TRUE.
    IF (fint > 0.0_r8) THEN
       fhr=REAL(idate(1),r8)+tod/3600.0_r8+1.0e-3_r8
       WillGetSbc = fsbc .OR. ABS( MOD(fhr,fint)) <= 1.0e-2_r8
    END IF
  END FUNCTION WillGetSbc
  !
  ! getsbc :read surface/atmosphere boundary conditions.
  !
  SUBROUTINE getsbc (imax,jmax,kmax,galb ,gsst ,gslm,gsnw,gozo,&
       ifday,tod,idate,idatec,&
       ifalb,ifsst,ifslm,ifsnw,ifozone,&
       sstlag,intsst,fint,tice,&
       yrl ,monl,ibMax,jbMax,ibMaxPerJB)
    IMPLICIT NONE
    !
    ! INPUT/OUTPUT VARIABLES
    !
    ! Real size of the grid
    INTEGER, INTENT(in   ) :: imax
    INTEGER, INTENT(in   ) :: jmax
    INTEGER, INTENT(in   ) :: kmax
    ! Size of block divided grid
    INTEGER, INTENT(in   ) :: ibMax
    INTEGER, INTENT(in   ) :: jbMax
    INTEGER, INTENT(in   ) :: ibMaxPerJB(:)

    ! Boundary fields output
    REAL(KIND=r8), INTENT(out  ) :: galb(ibMax,jbMax) ! albedo
    REAL(KIND=r8), INTENT(out  ) :: gsst(ibMax,jbMax) ! sst
    REAL(KIND=r8), INTENT(out  ) :: gslm(ibMax,jbMax) ! soil moisture
    REAL(KIND=r8), INTENT(out  ) :: gsnw(ibMax,jbMax) ! snow
    !hmjb o ozonio nao pode ser apenas 'out' pois, no caso de usar a antiga
    !  getoz(), ele sairia daqui com valores indefinidos... Com inout,
    !  ele entra e,  se nao for alterado, sai como entrou
    REAL(KIND=r8), INTENT(inout) :: gozo(ibMax,kMax,jbMax) ! ozone

    ! Options for reading boundary fields
    INTEGER, INTENT(inout) :: ifalb
    INTEGER, INTENT(inout) :: ifsst
    INTEGER, INTENT(inout) :: ifslm
    INTEGER, INTENT(inout) :: ifsnw
    INTEGER, INTENT(inout) :: ifozone

    ! Time
    INTEGER, INTENT(in   ) :: ifday
    REAL(KIND=r8), INTENT(in   ) :: tod
    INTEGER, INTENT(in   ) :: idate(4)
    INTEGER, INTENT(in   ) :: idatec(4)
    REAL(KIND=r8), INTENT(in   ) :: sstlag
    INTEGER, INTENT(in   ) :: intsst
    REAL(KIND=r8), INTENT(in   ) :: fint
    REAL(KIND=r8), INTENT(in   ) :: tice
    REAL(KIND=r8), INTENT(in   ) :: yrl
    INTEGER, INTENT(in   ) :: monl(12)
    !
    ! LOCAL VARIABLES
    !
    REAL(KIND=r8)                :: xsst    (ibMax,jbMax)
    REAL(KIND=r8)                :: bfr_in  (imax,jmax)
    REAL(KIND=r8)                :: bfr_in3 (imax,kmax,jmax)
    REAL(KIND=r8)                :: bfr_out (ibMax,jbMax)
    REAL(KIND=r8)                :: bfr_out3(ibMax,kmax,jbMax)
    REAL(KIND=r4)                :: rbrf    (iMax,jMax)

    !
    !
    INTEGER                :: lrecl,LRecIn
    REAL(KIND=r8)          :: fhr
    INTEGER                :: mf
    INTEGER                :: mn
    INTEGER                :: month
    INTEGER                :: mm
    INTEGER                :: i
    INTEGER                :: j
    INTEGER                :: k
    INTEGER                :: irec
    REAL(KIND=r8)                :: f1
    REAL(KIND=r8)                :: f2
    REAL(KIND=r8)                :: gmax
    REAL(KIND=r8)                :: gmin
    REAL(KIND=r8)                :: fsst
    REAL(KIND=r8)                :: fisst
    REAL(KIND=r8)                :: xx1
    REAL(KIND=r8)                :: xx2
    REAL(KIND=r8)                :: xday
    INTEGER :: ierr
    !
    !   ifxxx=0    xxx is not processed
    !   ifxxx=1    xxx is set to month=idate(2) in the first call,
    !              but not processed from the subsequent calls.
    !              ifxxx is set to zero after interpolation
    !   ifxxx=2    xxx is interpolated to current day and time every fint
    !              hours synchronized to 00z regardless of initial time.
    !              interpolation is continuous (every time step) if fint<0.
    !   ifxxx=3    xxx is interpolated to current day and time when ifday=0
    !              and tod=0.0 but not processed otherwise
    !              ( appropriate only when xxx is predicted )
    !
    !              the following are for sst only (fint applies as in
    !              ifxxx=2):
    !   ifsst=4    sst is linearly interpolated from continuous direct
    !              access data set to current day and time.  data set
    !              is assumed to be spaced every intsst days or every
    !              calendar month is intsst < 0.
    !   ifsst=5    sst is expanded from piecewise cubic coefficients in
    !              direct access data set to current day and time.  data set
    !              is assumed to be spaced every intsst days.
    !   note:      for ifsst=4 or 5 sstlag must be set.  sstlag is the
    !              number of days plus any fraction prior to the initial
    !              condition date and time the data set begins if intsst > 0
    !              sstlag is the number of whole months prior to the initial
    !              condition date the data set begins if intsst < 0.
    !
    !     ifsst=-1 for numerical weather forecasting using mean weekly sst:
    !              sst is read in the first call as the second record of
    !              the archieve but not processed from the subsequent calls.
    !
    rbrf = 0.0_r4
    IF (fint > 0.0_r8) THEN
       fhr=REAL(idate(1),r8)+tod/3600.0_r8+1.0e-3_r8
       IF (.NOT. fsbc .AND. ABS( MOD(fhr,fint)) > 1.0e-2_r8) THEN
          RETURN
       END IF
    END IF
    IF (ifsst == 4 .AND. intsst <= 0) THEN
       CALL GetRecWgtMonthlySST &
            (idate, idatec, tod, labelsi, labelsj, &
            irec, f1, f2, mf, mn,monl)

!!$       WRITE (UNIT=nfprt, FMT='(A)') ' GetRecWgtMonthlySST'
!!$       WRITE (UNIT=nfprt, FMT='(/,4(A,I5),/)') &
!!$            ' reci = ', irec, ' recf = ', irec+1, &
!!$            ' mra = ', mf, ' mrb = ', mf+1
!!$       WRITE (UNIT=nfprt, FMT=*) ' fa  (*mra) = ', f1, ' fb  (*mrb) = ', f2
    ELSE
       CALL GetWeightsOld(yrl,monl,idatec, tod, f1, f2,mf)
!!$       WRITE (UNIT=nfprt, FMT=*) ' fa  (*mra) = ', f1, ' fb  (*mrb) = ', f2
    END IF
    !
    ! process albedo file
    !
    IF (ifalb /= 0) THEN
       IF (ifalb == 1) THEN
          month=idate(2)
          INQUIRE (IOLENGTH=LRecIn) rbrf
          OPEN (UNIT=nfalb,FILE=TRIM(fNameAlbedo),FORM='UNFORMATTED', ACCESS='DIRECT', RECL=LRecIn, &
               ACTION='read', STATUS='old', IOSTAT=ierr)
          IF (ierr /= 0) THEN
             WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                  TRIM(fNameAlbedo), ierr
             STOP "**(ERROR)**"
          END IF
          irec=month
          CALL ReadGetALB(nfalb,irec,bfr_in)

          IF (reducedGrid) THEN
             CALL AveBoxIJtoIBJB(bfr_in,galb)
          ELSE
             CALL IJtoIBJB(bfr_in ,galb)
          END IF
          CLOSE(UNIT=nfalb)
          ifalb=0
       ELSE IF (&
            (ifalb == 2) .OR. &
            (ifalb == 3 .AND. tod == 0.0_r8 .AND. ifday == 0)) THEN
          INQUIRE (IOLENGTH=LRecIn) rbrf
          OPEN (UNIT=nfalb,FILE=TRIM(fNameAlbedo),FORM='UNFORMATTED', ACCESS='DIRECT', RECL=LRecIn, &
               ACTION='read', STATUS='old', IOSTAT=ierr)
          IF (ierr /= 0) THEN
             WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                  TRIM(fNameAlbedo), ierr
             STOP "**(ERROR)**"
          END IF
          irec=mf
          CALL ReadGetALB(nfalb,irec,bfr_in)
          IF (reducedGrid) THEN
             CALL AveBoxIJtoIBJB(bfr_in,galb)
          ELSE
             CALL IJtoIBJB(bfr_in ,galb)
          END IF
          IF (irec == 12) THEN
             irec=1
          ELSE   
             irec=irec+1
          END IF
          CALL ReadGetALB(nfalb,irec,bfr_in)
          IF (reducedGrid) THEN
             CALL AveBoxIJtoIBJB(bfr_in,bfr_out)
          ELSE
             CALL IJtoIBJB(bfr_in ,bfr_out)
          END IF
          CLOSE(UNIT=nfalb)
          gmax=-1.0e10_r8
          gmin=+1.0e10_r8

          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)
                galb(i,j)=f2*galb(i,j)+f1*bfr_out(i,j)
                gmax=MAX(gmax,galb(i,j))
                gmin=MIN(gmin,galb(i,j))
             END DO
          END DO

          IF (ifalb == 3 .AND. tod == 0.0_r8 .AND. ifday == 0) THEN
             ifalb=0
          END IF

          IF (nfctrl(23) >= 1) THEN
             WRITE(UNIT=nfprt,FMT=888) mf,f1,f2,gmax,gmin
          END IF

       ELSE
          WRITE(UNIT=nfprt,FMT=999)
          STOP
       END IF
    END IF

    !
    ! process sst file
    !

    IF (ifsst /= 0) THEN
       IF (ifsst == -1) THEN
          INQUIRE (IOLENGTH=LRecIn) rbrf
          OPEN (UNIT=nfsst, FILE=TRIM(fNameSSTAOI),FORM='UNFORMATTED', ACCESS='DIRECT', RECL=LRecIn,&
               ACTION='READ',STATUS='OLD', IOSTAT=ierr)
          IF (ierr /= 0) THEN
             WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                  TRIM(fNameSSTAOI), ierr
             STOP "**(ERROR)**"
          END IF
          irec=1
          CALL ReadGetSST(nfsst,irec,bfr_in)
          irec=2
          CALL ReadGetSST(nfsst,irec,bfr_in)
          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(bfr_in ,gsst)
          ELSE
             CALL IJtoIBJB(bfr_in ,gsst)
          END IF
          CLOSE(UNIT=nfsst)
          gmax=-1.0e10_r8
          gmin=+1.0e10_r8

          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)
                IF (gsst(i,j) > 10.0_r8) THEN
                   gsst(i,j)=-gsst(i,j)
                ELSE IF (gsst(i,j) < 0.0_r8) THEN
                   gsst(i,j)=290.0_r8
                ELSE
                   PRINT *, " OPTION ifsst=-1 INCORRECT VALUE OF SST "
                   STOP "**(ERROR)**"
                END IF
                gmax=MAX(gmax,gsst(i,j))
                gmin=MIN(gmin,gsst(i,j))
             END DO
          END DO

          WRITE(UNIT=nfprt,FMT=667) ifsst,gmax,gmin
          ifsst=0
       ELSE IF (ifsst == 1) THEN
          OPEN(UNIT=nfsst, FILE=TRIM(fNameSSTAOI), FORM='unformatted', ACCESS='sequential',&
               ACTION='read', STATUS='old', IOSTAT=ierr)
          IF (ierr /= 0) THEN
             WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                  TRIM(fNameSSTAOI), ierr
             STOP "**(ERROR)**"
          END IF
          READ(UNIT=nfsst)
          month=idate(2)
          DO mm=1,month
             CALL ReadGetSST(nfsst,irec,bfr_in)
          END DO
          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(bfr_in ,gsst)
          ELSE
             CALL IJtoIBJB(bfr_in ,gsst)
          END IF
          CLOSE(UNIT=nfsst)
          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)
                IF (gsst(i,j) > 10.0_r8) THEN
                   gsst(i,j)=gsst(i,j)
                ELSE IF (gsst(i,j) < 0.0_r8) THEN
                   gsst(i,j)=290.0_r8
                ELSE
                   PRINT *, " OPTION ifsst=-1 INCORRECT VALUE OF SST "
                   STOP "**(ERROR)**"
                END IF
             END DO
          END DO
          ifsst=0
       ELSE IF (ifsst == 2.OR. &
            (ifsst == 3.AND.tod == 0.0_r8.AND.ifday == 0)) THEN
          INQUIRE (IOLENGTH=LRecIn) rbrf
          OPEN (UNIT=nfsst, FILE=TRIM(fNameSSTAOI),FORM='UNFORMATTED', ACCESS='DIRECT', RECL=LRecIn,&
             ACTION='READ',STATUS='OLD', IOSTAT=ierr)
          IF (ierr /= 0) THEN
             WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                  TRIM(fNameSSTAOI), ierr
             STOP "**(ERROR)**"
          END IF

          irec = mf+1
          CALL ReadGetSST(nfsst,irec,bfr_in)

          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(bfr_in ,xsst)
          ELSE
             CALL IJtoIBJB(bfr_in ,xsst)
          END IF
          
          IF (irec == 13) THEN
             irec=2
          ELSE
             irec=irec+1
          END IF
          CALL ReadGetSST(nfsst,irec,bfr_in)
          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(bfr_in ,bfr_out)
          ELSE
             CALL IJtoIBJB(bfr_in ,bfr_out)
          END IF

          CLOSE(UNIT=nfsst)
          gmax=-1.0e10_r8
          gmin=+1.0e10_r8
          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)

                fsst=f2*xsst(i,j)+f1*bfr_out(i,j)
                IF (fsst > gmax) THEN
                   gmax=fsst
                END IF
                IF (fsst < gmin) THEN
                   gmin=fsst
                END IF
                IF (fsst > 10.0_r8) THEN
                   xsst(i,j)=-fsst
                ELSE IF (fsst < 0.0_r8) THEN
                   xsst(i,j)=290.0_r8
                ELSE
                   PRINT *, " OPTION ifsst=-1 INCORRECT VALUE OF SST "
                   STOP "**(ERROR)**"
                END IF
             END DO
          END DO
          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)
                IF (tod == 0.0_r8.AND.ifday == 0) THEN
                   gsst(i,j)=xsst(i,j)
                ELSE IF (xsst(i,j) < 0.0_r8.AND.ABS(xsst(i,j)) >= tice) THEN
                   gsst(i,j)=xsst(i,j)
                ELSE IF (xsst(i,j) < 0.0_r8.AND.ABS(gsst(i,j)) >= tice) THEN
                   gsst(i,j)=-tice+1.0e-2_r8
                END IF
             END DO
          END DO

          IF (ifsst == 3.AND.tod == 0.0_r8.AND.ifday == 0) THEN
             ifsst=0
          END IF
          IF (nfctrl(23) >= 1) THEN
             WRITE(UNIT=nfprt,FMT=666) mf,f1,f2,gmax,gmin
          END IF
       ELSE IF (ifsst == 4) THEN
          IF (intsst > 0) THEN
             fisst=REAL(intsst,r8)
             xday=ifday+tod/86400.0_r8+sstlag
             irec=xday/fisst+1.0e-3_r8+1
             xx1= MOD(xday,fisst)/fisst
             xx2=1.0_r8-xx1
          ELSE
             xx1=f1
             xx2=f2
          END IF
          INQUIRE (IOLENGTH=lrecl) bfr_in
          lrecl=lrecl/2
          OPEN(UNIT=nfsst,FILE=TRIM(fNameSSTAOI),FORM='unformatted',ACCESS='direct',&
               RECL=lrecl,ACTION='read', STATUS='old', IOSTAT=ierr)
          IF (ierr /= 0) THEN
             WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                  TRIM(fNameSSTAOI), ierr
             STOP "**(ERROR)**"
          END IF
          CALL ReadGetSST2(nfsst,bfr_in,irec)
          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(bfr_in ,xsst)
          ELSE
             CALL IJtoIBJB(bfr_in ,xsst)
          END IF
          CALL ReadGetSST2(nfsst,bfr_in,irec+1)
          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(bfr_in ,bfr_out)
          ELSE
             CALL IJtoIBJB(bfr_in ,bfr_out)
          END IF
          CLOSE(UNIT=nfsst)
          gmax=-1.0e10_r8
          gmin=+1.0e10_r8
          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)
                fsst=xx2*xsst(i,j)+xx1*bfr_out(i,j)
                IF (fsst > gmax) THEN
                   gmax=fsst
                END IF
                IF (fsst < gmin) THEN
                   gmin=fsst
                END IF
                IF (fsst > 10.0_r8) THEN
                   xsst(i,j)=-fsst
                ELSE IF (fsst < 0.0_r8) THEN
                   xsst(i,j)=290.0_r8
                ELSE
                   PRINT *, " OPTION ifsst=-1 INCORRECT VALUE OF SST "
                   STOP "**(ERROR)**"
                END IF
             END DO
          END DO

          IF (nfctrl(23) >= 1) THEN
             WRITE(UNIT=nfprt,FMT=666) irec,xx1,xx2,gmax,gmin
          END IF

          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)
                IF (tod == 0.0_r8.AND.ifday == 0) THEN
                   gsst(i,j)=xsst(i,j)
                ELSE IF (xsst(i,j) < 0.0_r8.AND.ABS(xsst(i,j)) >= tice) THEN
                   gsst(i,j)=xsst(i,j)
                ELSE IF (xsst(i,j) < 0.0_r8.AND.ABS(gsst(i,j)) >= tice) THEN
                   gsst(i,j)=-tice+1.0e-2_r8
                END IF
             END DO
          END DO

       ELSE IF (ifsst == 5.AND.intsst > 0) THEN

          !*(JP)* Eliminei este caso pelas obs do Bonatti e minhas

          PRINT *, " OPTION ifsst=5 NOT CORRECTLY IMPLEMENTED "
          STOP "**(ERROR)**"

       ELSE
          WRITE(UNIT=nfprt,FMT=1999)
          STOP
       END IF
    END IF

    !
    ! process snow file
    !

    IF (ifsnw /= 0) THEN
       IF (ifsnw == 1) THEN
          INQUIRE (IOLENGTH=LRecIn) rbrf
          OPEN (UNIT=nfsnw,FILE=TRIM(fNameSnow), FORM='UNFORMATTED', ACCESS='DIRECT', &
                RECL=LRecIn, ACTION='READ',STATUS='OLD', IOSTAT=ierr)
          IF (ierr /= 0) THEN
             WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                  TRIM(fNameSnow), ierr
             STOP "**(ERROR)**"
          END IF
          irec=1
          CALL ReadGetSNW(nfsnw,irec,bfr_in)
          IF (reducedGrid) THEN
             CALL AveBoxIJtoIBJB(bfr_in,gsnw)
          ELSE
             CALL IJtoIBJB(bfr_in,gsnw)
          END IF
          CLOSE(UNIT=nfsnw)
          ifsnw=0
       ELSE IF (ifsnw == 2.OR. &
            (ifsnw == 3.AND.tod == 0.0_r8.AND.ifday == 0)) THEN
          INQUIRE (IOLENGTH=LRecIn) rbrf
          OPEN (UNIT=nfsnw,FILE=TRIM(fNameSnow), FORM='UNFORMATTED', ACCESS='DIRECT', &
                RECL=LRecIn, ACTION='READ',STATUS='OLD', IOSTAT=ierr)
          IF (ierr /= 0) THEN
             WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                  TRIM(fNameSnow), ierr
             STOP "**(ERROR)**"
          END IF
          irec=1
          CALL ReadGetSNW(nfsnw,irec,bfr_in)
          IF (reducedGrid) THEN
             CALL AveBoxIJtoIBJB(bfr_in,gsnw)
          ELSE
             CALL IJtoIBJB(bfr_in,gsnw)
          END IF
          CLOSE(UNIT=nfsnw)
          gmax=-1.0e10_r8
          gmin=+1.0e10_r8
          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)
                gmax=MAX(gmax,gsnw(i,j))
                gmin=MIN(gmin,gsnw(i,j))
             END DO
          END DO

          IF (ifsnw == 3.AND.tod == 0.0_r8.AND.ifday == 0) THEN
             ifsnw=0
          END IF
          IF (nfctrl(23) >= 1) THEN
             WRITE(UNIT=nfprt,FMT=444) gmax,gmin
          END IF
       ELSE
          WRITE(UNIT=nfprt,FMT=555)
          STOP
       END IF
    END IF

    !
    ! process soil moisture file
    !

    IF (ifslm /= 0) THEN
       IF (ifslm == 1) THEN
          INQUIRE (IOLENGTH=LRecIn) rbrf
          OPEN (UNIT=nfslm,FILE=TRIM(fNameSoilms),FORM='UNFORMATTED', ACCESS='DIRECT', &
               ACTION='read', RECL=LRecIn, STATUS='OLD', IOSTAT=ierr) 
          IF (ierr /= 0) THEN
             WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                  TRIM(fNameSoilms), ierr
             STOP "**(ERROR)**"
          END IF
          
          irec=idate(2)
          CALL ReadGetSLM(nfslm,irec,bfr_in)
          IF (reducedGrid) THEN
             CALL AveBoxIJtoIBJB(bfr_in,gslm)
          ELSE
             CALL IJtoIBJB(bfr_in,gslm)
          END IF
          CLOSE(UNIT=nfslm)
          ifslm=0
       ELSE IF (ifslm == 2.OR. &
            (ifslm == 3.AND.tod == 0.0_r8.AND.ifday == 0)) THEN
          INQUIRE (IOLENGTH=LRecIn) rbrf
          OPEN (UNIT=nfslm,FILE=TRIM(fNameSoilms),FORM='UNFORMATTED', ACCESS='DIRECT', &
               ACTION='read', RECL=LRecIn, STATUS='OLD', IOSTAT=ierr) 
          IF (ierr /= 0) THEN
             WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                  TRIM(fNameSoilms), ierr
             STOP "**(ERROR)**"
          END IF
          irec=mf
          CALL ReadGetSLM(nfslm,irec,bfr_in)
 
          IF (reducedGrid) THEN
             CALL AveBoxIJtoIBJB(bfr_in,gslm)
          ELSE
             CALL IJtoIBJB(bfr_in,gslm)
          END IF
          IF (irec == 12) THEN
             irec=1
          ELSE
             irec=irec+1    
          END IF
  
          CALL ReadGetSLM(nfslm,irec,bfr_in)
          
          IF (reducedGrid) THEN
             CALL AveBoxIJtoIBJB(bfr_in,bfr_out)
          ELSE
             CALL IJtoIBJB(bfr_in,bfr_out)
          END IF
          CLOSE(UNIT=nfslm)
          gmax=-1.0e10_r8
          gmin=+1.0e10_r8
          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)
                gslm(i,j)=f2*gslm(i,j)+f1*bfr_out(i,j)
                gmax=MAX(gmax,gslm(i,j))
                gmin=MIN(gmin,gslm(i,j))
             END DO
          END DO
          IF (ifslm == 3.AND.tod == 0.0_r8.AND.ifday == 0) THEN
             ifslm=0
          END IF
          IF (nfctrl(23) >= 1) THEN
             WRITE(UNIT=nfprt,FMT=222) mf,f1,f2,gmax,gmin
          END IF
       ELSE
          WRITE(UNIT=nfprt,FMT=333)
          STOP
       END IF
    END IF

    !
    ! Process CO2 file/field/value
    !

    IF(ifco2.EQ.-1) THEN
       CALL getco2(idatec,co2val)
    ELSEIF(ifco2.EQ.1) THEN
       !CALL READ_MONTH_CO2
    ELSEIF(ifco2.EQ.2) THEN
    ELSEIF(ifco2.EQ.3) THEN
    ELSEIF(ifco2.EQ.4) THEN
    ENDIF

    !
    ! Process ozone file
    !

    IF (ifozone /= 0) THEN
       !   =1    read field from single month file (first call only)
       IF (ifozone == 1) THEN
          INQUIRE (IOLENGTH=LRecIn) rbrf
          OPEN (UNIT=nfozone, FILE=TRIM(fNameOzone), FORM='UNFORMATTED', &
          ACCESS='DIRECT', RECL=LRecIn*kMax, ACTION='READ', STATUS='OLD', IOSTAT=ierr)
          IF (ierr /= 0) THEN
             WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                  TRIM(fNameOzone), ierr
             STOP "**(ERROR)**"
          END IF
          CALL ReadOzone(nfozone,bfr_in3,1)
          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(bfr_in3 ,gozo)
          ELSE
             CALL IJtoIBJB(bfr_in3 ,gozo)
          END IF
          CLOSE(UNIT=nfozone)
          ifozone=-1
          !   =2    interpolated to current day and time from 12 month clim
          !   =3    interpolated to current day and time from 12 month predicted field
       ELSE IF (ifozone == 2.OR. &
            (ifozone == 3.AND.tod == 0.0_r8.AND.ifday == 0)) THEN
          INQUIRE (IOLENGTH=lrecl) bfr_in3
          lrecl=lrecl/2
          OPEN(UNIT=nfozone,file=TRIM(fNameOzone),ACCESS='direct',&
               FORM='unformatted',RECL=lrecl,STATUS='old')
          IF (ierr /= 0) THEN
             WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
                  TRIM(fNameOzone), ierr
             STOP "**(ERROR)**"
          END IF

          CALL ReadOzone(nfozone,bfr_in3,mf)
          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(bfr_in3 ,gozo)
          ELSE
             CALL IJtoIBJB(bfr_in3 ,gozo)
          END IF

          mf=mf+1
          IF (mf == 13) mf=1
          CALL ReadOzone(nfozone,bfr_in3,mf)
          IF (reducedGrid) THEN
             CALL NearestIJtoIBJB(bfr_in3 ,bfr_out3)
          ELSE
             CALL IJtoIBJB(bfr_in3 ,bfr_out3)
          END IF

          CLOSE (UNIT=nfozone)
          gmax=-1.0e10_r8
          gmin=+1.0e10_r8
          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)
                DO k=1,kMax
                   gozo(i,k,j)=f2*gozo(i,k,j)+f1*bfr_out3(i,k,j)
                   gmax=MAX(gmax,gozo(i,k,j))
                   gmin=MIN(gmin,gozo(i,k,j))
                END DO
             END DO
          END DO
          IF (ifozone == 3) THEN
             ifozone=-3
          END IF
          IF (nfctrl(23) >= 1) THEN
             WRITE(UNIT=nfprt,FMT=223) mf,f1,f2,gmax,gmin
          END IF
          !   =4    interpolated from continuous direct access data set to current day and time
       ELSE IF (ifozone == 4) THEN
          WRITE(UNIT=nfprt,FMT=*) 'ERROR: DIRECT ACCESS OZONE FILE NOT IMPLEMENTED! ABORTING...'
          STOP
       END IF
    END IF



222 FORMAT(' SOILM   START MONTH=',i2,'  F1,F2=',2f6.3,'  MAX,MIN=',2e12.5)
223 FORMAT(' OZONE   START MONTH=',i2,'  F1,F2=',2f6.3,'  MAX,MIN=',2e12.5)
333 FORMAT(' ABNORMAL END IN SUBR.GETSBC AT SOILM  INTERPOLATION')
444 FORMAT(' SNOW HAS ONLY ONE FILE','  MAX,MIN=',2E12.5)
555 FORMAT(' ABNORMAL END IN SUBR.GETSBC AT SNOW   INTERPOLATION')
666 FORMAT(' SST START REC (MONTH+2) =',I5, &
         '  F1,F2=',2G13.6,'  MAX,MIN=',2G12.5)
667 FORMAT(' SST:  IFSST=',I2,'  MAX,MIN=',2G12.5)
888 FORMAT(' ALBEDO  START MONTH=',I2, &
         '  F1,F2=',2F6.3,'  MAX,MIN=',2E12.5)
999 FORMAT(' ABNORMAL END IN SUBR.GETSBC AT ALBEDO INTERPOLATION')
1999 FORMAT('ABNORMAL END IN SUBR.GETSBC AT SST   INTERPOLATION')
  END SUBROUTINE getsbc


  SUBROUTINE GetRecWgtMonthlySST &
       (idate, idatec, tod, labelsi, labelsj, &
       irec, f1, f2, mra, mrb,monl)

    IMPLICIT NONE

    ! Computes the Corresponding Records to do Linear
    ! Time Interpolation and the Respectives Weights.

    INTEGER, INTENT (IN) :: idate(4), idatec(4),monl(12)
    REAL (KIND=r8), INTENT (IN) :: tod
    CHARACTER (LEN=10), INTENT (IN) :: labelsi, labelsj

    INTEGER, INTENT (OUT) :: irec, mra, mrb
    REAL (KIND=r8), INTENT (OUT) :: f1, f2

    ! Local Constants
    INTEGER :: ysi, msi, dsi, ysj, msj, dsj, ndij, nd, &
         tmca, tmcb, tmcf
    REAL (KIND=r8) :: xday, zdayf, zdaya, zdayb, tc

    ! Get Year, Month and Day of the Initial and Second Medium Date
    ! for SST Direct Access File Data

    READ (labelsi(1:4), '(I4)') ysi
    READ (labelsi(5:6), '(I2)') msi
    READ (labelsi(7:8), '(I2)') dsi
    READ (labelsj(1:4), '(I4)') ysj
    READ (labelsj(5:6), '(I2)') msj
    READ (labelsj(7:8), '(I2)') dsj

    ! Lag of Days for SST Data:
    ! Just for Checking if the Scale is a Month
    ndij=0
    IF (msi+1 <= msj-1) THEN
       DO nd=msi+1,msj-1
          ndij=ndij+monl(nd)
       END DO
    ELSE
       DO nd=msi+1,12
          ndij=ndij+monl(nd)
       END DO
       DO nd=1,msj-1
          ndij=ndij+monl(nd)
       END DO
    END IF
    ndij=ndij+monl(msi)-dsi+dsj+365*(ysj-ysi-1)

    ! Check for Monthly Scale SST Data
    IF (ABS(ndij) <= 27 .OR. ABS(ndij) >= 32) THEN
       WRITE (UNIT=0, FMT='(/,A)') ' *** Error: The SST Data Is Not On Monthly Scale   ***'
       WRITE (UNIT=0, FMT='(/,A,I8,12X,A,/)') ' *** Lag Of Days For SST Data: ', ndij, '***'
       WRITE (UNIT=0, FMT='(A,/)') ' *** Program STOP: SUBROUTINE GetRecWgtMonthlySST  ***'
       STOP
    END IF

    ! Length in Days of the Date of Forecasting
    tmcf=monl(idatec(2))
    IF (idatec(2) == 2 .AND. MOD(idatec(4),4) == 0) tmcf=29
    ! Medium Day of the Month of Forecasting
    zdayf=0.5_r8*REAL(tmcf,r8)+1.0_r8
    ! Fractional Day of Forecasting
    tc=REAL(idate(1),r8)/24.0_r8+tod/86400.0_r8
    ! Correcting Factor if Necessary (tc is in Days)
    IF (tc >= 1.0_r8) tc=tc-1.0_r8
    xday=REAL(idatec(3),r8)+tc
    ! Getting the Corresponding Record in SST Data
    irec=12-msi+idatec(2)+12*(idatec(4)-ysi-1)+2
    IF (xday >= zdayf) irec=irec+1

    ! Months for the Linear Time Interpolation Related to the Records
    mra=MOD(irec-3+msi,12)
    IF (mra == 0) mra=12
    mrb=mra+1
    IF (mrb > 12) mrb=1

    ! Length in Days for the First Month of Interpolation
    tmca=monl(mra)
    IF (mra == 2 .AND. MOD(ysi,4) == 0) tmca=29
    ! Medium Fracitonal Day for the First Month of Interpolation
    zdaya=0.5_r8*REAL(tmca,r8)+1.0_r8-REAL(tmca,r8)
    ! Length in Days for the Second Month of Interpolation
    tmcb=monl(mrb)
    IF (mrb == 2 .AND. MOD(ysj,4) == 0) tmcb=29
    ! Medium Fracitonal Day for the Second Month of Interpolation
    zdayb=0.5_r8*REAL(tmcb,r8)+1.0_r8
    ! Scaling Fractional Day of Forecasting, if Necessary
    IF (xday >= zdayf) xday=xday-REAL(tmca,r8)
    ! Interpolation Factors
    f1=(xday-zdaya)/(zdayb-zdaya)
    f2=1.0_r8-f1

  END SUBROUTINE GetRecWgtMonthlySST

  SUBROUTINE GetWeightsOld (yrl,monl,idatec, tod, f1, f2,mf)

    IMPLICIT NONE

    ! Computes Weights as in getsbc:

    INTEGER, PARAMETER :: r8 = SELECTED_REAL_KIND(15)
    INTEGER, INTENT (IN) :: idatec(4)
    INTEGER, INTENT (IN) :: monl(12)
    REAL (KIND=r8), INTENT (IN) :: tod
    REAL (KIND=r8), INTENT (IN) :: yrl
    REAL (KIND=r8), INTENT (OUT) :: f1, f2
    INTEGER,  INTENT (OUT):: mf
    INTEGER :: mon, mnl, mn, mnlf, mnln
    REAL (KIND=r8) :: yday, add
    LOGICAL :: ly

    mon=idatec(2)
    yday=REAL(idatec(3),r8)+REAL(idatec(1),r8)/24.0_r8+MOD(tod,3600.0_r8)/86400.0_r8
    mf=mon-1
    ly= yrl == 365.25_r8 .AND. MOD(idatec(4),4) == 0
    mnl=monl(mon)
    IF (ly .AND. mon == 2) mnl=29
    ! Em getsbc seria apenas >
    ! As consideracoes de interpolacao leva a >=
    IF (yday >= 1.0_r8+0.5_r8*REAL(mnl,r8)) mf=mon
    mn=mf+1
    IF (mf < 1) mf=12
    IF (mn > 12) mn=1
    mnlf=monl(mf)
    IF (ly .AND. mf == 2) mnlf=29
    add=0.5_r8*REAL(mnlf,r8)-1.0_r8
    IF (mf == mon) add=-add-2.0_r8
    mnln=monl(mn)
    IF (ly .AND. mn == 2) mnln=29
    f1=2.0_r8*(yday+add)/REAL(mnlf+mnln,r8)
    f2=1.0_r8-f1

  END SUBROUTINE GetWeightsOld

  ! gread : reads in history carrying variables for one time step,
  !         surface geopotential, and sigma coordinate levels.
  !         checks sigma coordinate levels for consistency.



  SUBROUTINE gread(n, ifday, tod, idate, idatec, &
       qgzs, qlnp, qtmp, qdiv, qrot, qq, sl, si)
    INTEGER, INTENT(IN ) :: n
    INTEGER, INTENT(OUT) :: ifday
    REAL(KIND=r8),    INTENT(OUT) :: tod
    INTEGER, INTENT(OUT) :: idate(4)
    INTEGER, INTENT(OUT) :: idatec(4)
    REAL(KIND=r8),    INTENT(OUT) :: qgzs(2*mymnMax)
    REAL(KIND=r8),    INTENT(OUT) :: qlnp(2*mymnMax)
    REAL(KIND=r8),    INTENT(OUT) :: qtmp(2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(OUT) :: qdiv(2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(OUT) :: qrot(2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(OUT) :: qq  (2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(IN ) :: si(kMax+1)
    REAL(KIND=r8),    INTENT(IN ) :: sl(kMax)

    INTEGER              :: k, i1, i2, m, nn, mm
    REAL(KIND=r8)                 :: aux(2*mnMax,kMax)
    REAL(KIND=r8)                 :: dphi(kMax+1)
    REAL(KIND=r8)                 :: dlam(kMax)
    !
    !     spectral data file format
    !     hour,idate(4),si( kMax+1 ),sl( kMax )
    !     zln qlnp qtmp qdiv qrot
    !
    IF(nfctrl(35).GE.1)WRITE(UNIT=nfprt,FMT=999) n,kMax,kMax+1
    CALL GReadHead(n, ifday, tod, idate, idatec, dphi, dlam, kMax)

    IF (maxnodes.eq.1) THEN
       CALL GReadField(n, qgzs)

       CALL GReadField(n, qlnp)

       CALL GReadField(n, qtmp)

       DO k = 1, kMax
          CALL GReadField(n, qdiv(:,k))
          CALL GReadField(n, qrot(:,k))
       END DO

       CALL GReadField(n, qq)
    ELSE
       CALL GReadField(n, aux(:,1))
       DO mm=1,mymmax
          m = msinproc(mm,myid_four)
          i1 = 2*mnmap(m,m)-1
          i2 = 2*mymnmap(mm,m)-1
          DO nn=0,2*(mmax-m)+1
             qgzs(i2+nn) = aux(i1+nn,1)
          ENDDO
       ENDDO

       CALL GReadField(n, aux(:,1))
       DO mm=1,mymmax
          m = msinproc(mm,myid_four)
          i1 = 2*mnmap(m,m)-1
          i2 = 2*mymnmap(mm,m)-1
          DO nn=0,2*(mmax-m)+1
             qlnp(i2+nn) = aux(i1+nn,1)
          ENDDO
       ENDDO

       CALL GReadField(n, aux)
       DO k=myfirstlev,mylastlev
          DO mm=1,mymmax
             m = msinproc(mm,myid_four)
             i1 = 2*mnmap(m,m)-1
             i2 = 2*mymnmap(mm,m)-1
             DO nn=0,2*(mmax-m)+1
                qtmp(i2+nn,k+1-myfirstlev) = aux(i1+nn,k)
             ENDDO
          ENDDO
       ENDDO

       DO k = 1, kMax
          CALL GReadField(n, aux(:,1))
          CALL GReadField(n, aux(:,2))
          IF (k.ge.myfirstlev.and.k.le.mylastlev) THEN
             DO mm=1,mymmax
                m = msinproc(mm,myid_four)
                i1 = 2*mnmap(m,m)-1
                i2 = 2*mymnmap(mm,m)-1
                DO nn=0,2*(mmax-m)+1
                   qdiv(i2+nn,k+1-myfirstlev) = aux(i1+nn,1)
                   qrot(i2+nn,k+1-myfirstlev) = aux(i1+nn,2)
                ENDDO
             ENDDO
          END IF
       ENDDO

       CALL GReadField(n, aux)
       DO k=myfirstlev,mylastlev
          DO mm=1,mymmax
             m = msinproc(mm,myid_four)
             i1 = 2*mnmap(m,m)-1
             i2 = 2*mymnmap(mm,m)-1
             DO nn=0,2*(mmax-m)+1
                qq(i2+nn,k+1-myfirstlev) = aux(i1+nn,k)
             ENDDO
          ENDDO
       ENDDO

    ENDIF

    CLOSE(UNIT=n)

    !cdir novector
    DO k=1, kMax
       dlam(k)=dlam(k)-sl(k)
    END DO
    IF(nfctrl(35).GE.1)WRITE(UNIT=nfprt,FMT=100)  (dlam(k),k=1, kMax )
    DO k=1, kMax+1
       dphi(k)=dphi(k)-si(k)
    END DO
    IF(nfctrl(35).GE.1)WRITE(UNIT=nfprt,FMT=100) (dphi(k),k=1, kMax+1 )
    IF(nfctrl(35).GE.1)WRITE(UNIT=nfprt,FMT=101) n,ifday,tod,idate,idatec
100 FORMAT(' ', 13(E9.3))
101 FORMAT (' ', 'IF ABOVE TWO ROWS NOT ZERO, ', &
         'INCONSISTENCY IN SIGMA DEFINITION ON N=',I2/' AT DAY=',I8, &
         ' TIME=',F8.1,' STARTING',3I3,I5,' CURRENT',3I3,I5)
999 FORMAT(' N,KMAX,KMAXP=',3I4)
  END SUBROUTINE gread






  SUBROUTINE gread4 (n, ifday, tod, idate, idatec, &
       qgzs, qlnp, qtmp, qdiv, qrot, qq, sl, si, dodyn, nfdyn)
    INTEGER, INTENT(IN ) :: n
    INTEGER, INTENT(OUT) :: ifday
    REAL(KIND=r8),    INTENT(OUT) :: tod
    INTEGER, INTENT(OUT) :: idate(4)
    INTEGER, INTENT(OUT) :: idatec(4)
    REAL(KIND=r8),    INTENT(OUT) :: qgzs(2*mymnMax)
    REAL(KIND=r8),    INTENT(OUT) :: qlnp(2*mymnMax)
    REAL(KIND=r8),    INTENT(OUT) :: qtmp(2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(OUT) :: qdiv(2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(OUT) :: qrot(2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(OUT) :: qq  (2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(IN)  :: si(kMax+1)
    REAL(KIND=r8),    INTENT(IN)  :: sl(kMax)
    LOGICAL, INTENT(IN ) :: dodyn
    INTEGER, INTENT(IN ) :: nfdyn

    INTEGER              :: k, i1, i2, m, nn, mm
    REAL(KIND=r8)                 :: aux(2*mnMax,kMax)
    REAL(KIND=r8)                 :: aux1(2*mnMax)
    REAL(KIND=r8)                 :: dphi(kMax+1)
    REAL(KIND=r8)                 :: dlam(kMax)


    INTEGER(KIND=i4)        :: ifday4
    INTEGER(KIND=i4)        :: idat4(4)
    INTEGER(KIND=i4)        :: idat4c(4)
    REAL(KIND=r4)           :: tod4
    REAL(KIND=r4)           :: dph4(kmax+1)
    REAL(KIND=r4)           :: dla4(kmax)
    INTEGER, SAVE        :: ifdyn = 0
    !
    !     spectral data file format
    !     hour,idate(4),si( kmax+1 ),sl( kmax )
    !     zln qlnp qtmp qdiv qrot
    !
    IF(nfctrl(35).GE.1)WRITE(UNIT=nfprt,FMT=999) n,kmax,kmax+1
    CALL ReadHead(n, ifday4, tod4, idat4, idat4c, dph4, dla4, kMax)
    ifday=ifday4
    tod=tod4
    !dph4=si
    !dla4=sl
    !si=dph4
    !sl=dla4
    DO k=1,4
       idate(k)=idat4(k)
       idatec(k)=idat4c(k)
    ENDDO
    DO k=1,kmax
       dphi(k)=dph4(k)
       dlam(k)=dla4(k)
    ENDDO
    dphi(kmax+1)=dph4(kmax+1)

    IF (maxnodes.eq.1) THEN
       CALL ReadField(n, qgzs)

       IF (ifdyn .EQ. 0) THEN
          ifdyn=1
          IF (dodyn) THEN
             WRITE(UNIT=nfprt,FMT='(A,I5,A,F15.2,A)') ' ifday=',ifday4,' tod=',tod4,' dyn'
             WRITE(UNIT=nfdyn) ifday4,tod4
             WRITE(UNIT=nfdyn) qgzs
          END IF
       END IF


       CALL ReadField(n, qlnp)

       CALL ReadField(n, qtmp)

       DO k = 1, kMax
          CALL ReadField(n, qdiv(:,k))
          CALL ReadField(n, qrot(:,k))
       END DO

       CALL ReadField(n, qq)
    ELSE
       CALL ReadField(n, aux1)
       IF(myid.eq.0) THEN
          IF (ifdyn .EQ. 0) THEN
             ifdyn=1
             IF (dodyn) THEN
                WRITE (UNIT=nfprt,FMT='(A,I5,A,F15.2,A)') ' ifday=',ifday4,' tod=',tod4,' dyn'
                WRITE (UNIT=nfdyn) ifday4,tod4
                WRITE (UNIT=nfdyn) aux1
             END IF
          END IF
       END IF

       DO mm=1,mymmax
          m = msinproc(mm,myid_four)
          i1 = 2*mnmap(m,m)-1
          i2 = 2*mymnmap(mm,m)-1
          DO nn=0,2*(mmax-m)+1
             qgzs(i2+nn) = aux1(i1+nn)
          ENDDO
       ENDDO

       CALL ReadField(n, aux1)
       DO mm=1,mymmax
          m = msinproc(mm,myid_four)
          i1 = 2*mnmap(m,m)-1
          i2 = 2*mymnmap(mm,m)-1
          DO nn=0,2*(mmax-m)+1
             qlnp(i2+nn) = aux1(i1+nn)
          ENDDO
       ENDDO

       CALL ReadField(n, aux)
       DO k=myfirstlev,mylastlev
          DO mm=1,mymmax
             m = msinproc(mm,myid_four)
             i1 = 2*mnmap(m,m)-1
             i2 = 2*mymnmap(mm,m)-1
             DO nn=0,2*(mmax-m)+1
                qtmp(i2+nn,k+1-myfirstlev) = aux(i1+nn,k)
             ENDDO
          ENDDO
       ENDDO

       DO k = 1, kMax
          CALL ReadField(n, aux(:,1))
          CALL ReadField(n, aux(:,2))
          IF (k.ge.myfirstlev.and.k.le.mylastlev) THEN
             DO mm=1,mymmax
                m = msinproc(mm,myid_four)
                i1 = 2*mnmap(m,m)-1
                i2 = 2*mymnmap(mm,m)-1
                DO nn=0,2*(mmax-m)+1
                   qdiv(i2+nn,k+1-myfirstlev) = aux(i1+nn,1)
                   qrot(i2+nn,k+1-myfirstlev) = aux(i1+nn,2)
                ENDDO
             ENDDO
          END IF
       ENDDO

       CALL ReadField(n, aux)
       DO k=myfirstlev,mylastlev
          DO mm=1,mymmax
             m = msinproc(mm,myid_four)
             i1 = 2*mnmap(m,m)-1
             i2 = 2*mymnmap(mm,m)-1
             DO nn=0,2*(mmax-m)+1
                qq(i2+nn,k+1-myfirstlev) = aux(i1+nn,k)
             ENDDO
          ENDDO
       ENDDO

    ENDIF

    CLOSE(UNIT=n)

    DO k=1, kmax
       dlam(k)=dlam(k)-sl(k)
    END DO

    IF(nfctrl(35).GE.1)WRITE(UNIT=nfprt,FMT=100) (dlam(k),k=1, kmax )
    DO k=1, kmax+1
       dphi(k)=dphi(k)-si(k)
    END DO
    IF(nfctrl(35).GE.1)WRITE(UNIT=nfprt,FMT=100) (dphi(k),k=1, kmax+1 )
    IF(nfctrl(35).GE.1)WRITE(UNIT=nfprt,FMT=101) n,ifday,tod,idate,idatec
100 FORMAT(' ', 13(E9.3))
101 FORMAT (' IF ABOVE TWO ROWS NOT ZERO, ', &
         'INCONSISTENCY IN SIGMA DEFINITION ON N=',I2/' AT DAY=',I8, &
         ' TIME=',F8.1,' STARTING',3I3,I5,' CURRENT',3I3,I5)
999 FORMAT(' N,KMAX,KMAXP=',3I4)
  END SUBROUTINE gread4

  !     gwrite : writes out the surface geopotential and history carrying
  !              fields of the spectral model after first inverting the
  !              laplacian to recapture the surface geopotential field.

  SUBROUTINE gwrite(n, ifday, tod, idate, idatec, &
       qlnp, qtmp, qdiv, qrot, qq, sl, si, qgzs)
    INTEGER, INTENT(IN) :: n
    INTEGER, INTENT(IN) :: ifday
    REAL(KIND=r8),    INTENT(IN) :: tod
    INTEGER, INTENT(IN) :: idate(4)
    INTEGER, INTENT(IN) :: idatec(4)
    REAL(KIND=r8),    INTENT(IN) :: qgzs (2*mymnMax)
    REAL(KIND=r8),    INTENT(IN) :: qlnp (2*mymnMax)
    REAL(KIND=r8),    INTENT(IN) :: qtmp (2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(IN) :: qdiv (2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(IN) :: qrot (2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(IN) :: qq   (2*mymnMax,kMaxloc)
    REAL(KIND=r8),    INTENT(IN) :: si(kMax+1)
    REAL(KIND=r8),    INTENT(IN) :: sl(kMax)

    INTEGER             :: k
    REAL(KIND=r8)                :: aux (2*mnMax,kMax)
    REAL(KIND=r8)                :: aux1(2*mnMax,kMax)

    IF(myid.eq.0)  CALL GWriteHead(n, ifday, tod, idate, idatec, si, sl)

    IF(maxnodes.eq.1) THEN
       CALL GWriteField(n, qgzs)

       CALL GWriteField(n, qlnp)

       CALL GWriteField(n, qtmp)

       DO k = 1, kMax
          CALL GWriteField(n, qdiv(:,k))
          CALL GWriteField(n, qrot(:,k))
       END DO

       CALL GWriteField(n, qq)
    ELSE
       CALL Collect_Spec(qgzs, aux(:,1), 1, 1, 0)
       IF(myid.eq.0) CALL GWriteField(n, aux(:,1))

       CALL Collect_Spec(qlnp, aux(:,1), 1, 1, 0)
       IF(myid.eq.0) CALL GWriteField(n, aux(:,1))

       CALL Collect_Spec(qtmp, aux, kmaxloc, kmax, 0)
       IF(myid.eq.0) CALL GWriteField(n, aux)

       CALL Collect_Spec(qdiv, aux, kmaxloc, kmax, 0)
       CALL Collect_Spec(qrot, aux1, kmaxloc, kmax, 0)
       IF(myid.eq.0) THEN
          DO k = 1, kMax
             CALL GWriteField(n, aux(:,k))
             CALL GWriteField(n, aux1(:,k))
          END DO
       ENDIF

       CALL Collect_Spec(qq, aux, kmaxloc, kmax, 0)
       IF(myid.eq.0) CALL GWriteField(n, aux)
    ENDIF

    IF(nfctrl(43).GE.1)WRITE(UNIT=nfprt,FMT=3001)ifday,tod,idate,idatec,n
3001 FORMAT(' GWRITE IFDAY=',I8,' TOD=',F8.1,2(2X,3I3,I5), 2X,'N=',I2)
  END SUBROUTINE gwrite
  !hmjb
  SUBROUTINE getco2(time,co2val)
    !==========================================================================
    ! getco2: Interpolates Mauna Loa data for a given time
    !
    ! *** Atmospheric CO2 concentrations (ppmv) derived from in situ  ***
    ! *** air samples collected at Mauna Loa Observatory, Hawaii      ***
    !
    ! Data:
    !
    !   http://cdiac.ornl.gov/trends/co2/contents.htm
    !   http://cdiac.ornl.gov/ftp/trends/co2/maunaloa.co2
    !
    ! Parabolic fitting by hbarbosa@cptec.inpe.br, 17 Jan 2007:
    !
    !   co2val = a*(time-2000)^2 + b*(time-2000) + c
    !
    !       a  = 0.0116696   +/- 0.0005706    (4.89%)
    !       b  = 1.79984     +/- 0.022        (1.222%)
    !       c  = 369         +/- 0.1794       (0.04863%)
    !
    !==========================================================================
    !     time.......date of current data
    !     time(1)....hour(00/12)
    !     time(2)....month
    !     time(3)....day of month
    !     time(4)....year
    !
    !    co2val....co2val is wgne standard value in ppm "co2val = /345.0/
    !==========================================================================

    IMPLICIT NONE
    REAL(KIND=r8), PARAMETER :: A = 0.0116696
    REAL(KIND=r8), PARAMETER :: B = 1.79984
    REAL(KIND=r8), PARAMETER :: C = 369.0

    INTEGER,       INTENT(IN ) :: time(4)
    REAL(KIND=r8), INTENT(OUT) :: co2val

    REAL(KIND=r8) :: TDIF

    tdif=time(4) + (time(2)-1.)/12. + (time(3)-1.+ time(1)/24.)/365. - 2000.

    co2val = A*tdif**2 + B*tdif + C

    !    WRITE(*,123) time,tdif+2000.,co2val
    !123 format('hmjb co2val date=',3(I2,1x),I4,' fyear=',F10.5,' val=',F7.3)

    RETURN
  END SUBROUTINE getco2
  !hmjb
END MODULE InputOutput
