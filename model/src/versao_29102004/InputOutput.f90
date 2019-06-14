!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE InputOutput
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
       ReadGetSST2

  USE Utils, ONLY: &
       TransDiagCol

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: InitInputOutput
  PUBLIC :: cnvray
  PUBLIC :: wrfld
  PUBLIC :: sclout
  PUBLIC :: transp
  PUBLIC :: transw
  PUBLIC :: WillGetSbc
  PUBLIC :: getsbc
  PUBLIC :: gread
  PUBLIC :: gread4
  PUBLIC :: gwrite

  PUBLIC :: nfprt
  PUBLIC :: nferr
  PUBLIC :: ifprt
  PUBLIC :: fsbc
  INTEGER              :: nfprt
  INTEGER              :: nferr
  INTEGER              :: ifprt(100)
  INTEGER              :: mMax
  INTEGER              :: nMax
  INTEGER              :: mnMax
  INTEGER, ALLOCATABLE :: mnMap(:,:)
  INTEGER              :: kMax

  LOGICAL              :: fsbc       
  INTEGER              :: ngrmx
  INTEGER              :: ncf
  INTEGER              :: ncf2
  CHARACTER(LEN=100)   :: path
  CHARACTER(LEN=200)   :: fNameSnow
  CHARACTER(LEN=200)   :: fNameSSTWKL
  CHARACTER(LEN=200)   :: fNameSSTAOI
  CHARACTER(LEN=200)   :: fNameSoilms
  CHARACTER(LEN=200)   :: fNameSibAlb
  INTEGER, ALLOCATABLE :: looku (:,:,:)    
  REAL,    ALLOCATABLE :: cnfac (:)     
  REAL,    ALLOCATABLE :: cnfac2(:)   

  REAL,    PARAMETER   :: undef =1.0e53
  !
  ! unit number of input output file 
  !
  INTEGER, PARAMETER   ::  nfin0   =18 ! input  file at time level t-dt
  INTEGER, PARAMETER   ::  nfin1   =18 ! input  file at time level t
  INTEGER, PARAMETER   ::  nfout0  =20 ! output file at time level t-dt
  INTEGER, PARAMETER   ::  nfout1  =21 ! output file at time level t
  INTEGER, PARAMETER   ::  nfout2  =21 ! output file at t=0 ( normal-mode initialized )
  INTEGER, PARAMETER   ::  nfclm0  =10 ! sst,soil moisture etc.  input
  INTEGER, PARAMETER   ::  nfclm1  =11 ! sst,soil moisture etc.  output
  INTEGER, PARAMETER   ::  nftgz0  =61 ! ground temperature and roughness length input
  INTEGER, PARAMETER   ::  nftgz1  =61 ! ground temperature and roughness length output
  INTEGER, PARAMETER   ::  nfsibt  =99 ! sib surface vegetation type
  INTEGER, PARAMETER   ::  nfsibd  =88 ! sib vegetation parameter
  INTEGER, PARAMETER   ::  nfsibi  =77 ! sib prognostic variable input  file
  INTEGER, PARAMETER   ::  nfsibo  =66 ! sib prognostic variable output file
  INTEGER, PARAMETER   ::  nfcnv0  = 0 ! initial information on convective clouds for int. radiation
  INTEGER, PARAMETER   ::  nfcnv1  =32 ! output information on convective clouds for int. radiation
  INTEGER, PARAMETER   ::  nfvar   =33 ! surface height variance
  INTEGER, PARAMETER   ::  nfsst   =50 ! sst   file
  INTEGER, PARAMETER   ::  nfsnw   =51 ! snow file
  INTEGER, PARAMETER   ::  nfalb   =52 ! albedo file
  INTEGER, PARAMETER   ::  nfslm   =53 ! soil moisture file
  INTEGER, PARAMETER   ::  nfcldr  =74 ! cloud radiation fields
  INTEGER, PARAMETER   ::  nfnmi   =80 ! normal modes
  INTEGER, PARAMETER   ::  igrfu   =45
  INTEGER, PARAMETER   ::  iptu    =42
  INTEGER, PARAMETER   ::  nfdbh   =75 ! heating rate used for diabatic nlnmi


  INTEGER, ALLOCATABLE :: TMap(:)
  INTEGER, PARAMETER   :: r4=SELECTED_REAL_KIND(6)
  INTEGER, PARAMETER   :: i4=SELECTED_INT_KIND(9)
  INTEGER, PARAMETER   :: r8=SELECTED_REAL_KIND(15)
  INTEGER, PARAMETER   :: i8=SELECTED_INT_KIND(14)

CONTAINS



  ! InitInputOutput: Initializes module



  SUBROUTINE InitInputOutput(nfprt_in, nferr_in, ifprt_in, ngrmx_in, ncf_in , &
       ncf2_in , mMax_in , nMax_in , mnMax_in,mnMap_in, &
       kmax_in ,path_in  ,fNameSnow_in,fNameSSTWKL_in , &
       fNameSSTAOI_in,fNameSoilms_in,fNameSibAlb_in   , &
       fNameCnftBl   ,fNameCnf2Tb   ,fNameLookTb)
    INTEGER,          INTENT(IN) :: mMax_in
    INTEGER,          INTENT(IN) :: nMax_in
    INTEGER,          INTENT(IN) :: mnMax_in
    INTEGER,          INTENT(IN) :: mnMap_in(:,:)
    INTEGER,          INTENT(IN) :: kmax_in
    INTEGER,          INTENT(IN) :: nfprt_in
    INTEGER,          INTENT(IN) :: nferr_in
    INTEGER,          INTENT(IN) :: ifprt_in(100)
    INTEGER,          INTENT(IN) :: ngrmx_in
    INTEGER,          INTENT(IN) :: ncf_in
    INTEGER,          INTENT(IN) :: ncf2_in
    CHARACTER(LEN=*), INTENT(IN) :: path_in
    CHARACTER(LEN=*), INTENT(IN) :: fNameSnow_in
    CHARACTER(LEN=*), INTENT(IN) :: fNameSSTWKL_in
    CHARACTER(LEN=*), INTENT(IN) :: fNameSSTAOI_in
    CHARACTER(LEN=*), INTENT(IN) :: fNameSoilms_in
    CHARACTER(LEN=*), INTENT(IN) :: fNameSibAlb_in
    CHARACTER(LEN=*), INTENT(IN) :: fNameCnftBl
    CHARACTER(LEN=*), INTENT(IN) :: fNameCnf2Tb
    CHARACTER(LEN=*), INTENT(IN) :: fNameLookTb
    INTEGER :: mm
    INTEGER :: nn
    INTEGER :: l
    ALLOCATE (mnMap(mMax_in,nMax_in))

    OPEN(37, file=TRIM(fNameCnftBl),FORM="FORMATTED")
    OPEN(38, file=TRIM(fNameCnf2Tb),FORM="FORMATTED")
    OPEN(39, file=TRIM(fNameLookTb),FORM="FORMATTED")

    path  = path_in       
    nfprt = nfprt_in
    nferr = nferr_in
    ifprt = ifprt_in
    mMax  = mMax_in 
    nMax  = nMax_in 
    mnMax = mnMax_in
    mnMap = mnMap_in
    kmax  = kmax_in
    fNameSnow  =fNameSnow_in
    fNameSSTWKL=fNameSSTWKL_in
    fNameSSTAOI=fNameSSTAOI_in
    fNameSoilms=fNameSoilms_in
    fNameSibAlb=fNameSibAlb_in
    ncf   = ncf_in
    ALLOCATE(cnfac(ncf))
    REWIND 37
    READ(37,"(5e16.8)") cnfac
    REWIND 37
    CLOSE(37,status='KEEP')


    ncf2  = ncf2_in
    ALLOCATE(cnfac2(ncf2))  
    REWIND 38  
    READ(38,"(5e16.8)") cnfac2
    REWIND 38  
    CLOSE(38,status='KEEP')


    ngrmx = ngrmx_in
    ALLOCATE(looku(0:9,0:9,0:ngrmx))
    REWIND 39
    READ(39,"(20i4)") looku
    REWIND 39
    CLOSE(39,status='KEEP')

    ALLOCATE(TMap(2*mnMax))
    l=0
    DO mm=1,mMax
       DO nn=mm,nMax
          l=l+1
          TMap(2*l-1)=2*mnMap(mm,nn)-1
          TMap(2*l  )=2*mnMap(mm,nn)
       END DO
    END DO
  END SUBROUTINE InitInputOutput



  ! cnvray: convert array



  SUBROUTINE cnvray (array, idim, ifr, ito)
    INTEGER, INTENT(IN   ) :: idim
    REAL,    INTENT(INOUT) :: array(idim)
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
    REAL              :: cf
    REAL              :: cf2

    CHARACTER(LEN=*), PARAMETER :: h="**(cnvray)**"

    ! consistency

    IF (ifr <= -1) THEN
       WRITE(c0,"(i20)") ifr
       WRITE(*,"(a)") h//" ERROR: ifr ("//TRIM(ADJUSTL(c0))//") <= -1 "
       STOP h
    ELSE IF (ito <= -1) THEN
       WRITE(c0,"(i20)") ito
       WRITE(*,"(a)") h//" ERROR: ito ("//TRIM(ADJUSTL(c0))//") <= -1 "
       STOP h
    ELSE IF (idim <= 0) THEN
       WRITE(c0,"(i20)") idim
       WRITE(*,"(a)") h//" ERROR: idim ("//TRIM(ADJUSTL(c0))//") <= 0 "
       STOP h
    ELSE IF (ito /= ifr) THEN
       igpf=ifr/10
       igpt=ito/10
       IF (igpf /= igpt) THEN
          WRITE(c0,"(i20)") igpf
          WRITE(c1,"(i20)") igpt
          WRITE(*,"(a)") h//" ERROR: igpf ("//TRIM(ADJUSTL(c0))//&
               &") /= igpt ("//TRIM(ADJUSTL(c1))//")"
          STOP h
       ELSE IF (igpf > ngrmx) THEN
          WRITE(c0,"(i20)") igpf
          WRITE(c1,"(i20)") ngrmx
          WRITE(*,"(a)") h//" ERROR: igpf ("//TRIM(ADJUSTL(c0))//&
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
             WRITE(*,"(a)") h//" ERROR: icf ("//TRIM(ADJUSTL(c0))//&
                  &") < 1 or > ncf ("//TRIM(ADJUSTL(c1))//")"
             STOP h
          END IF

          ! get coeficients

          cf=cnfac(icf)
          IF (icf <= ncf2) THEN
             cf2=cnfac2(icf)
          ELSE
             cf2=0.0
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


  ! wrfld: writes an unformatted 2-d field on file unit iudiag.


  SUBROUTINE wrfld(field,nwv,unit)
    !
    ! wrfld  :writes an unformatted 2-d field on file unit unit.
    !
    INTEGER     , INTENT(IN   ) :: nwv
    INTEGER     , INTENT(IN   ) :: unit
    REAL        , INTENT(IN   ) :: field(nwv)
    WRITE(unit)REAL(field,i4)
  END SUBROUTINE wrfld


  ! transp : after input, transposes arrays of spectral coefficients
  !          by swapping the order of the subscripts representing the
  !          degree and order of the associated legendre functions.



  SUBROUTINE transp(a,kmax,isign)
    INTEGER, INTENT(IN   ) :: kmax
    INTEGER, INTENT(IN   ) :: isign
    REAL   , INTENT(INOUT) :: a(2*mnMax,kmax)

    INTEGER                :: k  
    INTEGER                :: mn
    REAL                   :: w(2*mnMax)

    IF(isign.EQ.1) THEN
       DO k=1,kmax
          DO mn = 1, 2*mnMax
             w(mn) = a(TMap(mn),k)
          END DO
          DO mn = 1, 2*mnMax
             a(mn,k) = w(mn)
          END DO
       END DO
    ELSE
       DO k=1,kmax
          DO mn = 1, 2*mnMax
             w(TMap(mn)) = a(mn,k)
          END DO
          DO mn = 1, 2*mnMax
             a(mn,k) = w(mn)
          END DO
       END DO
    END IF
  END SUBROUTINE transp



  ! transw : writes out an array containing spectral coefficients
  !          representing one level of a global field after
  !          transposing the array by swapping the order of
  !          the subscripts for the degree and the order of
  !          the associated legendre functions.



  SUBROUTINE transw(a, unit)
    REAL,    INTENT(IN) :: a(2*mnMax)
    INTEGER, INTENT(IN) :: unit

    INTEGER :: mn
    REAL    :: w(2*mnMax)

    DO mn = 1, 2*mnMax
       w(mn) = a(TMap(mn))
    END DO
    WRITE(unit) w
  END SUBROUTINE transw
  !
  ! scale, convert to 32 bits and output field
  !
  SUBROUTINE sclout(unit, field, nharm, levs, fact1, ivar, nufr, nuto)
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(IN) :: nharm
    INTEGER, INTENT(IN) :: levs
    REAL,    INTENT(IN) :: field(nharm,levs)
    REAL,    INTENT(IN) :: fact1
    CHARACTER(LEN=4), INTENT(IN) :: ivar
    INTEGER, INTENT(IN) :: nufr
    INTEGER, INTENT(IN) :: nuto
    REAL :: fldaux(nharm,1)

    INTEGER :: k, i

    DO k=1,levs

       DO i=1,nharm
          fldaux(i,1) = fact1 * field(i,k)
       END DO

       CALL cnvray(fldaux,nharm,nufr,nuto)

       IF(ivar.EQ.'SPEC') THEN
          CALL transp(fldaux, 1, 1)
       END IF

       CALL WriteField(unit, fldaux(:,1))

    END DO

  END SUBROUTINE sclout




  LOGICAL FUNCTION WillGetSbc(idate, tod, fint)
    INTEGER, INTENT(IN) :: idate(4)
    REAL,    INTENT(IN) :: tod
    REAL,    INTENT(IN) :: fint
    REAL                :: fhr
    
    WillGetSbc = .TRUE.
    IF (fint > 0.0) THEN
       fhr=float(idate(1))+tod/3600.0+1.0e-3
       WillGetSbc = fsbc .OR. ABS( MOD(fhr,fint)) <= 1.0e-2
    END IF
  END FUNCTION WillGetSbc

  !
  ! getsbc :read surface boundary conditions.
  !
  SUBROUTINE getsbc (ncols,jmax,galb,gsst,gslm,gsnw,ifday,tod,&
       idate,idatec,nfprt,ifprt,ifalb,ifsst,& 
       ifslm,ifsnw,sstlag,intsst,fint,tice,yrl ,& 
       monl) 
    INTEGER(KIND=i8), INTENT(in   ) :: ncols  
    INTEGER(KIND=i8), INTENT(in   ) :: jmax  
    REAL   (KIND=r8), INTENT(out  ) :: galb(ncols*jmax)
    REAL   (KIND=r8), INTENT(out  ) :: gsst(ncols*jmax)
    REAL   (KIND=r8), INTENT(out  ) :: gslm(ncols*jmax)
    REAL   (KIND=r8), INTENT(out  ) :: gsnw(ncols*jmax)
    INTEGER(KIND=i8), INTENT(in   ) :: ifday
    REAL   (KIND=r8), INTENT(in   ) :: tod
    INTEGER(KIND=i8), INTENT(in   ) :: idate(4) 
    INTEGER(KIND=i8), INTENT(in   ) :: idatec(4)
    INTEGER(KIND=i8), INTENT(in   ) :: nfprt       ! intent(in)
    INTEGER(KIND=i8), INTENT(in   ) :: ifprt(100)  
    INTEGER(KIND=i8), INTENT(inout) :: ifalb 
    INTEGER(KIND=i8), INTENT(inout) :: ifsst 
    INTEGER(KIND=i8), INTENT(inout) :: ifslm 
    INTEGER(KIND=i8), INTENT(inout) :: ifsnw 
    REAL   (KIND=r8), INTENT(in   ) :: sstlag 
    INTEGER(KIND=i8), INTENT(in   ) :: intsst
    REAL   (KIND=r8), INTENT(in   ) :: fint  
    REAL   (KIND=r8), INTENT(in   ) :: tice
    REAL   (KIND=r8), INTENT(in   ) :: yrl 
    INTEGER(KIND=i8), INTENT(in   ) :: monl(12)
    !
    REAL   (KIND=r8)                :: xsst  (ncols*jmax)
    REAL   (KIND=r8)                :: bfr1  (ncols*jmax)
    REAL   (KIND=r8)                :: bfr2  (ncols*jmax)
    REAL   (KIND=r4)                :: bfra  (ncols*jmax)
    REAL   (KIND=r4)                :: bfrb  (ncols*jmax)
    INTEGER(KIND=i8)                :: lrecl
    LOGICAL                         :: ly
    REAL   (KIND=r8)                :: fhr
    INTEGER(KIND=i8)                :: mon
    INTEGER(KIND=i8)                :: mf
    INTEGER(KIND=i8)                :: mnl
    INTEGER(KIND=i8)                :: mn
    INTEGER(KIND=i8)                :: mnlf
    INTEGER(KIND=i8)                :: month
    INTEGER(KIND=i8)                :: mm
    INTEGER(KIND=i8)                :: ij
    INTEGER(KIND=i8)                :: irec
    INTEGER(KIND=i8)                :: mfx
    INTEGER(KIND=i8)                :: mnln
    REAL   (KIND=r8)                :: yday
    REAL   (KIND=r8)                :: f1
    REAL   (KIND=r8)                :: f2
    REAL   (KIND=r8)                :: add
    REAL   (KIND=r8)                :: gmax
    REAL   (KIND=r8)                :: gmin
    REAL   (KIND=r8)                :: fsst
    REAL   (KIND=r8)                :: fisst
    REAL   (KIND=r8)                :: xx1
    REAL   (KIND=r8)                :: xx2
    REAL   (KIND=r8)                :: xday
    REAL   (KIND=r8)                :: zday
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

    IF (fint > 0.0) THEN
       fhr=float(idate(1))+tod/3600.0+1.0e-3
       IF (.NOT. fsbc .AND. ABS( MOD(fhr,fint)) > 1.0e-2) THEN
          RETURN
       END IF
    END IF

    mon=idatec(2)
    yday=idatec(3)+float(idatec(1))/24.0+ MOD(tod,3600.0)/86400.
    mf=mon-1
    ly=yrl == 365.25 .AND. MOD(idatec(4),4) == 0
    mnl=monl(mon)

    IF (ly .AND. mon == 2) THEN
       mnl=29
    END IF
    IF (yday > 1.0+float(mnl)/2.0) THEN
       mf=mon
    END IF

    mn=mf+1

    IF (mf <  1) THEN
       mf=12
    END IF
    IF (mn > 12) THEN
       mn=1
    END IF

    mnlf=monl(mf)

    IF (ly .AND. mf == 2) THEN
       mnlf=29
    END IF

    add=float(mnlf)/2.0-1.0

    IF (mf == mon) THEN
       add=-add-2.0
    END IF

    mnln=monl(mn)

    IF (ly .AND. mn == 2) THEN
       mnln=29
    END IF

    f1=2.0*(yday+add)/float(mnlf+mnln)
    f2=1.0-f1


    ! process albedo file


    IF (ifalb /= 0) THEN
       IF (ifalb == 1) THEN
          month=idate(2)
          OPEN(nfalb, file=TRIM(fNameSibAlb), &
               ACTION="read",FORM="UNFORMATTED")   
          DO mm=1,month
             CALL ReadGetALB(nfalb,bfr1)
          END DO
          CLOSE (nfalb)
          DO ij=1,ncols*jmax
             galb(ij)=bfr1(ij)
          END DO
          ifalb=0
       ELSE IF (&
            (ifalb == 2) .OR. &
            (ifalb == 3 .AND. tod == 0.0 .AND. ifday == 0)) THEN
          OPEN(nfalb, file=TRIM(fNameSibAlb), &
               ACTION="read",FORM="UNFORMATTED")   
          DO mm=1,mf
             CALL ReadGetALB(nfalb,bfr1)
          END DO
          IF (mf == 12) THEN
             REWIND nfalb
          END IF
          CALL ReadGetALB(nfalb,bfr2)
          CLOSE (nfalb)
          gmax=-1.0e10
          gmin=+1.0e10
          DO ij=1,ncols*jmax
             galb(ij)=f2*bfr1(ij)+f1*bfr2(ij)
             gmax=MAX(gmax,galb(ij))
             gmin=MIN(gmin,galb(ij))
          END DO
          IF (ifalb == 3 .AND. tod == 0.0 .AND. ifday == 0) THEN
             ifalb=0
          END IF
          IF (ifprt(23) >= 1) THEN
             WRITE(nfprt,888) mf,f1,f2,gmax,gmin
          END IF
       ELSE
          WRITE(nfprt,999)
       END IF
    END IF


    ! process sst file


    IF (ifsst /= 0) THEN
       IF (ifsst == -1) THEN
          OPEN(nfsst, file=TRIM(fNameSSTAOI), &
               ACTION="read",FORM="UNFORMATTED")
          CALL ReadGetSST(nfsst,bfrb)
          CALL ReadGetSST(nfsst,bfra)
          CLOSE(nfsst)
          gmax=-1.0e10
          gmin=+1.0e10
          DO ij=1,ncols*jmax
             IF (bfra(ij) > 10.0) THEN
                gsst(ij)=-bfra(ij)
             ELSE IF (bfra(ij) < 0.0) THEN
                gsst(ij)=290.0
             END IF
             gmax=MAX(gmax,gsst(ij))
             gmin=MIN(gmin,gsst(ij))
          END DO
          WRITE(6,667) ifsst,gmax,gmin
          ifsst=0
       ELSE IF (ifsst == 1) THEN
          OPEN(nfsst, file=TRIM(fNameSSTAOI), &
               ACTION="read",FORM="UNFORMATTED")
          READ(nfsst)  
          month=idate(2)
          DO mm=1,month
             CALL ReadGetSST(nfsst,bfra)
          END DO
          CLOSE(nfsst)
          DO ij=1,ncols*jmax
             IF (bfra(ij) > 10.0) THEN
                gsst(ij)=-bfra(ij)
             ELSE IF (bfra(ij) < 0.0) THEN
                gsst(ij)=290.0
             END IF
          END DO
          ifsst=0
       ELSE IF (ifsst == 2.OR. &
            (ifsst == 3.AND.tod == 0.0.AND.ifday == 0)) THEN
          OPEN(nfsst, file=TRIM(fNameSSTAOI), &
               ACTION="read",FORM="UNFORMATTED")
          READ(nfsst)
          DO  mm=1,mf
             CALL ReadGetSST(nfsst,bfra)
          END DO
          IF (mf == 12) THEN
             REWIND nfsst
             READ(nfsst)
          END IF
          CALL ReadGetSST(nfsst,bfrb)
          CLOSE (nfsst)
          gmax=-1.0e10
          gmin=+1.0e10
          DO ij=1,ncols*jmax
             fsst=f2*bfra(ij)+f1*bfrb(ij)
             IF (fsst > gmax) THEN
                gmax=fsst
             END IF
             IF (fsst < gmin) THEN
                gmin=fsst
             END IF
             IF (fsst > 10.0) THEN
                xsst(ij)=-fsst
             ELSE IF (fsst < 0.0) THEN
                xsst(ij)=290.0
             END IF
          END DO
          DO ij=1,ncols*jmax
             IF (tod == 0.0.AND.ifday == 0) THEN
                gsst(ij)=xsst(ij)
             ELSE IF (xsst(ij) < 0.0.AND.ABS(xsst(ij)) >= tice) THEN
                gsst(ij)=xsst(ij)
             ELSE IF (xsst(ij) < 0.0.AND.ABS(gsst(ij)) >= tice) THEN
                gsst(ij)=-tice+1.0e-2
             END IF
          END DO
          IF (ifsst == 3.AND.tod == 0.0.AND.ifday == 0) THEN
             ifsst=0
          END IF
          IF (ifprt(23) >= 1) THEN
             WRITE(nfprt,666) mf,f1,f2,gmax,gmin
          END IF
       ELSE IF (ifsst == 4) THEN
          IF (intsst > 0) THEN
             fisst=float(intsst)
             xday=ifday+tod/86400.+sstlag
             irec=xday/fisst+1.0e-3+1
             xx1= MOD(xday,fisst)/fisst
             xx2=1.0-xx1
          ELSE
             mon=idatec(2)
             xday=idatec(3)+float(idatec(1))/24.0+MOD(tod,3600.0)/86400.0
             zday=1.0+float(monl(mon))/2.0    
             mfx=mon-1
             IF (xday > zday) THEN
                mfx=mon           
             END IF
             irec=12*(idatec(4)-idate(4))+mfx-idate(2)+INT(sstlag)+1
             xx1=f1
             xx2=f2
          END IF
          irec=irec+2
          INQUIRE (IOLENGTH=lrecl) bfra 
          lrecl=lrecl/2
          OPEN(nfsst,file=TRIM(fNameSSTAOI),ACCESS='DIRECT',&
               FORM='UNFORMATTED',RECL=lrecl,STATUS='unknown')
          CALL ReadGetSST2(nfsst,bfra,irec)
          CALL ReadGetSST2(nfsst,bfrb,irec+1)
          CLOSE(nfsst)
          gmax=-1.0e10
          gmin=+1.0e10
          DO ij=1,ncols*jmax
             fsst=xx2*bfra(ij)+xx1*bfrb(ij)
             IF (fsst > gmax) THEN
                gmax=fsst
             END IF
             IF (fsst < gmin) THEN
                gmin=fsst
             END IF
             IF (fsst > 10.0) THEN
                xsst(ij)=-fsst
             ELSE IF (fsst < 0.0) THEN
                xsst(ij)=290.0
             END IF
          END DO
          IF (ifprt(23) >= 1) THEN
             WRITE(nfprt,666) irec,xx1,xx2,gmax,gmin
          END IF
          DO ij=1,ncols*jmax
             IF (tod == 0.0.AND.ifday == 0) THEN
                gsst(ij)=xsst(ij)
             ELSE IF (xsst(ij) < 0.0.AND.ABS(xsst(ij)) >= tice) THEN
                gsst(ij)=xsst(ij)
             ELSE IF (xsst(ij) < 0.0.AND.ABS(gsst(ij)) >= tice) THEN
                gsst(ij)=-tice+1.0e-2
             END IF
          END DO
       ELSE IF (ifsst == 5.AND.intsst > 0) THEN

          !*(JP)* Eliminei este caso pelas obs do Bonatti e minhas

          PRINT *, " OPTION ifsst=5 NOT CORRECTLY IMPLEMENTED "
          STOP "**(ERROR)**"

       ELSE
          WRITE(nfprt,1999)
       END IF
    END IF


    ! process snow file


    IF (ifsnw /= 0) THEN
       IF (ifsnw == 1) THEN
          OPEN(nfsnw, file=TRIM(fNameSnow), &
               ACTION="read",FORM="UNFORMATTED")  
          CALL ReadGetSNW(nfsnw,bfra)
          CLOSE(nfsnw)
          DO ij=1,ncols*jmax
             gsnw(ij)=bfra(ij)
          END DO
          ifsnw=0
       ELSE IF (ifsnw == 2.OR. &
            (ifsnw == 3.AND.tod == 0.0.AND.ifday == 0)) THEN
          OPEN(nfsnw, file=TRIM(fNameSnow), &
               ACTION="read",FORM="UNFORMATTED")  
          CALL ReadGetSNW(nfsnw,bfra)
          CLOSE(nfsnw)
          gmax=-1.0e10
          gmin=+1.0e10
          DO ij=1,ncols*jmax
             gsnw(ij)=bfra(ij)
             gmax=MAX(gmax,gsnw(ij))
             gmin=MIN(gmin,gsnw(ij))
          END DO
          IF (ifsnw == 3.AND.tod == 0.0.AND.ifday == 0) THEN
             ifsnw=0
          END IF
          IF (ifprt(23) >= 1) THEN
             WRITE(nfprt,444) gmax,gmin
          END IF
       ELSE
          WRITE(nfprt,555)
       END IF
    END IF


    ! process soil moisture fiel


    IF (ifslm /= 0) THEN
       IF (ifslm == 1) THEN
          OPEN(nfslm, file=TRIM(fNameSoilms), &
               ACTION="read",FORM="UNFORMATTED")         
          month=idate(2)
          DO mm=1,month
             CALL ReadGetSLM(nfslm,bfr1)
          END DO
          CLOSE (nfslm)
          DO ij=1,ncols*jmax
             gslm(ij)=bfr1(ij)
          END DO
          ifslm=0
       ELSE IF (ifslm == 2.OR. &
            (ifslm == 3.AND.tod == 0.0.AND.ifday == 0)) THEN
          OPEN(nfslm, file=TRIM(fNameSoilms), &
               ACTION="read",FORM="UNFORMATTED")         
          DO mm=1,mf
             CALL ReadGetSLM(nfslm,bfr1)
          END DO
          IF (mf == 12) THEN
             REWIND nfslm
          END IF
          CALL ReadGetSLM(nfslm,bfr2)
          CLOSE(nfslm)
          gmax=-1.0e10
          gmin=+1.0e10
          DO ij=1,ncols*jmax
             gslm(ij)=f2*bfr1(ij)+f1*bfr2(ij)
             gmax=MAX(gmax,gslm(ij))
             gmin=MIN(gmin,gslm(ij))
          END DO
          IF (ifslm == 3.AND.tod == 0.0.AND.ifday == 0) THEN
             ifslm=0
          END IF
          IF (ifprt(23) >= 1) THEN
             WRITE(nfprt,222) mf,f1,f2,gmax,gmin
          END IF
       ELSE
          WRITE(nfprt,333)
       END IF
    END IF

222 FORMAT(' SOILM   START MONTH=',i2,'  F1,F2=',2f6.3,'  MAX,MIN=',2e12.5)
333 FORMAT(' ABNORMAL END IN SUBR.GETSBC AT SOILM  INTERPOLATION')
444 FORMAT(' SNOW HAS ONLY ONE FILE','  MAX,MIN=',2E12.5)
555 FORMAT(' ABNORMAL END IN SUBR.GETSBC AT SNOW   INTERPOLATION')
666 FORMAT(' SST     START MONTH=',I2, &
         '  F1,F2=',2G13.6,'  MAX,MIN=',2G12.5)
667 FORMAT(' SST:  IFSST=',I2,'  MAX,MIN=',2G12.5)
888 FORMAT(' ALBEDO  START MONTH=',I2, &
         '  F1,F2=',2F6.3,'  MAX,MIN=',2E12.5)
999 FORMAT(' ABNORMAL END IN SUBR.GETSBC AT ALBEDO INTERPOLATION')
1999 FORMAT('ABNORMAL END IN SUBR.GETSBC AT SST   INTERPOLATION')
  END SUBROUTINE getsbc



  ! gread : reads in history carrying variables for one time step,
  !         surface geopotential, and sigma coordinate levels.
  !         checks sigma coordinate levels for consistency.



  SUBROUTINE gread(n, ifday, tod, idate, idatec, &
       qgzs, qlnp, qtmp, qdiv, qrot, qq, sl, si)
    INTEGER, INTENT(IN ) :: n
    INTEGER, INTENT(OUT) :: ifday
    REAL,    INTENT(OUT) :: tod
    INTEGER, INTENT(OUT) :: idate(4)
    INTEGER, INTENT(OUT) :: idatec(4)
    REAL,    INTENT(OUT) :: qgzs(2*mnMax)
    REAL,    INTENT(OUT) :: qlnp(2*mnMax)
    REAL,    INTENT(OUT) :: qtmp(2*mnMax,kMax)
    REAL,    INTENT(OUT) :: qdiv(2*mnMax,kMax)
    REAL,    INTENT(OUT) :: qrot(2*mnMax,kMax)
    REAL,    INTENT(OUT) :: qq  (2*mnMax,kMax)
    REAL,    INTENT(IN ) :: si(kMax+1)
    REAL,    INTENT(IN ) :: sl(kMax)

    INTEGER              :: k
    REAL                 :: dphi(kMax+1)
    REAL                 :: dlam(kMax)
    LOGICAL, PARAMETER :: toDiag=.TRUE.
    REAL                 :: aux1(2*mnMax)
    REAL                 :: aux2(2*mnMax,kMax) 
    REAL                 :: aux3(2*mnMax,kMax)
    !
    !     spectral data file format
    !     hour,idate(4),si( kMax+1 ),sl( kMax )
    !     zln qlnp qtmp qdiv qrot
    !
    IF(ifprt(35).GE.1)WRITE(nfprt,999) n,kMax,kMax+1
    CALL GReadHead(n, ifday, tod, idate, idatec, dphi, dlam, kMax)

    CALL GReadField(n, aux1)
    CALL TransDiagCol(aux1, qgzs, toDiag)        

    CALL GReadField(n, aux1)
    CALL TransDiagCol(aux1, qlnp, toDiag)    

    CALL GReadField(n, aux2)
    CALL TransDiagCol(aux2, qtmp, toDiag)

    DO k = 1, kMax
       CALL GReadField(n, aux2(:,k))
       CALL GReadField(n, aux3(:,k))
    END DO
    CALL TransDiagCol(aux2, qdiv, toDiag)
    CALL TransDiagCol(aux3, qrot, toDiag)   

    CALL GReadField(n, aux2)
    CALL TransDiagCol(aux2, qq, toDiag)
    CLOSE(n)

    !cdir novector
    DO k=1, kMax
       dlam(k)=dlam(k)-sl(k)
    END DO
    IF(ifprt(35).GE.1)WRITE(nfprt,100) (dlam(k),k=1, kMax )
    DO k=1, kMax+1
       dphi(k)=dphi(k)-si(k)
    END DO
    IF(ifprt(35).GE.1)WRITE(nfprt,100) (dphi(k),k=1, kMax+1 )
    IF(ifprt(35).GE.1)WRITE(nfprt,101) n,ifday,tod,idate,idatec
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
    REAL,    INTENT(OUT) :: tod
    INTEGER, INTENT(OUT) :: idate(4)
    INTEGER, INTENT(OUT) :: idatec(4)
    REAL,    INTENT(OUT) :: qgzs(2*mnMax)
    REAL,    INTENT(OUT) :: qlnp(2*mnMax)
    REAL,    INTENT(OUT) :: qtmp(2*mnMax,kMax)
    REAL,    INTENT(OUT) :: qdiv(2*mnMax,kMax)
    REAL,    INTENT(OUT) :: qrot(2*mnMax,kMax)
    REAL,    INTENT(OUT) :: qq  (2*mnMax,kMax)
    REAL,    INTENT(IN)  :: si(kMax+1)
    REAL,    INTENT(IN)  :: sl(kMax)
    LOGICAL, INTENT(IN ) :: dodyn
    INTEGER, INTENT(IN ) :: nfdyn

    INTEGER              :: k
    REAL                 :: dphi(kMax+1)
    REAL                 :: dlam(kMax)

    LOGICAL, PARAMETER   :: toDiag=.TRUE.

    REAL                 :: aux1(2*mnMax)
    REAL                 :: aux2(2*mnMax,kMax)
    REAL                 :: aux3(2*mnMax,kMax)

    INTEGER*4            :: ifday4
    INTEGER*4            :: idat4(4)
    INTEGER*4            :: idat4c(4)
    REAL*4               :: tod4
    REAL*4               :: dph4(kmax+1)
    REAL*4               :: dla4(kmax)
    INTEGER, SAVE        :: ifdyn = 0
    !
    !     spectral data file format
    !     hour,idate(4),si( kmax+1 ),sl( kmax )
    !     zln qlnp qtmp qdiv qrot
    !     
    IF(ifprt(35).GE.1)WRITE(nfprt,999) n,kmax,kmax+1
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

    CALL ReadField(n, aux1)
    CALL TransDiagCol(aux1, qgzs, toDiag)

    IF (ifdyn .EQ. 0) THEN
       ifdyn=1
       IF (dodyn) THEN
          WRITE (6,'(A,I5,A,F15.2,A)') ' ifday=',ifday4,' tod=',tod4,' dyn'
          WRITE (nfdyn) ifday4,tod4
          WRITE (nfdyn) aux1
       END IF
    END IF

    CALL ReadField(n, aux1)
    CALL TransDiagCol(aux1, qlnp, toDiag)

    CALL ReadField(n, aux2)
    CALL TransDiagCol(aux2, qtmp, toDiag)

    DO k = 1, kMax
       CALL ReadField(n, aux2(:,k))
       CALL ReadField(n, aux3(:,k))
    END DO
    CALL TransDiagCol(aux2, qdiv, toDiag)
    CALL TransDiagCol(aux3, qrot, toDiag)

    CALL ReadField(n, aux2)
    CALL TransDiagCol(aux2, qq, toDiag)
    CLOSE(n)

    DO k=1, kmax
       dlam(k)=dlam(k)-sl(k)
    END DO

    IF(ifprt(35).GE.1)WRITE(nfprt,100) (dlam(k),k=1, kmax )
    DO k=1, kmax+1
       dphi(k)=dphi(k)-si(k)
    END DO
    IF(ifprt(35).GE.1)WRITE(nfprt,100) (dphi(k),k=1, kmax+1 )
    IF(ifprt(35).GE.1)WRITE(nfprt,101) n,ifday,tod,idate,idatec
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
    REAL,    INTENT(IN) :: tod
    INTEGER, INTENT(IN) :: idate(4)
    INTEGER, INTENT(IN) :: idatec(4)
    REAL,    INTENT(IN) :: qgzs (2*mnMax)
    REAL,    INTENT(IN) :: qlnp (2*mnMax)
    REAL,    INTENT(IN) :: qtmp (2*mnMax,kMax)
    REAL,    INTENT(IN) :: qdiv (2*mnMax,kMax)
    REAL,    INTENT(IN) :: qrot (2*mnMax,kMax)
    REAL,    INTENT(IN) :: qq   (2*mnMax,kMax)
    REAL,    INTENT(IN) :: si(kMax+1)
    REAL,    INTENT(IN) :: sl(kMax)

    INTEGER             :: k
    LOGICAL, PARAMETER  :: toCol=.FALSE.
    REAL                :: aux1(2*mnMax)
    REAL                :: aux2(2*mnMax,kMax)
    REAL                :: aux3(2*mnMax,kMax)

    CALL GWriteHead(n, ifday, tod, idate, idatec, si, sl)

    CALL TransDiagCol(qgzs, aux1, toCol)
    CALL GWriteField(n, aux1)   

    CALL TransDiagCol(qlnp, aux1, toCol)
    CALL GWriteField(n, aux1)

    CALL TransDiagCol(qtmp, aux2, toCol)
    CALL GWriteField(n, aux2)

    CALL TransDiagCol(qdiv, aux2, toCol)
    CALL TransDiagCol(qrot, aux3, toCol)
    DO k = 1, kMax
       CALL GWriteField(n, aux2(:,k))
       CALL GWriteField(n, aux3(:,k))
    END DO

    CALL TransDiagCol(qq, aux2, toCol)
    CALL GWriteField(n, aux2)

    IF(ifprt(43).GE.1)WRITE(nfprt,3001)ifday,tod,idate,idatec,n
3001 FORMAT(' GWRITE IFDAY=',I8,' TOD=',F8.1,2(2X,3I3,I5), 2X,'N=',I2)
  END SUBROUTINE gwrite
END MODULE InputOutput
