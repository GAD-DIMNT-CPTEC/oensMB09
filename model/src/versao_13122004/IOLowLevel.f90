!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE IOLowLevel

  USE Constants, ONLY: &
       r4,i4, r8, i8, ndavl, ndrq, ncdg

  USE Options, ONLY: &
       nfprt, ifprt

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: ReadHead  
  PUBLIC :: GReadHead
  PUBLIC :: ReadField
  PUBLIC :: GReadField
  PUBLIC :: WriteHead  
  PUBLIC :: GWriteHead
  PUBLIC :: WriteField
  PUBLIC :: GWriteField
  PUBLIC :: WriteDir
  PUBLIC :: WriteDire
  PUBLIC :: ReadProgHead 
  PUBLIC :: GReadProgHead
  PUBLIC :: WriteProgHead
  PUBLIC :: ReadLandSeaMask
  PUBLIC :: ReadLandSeaMask2
  PUBLIC :: GetUnit
  PUBLIC :: ReadVar
  PUBLIC :: ReadGetALB
  PUBLIC :: ReadGetSST
  PUBLIC :: ReadGetSST2
  PUBLIC :: ReadGetSLM  
  PUBLIC :: ReadGetSNW
  PUBLIC :: ReadGetNFTGZ
  PUBLIC :: InitReadWriteSpec
  PUBLIC :: WriteDiagHead
  PUBLIC :: LandSeaMask
  PUBLIC :: WriteGrdHist

  INTERFACE WriteDiagHead
     MODULE PROCEDURE WriteDiagHead4, WriteDiagHead8
  END INTERFACE
  INTERFACE ReadGetNFTGZ
     MODULE PROCEDURE ReadNFTGZ4, ReadNFTGZ8
  END INTERFACE
  INTERFACE ReadGetSNW 
     MODULE PROCEDURE ReadSNW4, ReadSNW8
  END INTERFACE
  INTERFACE ReadGetSLM 
     MODULE PROCEDURE ReadSLM4, ReadSLM8
  END INTERFACE
  INTERFACE ReadGetALB
     MODULE PROCEDURE ReadAlb4, ReadAlb8
  END INTERFACE
  INTERFACE ReadGetSST 
     MODULE PROCEDURE ReadSST4, ReadSST8 
  END INTERFACE
  INTERFACE ReadGetSST2 
     MODULE PROCEDURE ReadSST4Rec, ReadSST8Rec
  END INTERFACE
  INTERFACE ReadVar 
     MODULE PROCEDURE ReadVar4, ReadVar8
  END INTERFACE
  INTERFACE GReadHead
     MODULE PROCEDURE GReadHead4, GReadHead8
  END INTERFACE
  INTERFACE ReadHead
     MODULE PROCEDURE ReadHead4, ReadHead8
  END INTERFACE
  INTERFACE WriteHead
     MODULE PROCEDURE WriteHead4, WriteHead8
  END INTERFACE
  INTERFACE GWriteHead
     MODULE PROCEDURE GWriteHead4, GWriteHead8
  END INTERFACE
  INTERFACE ReadProgHead
     MODULE PROCEDURE ReadProgHead4, ReadProgHead8
  END INTERFACE
  INTERFACE GReadProgHead
     MODULE PROCEDURE GReadProgHead4, GReadProgHead8
  END INTERFACE
  INTERFACE WriteProgHead
     MODULE PROCEDURE WriteProgHead4, WriteProgHead8
  END INTERFACE
  INTERFACE WriteDir
     MODULE PROCEDURE WriteDir4, WriteDir8
  END INTERFACE
  INTERFACE WriteDire
     MODULE PROCEDURE WriteDire4, WriteDire8 
  END INTERFACE
  INTERFACE GReadField
     MODULE PROCEDURE GReadField41D, GReadField42D, GReadField81D, GReadField82D
  END INTERFACE
  INTERFACE ReadField
     MODULE PROCEDURE ReadField41D, ReadField42D, ReadField81D, ReadField82D
  END INTERFACE
  INTERFACE WriteField
     MODULE PROCEDURE WriteField41D, WriteField42D, WriteField81D, WriteField82D
  END INTERFACE
  INTERFACE GWriteField
     MODULE PROCEDURE GWriteField41D, GWriteField42D, GWriteField81D, GWriteField82D
  END INTERFACE
  INTERFACE ReadLandSeaMask
     MODULE PROCEDURE ReadLandSeaMask4, ReadLandSeaMask8
  END INTERFACE
  INTERFACE ReadLandSeaMask2
     MODULE PROCEDURE ReadLandSeaMask2_4, ReadLandSeaMask2_8
  END INTERFACE
  INTERFACE LandSeaMask
     MODULE PROCEDURE LandSeaMask4, LandSeaMask8
  END INTERFACE
  INTERFACE GetUnit
     MODULE PROCEDURE GetUnit4, GetUnit8
  END INTERFACE
  INTERFACE  WriteGrdHist
      MODULE PROCEDURE WriteGrdH4, WriteGrdH8
  END INTERFACE 

  CHARACTER(len=40), ALLOCATABLE :: reqdg(:)
  CHARACTER(len=40), ALLOCATABLE :: combf(:)
  LOGICAL   , ALLOCATABLE :: dodia(:)
  INTEGER   , ALLOCATABLE :: itavl(:)
  INTEGER   , ALLOCATABLE :: iavrq(:)
  INTEGER   , ALLOCATABLE :: nucf (:)
  INTEGER   , ALLOCATABLE :: lvrq (:)
  INTEGER   , ALLOCATABLE :: nurq (:)
  INTEGER   , ALLOCATABLE :: lvcf (:)
  INTEGER   , ALLOCATABLE :: itcf (:)
  INTEGER    :: mxavl   
  INTEGER    :: icf  
  INTEGER    :: mMax 
  INTEGER    :: mnMax
  INTEGER    :: kMax 
  INTEGER    :: ijMax
  INTEGER    :: iMax 
  INTEGER    :: jMax 
  INTEGER    :: ibMax
  INTEGER    :: jbMax

CONTAINS
  SUBROUTINE InitReadWriteSpec(&
       mxavl_in,icf_in  ,mMax_in , &
       mnMax_in,kMax_in ,ijMax_in,iMax_in ,jMax_in ,ibMax_in, &
       jbMax_in, &
       reqdg_in,combf_in,dodia_in,itavl_in,iavrq_in,&
       nucf_in ,lvrq_in ,nurq_in ,lvcf_in ,itcf_in )

    CHARACTER(len=40), INTENT(IN   ) :: reqdg_in(:)
    CHARACTER(len=40), INTENT(IN   ) :: combf_in(:)
    LOGICAL(KIND=i8), INTENT(IN   ) :: dodia_in(:)
    INTEGER(KIND=i8) , INTENT(IN   ) :: itavl_in(:)
    INTEGER(KIND=i8) , INTENT(IN   ) :: iavrq_in(:)
    INTEGER(KIND=i8) , INTENT(IN   ) :: nucf_in (:)
    INTEGER(KIND=i8) , INTENT(IN   ) :: lvrq_in (:)
    INTEGER(KIND=i8) , INTENT(IN   ) :: nurq_in (:)
    INTEGER(KIND=i8) , INTENT(IN   ) :: lvcf_in (:)
    INTEGER(KIND=i8) , INTENT(IN   ) :: itcf_in (:)
    INTEGER(KIND=i8) , INTENT(IN   ) :: mxavl_in   
    INTEGER(KIND=i8) , INTENT(IN   ) :: icf_in  
    INTEGER(KIND=i8) , INTENT(IN   ) :: mMax_in 
    INTEGER(KIND=i8) , INTENT(IN   ) :: mnMax_in
    INTEGER(KIND=i8) , INTENT(IN   ) :: kMax_in 
    INTEGER(KIND=i8) , INTENT(IN   ) :: ijMax_in
    INTEGER(KIND=i8) , INTENT(IN   ) :: iMax_in 
    INTEGER(KIND=i8) , INTENT(IN   ) :: jMax_in 
    INTEGER(KIND=i8) , INTENT(IN   ) :: ibMax_in
    INTEGER(KIND=i8) , INTENT(IN   ) :: jbMax_in

    ALLOCATE(reqdg(ndrq))
    ALLOCATE(combf(ncdg))  
    ALLOCATE(dodia(ndavl))
    ALLOCATE(itavl(ndavl))
    ALLOCATE(iavrq(ndavl))
    ALLOCATE(nucf (ncdg ))
    ALLOCATE(lvrq (ndrq ))
    ALLOCATE(nurq (ndrq ))
    ALLOCATE(lvcf (ncdg ))
    ALLOCATE(itcf (ncdg )) 
    mxavl =   mxavl_in
    icf   =     icf_in  
    mMax  =    mMax_in 
    mnMax =   mnMax_in
    kMax  =    kMax_in 
    ijMax =   ijMax_in
    iMax  =    iMax_in 
    jMax  =    jMax_in 
    ibMax =   ibMax_in
    jbMax =   jbMax_in

    reqdg =  reqdg_in
    combf =  combf_in
    dodia =  dodia_in
    itavl =  itavl_in
    iavrq =  iavrq_in
    nucf  =  nucf_in 
    lvrq  =  lvrq_in 
    nurq  =  nurq_in 
    lvcf  =  lvcf_in 
    itcf  =  itcf_in 

  END SUBROUTINE InitReadWriteSpec

  SUBROUTINE ReadHead4(n, ifday, tod, idate, idatec, si, sl, kMax)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    INTEGER(KIND=i8), INTENT(IN)  :: kMax
    INTEGER(KIND=i4), INTENT(OUT) :: ifday
    REAL   (KIND=r4), INTENT(OUT) :: tod
    INTEGER(KIND=i4), INTENT(OUT) :: idate(4)
    INTEGER(KIND=i4), INTENT(OUT) :: idatec(4)
    REAL   (KIND=r4), INTENT(OUT) :: si(:)
    REAL   (KIND=r4), INTENT(OUT) :: sl(:)
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadHead4)**"
    INTEGER(KIND=i8) :: lMax
    lMax=SIZE(sl)
    IF (kMax /= lMax) THEN
      WRITE (0, '(2(A,I3))') ' kMax = ',kMax, ' is /= lMax', lMax
      STOP h
    END IF
    READ(n)ifday, tod, idate, idatec, si, sl
  END SUBROUTINE ReadHead4
  SUBROUTINE ReadHead8(n, ifday, tod, idate, idatec, si, sl, kMax)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    INTEGER(KIND=i8), INTENT(IN)  :: kMax
    INTEGER(KIND=i8), INTENT(OUT) :: ifday
    REAL   (KIND=r8), INTENT(OUT) :: tod
    INTEGER(KIND=i8), INTENT(OUT) :: idate(4)
    INTEGER(KIND=i8), INTENT(OUT) :: idatec(4)
    REAL   (KIND=r8), INTENT(OUT) :: si(:)
    REAL   (KIND=r8), INTENT(OUT) :: sl(:)
    INTEGER(KIND=i4) :: iaux(10)
    REAL   (KIND=r4) :: raux1(kmax), raux2(kmax+1)
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadHead8)**"
    INTEGER(KIND=i8) :: lMax
    lMax=SIZE(sl)
    IF (kMax /= lMax) THEN
      WRITE (0, '(2(A,I3))') ' kMax = ',kMax, ' is /= lMax', lMax
      STOP h
    END IF
    READ(n)iaux, raux2, raux1
    ifday  = INT(iaux(  1 ), i8)
    tod    = INT(iaux(  2 ), r8)
    idate  = INT(iaux(3:6 ), i8)
    idatec = INT(iaux(7:10), i8)
    si     = REAL(raux2 , r8)
    sl     = REAL(raux1 , r8)
  END SUBROUTINE ReadHead8
  SUBROUTINE GReadHead4(n, ifday, tod, idate, idatec, si, sl, kMax)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    INTEGER(KIND=i8), INTENT(IN)  :: kMax
    INTEGER(KIND=i4), INTENT(OUT) :: ifday
    REAL   (KIND=r4), INTENT(OUT) :: tod
    INTEGER(KIND=i4), INTENT(OUT) :: idate(4)
    INTEGER(KIND=i4), INTENT(OUT) :: idatec(4)
    REAL   (KIND=r4), INTENT(OUT) :: si(:)
    REAL   (KIND=r4), INTENT(OUT) :: sl(:)
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadHead4)**"
    INTEGER(KIND=i8) :: lMax
    lMax=SIZE(sl)
    IF (kMax /= lMax) THEN
      WRITE (0, '(2(A,I3))') ' kMax = ',kMax, ' is /= lMax', lMax
      STOP h
    END IF
    READ(n)ifday, tod, idate, idatec, si, sl
  END SUBROUTINE GReadHead4
  SUBROUTINE GReadHead8(n, ifday, tod, idate, idatec, si, sl, kMax)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    INTEGER(KIND=i8), INTENT(IN)  :: kMax
    INTEGER(KIND=i8), INTENT(OUT) :: ifday
    REAL   (KIND=r8), INTENT(OUT) :: tod
    INTEGER(KIND=i8), INTENT(OUT) :: idate(4)
    INTEGER(KIND=i8), INTENT(OUT) :: idatec(4)
    REAL   (KIND=r8), INTENT(OUT) :: si(:)
    REAL   (KIND=r8), INTENT(OUT) :: sl(:)
    INTEGER(KIND=i8) :: iaux(10)
    REAL   (KIND=r8) :: raux1(kmax), raux2(kmax+1)
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadHead8)**"
    INTEGER(KIND=i8) :: lMax
    lMax=SIZE(sl)
    IF (kMax /= lMax) THEN
      WRITE (0, '(2(A,I3))') ' kMax = ',kMax, ' is /= lMax', lMax
      STOP h
    END IF
    READ(n)iaux, raux2, raux1
    ifday  = INT(iaux(  1 ), i8)
    tod    = INT(iaux(  2 ), r8)
    idate  = INT(iaux(3:6 ), i8)
    idatec = INT(iaux(7:10), i8)
    si     = REAL(raux2 , r8)
    sl     = REAL(raux1 , r8)
  END SUBROUTINE GReadHead8

  SUBROUTINE ReadField42D(n, field)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    REAL   (KIND=r4), INTENT(OUT) :: field(:,:)
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadField42D)**"
    INTEGER :: k
    INTEGER :: d2
    d2 = SIZE(field,2)
    DO k = 1, d2
       READ(n)field(:,k)
    END DO
  END SUBROUTINE ReadField42D
  SUBROUTINE ReadField82D(n, field)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    REAL   (KIND=r8), INTENT(OUT) :: field(:,:)
    REAL   (KIND=r4) :: raux3(SIZE(field,1))
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadField82D)**"
    INTEGER :: k
    INTEGER :: d2
    d2 = SIZE(field,2)
    DO k=1, d2
       READ(n)raux3
       field(:,k) = REAL(raux3, r8)
    END DO
  END SUBROUTINE ReadField82D
  SUBROUTINE ReadField41D(n, field)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    REAL   (KIND=r4), INTENT(OUT) :: field(:)
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadField41D)**"
    READ(n)field
  END SUBROUTINE ReadField41D
  SUBROUTINE ReadField81D(n, field)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    REAL   (KIND=r8), INTENT(OUT) :: field(:)
    REAL   (KIND=r4) :: raux3(SIZE(field))
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadField81D)**"
    READ(n)raux3
    field = REAL(raux3, r8)
  END SUBROUTINE ReadField81D


  SUBROUTINE GReadField42D(n, field)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    REAL   (KIND=r4), INTENT(OUT) :: field(:,:)
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadField42D)**"
    INTEGER :: k
    INTEGER :: d2
    d2 = SIZE(field,2)
    DO k = 1, d2
       READ(n)field(:,k)
    END DO
  END SUBROUTINE GReadField42D
  SUBROUTINE GReadField82D(n, field)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    REAL   (KIND=r8), INTENT(OUT) :: field(:,:)
    REAL   (KIND=r8) :: raux3(SIZE(field,1))
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadField82D)**"
    INTEGER :: k
    INTEGER :: d2
    d2 = SIZE(field,2)
    DO k=1, d2
       READ(n)raux3
       field(:,k) =raux3
    END DO
  END SUBROUTINE GReadField82D
  SUBROUTINE GReadField41D(n, field)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    REAL   (KIND=r4), INTENT(OUT) :: field(:)
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadField41D)**"
    READ(n)field
  END SUBROUTINE GReadField41D
  SUBROUTINE GReadField81D(n, field)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    REAL   (KIND=r8), INTENT(OUT) :: field(:)
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadField81D)**"
    READ(n)field
  END SUBROUTINE GReadField81D

  SUBROUTINE WriteHead4(n, ifday, tod, idate, idatec, si, sl)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    INTEGER(KIND=i4), INTENT(IN)  :: ifday
    REAL   (KIND=r4), INTENT(IN)  :: tod
    INTEGER(KIND=i4), INTENT(IN)  :: idate(4)
    INTEGER(KIND=i4), INTENT(IN)  :: idatec(4)
    REAL   (KIND=r4), INTENT(IN)  :: si(:)
    REAL   (KIND=r4), INTENT(IN)  :: sl(:)
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteHead4)**"
    WRITE(n)ifday, tod, idate, idatec, si, sl
  END SUBROUTINE WriteHead4
  SUBROUTINE WriteHead8(n, ifday, tod, idate, idatec, si, sl)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    INTEGER(KIND=i8), INTENT(IN)  :: ifday
    REAL   (KIND=r8), INTENT(IN)  :: tod
    INTEGER(KIND=i8), INTENT(IN)  :: idate(4)
    INTEGER(KIND=i8), INTENT(IN)  :: idatec(4)
    REAL   (KIND=r8), INTENT(IN)  :: si(:)
    REAL   (KIND=r8), INTENT(IN)  :: sl(:)
    INTEGER(KIND=i4) :: iaux(10)
    REAL   (KIND=r4) :: raux1(kmax), raux2(kmax+1)
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteHead8)**"
    iaux(  1 ) = INT (ifday , i4)
    iaux(  2 ) = INT (tod   , i4)
    iaux(3:6 ) = INT (idate , i4)
    iaux(7:10) = INT (idatec, i4)
    raux2      = REAL(si    , r4)
    raux1      = REAL(sl    , r4)
    WRITE(n)iaux, raux2, raux1
  END SUBROUTINE WriteHead8

  SUBROUTINE GWriteHead4(n, ifday, tod, idate, idatec, si, sl)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    INTEGER(KIND=i4), INTENT(IN)  :: ifday
    REAL   (KIND=r4), INTENT(IN)  :: tod
    INTEGER(KIND=i4), INTENT(IN)  :: idate(4)
    INTEGER(KIND=i4), INTENT(IN)  :: idatec(4)
    REAL   (KIND=r4), INTENT(IN)  :: si(:)
    REAL   (KIND=r4), INTENT(IN)  :: sl(:)
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteHead4)**"
    WRITE(n)ifday, tod, idate, idatec, si, sl
  END SUBROUTINE GWriteHead4
  SUBROUTINE GWriteHead8(n, ifday, tod, idate, idatec, si, sl)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    INTEGER(KIND=i8), INTENT(IN)  :: ifday
    REAL   (KIND=r8), INTENT(IN)  :: tod
    INTEGER(KIND=i8), INTENT(IN)  :: idate(4)
    INTEGER(KIND=i8), INTENT(IN)  :: idatec(4)
    REAL   (KIND=r8), INTENT(IN)  :: si(:)
    REAL   (KIND=r8), INTENT(IN)  :: sl(:)
    INTEGER(KIND=i8) :: iaux(10)
    REAL   (KIND=r8) :: raux1(kmax), raux2(kmax+1)
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteHead8)**"
    iaux(  1 ) = INT (ifday , i8)
    iaux(  2 ) = INT (tod   , r8)
    iaux(3:6 ) = INT (idate , i8)
    iaux(7:10) = INT (idatec, i8)
    raux2      = REAL(si    , r8)
    raux1      = REAL(sl    , r8)
    WRITE(n)iaux, raux2, raux1
  END SUBROUTINE GWriteHead8

  SUBROUTINE WriteField42D(n, field)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    REAL   (KIND=r4), INTENT(IN)  :: field(:,:)
    REAL   (KIND=r4) :: raux3(SIZE(field,1))
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteField42D)**"
    INTEGER :: k, l
    INTEGER :: d1, d2
    d2=SIZE(field,2);d1=SIZE(field,1)
    DO k = 1, d2
       DO l = 1, d1
          raux3(l) = field(l,k)
       END DO
       WRITE(n)raux3(:)
    END DO
  END SUBROUTINE WriteField42D
  SUBROUTINE WriteField82D(n, field)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    REAL   (KIND=r8), INTENT(IN)  :: field(:,:)
    REAL   (KIND=r4) :: raux3(SIZE(field,1))
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteField82D)**"
    INTEGER :: k, l
    INTEGER :: d1, d2
    d2=SIZE(field,2);d1=SIZE(field,1)
    DO k = 1, d2
       DO l = 1, d1
          raux3(l) = REAL(field(l,k),r4)
       END DO
       WRITE(n)raux3(:)
    END DO
  END SUBROUTINE WriteField82D


  SUBROUTINE WriteField41D(n, field)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    REAL   (KIND=r4), INTENT(IN)  :: field(:)
    REAL   (KIND=r4) :: raux3(SIZE(field,1))
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteField41D)**"
    INTEGER :: l
    INTEGER :: d1
    d1=SIZE(field,1)
    DO l = 1, d1
       raux3(l) = field(l)
    END DO
    WRITE(n)raux3(:)
  END SUBROUTINE WriteField41D
  SUBROUTINE WriteField81D(n, field)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    REAL   (KIND=r8), INTENT(IN)  :: field(:)
    REAL   (KIND=r4) :: raux3(SIZE(field,1))
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteField81D)**"
    INTEGER :: l
    INTEGER :: d1
    d1=SIZE(field,1)
    DO l = 1, d1
       raux3(l) = REAL(field(l),r4)
    END DO
    WRITE(n)raux3(:)
  END SUBROUTINE WriteField81D


  SUBROUTINE GWriteField42D(n, field)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    REAL   (KIND=r4), INTENT(IN)  :: field(:,:)
    REAL   (KIND=r4) :: raux3(SIZE(field,1))
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteField42D)**"
    INTEGER :: k, l
    INTEGER :: d1, d2
    d2=SIZE(field,2);d1=SIZE(field,1)
    DO k = 1, d2
       DO l = 1, d1
          raux3(l) = field(l,k)
       END DO
       WRITE(n)raux3(:)
    END DO
  END SUBROUTINE GWriteField42D
  SUBROUTINE GWriteField82D(n, field)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    REAL   (KIND=r8), INTENT(IN)  :: field(:,:)
    REAL   (KIND=r8) :: raux3(SIZE(field,1))
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteField82D)**"
    INTEGER :: k, l
    INTEGER :: d1, d2
    d2=SIZE(field,2);d1=SIZE(field,1)
    DO k = 1, d2
       DO l = 1, d1
          raux3(l) = field(l,k)
       END DO
       WRITE(n)raux3(:)
    END DO
  END SUBROUTINE GWriteField82D


  SUBROUTINE GWriteField41D(n, field)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    REAL   (KIND=r4), INTENT(IN)  :: field(:)
    REAL   (KIND=r4) :: raux3(SIZE(field,1))
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteField41D)**"
    INTEGER :: l
    INTEGER :: d1
    d1=SIZE(field,1)
    DO l = 1, d1
       raux3(l) = field(l)
    END DO
    WRITE(n)raux3(:)
  END SUBROUTINE GWriteField41D
  SUBROUTINE GWriteField81D(n, field)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    REAL   (KIND=r8), INTENT(IN)  :: field(:)
    REAL   (KIND=r8) :: raux3(SIZE(field,1))
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteField81D)**"
    INTEGER :: l
    INTEGER :: d1
    d1=SIZE(field,1)
    DO l = 1, d1
       raux3(l) = field(l)
    END DO
    WRITE(n)raux3(:)
  END SUBROUTINE GWriteField81D

  SUBROUTINE WriteDir4(n, idate, ihr, iday, mon, iyr, del,tod)
    INTEGER (KIND=i4), INTENT(IN ) :: n
    INTEGER (KIND=i4), INTENT(IN ) :: idate(4)
    INTEGER (KIND=i4), INTENT(IN ) :: ihr
    INTEGER (KIND=i4), INTENT(IN ) :: iday
    INTEGER (KIND=i4), INTENT(IN ) :: mon
    INTEGER (KIND=i4), INTENT(IN ) :: iyr
    REAL    (KIND=r4), INTENT(IN ) :: del(kMax)
    REAL    (KIND=r4), INTENT(IN ) :: tod
    INTEGER (KIND=i4)              :: isg(2)

    CHARACTER (LEN= 4) :: imdl
    CHARACTER (LEN=40) :: jttl
    CHARACTER (LEN=20), PARAMETER :: ittl='CPTEC SIGMA VERS 2.0'
    CHARACTER (LEN= 4), PARAMETER :: nexp='0001'
    CHARACTER (LEN=40), PARAMETER :: orogra = 'TOPOGRAPHY' 
    CHARACTER (LEN=40), PARAMETER :: lseamk = 'LAND SEA MASK'
    CHARACTER (LEN=40), PARAMETER :: lnsurf = 'LN SURFACE PRESSURE'
    CHARACTER (LEN=40), PARAMETER :: divrgn = 'DIVERGENCE'
    CHARACTER (LEN=40), PARAMETER :: vortic = 'VORTICITY'
    CHARACTER (LEN=40), PARAMETER :: spechu = 'SPECIFIC HUMIDITY'
    CHARACTER (LEN=40), PARAMETER :: tempvi = 'VIRTUAL TEMPERATURE'
    CHARACTER (LEN=40), PARAMETER :: surfte = 'SURFACE TEMPERATURE'
    CHARACTER (LEN=40), PARAMETER :: srfrou = 'ROUGHNESS LENGTH'
    CHARACTER (LEN=40), PARAMETER :: deepte = 'DEEP SOIL TEMPERATURE'
    CHARACTER (LEN=40), PARAMETER :: stcnpy = 'STORAGE ON CANOPY'
    CHARACTER (LEN=40), PARAMETER :: stgrnd = 'STORAGE ON GROUND'
    CHARACTER (LEN=40), PARAMETER :: wt1soi = 'SOIL WETNESS OF SURFACE'
    CHARACTER (LEN=40), PARAMETER :: wt2soi = 'SOIL WETNESS OF ROOT ZONE'
    CHARACTER (LEN=40), PARAMETER :: wt3soi = 'SOIL WETNESS OF DRAINAGE ZONE' 
    CHARACTER (LEN=29), PARAMETER :: fmt1='(A40,2X,A4,2X,I8,3X,I4,4X,I3)'
    CHARACTER (LEN=4 ), PARAMETER :: diag='DIAG'
    INTEGER :: m
    INTEGER :: nn
    INTEGER :: ix

    isg(1)=iMax*jMax
    isg(2)=2*mnMax

    jttl='CPTEC AGCM REVIS 1.0 2000  T   L    COLD'
    WRITE (jttl(29:31), '(i3.3)') mMax-1
    WRITE (jttl(33:34), '(i2.2)') kMax
    WRITE (imdl, '(A1,I3.3)') 'T', mMax-1

    WRITE (n, '(A20)')   ittl
    WRITE (n, '(A4,1X,A4,1X,A4,1X,11I5,1X,A4)') &
         nexp, 'SEQU', imdl, mMax, kmax, kmax, &
         ihr, iday, mon, iyr, idate, 'TAPE'
    WRITE (n, '(A40)')   jttl
    WRITE (n, '(5E16.8)')   del
    WRITE (n, fmt1) orogra, 'FIXD', 2*mnMax, 1, 10
    WRITE (n, fmt1) lseamk, 'FIXD', ijmax, 1, 0
    WRITE (n, fmt1) lnsurf, 'PROG', 2*mnMax, 1, 142
    WRITE (n, fmt1) divrgn, 'PROG', 2*mnMax, kmax, 50
    WRITE (n, fmt1) vortic, 'PROG', 2*mnMax, kmax, 50
    WRITE (n, fmt1) spechu, 'PROG', 2*mnMax, kmax, 0
    WRITE (n, fmt1) tempvi, 'PROG', 2*mnMax, kmax, 40
    WRITE (n, fmt1) srfrou, 'PROG', ijmax, 1, 10
    WRITE (n, fmt1) surfte, 'PROG', ijmax, 1, 40
    WRITE (n, fmt1) deepte, 'PROG', ijmax, 1, 40
    WRITE (n, fmt1) stcnpy, 'PROG', ijmax, 1, 10
    WRITE (n, fmt1) stgrnd, 'PROG', ijmax, 1, 10
    WRITE (n, fmt1) wt1soi, 'PROG', ijmax, 1, 0
    WRITE (n, fmt1) wt2soi, 'PROG', ijmax, 1, 0
    WRITE (n, fmt1) wt3soi, 'PROG', ijmax, 1, 0

    IF (iday == 0 .and. tod == 0.0) RETURN

    DO m=1,mxavl
       IF (dodia(m) .and. (iavrq(m) > 0)) THEN
          nn=iavrq(m)
          WRITE(n,160)reqdg(nn),diag,isg(itavl(m)),lvrq(nn),nurq(nn)
          IF(ifprt(91) >= 1)WRITE(nfprt,161) reqdg(nn),diag, &
               isg(itavl(m)),lvrq(nn),nurq(nn)
       END IF
    END DO

    IF(icf.ne.0)THEN
       DO ix=1,icf
          WRITE(n,160)combf(ix),diag,isg(itcf(ix)),lvcf(ix),nucf(ix)
          IF(ifprt(91) >= 1)WRITE(nfprt,161) combf(ix),diag, &
               isg(itcf(ix)),lvcf(ix),nucf(ix)
       END DO
    END IF
160 FORMAT(A40,2X,A4,2X,I8,3X,I4,4X,I3)
161 FORMAT(' ',A40,2X,A4,2X,I8,3X,I4,4X,I3)    
  END SUBROUTINE WriteDir4
  SUBROUTINE WriteDir8(n, idate, ihr, iday, mon, iyr, del,tod)
    INTEGER (KIND=i8), INTENT(IN ) :: n
    INTEGER (KIND=i8), INTENT(IN ) :: idate(4)
    INTEGER (KIND=i8), INTENT(IN ) :: ihr
    INTEGER (KIND=i8), INTENT(IN ) :: iday
    INTEGER (KIND=i8), INTENT(IN ) :: mon
    INTEGER (KIND=i8), INTENT(IN ) :: iyr
    REAL    (KIND=r8), INTENT(IN ) :: del(kMax) 
    REAL    (KIND=r8), INTENT(IN ) :: tod
    INTEGER (KIND=i8)              :: isg(2)

    CHARACTER (LEN= 4) :: imdl
    CHARACTER (LEN=40) :: jttl
    CHARACTER (LEN=20), PARAMETER :: ittl='CPTEC SIGMA VERS 2.0'
    CHARACTER (LEN= 4), PARAMETER :: nexp='0001'
    CHARACTER (LEN=40), PARAMETER :: orogra = 'TOPOGRAPHY' 
    CHARACTER (LEN=40), PARAMETER :: lseamk = 'LAND SEA MASK'
    CHARACTER (LEN=40), PARAMETER :: lnsurf = 'LN SURFACE PRESSURE'
    CHARACTER (LEN=40), PARAMETER :: divrgn = 'DIVERGENCE'
    CHARACTER (LEN=40), PARAMETER :: vortic = 'VORTICITY'
    CHARACTER (LEN=40), PARAMETER :: spechu = 'SPECIFIC HUMIDITY'
    CHARACTER (LEN=40), PARAMETER :: tempvi = 'VIRTUAL TEMPERATURE'
    CHARACTER (LEN=40), PARAMETER :: surfte = 'SURFACE TEMPERATURE'
    CHARACTER (LEN=40), PARAMETER :: srfrou = 'ROUGHNESS LENGTH'
    CHARACTER (LEN=40), PARAMETER :: deepte = 'DEEP SOIL TEMPERATURE'
    CHARACTER (LEN=40), PARAMETER :: stcnpy = 'STORAGE ON CANOPY'
    CHARACTER (LEN=40), PARAMETER :: stgrnd = 'STORAGE ON GROUND'
    CHARACTER (LEN=40), PARAMETER :: wt1soi = 'SOIL WETNESS OF SURFACE'
    CHARACTER (LEN=40), PARAMETER :: wt2soi = 'SOIL WETNESS OF ROOT ZONE'
    CHARACTER (LEN=40), PARAMETER :: wt3soi = 'SOIL WETNESS OF DRAINAGE ZONE' 
    CHARACTER (LEN=29), PARAMETER :: fmt1='(A40,2X,A4,2X,I8,3X,I4,4X,I3)'
    CHARACTER (LEN=4 ), PARAMETER :: diag='DIAG'
    INTEGER :: m
    INTEGER :: nn
    INTEGER :: ix

    isg(1)=iMax*jMax
    isg(2)=2*mnMax

    jttl='CPTEC AGCM REVIS 1.0 2000  T   L    COLD'
    WRITE (jttl(29:31), '(i3.3)') mMax-1
    WRITE (jttl(33:34), '(i2.2)') kMax
    WRITE (imdl, '(A1,I3.3)') 'T', mMax-1

    WRITE (n, '(A20)')   ittl
    WRITE (n, '(A4,1X,A4,1X,A4,1X,11I5,1X,A4)') &
         nexp, 'SEQU', imdl, mMax, kmax, kmax, &
         ihr, iday, mon, iyr, idate, 'TAPE'
    WRITE (n, '(A40)')   jttl
    WRITE (n, '(5E16.8)')   del
    WRITE (n, fmt1) orogra, 'FIXD', 2*mnMax, 1, 10
    WRITE (n, fmt1) lseamk, 'FIXD', ijmax, 1, 0
    WRITE (n, fmt1) lnsurf, 'PROG', 2*mnMax, 1, 142
    WRITE (n, fmt1) divrgn, 'PROG', 2*mnMax, kmax, 50
    WRITE (n, fmt1) vortic, 'PROG', 2*mnMax, kmax, 50
    WRITE (n, fmt1) spechu, 'PROG', 2*mnMax, kmax, 0
    WRITE (n, fmt1) tempvi, 'PROG', 2*mnMax, kmax, 40
    WRITE (n, fmt1) srfrou, 'PROG', ijmax, 1, 10
    WRITE (n, fmt1) surfte, 'PROG', ijmax, 1, 40
    WRITE (n, fmt1) deepte, 'PROG', ijmax, 1, 40
    WRITE (n, fmt1) stcnpy, 'PROG', ijmax, 1, 10
    WRITE (n, fmt1) stgrnd, 'PROG', ijmax, 1, 10
    WRITE (n, fmt1) wt1soi, 'PROG', ijmax, 1, 0
    WRITE (n, fmt1) wt2soi, 'PROG', ijmax, 1, 0
    WRITE (n, fmt1) wt3soi, 'PROG', ijmax, 1, 0

    IF (iday == 0 .and. tod == 0.0) RETURN

    DO m=1,mxavl
       IF (dodia(m) .and. (iavrq(m) > 0)) THEN
          nn=iavrq(m)
          WRITE(n,160)reqdg(nn),diag,isg(itavl(m)),lvrq(nn),nurq(nn)
          IF(ifprt(91) >= 1)WRITE(nfprt,161) reqdg(nn),diag, &
               isg(itavl(m)),lvrq(nn),nurq(nn)
       END IF
    END DO

    IF(icf.ne.0)THEN
       DO ix=1,icf
          WRITE(n,160)combf(ix),diag,isg(itcf(ix)),lvcf(ix),nucf(ix)
          IF(ifprt(91) >= 1)WRITE(nfprt,161) combf(ix),diag, &
               isg(itcf(ix)),lvcf(ix),nucf(ix)
       END DO
    END IF

160 FORMAT(A40,2X,A4,2X,I8,3X,I4,4X,I3)
161 FORMAT(' ',A40,2X,A4,2X,I8,3X,I4,4X,I3)
  END SUBROUTINE WriteDir8
  SUBROUTINE WriteDire4(n, idate, ihr, iday, mon, iyr, del,tod)
    INTEGER (KIND=i4), INTENT(IN ) :: n
    INTEGER (KIND=i4), INTENT(IN ) :: idate(4)
    INTEGER (KIND=i4), INTENT(IN ) :: ihr
    INTEGER (KIND=i4), INTENT(IN ) :: iday
    INTEGER (KIND=i4), INTENT(IN ) :: mon
    INTEGER (KIND=i4), INTENT(IN ) :: iyr
    REAL    (KIND=r4), INTENT(IN ) :: del(kMax)
    REAL    (KIND=r4), INTENT(IN ) :: tod
    REAL    (KIND=r4) :: r

    CHARACTER (LEN= 4) :: imdl
    CHARACTER (LEN=40) :: jttl
    CHARACTER (LEN=20), PARAMETER :: ittl='CPTEC SIGMA VERS 2.0'
    CHARACTER (LEN= 4), PARAMETER :: nexp='0001'
    CHARACTER (LEN=40), PARAMETER :: orogra = 'TOPOGRAPHY' 
    CHARACTER (LEN=40), PARAMETER :: lseamk = 'LAND SEA MASK'
    CHARACTER (LEN=40), PARAMETER :: lnsurf = 'LN SURFACE PRESSURE'
    CHARACTER (LEN=40), PARAMETER :: divrgn = 'DIVERGENCE'
    CHARACTER (LEN=40), PARAMETER :: vortic = 'VORTICITY'
    CHARACTER (LEN=40), PARAMETER :: spechu = 'SPECIFIC HUMIDITY'
    CHARACTER (LEN=40), PARAMETER :: tempvi = 'VIRTUAL TEMPERATURE'
    CHARACTER (LEN=40), PARAMETER :: surfte = 'SURFACE TEMPERATURE'
    CHARACTER (LEN=40), PARAMETER :: srfrou = 'ROUGHNESS LENGTH'
    CHARACTER (LEN=40), PARAMETER :: deepte = 'DEEP SOIL TEMPERATURE'
    CHARACTER (LEN=40), PARAMETER :: stcnpy = 'STORAGE ON CANOPY'
    CHARACTER (LEN=40), PARAMETER :: stgrnd = 'STORAGE ON GROUND'
    CHARACTER (LEN=40), PARAMETER :: wt1soi = 'SOIL WETNESS OF SURFACE'
    CHARACTER (LEN=40), PARAMETER :: wt2soi = 'SOIL WETNESS OF ROOT ZONE'
    CHARACTER (LEN=40), PARAMETER :: wt3soi = 'SOIL WETNESS OF DRAINAGE ZONE' 
    CHARACTER (LEN=29), PARAMETER :: fmt1='(A40,2X,A4,2X,I8,3X,I4,4X,I3)'

    r = tod
    jttl='CPTEC AGCM REVIS 1.0 2000  T   L    COLD'
    WRITE (jttl(29:31), '(i3.3)') mMax-1
    WRITE (jttl(33:34), '(i2.2)') kMax
    WRITE (imdl, '(A1,I3.3)') 'T', mMax-1

    WRITE (n, '(A20)')   ittl
    WRITE (n, '(A4,1X,A4,1X,A4,1X,11I5,1X,A4)') &
         nexp, 'SEQU', imdl, mMax, kmax, kmax, &
         ihr, iday, mon, iyr, idate, 'TAPE'
    WRITE (n, '(A40)')   jttl
    WRITE (n, '(5E16.8)')   del
    WRITE (n, fmt1) orogra, 'FIXD', 2*mnMax, 1, 10
    WRITE (n, fmt1) lseamk, 'FIXD', ijmax, 1, 0
    WRITE (n, fmt1) lnsurf, 'PROG', 2*mnMax, 1, 142
    WRITE (n, fmt1) divrgn, 'PROG', 2*mnMax, kmax, 50
    WRITE (n, fmt1) vortic, 'PROG', 2*mnMax, kmax, 50
    WRITE (n, fmt1) spechu, 'PROG', 2*mnMax, kmax, 0
    WRITE (n, fmt1) tempvi, 'PROG', 2*mnMax, kmax, 40
    WRITE (n, fmt1) srfrou, 'PROG', ijmax, 1, 10
    WRITE (n, fmt1) surfte, 'PROG', ijmax, 1, 40
    WRITE (n, fmt1) deepte, 'PROG', ijmax, 1, 40
    WRITE (n, fmt1) stcnpy, 'PROG', ijmax, 1, 10
    WRITE (n, fmt1) stgrnd, 'PROG', ijmax, 1, 10
    WRITE (n, fmt1) wt1soi, 'PROG', ijmax, 1, 0
    WRITE (n, fmt1) wt2soi, 'PROG', ijmax, 1, 0
    WRITE (n, fmt1) wt3soi, 'PROG', ijmax, 1, 0
160 FORMAT(A40,2X,A4,2X,I8,3X,I4,4X,I3)
161 FORMAT(' ',A40,2X,A4,2X,I8,3X,I4,4X,I3)    
  END SUBROUTINE WriteDire4
  SUBROUTINE WriteDire8(n, idate, ihr, iday, mon, iyr, del,tod)
    INTEGER (KIND=i8), INTENT(IN ) :: n
    INTEGER (KIND=i8), INTENT(IN ) :: idate(4)
    INTEGER (KIND=i8), INTENT(IN ) :: ihr
    INTEGER (KIND=i8), INTENT(IN ) :: iday
    INTEGER (KIND=i8), INTENT(IN ) :: mon
    INTEGER (KIND=i8), INTENT(IN ) :: iyr
    REAL    (KIND=r8), INTENT(IN ) :: del(kMax) 
    REAL    (KIND=r8), INTENT(IN ) :: tod
    REAL    (KIND=r8) :: r

    CHARACTER (LEN= 4) :: imdl
    CHARACTER (LEN=40) :: jttl
    CHARACTER (LEN=20), PARAMETER :: ittl='CPTEC SIGMA VERS 2.0'
    CHARACTER (LEN= 4), PARAMETER :: nexp='0001'
    CHARACTER (LEN=40), PARAMETER :: orogra = 'TOPOGRAPHY' 
    CHARACTER (LEN=40), PARAMETER :: lseamk = 'LAND SEA MASK'
    CHARACTER (LEN=40), PARAMETER :: lnsurf = 'LN SURFACE PRESSURE'
    CHARACTER (LEN=40), PARAMETER :: divrgn = 'DIVERGENCE'
    CHARACTER (LEN=40), PARAMETER :: vortic = 'VORTICITY'
    CHARACTER (LEN=40), PARAMETER :: spechu = 'SPECIFIC HUMIDITY'
    CHARACTER (LEN=40), PARAMETER :: tempvi = 'VIRTUAL TEMPERATURE'
    CHARACTER (LEN=40), PARAMETER :: surfte = 'SURFACE TEMPERATURE'
    CHARACTER (LEN=40), PARAMETER :: srfrou = 'ROUGHNESS LENGTH'
    CHARACTER (LEN=40), PARAMETER :: deepte = 'DEEP SOIL TEMPERATURE'
    CHARACTER (LEN=40), PARAMETER :: stcnpy = 'STORAGE ON CANOPY'
    CHARACTER (LEN=40), PARAMETER :: stgrnd = 'STORAGE ON GROUND'
    CHARACTER (LEN=40), PARAMETER :: wt1soi = 'SOIL WETNESS OF SURFACE'
    CHARACTER (LEN=40), PARAMETER :: wt2soi = 'SOIL WETNESS OF ROOT ZONE'
    CHARACTER (LEN=40), PARAMETER :: wt3soi = 'SOIL WETNESS OF DRAINAGE ZONE' 
    CHARACTER (LEN=29), PARAMETER :: fmt1='(A40,2X,A4,2X,I8,3X,I4,4X,I3)'



    r = tod
    jttl='CPTEC AGCM REVIS 1.0 2000  T   L    COLD'
    WRITE (jttl(29:31), '(i3.3)') mMax-1
    WRITE (jttl(33:34), '(i2.2)') kMax
    WRITE (imdl, '(A1,I3.3)') 'T', mMax-1

    WRITE (n, '(A20)')   ittl
    WRITE (n, '(A4,1X,A4,1X,A4,1X,11I5,1X,A4)') &
         nexp, 'SEQU', imdl, mMax, kmax, kmax, &
         ihr, iday, mon, iyr, idate, 'TAPE'
    WRITE (n, '(A40)')   jttl
    WRITE (n, '(5E16.8)')   del
    WRITE (n, fmt1) orogra, 'FIXD', 2*mnMax, 1, 10
    WRITE (n, fmt1) lseamk, 'FIXD', ijmax, 1, 0
    WRITE (n, fmt1) lnsurf, 'PROG', 2*mnMax, 1, 142
    WRITE (n, fmt1) divrgn, 'PROG', 2*mnMax, kmax, 50
    WRITE (n, fmt1) vortic, 'PROG', 2*mnMax, kmax, 50
    WRITE (n, fmt1) spechu, 'PROG', 2*mnMax, kmax, 0
    WRITE (n, fmt1) tempvi, 'PROG', 2*mnMax, kmax, 40
    WRITE (n, fmt1) srfrou, 'PROG', ijmax, 1, 10
    WRITE (n, fmt1) surfte, 'PROG', ijmax, 1, 40
    WRITE (n, fmt1) deepte, 'PROG', ijmax, 1, 40
    WRITE (n, fmt1) stcnpy, 'PROG', ijmax, 1, 10
    WRITE (n, fmt1) stgrnd, 'PROG', ijmax, 1, 10
    WRITE (n, fmt1) wt1soi, 'PROG', ijmax, 1, 0
    WRITE (n, fmt1) wt2soi, 'PROG', ijmax, 1, 0
    WRITE (n, fmt1) wt3soi, 'PROG', ijmax, 1, 0
160 FORMAT(A40,2X,A4,2X,I8,3X,I4,4X,I3)
161 FORMAT(' ',A40,2X,A4,2X,I8,3X,I4,4X,I3)
  END SUBROUTINE WriteDire8
  SUBROUTINE ReadProgHead4(n, ifday, tod, idate, idatec)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    INTEGER(KIND=i4), INTENT(OUT) :: ifday
    REAL   (KIND=r4), INTENT(OUT) :: tod
    INTEGER(KIND=i4), INTENT(OUT) :: idate(4)
    INTEGER(KIND=i4), INTENT(OUT) :: idatec(4)
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadProgHead4)**"
    READ(n)ifday, tod, idate, idatec
  END SUBROUTINE ReadProgHead4
  SUBROUTINE ReadProgHead8(n, ifday, tod, idate, idatec)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    INTEGER(KIND=i8), INTENT(OUT) :: ifday
    REAL   (KIND=r8), INTENT(OUT) :: tod
    INTEGER(KIND=i8), INTENT(OUT) :: idate(4)
    INTEGER(KIND=i8), INTENT(OUT) :: idatec(4)
    INTEGER(KIND=i4) :: iaux(10)
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadProgHead8)**"
    READ(n)iaux
    ifday  = INT(iaux(  1 ), i8)
    tod    = INT(iaux(  2 ), i8)
    idate  = INT(iaux(3:6 ), i8)
    idatec = INT(iaux(7:10), i8)
  END SUBROUTINE ReadProgHead8
  SUBROUTINE GReadProgHead4(n, ifday, tod, idate, idatec)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    INTEGER(KIND=i4), INTENT(OUT) :: ifday
    REAL   (KIND=r4), INTENT(OUT) :: tod
    INTEGER(KIND=i4), INTENT(OUT) :: idate(4)
    INTEGER(KIND=i4), INTENT(OUT) :: idatec(4)
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadProgHead4)**"
    READ(n)ifday, tod, idate, idatec
  END SUBROUTINE GReadProgHead4
  SUBROUTINE GReadProgHead8(n, ifday, tod, idate, idatec)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    INTEGER(KIND=i8), INTENT(OUT) :: ifday
    REAL   (KIND=r8), INTENT(OUT) :: tod
    INTEGER(KIND=i8), INTENT(OUT) :: idate(4)
    INTEGER(KIND=i8), INTENT(OUT) :: idatec(4)
    INTEGER(KIND=i8) :: iaux(10)
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadProgHead8)**"
    READ(n)iaux
    ifday  = INT(iaux(  1 ), i8)
    tod    = INT(iaux(  2 ), i8)
    idate  = INT(iaux(3:6 ), i8)
    idatec = INT(iaux(7:10), i8)
  END SUBROUTINE GReadProgHead8
  SUBROUTINE WriteProgHead4(n, ifday, tod, idate, idatec)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    INTEGER(KIND=i4), INTENT(IN)  :: ifday
    REAL   (KIND=r4), INTENT(IN)  :: tod
    INTEGER(KIND=i4), INTENT(IN)  :: idate(4)
    INTEGER(KIND=i4), INTENT(IN)  :: idatec(4)
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteProgHead4)**"
    WRITE(n)ifday, tod, idate, idatec
  END SUBROUTINE WriteProgHead4
  SUBROUTINE WriteProgHead8(n, ifday, tod, idate, idatec)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    INTEGER(KIND=i8), INTENT(IN)  :: ifday
    REAL   (KIND=r8), INTENT(IN)  :: tod
    INTEGER(KIND=i8), INTENT(IN)  :: idate(4)
    INTEGER(KIND=i8), INTENT(IN)  :: idatec(4)
    INTEGER(KIND=i4) :: iaux(10)
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteProgHead8)**"
    iaux(  1 ) = INT (ifday , i4)
    iaux(  2 ) = INT (tod   , i4)
    iaux(3:6 ) = INT (idate , i4)
    iaux(7:10) = INT (idatec, i4)
    WRITE(n)iaux
  END SUBROUTINE WriteProgHead8
  SUBROUTINE ReadLandSeaMask4(n, iMax, lsmk)
    INTEGER(KIND=i4), INTENT(IN ) :: n
    INTEGER(KIND=i4), INTENT(IN ) :: iMax
    REAL(KIND=r4),    INTENT(OUT) :: lsmk(:)
    INTEGER(KIND=i4)              :: int_lsmk(SIZE(lsmk))
    CHARACTER (LEN=7) :: fmti
    WRITE (fmti, '(''('',i3,''i1)'')') iMax
    READ (n, fmti) int_lsmk
    lsmk(:)=REAL(1-2*int_lsmk(:),r4)
  END SUBROUTINE ReadLandSeaMask4
  SUBROUTINE ReadLandSeaMask8(n, iMax, lsmk)
    INTEGER(KIND=i8), INTENT(IN ) :: n
    INTEGER(KIND=i8), INTENT(IN ) :: iMax
    REAL(KIND=r8),    INTENT(OUT) :: lsmk(:)
    INTEGER(KIND=i4)              :: int_lsmk(SIZE(lsmk))
    CHARACTER (LEN=7) :: fmti
    WRITE (fmti, '(''('',i3,''i1)'')') iMax
    READ (n, fmti) int_lsmk
    lsmk(:)=REAL(1-2*int_lsmk(:),r8)
  END SUBROUTINE ReadLandSeaMask8

  SUBROUTINE ReadLandSeaMask2_4(fname, lsmk)
    CHARACTER(LEN=*), INTENT(IN ) :: fname
    REAL(KIND=r4),    INTENT(OUT) :: lsmk(imax*jmax)
    INTEGER(KIND=i4) :: unit
    INTEGER(KIND=i4) :: ierr
    !**TO portable TX7** CALL GetUnit(unit)
    unit=22
    OPEN (unit, FILE=TRIM(fname), FORM="UNFORMATTED", ACCESS="SEQUENTIAL",&
         ACTION="READ", STATUS="OLD", IOSTAT=ierr)
    IF (ierr /= 0) THEN
       WRITE(*,"('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
            TRIM(fname), ierr
       STOP "**(ERROR)**"
    END IF
    READ (unit, IOSTAT=ierr) lsmk
    IF (ierr /= 0) THEN
       WRITE(*,"('**(ERROR)** Read file ',a,' returned iostat=',i4)") &
            TRIM(fname), ierr
       STOP "**(ERROR)**"
    END IF
    CLOSE (unit)
  END SUBROUTINE ReadLandSeaMask2_4

  SUBROUTINE ReadLandSeaMask2_8(fname, lsmk)
    CHARACTER(LEN=*  ), INTENT(IN ) :: fname
    REAL     (KIND=r8),    INTENT(OUT) :: lsmk(imax*jmax)
    REAL     (KIND=r4) :: aux(imax*jmax)
    INTEGER  (KIND=i8) :: unit
    INTEGER  (KIND=i8) :: ierr
    !**TO portable TX7** CALL GetUnit(unit)
    unit=22
    OPEN (unit, FILE=TRIM(fname), FORM="UNFORMATTED", ACCESS="SEQUENTIAL",&
         ACTION="READ", STATUS="OLD", IOSTAT=ierr)
    IF (ierr /= 0) THEN
       WRITE(*,"('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
            TRIM(fname), ierr
       STOP "**(ERROR)**"
    END IF
    READ (unit, IOSTAT=ierr) aux
    IF (ierr /= 0) THEN
       WRITE(*,"('**(ERROR)** Read file ',a,' returned iostat=',i4)") &
            TRIM(fname), ierr
       STOP "**(ERROR)**"
    END IF
    lsmk = REAL(aux,r8)
    CLOSE(unit)
  END SUBROUTINE ReadLandSeaMask2_8
  SUBROUTINE  LandSeaMask4(ifsst,nfsst,labeli,intsst,sstlag,fNameSSTAOI,rlsm)
    IMPLICIT NONE
    INTEGER  (KIND=i8), INTENT(IN   )  :: ifsst
    INTEGER  (KIND=i8), INTENT(IN   )  :: nfsst
    INTEGER  (KIND=i8), INTENT(INOUT)  :: intsst
    REAL     (KIND=r8), INTENT(OUT  )  :: sstlag
    REAL     (KIND=r4), INTENT(INOUT)  :: rlsm(:,:)
    CHARACTER(LEN=*  ), INTENT(IN   )  :: labeli
    CHARACTER(LEN=*  ), INTENT(IN   )  :: fNameSSTAOI

    CHARACTER(LEN=10 )                 :: labelsi
    CHARACTER(LEN=10 )                 :: labelsj
    INTEGER  (KIND=i8)                 :: lrecl
    INTEGER  (KIND=i4)                 :: nsst
    REAL     (KIND=i8)                 :: dlag
    REAL     (KIND=r8),ALLOCATABLE     :: var4(:,:)  
    INTEGER  (KIND=i8)                 :: j
    INTEGER  (KIND=i8)                 :: jmax
    INTEGER  (KIND=i8)                 :: i  
    INTEGER  (KIND=i8)                 :: imax
    jmax = SIZE(rlsm,2)   
    imax = SIZE(rlsm,1)
    ALLOCATE(var4(imax,jmax))


    ! Use open statement to open direct access file when ifsst .ge. 4:

    INQUIRE (IOLENGTH=lrecl) rlsm
    lrecl=lrecl/2

    OPEN (nfsst,file=TRIM(fNameSSTAOI),ACCESS='DIRECT',FORM='UNFORMATTED',&
         RECL=lrecl,STATUS='UNKNOWN')
    READ (nfsst,REC=1)nsst,labelsi,labelsj
    WRITE(*,*)nsst,labelsi,labelsj
    CALL daylag (labelsi,labelsj,dlag,intsst)
    intsst=NINT(dlag)
    IF (intsst > 10) intsst=-intsst
    CALL daylag (labelsi,labeli,dlag,intsst)
    sstlag=dlag
    WRITE(*,'(/,a)')' Direct Access SST File:'
    WRITE(*,'(1x,a4,3(1x,a2))') &
         labelsi(1:4),labelsi(5:6),labelsi(7:8),labelsi(9:10)
    WRITE(*,'(1x,a4,3(1x,a2))') &
         labelsj(1:4),labelsj(5:6),labelsj(7:8),labelsj(9:10)
    WRITE(*,'(i7,a)')intsst,'  Days'
    WRITE(*,'(1x,a4,3(1x,a2))') &
         labelsi(1:4),labelsi(5:6),labelsi(7:8),labelsi(9:10)
    WRITE(*,'(1x,a4,3(1x,a2))') &
         labeli(1:4),labeli(5:6),labeli(7:8),labeli(9:10)
    IF (intsst > 0) THEN
       WRITE(*,'(f7.1,a,/)')sstlag,'  Days'
    ELSE
       WRITE(*,'(f7.1,a,/)')sstlag,'  Months'
    END IF
    IF (sstlag < 0) THEN
       WRITE (*, 336) ifsst, sstlag
       STOP 336
    END IF
    READ (nfsst,REC=2) var4
    CLOSE (nfsst)

    DO j=1,jmax
       DO i=1,imax
          rlsm(i,j)=var4(i,j)
       END DO
    END DO
    DEALLOCATE(var4)
336 FORMAT(' FOR IFSST=',I5,' SSTLAG MUST BE SET NONNEGATIVE.  NOT ',G12.5)  
  END SUBROUTINE  LandSeaMask4
  SUBROUTINE  LandSeaMask8(ifsst,nfsst,labeli,intsst,sstlag,fNameSSTAOI,rlsm)
    IMPLICIT NONE
    INTEGER  (KIND=i8), INTENT(IN   )  :: ifsst
    INTEGER  (KIND=i8), INTENT(IN   )  :: nfsst
    INTEGER  (KIND=i8), INTENT(INOUT)  :: intsst
    REAL     (KIND=r8), INTENT(OUT  )  :: sstlag
    REAL     (KIND=r8), INTENT(INOUT)  :: rlsm(:,:)
    CHARACTER(LEN=*  ), INTENT(IN   )  :: labeli
    CHARACTER(LEN=*  ), INTENT(IN   )  :: fNameSSTAOI

    CHARACTER(LEN=10 )                 :: labelsi
    CHARACTER(LEN=10 )                 :: labelsj
    INTEGER  (KIND=i8)                 :: lrecl
    INTEGER  (KIND=i4)                 :: nsst
    REAL     (KIND=i8)                 :: dlag
    REAL     (KIND=r4),ALLOCATABLE     :: var4(:,:)  
    INTEGER  (KIND=i8)                 :: j
    INTEGER  (KIND=i8)                 :: jmax
    INTEGER  (KIND=i8)                 :: i  
    INTEGER  (KIND=i8)                 :: imax
    jmax = SIZE(rlsm,2)   
    imax = SIZE(rlsm,1)
    ALLOCATE(var4(imax,jmax))


    ! Use open statement to open direct access file when ifsst .ge. 4:

    INQUIRE (IOLENGTH=lrecl) rlsm
    lrecl=lrecl/2

    OPEN (nfsst,file=TRIM(fNameSSTAOI),ACCESS='DIRECT',FORM='UNFORMATTED',&
         RECL=lrecl,STATUS='UNKNOWN')
    READ (nfsst,REC=1)nsst,labelsi,labelsj
    WRITE(*,*)nsst,labelsi,labelsj
    CALL daylag (labelsi,labelsj,dlag,intsst)
    intsst=NINT(dlag)
    IF (intsst > 10) intsst=-intsst
    CALL daylag (labelsi,labeli,dlag,intsst)
    sstlag=dlag
    WRITE(*,'(/,a)')' Direct Access SST File:'
    WRITE(*,'(1x,a4,3(1x,a2))') &
         labelsi(1:4),labelsi(5:6),labelsi(7:8),labelsi(9:10)
    WRITE(*,'(1x,a4,3(1x,a2))') &
         labelsj(1:4),labelsj(5:6),labelsj(7:8),labelsj(9:10)
    WRITE(*,'(i7,a)')intsst,'  Days'
    WRITE(*,'(1x,a4,3(1x,a2))') &
         labelsi(1:4),labelsi(5:6),labelsi(7:8),labelsi(9:10)
    WRITE(*,'(1x,a4,3(1x,a2))') &
         labeli(1:4),labeli(5:6),labeli(7:8),labeli(9:10)
    IF (intsst > 0) THEN
       WRITE(*,'(f7.1,a,/)')sstlag,'  Days'
    ELSE
       WRITE(*,'(f7.1,a,/)')sstlag,'  Months'
    END IF
    IF (sstlag < 0) THEN
       WRITE (*, 336) ifsst, sstlag
       STOP 336
    END IF
    READ (nfsst,REC=2) var4
    CLOSE (nfsst)

    DO j=1,jmax
       DO i=1,imax
          rlsm(i,j)=var4(i,j)
       END DO
    END DO
    DEALLOCATE(var4)
336 FORMAT(' FOR IFSST=',I5,' SSTLAG MUST BE SET NONNEGATIVE.  NOT ',G12.5)  
  END SUBROUTINE  LandSeaMask8



  SUBROUTINE daylag (labeli,labelf,dlag,intsst)
    IMPLICIT NONE
    CHARACTER (LEN=10 ) :: labeli
    CHARACTER (LEN=10 ) :: labelf
    REAL      (KIND=i8) :: dlag
    REAL                :: xday
    REAL                :: yday
    INTEGER   (KIND=i8) :: intsst
    INTEGER :: yi,mi,di,hi,yf,mf,df,hf,ndy,y,n,ndi,ndf
    INTEGER, DIMENSION (12) :: ndm = &
         (/31,28,31,30,31,30,31,31,30,31,30,31/)
    INTEGER, DIMENSION (12) :: ndmi = &
         (/31,28,31,30,31,30,31,31,30,31,30,31/)
    INTEGER, DIMENSION (12) :: ndmf = &
         (/31,28,31,30,31,30,31,31,30,31,30,31/)

    READ (labeli(1:4), '(i4)') yi
    READ (labeli(5:6), '(i2)') mi
    READ (labeli(7:8), '(i2)') di
    READ (labeli(9:10),'(i2)') hi
    READ (labelf(1:4), '(i4)') yf
    READ (labelf(5:6), '(i2)') mf
    READ (labelf(7:8), '(i2)') df
    READ (labelf(9:10),'(i2)') hf
    IF (MOD(yi,4) .EQ. 0) ndmi(2)=29
    IF (MOD(yf,4) .EQ. 0) ndmf(2)=29

    IF (intsst > 0) THEN

       ndy=0
       DO y=yi+1,yf-1
          DO n=1,12
             ndy=ndy+ndm(n)
          END DO
          IF (MOD(y,4) .EQ. 0) ndy=ndy+1
       END DO

       ndi=di
       DO n=1,mi-1
          ndi=ndi+ndmi(n)
       END DO
       ndf=df
       DO n=1,mf-1
          ndf=ndf+ndmf(n)
       END DO

       IF (yf .EQ. yi) THEN
          dlag=REAL(ndf-ndi)+REAL(hf-hi)/24.0
       ELSE IF(yf .GT. yi) THEN
          ndi=365-ndi
          IF (ndmi(2) .EQ. 29) ndi=ndi+1
          dlag=REAL(ndf+ndi)+REAL(hf-hi)/24.0+ndy
       ELSE
          dlag=-1.0
       END IF

    ELSE

       IF (mf >= mi) THEN
          dlag=mf-mi+12*(yf-yi)
       ELSE
          dlag=12+mf-mi+12*(yf-yi-1)
       END IF
       IF (MOD(yf,4) .EQ. 0) ndmf(2)=29
       xday=REAL(df)+REAL(hf)/24.0
       yday=1.0+REAL(ndmf(mf))/2.0
       IF (xday <= yday) dlag=dlag-1.0

    END IF

  END SUBROUTINE daylag
  SUBROUTINE GetUnit4(unit)
    INTEGER(KIND=i4), INTENT(OUT) :: unit
    LOGICAL :: op
    INTEGER(KIND=i4), PARAMETER :: LBUnit=1   ! lower bound unit number
    INTEGER(KIND=i4), PARAMETER :: UBUnit=99  ! upper bound unit number
    DO unit = LBUnit, UBUnit
       INQUIRE(unit, OPENED=op)
       IF (.NOT. op) THEN
          EXIT
       END IF
    END DO
    IF (unit > UBUnit) THEN
       WRITE(*,"('**(ERROR)** All file units are opened')")
       STOP "**(ERROR)**"
    END IF
  END SUBROUTINE GetUnit4

  SUBROUTINE GetUnit8(unit)
    INTEGER(KIND=i8), INTENT(OUT) :: unit
    LOGICAL :: op
    INTEGER(KIND=i8), PARAMETER :: LBUnit=1   ! lower bound unit number
    INTEGER(KIND=i8), PARAMETER :: UBUnit=99  ! upper bound unit number
    DO unit = LBUnit, UBUnit
       INQUIRE(unit, OPENED=op)
       IF (.NOT. op) THEN
          EXIT
       END IF
    END DO
    IF (unit > UBUnit) THEN
       WRITE(*,"('**(ERROR)** All file units are opened')")
       STOP "**(ERROR)**"
    END IF
  END SUBROUTINE GetUnit8

  SUBROUTINE ReadVar4(nfvar,var)
    INTEGER  (kind=i4)         , INTENT(in   ) :: nfvar
    REAL     (kind=r4)         , INTENT(out  ) :: var (:,:)
    REAL     (kind=r4)                         :: var4(iMax,jMax)
    INTEGER  (kind=i4)                         :: i
    INTEGER  (kind=i4)                         :: j 
    READ(nfvar) var4
    DO j=1,jMax
       DO i=1,iMax
          var(i,j)=var4(i,j)
       END DO
    END DO
    CLOSE(nfvar,STATUS='KEEP')
  END SUBROUTINE ReadVar4
  SUBROUTINE ReadVar8(nfvar,var)
    INTEGER  (kind=i8)         , INTENT(in   ) :: nfvar
    REAL     (kind=r8)         , INTENT(out  ) :: var (:,:)  
    REAL     (kind=r4)                         :: var8(iMax,jMax)
    INTEGER  (kind=i8)                         :: i
    INTEGER  (kind=i8)                         :: j 
    READ(nfvar) var8
    DO j=1,jMax
       DO i=1,iMax
          var(i,j)=REAL(var8(i,j),r8)     
       END DO
    END DO
    CLOSE(nfvar,STATUS='KEEP') 
  END SUBROUTINE ReadVar8

  SUBROUTINE ReadAlb4(n,field)
    INTEGER  (kind=i4)         , INTENT(in   ) :: n
    REAL     (KIND=r4)         , INTENT(out  ) :: field  (:)
    REAL     (KIND=r8)                         :: raux3(SIZE(field))
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadAlb4)**"
    READ(n) raux3
    field = REAL(raux3, r4)
  END SUBROUTINE ReadAlb4
  SUBROUTINE ReadAlb8(n,field)
    INTEGER  (kind=i8)         , INTENT(in   ) :: n
    REAL     (KIND=r8)         , INTENT(out  ) :: field  (:)
    REAL     (KIND=r8)                         :: raux3(SIZE(field))
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadAlb8)**"   
    READ(n) raux3
    field = raux3
  END SUBROUTINE ReadAlb8

  SUBROUTINE ReadSST4(n,field)
    INTEGER  (kind=i8)         , INTENT(in   ) :: n
    REAL     (KIND=r4)         , INTENT(out  ) :: field  (:)
    REAL     (KIND=r8)                         :: raux3(SIZE(field))
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadSST4)**"   
    READ(n) raux3
    field = REAL(raux3, r4)
  END SUBROUTINE ReadSST4
  SUBROUTINE ReadSST8(n,field)
    INTEGER(KIND=i8)          , INTENT(IN   ) :: n
    REAL   (KIND=r8)          , INTENT(OUT  ) :: field(:)
    REAL   (KIND=r4)                          :: raux3(SIZE(field))
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadSST8)**"   
    READ(n)raux3
    field = REAL(raux3, r8)
  END SUBROUTINE ReadSST8

  SUBROUTINE ReadSST4Rec(n,field,irec)
    INTEGER  (kind=i8)         , INTENT(in   ) :: n
    INTEGER  (kind=i8)         , INTENT(in   ) :: irec
    REAL     (KIND=r4)         , INTENT(out  ) :: field  (:)
    REAL     (KIND=r8)                         :: raux3(SIZE(field))
    READ(n,rec=irec) raux3
    field = REAL(raux3, r4)
  END SUBROUTINE ReadSST4Rec
  SUBROUTINE ReadSST8Rec(n,field,irec)
    INTEGER(KIND=i8)          , INTENT(IN   ) :: n
    INTEGER(kind=i8)          , INTENT(in   ) :: irec
    REAL   (KIND=r8)          , INTENT(OUT  ) :: field(:)
    REAL   (KIND=r4)                          :: raux3(SIZE(field))
    READ(n,rec=irec)raux3
    field = REAL(raux3, r8)
  END SUBROUTINE ReadSST8Rec

  SUBROUTINE ReadSLM4(n,field)
    INTEGER  (kind=i4)         , INTENT(in   ) :: n
    REAL     (KIND=r4)         , INTENT(out  ) :: field  (:) 
    REAL     (KIND=r4)                         :: raux3(SIZE(field))
    READ(n) raux3
    field = raux3
  END SUBROUTINE ReadSLM4
  SUBROUTINE ReadSLM8(n,field)
    INTEGER  (kind=i8)         , INTENT(in   ) :: n
    REAL     (KIND=r8)         , INTENT(out  ) :: field  (:)
    REAL     (KIND=r8)                         :: raux3(SIZE(field))
    READ(n) raux3
    field = raux3
  END SUBROUTINE ReadSLM8

  SUBROUTINE ReadSNW4(n,field)
    INTEGER  (kind=i8)         , INTENT(in   ) :: n
    REAL     (KIND=r4)         , INTENT(out  ) :: field  (:)
    REAL    (KIND=r8)                         :: raux3(SIZE(field))
    READ(n) raux3
    field = REAL(raux3, r4)
  END SUBROUTINE ReadSNW4
  SUBROUTINE ReadSNW8(n,field)
    INTEGER(KIND=i8),             INTENT(IN)  :: n
    REAL   (KIND=r8),             INTENT(OUT) :: field(:)
    REAL   (KIND=r4)                          :: raux3(SIZE(field))
    READ(n)raux3
    field = REAL(raux3, r8)
  END SUBROUTINE ReadSNW8

  SUBROUTINE ReadNFTGZ4(n,field1,field2,field3,field4)
    INTEGER(kind=i8), INTENT(in   ) :: n 
    REAL   (kind=r8), INTENT(out  ) :: field1 (:,:)
    REAL   (kind=r8), INTENT(out  ) :: field2 (:,:)
    REAL   (kind=r8), INTENT(out  ) :: field3 (:,:)
    REAL   (kind=r8), INTENT(out  ) :: field4 (:,:)
    REAL   (KIND=r8)                :: raux1(SIZE(field1,1),SIZE(field1,2))
    REAL   (KIND=r8)                :: raux2(SIZE(field2,1),SIZE(field2,2))
    REAL   (KIND=r8)                :: raux3(SIZE(field3,1),SIZE(field3,2))
    REAL   (KIND=r8)                :: raux4(SIZE(field4,1),SIZE(field4,2))

    READ  (n) raux1,raux2,raux3,raux4
    field1 = REAL(raux1, r8)
    field2 = REAL(raux2, r8)
    field3 = REAL(raux3, r8)
    field4 = REAL(raux4, r8)

    REWIND n
  END SUBROUTINE ReadNFTGZ4
  SUBROUTINE ReadNFTGZ8(n,field1,field2,field3,field4)
    INTEGER(kind=i4), INTENT(in   ) :: n 
    REAL   (kind=r4), INTENT(out  ) :: field1 (:,:)
    REAL   (kind=r4), INTENT(out  ) :: field2 (:,:)
    REAL   (kind=r4), INTENT(out  ) :: field3 (:,:)
    REAL   (kind=r4), INTENT(out  ) :: field4 (:,:)
    REAL   (KIND=r8)                :: raux1(SIZE(field1,1),SIZE(field1,2))
    REAL   (KIND=r8)                :: raux2(SIZE(field2,1),SIZE(field2,2))
    REAL   (KIND=r8)                :: raux3(SIZE(field3,1),SIZE(field3,2))
    REAL   (KIND=r8)                :: raux4(SIZE(field4,1),SIZE(field4,2))

    READ  (n) raux1,raux2,raux3,raux4
    field1 = REAL(raux1, r4)
    field2 = REAL(raux2, r4)
    field3 = REAL(raux3, r4)
    field4 = REAL(raux4, r4)

    REWIND n
  END SUBROUTINE ReadNFTGZ8

  SUBROUTINE WriteDiagHead4(n, ifday, tod)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    INTEGER(KIND=i4), INTENT(IN)  :: ifday
    REAL   (KIND=r4), INTENT(IN)  :: tod
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteProgHead4)**"
    WRITE(n)ifday, tod
  END SUBROUTINE WriteDiagHead4
  SUBROUTINE WriteDiagHead8(n, ifday, tod)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    INTEGER(KIND=i8), INTENT(IN)  :: ifday
    REAL   (KIND=r8), INTENT(IN)  :: tod
    INTEGER(KIND=i4) :: iaux(2)
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteProgHead8)**"
    iaux(  1 ) = INT (ifday , i4)
    iaux(  2 ) = INT (tod   , i4)
    WRITE(n)iaux
  END SUBROUTINE WriteDiagHead8

  SUBROUTINE WriteGrdH4(n,field1,field2)
    INTEGER(KIND=i4), INTENT(IN)  :: n
    REAL   (KIND=r4), INTENT(IN)  :: field1(:)
    REAL   (KIND=r4), INTENT(IN)  :: field2(:,:)
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteGrdH4)**"
    WRITE(n)field1
    WRITE(n)field2
  END SUBROUTINE WriteGrdH4
  SUBROUTINE WriteGrdH8(n,field1,field2)
    INTEGER(KIND=i8), INTENT(IN)  :: n
    REAL   (KIND=r8), INTENT(IN)  :: field1(:)
    REAL   (KIND=r8), INTENT(IN)  :: field2(:,:)
    REAL   (KIND=r4) :: raux1(SIZE(field1,1))
    REAL   (KIND=r4) :: raux2(SIZE(field2,1),SIZE(field2,2))   
    CHARACTER(LEN=*), PARAMETER :: h="**(WriteGrdH8)**"
    raux1 = REAL(field1, r4)
    raux2 = REAL(field2, r4)
    WRITE(n)raux1
    WRITE(n)raux2
  END SUBROUTINE WriteGrdH8
END MODULE IOLowLevel
