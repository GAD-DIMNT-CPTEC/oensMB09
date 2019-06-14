!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE Utils



  !  ASSOCIATED LEGENDRE FUNCTIONS
  !  Module computes and stores Associated Legendre
  !  Functions and Epslon. 

  !  Module exports three routines:
  !     CreateAssocLegFunc  initializes module and compute functions;
  !     DestroyAssocLegFunc destroys module;
  !     DumpAssocLegFunc    Dumps module info

  !  Module usage:
  !  CreateAssocLegFunc should be invoked once, prior to any other
  !  module routine. It computes and hides the function values.
  !  DestroyAssocLegFunc destroys all internal info.

  !  Module use values exported by Sizes and procedures from Auxiliary



  USE Sizes, ONLY:  &
       mnMax,       &
       mnMap,       &
       mMax,        &
       nMax,        &
       nExtMax,     &
       mnExtMax,    &
       iMax,        &
       jMax,        &
       jMaxHalf,    &
       jbMax,       &
       ibMax,       &
       ibPerIJ,     &
       jbPerIJ,     &
       iPerIJB,     &
       jPerIJB,     &
       ibMaxPerJB,  &
       mExtMap,     &
       nExtMap,     &
       mnExtMap

  USE Constants, ONLY : pai 

  IMPLICIT NONE

  PRIVATE

  !
  !  LEGANDRE POLINOMIAL AND ITS ROOTS
  !
  !  Module exports four routines:
  !     CreateLegPol  initializes module;
  !     DestroyLegPol destroys module;
  !     LegPol        computes polinomial
  !     LegPolRoots   computes roots of even degree Legandre Pol
  !
  !  Module does not export (or require) any data value.
  !


  PUBLIC :: CreateLegPol
  PUBLIC :: DestroyLegPol
  PUBLIC :: LegPol
  PUBLIC :: LegPolRoots
  PUBLIC :: CreateGaussQuad
  PUBLIC :: DestroyGaussQuad
  PUBLIC :: DumpGaussQuad
  PUBLIC :: GaussColat
  PUBLIC :: SinGaussColat
  PUBLIC :: CosGaussColat
  PUBLIC :: AuxGaussColat
  PUBLIC :: GaussPoints
  PUBLIC :: GaussWeights
  PUBLIC :: colrad
  PUBLIC :: rcs2
  PUBLIC :: NoBankConflict
  PUBLIC :: DumpMatrix
  PUBLIC :: CreateAssocLegFunc
  PUBLIC :: DestroyAssocLegFunc
  PUBLIC :: DumpAssocLegFunc
  PUBLIC :: Epslon
  PUBLIC :: LegFuncF2S
  PUBLIC :: LegFuncF2STransp
  PUBLIC :: LegFuncS2F
  PUBLIC :: Iminv
  PUBLIC :: Rg
  PUBLIC :: Tql2
  PUBLIC :: Tred2
  PUBLIC :: tmstmp2
  PUBLIC :: InitTransDiagCol
  PUBLIC :: TransDiagCol
  PUBLIC :: InitTimeStamp
  PUBLIC :: TimeStamp
  PUBLIC :: IBJBtoIJ  
  PUBLIC :: IJtoIBJB 
 
  INTERFACE IBJBtoIJ
     MODULE PROCEDURE IBJBtoIJ_R, IBJBtoIJ_I
  END INTERFACE
  INTERFACE IJtoIBJB
     MODULE PROCEDURE IJtoIBJB_R, IJtoIBJB_I
  END INTERFACE


  INTERFACE TransDiagCol
     MODULE PROCEDURE TransDiagCol1D, TransDiagCol2D
  END INTERFACE


  INTERFACE NoBankConflict
     MODULE PROCEDURE NoBankConflictS, NoBankConflictV
  END INTERFACE
  INTERFACE DumpMatrix
     MODULE PROCEDURE &
          DumpMatrixReal1D, DumpMatrixReal2D, DumpMatrixReal3D, &
          DumpMatrixInteger1D, DumpMatrixInteger2D, DumpMatrixInteger3D
  END INTERFACE


  !  Module usage:
  !     CreateGaussQuad  should be invoked once, before any other routine
  !                      of this module, to set up maximum degree 
  !                      of base functions (say, n);
  !                      it computes and hides n Gaussian Points and Weights
  !                      over interval [-1:1];
  !                      it also creates and uses Mod LegPol
  !     DestroyGaussQuad destrois hidden data structure and leaves module ready
  !                      for re-start, if desired, with another maximum degree.

  REAL, ALLOCATABLE :: GaussColat(:)
  REAL, ALLOCATABLE :: SinGaussColat(:)
  REAL, ALLOCATABLE :: CosGaussColat(:)
  REAL, ALLOCATABLE :: AuxGaussColat(:)
  REAL, ALLOCATABLE :: GaussPoints(:)
  REAL, ALLOCATABLE :: GaussWeights(:)
  REAL, ALLOCATABLE :: colrad(:)
  REAL, ALLOCATABLE :: rcs2(:)

  !  Module Hided Data:
  !     maxDegree is the degree of the base functions (n)
  !     created specifies if module was created or not



  LOGICAL           :: created=.FALSE.
  INTEGER           :: maxDegree=-1



  REAL,    ALLOCATABLE :: Epslon(:)
  REAL,    ALLOCATABLE :: LegFuncF2S(:,:)
  REAL,    ALLOCATABLE :: LegFuncF2STransp(:,:)
  REAL,    ALLOCATABLE :: LegFuncS2F(:,:)


  !  Module Hidden data


  INTEGER                         :: nAuxPoly
  REAL, ALLOCATABLE, DIMENSION(:) :: AuxPoly1, AuxPoly2
  LOGICAL, PARAMETER   :: dumpLocal=.FALSE.

  ! Index mappings to/from diagonal from/to column 
  ! for 'normal' spectral representations

  INTEGER,  ALLOCATABLE :: DiagPerCol(:)   ! diag=DiagPerCol(col )
  INTEGER,  ALLOCATABLE :: ColPerDiag(:)   ! col =ColPerDiag(diag)

  ! Index mappings to/from diagonal from/to column for 
  ! 'extended' spectral representations

  INTEGER,  ALLOCATABLE :: ExtDiagPerCol(:)   ! diag=DiagPerCol(col )
  INTEGER,  ALLOCATABLE :: ExtColPerDiag(:)   ! col =ColPerDiag(diag)

  ! date are always in the form yyyymmddhh ( year, month, day, hour). hour in 
  ! is in 0-24 form 
  
  INTEGER, PRIVATE :: JulianDayInitIntegration  

CONTAINS



  !CreateAssocLegFunc  should be invoked once, before any other routine,
  !                    to compute and store Associated Legendre Functions
  !                    at Gaussian Points for all
  !                    Legendre Orders and Degrees (defined at Sizes)



  SUBROUTINE CreateAssocLegFunc()
    INTEGER :: m, n, mn, j
    REAL, ALLOCATABLE :: Square(:), Den(:)
    CHARACTER(LEN=*), PARAMETER :: h="**(CreateAssocLegFunc)**"

    ALLOCATE (Epslon(mnExtMax))
    ALLOCATE (LegFuncF2S(mnExtMax, jMaxHalf))
    ALLOCATE (LegFuncF2STransp(jMaxHalf,mnExtMax))
    ALLOCATE (LegFuncS2F(jMaxHalf, mnExtMax))

    ALLOCATE (Square(nExtMax))
    ALLOCATE (Den(nExtMax))
    DO n = 1, nExtMax
       Square(n) = REAL((n-1)*(n-1))
       Den(n)    = 1.0/(4.0*Square(n) - 1.0)
    END DO
    DO mn = 1, mnExtMax
       m = mExtMap(mn)
       n = nExtMap(mn)
       Epslon(mn) = SQRT((Square(n)-Square(m))*Den(n))
    END DO


    LegFuncS2F(1:jMaxHalf, mnExtMap(1,1)) = SQRT(0.5)
    DO m = 2, mMax
       DO j = 1, jMaxHalf
          LegFuncS2F(j, mnExtMap(m,m)) =   &
               SQRT(1.0 + 0.5/REAL(m-1)) * &
               SinGaussColat(j)          * &
               LegFuncS2F(j, mnExtMap(m-1,m-1))
       END DO
    END DO
    DO m = 1, mMax
       DO j = 1, jMaxHalf
          LegFuncS2F(j, mnExtMap(m,m+1)) =   &
               SQRT(1.0 + 2.0*REAL(m))     * &
               GaussPoints(j)              * &
               LegFuncS2F(j, mnExtMap(m,m))
       END DO
    END DO
    DO m = 1, mMax
       DO n = m+2, nExtMax
          DO j = 1, jMaxHalf
             LegFuncS2F(j, mnExtMap(m,n)) =          &
                  ( GaussPoints(j)                 * &
                    LegFuncS2F(j, mnExtMap(m,n-1)) - &
                    Epslon(mnExtMap(m,n-1))        * &
                    LegFuncS2F(j, mnExtMap(m,n-2))   &
                   ) / Epslon(mnExtMap(m,n))
          END DO
       END DO
    END DO

    DO j = 1, jMaxHalf
       DO mn = 1, mnExtMax
          LegFuncF2S(mn,j) = LegFuncS2F(j,mn)*GaussWeights(j)
       END DO
    END DO
    LegFuncF2STransp = TRANSPOSE(LegFuncF2S)

    DEALLOCATE (Square, Den)
  END SUBROUTINE CreateAssocLegFunc



  !DestroyAssocLegFunc  Deallocates all stored values



  SUBROUTINE DestroyAssocLegFunc()
    CHARACTER(LEN=*), PARAMETER :: h="**(DestroyAssocLegFunc)**"
    DEALLOCATE (Epslon, LegFuncF2S, LegFuncS2F)
  END SUBROUTINE DestroyAssocLegFunc



  !DumpAssocLegFunc  Dumps all stored values



  SUBROUTINE DumpAssocLegFunc()
    CHARACTER(LEN=*), PARAMETER :: h="**(DumpAssocLegFunc)**"
    WRITE(*,"(a,' dumping stored values')") h
    CALL DumpMatrix('Epslon', Epslon)
    CALL DumpMatrix('LegFuncF2S',LegFuncF2S)
    CALL DumpMatrix('LegFuncS2F',LegFuncS2F)
  END SUBROUTINE DumpAssocLegFunc

  !  AUXILIARY PROCEDURES

  !  Module exports two routines:
  !     NoBankConflict  given input integer (size of an array at
  !                     any dimension) returns the next integer
  !                     that should dimension the array to avoid 
  !                     memory bank conflicts. The vector version
  !                     returns a vector of integers, given a vector
  !                     of input integers.
  !     DumpMatrix      Dumps input matrix, for ranks 1 to 3, of type
  !                     integer or real. Output is limited to 10 columns.

  !  Module does not require any other module.
  !  Module does not export any value.



  FUNCTION NoBankConflictS(s) RESULT(p)
    INTEGER, INTENT(IN) :: s
    INTEGER             :: p
    IF ((MOD(s,2)==0) .AND. (s/=0)) THEN
       p = s + 1
    ELSE
       p = s
    END IF
  END FUNCTION NoBankConflictS
  FUNCTION NoBankConflictV(s) RESULT(p)
    INTEGER, INTENT(IN) :: s(:)
    INTEGER             :: p(SIZE(s))
    WHERE ((MOD(s,2)==0) .AND. (s/=0)) 
       p = s + 1
    ELSEWHERE
       p = s
    END WHERE
  END FUNCTION NoBankConflictV
  SUBROUTINE DumpMatrixReal1D(name, m)
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL, INTENT(IN) :: m(:)
    INTEGER :: n1, i1, i1h
    CHARACTER(LEN=10) :: c1
    n1=SIZE(m,1)
    WRITE(c1,"(i10)") n1
    WRITE(*,"(' Dump Matrix ',a,'[',a,']')") &
         TRIM(ADJUSTL(name)), TRIM(ADJUSTL(c1))
    i1h = MIN(10,n1)
    WRITE(c1,"(i10)") i1h
    WRITE(*,"(a,'[1:',a,']=',1P,10e9.1)") &
            TRIM(ADJUSTL(name)), TRIM(ADJUSTL(c1)),&
            (m(i1), i1=1,i1h)
  END SUBROUTINE DumpMatrixReal1D
  SUBROUTINE DumpMatrixReal2D(name, m)
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL, INTENT(IN) :: m(:,:)
    INTEGER :: n1, n2, i1, i2, i2h
    CHARACTER(LEN=10) :: c1, c2
    n1=SIZE(m,1); n2=SIZE(m,2)
    WRITE(c1,"(i10)") n1
    WRITE(c2,"(i10)") n2
    WRITE(*,"(' Dump Matrix ',a,'[',a,',',a,']')") &
         TRIM(ADJUSTL(name)), TRIM(ADJUSTL(c1)), &
         TRIM(ADJUSTL(c2))
    DO i1 = 1, n1
       WRITE(c1,"(i10)") i1
       i2h = MIN(10,n2)
       WRITE(c2,"(i10)") i2h
       WRITE(*,"(a,'[',a,',1:',a,']=',1P,10e9.1)") &
            TRIM(ADJUSTL(name)), TRIM(ADJUSTL(c1)), &
            TRIM(ADJUSTL(c2)), &
            (m(i1,i2), i2=1,i2h)
    END DO
  END SUBROUTINE DumpMatrixReal2D
  SUBROUTINE DumpMatrixReal3D(name, m)
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL, INTENT(IN) :: m(:,:,:)
    INTEGER :: n1, n2, n3, i1, i2, i3, i2h
    CHARACTER(LEN=10) :: c1, c2, c3
    n1=SIZE(m,1); n2=SIZE(m,2); n3=SIZE(m,3)
    WRITE(c1,"(i10)") n1
    WRITE(c2,"(i10)") n2
    WRITE(c3,"(i10)") n3
    WRITE(*,"(' Dump Matrix ',a,'[',a,',',a,',',a,']')") &
         TRIM(ADJUSTL(name)), TRIM(ADJUSTL(c1)), &
         TRIM(ADJUSTL(c2)), TRIM(ADJUSTL(c3))
    DO i3 = 1, n3
       WRITE(c3,"(i10)") i3
       PRINT *, ''
       DO i1 = 1, n1
          WRITE(c1,"(i10)") i1
          i2h = MIN(10,n2)
          WRITE(c2,"(i10)") i2h
          WRITE(*,"(a,'[',a,',1:',a,',',a,']=',1P,10e9.1)") &
               TRIM(ADJUSTL(name)), TRIM(ADJUSTL(c1)), &
               TRIM(ADJUSTL(c2)), TRIM(ADJUSTL(c3)), &
               (m(i1,i2,i3), i2=1,i2h)
       END DO
    END DO
  END SUBROUTINE DumpMatrixReal3D
  SUBROUTINE DumpMatrixInteger1D(name, m)
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: m(:)
    INTEGER :: n1, i1, i1h
    CHARACTER(LEN=10) :: c1
    n1=SIZE(m,1)
    WRITE(c1,"(i10)") n1
    WRITE(*,"(' Dump Matrix ',a,'[',a,']')") &
         TRIM(ADJUSTL(name)), TRIM(ADJUSTL(c1))
    i1h = MIN(10,n1)
    WRITE(c1,"(i10)") i1h
    WRITE(*,"(a,'[1:',a,']=',10i8)") &
            TRIM(ADJUSTL(name)), TRIM(ADJUSTL(c1)),&
            (m(i1), i1=1,i1h)
  END SUBROUTINE DumpMatrixInteger1D
  SUBROUTINE DumpMatrixInteger2D(name, m)
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: m(:,:)
    INTEGER :: n1, n2, i1, i2, i2h
    CHARACTER(LEN=10) :: c1, c2
    n1=SIZE(m,1); n2=SIZE(m,2)
    WRITE(c1,"(i10)") n1
    WRITE(c2,"(i10)") n2
    WRITE(*,"(' Dump Matrix ',a,'[',a,',',a,']')") &
         TRIM(ADJUSTL(name)), TRIM(ADJUSTL(c1)), &
         TRIM(ADJUSTL(c2))
    DO i1 = 1, n1
       WRITE(c1,"(i10)") i1
       i2h = MIN(10,n2)
       WRITE(c2,"(i10)") i2h
       WRITE(*,"(a,'[',a,',1:',a,']=',10i8)") &
            TRIM(ADJUSTL(name)), TRIM(ADJUSTL(c1)), &
            TRIM(ADJUSTL(c2)), &
            (m(i1,i2), i2=1,i2h)
    END DO
  END SUBROUTINE DumpMatrixInteger2D
  SUBROUTINE DumpMatrixInteger3D(name, m)
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: m(:,:,:)
    INTEGER :: n1, n2, n3, i1, i2, i3, i2h
    CHARACTER(LEN=10) :: c1, c2, c3
    n1=SIZE(m,1); n2=SIZE(m,2); n3=SIZE(m,3)
    WRITE(c1,"(i10)") n1
    WRITE(c2,"(i10)") n2
    WRITE(c3,"(i10)") n3
    WRITE(*,"(' Dump Matrix ',a,'[',a,',',a,',',a,']')") &
         TRIM(ADJUSTL(name)), TRIM(ADJUSTL(c1)), &
         TRIM(ADJUSTL(c2)), TRIM(ADJUSTL(c3))
    DO i3 = 1, n3
       WRITE(c3,"(i10)") i3
       PRINT *, ''
       DO i1 = 1, n1
          WRITE(c1,"(i10)") i1
          i2h = MIN(10,n2)
          WRITE(c2,"(i10)") i2h
          WRITE(*,"(a,'[',a,',1:',a,',',a,']=',10i8)") &
               TRIM(ADJUSTL(name)), TRIM(ADJUSTL(c1)), &
               TRIM(ADJUSTL(c2)), TRIM(ADJUSTL(c3)), &
               (m(i1,i2,i3), i2=1,i2h)
       END DO
    END DO
  END SUBROUTINE DumpMatrixInteger3D


  !  GAUSSIAN POINTS AND WEIGHTS FOR QUADRATURE
  !  OVER LEGENDRE POLINOMIALS BASE FUNCTIONS

  !  Module exports three routines:
  !     CreateGaussQuad   initializes module
  !     DestroyGaussQuad  destroys module
  !     DumpGaussQuade    dumps Gaussian Quadrature

  !  Module export two arrays: 
  !     GaussPoints and GaussWeights
  !
  !  Module uses Module LegPol (Legandre Polinomials);
  !  transparently to the user, that does not have
  !  to create and/or destroy LegPol






  !CreateGaussQuad  computes and hides 'degreeGiven' Gaussian Points 
  !                 and Weights over interval [-1:1];
  !                 creates and uses Mod LegPol.



  SUBROUTINE CreateGaussQuad (degreeGiven, pointsGiven, weightsGiven)
    INTEGER, INTENT(IN) :: degreeGiven
    REAL,    INTENT(IN), OPTIONAL :: pointsGiven(:)
    REAL,    INTENT(IN), OPTIONAL :: weightsGiven(:)
    REAL :: scale
    REAL, ALLOCATABLE :: FVals(:)
    CHARACTER(LEN=*), PARAMETER :: h="**(CreateGaussQuad)**"
    CHARACTER(LEN=10) :: c1
    INTEGER :: j

    !check invocation sequence and input data

    IF (degreeGiven <=0) THEN
       WRITE(c1,"(i10)") degreeGiven
       WRITE(*,"(a,' invoked with degree ',a)") h, TRIM(ADJUSTL(c1))
       STOP
    ELSE
       maxDegree = degreeGiven
    END IF

    !allocate areas

    ALLOCATE (GaussColat(maxDegree/2))
    ALLOCATE (SinGaussColat(maxDegree/2))
    ALLOCATE (CosGaussColat(maxDegree/2))
    ALLOCATE (AuxGaussColat(maxDegree/2))
    ALLOCATE (FVals(maxDegree/2))
    ALLOCATE (GaussPoints(maxDegree))
    ALLOCATE (GaussWeights(maxDegree))
    ALLOCATE (colrad(maxDegree))
    ALLOCATE (rcs2(maxDegree/2))

    !create ModLegPol

    CALL CreateLegPol (maxDegree)

    !Gaussian Points are the roots of legandre polinomial of degree maxDegree

    IF (PRESENT(pointsGiven)) THEN
       GaussPoints = pointsGiven
       GaussColat  = ACOS(pointsGiven(1:maxDegree/2))
       SinGaussColat = SIN(GaussColat)
       AuxGaussColat = 1.0/(SinGaussColat*SinGaussColat)
    ELSE
       GaussColat = LegPolRoots(maxDegree)
       CosGaussColat = COS(GaussColat)
       SinGaussColat = SIN(GaussColat)
       AuxGaussColat = 1.0/(SinGaussColat*SinGaussColat)
       GaussPoints(1:maxDegree/2) = CosGaussColat
       GaussPoints(maxDegree/2+1:maxDegree) = -GaussPoints(maxDegree/2:1:-1)
    END IF

    !Gaussian Weights

    IF (PRESENT(weightsGiven)) THEN
       GaussWeights = weightsGiven
    ELSE
       !
       scale       = 2.0/REAL(maxDegree*maxDegree)
       FVals(1:maxDegree/2) = LegPol(maxDegree-1, GaussColat)    
       GaussWeights(1:maxDegree/2) = scale*&
            (1.0 - CosGaussColat*CosGaussColat)/(FVals*FVals)
       GaussWeights(maxDegree/2+1:maxDegree) = GaussWeights(maxDegree/2:1:-1)
    END IF

  DO j=1,maxDegree/2
     colrad(       j)=    GaussColat(j)
     colrad(maxDegree+1-j)=pai-GaussColat(j)
     rcs2  (j)=AuxGaussColat(j)
  END DO
  END SUBROUTINE CreateGaussQuad



  !DestroyGaussQuad  destroy module internal data;
  !                  destroys module LegPol;
  !                  get ready for new module usage



  SUBROUTINE DestroyGaussQuad
    CHARACTER(LEN=*), PARAMETER :: h="**(DestroyGaussQuad)**"

    !check invocation sequence

    maxDegree = -1

    !destroy ModLegPol

    CALL DestroyLegPol()

    !deallocate module areas
    !
    DEALLOCATE (GaussColat)
    DEALLOCATE (SinGaussColat)
    DEALLOCATE (AuxGaussColat)
    DEALLOCATE (GaussPoints)
    DEALLOCATE (GaussWeights)
  END SUBROUTINE DestroyGaussQuad


  !DumpGaussPoints  dumps gaussian points and weights




  SUBROUTINE DumpGaussQuad()
    CHARACTER(LEN=*), PARAMETER :: h="**(DumpGaussQuad)**"
    CHARACTER(LEN=10) :: c1
    WRITE(c1,"(i10)") maxDegree
    WRITE(*,"(a,' created with maxDegree=',a)") h, TRIM(ADJUSTL(c1))
    CALL DumpMatrix('GaussPoints',GaussPoints)
    CALL DumpMatrix('GaussWeights',GaussWeights)
  END SUBROUTINE DumpGaussQuad
  !
  !  Module usage:
  !     CreateLegPol should be invoked once, before any other routine;
  !     LegPol can be invoked after CreateLegPol, as much as required;
  !     LegPolRoots can be invoked after CreateLegPol, as much as required;
  !     DestroyLegPol should be invoked at the end of the computation.
  !     CreateLegPol estabilishes the maximum degree for which LegPol
  !     and LegPolRoots should be invoked. If this maximum
  !     degree has to be changed, module should be destroied and 
  !     created again.
  !
  !     LegPol and LegPolRoots deal with colatitudes; 
  !     LegPol take colatitudes as abcissas and
  !     LegPolRoots produces colatitudes as roots.
  !     Colatitudes are expressed in radians, with 0 at the
  !     North Pole, pi/2 at the Equator and pi at the South Pole.
  !
  !
  !  Module Hided Data:
  !     AuxPoly1, AuxPoly2 are constants used to evaluate LegPol
  !     nAuxPoly is the maximum degree of the Polinomial to be computed
  !     created specifies if module was created or not
  !

  !
  !  CreateLegPol does module initialization;
  !             should be executed once, prior to LegPol;
  !             input argument is the maximum degree of LegPol invocations
  !
  SUBROUTINE CreateLegPol (maxDegree)
    INTEGER, INTENT(IN) :: maxDegree
    INTEGER i
    !
    !  Check Correction
    !
    IF ( maxDegree <= 0) THEN
       WRITE (0,"('**(CreateLegPol)** maxDegree <= 0; maxDegree =')") maxDegree
       STOP
    ELSE IF (created) THEN
       WRITE (0,"('**(CreateLegPol)** invoked twice without destruction')")
       STOP
    END IF
    !
    !  Allocate and Compute Constants
    !
    nAuxPoly = maxDegree
    ALLOCATE (AuxPoly1(nAuxPoly))
    ALLOCATE (AuxPoly2(nAuxPoly))
    DO i = 1, nAuxPoly
       AuxPoly1(i) = REAL(2*i-1)/REAL(i)
       AuxPoly2(i) = REAL(1-i)/REAL(i)
    END DO
    created = .TRUE.
  END SUBROUTINE CreateLegPol
  !
  ! DestroyLegPol destroys module initialization;
  !               required if maximum degree of LegPol has to be changed;
  !               in this case, invoke DestroyLegPol and CreateLegPol with
  !               new degree
  !
  SUBROUTINE DestroyLegPol
    IF (.NOT. created) THEN
       WRITE (0,"('**(DestroyLegPol)** invoked without initialization')")
       STOP
    END IF
    DEALLOCATE (AuxPoly1, AuxPoly2)
    created = .FALSE.
  END SUBROUTINE DestroyLegPol
  !
  ! LegPol computes Legandre Polinomial of degree 'degree' at a
  !        vector of colatitudes 'Col'
  !        degree should be <= maximum degree estabilished by CreateLegPol
  !
  FUNCTION LegPol (degree, Col)
    INTEGER,                      INTENT(IN ) :: degree
    REAL,    DIMENSION(:),        INTENT(IN ) :: Col
    REAL,    DIMENSION(SIZE(Col))             :: LegPol
    !
    !  Auxiliary Variables
    !
    REAL, DIMENSION(SIZE(Col)) :: P0     ! Polinomial of degree i - 2
    REAL, DIMENSION(SIZE(Col)) :: P1     ! Polinomial of degree i - 1
    REAL, DIMENSION(SIZE(Col)) :: X      ! Cosine of colatitude
    INTEGER iDegree                      ! loop index
    INTEGER left                         ! loop iterations before unrolling
    !
    !  Check Correctness
    !
    IF (.NOT. created) THEN
       WRITE (0,"('**(LegPol)** invoked without initialization')")
       STOP
    END IF
    !
    !  Case degree >=2 and degree <= maximum degree
    !
    IF ((degree >= 2) .AND. (degree <= nAuxPoly)) THEN
       !
       !  Initialization
       !
       left = MOD(degree-1,6)
       X  = COS(Col)
       P0 = 1.0
       P1 = X
       !
       !  Apply recurrence relation
       !     Loop upper bound should be 'degree';
       !     it is not due to unrolling
       !
       DO iDegree = 2, left+1
          LegPol = X*P1*AuxPoly1(iDegree) + P0*AuxPoly2(iDegree)
          P0 = P1; P1 = LegPol
       END DO
       !
       !  Unroll recurrence relation, to speed up computation
       !
       DO iDegree = left+2, degree, 6
          LegPol = X*P1    *AuxPoly1(iDegree  ) + P0    *AuxPoly2(iDegree  )
          P0     = X*LegPol*AuxPoly1(iDegree+1) + P1    *AuxPoly2(iDegree+1)
          P1     = X*P0    *AuxPoly1(iDegree+2) + LegPol*AuxPoly2(iDegree+2)
          LegPol = X*P1    *AuxPoly1(iDegree+3) + P0    *AuxPoly2(iDegree+3)
          P0     = X*LegPol*AuxPoly1(iDegree+4) + P1    *AuxPoly2(iDegree+4)
          P1     = X*P0    *AuxPoly1(iDegree+5) + LegPol*AuxPoly2(iDegree+5)
       END DO
       LegPol = P1
    !
    !  Case degree == 0
    !
    ELSE IF (degree == 0) THEN
       LegPol = 1.0
    !
    !  Case degree == 1
    !
    ELSE IF (degree == 1) THEN
       LegPol = COS(Col)
    !
    !  Case degree <= 0 or degree > maximum degree
    !
    ELSE  
       WRITE (*,"('**(LegPol)** invoked with degree ',i6,&
            &' out of bounds')") degree
       STOP
    END IF
  END FUNCTION LegPol
  !
  !  LegPolRoots computes the roots of the Legandre Polinomial
  !              of even degree 'degree'
  !              Roots are expressed as colatitudes at interval
  !              (0, pi/2) 
  !
  FUNCTION LegPolRoots (degree)
    INTEGER, INTENT(IN) :: degree
    REAL,                 DIMENSION(degree/2) :: LegPolRoots
    REAL,    ALLOCATABLE, DIMENSION(:)      :: XSearch, FSearch
    REAL,    ALLOCATABLE, DIMENSION(:)      :: XLow   , FLow
    REAL,    ALLOCATABLE, DIMENSION(:)      :: XMed   , FMed
    REAL,    ALLOCATABLE, DIMENSION(:)      :: XHigh  , FHigh
    LOGICAL, ALLOCATABLE, DIMENSION(:)      :: Bissec , Mask
    INTEGER i                               ! loop index
    INTEGER nPoints                         ! to start bissection
    INTEGER, PARAMETER :: multSearchStart=4 ! * factor to start bissection
    REAL    step
    REAL    pi
    INTEGER, PARAMETER  :: nDigitsOut=2 ! precision digits of gaussian points:
                                        ! machine epsilon - nDigitsOut
    REAL rootPrecision                  ! relative error in gaussian points
    INTEGER halfDegree
    !
    !  Check Correctness
    !
    IF (.NOT. created) THEN
       WRITE (0,"('**(LegPolRoots)** invoked without initialization')")
       STOP
    ELSE IF ( (degree <= 0) .OR. (degree > nAuxPoly) ) THEN
       WRITE (0,"('**(LegPolRoots)** invoked with degree ',i6,&
            &' out of bounds')") degree
       STOP
    ELSE IF ( MOD(degree,2) .NE. 0) THEN
       WRITE (0,"('**(LegPolRoots)** invoked with odd degree ',i6)") degree
       STOP
    END IF
    !
    !  Initialize Constants
    !
    pi = 4.0 * ATAN(1.0)
    rootPrecision = EPSILON(1.0)*10.0**(nDigitsOut)
    !
    !  LegPolRoots uses root simmetry with respect to pi/2.
    !  It finds all roots in the interval [0,pi/2]
    !  Remaining roots are simmetric
    !
    halfDegree = degree/2
    !
    !  Allocate areas
    !
    ALLOCATE (XLow  (halfDegree))
    ALLOCATE (FLow  (halfDegree))
    ALLOCATE (XMed  (halfDegree))
    ALLOCATE (FMed  (halfDegree))
    ALLOCATE (XHigh (halfDegree))
    ALLOCATE (FHigh (halfDegree))
    ALLOCATE (Bissec(halfDegree))
    !
    !  bissection method to find roots:
    !  get equally spaced points in interval [0,pi/2]
    !  to find intervals containing roots
    !
    nPoints = multSearchStart*halfDegree
    step = pi/(2.0*REAL(nPoints))
    ALLOCATE (XSearch(nPoints))
    ALLOCATE (FSearch(nPoints))
    ALLOCATE (Mask   (nPoints-1))
    DO i = 1, nPoints
       XSearch(i) = step*REAL(i-1)
    END DO
    FSearch = LegPol (degree, XSearch)
    !
    !  select intervals containing roots
    !
    Mask = FSearch(1:nPoints-1)*FSearch(2:nPoints) < 0.0
    !
    !  are there enough intervals?
    !
    IF (COUNT(Mask) .NE. halfDegree) THEN
       WRITE(*,"('**(LegPolRoots)** ',i6,' bracketing intervals to find '&
            &,i6,' roots')") COUNT(Mask), halfDegree
       STOP
    END IF
    !
    !  extract intervals containing roots
    !
    XLow  = PACK(XSearch(1:nPoints-1), Mask)
    XHigh = PACK(XSearch(2:nPoints  ), Mask)
    FLow  = PACK(FSearch(1:nPoints-1), Mask)
    FHigh = PACK(FSearch(2:nPoints  ), Mask)
    DEALLOCATE (XSearch)
    DEALLOCATE (FSearch)
    DEALLOCATE (Mask)
    !
    !  bissection:
    !    loop while there is a root to be found
    !
    Bissec = .TRUE.
    DO WHILE (ANY(Bissec))
       !   
       !   get interval new midpoint and apply stopping criteria
       !   
       XMed = 0.5 * (XLow + XHigh)
       WHERE (Bissec .AND. (XHigh-XLow <= XMed*rootPrecision))
          Bissec = .FALSE.
          LegPolRoots(1:halfDegree) = XMed
       END WHERE
       !
       !  get function at mid-point and reduce interval
       !
       FMed = LegPol (degree, XMed)
       DO i = 1, halfDegree
          IF (Bissec(i)) THEN
             IF (FMed(i) * FLow(i) < 0.0) THEN
                XHigh(i) = XMed(i)
                FHigh(i) = FMed(i)
             ELSE IF (FMed(i) * FHigh(i) < 0.0) THEN
                XLow(i) = XMed(i)
                FLow(i) = FMed(i)
             ELSE  ! Hit a zero
                LegPolRoots(i)  = XMed(i)
                Bissec(i) = .FALSE.
             END IF
          END IF
       END DO
    END DO
    !
    !  Deallocate Areas
    !
    DEALLOCATE (XLow)
    DEALLOCATE (FLow)
    DEALLOCATE (XMed)
    DEALLOCATE (FMed)
    DEALLOCATE (XHigh)
    DEALLOCATE (FHigh)
    DEALLOCATE (Bissec)
  END FUNCTION LegPolRoots

  !        LINEAR ALGEBRA PROCEDURES
  !
  !        Module exports several routines for matrix inversion
  !        and eigenvalue and eigenvector computations
  !
  
  !  Module does not require any other module.
  !  Module does not export any value.

SUBROUTINE Iminv (a,n,d,l,m)
  REAL, INTENT(INOUT) :: a(*)
  REAL, INTENT(OUT) :: d
  INTEGER, INTENT(IN) :: n
  INTEGER, INTENT(OUT) :: l(n), m(n) 
  !
  !        Iminv computes the inverse of matrix a through a gauss-jordan
  !        algorithm. The output matrix overwrites the input.
  !
  !        ...............................................................
  !
  !        search for largest element
  !
  REAL :: biga, hold
  INTEGER :: nk, k, j, iz, i, ij, ki, ji, jk, kj, jr, jq, jp, kk, ik
!cdir novector
  d=1.0
  nk=-n
  DO k=1,n
     nk=nk+n
     l(k)=k
     m(k)=k
     kk=nk+k
     biga=a(kk)
     DO j=k,n
        iz=n*(j-1)
        DO i=k,n
           ij=iz+i
           IF( ABS (biga) <  ABS (a(ij))) THEN
              biga=a(ij)
              l(k)=i
              m(k)=j
           END IF
        END DO
     END DO
     !
     !        interchange rows
     !
     j=l(k)
     IF (j > k) THEN
        ki=k-n
!cdir nodep
        DO i=1,n
           ki=ki+n
           hold=-a(ki)
           ji=ki-k+j
           a(ki)=a(ji)
           a(ji) =hold
        END DO
     END IF
     !
     !        interchange columns
     !
     i=m(k)
     IF (i > k) THEN
        jp=n*(i-1)
!cdir nodep
        DO j=1,n
           jk=nk+j
           ji=jp+j
           hold=-a(jk)
           a(jk)=a(ji)
           a(ji) =hold
        END DO
     END IF
     !
     !        divide column by minus pivot (value of pivot element is
     !        contained in biga)
     !
     IF(biga .EQ. 0.0) THEN
        d=0.0
        RETURN
     END IF
     DO i=1,n
        IF(i .NE. k) THEN
           ik=nk+i
           a(ik)=a(ik)/(-biga)
        END IF
     END DO
     !
     !        reduce matrix
     !
     DO i=1,n
        ik=nk+i
        ij=i-n
!cdir nodep
        DO j=1,n
           ij=ij+n
           IF (i .EQ. k) CYCLE
           IF (j .EQ. k) CYCLE
           kj=ij-i+k
           a(ij)=a(ik)*a(kj)+a(ij)
        END DO
     END DO
     !
     !        divide row by pivot
     !
     kj=k-n
     DO j=1,n
        kj=kj+n
        IF (j .EQ. k) CYCLE
        a(kj)=a(kj)/biga
     END DO
     !
     !        product of pivots
     !
     d=d*biga
     !
     !        replace pivot by reciprocal
     !
     a(kk)=1.0/biga
  END DO
  !
  !        final row and column interchange
  !
  k=n
  DO
     k=(k-1)
     IF (k .LE. 0) RETURN
     i=l(k)
     IF (i > k) THEN
        jq=n*(k-1)
        jr=n*(i-1)
!cdir nodep
        DO j=1,n
           jk=jq+j
           hold=a(jk)
           ji=jr+j
           a(jk)=-a(ji)
           a(ji) =hold
        END DO
     END IF
     j=m(k)
     IF (j .GT. k) THEN
        ki=k-n
!cdir nodep
        DO i=1,n
           ki=ki+n
           hold=a(ki)
           ji=ki-k+j
           a(ki)=-a(ji)
           a(ji) =hold
        END DO
     END IF
  END DO
END SUBROUTINE iminv
SUBROUTINE Balanc(nm,n,a,low,igh,scal)
  !
  !   ** balanc balances a real general matrix, and isolates
  !   **        eigenvalues whenever possible.
  !
  INTEGER, INTENT(IN) :: nm, n
  REAL, INTENT(INOUT) :: a(nm,*)
  REAL, INTENT(OUT) :: scal(*) 
  INTEGER, INTENT(OUT) :: low, igh
  !
  LOGICAL :: noconv
  INTEGER :: i,j,k,l,m,jj,iexc
  REAL :: c,f,g,r,s,b2,radi
  !
  !   ** radi  is a machine dependent parameter specifying
  !            the base of the machine floating pont representation.
  !
  radi = 2.0
  b2 = radi * radi 
  k=1
  l=n
      GOTO 100
  !
  !   ** In-line procedure for row and column exchange.
  !
   20 scal (M)=FLOAT(J)
      IF (J .EQ. M) GOTO 50
  !
      DO 30 I=1,L
      F=A(I,J)
      A(I,J)=A(I,M)
      A(I,M)=F
   30 CONTINUE
  !
      DO 40 I=K,N
      F=A(J,I)
      A(J,I)=A(M,I)
      A(M,I)=F
   40 CONTINUE
  !
   50 GOTO (80,130) IEXC
  !
  !   ** Search for rows isolating an eigenvalue and push them down.
  !
   80 IF (L .EQ. 1) GOTO 280
      L=L-1
  !
  !   ** For J = L  step -1 until 1 DO -- .
  !
  100 DO 120 JJ=1,L
      J=L+1-JJ
  !
      DO 110 I=1,L
      IF (I .EQ. J) GOTO 110
      IF (A(J,I) .NE. 0.) GOTO 120
  110 CONTINUE
  !
      M=L
      IEXC=1
      GOTO 20
  120 CONTINUE
  !
      GOTO 140
  !
  !   ** Search for columns isolating an eigenvalue and push them left.
  !
  130 K=K+1
  !
  140 DO 170 J=K,L
  !
      DO 150 I=K,L
      IF (I .EQ. J) GOTO 150
      IF (A(I,J) .NE. 0.) GOTO 170
  150 CONTINUE
  !
      M=K
      IEXC=2
      GOTO 20
  170 CONTINUE
  !
  !   ** Now balance the submatrix in rows K to L.
  !
      DO 180 I=K,L
      scal (I)=1. 
  180 CONTINUE
  !
  !   ** Interative loop for norm reduction.
  !
  190 NOCONV=.FALSE.
  !
      DO 270 I=K,L
      C=0.
      R=0.
  !
      DO 200 J=K,L
      IF (J .EQ. I) GOTO 200
      C=C+ABS(A(J,I))
      R=R+ABS(A(I,J))
  200 CONTINUE
  !
  !   ** Guard against zero C or R due to underflow.
  !
      IF (C.EQ.0. .OR. R.EQ.0.) GOTO 270
      G=R/radi 
      F=1.0
      S=C+R
  210 IF (C .GE. G) GOTO 220
      F=F*radi 
      C=C*B2
      GOTO 210
  220 G=R*radi 
  230 IF (C .LT.G) GOTO 240
      F=F/radi 
      C=C/B2
      GOTO 230
  !
  !   ** Now balance
  !
  240 IF ((C+R)/F .GE. 0.95*S) GOTO 270
      G=1./F
      scal (I)=scal (I)*F
      NOCONV=.TRUE.
  !
      DO 250 J=K,N
      A(I,J)=A(I,J)*G
  250 CONTINUE
  !
      DO 260 J=1,L
      A(J,I)=A(J,I)*F
  260 CONTINUE
  !
  270 CONTINUE
  !
      IF(NOCONV) GOTO 190
  !
  280 LOW=K
      IGH=L
  !
  RETURN
END SUBROUTINE Balanc
SUBROUTINE Balbak(nm,n,low,igh,scal,m,z)
  !
  !   ** BALBAK forms the eigenvectors of a real general matrix
  !             from the eigenvectors of that matrix
  !             transformed by BALANC.
  !
  INTEGER, INTENT(IN) :: nm,n,low,igh,m
  REAL, INTENT(INOUT) :: z(nm,n)
  REAL, INTENT(IN) :: scal(n)
  !
  INTEGER :: I,J,K,II
  REAL :: S
  !
  IF (M .EQ. 0) GOTO 200
  IF (IGH .EQ. LOW) GOTO 120
  !
  DO 110 I=LOW,IGH
    S=scal (I)
  !
  !   ** Left hand eigenvectors are back transformed
  !      if the foregoing statment is replaced by S = 1.0 / SCALE(I) .
  !
  DO 100 J=1,M
    Z(I,J)=Z(I,J)*S
  100 CONTINUE
  !
  110 CONTINUE
  !
  !   ** For I=LOW-1 step -1 until 1,
  !          IGH+1 step 1 until N DO -- .
  !
  120 DO 140 II=1,N
    I=II
    IF (I.GE.LOW .AND. I.LE.IGH) GOTO 140
    IF (I .LT. LOW) I=LOW-II
    K=INT(scal (I))
    IF (K .EQ. I) GOTO 140
  !
    DO 130 J=1,M
        S=Z(I,J)
        Z(I,J)=Z(K,J)
        Z(K,J)=S
  130 CONTINUE
  !
  140 CONTINUE
  !
  200 RETURN
END SUBROUTINE Balbak
SUBROUTINE Hqr2(nm,n,low,igh,h,wr,wi,z,ierr,matz,machep,tol,*)
  !
  !    ** HQR2 computes the eigenvalues and/or eigenvectors
  !            of a real upper Hessemberg matrix using the
  !            QR method.
  !
  REAL, INTENT(IN) :: machep, tol
  INTEGER, INTENT(IN) :: nm, n, low, igh, matz
  INTEGER, INTENT(OUT) :: ierr
  REAL, INTENT(INOUT) :: h(nm,n),z(nm,n)
  REAL, INTENT(OUT) :: wr(n),wi(n)
  !
  INTEGER :: i,j,k,l,m,en,ll,mm,na,its,mp2,enm2
  !
  REAL :: P,Q,R,S,T,X,W,Y,ZZ,NORM
  !
  LOGICAL :: NOTLAS
  !
  !    ** MACHEP is a machine dependent parameter specifying the
  !              relative precision of the floating point arithmetic.
  !              It must be recomputed and replaced for the specific
  !              machine in use.
  !
      IERR=0
      NORM=0.
      K = 1
  !
  !    ** Store roots isolated by BALANC and compute matrix norm.
  !
      DO 50 I=1,N
  !
      DO 40 J=K,N
      IF (ABS(H(I,J)) .LT. TOL) H(I,J)=TOL
      NORM=NORM+ABS(H(I,J))
   40 CONTINUE
  !
      K=I
      IF (I.GE.LOW .AND. I.LE.IGH) GOTO 50
      WR(I)=H(I,I)
      WI(I)=0.
   50 CONTINUE
  !
      EN=IGH
      T=0.
  !
  !    ** Search for next eigenvalues.
  !
   60 IF (EN .LT. LOW) GOTO 340
      ITS=0
      NA=EN-1
      ENM2=NA-1
  !
  !    ** Look for single small sub-diagonal element.
  !       For L=EN step -1 until LOW DO -- .
  !
   70 DO 80 LL=LOW,EN
      L=EN+LOW-LL
      IF (L .EQ. LOW) GOTO 100
      S=ABS(H(L-1,L-1))+ABS(H(L,L))
      IF (S .EQ. 0.) S=NORM
      IF (ABS(H(L,L-1)) .LE. MACHEP*S) GOTO 100
   80 CONTINUE
  !
  !    ** Form shift.
  !
  100 X=H(EN,EN)
      IF (L .EQ. EN) GOTO 270
      Y=H(NA,NA)
      W=H(EN,NA)*H(NA,EN)
      IF (L .EQ. NA) GOTO 280
      IF (ITS .EQ. 30) GOTO 1000
      IF (ITS.NE.10 .AND. ITS.NE.20) GOTO 130
  !
  !    Form exceptional shift.
  !
      T=T+X
  !
      DO 120 I=LOW,EN
      H(I,I)=H(I,I)-X
  120 CONTINUE
  !
      S=ABS(H(EN,NA))+ABS(H(NA,ENM2))
      X=0.75*S
      Y=X
      W=-0.4375*S*S
  130 ITS=ITS+1
  !
  !    ** Look for two consecutive small sub-diagonal elements.
  !       For M=EN-2 step -1 until L DO -- .
  !
      DO 140 MM=L,ENM2
      M=ENM2+L-MM
      ZZ=H(M,M)
      R=X-ZZ
      S=Y-ZZ
      P=(R*S-W)/H(M+1,M)+H(M,M+1)
      Q=H(M+1,M+1)-ZZ-R-S
      R=H(M+2,M+1)
      S=ABS(P)+ABS(Q)+ABS(R)
      P=P/S
      Q=Q/S
      R=R/S
      IF (M .EQ. L) GOTO 150
      IF (ABS(H(M,M-1))*(ABS(Q)+ABS(R)) .LE. MACHEP*ABS(P)* &
         (ABS(H(M-1,M-1))+ABS(ZZ)+ABS(H(M+1,M+1)))) GOTO 150
  140 CONTINUE
  !
  150 MP2=M+2
  !
      DO 160 I=MP2,EN
      H(I,I-2)=0.
      IF (I .EQ. MP2) GOTO 160
      H(I,I-3)=0.
  160 CONTINUE
  !
  !    ** Double QR step involving rows L to END and columns M to EN
  !
      DO 260 K=M,NA
      NOTLAS=K .NE. NA
      IF (K .EQ. M) GOTO 170
      P=H(K,K-1)
      Q=H(K+1,K-1)
      R=0.
      IF(NOTLAS) R=H(K+2,K-1)
      X=ABS(P)+ABS(Q)+ABS(R)
      IF (X .EQ. 0.) GOTO 260
      P=P/X
      Q=Q/X
      R=R/X
  170 S=SIGN(SQRT(P*P+Q*Q+R*R),P)
      IF (K .EQ. M) GOTO 180
      H(K,K-1)=-S*X
      GOTO 190
  180 IF (L .NE. M) H(K,K-1)=-H(K,K-1)
  190 P=P+S
      X=P/S
      Y=Q/S
      ZZ=R/S
      Q=Q/P
      R=R/P
  !
  !    ** Row modification
  !
      DO 210 J=K,N
      P=H(K,J)+Q*H(K+1,J)
      IF (.NOT.NOTLAS) GOTO 200
      P=P+R*H(K+2,J)
      H(K+2,J)=H(K+2,J)-P*ZZ
  200 H(K+1,J)=H(K+1,J)-P*Y
      H(K,J)=H(K,J)-P*X
  210 CONTINUE
  !
      J=MIN(EN,K+3)
  !
  !    ** Column modification
  !
      DO 230 I=1,J
      P=X*H(I,K)+Y*H(I,K+1)
      IF (.NOT.NOTLAS) GOTO 220
      P=P+ZZ*H(I,K+2)
      H(I,K+2)=H(I,K+2)-P*R
  220 H(I,K+1)=H(I,K+1)-P*Q
      H(I,K)=H(I,K)-P
  230 CONTINUE
  !
      IF (MATZ .EQ. 0) GOTO 260
  !
  !    ** Accumulate transformations
  !
      DO 250 I=LOW,IGH
      P=X*Z(I,K)+Y*Z(I,K+1)
      IF (.NOT.NOTLAS) GOTO 240
      P=P+ZZ*Z(I,K+2)
      Z(I,K+2)=Z(I,K+2)-P*R
  240 IF (ABS(P) .LT. TOL) P=TOL
      Z(I,K+1)=Z(I,K+1)-P*Q
      Z(I,K)=Z(I,K)-P
  250 CONTINUE
  !
  260 CONTINUE
  !
      GOTO 70
  !
  !    ** One root found.
  !
  270 H(EN,EN)=X+T
      WR(EN)=H(EN,EN)
      WI(EN)=0.
      EN=NA
      GOTO 60
  !
  !    ** Two roots found.
  !
  280 P=(Y-X)*0.5 
      Q=P*P+W
      ZZ=SQRT(ABS(Q))
      H(EN,EN)=X+T
      X=H(EN,EN)
      H(NA,NA)=Y+T
      IF (Q .LT. 0.) GOTO 320
  !
  !    ** Real pair.
  !
      ZZ=P+SIGN(ZZ,P)
      WR(NA)=X+ZZ
      WR(EN)=WR(NA)
      IF (ZZ .NE. 0.) WR(EN)=X-W/ZZ
      WI(NA)=0.
      WI(EN)=0.
  !
      IF (MATZ .EQ. 0) GOTO 330
  !
      X=H(EN,NA)
      S=ABS(X)+ABS(ZZ)
      P=X/S
      Q=ZZ/S
      R=SQRT(P*P+Q*Q)
      P=P/R
      Q=Q/R
  !
  !    ** Row  modification.
  ! 
      DO 290 J=NA,N
      ZZ = H(NA,J)
      H(NA,J)=Q*ZZ+P*H(EN,J)
      H(EN,J)=Q*H(EN,J)-P*ZZ
  290 CONTINUE
  !
  !    ** Column modification.
  !
      DO 300 I=1,EN
      ZZ=H(I,NA)
      H(I,NA)=Q*ZZ+P*H(I,EN)
      H(I,EN)=Q*H(I,EN)-P*ZZ
  300 CONTINUE
  !
  !    ** Accumulate transformations.
  !
      DO 310 I=LOW,IGH
      ZZ=Z(I,NA)
      Z(I,NA)=Q*ZZ+P*Z(I,EN)
      Z(I,EN)=Q*Z(I,EN)-P*ZZ
  310 CONTINUE
  !
      GOTO 330
  !
  !    ** Complex pair
  !
  320 WR(NA)=X+P
      WR(EN)=X+P
      WI(NA)=ZZ
      WI(EN)=-ZZ
  !
  330 EN=ENM2
  !
      GOTO 60
  !
  340 IF (MATZ .EQ. 0) RETURN 1
  !
  !    ** All roots found.
  !       Backsubstitute to find vectors of upper triangular form.
  !
      IF (NORM .EQ. 0.) GOTO 1001
  !
      CALL HQR3(NM,N,LOW,IGH,H,WR,WI,Z,MACHEP,NORM)
  !
      GOTO 1001
  !
  !    ** Set error - no convergence to an
  !       eigenvalue after 30 iterations
  !
 1000 IERR = EN
 1001 RETURN
END SUBROUTINE Hqr2
SUBROUTINE Hqr3(nm,n,low,igh,h,wr,wi,z,machep,norm)
  !
  !    ** HQR3 backsubstitutes to find
  !            vectors of upper triangular form.
  !
  INTEGER, INTENT(IN) ::  nm,n,low,igh
  REAL, INTENT(INOUT) ::  h(nm,n)
  REAL, INTENT(IN) ::  wr(n),wi(n),machep,norm
  REAL, INTENT(OUT) :: z(nm,n)

  !
  INTEGER ::  i,j,k,m,en,ii,jj,na,nn,enm2
  REAL :: P,Q,R,S,T,X,W,Y,AR,AI,BR,BI,RA,SA,ZZ
  REAL ZD,A1,A2,A3,A4
  !
  ZD(A1,A2,A3,A4)=(A1*A3+A2*A4)/(A3*A3+A4*A4)
  !
  !    ** For EN=N step -1 until 1 DO -- .
  !
      DO 800 NN=1,N
      EN=N+1-NN
      P=WR(EN)
      Q=WI(EN)
      NA=EN-1
      IF (Q) 710,600,800
  !
  !    ** Real vector.
  !
  600 M=EN
      H(EN,EN)=1. 
      IF (NA .EQ. 0) GOTO 800
  !
  !    ** For I=EN-1 step -1 until 1 DO -- .
  !
      DO 700 II=1,NA
      I=EN-II
      W=H(I,I)-P
      R=H(I,EN)
      IF (M .GT. NA) GOTO 620
  !
      DO 610 J=M,NA
      R=R+H(I,J)*H(J,EN)
  610 CONTINUE
  !
  620 IF (WI(I) .GE. 0.) GOTO 630
      ZZ=W
      S=R
      GOTO 700
  630 M=I
      IF (WI(I) .NE. 0.) GOTO 640
      T=W
      IF (W .EQ. 0.) T=MACHEP*NORM
      H(I,EN)=-R/T
      GOTO 700
  !
  !    ** Solve real equations.
  !
  640 X=H(I,I+1)
      Y=H(I+1,I)
      Q=(WR(I)-P)*(WR(I)-P)+WI(I)*WI(I)
      T=(X*S-ZZ*R)/Q
      H(I,EN)=T
      IF (ABS(X) .LE. ABS(ZZ)) GOTO 650
      H(I+1,EN)=(-R-W*T)/X
      GOTO 700
  650 H(I+1,EN)=(-S-Y*T)/ZZ
  !
  700 CONTINUE
  !
  !    ** End real vector.
  !
      GOTO 800
  !
  !    ** Complex vector.
  !
  710 M=NA
  !
  !    ** Last vector component chosen imaginary so that
  !       eigenvector matrix is triangular.
  !
      IF (ABS(H(EN,NA)) .LE. ABS(H(NA,EN))) GOTO 720
      H(NA,NA)=Q/H(EN,NA)
      H(NA,EN)=-(H(EN,EN)-P)/H(EN,NA)
      GOTO 730
  720 H(NA,NA)=ZD(0.0 ,-H(NA,EN),H(NA,NA)-P,Q)
      H(NA,EN)=ZD(-H(NA,EN),0.0 ,H(NA,NA)-P,Q)
  730 H(EN,NA)=0.
      H(EN,EN)=1.
      ENM2=NA-1
      IF (ENM2 .EQ. 0) GOTO 800
  !
  !    ** For I=EN-2 step -1 until 1 DO -- .
  !
      DO 790 II=1,ENM2
      I=NA-II
      W=H(I,I)-P
      RA=0.
      SA=H(I,EN)
  !
      DO 760 J=M,NA
      RA=RA+H(I,J)*H(J,NA)
      SA=SA+H(I,J)*H(J,EN)
  760 CONTINUE
  !
      IF (WI(I) .GE. 0.) GOTO 770
      ZZ=W
      R=RA
      S=SA
      GOTO 790
  770 M=I
      IF (WI(I) .NE. 0.) GOTO 780
      H(I,NA)=ZD(-RA,-SA,W,Q)
      H(I,EN)=ZD(-SA,RA,W,Q)
      GOTO 790
  !
  !    ** Solve complex equations.
  !
  780 X=H(I,I+1)
      Y=H(I+1,I)
      AR=X*R-ZZ*RA+Q*SA
      AI=X*S-ZZ*SA-Q*RA
      BR=(WR(I)-P)*(WR(I)-P)+WI(I)*WI(I)-Q*Q
      BI=(WR(I)-P)*2.0*Q
      IF (BR.EQ.0. .AND. BI.EQ.0.) BR=MACHEP*NORM*&
         (ABS(W)+ABS(Q)+ABS(X)+ABS(Y)+ABS(ZZ))
      H(I,NA)=ZD(AR,AI,BR,BI)
      H(I,EN)=ZD(AI,-AR,BR,BI)
      IF (ABS(X) .LE. (ABS(ZZ)+ABS(Q))) GOTO 785
      H(I+1,NA)=(-RA-W*H(I,NA)+Q*H(I,EN))/X
      H(I+1,EN)=(-SA-W*H(I,EN)-Q*H(I,NA))/X
      GOTO 790
  785 H(I+1,NA)=ZD(-R-Y*H(I,NA),-S-Y*H(I,EN),ZZ,Q)
      H(I+1,EN)=ZD(-S-Y*H(I,EN),R+Y*H(I,NA),ZZ,Q)
  790 CONTINUE
  !
  !    ** End complex vector.
  !
  800 CONTINUE
  !
  !    ** End back substitution.
  !       vectors of isolated roots.
  !
      DO 840 I=1,N
      IF (I.GE.LOW .AND. I.LE.IGH) GOTO 840
      DO 820 J=I,N
      Z(I,J)=H(I,J)
  820 CONTINUE
  !
  840 CONTINUE
  !
  !    ** Multiply by transformations matrix to give
  !       vectors of original full matrix.
  !       For J=N step -1 until LOW DO -- .
  !
      DO 880 JJ=LOW,N
      J=N+LOW-JJ
      M=MIN(J,IGH)
  !
      DO 880 I=LOW,IGH
      ZZ=0.
  !
      DO 860 K=LOW,M
      ZZ=ZZ+Z(I,K)*H(K,J)
  860 CONTINUE
  !
      Z(I,J)=ZZ
  880 CONTINUE
  !
      RETURN
END SUBROUTINE Hqr3
SUBROUTINE ortran(nm,n,low,igh,a,ort,z,tolh)
  !
  !   ** ORTRAN accumulates the orthogonal similarity
  !             tranformations used in the reduction of a real
  !             general matrix to upper Hessemberg form
  !             by ORTHES.
  !
  INTEGER, INTENT(IN) ::  nm,n,low,igh
  REAL, INTENT(IN) ::  a(nm,n), tolh
  REAL, INTENT(OUT) ::  z(nm,n),ort(n)
  !
  INTEGER ::  i,j,kl,mm,mp,mp1
  REAL :: g
  !
  !   ** Initialize Z to identity matrix
  !
      DO 80 I=1,N
      DO 60 J=1, N
      Z(I,J)=0.0 
   60 CONTINUE
      Z(I,I)=1.0
   80 CONTINUE
  !
      KL=IGH-LOW-1
      IF (KL .LT. 1) GOTO 200
  !
  !   ** For MP = IGH-1 step -1 until LOW+1 DO --.
  !
      DO 140 MM=1,KL
      MP=IGH-MM
      IF (A(MP,MP-1) .EQ. 0.) GOTO 140
      MP1=MP+1
  !
      DO 100 I=MP1,IGH
      ORT(I)=A(I,MP-1)
  100 CONTINUE
  !
      DO 130 J=MP,IGH
      G=0.
  !
      DO 110 I=MP,IGH
      G=G+ORT(I)*Z(I,J)
  110 CONTINUE
  !
  !   ** Divisor below is negative of H formed in ORTHES
  !      double division avoids possible underflow.
  !
      G=(G/ORT(MP))/A(MP,MP-1)
  !
      DO 120 I=MP,IGH
      Z(I,J)=Z(I,J)+G*ORT(I)
      IF (ABS(Z(I,J)) .LT. TOLH) Z(I,J)=TOLH
  120 CONTINUE
  !
  130 CONTINUE
  !
  140 CONTINUE
  !
  200 RETURN
END SUBROUTINE Ortran

SUBROUTINE Orthes(nm,n,low,igh,a,ort,tolh)
  !
  !   ** ORTHES reduces a real general matrix to upper
  !             Hessemberg form using orthogonal similarity.
  !
  INTEGER, INTENT(IN) :: nm, n, low, igh
  REAL, INTENT(INOUT) :: a(nm,n)
  REAL, INTENT(OUT) :: ort(igh)
  REAL, INTENT(IN) :: tolh
  !
  INTEGER :: I,J,M,II,JJ,LA,MP,KP1
  REAL :: F,G,H,SCAL
  !
      LA=IGH-1
      KP1=LOW+1
      IF (LA .LT. KP1) GOTO 200
  !
      DO 180 M=KP1,LA
      H=0.
      ORT(M)=0.
      SCAL=0.
  !
  !   ** Scale column.
  !
      DO 90 I=M,IGH
      SCAL=SCAL+ABS(A(I,M-1))
   90 CONTINUE
  !
      IF (SCAL .EQ. 0.) GOTO 180
      MP=M+IGH
  !
  !   ** For I = IGH step -1 until M DO -- .
  !
      DO 100 II=M,IGH
      I=MP-II
      ORT(I)=A(I,M-1)/SCAL
      H=H+ORT(I)*ORT(I)
  100 CONTINUE
  !
      G=-SIGN(SQRT(H),ORT(M))
      H=H-ORT(M)*G
      ORT(M)=ORT(M)-G
  !
  !   ** Form (I-(U*UT)/H) * A .
  !
      DO 130 J=M,N
      F=0.
  !
  !   ** For I = IGH step -1 until M DO -- .
  !
      DO 110 II=M,IGH
      I=MP-II
      F=F+ORT(I)*A(I,J)
  110 CONTINUE
  !
      F=F/H
  !
      DO 120 I=M,IGH
      A(I,J)=A(I,J)-F*ORT(I)
      IF (ABS(A(I,J)) .LT. TOLH) A(I,J)=TOLH
  120 CONTINUE
  !
  130 CONTINUE
  !
  !   ** Form (I-(U*UT)/H) * A * (I-(U*UT)/H) .
  !
      DO 160 I=1,IGH
      F=0.
  !
  !   ** For J = IGH step -1 until M DO -- .
  !
      DO 140 JJ=M,IGH
      J=MP-JJ
      F=F+ORT(J)*A(I,J)
  140 CONTINUE
  !
      F=F/H
  !
      DO 150 J=M,IGH
      A(I,J)=A(I,J)-F*ORT(J)
      IF (ABS(A(I,J)) .LT. TOLH) A(I,J)=TOLH
  150 CONTINUE
  !
  160 CONTINUE
  !
      ORT(M)=SCAL*ORT(M)
      A(M,M-1)=SCAL*G
      IF (ABS(A(M,M-1)) .LT. TOLH) A(M,M-1)=TOLH
  180 CONTINUE
  !
  200 RETURN
END SUBROUTINE Orthes


SUBROUTINE Rg(nm,n,a,wr,wi,matz,z,ierr,eps,scal,ort)
  !
  !   ** RG calculates the eigenvalues and/or eigenvectors
  !         of a real general matrix.
  !
  !   ** Arguments:
  !
  !   ** NM - row dimension of matrix A at the calling routine: Input
  !           Integer variable.
  !
  !   ** N - current dimension of matrix A: Input
  !           Integer variable; must be .LE. NM.
  !
  !   ** A - real matrix (destroyed): Input
  !          Real array with dimensions A(NM,N).
  !
  !   ** WR - real part of the eigenvalues: Output
  !           Real vector with dimensions WR(N).
  !
  !   ** WI - imaginary part of the eigenvalues: Output
  !           Real vector with dimensions WI(N).
  !
  !           OBS: There is nor ordenation for the eigenvalues, except
  !                that for the conjuate complex pairs are put together
  !                and the pair with real positive imaginary part comes
  !                in first place.
  !
  !   ** MATZ - integer variable: Input
  !             Meaning:
  !   ** MATZ = 0 - only eigenvalues non-filtered
  !   ** MATZ = 1 - eigenvalues and eigenvectors normalized and filtered
  !   ** MATZ = 2 - eigenvalues and eigenvectors non-norm. and non-filt.
  !   ** MATZ = 3 - eigenvalues and eigenvectors normalized, non-filt.
  !   **            and without zeroes for .LE. TOLx (see ZNORMA).
  !
  !   ** Z - Eigenvectors: real and imaginary parts, so that:
  !          a) for a real J-th eigenvalue WR(J).NE.0.AND.WI(J).EQ.0,
  !             the J-th eigenvector is (Z(I,J),I=1,N);
  !          b) for a imaginary J-th eigenvalue with WI(J).NE.0,
  !             the (J+1)-th eigenvalue is its conjugate complex;
  !             the J-th eigenvector has real part (Z(I,J),I=1,N) and
  !             imaginary part (Z(I,J+1),I=1,N), and the (J+1)-th
  !             eigenvector has real part (Z(I,J),I=1,N) and
  !             imaginary part (-Z(I,J+1),I=1,N).
  !          Real array with dimensions Z(NM,N): Output
  !
  !   ** IERR - is a integer variable: Output, indicating:
  !             - if N .GT. NM, then the routine RG stop calculations
  !               and returns with IERR=10*N;
  !             - if 30 iteractions is exceeded for the J-th eigenvalue
  !               computation, then the routine RG stop calculations
  !               and returns with IERR=J and the J+1, J+2, ..., N
  !               eigenvalues are computed, but none eigenvector is
  !               computed;
  !             - for a normal termination IERR is set zero.
  !
  !   ** EPS - is a machine dependent parameter specifying the
  !            relative precision of the floating point arithmetic.
  !            It must be recomputed for the specific machine in use.
  !            It is 2**(-20) for 32 bitsand 2**(-50) for 64 bits.
  !            Real variable: Input.
  !
  !   ** TOLH - tolerance value to filter the Hessemberg matrix.
  !             Real variable: Local.
  !
  !   ** TOLW - tolerance value to filter the eigenvalues.
  !             Real variable: Local.
  !
  !   ** TOLZ - tolerance value to filter the eigenvectors.
  !             Real variable: Local.
  !
  !   ** SCALE - working real vector with dimensions SCALE(N).
  !
  !   ** ORT - working real vector with dimensions ORT(N).
  !
  INTEGER, INTENT(IN) ::  nm,n,matz
  REAL, INTENT(INOUT) :: a(nm,n)
  REAL, INTENT(OUT) :: z(nm,n),wr(n),wi(n),scal(n),ort(n)
  REAL, INTENT(IN) :: eps
  INTEGER, INTENT(OUT) :: ierr
  !
  INTEGER :: low,igh
  REAL :: TOLH,TOLW,TOLZ
  !
      TOLH=EPS
      TOLW=EPS
      TOLZ=EPS
  !
      IF (N .GT. NM) THEN
      IERR=10*N
      RETURN
      ENDIF
  !
  !   Performing the balance of the input real general matrix
  !   (in place).
  !
      CALL BALANC(NM,N,A,LOW,IGH,SCAL)
  !
  !   Performing the redution of the balanced matrix (in place) to
  !   the Hessemberg superior form. It is used similarity orthogonal
  !   transformations.
  !
      CALL ORTHES(NM,N,LOW,IGH,A,ORT,TOLH)
  !
      IF (MATZ .NE. 0) THEN
  !
  !   Saving the transformations above for eigenvector computations.
  !
      CALL ORTRAN(NM,N,LOW,IGH,A,ORT,Z,TOLH)
      ENDIF
  !
  !   Computing the eigenvalues/eigenvectors of the Hessemberg matrix
  !   using the QR method.
  !
      CALL HQR2(NM,N,LOW,IGH,A,WR,WI,Z,IERR,MATZ,EPS,TOLH,*10)
  !
      IF (IERR .EQ. 0) THEN
  !
  !   Back-transforming the eigenvectors of the Hessembeg matrix to
  !   the eigenvectors of the original input matrix.
  !
      CALL BALBAK(NM,N,LOW,IGH,SCAL,N,Z)
  !
  !   Normalizing and filtering the eigenvectors (See MATZ above and
  !   comments inside ZNORMA routine).
  !
      CALL ZNORMA(NM,N,WR,WI,Z,MATZ,A,TOLW,TOLZ)
      ENDIF
  !
   10 IF (IERR .EQ. 0) RETURN
      WRITE(*,20) IERR
   20 FORMAT(/,1X,'**** The',I4,1X,'-th Eigenvalue Did Not Converge ',&
             '****',//)
  !
      RETURN
END SUBROUTINE Rg
SUBROUTINE Tql2(nm,n,d,e,z,eps,ierr)
  !
  !   Abstract: Computes the Eigenvalues and Eigenvectors of a real
  !             Symmetric Tridiagonal Matrix Using the QL Method.
  !
  INTEGER, INTENT(IN) ::  nm,n
  INTEGER, INTENT(OUT) ::  ierr
  REAL, INTENT(INOUT) ::  d(n),e(n),z(nm,n)
  REAL, INTENT(IN) :: eps
  !
  REAL :: B,C,F,G,H,P,R,S
  INTEGER :: itm=50
  INTEGER ::  i,j,k,l,m,ii,l1,mml
  !
  !   EPS is a Machine Dependent Parameter Specifying the
  !       Relative Precision of the Floating Point Arithmetic.
  !
  !   EPS = 2.0 ** -50 - 64 BITS
  !   EPS = 2.0 ** -20 - 32 BITS
  !
      IERR=0
  !
      DO 100 I=2,N
      E(I-1)=E(I)
  100 CONTINUE
  !
      F=0.0 
      B=0.0 
      E(N)=0.0 
  !
      DO 240 L=1,N
      J=0
      H=EPS*(ABS(D(L))+ABS(E(L)))
      IF (B .LT. H) B=H
  !
  !   Look for Small Sub-diagonal Element.
  !
      DO 110 M=L,N
      IF (ABS(E(M)) .LE. B) GOTO 120
  !
  !   E(N) is Always Zero, so there is No Exit
  !        Through the Bottom of the Loop.
  !
  110 CONTINUE
  !
  120 IF (M .EQ. L) GOTO 220
  130 IF (J .EQ. ITM) THEN
  !
  !   No Convergence to an Eigenvalue after 50 Iterations.
  !
      IERR=L
      WRITE(6,400) L
  400 FORMAT(/,' *** The',I4,'-th Eigenvalue Did Not Converge ***',/)
      RETURN
      ENDIF
  !
      J=J+1
  !
  !   Form Shift.
  !
      L1=L+1
      G=D(L)
      P=(D(L1)-G)/(2.0*E(L))
      R=SQRT(P*P+1.0)
      D(L)=E(L)/(P+SIGN(R,P))
      H=G-D(L)
  !
      DO 140 I=L1,N
      D(I)=D(I)-H
  140 CONTINUE
  !
      F=F+H
  !
  !   QL Transformation.
  !
      P=D(M)
      C=1.0
      S=0.0 
      MML=M-L
  !
  !   For I=M-1 Step -1 Until L DO -- .
  !
      DO 200 II=1,MML
      I=M-II
      G=C*E(I)
      H=C*P
  !
      IF (ABS(P) .LT. ABS(E(I))) THEN
      C=P/E(I)
      R=SQRT(C*C+1.0)
      E(I+1)=S*E(I)*R
      S=1.0/R
      C=C*S
      ELSE
      C=E(I)/P
      R=SQRT(C*C+1.0)
      E(I+1)=S*P*R
      S=C/R
      C=1.0/R
      ENDIF
  !
      P=C*D(I)-S*G
      D(I+1)=H+S*(C*G+S*D(I))
  !
  !   Form Vector.
  !
      DO 180 K=1,N
      H=Z(K,I+1)
      Z(K,I+1)=S*Z(K,I)+C*H
      Z(K,I)=C*Z(K,I)-S*H
  180 CONTINUE
  !
  200 CONTINUE
  !
      E(L)=S*P
      D(L)=C*P
      IF (ABS(E(L)) .GT. B) GOTO 130
  220 D(L)=D(L)+F
  240 CONTINUE
  !
  !   Order Eigenvalues and Eigenvectors.
  !
      DO 300 II=2,N
      I=II-1
      K=I
      P=D(I)
  !
      DO 260 J=II,N
      IF (D(J) .LT. P) THEN
      K=J
      P=D(J)
      ENDIF
  260 CONTINUE
  !
      IF (K .NE. I) THEN
      D(K)=D(I)
      D(I)=P
  !
      DO 280 J=1,N
      P=Z(J,I)
      Z(J,I)=Z(J,K)
      Z(J,K)=P
  280 CONTINUE
      ENDIF
  !
  300 CONTINUE
  !
      RETURN
END SUBROUTINE Tql2
SUBROUTINE Tred2(nm,n,a,d,e,z)
  !
  INTEGER, INTENT(IN) :: n,nm
  REAL, INTENT(IN) :: a(nm,n)
  REAL, INTENT(OUT) :: d(n),e(n),z(nm,n)
  !
  REAL :: F,G,H,HH,SCAL
  INTEGER I,J,K,L,II,JP1
  !
      DO 100 I=1,N
      DO 100 J=1,I
      Z(I,J)=A(I,J)
  100 CONTINUE                          
  !
      IF (N .EQ. 1) GOTO 320
  !   for I=N step -1 until 2 do
      DO 300 II=2,N
      I=N+2-II
      L=I-1
      H=0.0 
      SCAL=0.0 
      IF (L .LT. 2) GOTO 130
  !   scale row (algol tol then not needed)
      DO 120 K=1,L
      SCAL=SCAL+ABS(Z(I,K))
  120 CONTINUE
  !
      IF (SCAL .NE. 0.0 ) GOTO 140
  130 E(I)=Z(I,L)
      GOTO 290
  !
  140 DO 150 K=1,L
      Z(I,K)=Z(I,K)/SCAL
      H=H+Z(I,K)*Z(I,K)
  150 CONTINUE
  !
      F=Z(I,L)
      G=-SIGN(SQRT(H),F)
      E(I)=SCAL*G
      H=H-F*G
      Z(I,L)=F-G
      F=0.0 
  !
      DO 240 J=1,L
      Z(J,I)=Z(I,J)/H
      G=0.0 
  !   form element of A*U
      DO 180 K=1,J
      G=G+Z(J,K)*Z(I,K)
  180 CONTINUE
  !
      JP1=J+1
      IF (L .LT. JP1) GO TO 220
  !
      DO 200 K=JP1,L
      G=G+Z(K,J)*Z(I,K)
  200 CONTINUE
  !   form element of P
  220 E(J)=G/H
      F=F+E(J)*Z(I,J)
  240 CONTINUE
  !
      HH=F/(H+H)
  !   form reduced A
      DO 260 J=1,L
      F=Z(I,J)
      G=E(J)-HH*F
      E(J)=G
  !
      DO 260 K=1,J
      Z(J,K)=Z(J,K)-F*E(K)-G*Z(I,K)
  260 CONTINUE
  !
  290 D(I)=H
  300 CONTINUE
  !
  320 D(1)=0.0 
      E(1)=0.0 
  !   accumulation of transformation matrices
      DO 500 I=1,N
      L=I-1
      IF (D(I) .EQ. 0.0 ) GOTO 380
  !
      DO 360 J=1,L
      G=0.0 
  !
      DO 340 K=1,L
      G=G+Z(I,K)*Z(K,J)
  340 CONTINUE
  !
      DO 360 K=1,L
      Z(K,J)=Z(K,J)-G*Z(K,I)
  360 CONTINUE
  !
  380 D(I)=Z(I,I)
      Z(I,I)=1.0 
      IF (L .LT. 1) GOTO 500
  !
      DO 400 J=1,L
      Z(I,J)=0.0 
      Z(J,I)=0.0 
  400 CONTINUE
  !
  500 CONTINUE
  !
      RETURN
END SUBROUTINE Tred2
SUBROUTINE Znorma(nm,n,wr,wi,z,matz,h,tolw,tolz)
  !
  !   ** ZNORMA normalizes and filters the eigenvectors and filters the
  !             eigenvalues.
  !
  !      It sets ZZ = a + b * i, corresponding to the maximum absolute
  !      value of the eigenvector, to:
  !
  !      a) 1.0 + 0.0   * i - if B .EQ. 0.0
  !      b) 1.0 + (b/a) * i - if  ABS(a) .GE. ABS(b)
  !      c) 1.0 + (a/b) * i - if  ABS(a) .LT. ABS(b)
  !
  INTEGER, INTENT(IN) :: nm,n,matz
  REAL, INTENT(INOUT) :: z(nm,n),wr(n),wi(n)
  REAL, INTENT(OUT) :: h(nm,n)
  REAL, INTENT(IN) :: tolw, tolz
  !
  REAL :: ZZ,DIV
  INTEGER :: I,J,IC,J1
  !
      IF (MATZ .EQ. 2) RETURN
  !
      DO 900 J=1,N
      DO 900 I=1,N
      H(I,J)=Z(I,J)
  900 CONTINUE
  !
      DO 910 J=1,N
  !
      IF (WI(J) .EQ. 0.0 ) THEN
  !
      ZZ=0.0 
      DO 940 I=1,N
      ZZ=MAX(ZZ,ABS(H(I,J)))
      IF (ZZ .EQ. ABS(H(I,J))) IC=I
  940 CONTINUE
  !
      DO 950 I=1,N
      Z(I,J)=H(I,J)/H(IC,J)
  950 CONTINUE
  !
      ELSEIF (WI(J) .GT. 0.0 ) THEN
  !
      ZZ=0.0 
      J1=J+1
      DO 960 I=1,N
      DIV=H(I,J)*H(I,J)+H(I,J1)*H(I,J1)
      ZZ=MAX(ZZ,DIV)
      IF (ZZ .EQ. DIV) IC=I
  960 CONTINUE
      IF (ABS(H(IC,J)) .LT. ABS(H(IC,J1))) THEN
      DIV=1.0/H(IC,J1)
      ELSE
      DIV=1.0/H(IC,J)
      ENDIF
      IF (DIV .NE. 0.0 ) THEN
      DO 970 I=1,N
      Z(I,J)=H(I,J)*DIV
      Z(I,J1)=H(I,J1)*DIV
  970 CONTINUE
      ENDIF
  !
      ENDIF
  !
  910 CONTINUE
  !
      IF (MATZ .EQ. 3) RETURN
  !
      DIV=0.0 
      DO 980 J=1,N
      ZZ=SQRT(WR(J)*WR(J)+WI(J)*WI(J))
      DIV=MAX(DIV,ZZ)
  980 CONTINUE
      IF (DIV .LE. 0.0 ) DIV=1.0
  !
      DO 990 J=1,N
      IF (ABS(WR(J)/DIV) .LT. TOLW) WR(J)=0.0 
      IF (ABS(WI(J)/DIV) .LT. TOLW) WI(J)=0.0 
      DO 990 I=1,N
      IF (ABS(Z(I,J)) .LT. TOLZ) Z(I,J)=0.0 
  990 CONTINUE
  !
      RETURN
END SUBROUTINE Znorma



  SUBROUTINE tmstmp2(id, ifday, tod, ihr, iday, mon, iyr)
    !
    !
    !==========================================================================
    !    id(4).......date of current data
    !                id(1)....hour(00/12)
    !                id(2)....month
    !                id(3)....day of month
    !                id(4)....year
    !    ifday.......model forecast day
    !    tod.........todx=tod+swint*f3600, model forecast time of 
    !                day in seconds
    !                swint....sw subr. call interval in hours
    !                swint has to be less than or equal to trint
    !                              and mod(trint,swint)=0 
    !                f3600=3.6e3
    !    ihr.........hour(00/12)
    !    iday........day of month
    !    mon.........month
    !    iyr.........year
    !    yrl.........length of year in days      
    !    monl(12)....length of each month in days 
    !==========================================================================
    !
    IMPLICIT NONE
    INTEGER, INTENT(in ) :: id(4)
    INTEGER, INTENT(in ) :: ifday
    REAL,    INTENT(in ) :: tod
    INTEGER, INTENT(out) :: ihr
    INTEGER, INTENT(out) :: iday
    INTEGER, INTENT(out) :: mon
    INTEGER, INTENT(out) :: iyr

    INTEGER :: kday 
    INTEGER :: idaymn
    REAL    :: ctim 
    REAL    :: hrmodl
    INTEGER :: monl(12)

    REAL, PARAMETER :: yrl =   365.2500
    REAL, PARAMETER ::  ep = .015625
    DATA MONL/31,28,31,30,31,30,&
         31,31,30,31,30,31/

    ctim=tod+id(1)*3600.0

    IF (ctim >= 86400.e0) THEN
       kday=1
       ctim=ctim-86400.e0
    ELSE
       kday=0
    END IF
    !
    !     adjust time to reduce round off error in divsion
    !
    iday = id(3) + ifday + kday
    hrmodl = (ctim+ep)/3600.0
    ihr = hrmodl
    mon = id(2)
    iyr = id(4)
    DO
       idaymn = monl(mon)
       IF (yrl == 365.25e0 .AND. MOD(iyr,4) == 0 .AND. mon == 2) &
            idaymn=29
       IF (iday <= idaymn) RETURN
       iday = iday - idaymn
       mon = mon + 1
       IF (mon < 13) CYCLE
       mon = 1
       iyr = iyr + 1
    END DO
  END SUBROUTINE tmstmp2






  SUBROUTINE InitTransDiagCol()
    INTEGER :: m, n, col, dia
    ALLOCATE (DiagPerCol(2*mnMax))
    ALLOCATE (ColPerDiag(2*mnMax))
    col = 0
    DO m = 1, mMax
       DO n = m, nMax
          col = col+1
          dia = mnMap(m,n)
          DiagPerCol(2*col-1) = 2*dia-1
          DiagPerCol(2*col  ) = 2*dia
          ColPerDiag(2*dia-1) = 2*col-1
          ColPerDiag(2*dia  ) = 2*col
       END DO
    END DO
    ALLOCATE (ExtDiagPerCol(2*mnExtMax))
    ALLOCATE (ExtColPerDiag(2*mnExtMax))
    col = 0
    DO m = 1, mMax
       DO n = m, nExtMax
          col = col+1
          dia = mnExtMap(m,n)
          ExtDiagPerCol(2*col-1) = 2*dia-1
          ExtDiagPerCol(2*col  ) = 2*dia
          ExtColPerDiag(2*dia-1) = 2*col-1
          ExtColPerDiag(2*dia  ) = 2*col
       END DO
    END DO
  END SUBROUTINE InitTransDiagCol




  SUBROUTINE TransDiagCol2D(qin, qout, todiag)
    REAL,    INTENT(IN ) :: qin(:,:)
    REAL,    INTENT(OUT) :: qout(:,:)
    LOGICAL, INTENT(IN ) :: todiag
    LOGICAL :: extended
    INTEGER :: k, d1in, d2in, d1out, d2out
    CHARACTER(LEN=*), PARAMETER :: h="**(TransDiagCol2D)**"

    d1in  = SIZE(qin,1);  d2in  = SIZE(qin,2)
    d1out = SIZE(qout,1); d2out = SIZE(qout,2)
    

    IF (d1in /= d1out) THEN
       WRITE(*,"(a,' arrays differ on dim 1=',2i10)") h, d1in, d1out
       STOP h
    ELSE IF (d2in /= d2out) THEN
       WRITE(*,"(a,' arrays differ on dim 2=',2i10)") h, d2in, d2out
       STOP h
    ELSE IF (d1in == 2*mnMax) THEN
       extended = .FALSE.
    ELSE IF (d1in == 2*mnExtMax) THEN
       extended = .TRUE.
    ELSE
       WRITE(*,"(a,' unknown first dimension of input array=',i10)") h, d1in
       STOP h
    END IF

    IF (extended) THEN
       IF (todiag) THEN
          DO k = 1, d2in
             qout(:,k) = qin(ExtColPerDiag(:),k)
          END DO
       ELSE
          DO k = 1, d2in
             qout(:,k) = qin(ExtDiagPerCol(:),k)
          END DO
       END IF
    ELSE
       IF (todiag) THEN
          DO k = 1, d2in
             qout(:,k) = qin(ColPerDiag(:),k)
          END DO
       ELSE
          DO k = 1, d2in
             qout(:,k) = qin(DiagPerCol(:),k)
          END DO
       END IF
    END IF
  END SUBROUTINE TransDiagCol2D



  SUBROUTINE TransDiagCol1D(qin, qout, todiag)
    REAL,    INTENT(IN ) :: qin(:)
    REAL,    INTENT(OUT) :: qout(:)
    LOGICAL, INTENT(IN ) :: todiag
    LOGICAL :: extended
    INTEGER :: d1in, d1out
    CHARACTER(LEN=*), PARAMETER :: h="**(TransDiagCol1D)**"

    d1in  = SIZE(qin,1)
    d1out = SIZE(qout,1)
    

    IF (d1in /= d1out) THEN
       WRITE(*,"(a,' arrays differ on dim 1=',2i10)") h, d1in, d1out
       STOP h
    ELSE IF (d1in == 2*mnMax) THEN
       extended = .FALSE.
    ELSE IF (d1in == 2*mnExtMax) THEN
       extended = .TRUE.
    ELSE
       WRITE(*,"(a,' unknown first dimension of input array=',i10)") h, d1in
       STOP h
    END IF

    IF (extended) THEN
       IF (todiag) THEN
          qout(:) = qin(ExtColPerDiag(:))
       ELSE
          qout(:) = qin(ExtDiagPerCol(:))
       END IF
    ELSE
       IF (todiag) THEN
          qout(:) = qin(ColPerDiag(:))
       ELSE
          qout(:) = qin(DiagPerCol(:))
       END IF
    END IF
  END SUBROUTINE TransDiagCol1D





  SUBROUTINE InitTimeStamp(dateinit_s,idate) 
    CHARACTER(len=10), INTENT(out) ::  dateinit_s
    INTEGER,           INTENT(in ) :: idate(4) 
    
    !local variables

    INTEGER hhi, mmi, ddi, yyyyi 
    
    yyyyi = idate(4) 
    mmi   = idate(2) 
    ddi   = idate(3) 
    hhi   = idate(1) 
    
    dateinit_s='          '
    WRITE(dateinit_s,'(i4.4,3i2.2)') yyyyi, mmi, ddi, hhi
    
    ! computes the julian day of this calendar date 
    
    juliandayinitintegration = julday(mmi, ddi, yyyyi) 
  END SUBROUTINE InitTimeStamp





  
  SUBROUTINE TimeStamp(datenow_s, idatec, jdt, dt)
    CHARACTER(len=10), INTENT(out) :: datenow_s
    INTEGER,           INTENT(out) :: idatec(4)  
    INTEGER,           INTENT(in)  :: jdt 
    REAL,              INTENT(in)  :: dt

    INTEGER  :: hhc, mmc, ddc, yyyyc, juliandaynow    
    
    juliandaynow = juliandayinitintegration + (INT(dt)*jdt)/(24*3600)
    CALL caldat(juliandaynow, mmc, ddc, yyyyc) 
    hhc = MOD(INT(dt)*jdt/3600,24) 
    datenow_s='          '
    WRITE(datenow_s,'(i4.4, 3i2.2)' ) yyyyc, mmc, ddc, hhc
    idatec = (/hhc,mmc,ddc,yyyyc /) 
  END SUBROUTINE TimeStamp
  
  



  SUBROUTINE Caldat(julian,mm,id,iyyy)
    ! input:  julian day 
    ! output: mm = mes ; id = dia, iyyy = ano 
    INTEGER, PARAMETER :: igreg=2299161
    INTEGER, INTENT(IN ) :: julian
    INTEGER, INTENT(OUT) :: mm
    INTEGER, INTENT(OUT) :: id
    INTEGER, INTENT(OUT) :: iyyy
    INTEGER :: jalpha
    INTEGER :: ja, jb, jc, jd, je
    IF(julian.GE.igreg)THEN
       jalpha=INT(((julian-1867216)-0.25)/36524.25)
       ja=julian+1+jalpha-INT(0.25*jalpha)
    ELSE
       ja=julian
    ENDIF
    jb=ja+1524
    jc=INT(6680.+((jb-2439870)-122.1)/365.25)
    jd=365*jc+INT(0.25*jc)
    je=INT((jb-jd)/30.6001)
    id=jb-jd-INT(30.6001*je)
    mm=je-1
    IF(mm.GT.12)mm=mm-12
    iyyy=jc-4715
    IF(mm.GT.2)iyyy=iyyy-1
    IF(iyyy.LE.0)iyyy=iyyy-1
  END SUBROUTINE caldat


  INTEGER FUNCTION julday(mm,id,iyyy)
    ! input:  mm = mes;  id = dia;  iyyy = ano 
    ! output: julian day dessa data. 
    INTEGER, INTENT(IN) :: mm
    INTEGER, INTENT(IN) :: id
    INTEGER, INTENT(INOUT) :: iyyy
    INTEGER, PARAMETER  :: igreg=15+31*(10+12*1582)
    INTEGER :: jy, jm, ja
    IF (iyyy.EQ.0) THEN
       PRINT *, 'there is no year zero.'
       STOP "ERROR AT julday"
    END IF
    IF (iyyy.LT.0) iyyy=iyyy+1
    IF (mm.GT.2) THEN
       jy=iyyy
       jm=mm+1
    ELSE
       jy=iyyy-1
       jm=mm+13
    ENDIF
    julday=INT(365.25*jy)+INT(30.6001*jm)+id+1720995
    IF (id+31*(mm+12*iyyy).GE.igreg) THEN
       ja=INT(0.01*jy)
       julday=julday+2-ja+INT(0.25*ja)
    ENDIF
  END FUNCTION julday
 !
 ! maps (ib,jb) into (i,j)
 !
 SUBROUTINE IBJBtoIJ_R(var_in,var_out)
    REAL   , INTENT(IN   ) :: var_in (:,:)
    REAL   , INTENT(OUT  ) :: var_out(:,:)
    INTEGER                :: i
    INTEGER                :: j     
    INTEGER                :: ib
    INTEGER                :: jb 
!$OMP DO PRIVATE(i,ib,jb)
    DO j = 1, jMax
       DO i = 1, iMax
       ib = ibPerIJ(i,j)
   jb = jbPerIJ(i,j)
   var_out(i,j)=var_in(ib,jb)
       END DO
    END DO  
 END SUBROUTINE IBJBtoIJ_R      
 !
 ! maps (i,j) into (ib,jb)
 !   
 SUBROUTINE IJtoIBJB_R(var_in,var_out)
    REAL   , INTENT(IN  ) :: var_in (:,:)
    REAL   , INTENT(OUT ) :: var_out(:,:)
    INTEGER               :: i
    INTEGER               :: j     
    INTEGER               :: ib
    INTEGER               :: jb    
!$OMP DO PRIVATE(i,ib,j)
    DO jb = 1, jbMax
      DO ib = 1, ibMaxPerJB(jb)
       i = iPerIJB(ib,jb)
   j = jPerIJB(ib,jb)
   var_out(ib,jb)=var_in(i,j)
       END DO
    END DO  
 END SUBROUTINE IJtoIBJB_R
   
 SUBROUTINE IBJBtoIJ_I(var_in,var_out)
    INTEGER, INTENT(IN   ) :: var_in (:,:)
    INTEGER, INTENT(OUT  ) :: var_out(:,:)
    INTEGER                :: i
    INTEGER                :: j     
    INTEGER                :: ib
    INTEGER                :: jb 
!$OMP DO PRIVATE(i,ib,jb)
    DO j = 1, jMax
       DO i = 1, iMax
       ib = ibPerIJ(i,j)
   jb = jbPerIJ(i,j)
   var_out(i,j)=var_in(ib,jb)
       END DO
    END DO  
 END SUBROUTINE IBJBtoIJ_I      
 !
 ! maps (i,j) into (ib,jb)
 !   
 SUBROUTINE IJtoIBJB_I(var_in,var_out)
    INTEGER, INTENT(IN  ) :: var_in (:,:)
    INTEGER, INTENT(OUT ) :: var_out(:,:)
    INTEGER               :: i
    INTEGER               :: j     
    INTEGER               :: ib
    INTEGER               :: jb    
!$OMP DO PRIVATE(i,ib,j)
    DO jb = 1, jbMax
      DO ib = 1, ibMaxPerJB(jb)
       i = iPerIJB(ib,jb)
   j = jPerIJB(ib,jb)
   var_out(ib,jb)=var_in(i,j)
       END DO
    END DO  
 END SUBROUTINE IJtoIBJB_I   



END MODULE Utils
