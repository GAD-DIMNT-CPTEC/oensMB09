PROGRAM BIAS2
   IMPLICIT NONE
   
   INTEGER, PARAMETER         :: x=384, y=192, Z=9, t=60
                                
   REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: varVies_pl
   REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: varVies_sl

   REAL(KIND=8), ALLOCATABLE, DIMENSION (:,:,:,:,:) :: varIn1_pl
   REAL(KIND=8), ALLOCATABLE, DIMENSION (:,:,:,:,:) :: varIn1_sl

   REAL(KIND=8), DIMENSION (x*y)   :: varTmp

   INTEGER                    :: i, j, k, m
   REAL                       :: file_count
   
   CHARACTER(LEN=3)           :: chhAnl
   CHARACTER(LEN=4)           :: chhFct
   INTEGER                    :: hhAnl, hhFct
   
   CHARACTER (LEN=128)        :: input1, input2, input3, input4, vf
   
   CALL GETARG(1,input1)
   CALL GETARG(2,input2)
   CALL GETARG(3,input3)
   CALL GETARG(4,input4)
   
   ALLOCATE(varIn1_pl(x*y,z,5,60,4), varIn1_sl(x*y,1,5,60,4))
   
   viesa_bagarai (input1, varIn1_pl, varIn1_sl)
   
      stop
      
END PROGRAM BIAS2

SUBROUTINE viesa_bagarai (input1, varIn1_pl, varIn1_sl)
   IMPLICIT NONE
   
   INTEGER, PARAMETER         :: x=384, y=192, Z=9, t=60
   INTEGER                    :: i, j, k, m
   REAL                       :: file_count
   
   ! DEFINICAO DAS VARIAVEIS
   INTEGER                    :: varNumber (10)= (/ 51,  33,  34,   7,  11, 130, 131, 128,   2, 135/)  
   INTEGER                    :: varLvlType(10)= (/100, 100, 100, 100, 100, 105, 105, 105, 102,   1/)
   INTEGER                    :: lvlDesc(9)=     (/1000,925,850,700,500,300,250,200,50/)

   REAL(KIND=8), ALLOCATABLE, DIMENSION (:,:,:,:) :: varAnl_pl 
   REAL(KIND=8), ALLOCATABLE, DIMENSION (:,:,:,:) :: varAnl_sl 

   REAL(KIND=8), ALLOCATABLE, DIMENSION (:,:,:,:,:) :: varIn1_pl
   REAL(KIND=8), ALLOCATABLE, DIMENSION (:,:,:,:,:) :: varIn1_sl
   
   INTEGER                    :: hhAnl, hhFct
   CHARACTER (LEN=128)        :: input1, vf

   ALLOCATE(varAnl_pl(x*y,z,5,1)   , varAnl_sl(x*y,1,5,1))
   ALLOCATE(varIn1_pl(x*y,z,5,60,4), varIn1_sl(x*y,1,5,60,4))
   
   OPEN (10, FILE=TRIM(input1), FORM='FORMATTED', STATUS='OLD')
   READ (10,'(A128)') vf
   READ (vf(1:2),'(I2)') hhAnl
   READ (vf(4:6),'(I3)') hhFct
   READ (vf(8:128),'(A128)') vf
   print '(A128)', vf
   
   m=1
   DO i=1, 5
      DO k=1, z
         CALL getGrib(TRIM(vf), varNumber(m), 100, lvlDesc(k), varAnl_pl(:,k,i,1))
      END DO
      m=m+1
   END DO
   DO i=1, 5
      DO k=1, 1
         CALL getGrib(TRIM(vf), varNumber(m), 0, 0, varAnl_sl(:,k,i,1))
      END DO
      m=m+1
   END DO
   
   j=1
   file_count=0
   DO 
      READ(10,'(A128)', END=10) vf
      READ (vf(1:2),'(I2)') hhAnl
      READ (vf(4:6),'(I3)') hhFct
      READ (vf(8:128),'(A128)') vf
      !print '(I2,X,I3,X,A128)', hhAnl, hhFct, vf
      print *, hhFct/6,hhFct, hhAnl/6+1,hhAnl, TRIM(vf)
      file_count=file_count+1
      m=1
      DO i=1, 5
         DO k=1, z
            CALL getGrib(TRIM(vf), varNumber(m), 100, lvlDesc(k), varIn1_pl(:,k,i,hhFct/6,hhAnl/6+1))
         END DO
         m=m+1
      END DO
      DO i=1, 5
         CALL getGrib(TRIM(vf), varNumber(m), 0, 0, varIn1_sl(:,1,i,hhFct/6,hhAnl/6+1))
         m=m+1
      END DO
      
      j=j+1
   END DO
   DO i=1, 4
      DO j=1, 60
         varIn1_pl(:,:,:,j,i)=(varIn1_pl(:,:,:,j,i)-varAnl_pl(:,:,:,1))/file_count
         varIn1_sl(:,:,:,j,i)=(varIn1_sl(:,:,:,j,i)-varAnl_sl(:,:,:,1))/file_count
      END DO
   END DO

10 CLOSE (10)
END SUBROUTINE viesa_bagarai

SUBROUTINE getGrib (fileName, varNumber, varLvlType, lvlDesc, varOut)
   USE grib_api
   IMPLICIT NONE
   
   INTEGER, PARAMETER              :: x=384, y=192, Z=9, t=60
   
   INTEGER                         :: i, j, k
   INTEGER                         :: varNumber, varLvlType, lvlDesc
   
   !UTILIZADOS COM A ABERTURA DOS GRIBs
   INTEGER                         :: ifile, igrib, ierr
   CHARACTER(LEN=128)              :: fileName
   INTEGER                         :: indicatorOfParameter, indicatorOfTypeOfLevel, level, numberOfDataPoints
   REAL (KIND=8)                   :: varOut(x*y)


   CALL GRIB_OPEN_FILE(ifile, fileName(1:INDEX(fileName, "grb")+2), 'r')
   CALL GRIB_MULTI_SUPPORT_ON()
  
   k=1
   CALL GRIB_NEW_FROM_FILE(ifile,igrib,IERR)
   CALL GRIB_GET(igrib, 'numberOfDataPoints', numberOfDataPoints)
   
   GRIB: DO WHILE (IERR /= GRIB_END_OF_FILE)
      CALL GRIB_GET(igrib, 'indicatorOfParameter', indicatorOfParameter)
      IF ( indicatorOfParameter == varNumber) THEN
         IF ( varLvlType == 100 ) THEN
            CALL GRIB_GET(igrib, 'level', level)
            !print *, indicatorOfParameter, level
            IF ( level == lvlDesc) THEN
      	      CALL GRIB_GET(igrib, 'values', varOut(:))
               EXIT GRIB
            END IF                  	  
         ELSE
            !print *, indicatorOfParameter
            CALL GRIB_GET(igrib, 'values', varOut(:))
            EXIT GRIB
         END IF
      END IF
      CALL GRIB_RELEASE (igrib)
      CALL GRIB_NEW_FROM_FILE(ifile,igrib,IERR)
   END DO GRIB

   CALL GRIB_RELEASE (igrib)
   CALL GRIB_CLOSE_FILE(ifile)
!   PRINT '(A2,xA128)', ">>", TRIM(fileName)
!   PRINT '(A3,x,I3,x,A18)', ">>>", k, "Variaveis lidas..."
   
END SUBROUTINE getGrib

