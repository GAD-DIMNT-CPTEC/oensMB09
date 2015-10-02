
PROGRAM BIAS2
   IMPLICIT NONE
   
   INTEGER, PARAMETER         :: x=384, y=192, Z=9, t=60
                                
   REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: varVies_pl
   REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: varVies_sl
   
   REAL(KIND=8), ALLOCATABLE, DIMENSION (:,:,:,:) :: varIn2_pl, varIn3_pl
   REAL(KIND=8), ALLOCATABLE, DIMENSION (:,:,:,:) :: varIn2_sl, varIn3_sl
   REAL(KIND=8), DIMENSION (x*y)   :: varTmp

   INTEGER                    :: i, j, k, m
   
   CHARACTER (LEN=128)        :: input1, input2, input3, input4, vf
   
   ! DEFINICAO DAS VARIAVEIS
   INTEGER                    :: varNumber (10)= (/ 51,  33,  34,   7,  11, 130, 131, 128,   2, 135/)  
   INTEGER                    :: varLvlType(10)= (/100, 100, 100, 100, 100, 105, 105, 105, 102,   1/)
   INTEGER                    :: lvlDesc(9)=     (/1000,925,850,700,500,300,250,200,50/)
   
   CALL GETARG(1,input1)
   CALL GETARG(2,input2)
   CALL GETARG(3,input3)
   CALL GETARG(4,input4)
   
   ALLOCATE (varIn2_pl(x*y,z,5,t+1) &
            ,varIn3_pl(x*y,z,5,t+1) &
            ,varVies_pl(x*y,z,5,t)  &
            ,varIn2_sl(x*y,1,5,t+1) &
            ,varIn3_sl(x*y,1,5,t+1) &
            ,varVies_sl(x*y,1,5,t))
   
   OPEN (10, FILE=TRIM(input1), FORM="FORMATTED")
   j=1
   DO
      m=1
      READ (10, FMT='(A128)',END=49) vf
      print '(I3,2X,A128)', j,TRIM(vf)
      OPEN (11, FILE=TRIM(vf), STATUS='OLD', FORM='UNFORMATTED', ACCESS='SEQUENTIAL')
      !OPEN (11, FILE=TRIM(vf), STATUS='OLD', FORM='UNFORMATTED', ACCESS='DIRECT', RECl=x*y*4)
      DO i=1, 5
         DO k=1, z
            READ (11) varVies_pl(:,k,i,j)
            m=m+1
         END DO
      END DO

      DO i=1, 5
         DO k=1, 1
            READ (11) varVies_sl(:,k,i,j)
            m=m+1
         END DO
      END DO
      
      CLOSE (11)
      j=j+1
   END DO
   
49 CLOSE(10) ! FINAL DO LOOP DE LEITURA
   print *
   print *, "Inicio Lista 2"
   
! Leitura dos dados de entrada, Analise:
   OPEN (10, FILE=TRIM(input2), FORM="FORMATTED")
   
   i=1
   j=t+1
   DO 
      m=1
      READ (10, FMT='(A128)', END=77) vf
      print '(I3,2X,A128)', j,TRIM(vf)
      
      DO i=1, 5
         DO k=1, z
            CALL getGrib(TRIM(vf), varNumber(m), 100, lvlDesc(k), varIn2_pl(:,k,i,j))
         END DO
         m=m+1
      END DO

      DO i=1, 5
         DO k=1, 1
            CALL getGrib(TRIM(vf), varNumber(m), 0, 0, varIn2_sl(:,k,i,j))
         END DO
         m=m+1
      END DO
      j=j+1
      if (j.gt.t) j=1
    END DO
       
77 CLOSE(10)
    DO i=1, t
      varIn2_sl(:,:,:,i)=(varIn2_sl(:,:,:,i)-varIn2_sl(:,:,:,t+1))/60.0
      varIn2_pl(:,:,:,i)=(varIn2_pl(:,:,:,i)-varIn2_pl(:,:,:,t+1))/60.0
   ENDDO

   print *
   print *, "Inicio Lista 3"
   
! Leitura dos dados de entrada, Analise:
   OPEN (10, FILE=TRIM(input3), FORM="FORMATTED")
   
   i=1
   j=t+1
   DO 
      m=1
      READ (10, FMT='(A128)', END=126) vf
      print '(I3,2X,A128)', j,TRIM(vf)
      
      DO i=1, 5
         DO k=1, z
            CALL getGrib(TRIM(vf), varNumber(m), 100, lvlDesc(k), varIn3_pl(:,k,i,j))
         END DO
         m=m+1
      END DO

      DO i=1, 5
         DO k=1, 1
            CALL getGrib(TRIM(vf), varNumber(m), 0, 0, varIn3_sl(:,k,i,j))
         END DO
         m=m+1
      END DO
      j=j+1
      if (j.gt.t) j=1
    END DO
       
126 CLOSE(10)
    DO i=1, t
      varIn3_sl(:,:,:,i)=(varIn3_sl(:,:,:,i)-varIn3_sl(:,:,:,t+1))/60.0
      varIn3_pl(:,:,:,i)=(varIn3_pl(:,:,:,i)-varIn3_pl(:,:,:,t+1))/60.0
   ENDDO

   print *, "Calculando novo Vies..."
   
   varVies_pl=varVies_pl-varIn3_pl(:,:,:,1:t)+varIn2_pl(:,:,:,1:t)
   varVies_sl=varVies_sl-varIn3_sl(:,:,:,1:t)+varIn2_sl(:,:,:,1:t)
   
   print *, "Escrevendo novo Vies..."
   OPEN (10, FILE=TRIM(input4), FORM="FORMATTED")
   j=1
   DO
      m=1
      READ (10, FMT='(A128)',END=163) vf
      print '(I3,2X,A128)', j,TRIM(vf)
      OPEN (11, FILE=TRIM(vf), STATUS='UNKNOWN', FORM='UNFORMATTED', ACCESS='DIRECT', RECL=x*y*4)
      
      DO i=1, 5
         DO k=1, z
            WRITE (11,rec=m) varVies_pl(:,k,i,j)
            m=m+1
         END DO
      END DO

      k=1
      DO i=1, 5
         WRITE (11,rec=m) varVies_sl(:,k,i,j)
         m=m+1
      END DO
      
      CLOSE (11)
      j=j+1
   END DO
   
163 CLOSE(10) ! FINAL DO LOOP DE LEITURA
   
END PROGRAM BIAS2



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

