PROGRAM Gira
 IMPLICIT NONE
 INTEGER, PARAMETER :: Idim=360
 INTEGER, PARAMETER :: Jdim=180
 INTEGER, PARAMETER :: nMonth=1
 INTEGER :: LRec,i,j,k,ios
 REAL(KIND=4), ALLOCATABLE :: NDVIC(:,:,:)
 REAL(KIND=4), ALLOCATABLE :: NDVIC2(:,:,:)
 ALLOCATE (NDVIC(Idim,Jdim,nMonth))
 ALLOCATE (NDVIC2(Idim,Jdim,nMonth))
 INQUIRE (IOLENGTH=LRec) NDVIC(:,:,:)
 OPEN(1,FILE='ibismsk.form',FORM='unformatted',&
       ACCESS='DIRECT',recl=LRec,ACTION='READ',STATUS='OLD',&
       IOSTAT=ios)

   IF (ios /= 0) THEN
      WRITE (UNIT=*, FMT='(3A,I4)') &
            ' ** (Error) ** Open file ', &
              'ndviclm.form2', &
            ' returned IOStat = ', ios
      STOP  ' ** (Error) **'
   END IF
   READ  (UNIT=1,rec=1) NDVIC

   DO k=1,nMonth
!      DO j=1,Jdim
!         DO i=1,Idim
!	    !Lon=(REAL(i,KIND=4) - 1.0 )*1.0
!	    IF(i <= 180) THEN
!	       NDVIC2(i,j,k)=NDVIC(180+i,j,k)
!	    ELSE IF(i > 180) THEN
!	       NDVIC2(i,j,k)=NDVIC(i-180,j,k)
!	    END IF   
!	 END DO
!      END DO
      DO j=1,Jdim
         DO i=1,Idim
	    IF(NDVIC(i,j,k) == 13.0) NDVIC(i,j,k)=15.0
	 END DO
      END DO
   END DO
   INQUIRE (IOLENGTH=LRec) NDVIC(:,:,:)
   OPEN(2,FILE='ibismsk.form2',FORM='unformatted',&
       ACCESS='DIRECT',recl=LRec,ACTION='WRITE',STATUS='UNKNOWN',&
       IOSTAT=ios)
   IF (ios /= 0) THEN
      WRITE (UNIT=*, FMT='(3A,I4)') &
            ' ** (Error) ** Open file ', &
              'ndviclm.form2', &
            ' returned IOStat = ', ios
      STOP  ' ** (Error) **'
   END IF
   WRITE  (UNIT=2,rec=1) NDVIC
 
END PROGRAM Gira
