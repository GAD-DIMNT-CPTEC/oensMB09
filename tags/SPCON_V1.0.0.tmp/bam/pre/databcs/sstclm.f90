PROGRAM sstclm
 IMPLICIT NONE
 INTEGER, PARAMETER :: iMax=360
 INTEGER, PARAMETER :: jMax=180
 REAL(KIND=4) :: sst(iMax,jMax)
 REAL(KIND=4) :: SSTIn(iMax,jMax)
 INTEGER :: Header(8)
 INTEGER :: ios
 INTEGER :: m,i,j
 INTEGER :: reclen
 INTEGER, PARAMETER :: lmes(1:12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)  
 INQUIRE(IOLENGTH=reclen)sst
 OPEN(2,FILE='ersst.bin',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=reclen,&
 &     ACTION='READ',STATUS='OLD',IOSTAT=ios)
 
 OPEN (UNIT=1, FILE='ersst.form', &
       FORM='FORMATTED', ACCESS='SEQUENTIAL', &
       ACTION='WRITE', STATUS='UNKNOWN', IOSTAT=ios)
 IF (ios /= 0) THEN
    WRITE (UNIT=*, FMT='(4A,I4)')' ** (Error) ** Open file ', &
 	   'ersst.form', &
 	  ' returned IOStat = ', ios
    STOP  ' ** (Error) **'
 END IF
 ! Loop Through Months
 Header(1) = 99    
 Header(2) = 1    
 Header(3) = 1   
 Header(4) = 99    
 Header(5) = 1   
 Header(6) = 31   
 Header(7) = 31    
 Header(8) = 0 
 DO m=1,12
    Header(2)=m
    Header(5)=m
    Header(6)=lmes(m)
    Header(7)=lmes(m)
    READ(2,rec=m)sst
    DO j=1,jMax
       DO i=1,iMax
          SSTIn(i,jMax+1-j)=SST(i,j)*100.0
          IF(i <= 180) THEN
             !SSTIn(i,j)=SST(180+i,j)*100.0
          ELSE IF(i > 180) THEN
             !SSTIn(i,j)=SST(i-180,j)*100.0
          END IF   
      END DO
    END DO

    WRITE (UNIT=1, FMT='(8I5)') Header
    WRITE (UNIT=*, FMT='(/,1X,9I5,/)') m, Header
    WRITE (UNIT=1, FMT='(16I5)') INT(SSTIn)
 END DO
 
END PROGRAM sstclm
