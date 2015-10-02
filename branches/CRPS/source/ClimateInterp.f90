  MODULE ClimateInterp

   IMPLICIT NONE
   PRIVATE
   PUBLIC :: climint,stdfldint

   CONTAINS

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   ! Subroutine climint
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE CLIMINT(cdate,dirclm,psnmclim,zclim500,tclim850,TimeInterpClim)
  
  IMPLICIT NONE
  
  !
  ! This program makes the interpoJMAXon of the climatological precipitation. 
  !

  CHARACTER(LEN=*)  :: dirclm
  CHARACTER(LEN=10) :: cdate

  ! IMAX : MAXIMUM NUMBER OF LONGITUDE POINTS  
  ! JMAX : MINIMUM NUMBER OF LATITUDE  POINTS  
 
  INTEGER, PARAMETER :: IMAX=240,JMAX=121
  INTEGER, PARAMETER :: NM=3                     
  INTEGER, PARAMETER :: NumberOfBins=10                     
  REAL, PARAMETER :: UNDEF=-9.99E+08                     
 
  INTEGER :: i,j,l,mon,mf,mn,nd
  INTEGER :: DATE, NX, NY, NXY, N, NB, RFA
  INTEGER :: iyear,imonth,iday,iutc,rest,lastd,recread
  INTEGER, DIMENSION(nm) :: recdes
  INTEGER, DIMENSION(12) :: monl,monlb

  REAL :: yday,add,f1,f2
  REAL,DIMENSION(IMAX,JMAX):: bufa,bufb,bufi,bufm
  REAL,DIMENSION(IMAX,JMAX),INTENT(OUT):: zclim500,tclim850,psnmclim
  REAL,DIMENSION(IMAX,JMAX,NumberOfBins+1),INTENT(OUT) :: TimeInterpClim
  REAL,DIMENSION(101) :: CLIP
  LOGICAL :: CINDEX(IMAX,JMAX)


  CHARACTER(LEN=200) :: fnmd
  CHARACTER(LEN=1000), DIMENSION(12) :: fnmi

  DATA recdes  /6,54,90/

  DATA monl  /31,28,31,30,31,30,31,31,30,31,30,31/
  DATA monlb /31,29,31,30,31,30,31,31,30,31,30,31/

  DATA fnmi  /'LongTermMean.ERAInterim.TEMP850.Monthly.JAN.1989-2010.grads', &
              'LongTermMean.ERAInterim.TEMP850.Monthly.FEB.1989-2010.grads', &
              'LongTermMean.ERAInterim.TEMP850.Monthly.MAR.1989-2010.grads', &
              'LongTermMean.ERAInterim.TEMP850.Monthly.APR.1989-2010.grads', &
              'LongTermMean.ERAInterim.TEMP850.Monthly.MAY.1989-2010.grads', &
              'LongTermMean.ERAInterim.TEMP850.Monthly.JUN.1989-2010.grads', &
              'LongTermMean.ERAInterim.TEMP850.Monthly.JUL.1989-2010.grads', &
              'LongTermMean.ERAInterim.TEMP850.Monthly.AUG.1989-2010.grads', &
              'LongTermMean.ERAInterim.TEMP850.Monthly.SEP.1989-2010.grads', &
              'LongTermMean.ERAInterim.TEMP850.Monthly.OCT.1989-2010.grads', &
              'LongTermMean.ERAInterim.TEMP850.Monthly.NOV.1989-2010.grads', &
              'LongTermMean.ERAInterim.TEMP850.Monthly.DEC.1989-2010.grads'/

    !
    ! Get arguments from script
    !

    WRITE(*,*)'cdate: ',cdate

    READ(cdate,'(I10)') date
    READ(cdate(1: 4),'(I4)') iyear
    READ(cdate(5: 6),'(I2)') imonth
    READ(cdate(7: 8),'(I2)') iday
    READ(cdate(9:10),'(I2)') iutc

  
    !
    ! Make the interpoJMAXon of the climatology and evaluate the mean on the period  
    ! evaluate the weigths "f1" e "f2" for linear interpoJMAXon
    !
    mon=imonth
    yday=FLOAT(iday)+FLOAT(iutc)/24.0
    mf=mon-1
    IF(yday > 1.0+FLOAT(monl(mon))/2.0)mf=mon
    mn=mf+1
    IF(mf < 1)mf=12
    IF(mn > 12)mn=1
    add=FLOAT(monl(mf))/2.0-1.0
    IF(mf == mon)add=-add-2.0
    f1=2.0*(yday+add)/FLOAT(monl(mf)+monl(mn))
    f2=1.0-f1

    WRITE(*,*)' '
    WRITE(*,*)'fnmi(',mf,'): ',fnmi(mf)
    WRITE(*,*)'fnmi(',mn,'): ',fnmi(mn)
    WRITE(*,*)' '
    WRITE(*,*)'f1: ',f1
    WRITE(*,*)'f2: ',f2

    DO NB = 1,NumberOfBins+1 
       TimeInterpClim(:,:,NB)=UNDEF
    ENDDO

    ! ----
    ! READS THE MONTHLY CLIMATOLOGICAL FIELDS
    ! CLIMATOLOGICAL FIELDS ARE ORIENTED SOUTH TO NORTH
    ! ----
    OPEN(50, FILE=TRIM(dirclm)//fnmi(mf), STATUS='OLD', FORM='UNFORMATTED', & 
             ACTION='READ', ACCESS='DIRECT', RECL=IMAX*JMAX*4)
    OPEN(51, FILE=TRIM(dirclm)//fnmi(mn), STATUS='OLD', FORM='UNFORMATTED', & 
             ACTION='READ', ACCESS='DIRECT', RECL=IMAX*JMAX*4)

    READ(50,REC=1) bufa
    READ(51,REC=1) bufb
    CLOSE(50)
    CLOSE(51)

    DO J=1,JMAX
    DO I=1,IMAX
       !TCLIM850(I,JMAX+1-J)=f2*bufa(I,J)+f1*bufb(I,J)
       TCLIM850(I,J)=f2*bufa(I,J)+f1*bufb(I,J)
    END DO
    END DO

    NXY = 0
    DO J=1,JMAX
    DO I=1,IMAX
       !
       ! DO MULTIPLE COPIES OF THE TIME-INTERPOLATED CLIMATOLOGY
       !
       DO n = 1, 101
          CLIP(N) = TCLIM850(I,J)
       ENDDO
       NXY = NXY + 1
       RFA = 100/NumberOfBins
       TimeInterpClim(I,J,1)=clip(1)
       DO NB = 2,NumberOfBins 
          !PRINT*,"NB-1:",NB-1," (NB-1)*RFA:",(NB-1)*RFA
          TimeInterpClim(I,J,NB)=clip((NB-1)*RFA)
       ENDDO
       TimeInterpClim(I,J,NumberOfBins+1)=clip(101)
    END DO
    END DO

    OPEN(52,FILE='../dataout/TimeInterpClim.grads',FORM='UNFORMATTED', &
              ACTION='WRITE',ACCESS='SEQUENTIAL',STATUS='UNKNOWN')
    DO NB=1,NumberOfBins+1
       WRITE(52) ((TimeInterpClim(I,J,NB),I=1,IMAX),J=1,JMAX)
    ENDDO
    CLOSE(52)

  END SUBROUTINE climint



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Subroutine stdfldint
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE stdfldint(cdate,dirclm,psnmclim,zclim500,tclim850)
  
  IMPLICIT NONE
  
!
! This program makes the interpolation of the climatological precipitation. 
!

  CHARACTER(LEN=*)  :: dirclm
  CHARACTER(LEN=10) :: cdate


!loni : nb. of longitude points  
!lati : nb. of latitude points  
 
  INTEGER, PARAMETER :: loni=144,lati=73
  INTEGER, PARAMETER :: nm=3                     
 
  INTEGER :: i,j,n,l,mon,mf,mn,nd
  INTEGER :: date
  INTEGER :: iyear,imonth,iday,iutc,rest,lastd,recread
  INTEGER, DIMENSION(nm) :: recdes
  INTEGER, DIMENSION(12) :: monl,monlb

  REAL :: yday,add,f1,f2
  REAL, DIMENSION(loni,lati):: bufa,bufb,bufi,bufm,psnmclim,zclim500,tclim850


  CHARACTER(LEN=200) :: fnmd
  CHARACTER(LEN=33), DIMENSION(12) :: fnmi

  DATA recdes  /2,4,6/

  DATA monl  /31,28,31,30,31,30,31,31,30,31,30,31/
  DATA monlb /31,29,31,30,31,30,31,31,30,31,30,31/

  DATA fnmi  /'ncep_cdc_amipII_1979to2006_01.bin','ncep_cdc_amipII_1979to2006_02.bin',  &
              'ncep_cdc_amipII_1979to2006_03.bin','ncep_cdc_amipII_1979to2006_04.bin',  &
              'ncep_cdc_amipII_1979to2006_05.bin','ncep_cdc_amipII_1979to2006_06.bin',  &
              'ncep_cdc_amipII_1979to2006_07.bin','ncep_cdc_amipII_1979to2006_08.bin',  &
              'ncep_cdc_amipII_1979to2006_09.bin','ncep_cdc_amipII_1979to2006_10.bin',  &
              'ncep_cdc_amipII_1979to2006_11.bin','ncep_cdc_amipII_1979to2006_12.bin'/

!
! Get arguments from script
!

  WRITE(*,*)'cdate: ',cdate

  READ(cdate,'(I10)') date
  READ(cdate(1: 4),'(I4)') iyear
  READ(cdate(5: 6),'(I2)') imonth
  READ(cdate(7: 8),'(I2)') iday
  READ(cdate(9:10),'(I2)') iutc
  
!
! Make the interpolation of the climatology and evaluate the mean on the period  
!

    !evaluate the weighs "f1" e "f2" for linear interpolation
    
    mon=imonth
    yday=FLOAT(iday)+FLOAT(iutc)/24.0
    mf=mon-1
    IF(yday > 1.0+FLOAT(monl(mon))/2.0)mf=mon
    mn=mf+1
    IF(mf < 1)mf=12
    IF(mn > 12)mn=1
    add=FLOAT(monl(mf))/2.0-1.0
    IF(mf == mon)add=-add-2.0
    f1=2.0*(yday+add)/FLOAT(monl(mf)+monl(mn))
    f2=1.0-f1

    WRITE(*,*)' '
    WRITE(*,*)'fnmi(',mf,'): ',fnmi(mf)
    WRITE(*,*)'fnmi(',mn,'): ',fnmi(mn)
    WRITE(*,*)' '
    WRITE(*,*)'f1: ',f1
    WRITE(*,*)'f2: ',f2

    !input climatological files
    
    WRITE(*,*)'OPEN50: ',TRIM(dirclm)//fnmi(mf)
    WRITE(*,*)'OPEN51: ',TRIM(dirclm)//fnmi(mn)
    
    OPEN(50, FILE=TRIM(dirclm)//fnmi(mf), STATUS='OLD', FORM='UNFORMATTED', & 
             ACTION='READ', ACCESS='DIRECT', RECL=loni*lati)
    OPEN(51, FILE=TRIM(dirclm)//fnmi(mn), STATUS='OLD', FORM='UNFORMATTED', & 
             ACTION='READ', ACCESS='DIRECT', RECL=loni*lati)

    DO n=1,nm

       recread=recdes(n)

       READ(50,REC=recread)((bufa(i,j),i=1,loni),j=1,lati)
       READ(51,REC=recread)((bufb(i,j),i=1,loni),j=1,lati)

       DO j=1,lati
         DO i=1,loni
           bufi(i,j)=f2*bufa(i,j)+f1*bufb(i,j)
         END DO
       END DO

       IF (recread .EQ. 2) psnmclim(1:loni,lati:1:-1)=bufi(1:loni,1:lati)
       IF (recread .EQ. 4) zclim500(1:loni,lati:1:-1)=bufi(1:loni,1:lati)
       IF (recread .EQ. 6) tclim850(1:loni,lati:1:-1)=bufi(1:loni,1:lati)

    END DO

    CLOSE(50)
    CLOSE(51)

  END SUBROUTINE stdfldint


END MODULE ClimateInterp

