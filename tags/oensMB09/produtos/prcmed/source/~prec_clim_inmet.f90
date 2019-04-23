  PROGRAM prec_clim_inmet
  
  IMPLICIT NONE
  
!
! 
! This program makes the interpolation of the climatological precipitation
! to the requested date, and evaluate the mean on the period, set up by 
! date2 and date3. The date1 (analysis date) is used to construct the name of
! outpu file. 
! Cedido por: MARCOS MENDONCA (marcos.mendonca@cptec.inpe.br)

!loni : nb. of longitude points  
!lati : nb. of latitude points  
!nsph : number of second in a hour
!iprec: number of the precipitation record in the climatological files
!iprec: number of the TIME MEAN ABSOLUTE TEMPERATURE in the climatological files
 

! #######  Declarando variaveis

  INTEGER, PARAMETER :: loni=176,lati=160,nsph=86400 
  INTEGER, PARAMETER :: iprec=1                      
 
  INTEGER :: i,j,n,l,mon,mf,mn,nd
  INTEGER :: date,date1,date2,date3
  INTEGER :: iyear,imonth,iday,iutc,rest,lastd
  INTEGER, DIMENSION(12) :: monl,monlb

  REAL :: yday,add,f1,f2
  REAL, DIMENSION(loni,lati):: bufa,bufb,bufi,bufm

  CHARACTER(LEN=10)   :: cdate,cdate1,cdate2,cdate3

  CHARACTER(LEN=200) :: fnmd
  CHARACTER(LEN=200) :: dirclm
  CHARACTER(LEN=200), DIMENSION(12) :: fnmi

  DATA monl  /31,28,31,30,31,30,31,31,30,31,30,31/
  DATA monlb /31,29,31,30,31,30,31,31,30,31,30,31/

  DATA fnmi  /'prec01.bin','prec02.bin', &
              'prec03.bin','prec04.bin', &
	      'prec05.bin','prec06.bin', &
              'prec07.bin','prec08.bin', &
	      'prec09.bin','prec10.bin', &
	      'prec11.bin','prec12.bin'/

!
! Get arguments from scrpit
!

  CALL GETARG(1,cdate1)
  CALL GETARG(2,cdate2)
  CALL GETARG(3,cdate3)
  CALL GETARG(4,dirclm)

  READ(cdate1,'(I10)') date1
  READ(cdate2,'(I10)') date2
  READ(cdate3,'(I10)') date3
  READ(cdate2(1: 4),'(I4)') iyear
  READ(cdate2(5: 6),'(I2)') imonth
  READ(cdate2(7: 8),'(I2)') iday
  READ(cdate2(9:10),'(I2)') iutc
  WRITE(*,'(A,I10.10)')'date2: ',date2
  WRITE(*,'(A,I10.10)')'date3: ',date3

!
! Directory where the climatology is localized 
!

  dirclm='../INMET/prec/'

!
! Name of the day climatology. Mean over the period   
!

  WRITE(fnmd,'(A,2I10.10)')'clim_prec_inmet',date1,date3
  WRITE(*,*)'fnmd: ',TRIM(fnmd)

!
! Make the interpolation of the climatology and evaluate the mean on the period  
!

  rest=MOD(iyear,4)       ! Decide if is a leap year     
  IF ( rest .EQ. 0 ) THEN
    lastd=monlb(imonth)
  ELSE
    lastd=monl(imonth)
  END IF

  bufm=0.0
  nd=0
  WRITE(cdate,'(I4.4,3I2.2)') iyear,imonth,iday,iutc
  READ(cdate(1:10),'(I10)') date
  DO WHILE ( date .LE. date3 )
    nd=nd+1

    WRITE(*,*)'date: ',date
    WRITE(cdate,'(I10.10)') date
    READ(cdate(1: 4),'(I4)') iyear
    READ(cdate(5: 6),'(I2)') imonth
    READ(cdate(7: 8),'(I2)') iday
    READ(cdate(9:10),'(I2)') iutc

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
    
    OPEN(10, FILE=TRIM(dirclm)//TRIM(fnmi(mf)), STATUS='OLD', FORM='UNFORMATTED', READONLY,  & 
             ACCESS='DIRECT', RECL=loni*lati*4, CONVERT='LITTLE_ENDIAN')
    OPEN(11, FILE=TRIM(dirclm)//TRIM(fnmi(mn)), STATUS='OLD', FORM='UNFORMATTED', READONLY,  & 
             ACCESS='DIRECT', RECL=loni*lati*4, CONVERT='LITTLE_ENDIAN')

    READ(10,REC=iprec)((bufa(i,j),i=1,loni),j=1,lati)
    READ(11,REC=iprec)((bufb(i,j),i=1,loni),j=1,lati)

    DO j=1,lati
      DO i=1,loni
        bufi(i,j)=(f2*bufa(i,j)+f1*bufb(i,j))
      END DO
    END DO
    bufm(:,:)=bufm(:,:)+bufi(:,:)

    CLOSE(10)
    CLOSE(11)

    iday=iday+1
    IF ( iday .GT. lastd ) THEN
      iday=1
      imonth=imonth+1
      IF ( rest .EQ. 0 ) THEN
        lastd=monlb(imonth)
      ELSE
        lastd=monl(imonth)
      END IF
      IF ( imonth .GT. 12 ) THEN
        imonth=1
	iyear=iyear+1
        rest=MOD(iyear,4)
        IF ( rest .EQ. 0 ) THEN
          lastd=monlb(imonth)
        ELSE
          lastd=monl(imonth)
        END IF
      END IF
    END IF
    WRITE(cdate,'(I4.4,3I2.2)') iyear,imonth,iday,iutc
    READ(cdate(1:10),'(I10)') date

  END DO

  WRITE(*,*)'nd: ',nd
  bufm(:,:)=bufm(:,:)/FLOAT(nd)

  ! 12 (output): mean of the interpolated climatology on the period 
   
  OPEN(12, FILE=TRIM(fnmd), STATUS='UNKNOWN', FORM='UNFORMATTED',  &  
           ACCESS='DIRECT', RECL=loni*lati*4, CONVERT='LITTLE_ENDIAN')
  n=1
  WRITE(12,REC=n)(((bufm(i,j)),i=1,loni),j=1,lati)
  CLOSE(12)

!
! Generate the ctl file
!

  READ(cdate3(1: 4),'(I4)') iyear
  READ(cdate3(5: 6),'(I2)') imonth
  READ(cdate3(7: 8),'(I2)') iday
  READ(cdate3(9:10),'(I2)') iutc
  CALL clmdy(iday,imonth,iyear,iutc,fnmd)

  STOP
  END PROGRAM prec_clim_inmet


!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++
!
  SUBROUTINE clmdy(iday,imonth,iyear,iutc,fnmd)

  IMPLICIT NONE

  INTEGER,          INTENT(IN) :: iday,imonth,iyear,iutc
  CHARACTER(LEN=*), INTENT(IN) :: fnmd

  CHARACTER (LEN=3) :: mon(12)

  DATA mon/'JAN','FEB','MAR','APR','MAY','JUN',  &
           'JUL','AUG','SEP','OCT','NOV','DEC'/

  OPEN(20,FILE=TRIM(fnmd)//'.ctl',STATUS='UNKNOWN',FORM='FORMATTED')

  WRITE(20,'(2A)')'DSET ^',TRIM(fnmd)
  WRITE(20,'(A)')'*'
  WRITE(20,'(A)')'OPTIONS TEMPLATE'
  WRITE(20,'(A)')'*'
  WRITE(20,'(A)')'UNDEF 777.7'
  WRITE(20,'(A)')'*'
  WRITE(20,'(A)')'TITLE INTERPOLACAO'
  WRITE(20,'(A)')'*'
  WRITE(20,'(A)')'XDEF 176 LINEAR -76 .25'
  WRITE(20,'(A)')'YDEF 160 LINEAR -34 .25'
  WRITE(20,'(A)')'ZDEF   1 LINEAR 1 1'
  WRITE(20,'(A16,I2.2,A1,I2.2,A3,I4.4,A4)') 'TDEF   1 LINEAR ',iutc,'Z',iday,  &
                                           mon(imonth),iyear,' 1DY'
  WRITE(20,'(A)')'*'
  WRITE(20,'(A)')'VARS 1'
  WRITE(20,'(A)')'PRECIP  0 99 Total Precipitation rate (mm/day)'
  WRITE(20,'(A)')'ENDVARS'

  CLOSE(20)

  RETURN
  END SUBROUTINE clmdy
  
