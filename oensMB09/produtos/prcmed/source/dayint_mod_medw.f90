PROGRAM dayint_mod_medw 

IMPLICIT NONE
  
!
! This program makes the interpolation of the climatological precipitation
! to the requested date, and evaluate the mean on the period, set up by 
! date2 and date3. The date1 (analysis date) is used to construct the name of
! output file. 
!

! loni : nb. of longitude points  
! lati : nb. of latitude points  
! nsph : number of second in a hour
! iprec: number of the precipitation record in the climatological files
 
  INTEGER, PARAMETER :: loni=192,lati=96,nsph=86400   
  INTEGER, PARAMETER :: iprec=14, itast=11                   
 
  INTEGER :: i,j,n,l,mon,mf,mn,nd,k,m
  INTEGER :: date,date1,date2,date3
  INTEGER :: iyear,imonth,iday,iutc,rest,lastd
  INTEGER, DIMENSION(12) :: monl,monlb

  REAL :: yday,add,f1,f2
  REAL, DIMENSION(loni,lati):: bufa,bufb,bufi,bufm,bufg,bufc,bufd,bufe

  CHARACTER(LEN=10)   :: cdate,cdate1,cdate2,cdate3

  CHARACTER(LEN=200) :: fnmd,fnme
  CHARACTER(LEN=200) :: dirclm
  CHARACTER(LEN=200), DIMENSION(12) :: fnmi

  DATA monl  /31,28,31,30,31,30,31,31,30,31,30,31/
  DATA monlb /31,29,31,30,31,30,31,31,30,31,30,31/

  DATA fnmi  /'climatologia50yr.jan.bin','climatologia50yr.feb.bin', &
              'climatologia50yr.mar.bin','climatologia50yr.apr.bin', &
	           'climatologia50yr.may.bin','climatologia50yr.jun.bin', &
              'climatologia50yr.jul.bin','climatologia50yr.aug.bin', &
	           'climatologia50yr.sep.bin','climatologia50yr.oct.bin', &
	           'climatologia50yr.nov.bin','climatologia50yr.dec.bin'/

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
! Name of the day climatology. Mean over the period   
!

  WRITE(fnmd,'(A5,2I10.10)')'cprec',date1,date3
  WRITE(*,*)'fnmd: ',TRIM(fnmd)
  
    
  WRITE(fnme,'(A5,2I10.10)')'ctast',date1,date3
  WRITE(*,*)'fnme: ',TRIM(fnme)
  
  
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
  bufg=0.0
  nd=0
  WRITE(cdate,'(I4.4,3I2.2)') iyear,imonth,iday,iutc
  READ(cdate(1:10),'(I10)') date
  DO WHILE (date .LE. date3) 
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

    !For precipitation
    READ(10,REC=iprec)((bufa(i,j),i=1,loni),j=1,lati)
    READ(11,REC=iprec)((bufb(i,j),i=1,loni),j=1,lati)

    DO j=1,lati
      DO i=1,loni
        bufi(i,j)=(f2*bufa(i,j)+f1*bufb(i,j))
      END DO
    END DO
    bufm(:,:)=bufm(:,:)+bufi(:,:)


    !For time mean surface temperature
    READ(10,REC=itast)((bufc(k,m),k=1,loni),m=1,lati)
    READ(11,REC=itast)((bufd(k,m),k=1,loni),m=1,lati)
 
    DO m=1,lati
      DO k=1,loni
	bufe(k,m)=(f2*bufc(k,m)+f1*bufd(k,m))
     END DO
    END DO
    bufg(:,:)=bufg(:,:)+bufe(:,:)


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
  
  bufg(:,:)=bufg(:,:)/FLOAT(nd)

  
  ! 12 (output): mean of the interpolated climatology on the period 
   
  OPEN(12, FILE=TRIM(fnmd), STATUS='UNKNOWN', FORM='UNFORMATTED',  &  
           ACCESS='DIRECT', RECL=loni*lati*4, CONVERT='LITTLE_ENDIAN')

  n=1
  WRITE(12,REC=n)((bufm(i,j),i=1,loni),j=1,lati)
  
  n=2
  WRITE(12,REC=n)((bufg(k,m),k=1,loni),m=1,lati)
  
  CLOSE(12)
  !CLOSE(13)

!
! Generate the ctl file
!

  READ(cdate3(1: 4),'(I4)') iyear
  READ(cdate3(5: 6),'(I2)') imonth
  READ(cdate3(7: 8),'(I2)') iday
  READ(cdate3(9:10),'(I2)') iutc
  CALL clmdy(iday,imonth,iyear,iutc,fnmd)

  STOP
  END PROGRAM dayint_mod_medw


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
  WRITE(20,'(A)')'OPTIONS LITTLE_ENDIAN'
  WRITE(20,'(A)')'*'
  WRITE(20,'(A)')'UNDEF -2.56E33'
  WRITE(20,'(A)')'*'
  WRITE(20,'(A)')'TITLE CPTEC T062L28 MODEL CLIMATOLOGY'
  WRITE(20,'(A)')'*'
  WRITE(20,'(A)')'XDEF 192 LINEAR   0.0 1.875'
  WRITE(20,'(A)')'YDEF  96 LEVELS'
  WRITE(20,'(A)')' -88.57217 -86.72253 -84.86197 -82.99894 -81.13498 -79.27056 -77.40589 -75.54106'
  WRITE(20,'(A)')' -73.67613 -71.81113 -69.94608 -68.08099 -66.21587 -64.35073 -62.48557 -60.62040'
  WRITE(20,'(A)')' -58.75521 -56.89001 -55.02481 -53.15960 -51.29438 -49.42915 -47.56393 -45.69869'
  WRITE(20,'(A)')' -43.83346 -41.96822 -40.10298 -38.23774 -36.37249 -34.50724 -32.64199 -30.77674'
  WRITE(20,'(A)')' -28.91149 -27.04624 -25.18099 -23.31573 -21.45048 -19.58522 -17.71996 -15.85470'
  WRITE(20,'(A)')' -13.98945 -12.12419 -10.25893  -8.39367  -6.52841  -4.66315  -2.79789  -0.93263'
  WRITE(20,'(A)')'   0.93263   2.79789   4.66315   6.52841   8.39367  10.25893  12.12419  13.98945'
  WRITE(20,'(A)')'  15.85470  17.71996  19.58522  21.45048  23.31573  25.18099  27.04624  28.91149'
  WRITE(20,'(A)')'  30.77674  32.64199  34.50724  36.37249  38.23774  40.10298  41.96822  43.83346'
  WRITE(20,'(A)')'  45.69869  47.56393  49.42915  51.29438  53.15960  55.02481  56.89001  58.75521'
  WRITE(20,'(A)')'  60.62040  62.48557  64.35073  66.21587  68.08099  69.94608  71.81113  73.67613'
  WRITE(20,'(A)')'  75.54106  77.40589  79.27056  81.13498  82.99894  84.86197  86.72253  88.57217'
  WRITE(20,'(A)')'ZDEF   1 LEVELS 1000'
  WRITE(20,'(A16,I2.2,A1,I2.2,A3,I4.4,A4)') 'TDEF   1 LINEAR ',iutc,'Z',iday,  &
                                           mon(imonth),iyear,' 1DY'
  WRITE(20,'(A)')'*'
  WRITE(20,'(A)')'VARS 2'
  WRITE(20,'(A)')'PREC  0 99 Total Precipitation rate (mm/day)'
  WRITE(20,'(A)')'TSMT  0 99 Time Mean Surface Absolute Temperature K'
  WRITE(20,'(A)')'ENDVARS'

  CLOSE(20)

  RETURN
  END SUBROUTINE clmdy
