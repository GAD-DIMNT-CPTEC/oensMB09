!-------------------------------------------------------------------------
!
!  le saidas grib do modelo global e escolhe as variaveis de superficie e
!  outros niveis padroes para o projeto TIGGE (ensemble) em formato grib1
!
!  Autor: Julio Pablo Reyes Fernandez
!
!-------------------------------------------------------------------------

 program Tigge

 use Constants 
 
 implicit none
 
 integer ivar

!
! simple variables
!
 call simple

!
! accumulated variables
! 
 do ivar = 13,19
 
 call accum ( ivar )
 
 enddo

 close(5)

 stop 9999
 
CONTAINS 
 
 subroutine simple 
  
 use Constants

 implicit none

 integer i,iunit,ierr,ivar,klev,k,iret
 character*256 varname 

 iunit=10
 aok=.true.
!
!    read input file name
!
 read(5,80,end=999) fname
 80     format(a91)

 do while(fname.ne.'done')

  i=i+1
  iunit=i+1
!
  lengh=index(fname,' ')-1
  write(6,*) 'trying to open ', fname(1:lengh)
        
  call baopen(iunit,fname(1:lengh),ierr)
  if (ierr .ne. 0) write(6,*) 'baopen error!'

!
! Single level data
!

  print*,' Single level data'

  do ivar = 1, 12

   varname=tiggename(ivar)
   
!     JPDS         INTEGER (200) PDS PARAMETERS FOR WHICH TO SEARCH
!                  (=-1 FOR WILDCARD)
!          (1)   - ID OF CENTER
!          (2)   - GENERATING PROCESS ID NUMBER
!          (3)   - GRID DEFINITION
!          (4)   - GDS/BMS FLAG (RIGHT ADJ COPY OF OCTET 8)
!          (5)   - INDICATOR OF PARAMETER
!          (6)   - TYPE OF LEVEL
!          (7)   - HEIGHT/PRESSURE , ETC OF LEVEL
!          (8)   - YEAR INCLUDING (CENTURY-1)
!          (9)   - MONTH OF YEAR
!          (10)  - DAY OF MONTH
!          (11)  - HOUR OF DAY
!          (12)  - MINUTE OF HOUR
!          (13)  - INDICATOR OF FORECAST TIME UNIT
!          (14)  - TIME RANGE 1
!          (15)  - TIME RANGE 2
!          (16)  - TIME RANGE FLAG
!          (17)  - NUMBER INCLUDED IN AVERAGE
!          (18)  - VERSION NR OF GRIB SPECIFICATION
!          (19)  - VERSION NR OF PARAMETER TABLE
!          (20)  - NR MISSING FROM AVERAGE/ACCUMULATION
!          (21)  - CENTURY OF REFERENCE TIME OF DATA
!          (22)  - UNITS DECIMAL SCALE FACTOR
!          (23)  - SUBCENTER NUMBER
!          (24)  - PDS BYTE 29, FOR NMC ENSEMBLE PRODUCTS
!                  128 IF FORECAST FIELD ERROR
!                   64 IF BIAS CORRECTED FCST FIELD
!                   32 IF SMOOTHED FIELD
!                  WARNING: CAN BE COMBINATION OF MORE THAN 1
!          (25)  - PDS BYTE 30, NOT USED

   jpds(:)=-1
   jpds(5)=jpdssuns(5,ivar)
   jpds(6)=jpdssuns(6,ivar)
   jpds(7)=jpdssuns(7,ivar)
   jgds(:)=-1
   jens(:)=-1

   call postigge ( iunit,fname(1:lengh),varname )

  enddo

!
! Upper air data
!
  print*,' Upper air data'

  do ivar = 20, 24
  
   varname=tiggename(ivar)

   if(ivar == 21 ) then
!
! addition geopotential height at 50 hPa
!
   klev = 9
   else
   klev = 8
   endif

   do k = 1, klev

   jpds(:)=-1
   jpds(5)=jpdssuns(5,ivar)
   jpds(6)=jpdssuns(6,ivar)
   jpds(7)=ilev(k)
   jgds(:)=-1
   jens(:)=-1

   call postigge ( iunit,fname(1:lengh),varname )
 
   enddo

  enddo
      
!
! CLOSE GRIB UNIT
!        
  call baclose(iunit,ierr)
!
!
  if(iunit.gt.90) iunit=10

  read(5,80,end=999) fname
  if (iret.ne.0) then
  fname='done'
  aok=.false.
  endif
  enddo
!
!       end while
! 
 999    continue

   end subroutine simple
!
!--------------------------------------------------------------------------
!   
 subroutine accum (ivar)

 use Constants

 implicit none
 integer kpdso(200),kgdso(200),kenso(200)
 integer i,iunit,ierr,ivar,iret,kf,kr,iacc,n,ibs,nbits
 real sum(ji), fact
 character*256 varname
! character*256 fname,name,tiggename

 i=0
 ibs = 0
 nbits = 16
 iunit=10
 iacc=0
 sum=0.
 lisum=.true.
 aok=.true.

 
!
!    read input file name
!
 rewind 5
 read(5,80)fname
 read(5,80,end=999) fname
 80     format(a91)

 do while(fname.ne.'done')

  i=i+1
  iunit=i+1
!
  lengh=index(fname,' ')-1
  write(6,*) 'trying to open ', fname(1:lengh)
        
  call baopen(iunit,fname(1:lengh),ierr)
  if (ierr .ne. 0) write(6,*) 'baopen error!'   


   varname=tiggename(ivar)

   jpds(:)=-1
   jpds(5)=jpdssuns(5,ivar)
   jpds(6)=jpdssuns(6,ivar)
   jpds(7)=jpdssuns(7,ivar)
   jgds(:)=-1
   jens(:)=-1
   
!
! accum variables
!

  call GETGBE(iunit,0,JI,0,JPDS,JGDS,JENS,KF,KR,        &
      KPDS,KGDS,KENS,LI,dummy,IRET)

  lengh=index(varname,' ')-1

  if (iret .ne. 0) then
   write(6,*) varname(1:lengh)//' TROUBLE AHEAD!!!'
   write(6,*) jpds(5),jpds(6),jpds(7),'IRET=' ,IRET
   return
  else


    if (i.eq.1) then
!
!   assume files are in cronological order, first file will
!   have needed date info
!
      kpdso=kpds
      kgdso=kgds
      kenso=kens
      print*,'kens =',kens(1),kens(2),kens(3),kens(4),kens(5)
      print*,'jpds =',jpds(5),jpds(6),jpds(7)
      print*,'kpds =',kpds(5),kpds(6),kpds(7),kpds(16)
      kpdso(16)=4		! fix bug accum radiation variables
    endif

      iacc=iacc+kpds(15)-kpds(14)
      print*,'iacc, kpds15,kpds14 =',iacc,kpds(15),kpds(14)
!      print*,'kpds =',kpds
 
      if(ivar.ge.15) then
      fact=3600.*6.
      else
      fact=0.25
      endif

      DO N=1,kf
        sum(N)=sum(N) + dummy(N)*fact     ! warning only for 6 hours
        lisum(N)=lisum(N).and.li(N)
      ENDDO
!        endif
!      
!   save acc data
!
      KPDSo(15)=kpdso(14)+iacc
      KPDSo(22)=2

       print*,'Max('//varname(1:lengh)//')=',maxval(sum), &
         ' Min('//varname(1:lengh)//')=',minval(sum)

   name='tigge_'//varname(1:lengh)//'_sfc_'//fname(18:27)//'.grb'   
   lengh1=index(name,' ')-1
!
! Save data in individual files
!
  CALL BAOPEN(91,name(1:lengh1),ierr)
  if (ierr .ne. 0) write(6,*) 'output baopen error!'
   call putgben(91,kf,kpdso,kgdso,kenso,ibs,nbits,lisum,sum,iret)
   write(6,*) jpds(5),jpds(6),jpds(7),'IRET=' ,IRET
   if (iret .ne. 0) write(6,*) 'putgben error!'
  CALL BACLOSE(91,ierr)


   endif      
!
! CLOSE GRIB UNIT
!        
  call baclose(iunit,ierr)
!
!
  if(iunit.gt.90) iunit=10

  read(5,80,end=999) fname
  if (iret.ne.0) then
  fname='done'
  aok=.false.
  endif
  enddo
!
!       end while
! 
 999    continue   
   
   end subroutine accum
!   
! -------------------------------------------------------------------------
!
   subroutine postigge (iunit, fname,tiggename )

   use Constants, only: jpds,jgds,jens, kpds, kgds, kens, ji, &
                        lengh, lengh1, li, dummy

  implicit none
  integer ibs,ii,iunit,nbits,kf,kr,iret,ierr
  character(LEN=*) :: fname,tiggename
  character(LEN=256) :: name
  character*4 nivel

  ibs = 0
  nbits = 16  

  call GETGBE(iunit,0,JI,0,JPDS,JGDS,JENS,KF,KR,        &
      KPDS,KGDS,KENS,LI,dummy,IRET)

  lengh=index(tiggename,' ')-1

  if (iret .ne. 0) then
   write(6,*) tiggename(1:lengh)//' TROUBLE AHEAD!!!'
   write(6,*) jpds(5),jpds(6),jpds(7),'IRET=' ,IRET
   write(6,*) jpds(5),jpds(6),jpds(7)
   return
  endif

  if(jpds(5)==81) then	! mask

    do ii=1,ji
     if( dummy(ii)==1.  ) dummy(ii)=0.       ! ocean for TIGGE
     if( dummy(ii)==-1. ) dummy(ii)=1.       ! land for TIGGE
    enddo
       
  endif

  if(jpds(5)==135 .or. jpds(5)==2) then	! pressure

    do ii=1,ji
      dummy(ii) = dummy(ii)*100. ! from hPa to Pascal for TIGGE
    enddo

  endif

  if(jpds(5)==71) then	! cloud cover

    do ii=1,ji
      dummy(ii) = dummy(ii)*100. ! from 0-1 to % for TIGGE
    enddo

  endif
!
! create individual names
!
  if(jpds(6)/=100) then
   print*,'Max('//tiggename(1:lengh)//')=',maxval(dummy), &
         ' Min('//tiggename(1:lengh)//')=',minval(dummy)

   name='tigge_'//tiggename(1:lengh)//'_sfc_'//fname(18:27)//'.grb'
  else
   print*,'Level=',jpds(7),'Max('//tiggename(1:lengh)//')=',maxval(dummy), &
         ' Min('//tiggename(1:lengh)//')=',minval(dummy)

   write(nivel,50) jpds(7)
 50 format(I4.4)
   name='tigge_'//tiggename(1:lengh)//'_'//nivel//'_'//fname(18:27)//'.grb'
  endif
   lengh1=index(name,' ')-1
!
! Save data in individual files
!
  CALL BAOPEN(91,name(1:lengh1),ierr)
  if (ierr .ne. 0) write(6,*) 'output baopen error!'
   call putgben(91,kf,kpds,kgds,kens,ibs,nbits,li,dummy,iret)
   write(6,*) jpds(5),jpds(6),jpds(7),'IRET=' ,IRET
   if (iret .ne. 0) write(6,*) 'putgben error!'
  CALL BACLOSE(91,ierr)

 end subroutine postigge

 end program Tigge
