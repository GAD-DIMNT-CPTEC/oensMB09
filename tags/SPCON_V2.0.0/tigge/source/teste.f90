program le
   USE Constants
   
   implicit none 
   
   integer                   iunit, ierr, iret, kr, kf
!   character*256             fname
!   INTEGER                   ji
!   INTEGER                   jpds(200),jgds(200),jens(200)
!   INTEGER                   kpds(200),kgds(200),kens(200)
!   PARAMETER(ji=384*192)
!   LOGICAL*1                 li(ji)
!   REAL                      dummy(ji)

   READ(5,80,END=999) fname
   80  FORMAT(a91)

   lengh=INDEX(fname,' ')-1
   write (*,*)fname

   iunit=2
   
   CALL baopen(iunit,fname(1:lengh),ierr)
   
   print*, "baopen",ierr
   jpds=-1
   jgds=-1
   jens=-1
   jpds(5)=61
   jpds(6)=1
   jpds(7)=0
   write (*,*) jpds(5:7)
   CALL GETGBE (iunit,0,JI,0,JPDS,JGDS,JENS,KF,KR,        &
               KPDS,KGDS,KENS,LI,dummy,IRET)
   print*, 'IRET=',IRET
   write (*,*)KPDS
   call baclose (iunit,ierr)
   print*, ierr

   
   999 CONTINUE   
   
end program le
