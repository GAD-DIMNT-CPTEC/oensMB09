MODULE ReadFields_accum

   IMPLICIT NONE
   PRIVATE
   PUBLIC :: ReadGribFct,ReadGribIcn

   CONTAINS

   SUBROUTINE ReadGribFct(fname,imax,jmax,field,fieldb,fieldc,statfctrd)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   ! Subroutine ReadGrib
   !
   ! Julio P. R. Fernandez, DMD/CPTEC
   ! 11/2005
   ! Modified: Antonio Marcos Mendonca
   ! 10/2007
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   CHARACTER(LEN=*),         INTENT(IN)    :: fname
   INTEGER,  	             INTENT(IN)    :: imax,jmax
   REAL,DIMENSION(imax,jmax),INTENT(OUT)   :: field
   REAL,DIMENSION(imax,jmax),INTENT(OUT)   :: fieldb
   REAL,DIMENSION(imax,jmax),INTENT(OUT)   :: fieldc
   INTEGER,                  INTENT(INOUT) :: statfctrd

   ! Local Variables

   INTEGER*4                :: nxny
   INTEGER*4                :: i,j,k,iunit,ierr,nwords,knum
   INTEGER*4,DIMENSION(200) :: jpds,jgds,kpds,kgds
   INTEGER*4,DIMENSION(200) :: jpds1,jgds1,kpds1,kgds1
   INTEGER*4,DIMENSION(200) :: jpds2,jgds2,kpds2,kgds2

   REAL,DIMENSION(imax*jmax)     :: tmp

   LOGICAL*1,DIMENSION(imax*jmax) :: bitmap


   !
   ! READ DATA IN GRIB1 FORMAT
   !
   
   !!!!!!!!!!!!!!!!!!!!!!!
   ! 3D variable (ROLE) 
   !
   
   nxny=imax*jmax
   iunit=12   
   jpds=-1
   jgds=-1
 
   write(*,*) ' '
   write(*,*) 'fname: ',fname
!   CALL BAOPEN(iunit,fname,ierr)
   CALL BAOPENR(iunit,fname,ierr)
   IF (ierr .NE. 0) THEN
       WRITE(6,*) 'BAOPEN TROUBLE!!!! ', ierr
       statfctrd=1
   END IF 

   jpds(5)=114
   jpds(6)=8
   jpds(7)=0
   
   CALL GETGB(iunit,0,nxny,0,jpds,jgds,nwords,knum,kpds,kgds,bitmap,tmp,ierr)
   IF (ierr .ne. 0) THEN
       WRITE(6,*) 'getgb TROUBLE!!!! field', ierr
       statfctrd=1
   END IF

   WRITE(6,*)'Variabl1: ',jpds(5),jpds(6),jpds(7)
   WRITE(*,*) 'nwords: ',nwords   

   IF (nwords .NE. imax*jmax) THEN
       WRITE(*,*) 'field n.o of points is wrong: ',nwords  
       statfctrd=1
   END IF  

   DO j=1,jmax
      DO i=1,imax
         k=(j-1)*imax+i
         field(i,j)=tmp(k)	  
      END DO
   END DO 

   CALL BACLOSE(iunit,ierr)
   WRITE(6,*) 'BACLOSE :', ierr
   IF (ierr .NE. 0) WRITE(6,*) 'BACLOSE TROUBLE!!!! ', ierr
      
   !!!!!!!!!!!!!!!!!!!!!!!
   !  3D variable (U200mb)
   !

   iunit=13
   jpds1=-1
   jgds1=-1
 
   write(*,*) ' '
   write(*,*) 'fname: ',fname
   !CALL BAOPEN(iunit,fname,ierr)
   CALL BAOPENR(iunit,fname,ierr)
   IF (ierr .NE. 0) THEN
       WRITE(6,*) 'BAOPEN TROUBLE!!!! ', ierr
       statfctrd=1
   END IF
   
   jpds1(5)=255
   jpds1(6)=100
   jpds1(7)=200

   CALL GETGB(iunit,0,nxny,0,jpds1,jgds1,nwords,knum,kpds1,kgds1,bitmap,tmp,ierr)
   IF (ierr .ne. 0) THEN
       WRITE(6,*) 'getgb TROUBLE!!!! fieldb', ierr
       statfctrd=1
   END IF
   
   WRITE(6,*)'Variabl2: ',jpds1(5),jpds1(6),jpds1(7)
   WRITE(*,*) 'nwords: ',nwords   
   
   IF (nwords .NE. imax*jmax) THEN
       WRITE(*,*) 'fieldb n.o of points is wrong: ',nwords  
       statfctrd=1
   END IF
   
   DO j=1,jmax
      DO i=1,imax
         k=(j-1)*imax+i
         fieldb(i,j)=tmp(k)	  
      END DO
   END DO 
   
   CALL BACLOSE(iunit,ierr)
   WRITE(6,*) 'BACLOSE :', ierr
   IF (ierr .NE. 0) WRITE(6,*) 'BACLOSE TROUBLE!!!! ', ierr
      
   !!!!!!!!!!!!!!!!!!!!!!!
   !  3D variable (U850mb)
   !

   iunit=14   
   jpds2=-1
   jgds2=-1
 
   write(*,*) ' '
   write(*,*) 'fname: ',fname
   !CALL BAOPEN(iunit,fname,ierr)
   CALL BAOPENR(iunit,fname,ierr)
   IF (ierr .NE. 0) THEN
       WRITE(6,*) 'BAOPEN TROUBLE!!!! ', ierr
       statfctrd=1
   END IF
   
   jpds2(5)=255
   jpds2(6)=100
   jpds2(7)=850

   CALL GETGB(iunit,0,nxny,0,jpds2,jgds2,nwords,knum,kpds2,kgds2,bitmap,tmp,ierr)
   IF (ierr .ne. 0) THEN
       WRITE(6,*) 'getgb TROUBLE!!!! fieldc', ierr
       statfctrd=1
   END IF   
   
   WRITE(6,*)'Variabl3: ',jpds2(5),jpds2(6),jpds2(7)
   WRITE(*,*) 'nwords: ',nwords   
   
   IF (nwords .NE. imax*jmax) THEN
       WRITE(*,*) 'fieldc n.o of points is wrong: ',nwords  
       statfctrd=1
   END IF   
   
   DO j=1,jmax
      DO i=1,imax
         k=(j-1)*imax+i
         fieldc(i,j)=tmp(k)	  
      END DO
   END DO 
   
   CALL BACLOSE(iunit,ierr)
   WRITE(6,*) 'BACLOSE :', ierr
   IF (ierr .NE. 0) WRITE(6,*) 'BACLOSE TROUBLE!!!! ', ierr


   RETURN
   END SUBROUTINE ReadGribFct


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   SUBROUTINE ReadGribIcn(fname,imax,jmax,fieldb,fieldc,statfctrd)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   ! Subroutine ReadGrib
   !
   ! Julio P. R. Fernandez, DMD/CPTEC
   ! 11/2005
   ! Modified: Antonio Marcos Mendonca
   ! 10/2007
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   CHARACTER(LEN=*),         INTENT(IN)    :: fname
   INTEGER,  	             INTENT(IN)    :: imax,jmax
   REAL,DIMENSION(imax,jmax),INTENT(OUT)   :: fieldb
   REAL,DIMENSION(imax,jmax),INTENT(OUT)   :: fieldc
   INTEGER,                  INTENT(INOUT) :: statfctrd

   ! Local Variables

   INTEGER*4                :: nxny
   INTEGER*4                :: i,j,k,iunit,ierr,nwords,knum
   INTEGER*4,DIMENSION(200) :: jpds,jgds,kpds,kgds
   INTEGER*4,DIMENSION(200) :: jpds1,jgds1,kpds1,kgds1
   INTEGER*4,DIMENSION(200) :: jpds2,jgds2,kpds2,kgds2

   REAL,DIMENSION(imax*jmax)     :: tmp

   LOGICAL*1,DIMENSION(imax*jmax) :: bitmap


   !
   ! READ DATA IN GRIB1 FORMAT
   !
   
   !!!!!!!!!!!!!!!!!!!!!!!
   !  3D variable (U200mb)
   !
   nxny=imax*jmax
   iunit=13
   jpds1=-1
   jgds1=-1
 
   write(*,*) ' '
   write(*,*) 'fname: ',fname
   !CALL BAOPEN(iunit,fname,ierr)
   CALL BAOPENR(iunit,fname,ierr)
   IF (ierr .NE. 0) THEN
       WRITE(6,*) 'BAOPEN TROUBLE!!!! ', ierr
       statfctrd=1
   END IF
   
   jpds1(5)=33
   jpds1(6)=100
   jpds1(7)=200

   CALL GETGB(iunit,0,nxny,0,jpds1,jgds1,nwords,knum,kpds1,kgds1,bitmap,tmp,ierr)
   IF (ierr .ne. 0) THEN
       WRITE(6,*) 'getgb TROUBLE!!!! fieldb', ierr
       statfctrd=1
   END IF
   
   WRITE(6,*)'Variabl2: ',jpds1(5),jpds1(6),jpds1(7)
   WRITE(*,*) 'nwords: ',nwords   
   
   IF (nwords .NE. imax*jmax) THEN
       WRITE(*,*) 'fieldb n.o of points is wrong: ',nwords  
       statfctrd=1
   END IF
   
   DO j=1,jmax
      DO i=1,imax
         k=(j-1)*imax+i
         fieldb(i,j)=tmp(k)	  
      END DO
   END DO 
   
   CALL BACLOSE(iunit,ierr)
   WRITE(6,*) 'BACLOSE :', ierr
   IF (ierr .NE. 0) WRITE(6,*) 'BACLOSE TROUBLE!!!! ', ierr
      
   !!!!!!!!!!!!!!!!!!!!!!!
   !  3D variable (U850mb)
   !

   iunit=14   
   jpds2=-1
   jgds2=-1
 
   write(*,*) ' '
   write(*,*) 'fname: ',fname
   !CALL BAOPEN(iunit,fname,ierr)
   CALL BAOPENR(iunit,fname,ierr)
   IF (ierr .NE. 0) THEN
       WRITE(6,*) 'BAOPEN TROUBLE!!!! ', ierr
       statfctrd=1
   END IF
   
   jpds2(5)=33
   jpds2(6)=100
   jpds2(7)=850

   CALL GETGB(iunit,0,nxny,0,jpds2,jgds2,nwords,knum,kpds2,kgds2,bitmap,tmp,ierr)
   IF (ierr .ne. 0) THEN
       WRITE(6,*) 'getgb TROUBLE!!!! fieldc', ierr
       statfctrd=1
   END IF   
   
   WRITE(6,*)'Variabl3: ',jpds2(5),jpds2(6),jpds2(7)
   WRITE(*,*) 'nwords: ',nwords   
   
   IF (nwords .NE. imax*jmax) THEN
       WRITE(*,*) 'fieldc n.o of points is wrong: ',nwords  
       statfctrd=1
   END IF   
   
   DO j=1,jmax
      DO i=1,imax
         k=(j-1)*imax+i
         fieldc(i,j)=tmp(k)	  
   END DO
   END DO 
   
   CALL BACLOSE(iunit,ierr)
   WRITE(6,*) 'BACLOSE :', ierr
   IF (ierr .NE. 0) WRITE(6,*) 'BACLOSE TROUBLE!!!! ', ierr


   RETURN
   END SUBROUTINE ReadGribIcn


END MODULE ReadFields_accum
