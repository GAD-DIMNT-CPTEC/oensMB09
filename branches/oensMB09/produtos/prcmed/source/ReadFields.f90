MODULE ReadFields

   IMPLICIT NONE
   PRIVATE
   PUBLIC :: ReadGrib

   CONTAINS

   SUBROUTINE ReadGrib(fname,imax,jmax,prec,temp,statfctrd)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   ! Subroutine ReadGrib
   !
   ! Julio P. R. Fernandez, DMD/CPTEC
   ! 11/2005
   ! Modified: Antonio Marcos Mendonca
   ! Dec/2008
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   CHARACTER(LEN=*),         INTENT(IN)    :: fname
   INTEGER,  	             INTENT(IN)    :: imax,jmax
   REAL,DIMENSION(imax,jmax),INTENT(OUT)   :: prec,temp
   INTEGER,                  INTENT(INOUT) :: statfctrd

   ! Local Variables

   INTEGER*4                :: nxny
   INTEGER*4                :: i,j,k,iunit,ierr,nwords,knum
   INTEGER*4,DIMENSION(200) :: jpds,jgds,kpds,kgds

   REAL,DIMENSION(imax*jmax)     :: tmp

   LOGICAL*1,DIMENSION(imax*jmax) :: bitmap


   !
   ! READ DATA IN GRIB1 FORMAT
   !

   nxny=imax*jmax
   iunit=12

   write(*,*) ' '
   write(*,*) 'fname: ',fname
   CALL BAOPEN(iunit,fname,ierr)
   IF (ierr .NE. 0) THEN
      WRITE(6,*) 'BAOPEN TROUBLE!!!! ', ierr
      statfctrd=1
   END IF 


   !
   ! variable tems
   !

   jpds(:)=-1
   jgds(:)=-1
   jpds(5)=188
   jpds(6)=1
   jpds(7)=0

   CALL GETGB(iunit,0,nxny,0,jpds,jgds,nwords,knum,kpds,kgds,bitmap,tmp,ierr)
   IF (ierr .ne. 0) THEN
      WRITE(6,*) 'getgb TROUBLE!!!! field', ierr
      statfctrd=1
   END IF

   WRITE(6,*)'Variable: ',jpds(5),jpds(6),jpds(7)
   WRITE(*,*) 'nwords: ',nwords

   IF (nwords .NE. imax*jmax) THEN
      WRITE(*,*) 'field n.o of points is wrong: ',nwords  
      statfctrd=1
   END IF  

   DO j=1,jmax
   DO i=1,imax
      k=(j-1)*imax+i
      temp(i,j)=tmp(k)	  
   END DO
   END DO 

   !
   ! variable prec
   !

   jpds(:)=-1
   jgds(:)=-1
   jpds(5)=61 !- INDICATOR OF PARAMETER
   jpds(6)=1  !- TYPE OF LEVEL
   jpds(7)=0  !- HEIGHT/PRESSURE , ETC OF LEVEL

   CALL GETGB(iunit,0,nxny,0,jpds,jgds,nwords,knum,kpds,kgds,bitmap,tmp,ierr)
   IF (ierr .ne. 0) THEN
      WRITE(6,*) 'getgb TROUBLE!!!! field', ierr
      statfctrd=1
   END IF

   WRITE(6,*)'Variable: ',jpds(5),jpds(6),jpds(7)
   WRITE(*,*) 'nwords: ',nwords

   IF (nwords .NE. imax*jmax) THEN
      WRITE(*,*) 'field n.o of points is wrong: ',nwords  
      statfctrd=1
   END IF  

   DO j=1,jmax
   DO i=1,imax
      k=(j-1)*imax+i
      prec(i,j)=tmp(k)	  
   END DO
   END DO 


   CALL BACLOSE(iunit,ierr)
   WRITE(6,*) 'BACLOSE :', ierr
   IF (ierr .NE. 0) WRITE(6,*) 'BACLOSE TROUBLE!!!! ', ierr

   RETURN
   END SUBROUTINE ReadGrib


END MODULE ReadFields
