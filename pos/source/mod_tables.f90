!
! Author: Luiz Flavio (PAD)
! 
!
MODULE TABLES

  !Vars
  INTEGER :: size_tb(3)
  TYPE tb1
    CHARACTER(LEN=4)  :: name
    INTEGER           :: id
    CHARACTER(LEN=40) :: title
    CHARACTER(LEN=16) :: unit
    CHARACTER(LEN=5)  :: tipo
    CHARACTER(LEN=6)  :: level
    INTEGER           :: dec_scal_fact
    INTEGER           :: precision
    CHARACTER(LEN=10) :: coment1
    CHARACTER(LEN=10) :: coment2
  END TYPE tb1
  
  TYPE(tb1),ALLOCATABLE,DIMENSION(:) :: table1
  
  !Levels
  TYPE tb2
    CHARACTER(LEN=8)  :: level_type
    CHARACTER(LEN=26) :: level_descr
    CHARACTER(LEN=8)  :: unit
    CHARACTER(LEN=6)  :: vert
    CHARACTER(LEN=4)  :: positive
    INTEGER           :: default
    INTEGER           :: id
    INTEGER           :: p1
    INTEGER           :: p2
  END TYPE tb2
  
  TYPE(tb2),ALLOCATABLE,DIMENSION(:) :: table2
  
  !Center
  TYPE tb3
    CHARACTER(LEN=10) :: center
    INTEGER           :: id
    INTEGER           :: grib_center
    INTEGER           :: sub_center
  END TYPE tb3
  
  TYPE(tb3),ALLOCATABLE,DIMENSION(:) :: table3
  
  LOGICAL :: tables_readed=.FALSE.
  PUBLIC :: Init_tables  
 CONTAINS

  SUBROUTINE Init_tables(datalib)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: datalib
    INTEGER :: ierr,i,k,is
    CHARACTER(LEN=200) :: lixo
    CHARACTER :: ic
    is=MAX(1, LEN_TRIM(datalib))

    DO k=1,3    
    
      WRITE(ic,FMT='(I1.1)') k

      OPEN(UNIT=63,FILE=TRIM(datalib)//'/'//'tab'//ic//'.dat',ACCESS='sequential',STATUS='old')
    
      READ (63,FMT='(A200)') lixo
      READ (63,FMT='(A200)') lixo
      READ (63,FMT='(A200)') lixo
      READ (63,FMT='(A200)') lixo
      READ (63,FMT='(A200)') lixo
    
      i=0;ierr=0
    
      DO WHILE (ierr==0)
        i=i+1
        READ(63,FMT='(A200)',IOSTAT=ierr) lixo
      END DO
      size_tb(k)=i-1 
    
      CLOSE(UNIT=63)
    END DO
    ALLOCATE(table1(size_tb(1)),table2(size_tb(2)),table3(size_tb(3)))
    CALL ReadTables(datalib)
  END SUBROUTINE Init_Tables
 
  SUBROUTINE ReadTables(datalib)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: datalib
    INTEGER :: k,i,is
    CHARACTER(LEN=200) :: lixo
    CHARACTER :: ic

    is=MAX(1, LEN_TRIM(datalib))

    DO k=1,3    
    
      WRITE(ic,FMT='(I1.1)') k
!
! bug 
!
      OPEN(UNIT=63,FILE=TRIM(datalib)//'/'//'tab'//ic//'.dat',ACCESS='sequential',STATUS='old')
    
      READ (63,FMT='(A200)') lixo
      READ (63,FMT='(A200)') lixo
      READ (63,FMT='(A200)') lixo
      READ (63,FMT='(A200)') lixo
      READ (63,FMT='(A200)') lixo
 
      DO i=1,size_tb(k)
!        PRINT *,'i=',i
        IF(k==1) THEN
	  READ(63,FMT='(A4,3X,I3,3X,A40,5X,A16,3X,A5,3X,A6,3X,I4,3X,I2,3X,A2,3X,A2)')	  &
	                table1(i)%name,         &
                        table1(i)%id,           &
                        table1(i)%title,        &
                        table1(i)%unit,         &
                        table1(i)%tipo,         &
                        table1(i)%level,        &
                        table1(i)%dec_scal_fact,&
                        table1(i)%precision,    &
                        table1(i)%coment1,      &
                        table1(i)%coment2
!           PRINT *,table1(i)%name,table1(i)%id,table1(i)%title
        ELSE IF(k==2) THEN
	  READ(63,FMT='(A8,3X,A26,2X,A8,3X,A6,3X,A4,3X,I4,2X,I2,2X,I3,3X,I2)') &
	                table2(i)%level_type,   &
                        table2(i)%level_descr,  &
                        table2(i)%unit,         &
                        table2(i)%vert,         &
                        table2(i)%positive,     &
                        table2(i)%default,      &
                        table2(i)%id,           &
                        table2(i)%p1,           &
                        table2(i)%p2  
        ELSE
	  READ(63,FMT='(A10,2X,I3,3X,I3,3X,I1)') &
	                table3(i)%center,       &
                        table3(i)%id,           &
                        table3(i)%grib_center,  &
                        table3(i)%sub_center
        END IF

      END DO
    END DO  
    tables_readed=.TRUE.

  END SUBROUTINE ReadTables
  
END MODULE TABLES
