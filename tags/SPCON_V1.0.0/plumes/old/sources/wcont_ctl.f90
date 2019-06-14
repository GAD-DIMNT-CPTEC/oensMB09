SUBROUTINE writectl_out(bvar,nvar,dvar,var,ndiv,vmax,vmin,ndt)
  USE config
  USE geral
  IMPLICIT NONE
  
  INTEGER,INTENT(IN) :: nvar
  CHARACTER(LEN=*),INTENT(IN) :: bvar(nvar)
  CHARACTER(LEN=*),INTENT(IN) :: dvar(nvar)
  INTEGER,INTENT(IN) :: ndiv,ndt,var
  REAL,INTENT(IN) :: vmin,vmax
  
  CHARACTER(LEN=2) :: mes,dia,hor,lv
  CHARACTER(LEN=4) :: ano
  REAL :: delta
  INTEGER :: imes
  CHARACTER(LEN=3),PARAMETER,DIMENSION(12) :: cmes= &
                                     (/'JAN','FEB','MAR','APR','MAY','JUN',  &
                                       'JUL','AUG','SEP','OCT','NOV','DEC'/)
  CHARACTER(LEN=200) :: filenam
  
  WRITE(lv,FMT='(I2.2)') levels
  delta=(vmax-vmin)/ndiv
  
  ano=labeli(01:04)
  mes=labeli(05:06)
  dia=labeli(07:08)
  hor=labeli(09:10)
  imes=asc2int(mes)

  filenam=TRIM(dataoutdir)//'/'//bvar(var)//labeli// &
                   labelf//'.'//TRIM(trunc)//'L'//TRIM(lv)// &
		   '.ctl'
  print *,'Saida: ',filenam
  OPEN(22,FILE=TRIM(filenam),STATUS='UNKNOWN',FORM='FORMATTED')

  WRITE(22,'(A)')'DSET ^'//bvar(var)//labeli// &
                   labelf//'.'//TRIM(trunc)//'L'//TRIM(lv)// &
		   '.bin'
  WRITE(22,'(A)')'*'
  WRITE(22,'(A)')'OPTIONS SEQUENTIAL BIG_ENDIAN'
  WRITE(22,'(A)')'*'
  WRITE(22,'(A)')'UNDEF -2.56E33'
  WRITE(22,'(A)')'*'
  WRITE(22,'(A)')'TITLE '//dvar(var)
  WRITE(22,'(A)')'*'
  WRITE(22,'(A,I4,A)')'XDEF ',npmx,' LINEAR 1 1'
  WRITE(22,'(A,I4,A,F12.4,F12.4)')'YDEF ',ndiv,' LINEAR ', vmin, delta
  WRITE(22,'(A,I4,A)')'ZDEF ',1,' LINEAR 1000 -10'
  WRITE(22,'(A,I4,A,I3,A)') 'TDEF ',ndt,' LINEAR '//hor//':00Z'// &
      dia//cmes(imes)//ano//' 1HR'
  WRITE(22,'(A)')'*'
  WRITE(22,'(A,I5)')'VARS ',1
  WRITE(22,'(A)') 'prob 0 99 probabilidade'
  WRITE(22,'(A)')'ENDVARS'
  CLOSE(22)

 
END SUBROUTINE writectl_out
