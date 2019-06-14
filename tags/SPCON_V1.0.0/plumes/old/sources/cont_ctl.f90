SUBROUTINE cont_ctl(npmp,bvar,nvar,dvar,lvarc,uvarc,ndt,aunits)
  USE config
  USE geral
  IMPLICIT NONE
  
  INTEGER,INTENT(IN) :: nvar,npmp
  CHARACTER(LEN=*),INTENT(IN) :: bvar(nvar)
  CHARACTER(LEN=*),INTENT(IN) :: dvar(nvar)
  CHARACTER(LEN=*),INTENT(IN) :: aunits(nvar)
  INTEGER,INTENT(IN) :: ndt
  INTEGER,INTENT(IN) :: lvarc(nvar),uvarc(nvar)
  
  CHARACTER(LEN=2) :: mes,dia,hor,lv
  CHARACTER(LEN=4) :: ano
  INTEGER :: imes,j,nvarm1
  CHARACTER(LEN=3),PARAMETER,DIMENSION(12) :: cmes= &
                                     (/'JAN','FEB','MAR','APR','MAY','JUN',  &
                                       'JUL','AUG','SEP','OCT','NOV','DEC'/)
  CHARACTER(LEN=200) :: filenam
  
  nvarm1=nvar-1

  WRITE(lv,FMT='(I2.2)') levels
  
  ano=labeli(01:04)
  mes=labeli(05:06)
  dia=labeli(07:08)
  hor=labeli(09:10)
  imes=asc2int(mes)

  filenam=TRIM(dataoutdir)//'/CONT'//labeli// &
                   labelf//'.'//TRIM(trunc)//'L'//TRIM(lv)// &
		   '.ctl'
  print *,'Saida: ',filenam
  OPEN(22,FILE=TRIM(filenam),STATUS='UNKNOWN',FORM='FORMATTED')

  WRITE(22,'(A)')'DSET ^CONT'//labeli// &
                   labelf//'.'//TRIM(trunc)//'L'//TRIM(lv)// &
		   '.bin'
  WRITE(22,'(A)')'*'
  WRITE(22,'(A)')'OPTIONS SEQUENTIAL BIG_ENDIAN'
  WRITE(22,'(A)')'*'

  WRITE(22,'(A)')'UNDEF -2.56E33'
  WRITE(22,'(A)')'*'
  WRITE(22,'(A)')'TITLE GRID POINT HISTORY'
  WRITE(22,'(A)')'*'
  WRITE(22,'(A,I4,A)')'XDEF ',npmp,' LINEAR 1 1'
  WRITE(22,'(A,I4,A)')'YDEF ',1,' LINEAR 1 1'
  WRITE(22,'(A,I4,A)')'ZDEF ',1,' LINEAR 1000 -10'
  WRITE(22,'(A,I3,7A)') &
  	   'TDEF ',ndt,' LINEAR ',hor,':00Z',&
  	    dia,cmes(imes),ano,' 1HR'
  WRITE(22,'(A)')'*'
  WRITE(22,'(A,I5)')'VARS ',nvarm1
  DO J=1,nvarm1
    write(22,'(a,i3,5a)') &
  	     bvar(j),lvarc(j),' 99 ',dvar(j), &
  	     '(',aunits(uvarc(j)),')'
  ENDDO
  WRITE(22,'(A)')'ENDVARS'
 
  CLOSE(22)

 
END SUBROUTINE cont_ctl
