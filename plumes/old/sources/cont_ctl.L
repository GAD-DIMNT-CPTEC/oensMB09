SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:28:02 2005
FILE NAME: cont_ctl.f90
PROGRAM NAME: cont_ctl
DIAGNOSTIC LIST

  LINE  LEVEL( NO.): DIAGNOSTIC MESSAGE

    56  vec  (   2): Partially vectorized loop.
SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:28:02 2005
FILE NAME: cont_ctl.f90
PROGRAM NAME: cont_ctl
FORMAT LIST

  LINE    LOOP     FORTRAN STATEMENT

     1:            SUBROUTINE cont_ctl(npmp,bvar,nvar,dvar,lvarc,uvarc,ndt,aunits)
     2:              USE config
     3:              USE geral
     4:              IMPLICIT NONE
     5:              
     6:              INTEGER,INTENT(IN) :: nvar,npmp
     7:              CHARACTER(LEN=*),INTENT(IN) :: bvar(nvar)
     8:              CHARACTER(LEN=*),INTENT(IN) :: dvar(nvar)
     9:              CHARACTER(LEN=*),INTENT(IN) :: aunits(nvar)
    10:              INTEGER,INTENT(IN) :: ndt
    11:              INTEGER,INTENT(IN) :: lvarc(nvar),uvarc(nvar)
    12:              
    13:              CHARACTER(LEN=2) :: mes,dia,hor,lv
    14:              CHARACTER(LEN=4) :: ano
    15:              INTEGER :: imes,j,nvarm1
    16:              CHARACTER(LEN=3),PARAMETER,DIMENSION(12) :: cmes= &
    17:                                                 (/'JAN','FEB','MAR','APR','MAY','JUN',  &
    18:                                                   'JUL','AUG','SEP','OCT','NOV','DEC'/)
    19:              CHARACTER(LEN=200) :: filenam
    20:              
    21:              nvarm1=nvar-1
    22:            
    23:              WRITE(lv,FMT='(I2.2)') levels
    24:              
    25:              ano=labeli(01:04)
    26:              mes=labeli(05:06)
    27:              dia=labeli(07:08)
    28:              hor=labeli(09:10)
    29:              imes=asc2int(mes)
    30:            
    31:              filenam=TRIM(dataoutdir)//'/CONT'//labeli// &
    32:                               labelf//'.'//TRIM(trunc)//'L'//TRIM(lv)// &
    33:            		   '.ctl'
    34:              print *,'Saida: ',filenam
    35:              OPEN(22,FILE=TRIM(filenam),STATUS='UNKNOWN',FORM='FORMATTED')
    36:            
    37:              WRITE(22,'(A)')'DSET ^CONT'//labeli// &
    38:                               labelf//'.'//TRIM(trunc)//'L'//TRIM(lv)// &
    39:            		   '.bin'
    40:              WRITE(22,'(A)')'*'
    41:              WRITE(22,'(A)')'OPTIONS SEQUENTIAL BIG_ENDIAN'
    42:              WRITE(22,'(A)')'*'
    43:            
    44:              WRITE(22,'(A)')'UNDEF -2.56E33'
    45:              WRITE(22,'(A)')'*'
    46:              WRITE(22,'(A)')'TITLE GRID POINT HISTORY'
    47:              WRITE(22,'(A)')'*'
    48:              WRITE(22,'(A,I4,A)')'XDEF ',npmp,' LINEAR 1 1'
    49:              WRITE(22,'(A,I4,A)')'YDEF ',1,' LINEAR 1 1'
    50:              WRITE(22,'(A,I4,A)')'ZDEF ',1,' LINEAR 1000 -10'
    51:              WRITE(22,'(A,I3,7A)') &
    52:              	   'TDEF ',ndt,' LINEAR ',hor,':00Z',&
    53:              	    dia,cmes(imes),ano,' 1HR'
    54:              WRITE(22,'(A)')'*'
    55:              WRITE(22,'(A,I5)')'VARS ',nvarm1
    56: V------>     DO J=1,nvarm1
    57: |       S      write(22,'(a,i3,5a)') &
    58: |            	     bvar(j),lvarc(j),' 99 ',dvar(j), &
    59: |            	     '(',aunits(uvarc(j)),')'
    60: V------      ENDDO
    61:              WRITE(22,'(A)')'ENDVARS'
    62:             
    63:              CLOSE(22)
    64:            
    65:             
    66:            END SUBROUTINE cont_ctl
