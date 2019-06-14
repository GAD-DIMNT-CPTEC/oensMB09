SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:27:54 2005
FILE NAME: wout_ctl.f90
PROGRAM NAME: writectl_out
FORMAT LIST

  LINE    LOOP     FORTRAN STATEMENT

     1:            SUBROUTINE writectl_out(bvar,nvar,dvar,var,ndiv,vmax,vmin,ndt,typ)
     2:              USE config
     3:              USE geral
     4:              IMPLICIT NONE
     5:              
     6:              INTEGER,INTENT(IN) :: nvar
     7:              CHARACTER(LEN=*),INTENT(IN) :: bvar(nvar)
     8:              CHARACTER(LEN=*),INTENT(IN) :: dvar(nvar)
     9:              INTEGER,INTENT(IN) :: ndiv,ndt,var
    10:              INTEGER,INTENT(IN) :: typ !1 - prob, 0- prec
    11:              REAL,INTENT(IN) :: vmin,vmax
    12:              
    13:              CHARACTER(LEN=2) :: mes,dia,hor,lv
    14:              CHARACTER(LEN=4) :: ano
    15:              REAL :: delta
    16:              INTEGER :: imes
    17:              CHARACTER(LEN=3),PARAMETER,DIMENSION(12) :: cmes= &
    18:                                                 (/'JAN','FEB','MAR','APR','MAY','JUN',  &
    19:                                                   'JUL','AUG','SEP','OCT','NOV','DEC'/)
    20:              CHARACTER(LEN=200) :: filenam
    21:              
    22:              IF(typ==1) THEN
    23:                WRITE(lv,FMT='(I2.2)') levels
    24:            !AMM    delta=(vmax-vmin)/ndiv
    25:                delta=(vmax-vmin)/(ndiv-1) !Mudanca necessaria para resolver problema na umidadae relativa
    26:              ELSE
    27:                delta=1
    28:              END IF
    29:              ano=labeli(01:04)
    30:              mes=labeli(05:06)
    31:              dia=labeli(07:08)
    32:              hor=labeli(09:10)
    33:              imes=asc2int(mes)
    34:            
    35:              filenam=TRIM(dataoutdir)//'/'//bvar(var)//labeli// &
    36:                               labelf//'.'//TRIM(trunc)//'L'//TRIM(lv)// &
    37:            		   '.ctl'
    38:              print *,'Saida: ',filenam
    39:              OPEN(22,FILE=TRIM(filenam),STATUS='UNKNOWN',FORM='FORMATTED')
    40:            
    41:              WRITE(22,'(A)')'DSET ^'//bvar(var)//labeli// &
    42:                               labelf//'.'//TRIM(trunc)//'L'//TRIM(lv)// &
    43:            		   '.bin'
    44:              WRITE(22,'(A)')'*'
    45:              WRITE(22,'(A)')'OPTIONS SEQUENTIAL BIG_ENDIAN'
    46:              WRITE(22,'(A)')'*'
    47:              WRITE(22,'(A)')'UNDEF -2.56E33'
    48:              WRITE(22,'(A)')'*'
    49:              WRITE(22,'(A)')'TITLE '//dvar(var)
    50:              WRITE(22,'(A)')'*'
    51:              WRITE(22,'(A,I4,A)')'XDEF ',npmx,' LINEAR 1 1'
    52:              WRITE(22,'(A,I4,A,F12.4,F12.4)')'YDEF ',ndiv,' LINEAR ', vmin, delta
    53:              WRITE(22,'(A,I4,A)')'ZDEF ',1,' LINEAR 1000 -10'
    54:              WRITE(22,'(A,I4,A,I3,A)') 'TDEF ',ndt,' LINEAR '//hor//':00Z'// &
    55:                  dia//cmes(imes)//ano//' 1HR'
    56:              WRITE(22,'(A)')'*'
    57:              WRITE(22,'(A,I5)')'VARS ',1
    58:              IF(typ==1) THEN
    59:                WRITE(22,'(A)') 'prob 0 99 probabilidade'
    60:              ELSE IF(typ==2) THEN
    61:                WRITE(22,'(A)') 'neve 0 99 membros'
    62:              ELSE 
    63:                WRITE(22,'(A)') 'prec 0 99 membros'
    64:              END IF
    65:              WRITE(22,'(A)')'ENDVARS'
    66:              CLOSE(22)
    67:            
    68:             
    69:            END SUBROUTINE writectl_out
