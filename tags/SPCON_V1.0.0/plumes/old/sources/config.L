SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:27:52 2005
FILE NAME: config.f90
PROGRAM NAME: config
DIAGNOSTIC LIST

  LINE  LEVEL( NO.): DIAGNOSTIC MESSAGE

    94  vec  (   3): Unvectorized loop.
   100  vec  (   3): Unvectorized loop.
   110  vec  (   3): Unvectorized loop.
SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:27:52 2005
FILE NAME: config.f90
PROGRAM NAME: config
FORMAT LIST

  LINE    LOOP     FORTRAN STATEMENT

     1:            MODULE config
     2:              IMPLICIT NONE
     3:              
     4:              INTEGER           :: nmemb
     5:              CHARACTER(LEN=5)  :: trunc
     6:              INTEGER           :: levels
     7:              CHARACTER(LEN=10) :: labeli
     8:              CHARACTER(LEN=10) :: labelf
     9:              INTEGER           :: npmx
    10:              INTEGER           :: nfsf
    11:              INTEGER           :: nfkm
    12:              INTEGER           :: imax
    13:              INTEGER           :: jmax
    14:              CHARACTER(LEN=95) :: dataindir 
    15:              CHARACTER(LEN=95) :: dataoutdir 
    16:              CHARACTER(LEN=95) :: datadirunt 
    17:              CHARACTER(LEN=10) :: namef_pre
    18:              CHARACTER(LEN=10) :: nameh_pre
    19:              CHARACTER(LEN=10) :: namem_pre
    20:              CHARACTER(LEN=10) :: extp
    21:              CHARACTER(LEN=10) :: exth
    22:              CHARACTER(LEN=10) :: exdh
    23:              CHARACTER(LEN=10) :: extm
    24:              CHARACTER(LEN=10) :: extc
    25:              CHARACTER(LEN=95) :: unitfile
    26:              INTEGER           :: delt
    27:              INTEGER           :: dofprev
    28:              INTEGER           :: divbyvar
    29:              INTEGER           :: deltout
    30:              INTEGER           :: nthreads
    31:              CHARACTER(LEN=12) :: guia
    32:              CHARACTER(LEN=200) :: filen(27:100)
    33:              INTEGER :: nfiles
    34:              
    35:              CONTAINS 
    36:              
    37:              SUBROUTINE le_config()
    38:                USE geral
    39:              
    40:                OPEN(UNIT=1,FILE='config',STATUS='old')
    41:              
    42:                READ(1,FMT='(A12,I3)',ERR=100)  guia,nmemb
    43:                READ(1,FMT='(A12,A5)',ERR=100)  guia,trunc
    44:                READ(1,FMT='(A12,I2)',ERR=100)  guia,levels
    45:                READ(1,FMT='(A12,A10)',ERR=100) guia,labeli
    46:                READ(1,FMT='(A12,A10)',ERR=100) guia,labelf
    47:                READ(1,FMT='(A12,I4)',ERR=100)  guia,npmx
    48:                READ(1,FMT='(A12,I4)',ERR=100)  guia,nfsf
    49:                READ(1,FMT='(A12,I4)',ERR=100)  guia,nfkm
    50:                READ(1,FMT='(A12,I4)',ERR=100)  guia,imax
    51:                READ(1,FMT='(A12,I4)',ERR=100)  guia,jmax 
    52:                READ(1,FMT='(A12,A90)',ERR=100) guia,dataindir
    53:                READ(1,FMT='(A12,A90)',ERR=100) guia,dataoutdir
    54:                READ(1,FMT='(A12,A90)',ERR=100) guia,datadirunt
    55:                READ(1,FMT='(A12,A10)',ERR=100) guia,namef_pre
    56:                READ(1,FMT='(A12,A10)',ERR=100) guia,nameh_pre
    57:                READ(1,FMT='(A12,A10)',ERR=100) guia,namem_pre
    58:                READ(1,FMT='(A12,A10)',ERR=100) guia,extp
    59:                READ(1,FMT='(A12,A10)',ERR=100) guia,exth
    60:                READ(1,FMT='(A12,A10)',ERR=100) guia,exdh
    61:                READ(1,FMT='(A12,A10)',ERR=100) guia,extm
    62:                READ(1,FMT='(A12,A10)',ERR=100) guia,extc
    63:                READ(1,FMT='(A12,A90)',ERR=100) guia,unitfile
    64:                READ(1,FMT='(A12,I4)' ,ERR=100) guia,delt
    65:                READ(1,FMT='(A12,I4)' ,ERR=100) guia,dofprev
    66:                READ(1,FMT='(A12,I4)' ,ERR=100) guia,divbyvar
    67:                READ(1,FMT='(A12,I4)' ,ERR=100) guia,deltout
    68:                READ(1,FMT='(A12,I4)' ,ERR=100) guia,nthreads
    69:              
    70:                CLOSE(UNIT=1)
    71:                
    72:                RETURN
    73:                
    74:            100 PRINT *,Error('Erro na leitura do config',61,'config',2)
    75:                
    76:              END SUBROUTINE le_config
    77:              
    78:              SUBROUTINE filename(bvar,nvar)
    79:                IMPLICIT NONE
    80:                
    81:                INTEGER,INTENT(IN) :: nvar
    82:                CHARACTER(LEN=*) :: bvar(nvar)
    83:                INTEGER :: i,nf
    84:                CHARACTER(LEN=2) :: na,lv
    85:                
    86:                WRITE(lv,FMT='(I2.2)') levels
    87:                    
    88:                filen(27)=TRIM(datadirunt)//'/'//TRIM(unitfile) !aunits
    89:                filen(28)=TRIM(dataindir)//'/'//TRIM(nameh_pre)//'AVN'//labeli//labelf// &
    90:                           TRIM(exdh)//'.'//TRIM(trunc)//'L'//TRIM(lv)
    91:                filen(29)=TRIM(dataindir)//'/'//TRIM(namef_pre)//'AVN'//labeli//labeli// &
    92:                           TRIM(extp)//'.'//TRIM(trunc)//'L'//TRIM(lv)
    93:                nf=29
    94: +------>       DO i=1,(nmemb-1)/2
    95: |                 WRITE(na,FMT='(i2.2)') i
    96: |                 nf=nf+1
    97: |                 filen(nf)=TRIM(dataindir)//'/'//TRIM(nameh_pre)//na//'P'//labeli// &
    98: |                             labelf//TRIM(exth)//'.'//TRIM(trunc)//'L'//TRIM(lv) 
    99: +------        END DO
   100: +------>       DO i=1,(nmemb-1)/2
   101: |                 WRITE(na,FMT='(i2.2)') i
   102: |                 nf=nf+1
   103: |                 filen(nf)=TRIM(dataindir)//'/'//TRIM(nameh_pre)//na//'N'//labeli// &
   104: |                             labelf//TRIM(exth)//'.'//TRIM(trunc)//'L'//TRIM(lv) 
   105: +------        END DO
   106:                nf =nf+1
   107:                filen(nf)=TRIM(dataindir)//'/'//TRIM(nameh_pre)//'AVN'//labeli// &
   108:                               labelf//TRIM(exth)//'.'//TRIM(trunc)//'L'//TRIM(lv)
   109:            
   110: +------>       DO i=1,nvar
   111: |                nf=nf+1
   112: |                filen(nf)=TRIM(dataoutdir)//'/'//bvar(i)//labeli// &
   113: |                             labelf//'.'//TRIM(trunc)//'L'//TRIM(lv)// &
   114: |          		   '.bin'
   115: +------        END DO
   116:                nf=nf+1
   117:                filen(nf)=TRIM(dataoutdir)//'/'//'CONT'//labeli// &
   118:                               labelf//'.'//TRIM(trunc)//'L'//TRIM(lv)// &
   119:            		   '.bin'
   120:            
   121:                filen(95)=TRIM(dataoutdir)//'/'//'LOCMM'//labeli// &
   122:                               labelf//'.'//TRIM(trunc)//'L'//TRIM(lv)
   123:                
   124:                nfiles=nf		   
   125:            		    
   126:              END SUBROUTINE filename
   127:              
   128:                
   129:                
   130:              
   131:            END MODULE config
