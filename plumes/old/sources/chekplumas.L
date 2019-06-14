SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:27:59 2005
FILE NAME: chekplumas.f90
PROGRAM NAME: chek_plumas
DIAGNOSTIC LIST

  LINE  LEVEL( NO.): DIAGNOSTIC MESSAGE

    20  vec  (   3): Unvectorized loop.
    27  vec  (   2): Partially vectorized loop.
    31  vec  (   2): Partially vectorized loop.
SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:27:59 2005
FILE NAME: chekplumas.f90
PROGRAM NAME: chek_plumas
FORMAT LIST

  LINE    LOOP     FORTRAN STATEMENT

     1:            SUBROUTINE Chek_Plumas(loc,dt,nmemb,ndiv,var,prob,cloc,loc_s,vmax,vmin,cvar)
     2:              !Only to verify results
     3:              IMPLICIT NONE
     4:            
     5:              INTEGER,INTENT(IN) :: loc,dt,nmemb,ndiv,loc_s
     6:              REAL,INTENT(IN) :: var(loc,nmemb,dt)
     7:              REAL,INTENT(IN) :: prob(loc,ndiv,dt)
     8:              CHARACTER(LEN=*),INTENT(IN) :: cloc
     9:              CHARACTER(LEN=*),INTENT(IN) :: cvar
    10:              REAL,INTENT(IN) :: vmax,vmin
    11:              REAL :: delta
    12:              INTEGER :: i,j,k
    13:                
    14:              delta=(vmax-vmin)/ndiv
    15:              WRITE (*,'(A)')    '--------------Verificacao de Resultados------------------'
    16:              WRITE (*,'(A,I3,A)') 'Local : ',loc_s,' - '//cloc
    17:              WRITE (*,'(A,A)') 'Variavel: ',cvar
    18:              WRITE (*,'(A,F12.2,A,F12.2)') 'Minimo: ',vmin,' - Delta: ',delta
    19:              WRITE (*,'(A)')    '--------- Faixas --------'
    20: +------>     DO i=1,ndiv
    21: |               WRITE (*,'(F7.2,A,$)') vmin+(i-1)*delta,' '
    22: +------      END DO
    23:              WRITE(*,'(A)') ' '
    24:              WRITE (*,'(A)')    '--------- Membros/Probabilidade --------'
    25: +------>     DO j=1,dt
    26: |              WRITE (*,'(I3,A,$)') j,' - '
    27: |V----->       DO i=1,nmemb
    28: ||      S         WRITE (*,'(F12.2,A,$)') var(loc_s,i,j),' '
    29: |V-----        END DO
    30: |              WRITE (*,'(A)') ' '
    31: |V----->       DO k=1,ndiv
    32: ||      S         WRITE (*,'(F4.1,A,$)') prob(loc_s,k,j)*100,' '
    33: |V-----        END DO
    34: |              WRITE (*,'(A)') ' '
    35: +------      END DO
    36:            
    37:              
    38:            END SUBROUTINE Chek_Plumas
