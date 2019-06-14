SUBROUTINE Chek_Plumas(loc,dt,nmemb,ndiv,var,prob,cloc,loc_s,vmax,vmin,cvar)
  !Only to verify results
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: loc,dt,nmemb,ndiv,loc_s
  REAL,INTENT(IN) :: var(loc,nmemb,dt)
  REAL,INTENT(IN) :: prob(loc,ndiv,dt)
  CHARACTER(LEN=*),INTENT(IN) :: cloc
  CHARACTER(LEN=*),INTENT(IN) :: cvar
  REAL,INTENT(IN) :: vmax,vmin
  REAL :: delta
  INTEGER :: i,j,k
    
  delta=(vmax-vmin)/ndiv
  WRITE (*,'(A)')    '--------------Verificacao de Resultados------------------'
  WRITE (*,'(A,I3,A)') 'Local : ',loc_s,' - '//cloc
  WRITE (*,'(A,A)') 'Variavel: ',cvar
  WRITE (*,'(A,F12.2,A,F12.2)') 'Minimo: ',vmin,' - Delta: ',delta
  WRITE (*,'(A)')    '--------- Faixas --------'
  DO i=1,ndiv
     WRITE (*,'(F7.2,A,$)') vmin+(i-1)*delta,' '
  END DO
  WRITE(*,'(A)') ' '
  WRITE (*,'(A)')    '--------- Membros/Probabilidade --------'
  DO j=1,dt
    WRITE (*,'(I3,A,$)') j,' - '
    DO i=1,nmemb
       WRITE (*,'(F12.2,A,$)') var(loc_s,i,j),' '
    END DO
    WRITE (*,'(A)') ' '
    DO k=1,ndiv
       WRITE (*,'(F4.1,A,$)') prob(loc_s,k,j)*100,' '
    END DO
    WRITE (*,'(A)') ' '
  END DO

  
END SUBROUTINE Chek_Plumas
