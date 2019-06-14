      SUBROUTINE SSTWIN(JN,JS)
C*
      INTEGER ISST,JSST
      PARAMETER (ISST=360,JSST=180)
      INTEGER JN,JS,J
      REAL ALATS,ALATN,ALAT,DLAT
C*
C*    USE CLIMATOLOGICAL SST OUT OF 50S TO 60N
C*
      DATA ALATS/-50.0/, ALATN/60.0/
C*
      DLAT=180.0/FLOAT(JSST)
      DO J=1,JSST
      ALAT=89.5-FLOAT(J-1)*DLAT
      IF (ALAT .GT. ALATS) JS=J
      IF (ALAT .GT. ALATN) JN=J
      ENDDO
C*
      JS=JS+1
      WRITE(*,*)' '
      WRITE(*,*)' FROM SSTWIN:'
      WRITE(*,'(A,I3,A,F7.3)')
     *        ' JS = ',JS,' LATS=',89.5-FLOAT(JS-1)*DLAT
      WRITE(*,'(A,I3,A,F7.3)')
     *        ' JN = ',JN,' LATN=',89.5-FLOAT(JN-1)*DLAT
      WRITE(*,*)' '
C*
      RETURN
      END
