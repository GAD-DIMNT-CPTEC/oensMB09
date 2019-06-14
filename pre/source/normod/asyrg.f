      SUBROUTINE ASYRG(NXAS,NAS,LMAX,MMAX,NMD,IPR,NCUTA,
     *                 EPS,TWOMG,PERCUT,ALFA,BETA,GAMA,
     *                 WA,WK,EA,XA)
C*
      INTEGER NXAS,NAS,LMAX,MMAX,NMD,IPR,NCUTA
      REAL EPS,TWOMG,PERCUT
      REAL ALFA(*),BETA(*),GAMA(*)
      REAL WA(*),WK(*)
      REAL EA(NXAS,*),XA(NXAS,*)
C*
      INTEGER N,NN,MM,JJ,IERR
      REAL ZERO,ONE
      DATA ZERO /0.0E0/, ONE /1.0E0/
C*
C*    ASYMMETRIC CASE
C*
      CALL RESET(NXAS*NXAS,EA,ZERO)
      CALL RESET(NXAS,WA,ZERO)
      CALL RESET(NXAS,WK,ZERO)
C*
      DO N=1,MMAX
      EA(N,N)=ALFA(2*N-1)
      ENDDO
      DO N=1,LMAX
      NN=2*N
      JJ=MMAX+N
      EA(N,JJ)=BETA(NN)
      EA(JJ,N)=EA(N,JJ)
      IF (N.EQ.LMAX .AND. NMD.EQ.1) GOTO 99
      EA(N+1,JJ)=BETA(NN+1)
      EA(JJ,N+1)=EA(N+1,JJ)
      ENDDO
   99 CONTINUE
      DO N=1,LMAX
      NN=2*N
      JJ=MMAX+N
      EA(JJ,JJ)=ALFA(NN)
      MM=JJ+LMAX
      EA(JJ,MM)=GAMA(NN)
      EA(MM,JJ)=EA(JJ,MM)
      ENDDO
C*
      IF (IPR .GE. 3) THEN
      WRITE(*,*)' '
      WRITE(*,*)' EA:'
      DO N=1,NAS
      WRITE(*,'(1P,6G12.5)')(EA(N,NN),NN=1,NAS)
      ENDDO
      ENDIF
C*
      CALL TRED2(NXAS,NAS,EA,WA,WK,XA)
      CALL TQL2(NXAS,NAS,WA,WK,XA,EPS,IERR)
C*
      IF (IPR .GE. 1) THEN
      WRITE(*,*)' '
      WRITE(*,*)' WA: IERR=',IERR
      WRITE(*,'(1P,6G12.5)')(WA(N),N=1,NAS)
      ENDIF
C*
C*    REORDERING FREQUENCIES
C*
      CALL ORDER(NXAS,NAS,WA,WK,XA,EA,PERCUT,NCUTA)
      CALL FILTER(NXAS,NAS,NCUTA,XA,ZERO,EPS)
C*
      WRITE(*,*)' '
      WRITE(*,*)' FREQUENCY: NAS=',NAS,' NCUTA=',NCUTA
      WRITE(*,'(1P,6G12.5)')(WA(N)/TWOMG,N=1,NCUTA)
      WRITE(*,*)' PERIOD:'
      WRITE(*,'(1P,6G12.5)')(ONE/WA(N),N=1,NCUTA)
C*
      IF (IPR .GE. 0) THEN
      WRITE(*,*)' '
      WRITE(*,*)' XA:'
      DO N=1,NAS
      WRITE(*,'(1P,6G12.5)')(XA(N,NN),NN=1,MIN(6,NCUTA))
      ENDDO
      ENDIF
C*
      RETURN
      END
