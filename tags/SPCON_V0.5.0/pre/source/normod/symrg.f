      SUBROUTINE SYMRG(NXSY,NSY,LMAX,MMAX,NMD,IPR,NCUTS,
     *                 EPS,TWOMG,PERCUT,ALFA,BETA,GAMA,
     *                 WS,WK,ES,XS)
C*
      INTEGER NXSY,NSY,LMAX,MMAX,NMD,IPR,NCUTS
      REAL EPS,TWOMG,PERCUT
      REAL ALFA(*),BETA(*),GAMA(*)
      REAL WS(*),WK(*)
      REAL ES(NXSY,*),XS(NXSY,*)
C*
      INTEGER N,NN,MM,JJ,IERR
      REAL ZERO,ONE
      DATA ZERO /0.0E0/, ONE /1.0E0/
C*
C*    SYMMETRIC CASE
C*
      CALL RESET(NXSY*NXSY,ES,ZERO)
      CALL RESET(NXSY,WS,ZERO)
      CALL RESET(NXSY,WK,ZERO)
C*
      DO N=1,LMAX
      ES(N,N)=ALFA(2*N)
      ENDDO
      DO N=1,LMAX
      NN=2*N
      JJ=LMAX+N
      ES(N,JJ)=BETA(NN)
      ES(JJ,N)=ES(N,JJ)
      IF (N.EQ.LMAX .AND. NMD.EQ.1) GOTO 98
      ES(N,JJ+1)=BETA(NN+1)
      ES(JJ+1,N)=ES(N,JJ+1)
      ENDDO
   98 CONTINUE
      DO N=1,MMAX
      NN=2*N-1
      JJ=LMAX+N
      ES(JJ,JJ)=ALFA(NN)
      MM=JJ+MMAX
      ES(JJ,MM)=GAMA(NN)
      ES(MM,JJ)=ES(JJ,MM)
      ENDDO
C*
      IF (IPR .GE. 3) THEN
      WRITE(*,*)' '
      WRITE(*,*)' ES:'
      DO N=1,NSY
      WRITE(*,'(1P,6G12.5)')(ES(N,NN),NN=1,NSY)
      ENDDO
      ENDIF
C*
      CALL TRED2(NXSY,NSY,ES,WS,WK,XS)
      CALL TQL2(NXSY,NSY,WS,WK,XS,EPS,IERR)
C*
      IF (IPR .GE. 1) THEN
      WRITE(*,*)' '
      WRITE(*,*)' WS: IERR=',IERR
      WRITE(*,'(1P,6G12.5)')(WS(N),N=1,NSY)
      ENDIF
C*
C*    REORDERING FREQUENCIES
C*
      CALL ORDER(NXSY,NSY,WS,WK,XS,ES,PERCUT,NCUTS)
      CALL FILTER(NXSY,NSY,NCUTS,XS,ZERO,EPS)
C*
      WRITE(*,*)' '
      WRITE(*,*)' FREQUENCY: NSY=',NSY,' NCUTS=',NCUTS
      WRITE(*,'(1P,6G12.5)')(WS(N)/TWOMG,N=1,NCUTS)
      WRITE(*,*)' PERIOD:'
      WRITE(*,'(1P,6G12.5)')(ONE/WS(N),N=1,NCUTS)
C*
      IF (IPR .GE. 0) THEN
      WRITE(*,*)' '
      WRITE(*,*)' XS:'
      DO N=1,NSY
      WRITE(*,'(1P,6G12.5)')(XS(N,NN),NN=1,MIN(6,NCUTS))
      ENDDO
      ENDIF
C*
      RETURN
      END
