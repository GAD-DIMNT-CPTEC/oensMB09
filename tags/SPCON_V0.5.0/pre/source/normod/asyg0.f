      SUBROUTINE ASYG0(NXAS,NAS,NEND1,LMAX,KLMX,MMAX,NMD,IPR,
     *                 NCUTA,EPS,TWOMG,BETA,GAMA,
     *                 WA,SDG,DGL,EA,XA,XX)
C*
      INTEGER NXAS,NAS,NEND1,LMAX,KLMX,MMAX,NMD,IPR,NCUTA
      REAL EPS,TWOMG
      REAL BETA(*),GAMA(*)
      REAL WA(*),SDG(*),DGL(*)
      REAL EA(NXAS,*),XA(NXAS,*),XX(NEND1,*)
C*
      INTEGER N,NMX,NN,J,JJ,IERR
      REAL ZERO,ONE
      DATA ZERO /0.0E0/, ONE /1.0E0/
C*
C*    ASYMMETRIC CASE
C*
      NMX=1
      N=1
      SDG(N)=ZERO
      NN=2*N
      SDG(N)=BETA(NN-1)*BETA(NN)
      DGL(N)=BETA(NN)*BETA(NN)+BETA(NN+1)*BETA(NN+1)+
     *       GAMA(NN)*GAMA(NN)
      NMX=LMAX-1
      DO 40 N=2,NMX
      NN=2*N
      SDG(N)=BETA(NN-1)*BETA(NN)
      DGL(N)=BETA(NN)*BETA(NN)+BETA(NN+1)*BETA(NN+1)+
     *       GAMA(NN)*GAMA(NN)
   40 CONTINUE
      NMX=LMAX
      N=LMAX
      NN=2*N
      SDG(N)=BETA(NN-1)*BETA(NN)
      IF (NMD .EQ. 0) THEN
      DGL(N)=BETA(NN)*BETA(NN)+BETA(NN+1)*BETA(NN+1)+
     *       GAMA(NN)*GAMA(NN)
      ELSE
      DGL(N)=BETA(NN)*BETA(NN)+GAMA(NN)*GAMA(NN)
      ENDIF
C*
      IF (IPR .GE. 1) THEN
      WRITE(*,*)' '
      WRITE(*,*)' SDG:'
      WRITE(*,'(1P,6G12.5)')(SDG(N),N=1,NMX)
      WRITE(*,*)' DGA:'
      WRITE(*,'(1P,6G12.5)')(DGL(N),N=1,NMX)
      ENDIF
C*
      CALL IDENT(NEND1,NMX,XX)
      CALL TQL2(NEND1,NMX,DGL,SDG,XX,EPS,IERR)
C*
      DO J=1,NMX
      JJ=2*J
      WA(JJ-1)=-SQRT(DGL(J))
      WA(JJ)=SQRT(DGL(J))
      XA(1,JJ-1)=0.0
      XA(1,JJ)=-XA(1,JJ-1)
      XA(MMAX+1,JJ-1)=XX(1,J)
      XA(MMAX+1,JJ)=XA(MMAX+1,JJ-1)
      XA(KLMX+1,JJ-1)=GAMA(2)*XX(1,J)/WA(JJ-1)
      XA(KLMX+1,JJ)=-XA(KLMX+1,JJ-1)
      DO N=2,NMX
      NN=2*N
      XA(N,JJ-1)=(BETA(NN-1)*XX(N-1,J)+BETA(NN)*XX(N,J))/WA(JJ-1)
      XA(N,JJ)=-XA(N,JJ-1)
      XA(MMAX+N,JJ-1)=XX(N,J)
      XA(MMAX+N,JJ)=XA(MMAX+N,JJ-1)
      XA(KLMX+N,JJ-1)=GAMA(NN)*XX(N,J)/WA(JJ-1)
      XA(KLMX+N,JJ)=-XA(KLMX+N,JJ-1)
      ENDDO
      N=NMX
      NN=2*N+1
      IF (NMD .EQ. 0) THEN
      XA(MMAX,JJ-1)=BETA(NN)*XX(N,J)/WA(JJ-1)
      XA(MMAX,JJ)=-XA(MMAX,JJ-1)
      ENDIF
      ENDDO
C*
      NCUTA=2*NMX
      CALL FILTER(NXAS,NAS,NCUTA,XA,ZERO,EPS)
C*
      WRITE(*,*)' '
      WRITE(*,*)' FREQUENCY: NAS=',NAS,' NCUTA=',NCUTA
      WRITE(*,'(1P,6G12.5)')(WA(N)/TWOMG,N=1,NCUTA)
      WRITE(*,*)' PERIOD:'
      WRITE(*,'(1P,6G12.5)')(ONE/WA(N),N=1,NCUTA)
C*
      IF (IPR .GE. 1) THEN
      WRITE(*,*)' '
      WRITE(*,*)' XA: IERR=',IERR
      DO N=1,NMX
      WRITE(*,'(1P,6G12.5)')(XX(N,NN),NN=1,NMX)
      ENDDO
      ENDIF
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
