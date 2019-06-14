      SUBROUTINE SYMG0(NXSY,NSY,NEND1,LMAX,KLMX,NMD,IPR,NCUTS,
     *                 EPS,TWOMG,BETA,GAMA,
     *                 WS,SDG,DGL,ES,XS,XX)
C*
      INTEGER NXSY,NSY,NEND1,LMAX,KLMX,NMD,IPR,NCUTS
      REAL EPS,TWOMG
      REAL BETA(*),GAMA(*)
      REAL WS(*),SDG(*),DGL(*)
      REAL ES(NXSY,*),XS(NXSY,*),XX(NEND1,*)
C*
      INTEGER N,NMX,NN,J,JJ,IERR
      REAL ZERO,ONE
      DATA ZERO /0.0E0/, ONE /1.0E0/
C*
C*    SYMMETRIC CASE
C*
      NMX=1
      N=1
      SDG(N)=ZERO
      NN=2*N+1
      DGL(N)=BETA(NN)*BETA(NN)+BETA(NN+1)*BETA(NN+1)+
     *       GAMA(NN)*GAMA(NN)
      NMX=LMAX-1
      DO 30 N=2,NMX
      NN=2*N+1
      SDG(N)=BETA(NN-1)*BETA(NN)
      DGL(N)=BETA(NN)*BETA(NN)+BETA(NN+1)*BETA(NN+1)+
     *       GAMA(NN)*GAMA(NN)
   30 CONTINUE
      IF (NMD .EQ. 0) THEN
      NMX=LMAX
      N=LMAX
      NN=2*N+1
      SDG(N)=BETA(NN-1)*BETA(NN)
      DGL(N)=BETA(NN)*BETA(NN)+GAMA(NN)*GAMA(NN)
      ENDIF
C*
      IF (IPR .GE. 1) THEN
      WRITE(*,*)' '
      WRITE(*,*)' SDG:'
      WRITE(*,'(1P,6G12.5)')(SDG(N),N=1,NMX)
      WRITE(*,*)' DGS:'
      WRITE(*,'(1P,6G12.5)')(DGL(N),N=1,NMX)
      ENDIF
C*
      CALL IDENT(NEND1,NMX,XX)
      CALL TQL2(NEND1,NMX,DGL,SDG,XX,EPS,IERR)
C*
      DO J=1,NMX
      JJ=2*J
      WS(JJ-1)=-SQRT(DGL(J))
      WS(JJ)=SQRT(DGL(J))
      XS(1,JJ-1)=BETA(3)*XX(1,J)/WS(JJ-1)
      XS(1,JJ)=-XS(1,JJ-1)
      XS(LMAX+1,JJ-1)=0.0
      XS(LMAX+1,JJ)=XS(LMAX+1,JJ-1)
      XS(KLMX+1,JJ-1)=0.0
      XS(KLMX+1,JJ)=-XS(KLMX+1,JJ-1)
      DO N=2,NMX
      NN=2*N+1
      XS(N,JJ-1)=(BETA(NN-1)*XX(N-1,J)+BETA(NN)*XX(N,J))/WS(JJ-1)
      XS(N,JJ)=-XS(N,JJ-1)
      XS(LMAX+N,JJ-1)=XX(N-1,J)
      XS(LMAX+N,JJ)=XS(LMAX+N,JJ-1)
      XS(KLMX+N,JJ-1)=GAMA(NN-2)*XX(N-1,J)/WS(JJ-1)
      XS(KLMX+N,JJ)=-XS(KLMX+N,JJ-1)
      ENDDO
      N=LMAX
      NN=2*N-1
      IF (NMD .EQ. 1) THEN
      XS(N,JJ-1)=BETA(NN+1)*XX(N-1,J)/WS(JJ-1)
      XS(N,JJ)=-XS(N,JJ-1)
      XS(KLMX,JJ-1)=XX(N-1,J)
      XS(KLMX,JJ)=XS(KLMX,JJ-1)
      XS(NSY,JJ-1)=GAMA(NN)*XX(N-1,J)/WS(JJ-1)
      XS(NSY,JJ)=-XS(NSY,JJ-1)
      ELSE
      XS(KLMX,JJ-1)=XX(N,J)
      XS(KLMX,JJ)=XS(KLMX,JJ-1)
      XS(NSY,JJ-1)=GAMA(NN+2)*XX(N,J)/WS(JJ-1)
      XS(NSY,JJ)=-XS(NSY,JJ-1)
      ENDIF
      ENDDO
C*
      NCUTS=2*NMX
      CALL FILTER(NXSY,NSY,NCUTS,XS,ZERO,EPS)
C*
      WRITE(*,*)' '
      WRITE(*,*)' FREQUENCY: NSY=',NSY,' NCUTS=',NCUTS
      WRITE(*,'(1P,6G12.5)')(WS(N)/TWOMG,N=1,NCUTS)
      WRITE(*,*)' PERIOD:'
      WRITE(*,'(1P,6G12.5)')(ONE/WS(N),N=1,NCUTS)
C*
      IF (IPR .GE. 1) THEN
      WRITE(*,*)' '
      WRITE(*,*)' XS: IERR=',IERR
      DO NN=1,NMX
      WRITE(*,'(1P,6G12.5)')(XX(NN,N),N=1,NMX)
      ENDDO
      ENDIF
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
