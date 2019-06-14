      SUBROUTINE QREAD(N,QGZS,QLNP,QTMP,QDIV,QROT,QQ)
C*
C*    READS IN SPECTRAL COEFFICIENTS GLOBAL MODEL INITIAL CONDITION
C*
C*    ARGUMENT(DIMENSIONS)          DESCRIPTION
C*
C*    N                      INPUT: FORTRAN UNIT NUMBER FOR THE
C*                                  INITIAL CONDITION FILE
C*    GZ   (MNWV2)          OUTPUT: MODEL TERRAIN FIELD (SPECTRAL)
C*    QLNP (MNWV2)          OUTPUT: LOG OF SURFACE PRESSURE (SPECTRAL)
C*    QTMP (MNWV2,KMAX)     OUTPUT: TEMPERATURE (SPECTRAL)
C*    QDIV (MNWV2,KMAX)     OUTPUT: DIVERGENCE (SPECTRAL)
C*    QROT (MNWV2,KMAX)     OUTPUT: VORTICITY (SPECTRAL)
C*    QQ   (MNWV2,KQMAX     OUTPUT: SPECIFIC HUMIDITY (SPECTRAL)
C*
      INTEGER IMAX,JMAX,MEND,KMAX,LMAX
      PARAMETER (IMAX=384,JMAX=192,MEND=126,KMAX=28,LMAX=17)
C*
      INTEGER MEND1,MEND2,MNWV2
      PARAMETER(MEND1=MEND+1,MEND2=MEND+2,MNWV2=MEND1*MEND2)
      INTEGER NGAUS
      PARAMETER (NGAUS=IMAX*JMAX)
C*
      INTEGER N
      REAL QGZS(MNWV2),QLNP(MNWV2),QTMP(MNWV2,KMAX),
     *     QDIV(MNWV2,KMAX),QROT(MNWV2,KMAX),QQ(MNWV2,KMAX)
C*
      INTEGER K,MN
      INTEGER*4 IFDAY,IDATE(4),IDATEC(4)
      REAL*4 TOD,FHOUR,SI(KMAX+1),SL(KMAX),QWK(MNWV2)
      REAL*4 MASK(NGAUS)
C*
      COMMON /INDATA/ IFDAY,TOD,IDATE,IDATEC,SI,SL
C*
      write(*,*)'entrou qread'
CAMM      READ(N)IFDAY,TOD,IDATE,IDATEC,SI,SL
      READ(N)IFDAY,TOD,IDATE,IDATEC
      write(*,'(A)')' '
      WRITE(*,'(A,I5,A,F6.2)')' IFDAY = ',IFDAY,' TOD = ',TOD
      write(*,'(A,4I5)')' IDATE  =',IDATE
      write(*,'(A,4I5)')' IDATEC =',IDATEC
      write(*,'(A)')' SI :'
      write(*,'(8F10.7)')SI
      write(*,'(A)')' SL :'
      write(*,'(8F10.7)')SL
      write(*,'(A)')' '
C*
C*    read orography
      READ(N)QWK
      DO MN=1,MNWV2
      QGZS(MN)=QWK(MN)
      ENDDO
C*
C*    read land sea mask
      READ(N)MASK
C*
C*    read ln pressure 
      READ(N)QWK
      DO MN=1,MNWV2
      QLNP(MN)=QWK(MN)
      ENDDO
C*
C*    read divergence
      DO K=1,KMAX
      READ(N)QWK
      DO MN=1,MNWV2
      QDIV(MN,K)=QWK(MN)
      ENDDO
      ENDDO
C*
C*    read vorticity
      DO K=1,KMAX
      READ(N)QWK
      DO MN=1,MNWV2
      QROT(MN,K)=QWK(MN)
      ENDDO
      ENDDO
C*
C*    read specific humidity
      DO K=1,KMAX
      READ(N)QWK
      DO MN=1,MNWV2
      QQ(MN,K)=QWK(MN)
      ENDDO
      ENDDO
C*
C*    read virtual temperature
      DO K=1,KMAX
      READ(N)QWK
      DO MN=1,MNWV2
      QTMP(MN,K)=QWK(MN)
      ENDDO
      ENDDO
C*
      CALL TRANSS(QGZS,   1,-1)
      CALL TRANSS(QLNP,   1,-1)
      CALL TRANSS(QTMP,KMAX,-1)
      CALL TRANSS(QDIV,KMAX,-1)
      CALL TRANSS(QROT,KMAX,-1)
      CALL TRANSS(QQ  ,KMAX,-1)
C*
      RETURN
      END
