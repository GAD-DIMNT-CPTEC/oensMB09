      PROGRAM DECANL
C*
#include <decanl.h>
C*
      INTEGER MEND1,MEND2,MNWV2,MNWV3
      PARAMETER (MEND1=MEND+1,MEND2=MEND+2,
     *           MNWV2=MEND1*MEND2,
     *           MNWV3=MNWV2+2*MEND1)
C*
      INTEGER MN,J,I,K,L,IDL,IDI,IDG,IDS,IGL,IGI,IGG,IGS
      REAL RD
      REAL QGZS(MNWV2),QLNP(MNWV2),QTV(MNWV2,KMAX),
     *     QDIV(MNWV2,KMAX),QROT(MNWV2,KMAX),QQ(MNWV2,KMAX),
     *     QU(MNWV3,KMAX),QV(MNWV3,KMAX)
C*
      REAL T(IMAX,JMAX,KMAX),Q(IMAX,JMAX,KMAX),
     *     U(IMAX,JMAX,KMAX),V(IMAX,JMAX,KMAX),
     *     GLAT(JMAX),CLAT(JMAX)
      REAL*4 G(IMAX,JMAX)
      CHARACTER GNAMEG(LMAX)*64,GNAMES(LMAX)*64
      INTEGER LDIM
      CHARACTER DIRL*64,DIRI*64,DIRG*64,DIRS*64,
     *          GNAMEL*64,GNAMEI*64
      CHARACTER HUM*3
C*
      NAMELIST /DATAIN/ LDIM,GNAMEL,DIRL,DIRI,DIRG,DIRS
      NAMELIST /HUMIDI/ HUM
C*
      READ(*,DATAIN)
      WRITE(*,DATAIN)
c
      READ(*,HUMIDI)
      write(*,*)'hum=',hum
C*
      IDL=INDEX(DIRL//' ',' ')-1
      IDI=INDEX(DIRI//' ',' ')-1
      IDG=INDEX(DIRG//' ',' ')-1
      IDS=INDEX(DIRS//' ',' ')-1
      IGL=INDEX(GNAMEL//' ',' ')-1
C*
      WRITE(*,'(/,A)')' OPEN55: '//DIRL(1:IDL)//GNAMEL(1:IDL)
      OPEN(55,FILE=DIRL(1:IDL)//GNAMEL(1:IGL),STATUS='OLD')
      READ(55,'(A)')GNAMEI
      DO L=1,LDIM
      READ(55,'(A)')GNAMEG(L)
      READ(55,'(A)')GNAMES(L)
      ENDDO
      CLOSE(55)
C*
      RD=ATAN(1.0)/45.0
      CALL FFTPLN(GLAT)
      DO J=1,JMAX
      CLAT(J)=COS(GLAT(J)*RD)
      ENDDO
C*
      IGI=INDEX(GNAMEI//' ',' ')-1
      WRITE(*,'(/,A)')' OPEN11: '//DIRI(1:IDI)//GNAMEI(1:IGI)
      OPEN (11,FILE=DIRI(1:IDI)//GNAMEI(1:IGI),STATUS='OLD',
     *         FORM='UNFORMATTED')
      CALL QREAD(11,QGZS,QLNP,QTV,QDIV,QROT,QQ)
      CLOSE(11)
      CALL RESET(QTV,MNWV2*KMAX)
      CALL RESET(QDIV,MNWV2*KMAX)
      CALL RESET(QROT,MNWV2*KMAX)
      IF (HUM .EQ. 'YES') THEN
      write(*,*)'passou pelo primeiro YES'
      CALL RESET(QQ,MNWV2*KMAX)
      ENDIF
C*
      DO L=1,LDIM
      IGG=INDEX(GNAMEG(L)//' ',' ')-1
      IGS=INDEX(GNAMES(L)//' ',' ')-1
C*
      WRITE(*,'(/,A)')' OPEN10: '//DIRG(1:IDG)//GNAMEG(L)(1:IGG)
      OPEN (10,FILE=DIRG(1:IDG)//GNAMEG(L)(1:IGG),STATUS='UNKNOWN',
     *         FORM='UNFORMATTED')
      DO K=1,KMAX
      READ(10)G
      DO J=1,JMAX
      DO I=1,IMAX
      T(I,J,K)=G(I,J)
      ENDDO
      ENDDO
      ENDDO
      DO K=1,KMAX
      READ(10)G
      DO J=1,JMAX
      DO I=1,IMAX
      Q(I,J,K)=G(I,J)
      T(I,J,K)=T(I,J,K)*(1.0+0.608*G(I,J))
      ENDDO
      ENDDO
      ENDDO
      DO K=1,KMAX
      READ(10)G
      DO J=1,JMAX
      DO I=1,IMAX
      U(I,J,K)=G(I,J)*CLAT(J)
      ENDDO
      ENDDO
      ENDDO
      DO K=1,KMAX
      READ(10)G
      DO J=1,JMAX
      DO I=1,IMAX
      V(I,J,K)=G(I,J)*CLAT(J)
      ENDDO
      ENDDO
      ENDDO
C*
      CALL UVTODZ(U,V,QROT,QDIV)
C*
      CALL DECTRG(T,QTV,KMAX)
      IF (HUM .EQ. 'YES') THEN
      write(*,*)'passou pelo segundo YES'
      CALL DECTRG(Q,QQ,KMAX)
      ENDIF
C*
c*mmc:
      CLOSE(10)
      WRITE(*,'(/,A)')' OPEN20: '//DIRS(1:IDS)//GNAMES(L)(1:IGS)
      OPEN (20,FILE=DIRS(1:IDS)//GNAMES(L)(1:IGS),STATUS='UNKNOWN',
     *         FORM='UNFORMATTED')
      CALL QWRITE(20,QGZS,QLNP,QTV,QDIV,QROT,QQ)
      CLOSE(20)
C*
      ENDDO
C*
      STOP
      END
