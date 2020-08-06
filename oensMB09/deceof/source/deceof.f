      PROGRAM DECEOF
!*
      include "deceof.h"
!*
      INTEGER MEND1,MEND2,MNWV2,MNWV3
      PARAMETER (MEND1=MEND+1,MEND2=MEND+2,
     *           MNWV2=MEND1*MEND2,
     *           MNWV3=MNWV2+2*MEND1)
!*
      INTEGER MN,J,I,K,L,IDL,IDI,IDG,IDS,IGL,IGI,IGG,IGS
      REAL RD
      REAL QGZS(MNWV2),QLNP(MNWV2),QTV(MNWV2,KMAX),
     *     QDIV(MNWV2,KMAX),QROT(MNWV2,KMAX),QQ(MNWV2,KMAX),
     *     QU(MNWV3,KMAX),QV(MNWV3,KMAX)
!*
      REAL P(IMAX,JMAX),T(IMAX,JMAX,KMAX),Q(IMAX,JMAX,KMAX),
     *     U(IMAX,JMAX,KMAX),V(IMAX,JMAX,KMAX),
     *     GLAT(JMAX),CLAT(JMAX)
      REAL*4 G(IMAX,JMAX)

      INTEGER LDIM

      CHARACTER GNAMES*256
      CHARACTER(LEN=256), ALLOCATABLE :: GNAMEG(:)
      CHARACTER DIRL*256,DIRI*256,DIRG*256,DIRS*256,
     *          GNAMEL*256,GNAMEI*256
      CHARACTER HUM*3

      NAMELIST /DATAIN/ GNAMEL,DIRL,DIRI,DIRG,DIRS
      NAMELIST /HUMIDI/ HUM

      include "deceof2.h"

      READ(*,DATAIN)
      READ(*,HUMIDI)
      WRITE(*,DATAIN)
      write(*,*)'hum=',hum

      NFILE=NVARP*NREG
      ALLOCATE(GNAMEG(NFILE))

      IDL=INDEX(DIRL//' ',' ')-1
      IDI=INDEX(DIRI//' ',' ')-1
      IDG=INDEX(DIRG//' ',' ')-1
      IDS=INDEX(DIRS//' ',' ')-1
      IGL=INDEX(GNAMEL//' ',' ')-1
!*
      WRITE(*,'(/,A)')' OPEN55: '//DIRL(1:IDL)//GNAMEL(1:IDL)
      OPEN(55,FILE=DIRL(1:IDL)//GNAMEL(1:IGL),STATUS='OLD')

      READ(55,'(A)')GNAMEI
      DO L=1,NFILE
      READ(55,'(A)')GNAMEG(L)
      ENDDO
      READ(55,'(A)')GNAMES
      CLOSE(55)

      RD=ATAN(1.0)/45.0
      CALL FFTPLN(GLAT)
      DO J=1,JMAX
      CLAT(J)=COS(GLAT(J)*RD)
      ENDDO

      IGI=INDEX(GNAMEI//' ',' ')-1
      WRITE(*,'(/,A)')' OPEN11: '//DIRI(1:IDI)//GNAMEI(1:IGI)
      OPEN (11,FILE=DIRI(1:IDI)//GNAMEI(1:IGI),STATUS='OLD',
     *         FORM='UNFORMATTED')
      CALL QREAD(11,QGZS,QLNP,QTV,QDIV,QROT,QQ)
      CLOSE(11)
      CALL RESET(QLNP,MNWV2)
      CALL RESET(QTV, MNWV2*KMAX)
      CALL RESET(QDIV,MNWV2*KMAX)
      CALL RESET(QROT,MNWV2*KMAX)
      IF (HUM .EQ. 'YES') THEN
      CALL RESET(QQ,MNWV2*KMAX)
      ENDIF
!
! for pressure
!
      DO L=1,NREG

      LL=L
      IGG=INDEX(GNAMEG(LL)//' ',' ')-1
      WRITE(*,'(/,A)')' OPEN10: '//DIRG(1:IDG)//TRIM(GNAMEG(LL))
      OPEN (10,FILE=DIRG(1:IDG)//TRIM(GNAMEG(LL)),STATUS='UNKNOWN',
     *         FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
      READ(10)G
      PRINT*,'PK P','MAXVAL=',MAXVAL(G),';MINVAL P=',MINVAL(g)
      DO J=JN(L),JS(L)
      DO I=IW(L),IE(L)
      P(I,J)=ALOG(G(I,J)/10.0)
      ENDDO
      ENDDO
      CLOSE(10)
      ENDDO
!
! for temperature
!      
      DO L=1,NREG

      LL=L+NREG
      IGG=INDEX(GNAMEG(LL)//' ',' ')-1
      WRITE(*,'(/,A)')' OPEN10: '//DIRG(1:IDG)//TRIM(GNAMEG(LL))
      OPEN (10,FILE=DIRG(1:IDG)//TRIM(GNAMEG(LL)),STATUS='UNKNOWN',
     *         FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
      DO K=1,KMAX
      READ(10)G
      PRINT*,'PK T','MAXVAL=',MAXVAL(G),';MINVAL T=',MINVAL(g)

      DO J=JN(L),JS(L)
      DO I=IW(L),IE(L)
      T(I,J,K)=G(I,J)
      ENDDO
      ENDDO
      ENDDO
      CLOSE(10)
      ENDDO
!      
! for humidity
!      
      DO L=1,NREG

      LL=L+2*NREG
      IGG=INDEX(GNAMEG(LL)//' ',' ')-1
      WRITE(*,'(/,A)')' OPEN10: '//DIRG(1:IDG)//TRIM(GNAMEG(LL))
      OPEN (10,FILE=DIRG(1:IDG)//TRIM(GNAMEG(LL)),STATUS='UNKNOWN',
     *         FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
      DO K=1,KMAX
      READ(10)G
      PRINT*,'PK Q','MAXVAL=',MAXVAL(G),';MINVAL Q=',MINVAL(g)

      DO J=JN(L),JS(L)
      DO I=IW(L),IE(L)
      Q(I,J,K)=G(I,J)
      T(I,J,K)=T(I,J,K)*(1.0+0.608*G(I,J))
      ENDDO
      ENDDO
      ENDDO
      CLOSE(10)
      ENDDO
!      
! for u-wind
!      
      DO L=1,NREG

      LL=L+3*NREG
      IGG=INDEX(GNAMEG(LL)//' ',' ')-1
      WRITE(*,'(/,A)')' OPEN10: '//DIRG(1:IDG)//TRIM(GNAMEG(LL))
      OPEN (10,FILE=DIRG(1:IDG)//TRIM(GNAMEG(LL)),STATUS='UNKNOWN',
     *         FORM='UNFORMATTED',ACCESS='SEQUENTIAL')

      READ(10)G  !p
      DO K=1,KMAX
      READ(10)G  !t
      ENDDO
      DO K=1,KMAX
      READ(10)G  !q
      ENDDO

      DO K=1,KMAX
      READ(10)G

      DO J=JN(L),JS(L)
      DO I=IW(L),IE(L)
      U(I,J,K)=G(I,J)*CLAT(J)
      ENDDO
      ENDDO
      ENDDO
      PRINT*,'PK U','MAXVAL=',MAXVAL(U),';MINVAL U=',MINVAL(U)
      CLOSE(10)
      ENDDO
!      
! for v-wind
!      
      DO L=1,NREG

      LL=L+4*NREG
      IGG=INDEX(GNAMEG(LL)//' ',' ')-1
      WRITE(*,'(/,A)')' OPEN10: '//DIRG(1:IDG)//TRIM(GNAMEG(LL))
      OPEN (10,FILE=DIRG(1:IDG)//TRIM(GNAMEG(LL)),STATUS='UNKNOWN',
     *         FORM='UNFORMATTED',ACCESS='SEQUENTIAL')

      READ(10)G  !p
      DO K=1,KMAX
      READ(10)G  !t
      ENDDO
      DO K=1,KMAX
      READ(10)G  !q
      ENDDO
      DO K=1,KMAX
      READ(10)G  !u
      ENDDO

      DO K=1,KMAX
      READ(10)G

      DO J=JN(L),JS(L)
      DO I=IW(L),IE(L)
      V(I,J,K)=G(I,J)*CLAT(J)
      ENDDO
      ENDDO
      ENDDO
      PRINT*,'PK V','MAXVAL=',MAXVAL(V),';MINVAL V=',MINVAL(V)
      CLOSE(10)
      ENDDO
      
      WRITE(*,*)'NFILE: ',NFILE
      WRITE(*,*)'LL:    ',LL

      CALL UVTODZ(U,V,QROT,QDIV)

      CALL DECTRG(P,QLNP,  1)
      CALL DECTRG(T,QTV,KMAX)
      IF (HUM .EQ. 'YES') THEN
      CALL DECTRG(Q,QQ,KMAX)
      ENDIF


      WRITE(*,'(/,A)')' OPEN20: '//DIRS(1:IDS)//TRIM(GNAMES)
      OPEN (20,FILE=DIRS(1:IDS)//TRIM(GNAMES),STATUS='UNKNOWN',
     *         FORM='UNFORMATTED')
      PRINT*,'PK QGZS','MAXVAL=',MAXVAL(QGZS),
     *';MINVAL QGZS=',MINVAL(QGZS)
      PRINT*,'PK QLNP','MAXVAL=',MAXVAL(QLNP),
     * ';MINVAL QLNP=',MINVAL(QLNP)
      PRINT*,'PK QTV','MAXVAL=',MAXVAL(QTV),
     * ';MINVAL QTV=',MINVAL(QTV)
      PRINT*,'PK QDIV','MAXVAL=',MAXVAL(QDIV),
     * ';MINVAL QDIV=',MINVAL(QDIV)
      PRINT*,'PK QROT','MAXVAL=',MAXVAL(QROT),
     * ';MINVAL QROT=',MINVAL(QROT)
      PRINT*,'PK QQ','MAXVAL=',MAXVAL(QQ),
     * ';MINVAL QQ=',MINVAL(QQ)

      CALL QWRITE(20,QGZS,QLNP,QTV,QDIV,QROT,QQ)
      CLOSE(20)


      STOP
      END
