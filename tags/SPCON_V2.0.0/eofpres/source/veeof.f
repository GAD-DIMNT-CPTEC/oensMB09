      PROGRAM EOFTEM
C*
C*    TEMPERATURE EOF ANALYSIS
C*
      INTEGER IMAX,JMAX,KMAX,LMAX
      PARAMETER (IMAX=5,JMAX=3,KMAX=2,LMAX=4)
C*
      INTEGER MIJK
      PARAMETER (MIJK=IMAX*JMAX*KMAX)
C*
      INTEGER I,J,K,L,M,IERR,IDI,IDO,
     *        INL,IAL,IPN,ICN,ITO,ITC,ITP1
      REAL TMIN,TMAX,TMN,TSIG,SIGMA1,SIGMA2,FACTOR
      REAL TP(IMAX,JMAX,KMAX)
      REAL X(MIJK,LMAX),XP(LMAX,MIJK),XX(LMAX,LMAX)
      REAL EVEC(LMAX,LMAX),EVEC1(MIJK,LMAX),Y(LMAX,LMAX)
      REAL TMNK(KMAX)
      REAL EVAL(LMAX),EVALB(LMAX),EVAL1(LMAX)
      LOGICAL LVEC(LMAX)
      CHARACTER DIRI*64,DIRO*64,NAMEL*64,ANAME*84,
     *          TEMOUT*64,TEMPCM*64,TEMPER1*64,
     *          TEMPEN1*64
      CHARACTER CNAME(LMAX)*184,PNAME(LMAX)*184
      REAL*4 BUFC(IMAX,JMAX),BUFP(IMAX,JMAX),
     *       BUFPN(IMAX,JMAX),BUFPAUX
C*
      NAMELIST /DATAIN/ DIRI,DIRO,NAMEL,ANAME,
     *                  TEMOUT,TEMPCM,TEMPER1,
     *                  TEMPEN1 
C*
      write(0,*)' LMAX= ',LMAX
      LVEC(1)=.TRUE.
      DO L=2,LMAX
      LVEC(L)=.FALSE.
      ENDDO
C*
      WRITE(*,*)'entrou no eoft fortran'
      WRITE(0,*)'entrou no eoft fortran'
      READ(*,DATAIN)
      WRITE(0,DATAIN)
      WRITE(*,DATAIN)
      write(*,*)'leu'
      IDI=INDEX(DIRI//' ',' ')-1
      IDO=INDEX(DIRO//' ',' ')-1
      INL=INDEX(NAMEL//' ',' ')-1
      IAL=INDEX(ANAME//' ',' ')-1
      ITO=INDEX(TEMOUT//' ',' ')-1
      ITC=INDEX(TEMPCM//' ',' ')-1
      ITP1=INDEX(TEMPER1//' ',' ')-1
c*      ITP2=INDEX(TEMPER2//' ',' ')-1
C*
      WRITE(*,*)'vai abrir ',NAMEL
      OPEN(55,FILE=DIRI(1:IDI)//NAMEL(1:INL),STATUS='OLD')
      DO L=1,LMAX
      READ(55,'(A)')CNAME(L)
      READ(55,'(A)')PNAME(L)
      ENDDO
      CLOSE(55)
C*
      WRITE(*,*)'leu nomes arquivos',CNAME,PNAME
      OPEN(66,FILE=DIRO(1:IDO)//TEMOUT(1:ITO),
     *        STATUS='UNKNOWN')
C*
C*    READ IN THE DATA NEEDED
C*
      WRITE(*,'(A)')' BEGIN READING:'
      DO L=1,LMAX
      ICN=INDEX(CNAME(L)//' ',' ')-1
      IPN=INDEX(PNAME(L)//' ',' ')-1
      OPEN(10,FILE=CNAME(L)(1:ICN),STATUS='OLD',
     *        FORM='UNFORMATTED')
      OPEN(11,FILE=PNAME(L)(1:IPN),STATUS='OLD',
     *        FORM='UNFORMATTED')
      DO K=1,KMAX
      WRITE(*,*) 'K=',K,' KMAX=',KMAX
      READ(10) BUFC
c*mmc
      WRITE(0,*)K,' BUFC********************'
      WRITE(*,*)'                            BUFC********************'
      WRITE(*,*) BUFC
      READ(11) BUFP
c*mmc
      WRITE(*,*)'                            BUFP********************'
      WRITE(*,*) BUFP
      DO J=1,JMAX
      DO I=1,IMAX
      X(I+(J-1)*IMAX+(K-1)*IMAX*JMAX,L)=BUFP(I,J)-BUFC(I,J)
      ENDDO
      ENDDO
c*mmc
      WRITE(*,*)'                              X ********************'
      WRITE(*,*) X   
      ENDDO
      CLOSE(10)
      CLOSE(11)
      ENDDO
      WRITE(*,'(A)')' END   READING:'
C*
      DO L=1,LMAX
      DO M=1,MIJK
      XP(L,M)=X(M,L)
      ENDDO
      ENDDO
      CALL ABCX(XP,X,XX,LMAX,MIJK,LMAX)
C*
      WRITE(66,'(A)')' '
      WRITE(66,'(A)')' MATRIX XX:'
      DO M=1,LMAX
      WRITE(66,'(1P11G9.2)')(XX(M,L),L=1,LMAX)
      ENDDO
c*
C*
      CALL RSWZ(LMAX,LMAX,XX,EVAL,EVEC,EVALB,IERR)
C*
      WRITE(*,'(/,A,I5,/)')' IERR=',IERR
      WRITE(66,'(/,A,I5,/)')' IERR=',IERR
      WRITE(66,'(A)')' EVAL: '
      WRITE(66,'(1P11G9.2)')EVAL
      WRITE(66,'(A)')' '
      WRITE(66,'(A)')' EVEC:'
      DO M=1,LMAX
      WRITE(66,'(1P11G9.2)')(EVEC(M,L),L=1,LMAX)
      ENDDO
C*
      DO L=1,LMAX
      EVAL1(L)=EVAL(L)
      CALL ABCX(X,EVEC(1,L),EVEC1(1,L),MIJK,LMAX,1)
      ENDDO
C*
      WRITE(*,'(/,A)')' EIGENVALUES (FROM HIGH TO LOW)'
      WRITE(*,'(1P11G9.2)')(EVAL1(L),L=1,LMAX)
C*
C*    CHOOSE LTH EIGENVECTOR TO OUTPUT
C*
      OPEN(72,FILE=DIRO(1:IDO)//TEMPER1(1:ITP1),
     *        STATUS='UNKNOWN',FORM='UNFORMATTED')
c*      OPEN(73,FILE=DIRO(1:IDO)//TEMPER2(1:ITP2),
c*     *        STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(74,FILE=DIRO(1:IDO)//TEMPEN1(1:ITP1),
     *        STATUS='UNKNOWN',FORM='UNFORMATTED')
c*      OPEN(75,FILE=DIRO(1:IDO)//TEMPEN2(1:ITP2),
c*     *        STATUS='UNKNOWN',FORM='UNFORMATTED')
      DO L=1,LMAX
      IF (LVEC(L)) THEN
      WRITE(66,'(/,A,I2,A,L6)')' LVEC(',L,') :',LVEC(L)
C*
      DO K=1,KMAX
      DO J=1,JMAX
      DO I=1,IMAX
      TP(I,J,K)=EVEC1(I+(J-1)*IMAX+(K-1)*IMAX*JMAX,L)/SQRT(EVAL1(L))
      ENDDO
      ENDDO
      ENDDO
C*
C*    RE-SCALE THE FIELDS
C*
      DO K=1,KMAX
      CALL VARAVE(TP(1,1,K),IMAX*JMAX,TMIN,TMAX,TMN,TSIG)
      TMNK(K)=TMN
      ENDDO
      WRITE(66,'(/,A)')' TMNK:'
      WRITE(66,'(1P11G9.2)')(TMNK(K),K=1,KMAX)
      CALL VARAVE(TMNK,KMAX,TMIN,TMAX,TMN,TSIG)
      WRITE(66,'(/,A,1PG9.2)')' TMN = ',TMN
C*
      SIGMA2=0.0
      DO K=1,KMAX
      DO J=1,JMAX
      DO I=1,IMAX
      SIGMA2=SIGMA2+(TP(I,J,K)-TMN)*(TP(I,J,K)-TMN)
      ENDDO
      ENDDO
      ENDDO
      SIGMA1=0.7
      SIGMA2=SQRT(SIGMA2/FLOAT(MIJK))
      FACTOR=(SIGMA1/SIGMA2)
      WRITE(66,'(/,1P,2(A,G12.5))')
     *        ' SIGMA2 =',SIGMA2,' FACTOR =',FACTOR
C*
      OPEN(20,FILE=DIRI(1:IDI)//ANAME(1:IAL),STATUS='OLD',
     *        FORM='UNFORMATTED')
      DO K=1,KMAX
      READ(20)BUFC
      DO J=1,JMAX
      DO I=1,IMAX
      BUFPAUX=FACTOR*TP(I,J,K)
      BUFP(I,J)=BUFC(I,J)+BUFPAUX
      BUFPN(I,J)=BUFC(I,J)-BUFPAUX
      ENDDO
      ENDDO
      WRITE(71+L) BUFP
      WRITE(71+L+2)BUFPN
      ENDDO
C*
      CLOSE(20) 
      ENDIF
      ENDDO
      CLOSE(72)
      CLOSE(74)
C*
      DO L=1,LMAX
      DO M=1,MIJK
      XP(L,M)=EVEC1(M,L)/SQRT(EVAL1(L))
      ENDDO
      ENDDO
      CALL ABCX(XP,X,Y,LMAX,MIJK,LMAX)
      CALL VARAVE(Y,LMAX*LMAX,TMIN,TMAX,TMN,TSIG)
      TMAX=MAX(ABS(TMAX),ABS(TMIN))
      WRITE(66,'(/,A,1PG12.5)')' TMAX =',TMAX
      DO L=1,LMAX
      DO M=M,LMAX
      Y(M,L)=Y(M,L)/TMAX
      ENDDO
      ENDDO
C*
      OPEN(70,FILE=DIRO(1:IDO)//TEMPCM(1:ITC),
     *        STATUS='UNKNOWN')
      DO M=1,LMAX
      WRITE(70,'(/,A,I4)')' SET :',M
      DO L=1,LMAX
      WRITE(70,'(1PG12.5)') Y(M,L)
      ENDDO
      ENDDO
      CLOSE(70)
C*
      CLOSE(66)
      STOP
      END
