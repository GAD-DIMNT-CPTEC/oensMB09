      PROGRAM EOFHUM
C*
C*    TEMPERATURE EOF ANALYSIS
C*
      include 'reseofes.inc'
C*
      INTEGER MIJK
CAMM      PARAMETER (MIJK=IMAX*JMAX0*KMAX)
      PARAMETER (MIJK=IMAX*JMAX0)
C*
      INTEGER I,J,K,L,M,IERR,IDI,IDO,IDA,
     *        INL,IAL,IPN,ICN,IHO,IHC,IHP1
      REAL TMIN,TMAX,TMN,TSIG
      REAL SIGMA1(KMAX),SIGMA2(KMAX),FACTOR(KMAX)
      REAL TP(IMAX,JMAX0,KMAX)
CAMM      REAL X(MIJK,LMAX),XP(LMAX,MIJK),XX(LMAX,LMAX)
      REAL X(MIJK,LMAX,KMAX),XP(LMAX,MIJK,KMAX),XX(LMAX,LMAX,KMAX)
CAMM      REAL EVEC(LMAX,LMAX),EVEC1(MIJK,LMAX),Y(LMAX,LMAX)
      REAL EVEC(LMAX,LMAX,KMAX),EVEC1(MIJK,LMAX,KMAX),Y(LMAX,LMAX,KMAX)
      REAL TMNK(KMAX),STDQ(KMAX)
CAMM      REAL EVAL(LMAX),EVALB(LMAX),EVAL1(LMAX)
      REAL EVAL(LMAX,KMAX),EVALB(LMAX,KMAX),EVAL1(LMAX,KMAX)
CAMM      LOGICAL LVEC(LMAX)
      LOGICAL LVEC(LMAX,KMAX)
      CHARACTER DIRI*64,DIRA*64,DIRO*64,NAMEL*64,ANAME*84,
     *          HUMOUT*64,HUMPCM*64,HUMPER1*64,
     *          HUMPEN1*64
      CHARACTER CNAME(LMAX)*284,PNAME(LMAX)*284
      REAL*4 BUFP(IMAX,JMAX),BUFC(IMAX,JMAX),
     *       BUFPN(IMAX,JMAX),BUFPAUX
C*
      NAMELIST /DATAIN/ DIRI,DIRA,DIRO,NAMEL,ANAME,
     *                  HUMOUT,HUMPCM,HUMPER1,
     *                  HUMPEN1 
      NAMELIST /STHUMI/ STDQ
C*
c      write(0,*)' LMAX= ',LMAX
      DO K=1,KMAX
      LVEC(1,K)=.TRUE.
      ENDDO
      DO K=1,KMAX
      DO L=2,LMAX
      LVEC(L,K)=.FALSE.
      ENDDO
      ENDDO
C*
      WRITE(*,*)'entrou no eofh fortran'
      WRITE(0,*)'entrou no eofh fortran'
      READ(*,DATAIN)
      WRITE(0,DATAIN)
      WRITE(*,DATAIN)
      write(*,*)'leu'
      IDI=INDEX(DIRI//' ',' ')-1
      IDA=INDEX(DIRA//' ',' ')-1
      IDO=INDEX(DIRO//' ',' ')-1
      INL=INDEX(NAMEL//' ',' ')-1
      IAL=INDEX(ANAME//' ',' ')-1
      IHO=INDEX(HUMOUT//' ',' ')-1
      IHC=INDEX(HUMPCM//' ',' ')-1
      IHP1=INDEX(HUMPER1//' ',' ')-1
C*
      READ(*,STHUMI)
      write(*,*)'stdq'
      write(*,*)stdq
c
      WRITE(*,*)'unit55=',NAMEL(1:INL)
      OPEN(55,FILE=DIRI(1:IDI)//NAMEL(1:INL),STATUS='OLD')
      DO L=1,LMAX
      READ(55,'(A)')CNAME(L)
      READ(55,'(A)')PNAME(L)
      ENDDO
      CLOSE(55)
C*
      WRITE(*,*)'leu nomes arquivos',CNAME,PNAME
      WRITE(*,*)'file66=',DIRO(1:IDO)//HUMOUT(1:IHO)
      OPEN(66,FILE=DIRO(1:IDO)//HUMOUT(1:IHO),
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
c
cAMM READ TEMPERATURE
      DO K=1,KMAX
      READ(10) BUFC
      READ(11) BUFP
      ENDDO
cAMM READ HUMIDITY    
      DO K=1,KMAX
c
      READ(10) BUFC
      READ(11) BUFP
c*mmc
cAMM      if ((k .eq. 1).or.(k .eq. 28)) then
cAMM     WRITE(*,*)'BUFC********************'
cAMM      WRITE(*,*) BUFC
cAMM      WRITE(*,*)'BUFP********************'
cAMM      WRITE(*,*) BUFP
cAMM      endif

CAMM      DO J=1,JMAX0
CAMM      DO I=1,IMAX
CAMM      X(I+(J-1)*IMAX+(K-1)*IMAX*JMAX0,L)=
CAMM     *BUFP(I,J+JINF-1)-BUFC(I,J+JINF-1)
CAMM      ENDDO
CAMM      ENDDO
      DO J=1,JMAX0
      DO I=1,IMAX
      X(I+(J-1)*IMAX,L,K)=BUFP(I,J+JINF-1)-BUFC(I,J+JINF-1)
      ENDDO
      ENDDO
c*mmc
c*      WRITE(*,*)'X ********************'
c*      WRITE(*,*) X   
      ENDDO
      CLOSE(10)
      CLOSE(11)
      ENDDO
      WRITE(*,'(A)')' END   READING:'
C*
      DO K=1,KMAX
      DO L=1,LMAX
      DO M=1,MIJK
      XP(L,M,K)=X(M,L,K)
      ENDDO
      ENDDO
      ENDDO
      DO K=1,KMAX
      CALL ABCX(XP(1,1,K),X(1,1,K),XX(1,1,K),LMAX,MIJK,LMAX)
      ENDDO
C*
      WRITE(66,'(A)')' '
      WRITE(66,'(A)')' MATRIX XX:'
      DO K=1,KMAX
      WRITE(66,'(A,I2)')'K=',K
      DO M=1,LMAX
      WRITE(66,'(1P11G9.2)')(XX(M,L,K),L=1,LMAX)
      ENDDO
      ENDDO
c*
C*
      CALL RSWZ(LMAX,LMAX,XX,EVAL,EVEC,EVALB,IERR,KMAX)
C*
      WRITE(*,'(/,A,I5,/)')' IERR=',IERR
      WRITE(66,'(/,A,I5,/)')' IERR=',IERR
      WRITE(66,'(A)')' EVAL: '
      WRITE(66,'(1P11G9.2)')EVAL
      WRITE(66,'(A)')' '
      WRITE(66,'(A)')' EVEC:'
      DO K=1,KMAX
      DO M=1,LMAX
      WRITE(66,'(1P11G9.2)')(EVEC(M,L,K),L=1,LMAX)
      ENDDO
      ENDDO
C*

      DO K=1,KMAX
      DO L=1,LMAX
      EVAL1(L,K)=EVAL(L,K)
camm      write(*,*)'valor de evecl=',l
camm      write(*,*)evec(1,l)
      CALL ABCX(X(1,1,K),EVEC(1,L,K),EVEC1(1,L,K),MIJK,LMAX,1)
      ENDDO
      ENDDO
C*
      WRITE(*,'(/,A)')' EIGENVALUES (FROM HIGH TO LOW)'
      DO K=1,KMAX
      write(*,*)'K=',K
      WRITE(*,'(1P11G9.2)')(EVAL1(L,K),L=1,LMAX)
      ENDDO

c      write(*,*)'mijk=',mijk
c      write(*,*)'valores de evec1'
c      do k=1,kmax
c      write(*,*)'K=',K
c      if (k .ge. 20) then
c      do j=1,lmax
c      write(*,*)'L=',j
c      do i=1,mijk
c      write(*,*) (evec1(i,j,k),i=1,mijk)
c      enddo
c      enddo
c      endif
c      enddo
      
C*
C*    CHOOSE LTH EIGENVECTOR TO OUTPUT
C*
      OPEN(72,FILE=DIRO(1:IDO)//HUMPER1(1:IHP1),
     *        STATUS='UNKNOWN',FORM='UNFORMATTED')
c*      OPEN(73,FILE=DIRO(1:IDO)//HUMEPR2(1:IHP2),
c*     *        STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(74,FILE=DIRO(1:IDO)//HUMPEN1(1:IHP1),
     *        STATUS='UNKNOWN',FORM='UNFORMATTED')
c*      OPEN(75,FILE=DIRO(1:IDO)//HUMPEN2(1:IHP2),
c*     *        STATUS='UNKNOWN',FORM='UNFORMATTED')

      write(*,*)'valores de lvec'
      write(*,*)lvec

      DO L=1,LMAX
      DO KM=1,KMAX
      IF (LVEC(L,KM)) THEN
CAMM      WRITE(66,'(/,A,I2,A,L6)')' LVEC(',L,',',KM,') :',LVEC(L,KM)
      WRITE(66,'(/,A,I2,A,I2,A,L6)')' LVEC(',L,',',KM,') :',LVEC(L,KM)
C*
CAMM      DO K=1,KMAX
      DO J=1,JMAX0
      DO I=1,IMAX
CAMM      TP(I,J,KM)=EVEC1(I+(J-1)*IMAX+(K-1)*IMAX*JMAX0,L)/SQRT(EVAL1(L))
      TP(I,J,KM)=EVEC1(I+(J-1)*IMAX,L,KM)/SQRT(EVAL1(L,KM))
      ENDDO
      ENDDO
CAMM      ENDDO
C*
C*    RE-SCALE THE FIELDS
      write(*,*) 'vai reescalonar  '   
C*
CAMM      DO K=1,KMAX
CAMM      CALL VARAVE(TP(1,1,K),IMAX*JMAX0,TMIN,TMAX,TMN,TSIG)
CAMM      TMNK(K)=TMN
      CALL VARAVE(TP(1,1,KM),IMAX*JMAX0,TMIN,TMAX,TMN,TSIG)
      TMNK(KM)=TMN
CAMM      ENDDO
      WRITE(66,'(/,A)')' TMNK:'
CAMM      WRITE(66,'(1P11G9.2)')(TMNK(K),K=1,KMAX)
      WRITE(66,'(1P11G9.2)')TMNK(KM)
cAMM      CALL VARAVE(TMNK,KMAX,TMIN,TMAX,TMN,TSIG)
cAMM      WRITE(66,'(/,A,1PG9.2)')' TMN = ',TMN
C*
CAMM      DO K=1,KMAX
      SIGMA2(KM)=0.0
      DO J=1,JMAX0
      DO I=1,IMAX
CAMM      SIGMA2(K)=SIGMA2(K)+(TP(I,J,K)-TMNK(K))*(TP(I,J,K)-TMNK(K))
      SIGMA2(KM)=SIGMA2(KM)+(TP(I,J,KM)-TMNK(KM))*(TP(I,J,KM)-TMNK(KM))
      ENDDO
      ENDDO
CAMM      ENDDO
c*mmc      SIGMA1=0.7
CAMM      DO K=1,KMAX
      SIGMA1(KM)=STDQ(KM)/1000.0
      SIGMA2(KM)=SQRT(SIGMA2(KM)/FLOAT(IMAX*JMAX0))
      write(*,*)'sigma1(',KM,')=',sigma1(KM)
      write(*,*)'sigma2(',KM,')=',sigma2(KM)
      IF (SIGMA2(KM) .GT. 0.0) THEN
      FACTOR(KM)=(SIGMA1(KM)/SIGMA2(KM))
      ELSE
      FACTOR(KM)=0.0
      ENDIF
      write(*,*)'factor(',KM,')=',factor(KM)
      WRITE(66,'(/,1P,2(A,G12.5))')
     *        ' SIGMA2 =',SIGMA2(KM),' FACTOR =',FACTOR(KM)
CAMM      ENDDO
C*
      IF ( (L.EQ.1) .AND. (KM.EQ.1) ) THEN 
      write(*,*)'unit20=',DIRA(1:IDA)//ANAME(1:IAL)
      OPEN(20,FILE=DIRA(1:IDA)//ANAME(1:IAL),STATUS='OLD',
     *        FORM='UNFORMATTED')
cAMM  READ TEMPERATURE
      DO K=1,KMAX
      READ(20)BUFC
      ENDDO
      ENDIF

cAMM  READ HUMIDITY
CAMM      DO K=1,KMAX
      READ(20)BUFC
      DO J=1,JINF-1
      DO I=1,IMAX
      BUFP(I,J)=BUFC(I,J)
      BUFPN(I,J)=BUFC(I,J)
      ENDDO
      ENDDO
      DO J=JINF,JSUP
      DO I=1,IMAX
      BUFPAUX=FACTOR(KM)*TP(I,J-JINF+1,KM)
      BUFP(I,J)=BUFC(I,J)+BUFPAUX
      BUFPN(I,J)=BUFC(I,J)-BUFPAUX
      ENDDO
      ENDDO
      DO J=JSUP+1,JMAX
      DO I=1,IMAX
      BUFP(I,J)=BUFC(I,J)
      BUFPN(I,J)=BUFC(I,J)
      ENDDO
      ENDDO
      WRITE(71+L) BUFP
      WRITE(71+L+2)BUFPN
CAMM      ENDDO
C*
CAMM      CLOSE(20) 
      ENDIF
      ENDDO
      CLOSE(20) 
      ENDDO
      CLOSE(72)
      CLOSE(74)
C*
      DO K=1,KMAX
      DO L=1,LMAX
      DO M=1,MIJK
      XP(L,M,K)=EVEC1(M,L,K)/SQRT(EVAL1(L,K))
      ENDDO
      ENDDO
      ENDDO
      DO K=1,KMAX
      CALL ABCX(XP(1,1,K),X(1,1,K),Y(1,1,K),LMAX,MIJK,LMAX)
      ENDDO
      DO K=1,KMAX
      CALL VARAVE(Y(1,1,K),LMAX*LMAX,TMIN,TMAX,TMN,TSIG)
      TMAX=MAX(ABS(TMAX),ABS(TMIN))
      WRITE(66,'(/,A,1PG12.5)')' TMAX =',TMAX
      DO L=1,LMAX
      DO M=1,LMAX
      Y(M,L,K)=Y(M,L,K)/TMAX
      ENDDO
      ENDDO
      ENDDO
C*
      write(*,*)'file70= ',DIRO(1:IDO)//HUMPCM(1:IHC)
      OPEN(70,FILE=DIRO(1:IDO)//HUMPCM(1:IHC),
     *        STATUS='UNKNOWN')
      DO K=1,KMAX
      DO M=1,LMAX
      WRITE(70,'(/,A,I4)')' SET :',M
      DO L=1,LMAX
      WRITE(70,'(1PG12.5)') Y(M,L,K)
      ENDDO
      ENDDO
      ENDDO
      CLOSE(70)
C*
      CLOSE(66)
      STOP
      END
