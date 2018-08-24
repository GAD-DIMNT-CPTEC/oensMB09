      PROGRAM EOFHUM
C*
C*    TEMPERATURE EOF ANALYSIS
C*
      include "reseofes.inc"
C*
      INTEGER MIJK
CAMM      PARAMETER (MIJK=IMAX0*JMAX0*KMAX)
C*
      INTEGER I,J,K,L,M,IERR,IDI,IDO,IDA,
     *        INL,IAL,IPN,ICN,IHO,IHC,IHP1
      INTEGER IINF,ISUP,IMAX0,JINF,JSUP,JMAX0
      INTEGER IIPS,ISPS,JIPS,JSPS
      REAL TMIN,TMAX,TMN,TSIG
      REAL SIGMA1(KMAX),SIGMA2(KMAX),FACTOR(KMAX)
      REAL,ALLOCATABLE :: TP(:,:,:)
      REAL,ALLOCATABLE :: X(:,:),XP(:,:)
      REAL,ALLOCATABLE :: EVEC1(:,:)
      REAL EVEC(LMAX,LMAX),Y(LMAX,LMAX),XX(LMAX,LMAX)
      REAL TMNK(KMAX),STDQ(KMAX)
      REAL EVAL(LMAX),EVALB(LMAX),EVAL1(LMAX)

      REAL STOT,SREL(LMAX)

      REAL PENPS(LMAX)
      REAL ENERPR(LMAX),ENERPS(LMAX)
      REAL TEMPP(IMAX,JMAX,KMAX,LMAX)
      CHARACTER MODEOUT*258

      LOGICAL LVEC(LMAX)
      CHARACTER DIRI*258,DIRA*258,DIRO*258,NAMEL*258,ANAME*258,
     *          HUMOUT*258,HUMPCM*258,
     *          HUMPER1(LMAX)*258,HUMPEN1(LMAX)*258
      CHARACTER CNAME(LMAX)*284,PNAME(LMAX)*284
      REAL*4 BUFP(IMAX,JMAX),BUFC(IMAX,JMAX),
     *       BUFPN(IMAX,JMAX),BUFPAUX
C*
      NAMELIST /DATAIN/ DIRI,DIRA,DIRO,NAMEL,ANAME,
     *                  HUMOUT,HUMPCM
      NAMELIST /HUMPER/ HUMPER1
      NAMELIST /HUMPEN/ HUMPEN1
      NAMELIST /STHUMI/ STDQ
      NAMELIST /PARMET/ IINF,ISUP,IMAX0,
     *                  JINF,JSUP,JMAX0,
     *                  IIPS,ISPS,JIPS,JSPS
C*
      WRITE(*,*)'entrou no eofh fortran'
      READ(*,DATAIN)
      WRITE(0,DATAIN)
      WRITE(*,DATAIN)
      READ(*,HUMPER)
      READ(*,HUMPEN)
      READ(*,STHUMI)
      READ(*,PARMET)
      write(*,*)'stdq'
      write(*,*)stdq
      write(*,*)'PARMET:'
      write(*,*) IINF,ISUP,IMAX0,
     *           JINF,JSUP,JMAX0,
     *           IIPS,ISPS,JIPS,JSPS

      MIJK=IMAX0*JMAX0*KMAX

      ALLOCATE(TP(IMAX0,JMAX0,KMAX),X(MIJK,LMAX),XP(LMAX,MIJK),
     *         EVEC1(MIJK,LMAX))

      write(*,*)' LMAX= ',LMAX

      LVEC(1)=.TRUE.
      DO L=2,LMAX
      LVEC(L)=.FALSE.
      ENDDO

      IDI=INDEX(DIRI//' ',' ')-1
      IDA=INDEX(DIRA//' ',' ')-1
      IDO=INDEX(DIRO//' ',' ')-1
      INL=INDEX(NAMEL//' ',' ')-1
      IAL=INDEX(ANAME//' ',' ')-1
      IHO=INDEX(HUMOUT//' ',' ')-1
      IHC=INDEX(HUMPCM//' ',' ')-1
C*
      WRITE(*,*)'unit55=',NAMEL(1:INL)
      OPEN(55,FILE=DIRI(1:IDI)//NAMEL(1:INL),STATUS='OLD')
      DO L=1,LMAX
      READ(55,'(A)')CNAME(L)
      READ(55,'(A)')PNAME(L)
      WRITE(*,'(A)')TRIM(CNAME(L))
      WRITE(*,'(A)')TRIM(PNAME(L))
      ENDDO
      CLOSE(55)
C*
      WRITE(*,*)'leu nomes arquivos'
C*      WRITE(*,'(A)')TRIM(CNAME)
C*      WRITE(*,'(A)')TRIM(PNAME)
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
C
CAMM SURFACE PRESSURE
C
      READ(10) BUFC
      READ(11) BUFP
C
CAMM READ TEMPERATURE
C
      DO K=1,KMAX
      READ(10) BUFC
      READ(11) BUFP
      ENDDO
C
CAMM READ HUMIDITY    
C
      DO K=1,KMAX

      READ(10) BUFC
      READ(11) BUFP

      DO J=1,JMAX0
      DO I=1,IMAX0
      X(I+(J-1)*IMAX0+(K-1)*IMAX0*JMAX0,L)=
     *BUFP(I+IINF-1,J+JINF-1)-BUFC(I+IINF-1,J+JINF-1)
      ENDDO
      ENDDO

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

      CALL RSWZ(LMAX,LMAX,XX,EVAL,EVEC,EVALB,IERR)
CAMM 
CAMM eigenvalues from greatest to lowest
CAMM 
      WRITE(66,'(/,A,I2)')'Eigenvalues: 1 to ',LMAX
      DO M=1,LMAX
      WRITE(66,*) EVAL(M)
      ENDDO
CAMM 
      WRITE(*,'(/,A,I5,/)')' IERR=',IERR
      WRITE(66,'(/,A,I5,/)')' IERR=',IERR
      WRITE(66,'(A)')' EVAL: '
      WRITE(66,'(1P11G9.2)')EVAL
      WRITE(66,'(A)')' '
      WRITE(66,'(A)')' EVEC:'
      DO M=1,LMAX
      WRITE(66,'(1P11G9.2)')(EVEC(M,L),L=1,LMAX)
      ENDDO
CAMM
CAMM evaluate the total and the relative variance and write in file
CAMM
      STOT=0.0
      DO M=1,LMAX
         STOT=STOT+EVAL(M)
      ENDDO
      WRITE(66,*)' '
      WRITE(66,*)'the total variance is'
      WRITE(66,'(1P1G9.2)') STOT
      WRITE(66,*)'the percent of explained variance is:'
      DO M=1,LMAX
         SREL(M)=EVAL(M)/STOT
         WRITE(66,'(A,I2)')'Eigenvalue',M
         WRITE(66,'(1P1G9.2)') SREL(M)
      ENDDO

C*

      DO L=1,LMAX
      EVAL1(L)=EVAL(L)
      write(*,*)'value of evecl=',l
      write(*,*)evec(1,l)
      CALL ABCX(X(1,1),EVEC(1,L),EVEC1(1,L),MIJK,LMAX,1)
      ENDDO
C*
      WRITE(*,'(/,A)')' EIGENVALUES (FROM HIGH TO LOW)'
      WRITE(*,'(1P11G9.2)')(EVAL1(L),L=1,LMAX)
      write(*,*)'eigenvectors'
      write(*,*)evec
C*
C*    CHOOSE LTH EIGENVECTOR TO OUTPUT
C*
      DO L=1,LMAX
      IF (LVEC(L)) THEN

      DO K=1,KMAX
      DO J=1,JMAX0
      DO I=1,IMAX0
         TP(I,J,K)=EVEC1(I+(J-1)*IMAX0+(K-1)*IMAX0*JMAX0,L)/
     &	           SQRT(EVAL1(L))
      ENDDO
      ENDDO
      ENDDO
C*
C    RE-SCALE THE FIELDS AND PUT THE PERTURBATIONS AT TEMPP
C*
      DO K=1,KMAX
         CALL VARAVE(TP(1,1,K),IMAX0*JMAX0,TMIN,TMAX,TMN,TSIG)
         TMNK(K)=TMN
      ENDDO
      WRITE(66,'(/,A)')' TMNK:'
      WRITE(66,'(1P11G9.2)')TMNK(K)

      DO K=1,KMAX
      SIGMA2(K)=0.0
      DO J=1,JMAX0
      DO I=1,IMAX0
      SIGMA2(K)=SIGMA2(K)+(TP(I,J,K)-TMNK(K))*(TP(I,J,K)-TMNK(K))
      ENDDO
      ENDDO

C*    CFB: STDQ está em g/Kg; SIGMA1 está em Kg/Kg (1000 g/kg = 1 kg/kg => g/Kg / 1000 -> Kg/Kg)
      SIGMA1(K)=STDQ(K)/1000.0 ! CFB: Usar quando a umidade especifica do modelo estiver em g/Kg (fica coerente com o perfil de SIGMA1)
!      SIGMA1(K)=STDQ(K)         ! CFB: Usar quando a umidade especifica do modelo estiver em Kg/Kg (fica coerente com o perfil de SIGMA1) -->> ORIGINAL
      SIGMA2(K)=SQRT(SIGMA2(K)/REAL(IMAX0*JMAX0))
      write(*,*)'sigma1(',K,')=',sigma1(K)
      write(*,*)'sigma2(',K,')=',sigma2(K)
      IF (SIGMA2(K) .GT. 0.0) THEN
      FACTOR(K)=(SIGMA1(K)/SIGMA2(K))
      ELSE
      FACTOR(K)=0.0
      ENDIF
      write(*,*)'factor(',K,')=',factor(K)
      WRITE(66,'(/,1P,2(A,G12.5))')
     *        ' SIGMA1 =',SIGMA1(K),
     *        ' SIGMA2 =',SIGMA2(K),' FACTOR =',FACTOR(K)
      ENDDO


      DO K=1,KMAX
      DO J=1,JINF-1
      DO I=1,IMAX
         TEMPP(I,J,K,L)=0.0
      ENDDO
      ENDDO
      DO J=JINF,JSUP
      DO I=1,IMAX
         IF (I.GE.IINF .AND. I.LE.ISUP) THEN 
           BUFPAUX=FACTOR(K)*TP(I-IINF+1,J-JINF+1,K)
           TEMPP(I,J,K,L)=BUFPAUX
         ELSE
           TEMPP(I,J,K,L)=0.0
         ENDIF 
      ENDDO
      ENDDO
      DO J=JSUP+1,JMAX
      DO I=1,IMAX
         TEMPP(I,J,K,L)=0.0
      ENDDO
      ENDDO
      ENDDO
C
C    WRITE THE PERTURBED FIELD FOR THE ANALYSIS
C
      WRITE(66,'(/,A,I2,A,L6)')' LVEC(',L,') :',LVEC(L)

      IHP1=INDEX(HUMPER1(L)//' ',' ')-1
      OPEN(72,FILE=DIRO(1:IDO)//HUMPER1(L)(1:IHP1),
     *        STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(74,FILE=DIRO(1:IDO)//HUMPEN1(L)(1:IHP1),
     *        STATUS='UNKNOWN',FORM='UNFORMATTED')

      write(*,*)'unit20=',DIRA(1:IDA)//ANAME(1:IAL)
      OPEN(20,FILE=DIRA(1:IDA)//ANAME(1:IAL),STATUS='OLD',
     *          FORM='UNFORMATTED')
C
cAMM  SURFACE PRESSURE
C
      READ(20)BUFC
C
cAMM  READ TEMPERATURE
C
      DO K=1,KMAX
      READ(20)BUFC
      ENDDO
C
CAMM  READ HUMIDITY
C
      DO K=1,KMAX
      READ(20)BUFC
      DO J=1,JINF-1
      DO I=1,IMAX
      BUFP(I,J)=BUFC(I,J)
      BUFPN(I,J)=BUFC(I,J)
      ENDDO
      ENDDO
      DO J=JINF,JSUP
      DO I=1,IMAX
         IF (I.GE.IINF .AND. I.LE.ISUP) THEN 
           BUFP(I,J)=BUFC(I,J)+TEMPP(I,J,K,L)
           BUFPN(I,J)=BUFC(I,J)-TEMPP(I,J,K,L)
         ELSE
           BUFP(I,J)=BUFC(I,J)
           BUFPN(I,J)=BUFC(I,J)
         ENDIF 
      ENDDO
      ENDDO
      DO J=JSUP+1,JMAX
      DO I=1,IMAX
      BUFP(I,J)=BUFC(I,J)
      BUFPN(I,J)=BUFC(I,J)
      ENDDO
      ENDDO

      DO J=1,JMAX
      DO I=1,IMAX
      IF ( BUFP(I,J)  .LT. 0.0 ) BUFP(I,J) =0.0
      IF ( BUFPN(I,J) .LT. 0.0 ) BUFPN(I,J)=0.0
      ENDDO
      ENDDO

      WRITE(72) BUFP
      WRITE(74)BUFPN

      ENDDO

      CLOSE(20) 
      CLOSE(72)
      CLOSE(74)

      ENDIF
      ENDDO

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
      DO M=1,LMAX
      Y(M,L)=Y(M,L)/TMAX
      ENDDO
      ENDDO

      write(*,*)'file70= ',DIRO(1:IDO)//HUMPCM(1:IHC)
      OPEN(70,FILE=DIRO(1:IDO)//HUMPCM(1:IHC),
     *        STATUS='UNKNOWN')
      DO M=1,LMAX
      WRITE(70,'(/,A,I4)')' SET :',M
      DO L=1,LMAX
      WRITE(70,'(1PG12.5)') Y(M,L)
      ENDDO
      ENDDO
      CLOSE(70)

      CLOSE(66)
      STOP
      END
