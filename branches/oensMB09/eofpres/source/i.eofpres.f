      PROGRAM EOFPRES
C*
C*    SURFACE PRESSURE EOF ANALYSIS
C*
      INTEGER IMAX,JMAX,MEND,KMAX,LMAX
      PARAMETER (IMAX=384,JMAX=192,KMAX=28)
      PARAMETER (MEND=126,LMAX=11)
C*
      INTEGER MIJK
cAMM      PARAMETER (MIJK=IMAX0*JMAX0) 
C*
      INTEGER I,J,K,L,M,IERR,IDI,IDO,IDA,
     *        INL,IAL,IAL2,IPN,ICN,ITO,ITC,ITP1
      INTEGER IINF,ISUP,IMAX0,JINF,JSUP,JMAX0
      INTEGER IIPS,ISPS,JIPS,JSPS
      REAL TMIN,TMAX,TMN,TSIG,SIGMA1,SIGMA2,FACTOR
      REAL,ALLOCATABLE :: TP(:,:)
      REAL,ALLOCATABLE :: X(:,:),XP(:,:)
      REAL,ALLOCATABLE :: EVEC1(:,:)
      REAL EVEC(LMAX,LMAX),Y(LMAX,LMAX),XX(LMAX,LMAX)
      REAL TMNK,STDP
      REAL EVAL(LMAX),EVALB(LMAX),EVAL1(LMAX)

      REAL STOT,SREL(LMAX),DEL(KMAX)
      REAL PENPS(LMAX)
      REAL TEMPA(IMAX,JMAX),TEMPVA(IMAX,JMAX)
      REAL TEMPP(IMAX,JMAX,LMAX),UMES(IMAX,JMAX)
      REAL TEMPVF(IMAX,JMAX,LMAX),PENERF(IMAX,JMAX,LMAX)
      REAL PENERA(IMAX,JMAX),PENER(IMAX,JMAX)
      REAL PZGEO(IMAX,JMAX),TAS(IMAX,JMAX)
      REAL,ALLOCATABLE :: TT(:,:)
      REAL ENERPR(LMAX),ENERAS(LMAX),ENERSB(LMAX),ENERNB(LMAX)
      REAL ENERPS(LMAX)
      CHARACTER FLZOUT*64,MODEOUT*64
C*
      LOGICAL LVEC(LMAX)
      CHARACTER DIRI*64,DIRA*64,DIRO*64,NAMEL*64,ANAME*84,
     *          PRSOUT*64,PRSSCM*64,
     *          PRSSER1(LMAX)*64,PRSSEN1(LMAX)*64
      CHARACTER CNAME(LMAX)*284,PNAME(LMAX)*284
      REAL*4 BUFP(IMAX,JMAX),BUFC(IMAX,JMAX),
     *       BUFPN(IMAX,JMAX),BUFPAUX
      REAL*4 TOPO(IMAX,JMAX),PSLC(IMAX,JMAX)

C*

      NAMELIST /DATAIN/ DIRI,DIRA,DIRO,NAMEL,
     *                  ANAME,PRSOUT,PRSSCM
      NAMELIST /PRSSER/ PRSSER1
      NAMELIST /PRSSEN/ PRSSEN1
      NAMELIST /STPRES/ STDP
      NAMELIST /PARMET/ IINF,ISUP,IMAX0,
     *                  JINF,JSUP,JMAX0,
     *                  IIPS,ISPS,JIPS,JSPS

C*

      WRITE(*,*)'entrou no eoft fortran'
      READ(*,DATAIN)
      WRITE(0,DATAIN)
      WRITE(*,DATAIN)
      READ(*,PRSSER)
      READ(*,PRSSEN)
      READ(*,STPRES)
      READ(*,PARMET)
      write(*,*)'PRSSER1:'
      write(*,'(2A20)')PRSSER1
      write(*,*)'stdp'
      write(*,*)stdp
      write(*,*)'PARMET:'
      write(*,*) IINF,ISUP,IMAX0,
     *           JINF,JSUP,JMAX0,
     *           IIPS,ISPS,JIPS,JSPS
      

      MIJK=IMAX0*JMAX0

      ALLOCATE(TP(IMAX0,JMAX0),X(MIJK,LMAX),XP(LMAX,MIJK),
     *         EVEC1(MIJK,LMAX),TT(IMAX0,JMAX0))


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
      ITO=INDEX(PRSOUT//' ',' ')-1
      ITC=INDEX(PRSSCM//' ',' ')-1
C*
      WRITE(*,*)'unit=55',DIRI(1:IDI)//NAMEL(1:INL)
      OPEN(55,FILE=DIRI(1:IDI)//NAMEL(1:INL),STATUS='OLD')
      DO L=1,LMAX
      READ(55,'(A)')CNAME(L)
      READ(55,'(A)')PNAME(L)
      ENDDO
      CLOSE(55)
C*
      WRITE(*,*)'leu nomes arquivos'
      WRITE(*,'(A100)')CNAME
      WRITE(*,'(A100)')PNAME
      OPEN(66,FILE=DIRO(1:IDO)//PRSOUT(1:ITO),
     *        STATUS='UNKNOWN')
C*
C*    READ IN THE DATA NEEDED
C*
      WRITE(*,'(A)')' BEGIN READING:'
      DO L=1,LMAX
      ICN=INDEX(CNAME(L)//' ',' ')-1
      IPN=INDEX(PNAME(L)//' ',' ')-1
      WRITE(*,'(A,A)')' unit10:',CNAME(L)(1:ICN)
      WRITE(*,'(A,A)')' unit11:',PNAME(L)(1:IPN)
      OPEN(10,FILE=CNAME(L)(1:ICN),STATUS='OLD',
     *        FORM='UNFORMATTED')
      OPEN(11,FILE=PNAME(L)(1:IPN),STATUS='OLD',
     *        FORM='UNFORMATTED')

      READ(10) BUFC
      READ(11) BUFP

      DO J=1,JMAX0
      DO I=1,IMAX0
      X(I+(J-1)*IMAX0,L)=
     *BUFP(I+IINF-1,J+JINF-1)-BUFC(I+IINF-1,J+JINF-1)
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
c*
C*
      CALL RSWZ(LMAX,LMAX,XX,EVAL,EVEC,EVALB,IERR)
C*
CAMM eigenvalues from greatest to lowest
C
      WRITE(66,'(/,A,I2)')'Eigenvalues: 1 to ',LMAX
      DO M=1,LMAX
      WRITE(66,*) EVAL(M)
      ENDDO

      WRITE(*,'(/,A,I5,/)')' IERR=',IERR
      WRITE(66,'(/,A,I5,/)')' IERR=',IERR
      WRITE(66,'(A)')' EVAL: '
      WRITE(66,'(1P11G9.2)')EVAL
      WRITE(66,'(A)')' '
      WRITE(66,'(A)')' EVEC:'
      DO M=1,LMAX
      WRITE(66,'(1P11G9.2)')(EVEC(M,L),L=1,LMAX)
      ENDDO
C
CAMM evaluate the total and the relative variance and write in file
C
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
CAMM
C*
      DO L=1,LMAX
      EVAL1(L)=EVAL(L)
      write(*,*)'value of evecl=',l
      write(*,*)evec(1,l)
      CALL ABCX(X,EVEC(1,L),EVEC1(1,L),MIJK,LMAX,1)
      ENDDO
C*
      WRITE(*,'(/,A)')' EIGENVALUES (FROM HIGH TO LOW)'
      WRITE(*,'(1P11G9.2)')(EVAL1(L),L=1,LMAX)
      write(*,*)'eigenvectors'
      write(*,*)evec
C
C    CHOOSE LTH EIGENVECTOR TO OUTPUT
C
      DO L=1,LMAX
      IF (LVEC(L)) THEN

          DO J=1,JMAX0
          DO I=1,IMAX0
            TP(I,J)=EVEC1(I+(J-1)*IMAX0,L)/
     &	              SQRT(EVAL1(L))
          ENDDO
          ENDDO
C
C    RE-SCALE THE FIELDS AND PUT THE PERTURBATIONS AT TEMPP
C
          CALL VARAVE(TP(1,1),IMAX0*JMAX0,TMIN,TMAX,TMN,TSIG)
          TMNK=TMN

        WRITE(66,'(/,A)')' TMNK:'

        WRITE(66,'(/,A,1PG9.2)')' TMN = ',TMN

        SIGMA2=0.0

          DO J=1,JMAX0
          DO I=1,IMAX0
            SIGMA2=SIGMA2+(TP(I,J)-TMN)*(TP(I,J)-TMN)
          ENDDO
          ENDDO

        SIGMA1=STDP
        SIGMA2=SQRT(SIGMA2/FLOAT(MIJK))
        FACTOR=(SIGMA1/SIGMA2)
        WRITE(66,'(/,1P,2(A,G12.5))')
     *        ' SIGMA1 =',SIGMA1,' SIGMA2 =',SIGMA2,
     *        ' FACTOR =',FACTOR

          DO J=1,JINF-1
          DO I=1,IMAX
            TEMPP(I,J,L)=0.0
          ENDDO
          ENDDO
          DO J=JINF,JSUP
          DO I=1,IMAX
            IF (I.GE.IINF .AND. I.LE.ISUP) THEN 
              BUFPAUX=FACTOR*TP(I-IINF+1,J-JINF+1)
              TEMPP(I,J,L)=BUFPAUX
            ELSE
              TEMPP(I,J,L)=0.0
            ENDIF 
          ENDDO
          ENDDO
          DO J=JSUP+1,JMAX
          DO I=1,IMAX
            TEMPP(I,J,L)=0.0
          ENDDO
          ENDDO
C
C    WRITE THE PERTURBATED FIELD FOR THE ANALYSIS
C
        WRITE(66,'(/,A,I2,A,L6)')' LVEC(',L,') :',LVEC(L)
      
        ITP1=INDEX(PRSSER1(L)//' ',' ')-1
        OPEN(72,FILE=DIRO(1:IDO)//PRSSER1(L)(1:ITP1),
     *          STATUS='UNKNOWN',FORM='UNFORMATTED')
        OPEN(74,FILE=DIRO(1:IDO)//PRSSEN1(L)(1:ITP1),
     *        STATUS='UNKNOWN',FORM='UNFORMATTED')

        write(*,*)'unit20=',DIRA(1:IDA)//ANAME(1:IAL)
        OPEN(20,FILE=DIRA(1:IDA)//ANAME(1:IAL),STATUS='OLD',
     *          FORM='UNFORMATTED')

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
              BUFP(I,J)=BUFC(I,J)+TEMPP(I,J,L)
              BUFPN(I,J)=BUFC(I,J)-TEMPP(I,J,L)
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
          WRITE(72) BUFP
          WRITE(74)BUFPN

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
C*
      OPEN(70,FILE=DIRO(1:IDO)//PRSSCM(1:ITC),
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
