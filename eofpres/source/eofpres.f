      PROGRAM EOFPRES
!*
!*    SURFACE PRESSURE EOF ANALYSIS
!*
      include "reseofes.inc"
!*
      INTEGER MIJK
!AMM      PARAMETER (MIJK=IMAX0*JMAX0) 
!*
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
      CHARACTER FLZOUT*256,MODEOUT*256
!*
      LOGICAL LVEC(LMAX)
      CHARACTER DIRI*512,DIRA*512,DIRO*512,NAMEL*256,ANAME*84,
     *          PRSOUT*512,PRSSCM*512,
     *          PRSSER1(LMAX)*256,PRSSEN1(LMAX)*256
      CHARACTER CNAME(LMAX)*284,PNAME(LMAX)*284
      REAL*4 BUFP(IMAX,JMAX),BUFC(IMAX,JMAX),
     *       BUFPN(IMAX,JMAX),BUFPAUX
      REAL*4 TOPO(IMAX,JMAX),PSLC(IMAX,JMAX)

!*

      NAMELIST /DATAIN/ DIRI,DIRA,DIRO,NAMEL,
     *                  ANAME,PRSOUT,PRSSCM
      NAMELIST /PRSSER/ PRSSER1
      NAMELIST /PRSSEN/ PRSSEN1
      NAMELIST /STPRES/ STDP
      NAMELIST /PARMET/ IINF,ISUP,IMAX0,
     *                  JINF,JSUP,JMAX0,
     *                  IIPS,ISPS,JIPS,JSPS

!*

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
!*
      WRITE(*,*)'unit=55',DIRI(1:IDI)//NAMEL(1:INL)
      OPEN(55,FILE=DIRI(1:IDI)//NAMEL(1:INL),STATUS='OLD')
      DO L=1,LMAX
      READ(55,'(A)')CNAME(L)
      READ(55,'(A)')PNAME(L)
      ENDDO
      CLOSE(55)
!*
      WRITE(*,*)'leu nomes arquivos'
      WRITE(*,'(A100)')CNAME
      WRITE(*,'(A100)')PNAME
      OPEN(66,FILE=DIRO(1:IDO)//PRSOUT(1:ITO),
     *        STATUS='UNKNOWN')
!*
!*    READ IN THE DATA NEEDED
!*
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

      READ(10) BUFC ! CFB - BUFC sao as previsoes da analise controle
      READ(11) BUFP ! CFB - BUFP sao as previsoes da analise rand. perturbada
      PRINT*,'PK','MAXVAL P =',MAXVAL(BUFC),';MINVAL P =',MINVAL(BUFC)
      PRINT*,'PK','MAXVAL P =',MAXVAL(BUFP),';MINVAL P =',MINVAL(BUFP)

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

!*
      DO L=1,LMAX
      DO M=1,MIJK
      XP(L,M)=X(M,L)
      ENDDO
      ENDDO
      PRINT*,'PK','MAXVAL XP =',MAXVAL(XP),';MINVAL XP =',MINVAL(XP)      
      CALL ABCX(XP,X,XX,LMAX,MIJK,LMAX) ! !CFB: XX=XX+XP*X (?)
!*
      WRITE(66,'(A)')' '
      WRITE(66,'(A)')' MATRIX XX:'     ! CFB - XX e uma especie de matriz de residuos
                                       ! (prev(ctrl)-prev(rand_pert))
      DO M=1,LMAX
      WRITE(66,'(1P11G9.2)')(XX(M,L),L=1,LMAX)
      ENDDO
!*
!*
      CALL RSWZ(LMAX,LMAX,XX,EVAL,EVEC,EVALB,IERR) ! CFB - chama outras duas subrotinas, uma para tornar
                                                   ! XX numa matriz tridiagonal e outra para encontrar
                                                   ! os autovalores de XX utilizando o metodo QL(QR)
!*
!AMM eigenvalues from greatest to lowest
!
      WRITE(66,'(/,A,I2)')'Eigenvalues: 1 to ',LMAX
      DO M=1,LMAX
      WRITE(66,*) EVAL(M) ! CFB - EVAL e o vetor com os autovalores ordenados (maior para menor)
      ENDDO

      WRITE(*,'(/,A,I5,/)')' IERR=',IERR
      WRITE(66,'(/,A,I5,/)')' IERR=',IERR
      WRITE(66,'(A)')' EVAL: '
      WRITE(66,'(1P11G9.2)')EVAL
      WRITE(66,'(A)')' '
      WRITE(66,'(A)')' EVEC:'
      DO M=1,LMAX
      WRITE(66,'(1P11G9.2)')(EVEC(M,L),L=1,LMAX) ! CFB - EVEC e a matriz em que cada coluna
                                                 ! corresponde a um autovetor; cada autovalor
                                                 ! esta associado com uma coluna
      ENDDO
!
!AMM evaluate the total and the relative variance and write in file
!
      STOT=0.0
      DO M=1,LMAX
         STOT=STOT+EVAL(M)
      ENDDO
      WRITE(66,*)' '
      WRITE(66,*)'the total variance is'
      WRITE(66,'(1P1G9.2)') STOT ! CFB - a variancia total e a soma de todos os autovalores
      WRITE(66,*)'the percent of explained variance is:'
      DO M=1,LMAX
         SREL(M)=EVAL(M)/STOT 
         WRITE(66,'(A,I2)')'Eigenvalue',M
         WRITE(66,'(1P1G9.2)') SREL(M) ! CFB - a porcentagem da variancia que cada autovalor
                                       ! "explica" e a razao entre o autovalor e a variancia total
      ENDDO
!AMM
!*
      DO L=1,LMAX
      EVAL1(L)=EVAL(L)
      write(*,*)'value of evecl=',l
      write(*,*)evec(1,l)
      CALL ABCX(X,EVEC(1,L),EVEC1(1,L),MIJK,LMAX,1) ! CFB - EVEC1=EVEC1+X*EVEC (?) 
      ENDDO
!*
      WRITE(*,'(/,A)')' EIGENVALUES (FROM HIGH TO LOW)'
      WRITE(*,'(1P11G9.2)')(EVAL1(L),L=1,LMAX)
      write(*,*)'eigenvectors'
      write(*,*)evec

!
!    CHOOSE LTH EIGENVECTOR TO OUTPUT ! CFB - LTH = "l-ezimo"?!
!
      DO L=1,LMAX
      IF (LVEC(L)) THEN

          DO J=1,JMAX0
          DO I=1,IMAX0
            TP(I,J)=EVEC1(I+(J-1)*IMAX0,L)/
     &	              SQRT(EVAL1(L))
          ENDDO
          ENDDO
!
!    RE-SCALE THE FIELDS AND PUT THE PERTURBATIONS AT TEMPP
!
          CALL VARAVE(TP(1,1),IMAX0*JMAX0,TMIN,TMAX,TMN,TSIG) ! CFB - o que esta entrando sao apenas
                                                              ! os autovalores, nao o campo!
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
        SIGMA2=SQRT(SIGMA2/REAL(MIJK))
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
!
!    WRITE THE PERTURBATED FIELD FOR THE ANALYSIS
!
        WRITE(66,'(/,A,I2,A,L6)')' LVEC(',L,') :',LVEC(L)
      
        ITP1=INDEX(PRSSER1(L)//' ',' ')-1
        OPEN(72,FILE=DIRO(1:IDO)//PRSSER1(L)(1:ITP1),
     *          STATUS='UNKNOWN',FORM='UNFORMATTED')
        OPEN(74,FILE=DIRO(1:IDO)//PRSSEN1(L)(1:ITP1),
     *        STATUS='UNKNOWN',FORM='UNFORMATTED')

        write(*,*)'unit20=',DIRA(1:IDA)//ANAME(1:IAL)
        OPEN(20,FILE=DIRA(1:IDA)//ANAME(1:IAL),STATUS='OLD',
     *          FORM='UNFORMATTED')

          READ(20)BUFC  ! CFB - BUFC possui valores semelhantes a temperatura [K]????????????
! CFB
          print *, "MAX BUFC = ", maxval(BUFC),"; MIN BUFC = ",
     *    minval(BUFC)
          print *, "MAX BUFP = ", maxval(BUFP),"; MIN BUFP = ",
     *    minval(BUFP)
          print *, "MAX BUFPN = ", maxval(BUFPN),"; MIN BUFPN = ",
     *    minval(BUFPN)
! CFB
          DO J=1,JINF-1
          DO I=1,IMAX
            BUFP(I,J)=BUFC(I,J)
            BUFPN(I,J)=BUFC(I,J)
          ENDDO
          ENDDO

! CFB
          print *, "MAX BUFC = ", maxval(BUFC),"; MIN BUFC = ",
     *    minval(BUFC)
          print *, "MAX BUFP = ", maxval(BUFP),"; MIN BUFP = ",
     *    minval(BUFP)
          print *, "MAX BUFPN = ", maxval(BUFPN),"; MIN BUFPN = ",
     *    minval(BUFPN)
! CFB
          DO J=JINF,JSUP
          DO I=1,IMAX
            IF (I.GE.IINF .AND. I.LE.ISUP) THEN 
              BUFP(I,J)=BUFC(I,J)+TEMPP(I,J,L) ! CFB - BUFC sao as previsoes da analise controle; 
                                               ! TEMPP sao as perturbcaoes
              BUFPN(I,J)=BUFC(I,J)-TEMPP(I,J,L)! aqui BUFP e a previsao de pressao perturbada "positiva" (soma);
            ELSE                               ! e BUFPN e a analise de pressao perturbada "negativa" (subtraida)
                                               ! OBS.: BUFP e BUFPN nao deveriam ser as ANALISES????
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
      Y(M,L)=Y(M,L)/TMAX ! CFB - escalonamento dos autovalores (normaliza Y por TMAX)
      ENDDO
      ENDDO
!*
      OPEN(70,FILE=DIRO(1:IDO)//PRSSCM(1:ITC), ! CFB - escreve um arquivo texto com 11 conjuntos 
                                               ! de perturbacoes (?) escalonadas para cada um dos 7
                                               ! membros do conjunto
     *        STATUS='UNKNOWN')
      DO M=1,LMAX
      WRITE(70,'(/,A,I4)')' SET :',M
      DO L=1,LMAX
      WRITE(70,'(1PG12.5)') Y(M,L)
      ENDDO
      ENDDO
      CLOSE(70)
!*
      CLOSE(66)
      STOP
      END
