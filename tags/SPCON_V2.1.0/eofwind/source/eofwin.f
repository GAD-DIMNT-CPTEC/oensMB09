      PROGRAM EOFWIN
C*
C*    COMPLEX (U+iV) WIND EOF ANALYSIS
C*
      include "reseofes.inc"
C*
           INTEGER MIJK
C*
      INTEGER I,J,K,L,M,IERR,IDI,IDO,IDA,
     *        INL,IAL,IPN,ICN,IWO,IWC,IWP1,IWP2,
     *        ITP1,IHP1
      INTEGER IINF,ISUP,IMAX0,JINF,JSUP,JMAX0
      INTEGER IIPS,ISPS,JIPS,JSPS
      REAL UMIN,VMIN,UMAX,VMAX,UMN,VMN,
     *     USIG,VSIG,SIGMA1,SIGMA2,FACTOR
      REAL UVMN(KMAX),STDU(KMAX),STDV(KMAX)
      REAL,   ALLOCATABLE :: UP(:,:,:),VP(:,:,:)
      COMPLEX,ALLOCATABLE :: X(:,:),XP(:,:)
      COMPLEX,ALLOCATABLE :: EVEC1(:,:)
      COMPLEX EVEC(LMAX,LMAX),Y(LMAX,LMAX),XX(LMAX,LMAX)
      REAL EVAL(LMAX),EVAL1(LMAX),YU(LMAX,LMAX),YV(LMAX,LMAX)
      REAL XR(LMAX,LMAX),XI(LMAX,LMAX),ZR(LMAX,LMAX),ZI(LMAX,LMAX),
     *     W1(LMAX),W2(LMAX),W3(2,LMAX)

      INTEGER NPIAS
      REAL STOT,SREL(LMAX)
      REAL TUP(IMAX,JMAX,KMAX,LMAX),TVP(IMAX,JMAX,KMAX,LMAX)
      REAL ZMTUAS(JMAX,KMAX),ZMTVAS(JMAX,KMAX)
      REAL TUAS(IMAX,JMAX,KMAX),TVAS(IMAX,JMAX,KMAX)
      REAL ENERPR(LMAX),ENERAS(LMAX),ENERSB(LMAX),ENERNB(LMAX)
      REAL ENERPS(LMAX),PENPS(LMAX)
      CHARACTER FLZOUT*256,MODEOUT*256

      LOGICAL LVEC(LMAX)
      CHARACTER HUM*3
      CHARACTER DIRI*512,DIRA*256,DIRO*256,NAMEL*256,ANAME*184,
     *          WINOUT*256,WINPCM*256,
     *          WINPER1(LMAX)*256,WINPEN1(LMAX)*256,
     *          PRSSER1(LMAX)*256,PRSSEN1(LMAX)*256,
     *          TEMPER1(LMAX)*256,TEMPEN1(LMAX)*256,
     *          HUMPER1(LMAX)*256,HUMPEN1(LMAX)*256
      CHARACTER CNAME(LMAX)*184,PNAME(LMAX)*184
      REAL*4 BFUC(IMAX,JMAX),BFUP(IMAX,JMAX),
     *       BFUPN(IMAX,JMAX),BFUPAUX,
     *       BFVC(IMAX,JMAX),BFVP(IMAX,JMAX),
     *       BFVPN(IMAX,JMAX),BFVPAUX
      REAL*4 BFUCP(IMAX,JMAX,KMAX),BFUPP(IMAX,JMAX,KMAX),
     *       BFVCP(IMAX,JMAX,KMAX),BFVPP(IMAX,JMAX,KMAX)


      NAMELIST /DATAIN/ DIRI,DIRA,DIRO,NAMEL,ANAME,
     *                  WINOUT,WINPCM
      NAMELIST /WINPER/ WINPER1
      NAMELIST /WINPEN/ WINPEN1
      NAMELIST /PRSSER/ PRSSER1
      NAMELIST /PRSSEN/ PRSSEN1
      NAMELIST /TEMPER/ TEMPER1
      NAMELIST /TEMPEN/ TEMPEN1
      NAMELIST /HUMPER/ HUMPER1
      NAMELIST /HUMPEN/ HUMPEN1
      NAMELIST /STZWIN/ STDU
      NAMELIST /STMWIN/ STDV
      NAMELIST /HUMIDI/ HUM
      NAMELIST /PARMET/ IINF,ISUP,IMAX0,
     *                  JINF,JSUP,JMAX0,
     *                  IIPS,ISPS,JIPS,JSPS
C*
      READ(*,DATAIN)
      READ(*,WINPER)
      READ(*,WINPEN)
      READ(*,PRSSER)
      READ(*,PRSSEN)
      READ(*,TEMPER)
      READ(*,TEMPEN)
      READ(*,HUMPER)
      READ(*,HUMPEN)
      READ(*,STZWIN)
      READ(*,STMWIN)
      READ(*,HUMIDI)
      READ(*,PARMET)
      write(*,'(A)')'winper1'
      write(*,'(2A30)')winper1
      write(*,'(A)')'winpen1'
      write(*,'(2A30)')winpen1
      write(*,*)'stdu'
      write(*,*)stdu
      write(*,*)'stdv'
      write(*,*)stdv
      write(*,*)'HUM=',hum
      write(*,*)'PARMET:'
      write(*,*) IINF,ISUP,IMAX0,
     *           JINF,JSUP,JMAX0,
     *           IIPS,ISPS,JIPS,JSPS
      

      MIJK=IMAX0*JMAX0*KMAX

      ALLOCATE(UP(IMAX0,JMAX0,KMAX),VP(IMAX0,JMAX0,KMAX),
     *         X(MIJK,LMAX),XP(LMAX,MIJK),EVEC1(MIJK,LMAX))

      LVEC(1)=.TRUE.
      DO L=2,LMAX
      LVEC(L)=.FALSE.
      ENDDO


      IDI=INDEX(DIRI//' ',' ')-1
      IDA=INDEX(DIRA//' ',' ')-1
      IDO=INDEX(DIRO//' ',' ')-1
      INL=INDEX(NAMEL//' ',' ')-1
      IAL=INDEX(ANAME//' ',' ')-1
      IWO=INDEX(WINOUT//' ',' ')-1
      IWC=INDEX(WINPCM//' ',' ')-1
C*
      write(*,*)'unit=55',DIRI(1:IDI)//NAMEL(1:INL)
      OPEN(55,FILE=DIRI(1:IDI)//NAMEL(1:INL),STATUS='OLD')
      DO L=1,LMAX
      READ(55,'(A)')CNAME(L)
      READ(55,'(A)')PNAME(L)
      ENDDO
      CLOSE(55)
C*
      OPEN(66,FILE=DIRO(1:IDO)//WINOUT(1:IWO),
     *        STATUS='UNKNOWN')
C*
C*    READ IN THE DATA NEEDED
C*
      WRITE(*,'(A)')' BEGIN READING:'
      DO L=1,LMAX
      ICN=INDEX(CNAME(L)//' ',' ')-1
      IPN=INDEX(PNAME(L)//' ',' ')-1
      write(*,*)'unit10=cname=',CNAME(L)(1:ICN)
      write(*,*)'unit11=pname=',PNAME(L)(1:IPN)
      OPEN(10,FILE=CNAME(L)(1:ICN),STATUS='OLD',
     *        FORM='UNFORMATTED')
      OPEN(11,FILE=PNAME(L)(1:IPN),STATUS='OLD',
     *        FORM='UNFORMATTED')
CAMM  READ SURFACE PRESSURE                
      READ(10) BFUC
      READ(11) BFUP
CAMM  READ TEMPERATURE                
      DO K=1,KMAX
      READ(10) BFUC
      READ(11) BFUP
      ENDDO
CAMM  READ SPECIFIC HUMIDITY            
      DO K=1,KMAX
      READ(10) BFUC
      READ(11) BFUP
      ENDDO
CAMM  READ WINDS                           
      DO K=1,KMAX
      READ(10)BFUC
      READ(11)BFUP
      DO J=1,JMAX
      DO I=1,IMAX
      BFUCP(I,J,K)=BFUC(I,J)
      BFUPP(I,J,K)=BFUP(I,J)
      ENDDO
      ENDDO
      ENDDO
      DO K=1,KMAX
      READ(10)BFVC
      READ(11)BFVP
      DO J=1,JMAX
      DO I=1,IMAX
      BFVCP(I,J,K)=BFVC(I,J)
      BFVPP(I,J,K)=BFVP(I,J)
      ENDDO
      ENDDO
      ENDDO
      DO K=1,KMAX

      DO J=1,JMAX0
      DO I=1,IMAX0
      X(I+(J-1)*IMAX0+(K-1)*IMAX0*JMAX0,L)=
     *  CMPLX(BFUPP(I+IINF-1,J+JINF-1,K)-BFUCP(I+IINF-1,J+JINF-1,K),
     *  BFVPP(I+IINF-1,J+JINF-1,K)-BFVCP(I+IINF-1,J+JINF-1,K))
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
      XP(L,M)=CMPLX(REAL(X(M,L)),-AIMAG(X(M,L)))
      ENDDO
      ENDDO
      CALL ABCX(XP,X,XX,LMAX,MIJK,LMAX)
C*
      DO L=1,LMAX
      DO M=1,LMAX
      XR(M,L)=REAL(XX(M,L))
      XI(M,L)=AIMAG(XX(M,L))
      ENDDO
      ENDDO
      WRITE(66,'(A)')' '
      WRITE(66,'(A)')' MATRIX XX: REAL'
      DO M=1,LMAX
      WRITE(66,'(1P11G9.2)')(XR(M,L),L=1,LMAX)
      ENDDO
      WRITE(66,'(A)')' '
      WRITE(66,'(A)')' MATRIX XX: IMAGINARY'
      DO M=1,LMAX
      WRITE(66,'(1P11G9.2)')(XI(M,L),L=1,LMAX)
      ENDDO
C*
      CALL CHWZ(LMAX,LMAX,XR,XI,EVAL,ZR,ZI,W1,W2,W3,IERR)
      DO L=1,LMAX
      DO M=1,LMAX
      EVEC(M,L)=CMPLX(ZR(M,L),ZI(M,L))
      ENDDO
      ENDDO
C*
      WRITE(*,'(/,A,I5,/)')' IERR=',IERR
      WRITE(66,'(/,A,I5,/)')' IERR=',IERR
      WRITE(66,'(A)')' EVAL: '
      WRITE(66,'(1P11G9.2)')EVAL
      WRITE(66,'(A)')' '
      WRITE(66,'(A)')' EVEC: REAL'
      DO M=1,LMAX
      WRITE(66,'(1P11G9.2)')(ZR(M,L),L=1,LMAX)
      ENDDO
      WRITE(66,'(A)')' '
      WRITE(66,'(A)')' EVEC: IMAGINARY'
      DO M=1,LMAX
      WRITE(66,'(1P11G9.2)')(ZI(M,L),L=1,LMAX)
      ENDDO
C
CAMM calcula variancia total e a relativa e escreve em arquivo
C
      STOT=0.0
      DO M=1,LMAX
         STOT=STOT+EVAL(M)
      ENDDO
      WRITE(66,*)' '
      WRITE(66,*)'a variancia total eh:'
      WRITE(66,'(1P1G9.2)') STOT
      WRITE(66,*)'a percentagem da variancia explicada eh:'
      DO M=1,LMAX
         SREL(M)=EVAL(M)/STOT
         WRITE(66,'(A,I2)')'Autovalor ',M
         WRITE(66,'(1P1G9.2)') SREL(M)
      ENDDO
CAMM  
      ENERTPR=0.0
      ENERTPS=0.0
CAMM  
      DO L=1,LMAX
        EVAL1(L)=EVAL(L)
        CALL ABCX(X,EVEC(1,L),EVEC1(1,L),MIJK,LMAX,1)
      ENDDO
C
      WRITE(*,'(/,A)')' EIGENVALUES (FROM HIGH TO LOW)'
      WRITE(*,'(1P11G9.2)')(EVAL1(L),L=1,LMAX)
C
C    CHOOSE NTH EIGENVECTOR TO OUTPUT
C
      DO L=1,LMAX
      IF (LVEC(L)) THEN
        WRITE(66,'(/,A,I2,A,L6)')' LVEC(',L,') :',LVEC(L)

        DO K=1,KMAX
        DO J=1,JMAX0
        DO I=1,IMAX0
          UP(I,J,K)=REAL(EVEC1(I+(J-1)*IMAX0+(K-1)*IMAX0*JMAX0,L))/
     *              SQRT(EVAL1(L))
          VP(I,J,K)=AIMAG(EVEC1(I+(J-1)*IMAX0+(K-1)*IMAX0*JMAX0,L))/
     *              SQRT(EVAL1(L))
        ENDDO
        ENDDO
        ENDDO
C
C    RE-SCALE THE FIELDS
C
        DO K=1,KMAX
          CALL VARAVE(UP(1,1,K),IMAX0*JMAX0,UMIN,UMAX,UMN,USIG)
          CALL VARAVE(VP(1,1,k),IMAX0*JMAX0,VMIN,VMAX,VMN,VSIG)
          UVMN(K)=0.5*(UMN+VMN)
        ENDDO
        WRITE(66,'(/,A)')' UVMN:'
        WRITE(66,'(1P11G9.2)')(UVMN(K),K=1,KMAX)
        CALL VARAVE(UVMN,KMAX,VMIN,VMAX,VMN,USIG)
        WRITE(66,'(/,A,1PG9.2)')' VMN = ',VMN
C
        SIGMA2=0.0
        DO K=1,KMAX
        DO J=1,JMAX0
        DO I=1,IMAX0
          SIGMA2=SIGMA2+(UP(I,J,K)-VMN)*(UP(I,J,K)-VMN)
     *                 +(VP(I,J,K)-VMN)*(VP(I,J,K)-VMN)
        ENDDO
        ENDDO
        ENDDO
C
        SIGMA1=STDU(1)
        SIGMA2=SQRT(SIGMA2/FLOAT(MIJK))
        FACTOR=(SIGMA1/SIGMA2)
        WRITE(66,'(/,1P,2(A,G12.5))')
     *        ' SIGMA1 =',SIGMA1,' SIGMA2 =',SIGMA2,
     *        ' FACTOR =',FACTOR
C
C   Zonal Wind
C
        DO K=1,KMAX
          DO J=1,JINF-1
          DO I=1,IMAX
            TUP(I,J,K,L)=0.0
          ENDDO
          ENDDO
          DO J=JINF,JSUP  
          DO I=1,IMAX
            IF (I.GE.IINF .AND. I.LE.ISUP) THEN
              BFUPAUX=FACTOR*UP(I-IINF+1,J-JINF+1,K)
c              TUP(I,J,K,L)=BFUPAUX !It is necessary adjust the phase of the wind pert. to obtain balance with pres. pert.
              TUP(I,J,K,L)=-BFUPAUX
            ELSE
              TUP(I,J,K,L)=0.0
            ENDIF
          ENDDO
          ENDDO
          DO J=JSUP+1,JMAX
          DO I=1,IMAX
            TUP(I,J,K,L)=0.0
          ENDDO
          ENDDO
        ENDDO
C
C   Meridional Wind
C
        DO K=1,KMAX
          DO J=1,JINF-1
          DO I=1,IMAX
            TVP(I,J,K,L)=0.0
          ENDDO
          ENDDO
          DO J=JINF,JSUP
          DO I=1,IMAX
            IF (I.GE.IINF .AND. I.LE.ISUP) THEN
              BFVPAUX=FACTOR*VP(I-IINF+1,J-JINF+1,K)
c              TVP(I,J,K,L)=BFVPAUX  !It is necessary adjust the phase of the wind pert. to obtain balance with pres. pert.
              TVP(I,J,K,L)=-BFVPAUX 
            ELSE
              TVP(I,J,K,L)=0.0
            ENDIF 
          ENDDO
          ENDDO
          DO J=JSUP+1,JMAX
          DO I=1,IMAX
            TVP(I,J,K,L)=0.0
          ENDDO
          ENDDO
        ENDDO

      ENDIF
      ENDDO
C
C    WRITE ALL PERTURBED FIELDS FOR THE ANALISYS
C
      DO L=1,LMAX
      IF (LVEC(L)) THEN

      WRITE(66,'(/,A,I2,A,L6)')' LVEC(',L,') :',LVEC(L)

      IWP1=INDEX(WINPER1(L)//' ',' ')-1
      IPP1=INDEX(PRSSER1(L)//' ',' ')-1
      ITP1=INDEX(TEMPER1(L)//' ',' ')-1
      IHP1=INDEX(HUMPER1(L)//' ',' ')-1
      write(66,*)'unit72=winper1=',DIRO(1:IDO)//WINPER1(L)(1:IWP1)
      write(66,*)'unit74=winpen1=',DIRO(1:IDO)//WINPEN1(L)(1:IWP1)
      write(66,*)'unit60=prsser1=',DIRO(1:IDO)//PRSSER1(L)(1:ITP1)
      write(66,*)'unit61=prssen1=',DIRO(1:IDO)//PRSSEN1(L)(1:ITP1)
      write(66,*)'unit62=temper1=',DIRO(1:IDO)//TEMPER1(L)(1:ITP1)
      write(66,*)'unit64=tempen1=',DIRO(1:IDO)//TEMPEN1(L)(1:ITP1)
      write(66,*)'unit68=humper1=',DIRO(1:IDO)//HUMPER1(L)(1:IHP1)
      write(66,*)'unit69=humpen1=',DIRO(1:IDO)//HUMPEN1(L)(1:IHP1)
      OPEN(72,FILE=DIRO(1:IDO)//WINPER1(L)(1:IWP1),
     *        STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(74,FILE=DIRO(1:IDO)//WINPEN1(L)(1:IWP1),
     *        STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(60,FILE=DIRO(1:IDO)//PRSSER1(L)(1:IPP1),
     *        STATUS='OLD',FORM='UNFORMATTED')
      OPEN(61,FILE=DIRO(1:IDO)//PRSSEN1(L)(1:IPP1),
     *        STATUS='OLD',FORM='UNFORMATTED')
      OPEN(62,FILE=DIRO(1:IDO)//TEMPER1(L)(1:ITP1),
     *        STATUS='OLD',FORM='UNFORMATTED')
      OPEN(64,FILE=DIRO(1:IDO)//TEMPEN1(L)(1:ITP1),
     *        STATUS='OLD',FORM='UNFORMATTED')
      IF (HUM .EQ. 'YES') THEN
      write(*,*)'first YES'
      OPEN(68,FILE=DIRO(1:IDO)//HUMPER1(L)(1:IHP1),
     *        STATUS='OLD',FORM='UNFORMATTED')
      OPEN(69,FILE=DIRO(1:IDO)//HUMPEN1(L)(1:IHP1),
     *        STATUS='OLD',FORM='UNFORMATTED')
      ENDIF


      write(66,*)'unit20=',DIRA(1:IDA)//ANAME(1:IAL)
      OPEN(20,FILE=DIRA(1:IDA)//ANAME(1:IAL),STATUS='OLD',
     *        FORM='UNFORMATTED')
C
CAMM  READ SURFACE PRESSURE (UNIT=20 BFUC IS ELIMINATED)
C
        READ(20)BFUC
        READ(60)BFUC
        WRITE(72)BFUC
        READ(61)BFUC
        WRITE(74)BFUC
C
CAMM  READ TEMPERATURE (UNIT=20 BFUC IS ELIMINATED)
C
      DO K=1,KMAX
        READ(20)BFUC
        READ(62)BFUC
        WRITE(72)BFUC
        READ(64)BFUC
        WRITE(74)BFUC
      ENDDO
C
CAMM  READ HUMIDITY (UNIT=20 BFUC IS ELIMINATED, IF HUM=YES)
C
      DO K=1,KMAX
        READ(20)BFUC
        IF (HUM .EQ. 'YES') THEN
          READ(68)BFUC
          WRITE(72)BFUC
        ELSE
          WRITE(72)BFUC
        ENDIF
        IF (HUM .EQ. 'YES') THEN
          READ(69)BFUC
          WRITE(74)BFUC
        ELSE
          WRITE(74)BFUC
        ENDIF
      ENDDO
C
CAMM  READ WINDS 
C
C   Zonal Wind
C
      DO K=1,KMAX
      READ(20)BFUC
      DO J=1,JINF-1
      DO I=1,IMAX
      BFUP(I,J)=BFUC(I,J)
      BFUPN(I,J)=BFUC(I,J)
      ENDDO
      ENDDO
      DO J=JINF,JSUP  
      DO I=1,IMAX
        IF (I.GE.IINF .AND. I.LE.ISUP) THEN
          BFUP(I,J)=BFUC(I,J)+TUP(I,J,K,L)
          BFUPN(I,J)=BFUC(I,J)-TUP(I,J,K,L)
        ELSE
          BFUP(I,J)=BFUC(I,J)
          BFUPN(I,J)=BFUC(I,J)
        ENDIF
      ENDDO
      ENDDO
      DO J=JSUP+1,JMAX
      DO I=1,IMAX
      BFUP(I,J)=BFUC(I,J)
      BFUPN(I,J)=BFUC(I,J)
      ENDDO
      ENDDO
      WRITE(72) BFUP
      WRITE(74) BFUPN
      ENDDO
C
C   Meridional Wind
C
      DO K=1,KMAX
      READ(20)BFVC
      DO J=1,JINF-1
      DO I=1,IMAX
      BFVP(I,J)=BFVC(I,J)
      BFVPN(I,J)=BFVC(I,J)
      ENDDO
      ENDDO
      DO J=JINF,JSUP
      DO I=1,IMAX
        IF (I.GE.IINF .AND. I.LE.ISUP) THEN
          BFVP(I,J)=BFVC(I,J)+TVP(I,J,K,L)                        
          BFVPN(I,J)=BFVC(I,J)-TVP(I,J,K,L)   
        ELSE
          BFVP(I,J)=BFVC(I,J)
          BFVPN(I,J)=BFVC(I,J)
        ENDIF 
      ENDDO
      ENDDO
      DO J=JSUP+1,JMAX
      DO I=1,IMAX
      BFVP(I,J)=BFVC(I,J)
      BFVPN(I,J)=BFVC(I,J)
      ENDDO
      ENDDO
      WRITE(72) BFVP
      WRITE(74) BFVPN
      ENDDO
C
      CLOSE(20)
      CLOSE(72)
      CLOSE(74)
      CLOSE(60)
      CLOSE(61)
      CLOSE(62)
      CLOSE(64)
      CLOSE(68)
      CLOSE(69)

      ENDIF
      ENDDO
C
      DO L=1,LMAX
      DO M=1,MIJK
      XP(L,M)=CMPLX(REAL(EVEC1(M,L))/SQRT(EVAL1(L)),
     *            -AIMAG(EVEC1(M,L))/SQRT(EVAL1(L)))
      ENDDO
      ENDDO
      CALL ABCX(XP,X,Y,LMAX,MIJK,LMAX)
      DO L=1,LMAX
      DO M=1,LMAX
      YU(M,L)=REAL(Y(M,L))
      YV(M,L)=AIMAG(Y(M,L))
      ENDDO
      ENDDO
      CALL VARAVE(YU,LMAX*LMAX,UMIN,UMAX,UMN,USIG)
      CALL VARAVE(YV,LMAX*LMAX,VMIN,VMAX,VMN,VSIG)
      UMAX=MAX(ABS(UMIN),ABS(UMAX))
      VMAX=MAX(ABS(VMIN),ABS(VMAX))
      WRITE(66,'(/,1P,2(A,G12.5))')' UMAX =',UMAX,' VMAX =',VMAX
      DO L=1,LMAX
      DO M=1,LMAX
      YU(M,L)=YU(M,L)/UMAX
      YV(M,L)=YV(M,L)/UMAX
      ENDDO
      ENDDO
C*
      OPEN(70,FILE=DIRO(1:IDO)//WINPCM(1:IWC),
     *        STATUS='UNKNOWN')
      WRITE(70,'(/,A)') ' REAL PART'
      DO M=1,LMAX
      WRITE(70,'(/,A,I4)')' SET :',M
      DO L=1,LMAX
      WRITE(70,'(1PG12.5)') YU(M,L)
      ENDDO
      ENDDO
      WRITE(70,'(/,A)') ' IMAGINARY PART'
      DO M=1,LMAX
      WRITE(70,'(/,A,I4)')' SET :',M
      DO L=1,LMAX
      WRITE(70,'(1PG12.5)') YV(M,L)
      ENDDO
      ENDDO
      DO L=1,LMAX
      DO M=1,LMAX
      YU(M,L)=SQRT(YU(M,L)*YU(M,L)+YV(M,L)*YV(M,L))
      ENDDO
      ENDDO
      CALL VARAVE(YU,LMAX*LMAX,UMIN,UMAX,UMN,USIG)
      WRITE(66,'(/,1P,4(A,G12.5))')
     *         ' UMIN =',UMIN,' UMAX =',UMAX,
     *         ' UMN  =',UMN,' USIG =',USIG
      DO M=1,LMAX
      DO L=1,LMAX
      YU(M,L)=YU(M,L)/UMAX
      ENDDO
      ENDDO
      WRITE(70,'(A)') ' AMPLITUDE'
      DO M=1,LMAX
      WRITE(70,'(/,A,I4)')' SET :',M
      DO L=1,LMAX
      WRITE(70,'(1PG12.5)') YU(M,L)
      ENDDO
      ENDDO
      CLOSE(70)
C*
      CLOSE(66)
      STOP
      END
      
