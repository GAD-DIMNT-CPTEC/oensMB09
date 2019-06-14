      PROGRAM EOFWIN
C*
C*    COMPLEX (U+iV) WIND EOF ANALYSIS
C*
#include <reseofes.inc>
C*
      INTEGER MIJK
      PARAMETER (MIJK=IMAX*JMAX0*KMAX)
C*
      INTEGER I,J,K,L,M,IERR,IDI,IDO,IDA,
     *        INL,IAL,IPN,ICN,IWO,IWC,IWP1,IWP2,
     *        ITP1,IHP1
      REAL UMIN,VMIN,UMAX,VMAX,UMN,VMN,
     *     USIG,VSIG,SIGMA1,SIGMA2,FACTOR
      REAL UP(IMAX,JMAX0,KMAX),VP(IMAX,JMAX0,KMAX)
      REAL UVMN(KMAX),STDU(KMAX),STDV(KMAX)
      COMPLEX X(MIJK,LMAX),XP(LMAX,MIJK),XX(LMAX,LMAX)
      COMPLEX EVEC(LMAX,LMAX),EVEC1(MIJK,LMAX),Y(LMAX,LMAX)
      REAL EVAL(LMAX),EVAL1(LMAX),YU(LMAX,LMAX),YV(LMAX,LMAX)
      REAL XR(LMAX,LMAX),XI(LMAX,LMAX),ZR(LMAX,LMAX),ZI(LMAX,LMAX),
     *     W1(LMAX),W2(LMAX),W3(2,LMAX)
      LOGICAL LVEC(LMAX)
      CHARACTER HUM*3
      CHARACTER DIRI*64,DIRA*64,DIRO*64,NAMEL*64,ANAME*84,
     *          WINOUT*64,WINPCM*64,WINPER1*64,
     *          WINPEN1*64,
     *          TEMPER1*64,TEMPEN1*64,
     *          HUMPER1*64,HUMPEN1*64
      CHARACTER CNAME(LMAX)*184,PNAME(LMAX)*184
      REAL*4 BFUC(IMAX,JMAX),BFUP(IMAX,JMAX),
     *       BFUPN(IMAX,JMAX),BFUPAUX,
     *       BFVC(IMAX,JMAX),BFVP(IMAX,JMAX),
     *       BFVPN(IMAX,JMAX),BFVPAUX
      REAL*4 BFUCP(IMAX,JMAX,KMAX),BFUPP(IMAX,JMAX,KMAX),
     *       BFVCP(IMAX,JMAX,KMAX),BFVPP(IMAX,JMAX,KMAX)
C*
      NAMELIST /DATAIN/ DIRI,DIRA,DIRO,NAMEL,ANAME,
     *                  WINOUT,WINPCM,WINPER1,
     *                  WINPEN1,
     *                  TEMPER1,TEMPEN1,
     *                  HUMPER1,HUMPEN1
      NAMELIST /STZWIN/ STDU
      NAMELIST /STMWIN/ STDV
      NAMELIST /HUMIDI/ HUM
C*
      LVEC(1)=.TRUE.
c*      LVEC(2)=.TRUE.
      DO L=2,LMAX
      LVEC(L)=.FALSE.
      ENDDO
C*
      READ(*,DATAIN)
      IDI=INDEX(DIRI//' ',' ')-1
      IDA=INDEX(DIRA//' ',' ')-1
      IDO=INDEX(DIRO//' ',' ')-1
      INL=INDEX(NAMEL//' ',' ')-1
      IAL=INDEX(ANAME//' ',' ')-1
      IWO=INDEX(WINOUT//' ',' ')-1
      IWC=INDEX(WINPCM//' ',' ')-1
      IWP1=INDEX(WINPER1//' ',' ')-1
      ITP1=INDEX(TEMPER1//' ',' ')-1
      IHP1=INDEX(HUMPER1//' ',' ')-1
C*
      READ(*,STZWIN)
      READ(*,STMWIN)
      READ(*,HUMIDI)
      write(*,*)'stdu'
      write(*,*)stdu
      write(*,*)'stdv'
      write(*,*)stdv
      write(*,*)'HUM=',hum
c
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
cAMM      write(*,*)'unit10=cname=',CNAME(L)(1:ICN)
cAMM      write(*,*)'unit11=pname=',PNAME(L)(1:IPN)
      OPEN(10,FILE=CNAME(L)(1:ICN),STATUS='OLD',
     *        FORM='UNFORMATTED')
      OPEN(11,FILE=PNAME(L)(1:IPN),STATUS='OLD',
     *        FORM='UNFORMATTED')
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
cAMM      READ(10) BFUC
cAMM      READ(10) BFVC
cAMM      READ(11) BFUP
cAMM      READ(11) BFVP
      DO J=1,JMAX0
      DO I=1,IMAX
      X(I+(J-1)*IMAX+(K-1)*IMAX*JMAX0,L)=
     *  CMPLX(BFUPP(I,J+JINF-1,K)-BFUCP(I,J+JINF-1,K),
     *  BFVPP(I,J+JINF-1,K)-BFVCP(I,J+JINF-1,K))
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
C*
      DO L=1,LMAX
      EVAL1(L)=EVAL(L)
      CALL ABCX(X,EVEC(1,L),EVEC1(1,L),MIJK,LMAX,1)
      ENDDO
C*
      WRITE(*,'(/,A)')' EIGENVALUES (FROM HIGH TO LOW)'
      WRITE(*,'(1P11G9.2)')(EVAL1(L),L=1,LMAX)
C*
C*    CHOOSE NTH EIGENVECTOR TO OUTPUT
C*
      write(*,*)'unit72=winper1=',DIRO(1:IDO)//WINPER1(1:IWP1)
      write(*,*)'unit74=winpen1=',DIRO(1:IDO)//WINPEN1(1:IWP1)
      write(*,*)'unit62=temper1=',DIRO(1:IDO)//TEMPER1(1:ITP1)
      write(*,*)'unit64=tempen1=',DIRO(1:IDO)//TEMPEN1(1:ITP1)
      write(*,*)'unit68=humper1=',DIRO(1:IDO)//HUMPER1(1:IHP1)
      write(*,*)'unit69=humpen1=',DIRO(1:IDO)//HUMPEN1(1:IHP1)
      OPEN(72,FILE=DIRO(1:IDO)//WINPER1(1:IWP1),
     *        STATUS='UNKNOWN',FORM='UNFORMATTED')
c*      OPEN(73,FILE=DIRO(1:IDO)//WINPER2(1:IWP2),
c*    *        STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(74,FILE=DIRO(1:IDO)//WINPEN1(1:IWP1),
     *        STATUS='UNKNOWN',FORM='UNFORMATTED')
c*      OPEN(75,FILE=DIRO(1:IDO)//WINPEN2(1:IWP2),
c*    *        STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(62,FILE=DIRO(1:IDO)//TEMPER1(1:ITP1),
     *        STATUS='OLD',FORM='UNFORMATTED')
c*      OPEN(63,FILE=DIRO(1:IDO)//TEMPER2(1:ITP2),
c*    *        STATUS='OLD',FORM='UNFORMATTED')
      OPEN(64,FILE=DIRO(1:IDO)//TEMPEN1(1:ITP1),
     *        STATUS='OLD',FORM='UNFORMATTED')
c*      OPEN(65,FILE=DIRO(1:IDO)//TEMPEN2(1:ITP2),
c*    *        STATUS='OLD',FORM='UNFORMATTED')
      IF (HUM .EQ. 'YES') THEN
      write(*,*)'passou pelo primeiro YES'
      OPEN(68,FILE=DIRO(1:IDO)//HUMPER1(1:IHP1),
     *        STATUS='OLD',FORM='UNFORMATTED')
      OPEN(69,FILE=DIRO(1:IDO)//HUMPEN1(1:IHP1),
     *        STATUS='OLD',FORM='UNFORMATTED')
      ENDIF

      DO L=1,LMAX
      IF (LVEC(L)) THEN
      write(*,*)' LVEC(',L,') :',LVEC(L)
      WRITE(66,'(/,A,I2,A,L6)')' LVEC(',L,') :',LVEC(L)
C*
      DO K=1,KMAX
      DO J=1,JMAX0
      DO I=1,IMAX
      UP(I,J,K)=REAL(EVEC1(I+(J-1)*IMAX+(K-1)*IMAX*JMAX0,L))/
     *          SQRT(EVAL1(L))
      VP(I,J,K)=AIMAG(EVEC1(I+(J-1)*IMAX+(K-1)*IMAX*JMAX0,L))/
     *          SQRT(EVAL1(L))
      ENDDO
      ENDDO
      ENDDO
C*
C*    RE-SCALE THE FIELDS
C*
      DO K=1,KMAX
      CALL VARAVE(UP(1,1,K),IMAX*JMAX0,UMIN,UMAX,UMN,USIG)
      CALL VARAVE(VP(1,1,k),IMAX*JMAX0,VMIN,VMAX,VMN,VSIG)
      UVMN(K)=0.5*(UMN+VMN)
      ENDDO
      WRITE(66,'(/,A)')' UVMN:'
      WRITE(66,'(1P11G9.2)')(UVMN(K),K=1,KMAX)
      CALL VARAVE(UVMN,KMAX,VMIN,VMAX,VMN,USIG)
      WRITE(66,'(/,A,1PG9.2)')' VMN = ',VMN
C*
      SIGMA2=0.0
      DO K=1,KMAX
      DO J=1,JMAX0
      DO I=1,IMAX
      SIGMA2=SIGMA2+(UP(I,J,K)-VMN)*(UP(I,J,K)-VMN)
     *             +(VP(I,J,K)-VMN)*(VP(I,J,K)-VMN)
      ENDDO
      ENDDO
      ENDDO
c*MMC      SIGMA1=1.5
cAMM      SIGMA1=5.0
      SIGMA1=STDU(1)
      SIGMA2=SQRT(SIGMA2/FLOAT(MIJK))
      FACTOR=(SIGMA1/SIGMA2)
      WRITE(66,'(/,1P,2(A,G12.5))')
     *        ' SIGMA1 =',SIGMA1,' SIGMA2 =',SIGMA2,
     *        ' FACTOR =',FACTOR
C*
      write(*,*)'unit20=',DIRA(1:IDA)//ANAME(1:IAL)
      OPEN(20,FILE=DIRA(1:IDA)//ANAME(1:IAL),STATUS='OLD',
     *        FORM='UNFORMATTED')
     
c*MMC
cAMM  READ TEMPERATURE (UNIT=20 BFUC IS ELIMINATED)
      DO K=1,KMAX
      READ(20)BFUC
      READ(61+L)BFUC
      WRITE(71+L)BFUC
      READ(61+L+2)BFUC
      WRITE(71+L+2)BFUC
      ENDDO

cAMM  READ HUMIDITY (UNIT=20 BFUC IS ELIMINATED, IF HUM=YES)
      DO K=1,KMAX
      READ(20)BFUC
      IF (HUM .EQ. 'YES') THEN
      write(*,*)'passou pelo segundo YES'
      READ(68)BFUC
      WRITE(71+L)BFUC
      ELSE
      WRITE(71+L)BFUC
      ENDIF
      IF (HUM .EQ. 'YES') THEN
      write(*,*)'passou pelo terceiro YES'
      READ(69)BFUC
      WRITE(71+L+2)BFUC
      ELSE
      WRITE(71+L+2)BFUC
      ENDIF
      ENDDO

cAMM  READ WINDS 
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
      BFUPAUX=FACTOR*UP(I,J-JINF+1,K)
      BFUP(I,J)=BFUC(I,J)+BFUPAUX
      BFUPN(I,J)=BFUC(I,J)-BFUPAUX
      ENDDO
      ENDDO
      DO J=JSUP+1,JMAX
      DO I=1,IMAX
      BFUP(I,J)=BFUC(I,J)
      BFUPN(I,J)=BFUC(I,J)
      ENDDO
      ENDDO
      WRITE(71+L) BFUP
      WRITE(71+L+2) BFUPN
      ENDDO
c*
c*MMC
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
      BFVPAUX=FACTOR*VP(I,J-JINF+1,K)
      BFVP(I,J)=BFVC(I,J)+BFVPAUX                        
      BFVPN(I,J)=BFVC(I,J)-BFVPAUX 
      ENDDO
      ENDDO
      DO J=JSUP+1,JMAX
      DO I=1,IMAX
      BFVP(I,J)=BFVC(I,J)
      BFVPN(I,J)=BFVC(I,J)
      ENDDO
      ENDDO
      WRITE(71+L) BFVP
      WRITE(71+L+2) BFVPN
      ENDDO

C*
      CLOSE(20)
      ENDIF
      ENDDO
      CLOSE(72)
      CLOSE(74)
      CLOSE(62)
      CLOSE(64)
      CLOSE(68)
      CLOSE(69)
C*
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
