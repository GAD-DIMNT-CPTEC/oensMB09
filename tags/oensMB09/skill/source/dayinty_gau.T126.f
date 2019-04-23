      PROGRAM DAYINT
C*
      INTEGER LONI,LATI
      PARAMETER (LONI=192,LATI=96)
C*
      INTEGER KMAX
      PARAMETER (KMAX=8)
C*
C*
C* /gfs/home3/modoper/tempo/global/oens/SKILL/CLIMA/
      INTEGER N,K,L,I,J,MONTH,IDAY,IUTC,
     *        IYEAR,JYEAR,MON,MF,MN
      REAL YDAY,ADD,F1,F2
      INTEGER MONL(12)
      CHARACTER CIYEAR*4,CMONTH*2,CIDAY*2,CIUTC*2
      CHARACTER*12 FNMI(12)
      CHARACTER*14 FNMD
      CHARACTER*49 DIRC
      REAL BUFA(LONI,LATI),BUFB(LONI,LATI),BUFI(LONI,LATI)
CAMM      NAMELIST /DAYNML/ IYEAR,MONTH,IDAY,IUTC
C*
      DATA MONL /31,28,31,30,31,30,31,31,30,31,30,31/
      DATA FNMI /'clima.gau.01','clima.gau.02','clima.gau.03',
     *           'clima.gau.04','clima.gau.05','clima.gau.06',
     *           'clima.gau.07','clima.gau.08','clima.gau.09',
     *           'clima.gau.10','clima.gau.11','clima.gau.12'/
      DATA FNMD /'clima.gau.T126'/
C*
CAMM       open(1,file='DAYNML',status='old')
CAMM       read(1,nml=daynml)
C*
CAMM      IF (IYEAR .GT. 1900) THEN
CAMM      JYEAR=IYEAR-1900
CAMM      ELSE
CAMM      JYEAR=IYEAR
CAMM      ENDIF
       CALL GETARG(1,CIYEAR)
       CALL GETARG(2,CMONTH)
       CALL GETARG(3,CIDAY)
       CALL GETARG(4,CIUTC)
       CALL GETARG(5,DIRC)
       READ(CIYEAR,'(I4)') IYEAR
       READ(CMONTH,'(I2)') MONTH
       READ(CIDAY,'(I2)') IDAY
       READ(CIUTC,'(I2)') IUTC
       write(*,*)'iyear= ',iyear
       write(*,*)'month= ',month
       write(*,*)'iday= ',iday
       write(*,*)'iutc= ',iutc
       JYEAR=IYEAR
C*
      MON=MONTH
      YDAY=FLOAT(IDAY)+FLOAT(IUTC)/24.0
      MF=MON-1
      IF(YDAY.GT.1.0+FLOAT(MONL(MON))/2.0)MF=MON
      MN=MF+1
      IF(MF.LT.1)MF=12
      IF(MN.GT.12)MN=1
      ADD=FLOAT(MONL(MF))/2.0-1.0
      IF(MF.EQ.MON)ADD=-ADD-2.0
      F1=2.0*(YDAY+ADD)/FLOAT(MONL(MF)+MONL(MN))
      F2=1.0-F1

C*    WRITE(FNMD(11:12),'(I2.2)')IDAY
C*    WRITE(FNMA(12:13),'(I2.2)')IDAY
C*
C*      DIRC='/gfs/home3/modoper/tempo/global/oenspro/produtos/SKILL/
C*     *CLIMA/'
      write(*,*)'file10= ',DIRC//FNMI(MF) 
      write(*,*)'file11= ',DIRC//FNMI(MN) 
      OPEN(10,FILE=DIRC//FNMI(MF),
     *        STATUS='OLD',
     *        FORM='UNFORMATTED',READONLY,
     *        ACCESS='DIRECT',RECL=LONI*LATI)
C*
      OPEN(11,FILE=DIRC//FNMI(MN),
     *        STATUS='OLD',
     *        FORM='UNFORMATTED',READONLY,
     *        ACCESS='DIRECT',RECL=LONI*LATI)
      OPEN(12,FILE=FNMD,STATUS='UNKNOWN',
     *        FORM='UNFORMATTED',
     *        ACCESS='DIRECT',RECL=LONI*LATI)
C*
      N=0
      DO K=1,8
      DO L=1,6
C*
      N=N+1
      READ(10,REC=N)((BUFA(I,J),I=1,LONI),J=1,LATI)
      READ(11,REC=N)((BUFB(I,J),I=1,LONI),J=1,LATI)
      DO J=1,LATI
      DO I=1,LONI
      BUFI(I,J)=F2*BUFA(I,J)+F1*BUFB(I,J)
      ENDDO
      ENDDO
C*
      WRITE(12,REC=N)((BUFI(I,J),I=1,LONI),J=1,LATI)
C*
      ENDDO
      ENDDO
C*
      CLOSE(10)
      CLOSE(11)
      CLOSE(12)
C*
      CALL CLMDY(IDAY,MONTH,IYEAR,IUTC)
C*
      STOP
      END
C*
      SUBROUTINE CLMDY(IDAY,MONTH,IYEAR,IUTC)
C*
      INTEGER IDAY,MONTH,IYEAR,IUTC,JYEAR,KYEAR
      CHARACTER*3 MON(12)
      DATA MON/'JAN','FEB','MAR','APR','MAY','JUN',
     *         'JUL','AUG','SEP','OCT','NOV','DEC'/
C*
      IF (IYEAR .GT. 1900) THEN
      JYEAR=IYEAR-1900
      KYEAR=IYEAR
      ELSE
      JYEAR=IYEAR
      KYEAR=IYEAR+1900
      ENDIF
C*
      OPEN(20,FILE='clima.gau.T126.ctl',STATUS='UNKNOWN')
C*
      WRITE(20,'(A20)')'DSET ^clima.gau.T126'
      WRITE(20,'(A1)')'*'
      WRITE(20,'(A23)')'OPTIONS YREV BIG_ENDIAN'
      WRITE(20,'(A1)')'*'
      WRITE(20,'(A14)')'UNDEF -2.56E33'
      WRITE(20,'(A1)')'*'
      WRITE(20,'(A22)')'TITLE Climatologia NMC'
      WRITE(20,'(A1)')'*'
      WRITE(20,'(A27)')'XDEF 192 LINEAR   0.0 1.875'
      WRITE(20,'(A15)')'YDEF  96 LEVELS'
      WRITE(20,'(A40)')' -88.57217 -86.72253 -84.86197 -82.99894'
      WRITE(20,'(A40)')' -81.13498 -79.27056 -77.40589 -75.54106'
      WRITE(20,'(A40)')' -73.67613 -71.81113 -69.94608 -68.08099'
      WRITE(20,'(A40)')' -66.21587 -64.35073 -62.48557 -60.62040' 
      WRITE(20,'(A40)')' -58.75521 -56.89001 -55.02481 -53.15960'
      WRITE(20,'(A40)')' -51.29438 -49.42915 -47.56393 -45.69869' 
      WRITE(20,'(A40)')' -43.83346 -41.96822 -40.10298 -38.23774'
      WRITE(20,'(A40)')' -36.37249 -34.50724 -32.64199 -30.77674' 
      WRITE(20,'(A40)')' -28.91149 -27.04624 -25.18099 -23.31573'
      WRITE(20,'(A40)')' -21.45048 -19.58522 -17.71996 -15.85470' 
      WRITE(20,'(A40)')' -13.98945 -12.12419 -10.25893  -8.39367'
      WRITE(20,'(A40)')'  -6.52841  -4.66315  -2.79789  -0.93263'
      WRITE(20,'(A40)')'   0.93263   2.79789   4.66315   6.52841'
      WRITE(20,'(A40)')'   8.39367  10.25893  12.12419  13.98945' 
      WRITE(20,'(A40)')'  15.85470  17.71996  19.58522  21.45048'
      WRITE(20,'(A40)')'  23.31573  25.18099  27.04624  28.91149'
      WRITE(20,'(A40)')'  30.77674  32.64199  34.50724  36.37249'
      WRITE(20,'(A40)')'  38.23774  40.10298  41.96822  43.83346'
      WRITE(20,'(A40)')'  45.69869  47.56393  49.42915  51.29438'
      WRITE(20,'(A40)')'  53.15960  55.02481  56.89001  58.75521'
      WRITE(20,'(A40)')'  60.62040  62.48557  64.35073  66.21587'
      WRITE(20,'(A40)')'  68.08099  69.94608  71.81113  73.67613'
      WRITE(20,'(A40)')'  75.54106  77.40589  79.27056  81.13498'
      WRITE(20,'(A40)')'  82.99894  84.86197  86.72253  88.57217'
      WRITE(20,'(A31)')'ZDEF  8 LEVELS 1000 850 700 500'
      WRITE(20,'(A31)')'               300 250 200 100'
      WRITE(20,'(A16,I2.2,A1,I2.2,A3,I4.4,A4)')
     *         'TDEF   1 LINEAR ',IUTC,'Z',IDAY,
     *          MON(MONTH),KYEAR,' 1DY'
      WRITE(20,'(A1)')'*'
      WRITE(20,'(A6)')'VARS 6'
      WRITE(20,'(A26)')'U   8 99 VENTO ZONAL (M/S)'
      WRITE(20,'(A31)')'V   8 99 VENTO MERIDIONAL (M/S)'
      WRITE(20,'(A32)')'TV  8 99 TEMPERATURA VIRTUAL (K)'
      WRITE(20,'(A32)')'Z   8 99 ALTURA GEOPOTENCIAL (M)'
      WRITE(20,'(A39)')'PSI 8 99 FUNCAO DE CORRENTE      (M2/S)'
      WRITE(20,'(A39)')'CHI 8 99 POTENCIAL DE VELOCIDADE (M2/S)'
      WRITE(20,'(A7)')'ENDVARS'
C*
      CLOSE(20)
C*
      RETURN
      END
