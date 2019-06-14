      PROGRAM DAYINT
C*
      INTEGER LONI,LATI,NV,NM
      PARAMETER (LONI=144,LATI=73,NV=4*17+12+8,NM=NV+2)
C*
      INTEGER N,I,J,MONTH,IDAY,IUTC,IYEAR,MON,
     *        MF,MN,IDC,IDP,MNL,MNLF,MNLN,NN,IDS
      REAL YDAY,ADD,F1,F2,CT,FA,FB
      CHARACTER CYEAR*4,CMONTH*2,CDAY*2,CUTC*2,NF*2
      CHARACTER NFCT*10,NICN*15,NCLD*15,NPST*10,FNMANL*40,
     *          DCLM*256,DPOS*256,TRUNC*7,ANL*7
      LOGICAL EXANL
      INTEGER MONL(12)
      CHARACTER*12 NCLM(12)
      REAL BUFA(LONI,LATI),BUFB(LONI,LATI),BUFI(LONI,LATI)
C*
      DATA MONL /31,28,31,30,31,30,31,31,30,31,30,31/
      DATA IDS /7/
      DATA NCLM /'g3dclm01.dat','g3dclm02.dat','g3dclm03.dat',
     *           'g3dclm04.dat','g3dclm05.dat','g3dclm06.dat',
     *           'g3dclm07.dat','g3dclm08.dat','g3dclm09.dat',
     *           'g3dclm10.dat','g3dclm11.dat','g3dclm12.dat'/
      DATA NCLD /'clmreannmc.T126'/, NFCT /'gposnmcfct'/,
     *     NICN /'gposnmcicn'/, NPST /'gposnmcpst'/
C*
      CALL GETARG(1,CYEAR)
      CALL GETARG(2,CMONTH)
      CALL GETARG(3,CDAY)
      CALL GETARG(4,CUTC)
C*
      ANL='GPOSAVN'
      TRUNC='T126L28'
      DPOS='/rede/nas/modoper/tempo/global/oens/nmc/T126L28/GPOS/'
      DPOS(54:57)=CYEAR
      DPOS(58:58)='/'
      DPOS(59:60)=CMONTH
      DPOS(61:61)='/'
      DPOS(62:63)=CDAY  
      DPOS(64:64)='/'
      WRITE(*,*) "DPOS=",DPOS
      IDP=INDEX(DPOS//' ',' ')-1
      DCLM='/gfs/home3/modoper/tempo/global/oens/SKILL/CLIMA/'
      IDC=INDEX(DCLM//' ',' ')-1
      write(*,*)DCLM
C*
      READ(CYEAR,'(I4)')IYEAR
      READ(CMONTH,'(I2)')MONTH
      READ(CDAY,'(I2)')IDAY
      READ(CUTC,'(I2)')IUTC
      WRITE(*,'(A,I4.4,3I2.2)')' LABEL = ',IYEAR,MONTH,IDAY,IUTC
C*
      FNMANL(1:7)=ANL
      WRITE(FNMANL(8:27),'(2(I4.4,3I2.2))')
     *      IYEAR,MONTH,IDAY,IUTC,IYEAR,MONTH,IDAY,IUTC
      FNMANL(28:33)='P.icn.'
      FNMANL(34:40)=TRUNC
CAMM      OPEN(97,FILE='filepst',STATUS='UNKNOWN')
CAMM      OPEN(98,FILE='filefct',STATUS='UNKNOWN')
      OPEN(99,FILE='fileinput.T126',STATUS='UNKNOWN')
      write(*,'(a)')DPOS(1:IDP)//FNMANL//'.grb'
      INQUIRE(FILE=DPOS(1:IDP)//FNMANL//'.grb',EXIST=EXANL)
CAMM      write(*,'(a)')DPOS(1:IDP)//FNMANL
CAMM      INQUIRE(FILE=DPOS(1:IDP)//FNMANL,EXIST=EXANL)
      IF (EXANL) THEN
      WRITE(99,'(A)')'avail'
      WRITE(99,'(A)')FNMANL//'.grb'
CAMM      WRITE(99,'(A)')FNMANL
CAMM      WRITE(99,'(A)')NCLD//'.ctl'
CAMM      WRITE(99,'(A)')NICN//'.ctl'
CAMM      DO NN=0,IDS-1
CAMM      WRITE(NF,'(I2.2)')NN
CAMM      WRITE(99,'(A)')NFCT//NF//'.ctl'
CAMM      ENDDO
CAMM      DO NN=0,IDS-1
CAMM      WRITE(NF,'(I2.2)')NN
CAMM      WRITE(99,'(A)')NPST//NF//'.ctl'
CAMM      ENDDO
      ELSE
      WRITE(99,'(A)')'unavl'
      WRITE(99,'(A)')FNMANL//'.grb'
CAMM      WRITE(99,'(A)')FNMANL
      STOP 'Input Analysis File Error:'
      ENDIF
C*
      MON=MONTH
      YDAY=FLOAT(IDAY)+FLOAT(IUTC)/24.0
      MF=MON-1
      MNL=MONL(MON)
      IF (MOD(IYEAR,4).EQ.0 .AND. MON.EQ.2) MNL=29
      IF (YDAY .GT. 1.0+FLOAT(MNL)/2.0) MF=MON
      MN=MF+1
      IF (MF .LT. 1) MF=12
      IF (MN .GT. 12) MN=1
      MNLF=MONL(MF)
      IF (MOD(IYEAR,4).EQ.0 .AND. MF.EQ.2) MNLF=29
      ADD=FLOAT(MNLF)/2.0-1.0
      IF (MF .EQ. MON) ADD=-ADD-2.0
      MNLN=MONL(MN)
      IF (MOD(IYEAR,4).EQ.0 .AND. MN.EQ.2) MNLN=29
      F1=2.0*(YDAY+ADD)/FLOAT(MNLF+MNLN)
      F2=1.0-F1
      WRITE(*,'(2(A,F7.4))')' F1 = ',F1,'  F2 = ',F2
C*
      OPEN(10,FILE=DCLM(1:IDC)//NCLM(MF),STATUS='OLD',
     *        FORM='UNFORMATTED',READONLY,
     *        ACCESS='DIRECT',RECL=LONI*LATI)
      OPEN(11,FILE=DCLM(1:IDC)//NCLM(MN),STATUS='OLD',
     *        FORM='UNFORMATTED',READONLY,
     *        ACCESS='DIRECT',RECL=LONI*LATI)
      OPEN(12,FILE=NCLD,STATUS='UNKNOWN',
     *        FORM='UNFORMATTED',
     *        ACCESS='DIRECT',RECL=LONI*LATI)
C*
      DO N=1,NM
C*
      READ(10,REC=N)((BUFA(I,J),I=1,LONI),J=1,LATI)
      READ(11,REC=N)((BUFB(I,J),I=1,LONI),J=1,LATI)
C*
      CT=1.0
      IF (N .EQ. NV+2) CT=0.01
      FA=CT*F2
      FB=CT*F1
      DO J=1,LATI
      DO I=1,LONI
      BUFI(I,J)=FA*BUFA(I,J)+FB*BUFB(I,J)
      ENDDO
      ENDDO
C*
      WRITE(12,REC=N)((BUFI(I,J),I=1,LONI),J=1,LATI)
C*
      ENDDO
C*
      CLOSE(10)
      CLOSE(11)
      CLOSE(12)
C*
      CALL CLMCTL(IDAY,MONTH,IYEAR,IUTC,NCLD)
CAMM      CALL FCTCTL(IDAY,MONTH,IYEAR,IUTC,NFCT,DPOS,IDP,ANL,TRUNC)
CAMM      CALL ICNCTL(IDAY,MONTH,IYEAR,IUTC,NICN,DPOS,IDP,ANL,TRUNC)
CAMM      CALL PSTCTL(IDAY,MONTH,IYEAR,IUTC,NPST,DPOS,IDP,ANL,TRUNC)
C*
      STOP" *** End of Climatology Interpolation ***"
      END
C*
      SUBROUTINE CLMCTL(IDAY,MONTH,IYEAR,IUTC,NCLD)
C*
      INTEGER IDAY,MONTH,IYEAR,IUTC
      CHARACTER NCLD*15
      CHARACTER MON(12)*3
      DATA MON /'JAN','FEB','MAR','APR','MAY','JUN',
     *          'JUL','AUG','SEP','OCT','NOV','DEC'/
C*
      OPEN(20,FILE=NCLD//'.ctl',STATUS='UNKNOWN')
C*
      WRITE(20,'(A)')'DSET ^'//NCLD
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'OPTIONS BIG_ENDIAN'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'UNDEF 1e+20'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'TITLE NCEP/NCAR REANALYSIS CLIMATOLOGY: '//
     *               'JAN82-DEC94 '
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'XDEF 144 LINEAR   0.0 2.5'
      WRITE(20,'(A)')'YDEF  73 LINEAR -90.0 2.5'
      WRITE(20,'(A)')'ZDEF  17 LEVELS 1000 925 850 700 600 500 '
      WRITE(20,'(A)')'                 400 300 250 200 150 100 '
      WRITE(20,'(A)')'                      70  50  30  20  10'
      WRITE(20,'(A,I2.2,A,I2.2,A,I4.4,A)')
     *         'TDEF   1 LINEAR ',IUTC,'Z',IDAY,
     *          MON(MONTH),IYEAR,' 1DY'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'VARS 8'
      WRITE(20,'(A)')'ZGEO 17 99 Geopotential height (gpm)'
      WRITE(20,'(A)')'UVEL 17 99 u wind (m/s)'
      WRITE(20,'(A)')'VVEL 17 99 v wind (m/s)'
      WRITE(20,'(A)')'TEMP 17 99 Temperature (K)'
      WRITE(20,'(A)')'OMEG 12 99 Pressure vertical velocity (Pa/s)'
      WRITE(20,'(A)')'UMES  8 99 Specific humidity (kg/kg)'
      WRITE(20,'(A)')'AGPL  0 99 Precipitable water (kg/m**2)'
      WRITE(20,'(A)')'PSNM  0 99 Pressure reduced to MSL (hPa)'
      WRITE(20,'(A)')'ENDVARS'
C*
      CLOSE(20)
C*
      RETURN
      END
C*
      SUBROUTINE FCTCTL(IDAY,MONTH,IYEAR,IUTC,
     *                  NFCT,DPOS,IDP,ANL,TRUNC)
C*
      INTEGER IDAY,MONTH,IYEAR,IUTC,
     *        KYEAR,JDAY,MN,IDS,IDP,
     *        NDAY,MDAY,LDAY,LMN,LYEAR
      CHARACTER NFCT*10,DPOS*256,ANL*7,TRUNC*7,NF*2
      INTEGER MONL(12)
      CHARACTER MON(12)*3
      DATA IDS /7/
      DATA MONL /31,28,31,30,31,30,31,31,30,31,30,31/
      DATA MON /'JAN','FEB','MAR','APR','MAY','JUN',
     *          'JUL','AUG','SEP','OCT','NOV','DEC'/
C*
      IF (MOD (IYEAR,4) .EQ. 0) MONL(2)=29
      KYEAR=IYEAR
      IF (IDAY .LE. IDS) THEN
      MN=MONTH-1
      IF (MN .EQ. 0) THEN
      MN=12
      KYEAR=KYEAR-1
      ENDIF
      JDAY=MONL(MN)-(IDS-IDAY)
      ELSE
      MN=MONTH
      JDAY=IDAY-IDS
      ENDIF
C*
      LDAY=IDAY+1
      LMN=MONTH
      LYEAR=IYEAR
      IF (LDAY .GT. MONL(LMN)) THEN
      LDAY=1
      LMN=LMN+1
      IF (LMN .GT. 12) THEN
      LMN=1
      LYEAR=LYEAR+1
      ENDIF
      ENDIF
C*
      NDAY=JDAY
      DO MDAY=JDAY,JDAY+IDS-1
      IF (NDAY .GT. MONL(MN)) THEN
      NDAY=MDAY-MONL(MN)
      MN=MN+1
      IF (MN .GT. 12) THEN
      MN=1
      KYEAR=KYEAR+1
      ENDIF
      ENDIF
      WRITE(NF,'(I2.2)')MDAY-JDAY
C*
      OPEN(20,FILE=NFCT//NF//'.ctl',STATUS='UNKNOWN')
C*
      WRITE(98,'(A,2(I4.4,3I2.2),A)')
     *          ANL,KYEAR,MN,NDAY,IUTC,
     *          IYEAR,MONTH,IDAY,IUTC,'P.fct.'//TRUNC
CAMM     *          IYEAR,MONTH,IDAY,IUTC,'P.fct.'//TRUNC//'.grb'
      WRITE(20,'(A,2(I4.4,3I2.2),A)')
     *         'DSET '//ANL,KYEAR,MN,NDAY,IUTC,
     *          IYEAR,MONTH,IDAY,IUTC,'P.fct.'//TRUNC
CAMM     *          IYEAR,MONTH,IDAY,IUTC,'P.fct.'//TRUNC//'.grb'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'OPTIONS YREV'
      WRITE(20,'(A)')'DTYPE GRIB'
      WRITE(20,'(A)')'INDEX '//NFCT//NF//'.gmp'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'UNDEF 1e+20'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'TITLE Forecasted Fields'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'XDEF 192 LINEAR   0.0 1.875'
      WRITE(20,'(A)')'YDEF  96 LEVELS '
      WRITE(20,'(A)')'-88.572 -86.723 -84.862 -82.999 -81.135 '
      WRITE(20,'(A)')'-79.271 -77.406 -75.541 -73.676 -71.811 '
      WRITE(20,'(A)')'-69.946 -68.081 -66.216 -64.351 -62.486 '
      WRITE(20,'(A)')'-60.620 -58.755 -56.890 -55.025 -53.160 '
      WRITE(20,'(A)')'-51.294 -49.429 -47.564 -45.699 -43.833 '
      WRITE(20,'(A)')'-41.968 -40.103 -38.238 -36.372 -34.507 '
      WRITE(20,'(A)')'-32.642 -30.777 -28.911 -27.046 -25.181 '
      WRITE(20,'(A)')'-23.316 -21.450 -19.585 -17.720 -15.855 '
      WRITE(20,'(A)')'-13.989 -12.124 -10.259  -8.394  -6.528 '
      WRITE(20,'(A)')' -4.663  -2.798  -0.933   0.933   2.798 '
      WRITE(20,'(A)')'  4.663   6.528   8.394  10.259  12.124 '
      WRITE(20,'(A)')' 13.989  15.855  17.720  19.585  21.450 '
      WRITE(20,'(A)')' 23.316  25.181  27.046  28.911  30.777 '
      WRITE(20,'(A)')' 32.642  34.507  36.372  38.238  40.103 '
      WRITE(20,'(A)')' 41.968  43.833  45.699  47.564  49.429 '
      WRITE(20,'(A)')' 51.294  53.160  55.025  56.890  58.755 '
      WRITE(20,'(A)')' 60.620  62.486  64.351  66.216  68.081 '
      WRITE(20,'(A)')' 69.946  71.811  73.676  75.541  77.406 '
      WRITE(20,'(A)')' 79.271  81.135  82.999  84.862  86.723 '
      WRITE(20,'(A)')' 88.572'
      WRITE(20,'(A)')'ZDEF  17 LEVELS 1000 925 850 700 500 400 '
      WRITE(20,'(A)')'                 300 250 200 150 100  70 '
      WRITE(20,'(A)')'                      50  30  20  10   3'
      WRITE(20,'(A,I3,A,I2.2,A,I2.2,A,I4.4,A)')
     *         'TDEF ',1,' LINEAR ',
     *          IUTC,'Z',IDAY,MON(MONTH),IYEAR,' 1DY'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'VARS 42'
      WRITE(20,'(A)')'topo  0 132,1,0,0 '//
     *               'TOPOGRAPHY [m]'
      WRITE(20,'(A)')'lsmk  0  81,1,0,0 '//
     *               'LAND SEA MASK [0,1]'
      WRITE(20,'(A)')'pslc  0 135,1,0,0 '//
     *               'SURFACE PRESSURE [hPa]'
      WRITE(20,'(A)')'uves  0 192,1,0,0 '//
     *               'SURFACE ZONAL WIND (U) [m/s]'
      WRITE(20,'(A)')'vves  0 194,1,0,0 '//
     *               'SURFACE MERIDIONAL WIND (V) [m/s]'
      WRITE(20,'(A)')'psnm  0   2,102,0,0 '//
     *               'SEA LEVEL PRESSURE [hPa]'
      WRITE(20,'(A)')'tems  0 188,1,0,0 '//
     *               'SURFACE ABSOLUTE TEMPERATURE [K]'
      WRITE(20,'(A)')'umrs  0 226,1,0,0 '//
     *               'SURFACE RELATIVE HUMIDITY [no Dim]'
      WRITE(20,'(A)')'agpl  0  54,200,0,0 '//
     *               'INST. PRECIPITABLE WATER [Kg/m2]'
      WRITE(20,'(A)')'tsfc  0 187,1,0,0 '//
     *               'SURFACE TEMPERATURE [K]'
      WRITE(20,'(A)')'psmt  0 250,1,0,0 '//
     *               'TIME MEAN SURFACE PRESSURE [hPa]'
      WRITE(20,'(A)')'usmt  0 230,1,0,0 '//
     *               'TIME MEAN SURFACE ZONAL WIND (U) [m/s]'
      WRITE(20,'(A)')'vsmt  0 231,1,0,0 '//
     *               'TIME MEAN SURFACE MERIDIONAL WIND (V) [m/s]'
      WRITE(20,'(A)')'tsmt  0 232,1,0,0 '//
     *               'TIME MEAN SURFACE ABSOLUTE TEMPERATURE [K]'
      WRITE(20,'(A)')'rsmt  0 233,1,0,0 '//
     *               'TIME MEAN SURFACE RELATIVE HUMIDITY [no Dim]'
      WRITE(20,'(A)')'prec  0  61,1,0,0 '//
     *               'TOTAL PRECIPITATION [Kg/m2/day]'
      WRITE(20,'(A)')'prcv  0  63,1,0,0 '//
     *               'CONVECTIVE PRECIPITATION [Kg/m2/day]'
      WRITE(20,'(A)')'cbnv  0  71,3,0,0 '//
     *               'CLOUD COVER [0-1]'
      WRITE(20,'(A)')'role  0 114,8,0,0 '//
     *               'OUTGOING LONG WAVE AT TOP [W/m2]'
      WRITE(20,'(A)')'mask 17 137,100 '//
     *               'MASK [-/+]'
      WRITE(20,'(A)')'uvel 17  33,100 '//
     *               'ZONAL WIND (U) [m/s]'
      WRITE(20,'(A)')'vvel 17  34,100 '//
     *               'MERIDIONAL WIND (V) [m/s]'
      WRITE(20,'(A)')'omeg 17  39,100 '//
     *               'OMEGA [Pa/s]'
      WRITE(20,'(A)')'divg 17  44,100 '//
     *               'DIVERGENCE [1/s]'
      WRITE(20,'(A)')'vort 17  43,100 '//
     *               'VORTICITY [1/s]'
      WRITE(20,'(A)')'fcor 17  35,100 '//
     *               'STREAM FUNCTION [m2/s]'
      WRITE(20,'(A)')'potv 17  36,100 '//
     *               'VELOCITY POTENTIAL [m2/s]'
      WRITE(20,'(A)')'zgeo 17   7,100 '//
     *               'GEOPOTENTIAL HEIGHT [gpm]'
      WRITE(20,'(A)')'temp 17  11,100 '//
     *               'ABSOLUTE TEMPERATURE [K]'
      WRITE(20,'(A)')'umrl 17  52,100 '//
     *               'RELATIVE HUMIDITY [no Dim]'
      WRITE(20,'(A)')'umes 17  51,100 '//
     *               'SPECIFIC HUMIDITY [kg/kg]'
      WRITE(20,'(A)')'mkmt 17 240,100 '//
     *               'TIME MEAN MASK [-/+]'
      WRITE(20,'(A)')'uvmt 17 255,100 '//
     *               'TIME MEAN ZONAL WIND (U) [m/s]'
      WRITE(20,'(A)')'vvmt 17 241,100 '//
     *               'TIME MEAN MERIDIONAL WIND (V) [m/s]'
      WRITE(20,'(A)')'ommt 17 236,100 '//
     *               'TIME MEAN DERIVED OMEGA [Pa/s]'
      WRITE(20,'(A)')'dvmt 17 237,100 '//
     *               'TIME MEAN DIVERGENCE [1/s]'
      WRITE(20,'(A)')'vtmt 17 254,100 '//
     *               'TIME MEAN VORTICITY [1/s]'
      WRITE(20,'(A)')'fcmt 17 249,100 '//
     *               'TIME MEAN STREAM FUNCTION [m2/s]'
      WRITE(20,'(A)')'pvmt 17 252,100 '//
     *               'TIME MEAN VELOCITY POTENTIAL [m2/s]'
      WRITE(20,'(A)')'tvmt 17 253,100 '//
     *               'TIME MEAN VIRTUAL TEMPERATURE [K]'
      WRITE(20,'(A)')'uemt 17 248,100 '//
     *               'TIME MEAN SPECIFIC HUMIDITY [kg/kg]'
      WRITE(20,'(A)')'vdcc 17 227,100 '//
     *               'VERTICAL DIST TOTAL CLOUD COVER [no Dim]'
      WRITE(20,'(A)')'ENDVARS'
C*
      CLOSE(20)
C*
      NDAY=NDAY+1
      ENDDO
C*
      WRITE(98,'(A)')DPOS(1:IDP)
      WRITE(98,'(A,2(I4.4,3I2.2),A)')
     *          ANL,IYEAR,MONTH,IDAY,IUTC,
     *          LYEAR,MN,LDAY,IUTC,'P.fct.'//TRUNC
CAMM     *          LYEAR,MN,LDAY,IUTC,'P.fct.'//TRUNC//'.grb'
C*
      RETURN
      END
C*
      SUBROUTINE ICNCTL(IDAY,MONTH,IYEAR,IUTC,
     *                  NICN,DPOS,IDP,ANL,TRUNC)
C*
      INTEGER IDAY,MONTH,IYEAR,IUTC,IDP
      CHARACTER NICN*10,DPOS*256,ANL*7,TRUNC*7
      CHARACTER MON(12)*3
      DATA MON /'JAN','FEB','MAR','APR','MAY','JUN',
     *          'JUL','AUG','SEP','OCT','NOV','DEC'/
C*
      OPEN(20,FILE=NICN//'.ctl',STATUS='UNKNOWN')
C*
      WRITE(20,'(A,2(I4.4,3I2.2),A)')
     *         'DSET '//DPOS(1:IDP)//ANL,IYEAR,MONTH,IDAY,IUTC,
     *          IYEAR,MONTH,IDAY,IUTC,'P.icn.'//TRUNC
CAMM     *          IYEAR,MONTH,IDAY,IUTC,'P.icn.'//TRUNC//'.grb'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'OPTIONS YREV'
      WRITE(20,'(A)')'DTYPE GRIB'
      WRITE(20,'(A)')'INDEX '//NICN//'.gmp'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'UNDEF 1e+20'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'TITLE Initial Fields'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'XDEF 192 LINEAR   0.0 1.875'
      WRITE(20,'(A)')'YDEF  96 LEVELS '
      WRITE(20,'(A)')'-88.572 -86.723 -84.862 -82.999 -81.135 '
      WRITE(20,'(A)')'-79.271 -77.406 -75.541 -73.676 -71.811 '
      WRITE(20,'(A)')'-69.946 -68.081 -66.216 -64.351 -62.486 '
      WRITE(20,'(A)')'-60.620 -58.755 -56.890 -55.025 -53.160 '
      WRITE(20,'(A)')'-51.294 -49.429 -47.564 -45.699 -43.833 '
      WRITE(20,'(A)')'-41.968 -40.103 -38.238 -36.372 -34.507 '
      WRITE(20,'(A)')'-32.642 -30.777 -28.911 -27.046 -25.181 '
      WRITE(20,'(A)')'-23.316 -21.450 -19.585 -17.720 -15.855 '
      WRITE(20,'(A)')'-13.989 -12.124 -10.259  -8.394  -6.528 '
      WRITE(20,'(A)')' -4.663  -2.798  -0.933   0.933   2.798 '
      WRITE(20,'(A)')'  4.663   6.528   8.394  10.259  12.124 '
      WRITE(20,'(A)')' 13.989  15.855  17.720  19.585  21.450 '
      WRITE(20,'(A)')' 23.316  25.181  27.046  28.911  30.777 '
      WRITE(20,'(A)')' 32.642  34.507  36.372  38.238  40.103 '
      WRITE(20,'(A)')' 41.968  43.833  45.699  47.564  49.429 '
      WRITE(20,'(A)')' 51.294  53.160  55.025  56.890  58.755 '
      WRITE(20,'(A)')' 60.620  62.486  64.351  66.216  68.081 '
      WRITE(20,'(A)')' 69.946  71.811  73.676  75.541  77.406 '
      WRITE(20,'(A)')' 79.271  81.135  82.999  84.862  86.723 '
      WRITE(20,'(A)')' 88.572'
      WRITE(20,'(A)')'ZDEF  17 LEVELS 1000 925 850 700 500 400 '
      WRITE(20,'(A)')'                 300 250 200 150 100  70 '
      WRITE(20,'(A)')'                      50  30  20  10   3'
      WRITE(20,'(A,I2.2,A,I2.2,A,I4.4,A)')
     *         'TDEF   1 LINEAR ',IUTC,'Z',IDAY,
     *          MON(MONTH),IYEAR,' 1DY'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'VARS 22'
      WRITE(20,'(A)')'topo  0 132,1,0,0 '//
     *               'TOPOGRAPHY [m]'
      WRITE(20,'(A)')'lsmk  0  81,1,0,0 '//
     *               'LAND SEA MASK [0,1]'
      WRITE(20,'(A)')'pslc  0 135,1,0,0 '//
     *               'SURFACE PRESSURE [hPa]'
      WRITE(20,'(A)')'uves  0 192,1,0,0 '//
     *               'SURFACE ZONAL WIND (U) [m/s]'
      WRITE(20,'(A)')'vves  0 194,1,0,0 '//
     *               'SURFACE MERIDIONAL WIND (V) [m/s]'
      WRITE(20,'(A)')'psnm  0   2,102,0,0 '//
     *               'SEA LEVEL PRESSURE [hPa]'
      WRITE(20,'(A)')'tems  0 188,1,0,0 '//
     *               'SURFACE ABSOLUTE TEMPERATURE [K]'
      WRITE(20,'(A)')'umrs  0 226,1,0,0 '//
     *               'SURFACE RELATIVE HUMIDITY [no Dim]'
      WRITE(20,'(A)')'agpl  0  54,200,0,0 '//
     *               'INST. PRECIPITABLE WATER [Kg/m2]'
      WRITE(20,'(A)')'tsfc  0 187,1,0,0 '//
     *               'SURFACE TEMPERATURE [K]'
      WRITE(20,'(A)')'mask 17 137,100 '//
     *               'MASK [-/+]'
      WRITE(20,'(A)')'uvel 17  33,100 '//
     *               'ZONAL WIND (U) [m/s]'
      WRITE(20,'(A)')'vvel 17  34,100 '//
     *               'MERIDIONAL WIND (V) [m/s]'
      WRITE(20,'(A)')'omeg 17  39,100 '//
     *               'OMEGA [Pa/s]'
      WRITE(20,'(A)')'divg 17  44,100 '//
     *               'DIVERGENCE [1/s]'
      WRITE(20,'(A)')'vort 17  43,100 '//
     *               'VORTICITY [1/s]'
      WRITE(20,'(A)')'fcor 17  35,100 '//
     *               'STREAM FUNCTION [m2/s]'
      WRITE(20,'(A)')'potv 17  36,100 '//
     *               'VELOCITY POTENTIAL [m2/s]'
      WRITE(20,'(A)')'zgeo 17   7,100 '//
     *               'GEOPOTENTIAL HEIGHT [gpm]'
      WRITE(20,'(A)')'temp 17  11,100 '//
     *               'ABSOLUTE TEMPERATURE [K]'
      WRITE(20,'(A)')'umrl 17  52,100 '//
     *               'RELATIVE HUMIDITY [no Dim]'
      WRITE(20,'(A)')'umes 17  51,100 '//
     *               'SPECIFIC HUMIDITY [kg/kg]'
      WRITE(20,'(A)')'ENDVARS'
C*
      CLOSE(20)
C*
      RETURN
      END
C*
      SUBROUTINE PSTCTL(IDAY,MONTH,IYEAR,IUTC,
     *                  NPST,DPOS,IDP,ANL,TRUNC)
C*
      INTEGER IDAY,MONTH,IYEAR,IUTC,
     *        KYEAR,JDAY,MN,IDS,IDP,
     *        NDAY,MDAY,LDAY,LMN,LYEAR
      CHARACTER NPST*10,DPOS*256,ANL*7,TRUNC*7,NF*2
      INTEGER MONL(12)
      CHARACTER MON(12)*3
      DATA IDS /7/
      DATA MONL /31,28,31,30,31,30,31,31,30,31,30,31/
      DATA MON /'JAN','FEB','MAR','APR','MAY','JUN',
     *          'JUL','AUG','SEP','OCT','NOV','DEC'/
C*
      IF (MOD (IYEAR,4) .EQ. 0) MONL(2)=29
      KYEAR=IYEAR
      IF (IDAY .LE. IDS) THEN
      MN=MONTH-1
      IF (MN .EQ. 0) THEN
      MN=12
      KYEAR=KYEAR-1
      ENDIF
      JDAY=MONL(MN)-(IDS-IDAY)
      ELSE
      MN=MONTH
      JDAY=IDAY-IDS
      ENDIF
C*
      LDAY=IDAY+1
      LMN=MONTH
      LYEAR=IYEAR
      IF (LDAY .GT. MONL(LMN)) THEN
      LDAY=1
      LMN=LMN+1
      IF (LMN .GT. 12) THEN
      LMN=1
      LYEAR=LYEAR+1
      ENDIF
      ENDIF
C*
      NDAY=JDAY
      DO MDAY=JDAY,JDAY+IDS-1
      IF (NDAY .GT. MONL(MN)) THEN
      NDAY=MDAY-MONL(MN)
      MN=MN+1
      IF (MN .GT. 12) THEN
      MN=1
      KYEAR=KYEAR+1
      ENDIF
      ENDIF
      WRITE(NF,'(I2.2)')MDAY-JDAY
C*
      OPEN(20,FILE=NPST//NF//'.ctl',STATUS='UNKNOWN')
C*
      WRITE(97,'(A,2(I4.4,3I2.2),A)')
     *          ANL,KYEAR,MN,NDAY,IUTC,
     *          KYEAR,MN,NDAY,IUTC,'P.icn.'//TRUNC
CAMM     *          KYEAR,MN,NDAY,IUTC,'P.icn.'//TRUNC//'.grb'
      WRITE(20,'(A,2(I4.4,3I2.2),A)')
     *         'DSET '//ANL,KYEAR,MN,NDAY,IUTC,
     *          KYEAR,MN,NDAY,IUTC,'P.icn.'//TRUNC
CAMM     *          KYEAR,MN,NDAY,IUTC,'P.icn.'//TRUNC//'.grb'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'OPTIONS YREV'
      WRITE(20,'(A)')'DTYPE GRIB'
      WRITE(20,'(A)')'INDEX '//NPST//NF//'.gmp'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'UNDEF 1e+20'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'TITLE Initial Fields'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'XDEF 192 LINEAR   0.0 1.875'
      WRITE(20,'(A)')'YDEF  96 LEVELS '
      WRITE(20,'(A)')'-88.572 -86.723 -84.862 -82.999 -81.135 '
      WRITE(20,'(A)')'-79.271 -77.406 -75.541 -73.676 -71.811 '
      WRITE(20,'(A)')'-69.946 -68.081 -66.216 -64.351 -62.486 '
      WRITE(20,'(A)')'-60.620 -58.755 -56.890 -55.025 -53.160 '
      WRITE(20,'(A)')'-51.294 -49.429 -47.564 -45.699 -43.833 '
      WRITE(20,'(A)')'-41.968 -40.103 -38.238 -36.372 -34.507 '
      WRITE(20,'(A)')'-32.642 -30.777 -28.911 -27.046 -25.181 '
      WRITE(20,'(A)')'-23.316 -21.450 -19.585 -17.720 -15.855 '
      WRITE(20,'(A)')'-13.989 -12.124 -10.259  -8.394  -6.528 '
      WRITE(20,'(A)')' -4.663  -2.798  -0.933   0.933   2.798 '
      WRITE(20,'(A)')'  4.663   6.528   8.394  10.259  12.124 '
      WRITE(20,'(A)')' 13.989  15.855  17.720  19.585  21.450 '
      WRITE(20,'(A)')' 23.316  25.181  27.046  28.911  30.777 '
      WRITE(20,'(A)')' 32.642  34.507  36.372  38.238  40.103 '
      WRITE(20,'(A)')' 41.968  43.833  45.699  47.564  49.429 '
      WRITE(20,'(A)')' 51.294  53.160  55.025  56.890  58.755 '
      WRITE(20,'(A)')' 60.620  62.486  64.351  66.216  68.081 '
      WRITE(20,'(A)')' 69.946  71.811  73.676  75.541  77.406 '
      WRITE(20,'(A)')' 79.271  81.135  82.999  84.862  86.723 '
      WRITE(20,'(A)')' 88.572'
      WRITE(20,'(A)')'ZDEF  17 LEVELS 1000 925 850 700 500 400 '
      WRITE(20,'(A)')'                 300 250 200 150 100  70 '
      WRITE(20,'(A)')'                      50  30  20  10   3'
      WRITE(20,'(A,I3,A,I2.2,A,I2.2,A,I4.4,A)')
     *         'TDEF ',1,' LINEAR ',
     *          IUTC,'Z',NDAY,MON(MN),KYEAR,' 1DY'
      WRITE(20,'(A)')'*'
      WRITE(20,'(A)')'VARS 22'
      WRITE(20,'(A)')'topo  0 132,1,0,0 '//
     *               'TOPOGRAPHY [m]'
      WRITE(20,'(A)')'lsmk  0  81,1,0,0 '//
     *               'LAND SEA MASK [0,1]'
      WRITE(20,'(A)')'pslc  0 135,1,0,0 '//
     *               'SURFACE PRESSURE [hPa]'
      WRITE(20,'(A)')'uves  0 192,1,0,0 '//
     *               'SURFACE ZONAL WIND (U) [m/s]'
      WRITE(20,'(A)')'vves  0 194,1,0,0 '//
     *               'SURFACE MERIDIONAL WIND (V) [m/s]'
      WRITE(20,'(A)')'psnm  0   2,102,0,0 '//
     *               'SEA LEVEL PRESSURE [hPa]'
      WRITE(20,'(A)')'tems  0 188,1,0,0 '//
     *               'SURFACE ABSOLUTE TEMPERATURE [K]'
      WRITE(20,'(A)')'umrs  0 226,1,0,0 '//
     *               'SURFACE RELATIVE HUMIDITY [no Dim]'
      WRITE(20,'(A)')'agpl  0  54,200,0,0 '//
     *               'INST. PRECIPITABLE WATER [Kg/m2]'
      WRITE(20,'(A)')'tsfc  0 187,1,0,0 '//
     *               'SURFACE TEMPERATURE [K]'
      WRITE(20,'(A)')'mask 17 137,100 '//
     *               'MASK [-/+]'
      WRITE(20,'(A)')'uvel 17  33,100 '//
     *               'ZONAL WIND (U) [m/s]'
      WRITE(20,'(A)')'vvel 17  34,100 '//
     *               'MERIDIONAL WIND (V) [m/s]'
      WRITE(20,'(A)')'omeg 17  39,100 '//
     *               'OMEGA [Pa/s]'
      WRITE(20,'(A)')'divg 17  44,100 '//
     *               'DIVERGENCE [1/s]'
      WRITE(20,'(A)')'vort 17  43,100 '//
     *               'VORTICITY [1/s]'
      WRITE(20,'(A)')'fcor 17  35,100 '//
     *               'STREAM FUNCTION [m2/s]'
      WRITE(20,'(A)')'potv 17  36,100 '//
     *               'VELOCITY POTENTIAL [m2/s]'
      WRITE(20,'(A)')'zgeo 17   7,100 '//
     *               'GEOPOTENTIAL HEIGHT [gpm]'
      WRITE(20,'(A)')'temp 17  11,100 '//
     *               'ABSOLUTE TEMPERATURE [K]'
      WRITE(20,'(A)')'umrl 17  52,100 '//
     *               'RELATIVE HUMIDITY [no Dim]'
      WRITE(20,'(A)')'umes 17  51,100 '//
     *               'SPECIFIC HUMIDITY [kg/kg]'
      WRITE(20,'(A)')'ENDVARS'
C*
      CLOSE(20)
C*
      NDAY=NDAY+1
      ENDDO
C*
      WRITE(97,'(A)')DPOS(1:IDP)
      WRITE(97,'(A,2(I4.4,3I2.2),A)')
     *          ANL,IYEAR,MONTH,IDAY,IUTC,
     *          LYEAR,MN,LDAY,IUTC,'P.fct.'//TRUNC
CAMM     *          LYEAR,MN,LDAY,IUTC,'P.fct.'//TRUNC//'.grb'
      CLOSE(97)
C*
      RETURN
      END
