#!/bin/ksh
#help#
#************************************************************************#
#                                                                        #
#     Name:           runchop.ksh                                        #
#                                                                        #
#     Function:       This script submits the chopping of an             #
#                     initial condition script to the NQS queue.         #
#                     It runs in Korn Shell.                             #
#                                                                        #
#       Date:         July 01th, 2003.                                   #
#       Last change:  July 01th, 2003.                                   #
#                                                                        #
#     Valid Arguments for runchop.sx6                                    #
#                                                                        #
#     First:     COMPILE: help, make, clean or run                       #
#     Second:       TRCI: input three-digit triangular truncation        #
#     Third:         LVI: input two-digit number of vertical layers      #
#     Fourth:       TRCO: output three-digit triangular truncation       #
#     Fifth:         LVO: output two-digit number of vertical layers     #
#     Sixth:      LABELI: initial forecasting label                      #
#     Seventh:     KINDI: flag to kind of input file: NMC, CPT, PAD      #
#     Eighth:      KINDO: flag to kind of output file: NMC, CPT, PAD     #
#     Nineth:     NEWSIG: flag to get a new delta sigma: YES or NO       #
#     Tenth:      NEWTOP: flag to get a new topography: YES or NO        #
#     Eleventh:   GOzone: flag to get Ozone file: T or  F                #
#     Twelveth:  GTracer: flag to get Tracers file: T or F               #
#     Thirteenth: GrADSI: flag to set IC GrADS file: T or  F             #
#     Fourteenth: GrADSO: stop after set IC GrADS file: T or F           #
#     Fifteenth:   GDASO: stop after get GANL from GDAS: T or F          #
#     Sixteenth: SmthTop: Flag to smooth topography: T or F              #
#                                                                        #
#               LABELI : yyyymmddhh                                      #
#                        yyyy = four digit year                          #
#                          mm = two digit month                          #
#                          dd = two digit day                            #
#                          hh = two digit hour                           #
#                                                                        #
#               If (KINDI = AVN ):                                       #
#                   Input file = gblav.ThhZ.SAnl.yyyymmddhh              #
#               Else                                                     #
#                   Input file = gdas1.ThhZ.SAnl.yyyymmddhh              #
#                                                                        #
#************************************************************************#
#help#
#
#       Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
cat < ${0} | sed -n '/^#help#/,/^#help#/p'
exit 0
fi
#
# Check First Argument
#
if [ "${1}" != "run" ]
then
  if [ "${1}" != "make" ]
  then
    if [ "${1}" != "clean" ]
    then
      echo "First argument: ${1}, is wrong. Must be: make, clean or run"
      exit
    fi
  fi
fi
#
  if [ "${1}" = "run" ]
  then
#
# Check Other Arguments and Set Defaults
#
if [ -z "${2}" ]
then
  TRCI=062
else
  TRCI=${2}
fi
if [ -z "${3}" ]
then
  LVI=28
else
  LVI=${3}
fi
if [ -z "${4}" ]
then
  TRCO=042
else
  TRCO=${4}
fi
if [ -z "${5}" ]
then
  LVO=18
else
  LVO=${5}
fi
if [ -z "${6}" ]
then
  LABI=`date +'%Y%m%d'`12
else
  LABI=${6}
fi
if [ -z "${7}" ]
then
  KINDI=NMC
else
  KINDI=${7}
fi
if [ -z "${8}" ]
then
  KINDO=NMC
else
  KINDO=${8}
fi
if [ -z "${9}" ]
then
  NEWSIG=YES
else
  NEWSIG=${9}
fi
if [ -z "${10}" ]
then
  NEWTOP=NO
else
  NEWTOP=${10}
fi
if [ -z "${11}" ]
then
  GOzone=F
else
  GOzone=${11}
fi
if [ -z "${12}" ]
then
  GTracer=F
else
  GTracer=${12}
fi
if [ -z "${13}" ]
then
  GrADSI=F
else
  GrADSI=${13}
fi
if [ -z "${14}" ]
then
  GrADSO=F
else
  GrADSO=${14}
fi
if [ -z "${15}" ]
then
  GDASO=F
else
  GDASO=${15}
fi
if [ -z "${16}" ]
then
  SmthTop=T
else
  SmthTop=${16}
fi
#
PREFX=${KINDO}
HH=`echo $LABI | cut -c 9-10`
echo " "
echo "${0} ${1} ${TRCI} ${LVI} ${TRCO} ${LVO} ${LABI} ${KINDI} ${KINDO} ${NEWSIG} ${NEWTOP} ${GOzone} ${GTracer} ${GrADSI} ${GrADSO} ${GDASO} ${SmthTop}"
echo " "
#
# Select parameters for the given resolution:
#
case ${TRCI} in
021) MTI=021 ; IRI=064 ; JRI=032 ;;
042) MTI=042 ; IRI=128 ; JRI=064 ;;
059) MTI=059 ; IRI=180 ; JRI=090 ;;
062) MTI=062 ; IRI=192 ; JRI=096 ;;
066) MTI=066 ; IRI=200 ; JRI=100 ;;
126) MTI=126 ; IRI=384 ; JRI=192 ;;
170) MTI=170 ; IRI=512 ; JRI=256 ;;
213) MTI=213 ; IRI=640 ; JRI=320 ;;
254) MTI=254 ; IRI=768 ; JRI=384 ;;
319) MTI=319 ; IRI=960 ; JRI=480 ;;
382) MTI=382 ; IRI=1152 ; JRI=576 ;;
511) MTI=511 ; IRI=1536 ; JRI=768 ;;
574) MTI=574 ; IRI=1536 ; JRI=768 ;;
*) echo "Wrong request for horizontal resolution: TRCI = ${TRCI}" ; exit 1;
esac
case ${LVI} in
09) KRI=9 ;;
18) KRI=18 ;;
28) KRI=28 ;;
42) KRI=42 ;;
64) KRI=64 ;;
*) echo "Wrong request for vertical resolution: LVI = ${LVI}" ; exit 1 ;;
esac
case ${TRCO} in
021) MTO=021 ; MTC=021 ; ITR=10 ; CUT=12 ; IRO=064 ; JRO=032 ;;
042) MTO=042 ; MTC=042 ; ITR=10 ; CUT=12 ; IRO=128 ; JRO=064 ;;
059) MTO=059 ; MTC=059 ; ITR=10 ; CUT=12 ; IRO=180 ; JRO=090 ;;
062) MTO=062 ; MTC=062 ; ITR=10 ; CUT=12 ; IRO=192 ; JRO=096 ;;
066) MTO=066 ; MTC=066 ; ITR=10 ; CUT=12 ; IRO=200 ; JRO=100 ;;
126) MTO=126 ; MTC=126 ; ITR=10 ; CUT=12 ; IRO=384 ; JRO=192 ;;
170) MTO=170 ; MTC=170 ; ITR=10 ; CUT=12 ; IRO=512 ; JRO=256 ;;
213) MTO=213 ; MTC=213 ; ITR=10 ; CUT=12 ; IRO=640 ; JRO=320 ;;
254) MTO=254 ; MTC=254 ; ITR=10 ; CUT=12 ; IRO=768 ; JRO=384 ;;
319) MTO=319 ; MTC=319 ; ITR=10 ; CUT=12 ; IRO=960 ; JRO=480 ;;
382) MTO=382 ; MTC=382 ; ITR=10 ; CUT=12 ; IRO=1152 ; JRO=576 ;;
511) MTO=511 ; MTC=511 ; ITR=10 ; CUT=12 ; IRO=1536 ; JRO=768 ;;
574) MTO=574 ; MTC=574 ; ITR=10 ; CUT=12 ; IRO=1536 ; JRO=768 ;;
*) echo "Wrong request for horizontal resolution: TRCO = ${TRCO}" ; exit 1;
esac
case ${LVO} in
09) KRO=9 ;;
18) KRO=18 ;;
28) KRO=28 ;;
42) KRO=42 ;;
64) KRO=64 ;;
*) echo "Wrong request for vertical resolution: LVO = ${LVO}" ; exit 1 ;;
esac
#
  fi
#
# Set host, machine, NQS Queue, Run time and Extention
#
if [ "${1}" != "run" ]
then
  QUEUE=compila
else
  QUEUE=${T213queue}
fi
RUNTM=`date +'%Y%m%d%T'`
EXT=out
echo ${HSTMAQ}
echo ${MAQUI}
echo ${QUEUE}
echo ${RUNTM}
echo ${EXT}
#
# Set directories
#
# srcname:     name of main program source.
# srcsubdir:   subdirectory for sources.
# srcmaindir:  directory for sources, scripts and printouts.
# datamaindir: directory for input and output files.
# gdasdir:     directory for NCEP initial condition
# gradsdir:    directory for GrADS output
#
srcname=Chopping
srcsubdir=Chopping

. ../include/config.sx6

caldate=$HOME/bin/caldate.3.0.1

set -x
HORAR=`echo ${LABI} | cut -c 9-10`
set +x

srcmaindir=$OPERM ; mkdir -p $srcmaindir
datamaindir=$ROPERM ; mkdir -p $datamaindir
gdasdir=${ROPERM}/model/datain ; #mkdir -p $gdasdir
gradsdir=${OPERM}/Chopping/dataout ; #mkdir -p $gradsdir

mkdir -p ${OPERM}/Chopping/bin
mkdir -p ${OPERM}/Chopping/datain
mkdir -p ${OPERM}/Chopping/output

echo ${srcname}
echo ${srcsubdir}
echo ${srcmaindir}
echo ${datamaindir}
echo ${gdasdir}
echo ${gradsdir}
#
# Set Extension for Script and Output File
#
if [ -z "${TRCI}" ]
then
  EXTC=Compilation
else
  EXTC=T${TRCI}L${LVI}T${TRCO}L${LVO}
fi
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#			CLEANING
#
# Defining the label date to remove old files
# labelr: to remove the run made 12 hours ago
#
set -x
export labelr=`${caldate} ${6} - 24h 'yyyymmddhh'`
#
# Eliminating the dataout files from yesterday's run at same hour
#
#rm -f ${OPERM}/model/datain/GANLAVN${labelr}S.unf.T382L64
#rm -f ${OPERM}/model/datain/GANLAVN${labelr}S.unf.T${TRC}L${LV}
#rm -f ${OPERM}/model/datain/snowfd${labelr}S.unf.T213
#rm -f ${OPERM}/model/datain/sstwkl${labelr}.T213L42
#
# Moving the analisy file from yesterday's to HSM disk (Bangu)
#mv -f ${OPERM}/model/datain/gblav.T00Z.SAnl.${labelr} ${T213HSM}/ANLAVN
#mv -f ${OPERM}/model/datain/gblav.T12Z.SAnl.${labelr} ${T213HSM}/ANLAVN
#
# Eliminating the outputs older than 1 day
#
#find ${OPERM}/Chopping/output -mtime +1 -type f -exec rm -f {} \;
#find ${OPERM}/run/setout -name "chop*" -mtime +1 -type f -exec rm -f {} \;
#
set +x
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cd ${srcmaindir}/run
rm -f chop${EXTC}.${MAQUI}
rm -f ${mDK}/model/datain/GANL${PREFX}${LABELI}S.unf.T${TRCI}L${LVI}

#
cat <<EOT0 > chop${EXTC}.${MAQUI}
#!/bin/ksh
#
#
#*************************************************************#
#                                                             #
#     Name:        chop${EXTC}.${MAQUI}                       #
#                                                             #
#     Function:    This script file is used to set the        #
#                  environmental variables and submit the     #
#                  initial condition chooping script.         #
#                  It generates a new initial condition       #
#                  in a new given resolution.                 #
#                                                             #
#*************************************************************#
#
# Set date (year,month,day) and hour (hour:minute) 
#
#PBS -l cpunum_prc=1
#PBS -l tasknum_prc=1
#PBS -l cputim_job=1000.0
#PBS -l memsz_job=32GB
#PBS -o ${HSTMAQ}:${srcmaindir}/run/setout/chop${EXTC}.${MAQUI}.${RUNTM}.${EXT}.%s
#PBS -j o
#PBS -N CHOPT126
#
# DATE=yyyymmdd
# HOUR=hh:mn
#
DATE=`date +'%Y%m%d'`
HOUR=`date +'%H:%M'`
EXT=${EXT}
echo 'Date: '\${DATE}
echo 'Hour: '\${HOUR}
echo 'Case: '\${EXT}
export DATE HOUR EXT
#
# Set labels (date, UTC hour, ...)
#
# LABELI = yyyymmddhh
# LABELI = initial condition label
#
LABELI=${LABI}
export LABELI
echo \${LABELI}
#
# Prefix names for the FORTRAN files
#
# NAMEI - Input Initial Condition file's prefix
# NAMEO - Output Initial Condition file's prefix
# NAMSG - New Delta Sigma file's prefix
# NAMTP - New Topography file's prefix
# NAMGD - NCEP GDAS Initial Condition file's prefix
# NAMZI - Input Ozone file's prefix
# NAMZO - Output Ozone file's prefix
# NAMTI - Input Tracers file's prefix
# NAMTO - Output Tracers file's prefix
#
NAMEI=GANL${KINDI}
NAMEO=GANL${KINDO}
NAMSG=Delta_Sigma
if [ "${NEWTOP}" = "YES" ]
then
  NAMTP=Topography
else
  rm -f TopoFake.T${TRCO}
  NAMTP=TopoFake
fi
if [ "${KINDI}" = "AVN" ]
then
  NAMGD=gblav
else
  NAMGD=gdas1
fi
NAMZI=OZON${KINDI}
NAMZO=OZON${KINDO}
NAMTI=TRAC${KINDI}
NAMTO=TRAC${KINDO}
export NAMEI NAMEO NAMSG NAMTP NAMGD NAMZI NAMZO NAMTI NAMTO
#
# Suffixes names for the initial condition files
#
# EXTI - Input Initial Condition file's extension
# EXTO - Output Initial Condition file's extension
# EXZT - Output Ozone and Tracers file's extension
# EXTG - NCEP GDAS Initial Condition file's extension
#
EXTI=S.unf.
EXTO=S.unf.
EXZT=S.grd.
typeset -Z2 hi
hi=`awk 'BEGIN {print substr("'${LABI}'",9,2)}'`
EXTG=.T\${hi}Z.SAnl.
export EXTI EXTO EXZT EXTG
#
# Set directories
#
# SRCNAME:     name of main program source.
# SRCSUBDIR:   subdirectory for sources.
# SRCMAINDIR:  directory for sources, scripts and printouts.
# DATAMAINDIR: directory for input and output files.
# GDASDIR:     directory for NCEP initial condition
# GRADSDIR:    directory for GrADS output
#
SRCNAME=${srcname}
SRCSUBDIR=${srcsubdir}
SRCMAINDIR=${srcmaindir}
DATAMAINDIR=${datamaindir}
GDASDIR=${gdasdir}
GRADSDIR=${gradsdir}
export SRCNAME SRCSUBDIR SRCMAINDIR DATAMAINDIR GDASDIR GRADSDIR
echo \${SRCNAME}
echo \${SRCSUBDIR}
echo \${SRCMAINDIR}
echo \${DATAMAINDIR}
echo \${GDASDIR}
echo \${GRADSDIR}
#
cd \${SRCMAINDIR}/run
#
# Set Horizontal Truncation and Vertical Layers
#
TRUNCI=T${TRCI}
LEVI=L${LVI}
TRUNCO=T${TRCO}
LEVO=L${LVO}
export TRUNCI LEVI TRUNCO LEVO
#
# Set machine
#
MACH=${MAQUI}
export MACH
#
# Set option for compiling or not the source codes.
#
# If COMPILE=make then only the modified sources will be compiled.
# If COMPILE=clean then the touch files will be removed and 
#            all sources will be compiled.
#           =run for run with no compilation
#
# If COMPILE is make or clean then the script generates the binary file 
#            and exits;
#            if it is run then the script runs the existent binary file.
#
COMPILE=${1}
export COMPILE
echo \${COMPILE}
#
#   Set envoirement variables for output time diagnostics
#
#   F_PROGINF gives the elapsed, user, system and vector instruction
#             execution time, and execution count of all instructions
#             and number of vector instruction executions.
#   F_FILEINF gives informations about I/O operations.
#
F_PROGINF=DETAIL
export F_PROGINF
#F_FILEINF=DETAIL
#export F_FILEINF
#
# Set FORTRAN compilation flags
#
# -pvctl noaltcode either scalar or vector code is generated at compilation
# -O nodiv         division may not be changed to reciprocal multiplication
# -O nomove        not move invariant expression outside the loop
# -float0          floating-point data format IEEE is enabled
# -ew              sets the basic numeric size to 8 bytes
#
FTNFLAG=' -float0 -ew -w -Wf" -pvctl noaltcode noassume vwork=stack -O nodiv nomove " '
#export FTNFLAG
#
# Set FORTRAN compiler name
#
FTN=`echo ${T213f90}`
#export FTN
#
# Set environmental variables to binary conversion
#
F_UFMTIEEE=10,15,20,25,30,35,40,45,50,55,60,65,70,75
#export F_UFMTIEEE
F_UFMTADJUST10=TYPE2
F_UFMTADJUST15=TYPE2
F_UFMTADJUST20=TYPE2
F_UFMTADJUST25=TYPE2
F_UFMTADJUST30=TYPE2
F_UFMTADJUST35=TYPE2
F_UFMTADJUST40=TYPE2
F_UFMTADJUST45=TYPE2
F_UFMTADJUST50=TYPE2
F_UFMTADJUST55=TYPE2
F_UFMTADJUST60=TYPE2
F_UFMTADJUST65=TYPE2
F_UFMTADJUST70=TYPE2
F_UFMTADJUST75=TYPE2
#export F_UFMTADJUST10 F_UFMTADJUST15 F_UFMTADJUST20 F_UFMTADJUST25 F_UFMTADJUST30
#export F_UFMTADJUST35 F_UFMTADJUST40 F_UFMTADJUST45 F_UFMTADJUST50 F_UFMTADJUST55
#export F_UFMTADJUST60 F_UFMTADJUST65 F_UFMTADJUST70 F_UFMTADJUST75
#
F_SETBUF=4096
#export F_SETBUF
#echo " F_SETBUF = \${F_SETBUF}"
#
# Set Source and Namelist Directories
#
SRC=\${SRCMAINDIR}/\${SRCSUBDIR}/source
NML=\${SRCMAINDIR}/\${SRCSUBDIR}/bin
export SRC NML
echo \${SRC}
echo \${NML}
#
# Include File
#
  if [ "\${COMPILE}" != "run" ]
  then
#
cd \${SRC}
#
cat <<EOT > m_Parameters.n

! SELECTED_INT_KIND(R):
!   The value of the result is the kind type parameter value 
!   of the integer type that can represent all integer values 
!   n in the range -10**R < n < 10**R
!   R must be a scalar of integer type (Ri4, Ri8).

! SELECTED_REAL_KIND(P,R):
!   The value of the result is the kind type parameter value 
!   of the real type that has a decimal precision greater than 
!   or equal to P digits as returned by the function PRECISION 
!   and a decimal exponent range greater than or equal to R 
!   as returned by the function RANGE.
!   P (optional) must be a scalar of integer type (Pr4, Pr8). 
!   R (optional) must be a scalar of integer type (Rr4, Rr8).

INTEGER, PARAMETER :: Ri4=9,  Ri8=15, &
                      Pr4=6,  Rr4=37, &
                      Pr8=15, Rr8=307

INTEGER, PARAMETER, PUBLIC :: ki4=SELECTED_INT_KIND(Ri4)
INTEGER, PARAMETER, PUBLIC :: ki8=SELECTED_INT_KIND(Ri8)
INTEGER, PARAMETER, PUBLIC :: kr4=SELECTED_REAL_KIND(Pr4,Rr4)
INTEGER, PARAMETER, PUBLIC :: kr8=SELECTED_REAL_KIND(Pr8,Rr8)

EOT
if (diff m_Parameters.n m_Parameters.h > /dev/null)
then
    echo "m_Parameters.n and m_Parameters.h are the same"
    rm -f m_Parameters.n
else
    echo "m_Parameters.n and m_Parameters.h are different"
    mv m_Parameters.n m_Parameters.h
fi
#
  fi
#
# Namelist File
#
  if [ "\${COMPILE}" = "run" ]
  then
#
ganl=\${DATAMAINDIR}/\${SRCSUBDIR}/datain/\${NAMEI}\${LABELI}\${EXTI}\${TRUNCI}\${LEVI}
if [ -s \${ganl} ]
then
  ls -o \${ganl}
else
  echo "rm -f \${ganl}"
  rm -f \${ganl}
fi
#
rm -f \${NML}/\${SRCNAME}.nml
cat <<EOT > \${NML}/\${SRCNAME}.nml
 &ChopNML
  MendInp=${MTI},         ! Spectral Horizontal Resolution of Input Data
  KmaxInp=${KRI},         ! Number of Layers of Input Data
  MendOut=${MTO},         ! Spectral Horizontal Resolution of Output Data
  KmaxOut=${KRO},         ! Number of Layers of Output Data
  MendCut=${MTC},             ! Spectral Resolution Cut Off for Topography Smoothing
  Iter=${ITR},            ! Number of Iteractions in Topography Smoothing
  SmthPerCut=0.${CUT},    ! Percentage for Topography Smoothing
  GetOzone=T,             ! Flag to Produce Ozone Files
  GetTracers=F,           ! Flag to Produce Tracers Files
  GrADS=F,                ! Flag to Produce GrADS Files
  GrADSOnly=F,            ! Flag to Only Produce GrADS Files (Do Not Produce Inputs for Model)
  GDASOnly=F,             ! Flag to Only Produce Input CPTEC Analysis File
  SmoothTopo=T,           ! Flag to Performe Topography Smoothing
  RmGANL=T,               ! Flag to Remove GANL File if Desired
  Linear=F,               ! Flag to Set Linear or Quadratic Gaussian Grid
  DateLabel='\${LABELI}', ! Date Label: yyyymmddhh or DateLabel='          '
                          !       If Year (yyyy), Month (mm) and Day (dd) Are Unknown
  UTC='${HH}',            ! UTC Hour: hh, Must Be Given if Label='          ', else UTC=' '
  NCEPName='gblav.T${HH}Z.SAnl.\${LABELI} ',  ! NCEP Analysis Preffix for Input File Name

  DirMain='${DATAMAINDIR}/ '      ! Main User Data Directory
  DGDInp='\${GDASDIR}/ ',
  DirInp='\${DATAMAINDIR}/model/datain/ ',
  DirOut='\${DATAMAINDIR}/model/datain/ ',
  DirTop='\${DATAMAINDIR}/\${SRCSUBDIR}/datain/ ',
  DirSig='\${DATAMAINDIR}/\${SRCSUBDIR}/datain/ ',
  DirGrd='\${GRADSDIR}/ ',
  DirHome='${DATAMAINDIR}/ ',      ! Home User Source Directory
  Pref='${PREFX}'
 /
EOT
#
# Delta Sigma File
#
rm -f \${NML}/\${NAMSG}.L*
    if [ "${NEWSIG}" = "YES" ]
    then
case \${LEVO} in
L09)
cat <<EOT >> \${NML}/\${NAMSG}.L09
  0.10000000E-01  0.17000000E-01  0.25000000E-01  0.14800000E+00  0.20000000E+00
  0.30000000E+00  0.20000000E+00  0.20000000E-01  0.80000000E-01
EOT
;;
#L18)
# NCEP Pre 1993
#cat <<EOT >> \${NML}/\${NAMSG}.L18
#  0.10000000E-01  0.17000000E-01  0.25000000E-01  0.55000000E-01  0.73000000E-01
#  0.85000000E-01  0.93000000E-01  0.96000000E-01  0.96000000E-01  0.50000000E-01
#  0.50000000E-01  0.50000000E-01  0.50000000E-01  0.50000000E-01  0.50000000E-01
#  0.50000000E-01  0.50000000E-01  0.50000000E-01
#EOT
#;;
# CPTEC Pos 2003
L18)
cat <<EOT >> \${NML}/\${NAMSG}.L18
  0.10000000E-01  0.17000000E-01  0.25000000E-01  0.43000000E-01  0.63000000E-01
  0.80000000E-01  0.91000000E-01  0.97000000E-01  0.98000000E-01  0.96000000E-01
  0.92000000E-01  0.83000000E-01  0.74000000E-01  0.63000000E-01  0.37000000E-01
  0.15000000E-01  0.90000000E-02  0.70000000E-02
EOT
;;
L28)
cat <<EOT >> \${NML}/\${NAMSG}.L28
  0.10000000E-01  0.15820000E-01  0.19590000E-01  0.24050000E-01  0.29190000E-01
  0.34930000E-01  0.41150000E-01  0.47540000E-01  0.53720000E-01  0.59190000E-01
  0.63470000E-01  0.66060000E-01  0.66690000E-01  0.65260000E-01  0.61970000E-01
  0.57160000E-01  0.51350000E-01  0.45030000E-01  0.38670000E-01  0.32620000E-01
  0.27090000E-01  0.22220000E-01  0.18030000E-01  0.14510000E-01  0.11600000E-01
  0.92300000E-02  0.72900000E-02  0.65700000E-02
EOT
;;
L42)
cat <<EOT >> \${NML}/\${NAMSG}.L42
  0.80300000E-02  0.92300000E-02  0.10580000E-01  0.12100000E-01  0.13800000E-01
  0.15650000E-01  0.17680000E-01  0.19870000E-01  0.22200000E-01  0.24660000E-01
  0.27170000E-01  0.29720000E-01  0.32230000E-01  0.34620000E-01  0.36810000E-01
  0.38740000E-01  0.40300000E-01  0.41450000E-01  0.42110000E-01  0.42280000E-01
  0.41910000E-01  0.41060000E-01  0.39750000E-01  0.38040000E-01  0.36000000E-01
  0.33720000E-01  0.31280000E-01  0.28750000E-01  0.26200000E-01  0.23700000E-01
  0.21300000E-01  0.19010000E-01  0.16890000E-01  0.14920000E-01  0.13120000E-01
  0.11500000E-01  0.10050000E-01  0.87500000E-02  0.76000000E-02  0.65900000E-02
  0.57100000E-02  0.49200000E-02
EOT
;;
# Valores do NCEP e T170L64 e acima do CPTEC
L64)
cat <<EOT >> \${NML}/\${NAMSG}.L64
  0.53290000E-02  0.60390000E-02  0.68320000E-02  0.77170000E-02  0.86980000E-02
  0.97820000E-02  0.10972000E-01  0.12271000E-01  0.13682000E-01  0.15198000E-01
  0.16817000E-01  0.18524000E-01  0.20309000E-01  0.22145000E-01  0.24008000E-01
  0.25866000E-01  0.27678000E-01  0.29404000E-01  0.30998000E-01  0.32415000E-01
  0.33611000E-01  0.34545000E-01  0.35186000E-01  0.35511000E-01  0.35508000E-01
  0.35177000E-01  0.34529000E-01  0.33590000E-01  0.32390000E-01  0.30969000E-01
  0.29372000E-01  0.27644000E-01  0.25830000E-01  0.23972000E-01  0.22110000E-01
  0.20273000E-01  0.18491000E-01  0.16785000E-01  0.15168000E-01  0.13653000E-01
  0.12246000E-01  0.10948000E-01  0.97590000E-02  0.86790000E-02  0.76990000E-02
  0.68160000E-02  0.60240000E-02  0.53160000E-02  0.46850000E-02  0.41220000E-02
  0.36240000E-02  0.31830000E-02  0.27940000E-02  0.24490000E-02  0.21470000E-02
  0.18800000E-02  0.16460000E-02  0.14410000E-02  0.12600000E-02  0.11010000E-02
  0.96300000E-03  0.84200000E-03  0.73600000E-03  0.64200000E-03
EOT
# CPTEC teste modelo acoplado T062L28 (3 primeiros = L42)
#L64)
#cat <<EOT >> \${NML}/\${NAMSG}.L64
#  0.80300000E-02  0.92300000E-02  0.10580000E-01  0.11187000E-01  0.12037000E-01
#  0.12974000E-01  0.14003000E-01  0.15124000E-01  0.16339000E-01  0.17644000E-01
#  0.19033000E-01  0.20496000E-01  0.22018000E-01  0.23580000E-01  0.25158000E-01
#  0.26721000E-01  0.28235000E-01  0.29663000E-01  0.30964000E-01  0.32098000E-01
#  0.33026000E-01  0.33714000E-01  0.34133000E-01  0.34264000E-01  0.34099000E-01
#  0.33638000E-01  0.32894000E-01  0.31890000E-01  0.30656000E-01  0.29229000E-01
#  0.27649000E-01  0.25959000E-01  0.24199000E-01  0.22408000E-01  0.20620000E-01
#  0.18865000E-01  0.17168000E-01  0.15545000E-01  0.14013000E-01  0.12578000E-01
#  0.11248000E-01  0.10022000E-01  0.89020000E-02  0.78820000E-02  0.69600000E-02
#  0.61300000E-02  0.53850000E-02  0.47190000E-02  0.41250000E-02  0.35970000E-02
#  0.31290000E-02  0.27150000E-02  0.23490000E-02  0.20270000E-02  0.17420000E-02
#  0.14920000E-02  0.12730000E-02  0.10800000E-02  0.91000000E-03  0.76200000E-03
#  0.63200000E-03  0.51800000E-03  0.41500000E-03  0.32500000E-03
#EOT
;;
*) echo "Wrong request for vertical resolution: \${LEVO}" ; exit 1 ;;
esac
#
    fi
#
  fi
#
cd \${SRCMAINDIR}/run
#
# Run Initial Condition Chopping
#
\${SRC}/\${SRCNAME}.scr
#

cd ${OPERM}/run
echo "runsstw1.${MAQUI} run ${TRC} ${LV} \${LABELI}"
runsstw1.sx6 run ${TRC} ${LV} \${LABELI}
echo "runsnow1.${MAQUI} run ${TRC} \${LABELI}"
runsnow1.sx6 run ${TRC} \${LABELI}
#
EOT0
#
# Change mode to be executable
#
chmod +x chop${EXTC}.${MAQUI}
#
echo "Initial Condition Chopping -- Submitted to Batch ..."
#
#qsub -q ${QUEUE} -N CHOPT126 ${srcmaindir}/run/chop${EXTC}.${MAQUI}
submit ${srcmaindir}/run/chop${EXTC}.${MAQUI} ${QUEUE} Info
qstat
#${srcmaindir}/run/chop${EXTC}.${MAQUI}
#
