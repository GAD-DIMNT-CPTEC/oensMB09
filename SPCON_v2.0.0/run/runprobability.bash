#!/bin/bash
#help#
#***********************************************************************#
#                                                                       #
#     Name:           runprobability.sx6                                #
#                                                                       #
#     Function:       This script evaluate the probabilities            #
#                     from CPTEC global ensemble forecasting.           #
#                     It runs in Korn Shell.                            #
#                                                                       #
#     Date:           Apr 04th, 2005.                                   #
#     Last change:    Apr 04th, 2005.                                   #
#                                                                       #
#     Valid Arguments for runprobability.sx6:                           #
#                                                                       #
#      First:    COMPILE: help, make, clean or run                      #
#     Second:        TRC: three-digit triangular truncation             #
#      Third:         LV: two-digit number of vertical sigma-layers     #
#     Fourth:     LABELI: initial forecasting label                     #
#      Fifth:     NFCTDY: number of forecasting days                    #
#      Sixth:     NMEMBR: total number of members of the ensemble       #
#    Seventh:      PREFX: preffix for input and output files            #
#                                                                       #
#                  LABELx: yyyymmddhh                                   #
#                          yyyy = four digit year                       #
#                            mm = two digit month                       #
#                            dd = two digit day                         #
#                            hh = two digit hour                        #
#                                                                       #
#***********************************************************************#
#end#
#
#       Help:
#
#
if [ "${1}" = "help" -o -z "${1}" ]
then
cat < ${0} | sed -n '/^#help#/,/^#end#/p'
exit 0
fi
#
#       Testing Valid Arguments
#
if [ "${1}" != "run" ]
then
if [ "${1}" != "make" ]
then
if [ "${1}" != "clean" ]
then
echo "First argument: ${1}, is wrong. Must be: make, clean or run"
exit 0
fi
fi
fi
#
#  Set parameters to run
#

if [ -z "${2}" ]
then
echo "TRC is not set"
exit
else
export TRC=`echo ${2} | awk '{print $1/1}'`
fi
if [ -z "${3}" ]
then
echo "LV is not set"
exit
else
export LV=`echo ${3} | awk '{print $1/1}'` 
fi
if [ -z "${4}" ]
then
echo "LABELI is not set"
exit
else
export LABELI=${4}
fi
if [ -z "${5}" ]
then
echo "NFCTDY is not set"
exit
else
NFCTDY=${5}
fi
if [ -z "${6}" ]
then
echo "NMEMBR is not set"
exit
else
NMEMBR=${6}
fi
if [ -z "${7}" ]
then
echo "PREFX is not set"
exit
else
PREFX=${7}
fi
OUT=out
NPROC=1
#
#  End of setting parameters to run
#

#
#   Set machine, Run time and Extention
#
export COMPILE=${1}
CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
HSTMAQ=`hostname`
MACHINE=una
RUNTM=`date +'%Y'``date +'%m'``date +'%d'``date +'%H:%M'`
EXT=out
echo ${MACHINE}
echo ${RUNTM}
echo ${EXT}
echo ${LABELI}

#
#   Set final forecasting labels and UTC Hour
#
calday ()
{
yi=`echo  ${LABELI} | cut -c1-4`
mi=`echo  ${LABELI} | cut -c5-6`
di=`echo  ${LABELI} | cut -c7-8`
hi=`echo  ${LABELI} | cut -c9-10`
let ybi=${yi}%4
if [ ${ybi} = 0 ]
then
declare -a md=( 31 29 31 30 31 30 31 31 30 31 30 31 )
else
declare -a md=( 31 28 31 30 31 30 31 31 30 31 30 31 )
fi
let df=${di}+${NFCTDY}
let mf=${mi}
let yf=${yi}
let hf=${hi}
let n=${mi}-1
if [ ${df} -gt ${md[${n}]} ]
then
let df=${df}-${md[${n}]}
let mf=${mf}+1
if [ ${mf} -eq 13 ]
then
let mf=1
let yf=${yf}+1
fi
fi
if [ ${df} -lt 10 ]
then DF=0${df}
else DF=${df}
fi
if [ ${mf} -lt 10 ]
then MF=0${mf}
else MF=${mf}
fi
YF=${yf}
if [ ${hf} -lt 10 ]
then HF=0${hf}
else HF=${hf}
fi
}
calday
LABELF=${YF}${MF}${DF}${HF}
export LABELI LABELF
echo "LABELI="${LABELI}
echo "LABELF="${LABELF}
#
#     Select parameter for the resolution:
#
if [ "${1}" = "run" ]
then
case ${TRC} in
21) MR=22 ; IR=64 ; JR=32 ; NPGH=93 ;
     DT=1800
;;
30) MR=31 ; IR=96 ; JR=48 ; NPGH=140 ;
     DT=1800
;;
42) MR=43 ; IR=128 ; JR=64 ; NPGH=187 ;
     DT=1800
;;
47) MR=48 ; IR=144 ; JR=72 ; NPGH=26 ;
     DT=1200
;;
62) MR=63 ; IR=192 ; JR=96 ; NPGH=315 ;
     DT=1200
;;
79) MR=80 ; IR=240 ; JR=120 ; NPGH=26 ;
     DT=900
;;
05) MR=86 ; IR=256 ; JR=128 ; NPGH=26 ;
     DT=720
;;
94) MR=95 ; IR=288 ; JR=144 ; NPGH=591 ;
     DT=720
;;
106) MR=107 ; IR=320 ; JR=160 ; NPGH=711 ;
     DT=600
;;
126) MR=127 ; IR=384 ; JR=192 ; NPGH=284 ;
     DT=600
;;
159) MR=160 ; IR=480 ; JR=240 ; NPGH=1454 ;
     DT=450
;;
170) MR=171 ; IR=512 ; JR=256 ; NPGH=1633 ;
     DT=450
;;
213) MR=214 ; IR=640 ; JR=320 ; NPGH=2466 ;
     DT=360
;;
254) MR=255 ; IR=768 ; JR=384 ; NPGH=3502 ;
     DT=300
;;
319) MR=320 ; IR=960 ; JR=480 ; NPGH=26 ;
     DT=240
;;
*) echo "Wrong request for horizontal resolution: ${TRC}" ; exit 1;
esac
fi
#
#   Set host, machine, NQS Queue, Run time and Extention
#
HSTMAQ=`hostname`
MAQUI=sx6
if [ "${1}" != "run" ]
then
  QUEUE=${QUEUE3}
else
  QUEUE=${QUEUE3}
fi
RUNTM=`date +'%Y%m%d%T'`
EXT=${OUT}
echo ${MAQUI}
echo ${QUEUE}
echo ${RUNTM}
echo ${EXT}
#
#   Set directories
#
#   OPERMO is the directory for sources, scripts and printouts.
#   SOPERM is the directory for input and output files.
#   ROPERM is the directory for big selected output files.
#   DIRBAN is the directory for archive files.
#
PATHA=`pwd`
export FILEENV=`find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print`

. ${FILEENV} ${CASE} ${PREFX}
cd ${HOME_suite}/run

#
#   Set truncation and layers
#
export RESOL=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' `
export NIVEL=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `
export TRUNC=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' `
export LEV=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `

#
cd ${HOME_suite}/run
#
NPROC=1
export cpu_mpi=${NPROC}
export cpu_node=1
export cpu_tot=${NPROC}

SCRIPTFILENAME=${HOME_suite}/run/set$(basename ${0}).${CASE}.$(hostname)
OUTPUTFILEPATH=${SC2_suite}/out_err/${CASE}/${LABELI}; mkdir -p $OUTPUTFILEPATH

cat <<EOT0 > ${SCRIPTFILENAME}
#!/bin/bash -x
#PBS -l walltime=4:00:00
#PBS -W umask=026
#PBS -o aux20-eth4:${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.out
#PBS -e aux20-eth4:${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.err
#PBS -q $QUEUE3
#PBS -S /bin/bash
#PBS -N probability
#
#
#*****************************************************************#
#                                                                 #
#       Name:           setprobability${RESOL}${NIVEL}.${MAQUI}   #
#                                                                 #
#       Function:       This script file is used to set the       #
#                       environmental variables and start         #
#                       the week mean precipitation.              #
#                                                                 #
#*****************************************************************#
#
#  At SX6 Both the output (stdout) and the error
#  messages (stderr) are written to the same file
#
#   Set date (day,month,year) and hour (hour:minute) 
#
#   DATE=yyyymmdd
#   HOUR=hh:mn:ss
#
DATE=`date +'%Y%m%d'`
HOUR=`date +'%T'`
export DATE HOUR
#
cd ${HOME_suite}/run
#
#   Set Horizontal Truncation and Vertical Layers
#
LEV=${NIVEL}
TRUNC=${RESOL}
export TRUNC LEV
#
#   Set machine
export MACH
#
#   Set option for compiling or not the source codes.
#
#   If COMPILE=make then only the modified sources will be compiled.
#   If COMPILE=clean then the touch files will be removed and 
#              all sources will be compiled.
#             =run for run with no compilation
#
#   If COMPILE is make or clean then the script generates the binary file 
#              and exits;
#              if it is run then the script runs the existent binary file.
#

COMPILE=${1}
export COMPILE
echo \${COMPILE}
#
# Define variables to generate variable data file names:
#
OUT=${EXT}
export OUT
#
EXTS=S.unf
export EXTS 
#
#   Set SX6 FORTRAN variables for output time diagnostics
#
#   F_PROGINF gives the elapsed, user, system and vector instruction
#             execution time, and execution count of all instructions
#             and number of vector instruction executions.
#   F_FILEINF gives informations about I/O operations.
#
F_PROGINF=DETAIL
export F_PROGINF
#
#   Set FORTRAN compilation flags
#
#   -float0 floating-point data format IEEE is enabled
#   -ew     sets the basic numeric size to 8 bytes
#
#   Set FORTRAN environment file name
#
#   FFFn is associated with FORTRAN file unit = n
#
#FFF=F_FF
#export FFF
#
#   Set environmental variables to binary conversion
#
#
export F_UFMTENDIAN=80
#F_UFMTIEEE=70,71,80
#export F_UFMTIEEE
#F_UFMTADJUST70=TYPE2
#F_UFMTADJUST71=TYPE2
#F_UFMTADJUST80=TYPE2
#export F_UFMTADJUST70 F_UFMTADJUST71 F_UFMTADJUST80
#
F_SETBUF=2048
export F_SETBUF
echo " F_SETBUF = \${F_SETBUF}"
#
#  Now, verify if compile or run
#
#
#   Run prcmed
#
#Parameter to be read by prcmed.f90 : namelist file
#UNDEF     : ( REAL    ) undef value set up according to original grib files
#IMAX      : ( INTEGER ) number of points in zonal direction
#JMAX      : ( INTEGER ) number of points in merdional direction
#NMEMBERS  : ( INTEGER ) number of members of the ensemble
#NFCTDY    : ( INTEGER ) number of forecast days
#FREQCALC  : ( INTEGER ) interval in hours for computing the probability precipitation
#DIRINP    : ( CHAR    ) input directory (ensemble members)
#DIROUT    : ( CHAR    ) output directory (probability outputs)
#RESOL     : ( CHAR    ) horizontal and vertical model resolution
#PREFX     : ( CHAR    ) preffix for input and output files 
cat <<EOT > ${SC1_suite}/produtos/probability/bin/probsetup.${LABELI}.nml
UNDEF     :   9.999E+20
IMAX      :   ${IR}
JMAX      :   ${JR}
NMEMBERS  :   ${NMEMBR}
NFCTDY    :   ${NFCTDY}
FREQCALC  :   6
DIRINP    :   ${SC2_suite}/pos/dataout/\${TRUNC}\${LEV}/${LABELI}/
DIROUT    :   ${SC2_suite}/produtos/probability/dataout/\${TRUNC}\${LEV}/${LABELI}/
RESOL     :   \${TRUNC}\${LEV}
PREFX     :   ${PREFX}
EOT
mkdir -p ${SC2_suite}/produtos/probability/dataout/\${TRUNC}\${LEV}/${LABELI}/
#
#-------------------------------------------------------------
mkdir -p ${SC2_suite}/probability/dataout/\${TRUNC}\${LEV}
#
#   Run the probability fortran program
#
cd ${SC1_suite}/produtos/probability/bin
${SC1_suite}/produtos/probability/bin/probability.x ${LABELI}

arqlist=prob${LABELI}${LABELF}.${RESOL}.lst
ls -l --full-time ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/prob${LABELI}* | awk '{print \$9}'> ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/\${arqlist}

rm -f ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/filefct${LABELI}.${TRC}
grep ctl ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/\${arqlist} | grep prob${LABELI}2 > ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/filefct${LABELI}.${TRC}

#
# Number of ctl files on the list 
#
cd ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/

nblst=\$(wc -l ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/filefct${LABELI}.${TRC} | awk '{ print \$1 }')
echo "nblst="\${nblst}

#
# Generate the figures
#
cd ${HOME_suite}/produtos/probability/scripts
export GADLIB="$GADLIB ${HOME_suite}/produtos/probability/scripts"


mkdir -p ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/gif/
echo "${RESOL} ${TRC} ${LABELI} \${nblst} ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/ ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/gif/"
/${GRADSB}/grads -lb << EOT
run ${HOME_suite}/produtos/probability/scripts/plot_precprob.gs
${RESOL} ${TRC} ${LABELI} \${nblst} ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/ ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/gif/
EOT
rm -f ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/filefct${LABELI}.${TRC}

YYYY=`echo $LABELI | cut -c 1-4`
MM=`echo $LABELI | cut -c 5-6`
DD=`echo $LABELI | cut -c 7-8`
HH=`echo $LABELI | cut -c 9-10`

#CRIA TEMPLATE
DATECTLLX=\$(date -d "\${YYYY}\${MM}\${DD} \${HH}:00 1 day" +"%HZ%d%b%Y")

cat << EOF > ${SC2_suite}/produtos/probability/dataout/${CASE}/${LABELI}/prob${LABELI}.ctl
DSET ^prob${LABELI}%y4%m2%d2%h2.${CASE}.bin
*
OPTIONS SEQUENTIAL BIG_ENDIAN YREV TEMPLATE
*
UNDEF -2.56E+33
*
TITLE PROBABILITIES FROM ENS CPTEC AGCM v3.0 1999 ${RESOL}  COLD
*
XDEF    384  LINEAR    0.000000   0.937500
YDEF    192  LEVELS
 -89.28423 -88.35700 -87.42430 -86.49037 -85.55596 -84.62133 -83.68657 -82.75173
 -81.81684 -80.88191 -79.94696 -79.01199 -78.07701 -77.14201 -76.20701 -75.27199
 -74.33697 -73.40195 -72.46692 -71.53189 -70.59685 -69.66182 -68.72678 -67.79173
 -66.85669 -65.92165 -64.98660 -64.05155 -63.11650 -62.18145 -61.24640 -60.31135
 -59.37630 -58.44124 -57.50619 -56.57114 -55.63608 -54.70103 -53.76597 -52.83091
 -51.89586 -50.96080 -50.02574 -49.09069 -48.15563 -47.22057 -46.28551 -45.35045
 -44.41540 -43.48034 -42.54528 -41.61022 -40.67516 -39.74010 -38.80504 -37.86998
 -36.93492 -35.99986 -35.06480 -34.12974 -33.19468 -32.25962 -31.32456 -30.38950
 -29.45444 -28.51938 -27.58431 -26.64925 -25.71419 -24.77913 -23.84407 -22.90901
 -21.97395 -21.03889 -20.10383 -19.16876 -18.23370 -17.29864 -16.36358 -15.42852
 -14.49346 -13.55839 -12.62333 -11.68827 -10.75321  -9.81815  -8.88309  -7.94802
  -7.01296  -6.07790  -5.14284  -4.20778  -3.27272  -2.33765  -1.40259  -0.46753
   0.46753   1.40259   2.33765   3.27272   4.20778   5.14284   6.07790   7.01296
   7.94802   8.88309   9.81815  10.75321  11.68827  12.62333  13.55839  14.49346
  15.42852  16.36358  17.29864  18.23370  19.16876  20.10383  21.03889  21.97395
  22.90901  23.84407  24.77913  25.71419  26.64925  27.58431  28.51938  29.45444
  30.38950  31.32456  32.25962  33.19468  34.12974  35.06480  35.99986  36.93492
  37.86998  38.80504  39.74010  40.67516  41.61022  42.54528  43.48034  44.41540
  45.35045  46.28551  47.22057  48.15563  49.09069  50.02574  50.96080  51.89586
  52.83091  53.76597  54.70103  55.63608  56.57114  57.50619  58.44124  59.37630
  60.31135  61.24640  62.18145  63.11650  64.05155  64.98660  65.92165  66.85669
  67.79173  68.72678  69.66182  70.59685  71.53189  72.46692  73.40195  74.33697
  75.27199  76.20701  77.14201  78.07701  79.01199  79.94696  80.88191  81.81684
  82.75173  83.68657  84.62133  85.55596  86.49037  87.42430  88.35700  89.28423
ZDEF   1 LEVELS 1000
TDEF   57 LINEAR \${DATECTLLX} 06HR
*
VARS  4
PROB1   0 99 PROB. OF 24HR ACCUMULATED PRECIPITATION >  1.0 mm
PROB5   0 99 PROB. OF 24HR ACCUMULATED PRECIPITATION >  5.0 mm
PROB10  0 99 PROB. OF 24HR ACCUMULATED PRECIPITATION > 10.0 mm
PROB20  0 99 PROB. OF 24HR ACCUMULATED PRECIPITATION > 20.0 mm
ENDVARS
EOF

exit 0
EOT0
#

echo "qsub  $SCRIPTFILENAME"

export PBS_SERVER=aux20-eth4

JID=$(qsub ${SCRIPTFILENAME} | awk -F "." '{print $1}')
it=2
while [ ${it} -gt 0 ];do
	it=`qstat | grep $JID | wc -l`
	sleep 3
done

exit 0
