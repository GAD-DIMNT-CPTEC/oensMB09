#!/bin/bash
#help#
#***********************************************************************#
#                                                                       #
#     Name:           runprobweek.sx6                                   #
#                                                                       #
#     Function:       This script evaluate the probabilities            #
#                     from CPTEC global ensemble forecasting.           #
#                     It runs in Korn Shell.                            #
#                                                                       #
#     Date:           Apr 04th, 2005.                                   #
#     Last change:    Apr 04th, 2005.                                   #
#                                                                       #
#     Valid Arguments for runprobweek.sx6:                              #
#                                                                       #
#      First:    COMPILE: help, make, clean or run                      #
#     Second:        TRC: three-digit triangular truncation             #
#      Third:         LV: two-digit number of vertical sigma-layers     #
#     Fourth:     LABELI: initial forecasting label                     #
#      Fifth:     NFCTDY: number of forecasting days                    #
#      Sixth:      NPERT: number of members of the ensemble             #
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
echo "NPERT is not set"
exit
else
NPERT=${6}
fi
if [ -z "${7}" ]
then
echo "PREFX is not set"
exit
else
PREFX=${7}
fi

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

if [ -s $LABELI ]; then
      echo "ERRO: FALTA PARAMETRO.\nrunensmedg.sx6 YYYYMMDDHH"
      exit 1
else
      if [ ${#LABELI} -lt 10 ]; then
            echo "ERRO: PARAMETRO INCORRETO.\nrunensmedg.sx6 YYYYMMDDHH"
            exit 2
      else
            YYYY=`echo $LABELI |cut -c 1-4`
            MM=`echo $LABELI |cut -c 5-6`
            DD=`echo $LABELI |cut -c 7-8`
            HH=`echo $LABELI |cut -c 9-10`

            LABELF=`date -d "${NFDAYS} day ${YYYY}${MM}${DD}" +"%Y%m%d${HH}"`
            YYYYF=`echo $LABELF |cut -c 1-4`
            MMF=`echo $LABELF |cut -c 5-6`
            DDF=`echo $LABELF |cut -c 7-8`
            HHF=`echo $LABELF |cut -c 9-10`
      fi
fi

NMEMBR=`echo ${NPERT}*2+1 | bc -l`
OUT=out
NPROC=1

export LABELI LABELF
echo "LABELI="${LABELI}
echo "LABELF="${LABELF}
#
#     Select parameter for the resolution:
#
if [ "run" = "run" ]
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
85) MR=86 ; IR=256 ; JR=128 ; NPGH=26 ;
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
if [ "run" != "run" ]
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
PATHA=`pwd`runprobweek.bash
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


SCRIPTFILENAME=${HOME_suite}/run/set$(basename ${0}).${CASE}.$(hostname)
OUTPUTFILEPATH=${SC2_suite}/out_err/${CASE}/${LABELI}; mkdir -p $OUTPUTFILEPATH

cat <<EOT0 > ${SCRIPTFILENAME}
#PBS -W umask=026
#PBS -o ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.out
#PBS -e ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.err
#PBS -q $QUEUE3
#PBS -S /bin/bash
#PBS -N PROBWEEK
#*****************************************************************#
#                                                                 #
#       Name:           setprobagr${RESOL}${NIVEL}.${MAQUI}       #
#                                                                 #
#       Function:       This script file is used to set the       #
#                       environmental variables and start         #
#                       the week mean precipitation.              #
#                                                                 #
#*****************************************************************#
#
#  At SX6 Both the output (stdout) and the error
#  messages (stderr) are written to the srunprobweek.bashame file
#
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
MACH=${MAQUI}
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
export F_UFMTENDIAN=80,90
#F_UFMTIEEE=80,90
#export F_UFMTIEEE
#F_UFMTADJUST80=TYPE2
#F_UFMTADJUST90=TYPE2
#export F_UFMTADJUST80 F_UFMTADJUST90
##
F_SETBUF=2048
export F_SETBUF
echo " F_SETBUF = \${F_SETBUF}"
#
#  Now, verify if compile or run
#
if [ "${COMPILE}" != "run" ]
then
cd ${HOME_suite}/probweek/source
make -f Makefile clean
make -f Makefile
exit
#
else
#
#   Run probweek
#
#Parameter to be read by probweek.f90 : namelist file
#UNDEF     : ( REAL    ) undef value set up according to original grib files
#IMAX      : ( INTEGER ) number of points in zonal direction
#JMAX      : ( INTEGER ) number of points in merdional direction
#NMEMBERS  : ( INTEGER ) number of members of the ensemble
#NFCTDY    : ( INTEGER ) number of forecast days
#FREQCALC  : ( INTEGER ) interval in hours of output ensemble forecast
#NWEEK     : ( INTEGER ) number of times that will be accumulate the precipitation
#NDACC     : ( INTEGER ) number of days to accumlate the precipitation 
#DATALSTDIR: ( CHAR    ) input directory (ensemble members)
#DATAOUTDIR: ( CHAR    ) output directory (probability outputs)
#PREFX     : ( CHAR    ) preffix for input and output files 
#RESOL     : ( CHAR    ) horizontal and vertical model resolution
mkdir -p ${HOME_suite}/produtos/probweek/dataout/\${TRUNC}\${LEV}/${LABELI}
cat <<EOT > ${SC1_suite}/produtos/probweek/bin/probweek.${LABELI}.nml
UNDEF     :   9.999E+20
IMAX      :   ${IR}
JMAX      :   ${JR}
NMEMBERS  :   ${NMEMBR}
NFCTDY    :   ${NFCTDY}
FREQCALC  :   12
NWEEK     :   2
NDACC     :   7
DATAINDIR :   ${SC2_suite}/pos/dataout/\${TRUNC}\${LEV}/${LABELI}/
DATAOUTDIR:   ${SC2_suite}/produtos/probweek/dataout/\${TRUNC}\${LEV}/${LABELI}/
PREFX     :   ${PREFX}
RESOL     :   \${TRUNC}\${LEV}
EOT
#
#-------------------------------------------------------------

#
#   Run the probability fortran program
#

mkdir -p ${SC2_suite}/produtos/probweek/dataout/\${TRUNC}\${LEV}/${LABELI}/
cd ${SC1_suite}/produtos/probweek/bin
${SC1_suite}/produtos/probweek/bin/probweek.x ${LABELI}

#
#   Transfer files of probability to bangu and generate the figures
#
EXP=Exp1

fi
#
EOT0
#


echo "qsub  $SCRIPTFILENAME"

export PBS_SERVER=aux20-eth4

qsub -W block=true ${SCRIPTFILENAME}

exit 0
