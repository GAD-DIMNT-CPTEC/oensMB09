#!/bin/bash
#help#
#***********************************************************************#
#                                                                       #
#     Name:           runplumes.sx6                                     #
#                                                                       #
#     Function:       This script submits the global                    #
#                     model script to the NQS queue.                    #
#                     It runs in Korn Shell.                            #
#                                                                       #
#     Date:           October 08th, 2002.                               #
#     Last change:    October 08th, 2002.                               #
#                                                                       #
#     Valid Arguments for runplumes.sx6:                                #
#                                                                       #
#     First:     COMPILE: help, make, clean or run                      #
#     Second:        TRC: three-digit triangular truncation             #
#     Third:          LV: two-digit number of vertical sigma-layers     #
#     Fourth:     LABELI: initial forecasting label                     #
#     Fifth:      NFCTDY: number of forecast days                       #
#     Sixth:      NMEMBR: number of members of the ensemble             #
#    Seventh:      PREFX: preffix for input and output files            #
#                                                                       #
#                  LABELx: yyyymmddhh                                   #
#                          yyyy = four digit year                       #
#                            mm = two digit month                       #
#                            dd = two digit day                         #
#                            hh = two digit hour                        #
#                                                                       #
#***********************************************************************#
#help#
#
#       Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
cat < $0 | sed -n '/^#help#/,/^#help#/p'
exit 0
fi
#
#       Test of Valid Arguments
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

if [ -z "${2}" ]
then
echo "Second argument is not set (TRC)"
exit
else
export TRC=`echo ${2} | awk '{print $1/1}'`
fi
if [ -z "${3}" ]
then
echo "Third argument is not set (LV)"
exit
else
export LV=`echo ${3} | awk '{print $1/1}'` 
fi
if [ -z "${4}" ]
then
echo "Sixth argument is not set (LABELI: yyyymmddhh)"
exit
else
LABELI=${4}
fi
if [ -z "${5}" ]
then
echo "Seventh argument is not set (NFDAYS)"
exit
else
NFDAYS=${5}
fi
if [ -z "${6}" ]
then
echo "Eigth argument is not set (NUMPERT)"
exit
else
NPERT=${6}
fi

if [ -z "${7}" ]
then
echo "Eigth argument is not set (NUMPERT)"
exit
else
PREFX=${7}
fi


#
#   Set machine, Run time and Extention
#
export COMPILE=${1}
CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
HSTMAQ=`hostname`
MACHINE=bash
RUNTM=`date +'%Y'``date +'%m'``date +'%d'``date +'%H:%M'`
EXT=out
echo ${MACHINE}
echo ${RUNTM}
echo ${EXT}
#
MAQUI=tupa
PATHA=`pwd`
export FILEENV=`find ${PATHA} -name EnvironmentalVariablesOENS -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${CASE} ${PREFX}
cd ${HOME_suite}/run

if [ -s $LABELI ]; then
      echo "ERRO: FALTA PARAMETRO.\nrunmodgmpi.sx6 YYYYMMDDHH"
      exit 1
else
      if [ ${#LABELI} -lt 10 ]; then
            echo "ERRO: PARAMETRO INCORRETO.\nrunmodgmpi.sx6 YYYYMMDDHH"
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

NFCTDY=${NFDAYS}
let NMEMBR=${NPERT}*2+1
OUT=out
NPROC=1
#
calday ()
{
echo ${LABELI} > labeli.plumes.out
yi=`awk '{ print substr($1,1,4) }' labeli.plumes.out`
mi=`awk '{ print substr($1,5,2) }' labeli.plumes.out`
di=`awk '{ print substr($1,7,2) }' labeli.plumes.out`
hi=`awk '{ print substr($1,9,2) }' labeli.plumes.out`
rm -f labeli.plumes.out
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
#
calday
LABELF=${YF}${MF}${DF}${HF}
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
#MAQUI=bash
if [ "run" != "run" ]
then
  QUEUE=${AUX_QUEUE}

else
  QUEUE=${AUX_QUEUE}

fi
RUNTM=`date +'%Y%m%d%T'`
EXT=${OUT}
echo ${MAQUI}
echo ${QUEUE}
echo ${RUNTM}
echo ${EXT}
export COMPLIE=run
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
cat <<EOT0 > setplumes${RESOL}${NIVEL}.${MAQUI}
#!/bin/bash -x
#PBS -o ${HSTMAQ}:${HOME_suite}/run/setout/setplumes${RESOL}${NIVEL}.${MAQUI}.${RUNTM}.${EXT}
#PBS -j oe
#PBS -l walltime=4:00:00
############PBS -l mppwidth=1
#PBS -l mppnppn=1
#PBS -l mppdepth=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N PLUMES${NUM}
#PBS -q ${QUEUE1}

#*****************************************************************#
#                                                                 #
#       Name:           setplumes${RESOL}${NIVEL}.${MAQUI}        #
#                                                                 #
#       Function:       This script file is used to set the       #
#                       environmental variables and start         #
#                       the global model scripts.                 #
#                                                                 #
#*****************************************************************#
#
#  At SX6 Both the output (stdout) and the error
#  messages (stderr) are written to the same file
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
FFF=F_FF
export FFF
#
#   Set environmental variables to binary conversion
#
F_SETBUF=2048
export F_SETBUF
echo " F_SETBUF = \${F_SETBUF}"
#
#   Set number of processors to use on the execution of the script
#
OMP_NUM_THREADS=${NPROC}
export OMP_NUM_THREADS
#
#  Now, verify if compile or run
#
if [ "${COMPILE}" != "run" ]
then
cd ${HOME_suite}/plumes/source
make -f Makefile.pgi clean
make -f Makefile.pgi
#
else
#
#
#   Run plumes
#
#Parameter to be read by plumes : namelist file
#UNDEF     : ( REAL    ) undef value set up according to original grib files
#IMAX      : ( INTEGER ) number of points in zonal direction
#JMAX      : ( INTEGER ) number of points in merdional direction
#NMEMBERS  : ( INTEGER ) number of members of the ensemble
#NFCTDY    : ( INTEGER ) number of forecast days of model integration
#NFCTHOURS : ( INTEGER ) number of hours of model integration
#FREQCALC  : ( INTEGER ) interval in hours of output ensemble forecast
#DIRINP    : ( CHAR    ) input directory (ensemble members)
#DIROUT    : ( CHAR    ) output directory (probability outputs)
#RESOL     : ( CHAR    ) horizontal and vertical model resolution
#PREFX     : ( CHAR    ) preffix for input and output files 
mkdir -p ${SC2_suite}/plumes/dataout/\${TRUNC}\${LEV}/${LABELI}
cat <<EOT > ${SC1_suite}/plumes/bin/plmsetup.${LABELI}.nml
UNDEF     :   -2.56E33
IMAX      :   ${IR}
JMAX      :   ${JR}
NMEMBERS  :   ${NMEMBR}
NFCTDY    :   ${NFCTDY}
NFCTHOURS :   360
FREQCALC  :   6
DIRINP    :   ${SC2_suite}/pos/dataout/\${TRUNC}\${LEV}/${LABELI}/
DIROUT    :   ${SC2_suite}/plumes/dataout/\${TRUNC}\${LEV}/${LABELI}/
RESOL     :   \${TRUNC}\${LEV}
PREFX     :   ${PREFX}
EOT

cd ${SC1_suite}/plumes/bin

time plumes.exe ${LABELI}
#
#   Transfer files of probability plumes to DEC
#

cd ${HOME_suite}/run
#echo "runtransfer.plumes.${MAQUI} ${TRC} ${LV} ${LABELI}"   
#AMM runtransfer.plumes.${MAQUI} ${TRC} ${LV} ${LABELI}   

fi

EOT0
#
chmod +x setplumes${RESOL}${NIVEL}.${MAQUI}
#
#   Run plumes script
#
echo 'plumes  -- Run script ...'
#
echo 'plumes  -- SUBMITTED TO NQS QUEUE ...'
#
echo "qsub  -W block=true ${HOME_suite}/run/setplumes${RESOL}${NIVEL}.${MAQUI}"

export PBS_SERVER=aux20-eth4
qsub  -W block=true ${HOME_suite}/run/setplumes${RESOL}${NIVEL}.${MAQUI}
