#!/bin/bash -x
#help#
#***********************************************************************#
#                                                                       #
#     Name:           runensmed.una                                     #
#                                                                       #
#     Function:       This script submits the global                    #
#                     model script to the NQS queue.                    #
#                     It runs in Korn Shell.                            #
#                                                                       #
#     Date:           October 08th, 2002.                               #
#     Last change:    October 08th, 2002.                               #
#                                                                       #
#     Valid Arguments for runensmed.una:                                #
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
MACHINE=una
RUNTM=`date +'%Y'``date +'%m'``date +'%d'``date +'%H:%M'`
EXT=out
echo ${MACHINE}
echo ${RUNTM}
echo ${EXT}

#
#   Set directories
#
export FILEENV=`find ${HOME}/oensMB09 -maxdepth 2 -name EnvironmentalVariablesOENS -print`
. ${FILEENV} ${CASE} ${PREFX}

cd ${HOME_suite}/run

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

NFCTDY=$NFDAYS
let NMEMBR=${NPERT}*2+1
#
#     End of setting parameters to run
#####
#
export LABELI
echo "LABELI="${LABELI}
#
#     Select parameter for the resolution:
#
if [ "${COMPILE}" = "run" ]
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
MAQUI=una
if [ "${COMPILE}" != "run" ]
then
  QUEUE=${QUEUE}
  NPROC=1
else
  QUEUE=${QUEUE}
  NPROC=4
  NNODE=4
fi
RUNTM=`date +'%Y%m%d%T'`
EXT=out
echo ${MAQUI}
echo ${QUEUE}
echo ${RUNTM}
echo ${EXT}

#   Set truncation and layers
#
export RESOL=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' `
export NIVEL=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `
export TRUNC=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' `
export LEV=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `
#
cd ${HOME_suite}/run

if [ "${COMPILE}" != "run" ]
then

cat <<EOT0 > setensmed${RESOL}${NIVEL}.${MAQUI}
#!/bin/bash -x
#$ -o $(hostname):${HOME_suite}/run/setout/setensmed${RESOL}${NIVEL}.${MAQUI}.${RUNTM}.${EXT}
#$ -j y
#$ -V
#$ -S /bin/bash
#$ -N Ensmed.$LABELI.${TRUNC}${NIVEL}.${COMPILE}

SC2_suiteOD=${SC2_suite}

cd ${HOME_suite}/ensmed/source
make -f Makefile clean
make -f Makefile

EOT0

chmod +x setensmed${RESOL}${NIVEL}.${MAQUI}
echo "/usr/bin/nqsII/qsub -q ${QUEUE} ${HOME_suite}/run/setensmed${RESOL}${NIVEL}.${MAQUI}"
${HOME_suite}/run/setensmed${RESOL}${NIVEL}.${MAQUI}
exit
else
NPROC=24
export cpu_mpi=${NPROC}
export cpu_node=1
export cpu_tot=${NPROC}

SCRIPTFILENAME=${HOME_suite}/run/set$(basename ${0}).${CASE}.$(hostname)
OUTPUTFILEPATH=${SC2_suite}/out_err/${CASE}/${LABELI}; mkdir -p $OUTPUTFILEPATH

tmstp=$(date +"%s")
#find ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/ -name "GPOS*.???" -exec ln -sf {} ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/ \;

#for arq in $(ls ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/???/GPOS???${LABELI}${LABELI}P.icn.${CASE}.grb); do
#   ln -sf ${arq} ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/
#done
#
#for arq in $(ls ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/???/GPOSNMC${LABELI}*.[if]c[nt].*.ctl); do
#   cp -f ${arq} ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/
#   if [ $(echo $arq | grep fct | wc -l) -eq 1 ]; then
#      nome_arq=$(basename $arq)
#      nome_arq=GBRM${nome_arq:4:23}.grads.grb
#      cat ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/$(basename $arq) | sed -e "s%^dest.*%dset \^${nome_arq}%" > ${nome_arq}.tmp
#      mv ${nome_arq}.tmp ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/$(basename $arq)
#      gribmap -i ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/$(basename $arq)
#   fi
#done

#for arq in $(find ${SC2_suite}/vies_oper/${LABELI}/ -name "*grads.grb" -print); do
#   echo ${arq}
#   nome_arq=$(basename ${arq})
#   nome_arq_pos=GPOS${nome_arq:4:23}P.fct.${CASE}.grb
#   ln -sf ${arq} ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/GPOS${nome_arq:4:23}P.fct.${CASE}.grb
#done

cat <<EOT0 > $SCRIPTFILENAME
#!/bin/bash -x
#PBS -l walltime=0:10:00
#PBS -l mppwidth=${cpu_mpi}
#PBS -l mppnppn=${cpu_node}
#PBS -W umask=026
#PBS -V
#PBS -A CPTEC
#PBS -o $(hostname):${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.out
#PBS -e $(hostname):${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.err
#PBS -q $QUEUE2
#PBS -S /bin/bash
#PBS -N ENSME
#
#*****************************************************************#
#                                                                 #
#       Name:           setensmed${RESOL}${NIVEL}.${MAQUI}        #
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
##   Set date (day,month,year) and hour (hour:minute) 
#
#   DATE=yyyymmdd
#   HOUR=hh:mn:ss
#
DATE=`date +'%Y%m%d'`
HOUR=`date +'%T'`
export DATE HOUR
#
#   Set directories
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
COMPILE=run
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
FFF=F_FF
export FFF
#
#   Set environmental variables to binary conversion
#
F_SETBUF=2048
export F_SETBUF
echo " F_SETBUF = \${F_SETBUF}"
#
#   Run AGCM
#
echo 'MODEL  -- SUBMITTED ...'
#
####################################################################
#                         				           #
#        Os dados devem sempre ser organizados de forma            #
#        que os dados fiquem separados por blocos dos tipos:       #            
#        1o. Inteiros                                              #
#        2o. Floats                                                #
#	 3o. Chars                                                 #
#								   #
####################################################################
#
#
#Parametros a serem lidos pelo programa ensmed.f90
#NBYTE     : ( INTEGER ) number of bytes for each grib point information
#NFCTDY    : ( INTEGER ) number of forecast days
#FREQCALC  : ( INTEGER ) interval in hours for computing ensemble mean
#MEMB      : ( INTEIRO ) number of members of the ensemble
#IMAX      : ( INTEIRO ) number of points in zonal direction
#JMAX      : ( INTEIRO ) number of points in merdional direction
#DATAINDIR : ( CHAR    ) input directory (ensemble members)
#DATAOUTDIR: ( CHAR    ) output directory of ensemble mean
#RESOL     : ( CHAR    ) horizontal and vertical model resolution
#PREFX     : ( CHAR    ) preffix for input and output files 
cat <<EOT > ${SC1_suite}/ensmed/bin/ensmed.${LABELI}.nml
NBYTE     :   2
NFCTDY    :   ${NFCTDY}
FREQCALC  :   6
MEMB      :   ${NMEMBR}
IMAX      :   ${IR}
JMAX      :   ${JR}
DATAINDIR :   ${SC2_suite}/pos/dataout/\${TRUNC}\${LEV}/${LABELI}/
DATAOUTDIR:   ${SC2_suite}/ensmed/dataout/\${TRUNC}\${LEV}/${LABELI}/
RESOL     :   \${TRUNC}\${LEV}
PREFX     :   ${PREFX}
EOT
mkdir -p ${SC2_suite}/ensmed/dataout/\${TRUNC}\${LEV}/${LABELI}/

#
#-------------------------------------------------------------
#
# Verify if all files of grid-history ensemble forecast were produced
# If is True, it calculates the plumes. If is False, makes 600 trials (each trial equal 1 minute)
# 
ntrialmax=600
NMEMBRP=${NMEMBR}
let NP=NMEMBRP-1
let NPR=NP/2

cd ${SC2_suite}/pos/dataout/\${TRUNC}\${LEV}
rm -f ${HOME_suite}/ensmed/bin/name.${LABELI}.list

#
#   Submit probabilities evaluation
#
#
#   Run the ensmed fortran program
#
cd ${SC1_suite}/ensmed/bin
time aprun -n  ${cpu_mpi} -N ${cpu_node} -ss ${SC1_suite}/ensmed/bin/ensmed.x ${LABELI}
#time ${SC1_suite}/ensmed/bin/ensmed.x ${LABELI}
#
#   Transfer files of ensemble mean to DEC
#
exit 0
EOT0

sleep 1
chmod 700  $SCRIPTFILENAME

#
#   Run ensmed script
#
echo 'ensmed  -- Run script ...'
#
echo 'ensmed  -- SUBMITTED TO NQS QUEUE ...'
#
echo "qsub  $SCRIPTFILENAME"

qsub -W block=true ${SCRIPTFILENAME} 
sleep 60

cd ${SC2_suite}/ensmed/dataout/${TRUNC}${LEV}/${LABELI}/
set +e
for arqctl in `ls GPOSENM${LABELI}2*ctl`
do
      ${GRADSB}/gribmap -i ${arqctl}
done

cd ${HOME_suite}/run
./runtemplate.bash $TRC $LV ${LABELI} ENM
fi 

set -e
exit 0
