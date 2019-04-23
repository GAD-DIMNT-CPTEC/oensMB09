#!/bin/bash -x
#help#
#***********************************************************************#
#                                                                       #
#     Name:           runspread.una                                     #
#                                                                       #
#     Function:       This script evaluate the spread of the            #
#                     CPTEC global ensemble forecasting.                #
#                     It runs in Korn Shell.                            #
#                                                                       #
#     Date:           Jul 18th, 2005.                                   #
#     Last change:    Jul 18th, 2005.                                   #
#                                                                       #
#     Valid Arguments for runspread.una:                                #
#                                                                       #
#      First:    COMPILE: help, make, clean or run                      #
#     Second:        TRC: three-digit triangular truncation             #
#      Third:         LV: two-digit number of vertical sigma-layers     #
#     Fourth:     LABELI: initial forecasting label                     #
#      Fifth:     NFCTDY: number of forecasting days                    #
#      Sixth:     NMEMBR: number of members of the ensemble             #
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
if [ "${1}" = "help" -o -z "${1}" ]
then
cat < ${0} | sed -n '/^#help#/,/^#end#/p'
exit 0
fi
#
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
PATHA=`pwd`
export FILEENV=`find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print`

. ${FILEENV} ${CASE} ${PREFX}
cd ${HOME_suite}/run

if [ -s $LABELI ]; then
      echo "ERRO: FALTA PARAMETRO.\nrunmodgmpi.una YYYYMMDDHH"
      exit 1
else
      if [ ${#LABELI} -lt 10 ]; then
            echo "ERRO: PARAMETRO INCORRETO.\nrunmodgmpi.una YYYYMMDDHH"
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


if [ -s $PREFX ]; then
      echo "ERRO - PARAMETRO PERT\nFORMATO: runrectigge.una yyyymmddhh 01N"
      exit 2
fi
NFCTDY=$NFDAYS
NMEMBR=${NPERT}
OUT=out
NPROC=1
#
#  End of setting parameters to run
#
########
#
#
#   Set final forecasting labels and UTC Hour
#

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
MAQUI=una
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

tmstp=$(date +"%s")
SCRIPTFILENAME=${HOME_suite}/run/set$(basename ${0}).${CASE}.$(hostname)
OUTPUTFILEPATH=${SC2_suite}/out_err/${CASE}/${LABELI}; mkdir -p $OUTPUTFILEPATH

#
cat <<EOS_> ${SCRIPTFILENAME}
#PBS -l walltime=4:00:00
#PBS -W umask=026
#PBS -o ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.out
#PBS -e ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.err
#PBS -q $QUEUE3
#PBS -S /bin/bash
#PBS -N SPREAD

#*****************************************************************#
#                                                                 #
#       Name:           setspread${RESOL}${NIVEL}.${MAQUI}        #
#                                                                 #
#       Function:       This script file is used to set the       #
#                       environmental variables and start         #
#                       the evaluation of the ensemble spread.    #
#                                                                 #
#*****************************************************************#
#
#  At UNA Both the output (stdout) and the error
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
#COMPILE=run
#export COMPILE
#echo \${COMPILE}
#
# Define variables to generate variable data file names:
#
OUT=${EXT}
export OUT
#
EXTS=S.unf
export EXTS 
#
#   Set UNA FORTRAN variables for output time diagnostics
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
export F_UFMTENDIAN=70,50

#F_UFMTIEEE=70,50
#export F_UFMTIEEE
#F_UFMTADJUST50=TYPE2
#F_UFMTADJUST70=TYPE2
#export F_UFMTADJUST70 F_UFMTADJUST50
#
F_SETBUF=2048
export F_SETBUF
echo " F_SETBUF = \${F_SETBUF}"
#
#  Now, verify if compile or run
#
if [ "${COMPILE}" != "run" ]
then
cd ${HOME_suite}/produtos/spread/source
make -f Makefile clean
make -f Makefile 
exit
#
else
#
#   Run spread
#
#Parameter to be read by spread.f90 : namelist file
#UNDEF     : ( REAL    ) undef value set up according to original grib files
#IMAX      : ( INTEGER ) number of points in zonal direction
#JMAX      : ( INTEGER ) number of points in merdional direction
#LONW      : ( REAL    ) western longitude for region used to evaluate the clusters 
#LONE      : ( REAL    ) eastern longitude for region used to evaluate the clusters 
#LATS      : ( REAL    ) southern latitude for region used to evaluate the clusters 
#LATN      : ( REAL    ) northern latitude for region used to evaluate the clusters 
#NMEMBERS  : ( INTEGER ) number of members of the ensemble
#NFCTDY    : ( INTEGER ) number of forecast days
#FREQCALC  : ( INTEGER ) interval in hours for computing clusters
#DATALSTDIR: ( CHAR    ) input directory (ensemble members)
#DATAOUTDIR: ( CHAR    ) output directory (spread outputs)
#RESOL     : ( CHAR    ) horizontal and vertical model resolution
#PREFX     : ( CHAR    ) preffix for input and output files 

mkdir -p ${SC2_suite}/produtos/spread/dataout/${CASE}/${LABELI}/gif/

cat <<_E.> ${SC1_suite}/produtos/spread/bin/spreadsetup.${LABELI}.nml
UNDEF     :   9.999E+20
IMAX      :   ${IR}
JMAX      :   ${JR}
LONW      :   -180.00 
LONE      :    180.00 
LATS      :    -90.00
LATN      :     90.00
NMEMBERS  :   ${NMEMBR}
NFCTDY    :   ${NFCTDY}
FREQCALC  :   6
DATALSTDIR:   ${SC2_suite}/pos/dataout/\${TRUNC}\${LEV}/${LABELI}/
DATAOUTDIR:   ${SC2_suite}/produtos/spread/dataout/\${TRUNC}\${LEV}/${LABELI}/
RESOL     :   \${TRUNC}\${LEV}
PREFX     :   ${PREFX}
_E.
#
#-------------------------------------------------------------

#
#   Run the spread fortran program
#
cd ${SC1_suite}/produtos/spread/bin
${SC1_suite}/produtos/spread/bin/spread.x ${LABELI}


cd ${HOME_suite}/produtos/spread/scripts

rm -f filefctENM${LABELI}.${TRC}
rm -f filespr${LABELI}.${TRC}


let NHOURS=24*${NFDAYS}
NCTLS=0
TIM=0

set -x
while [ \${TIM} -le \${NHOURS} ]; do

   LABELF=\$(date -d "${LABELI:0:8} ${LABELI:8:2}:00 \${TIM} hours" +"%Y%m%d%H")
   echo 'LABELF='\${LABELF}

   if [ \${TIM} -eq 0 ]; then
      TYPE='P.icn'
   else
      TYPE='P.fct'
   fi

   if [ -s ${SC2_suite}/ensmed/dataout/${CASE}/${LABELI}/GPOSENM${LABELI}\${LABELF}\${TYPE}.${CASE}.ctl ]; then
cat <<_E.>> filefctENM${LABELI}.${TRC}
${SC2_suite}/ensmed/dataout/${CASE}/${LABELI}/GPOSENM${LABELI}\${LABELF}\${TYPE}.${CASE}.ctl
_E.
   else
      echo "${SC2_suite}/ensmed/dataout/${CASE}/${LABELI}/GPOSENM${LABELI}\${LABELF}\${TYPE}.${CASE}.ctl does not exist"
      exit
   fi

   if [ -s ${SC2_suite}/produtos/spread/dataout/${CASE}/${LABELI}/spread${LABELI}\${LABELF}.${CASE}.ctl ]; then  
cat <<_E.>> filespr${LABELI}.${TRC}
${SC2_suite}/produtos/spread/dataout/${CASE}/${LABELI}/spread${LABELI}\${LABELF}.${CASE}.ctl
_E.
   else
      echo "${SC2_suite}/produtos/spread/dataout/${CASE}/${LABELI}/spread${LABELI}\${LABELF}.${CASE}.ctl does not exist"
      exit
   fi

   let NCTLS=\$NCTLS+1
   let TIM=\$TIM+6
done

echo "NCTLS=\${NCTLS}"

fi

#
# Plot figures
#

echo "grads -lbc run gposens.gs ${TRC} ${LABELI} ${NCTLS} ${RESOL}"
${GRADSB}/grads -lb << EOT
run gposens.gs
${TRC} ${LABELI} \${NCTLS} ${CASE} ${SC2_suite}/produtos/spread/dataout/${CASE}/${LABELI}/gif/
EOT

EOS_

echo "qsub  $SCRIPTFILENAME"

export PBS_SERVER=aux20-eth4

JID=$(qsub ${SCRIPTFILENAME} | awk -F "." '{print $1}')
it=2
while [ ${it} -gt 0 ];do
	it=`qstat | grep $JID | wc -l`
	sleep 3
done

