#!/bin/bash
#help#
#********************************************************************#
#                                                                    #
#     Name:           runperpntg.sx6                                 #
#                                                                    #
#     Function:       This script generated and submit               #
#                     a script to start the global model             #
#                     at a given date/time to integration            #
#                     from initial date till final date.             #
#                                                                    #
#     Date:           May    28th, 2003.                             #
#     Last change:    May    28th, 2003.                             #
#                                                                    #
#     Valid Arguments for runperpntg.sx6                             #
#                                                                    #
#     First    :   HELP: help or nothing for getting help            #
#     First    : COMPILE: help, make, clean or run                   #
#     Second   :    TRC: three-digit triangular truncation           #
#     Third    :     LV: two-digit number of vertical sigma-layers   #
#     Fourth   :    NUM: pertubation number                          #
#     Fifth    :     PT: pertubation suffix                          #        
#     Sixth    : LABELI: initial forecasting label                   #
#     Seventh  : LABELF: final perturbation forecasting label        #
#     Eighth   : NFDAYS: number of forecasting days                  #
#     Nineth   :  HUMID: YES or NO (humidity will be perturbed)      #
#     Tenth    :NUMPERT: number of random perturbations              #
#     Eleventh :  NMSST: SST file name or nothing                    #
#                                                                    #
#     Obs.   : NMSST : default sstaoi                                #
#              LABELx: yyyymmddhh                                    #
#                      yyyy = four digit year                        #
#                        mm = two digit month                        #
#                        dd = two digit day                          #
#                        hh = two digit UTC hour                     #
#********************************************************************#
#help#
#
# Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
    cat < ${0} | sed -n '/^#help#/,/^#help#/p'
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
exit
fi
fi
fi

if [  -z "${2}" ]
then
echo "Second argument is not set: TRC"
exit
else
export TRC=`echo ${2} | awk '{print $1/1}'`
fi

if [ -z "${3}" ]
then
echo "Third argument is not set: LV"
exit
else
export LV=`echo ${3} | awk '{print $1/1}'` 
fi

if [ -z "${4}" ]
then
echo "Fourth argument is not set: NUM"
exit
fi

if [ -z "${5}" ]
then
echo "Fifth argument is not set: PT"
exit
fi
if [ -z "${6}" ]
then
echo "Sixth argument is not set (LABELI: yyyymmddhh)"
exit
fi
if [ -z "${7}" ]
then
echo "Seventh argument is not set (LABELF: yyyymmddhh)"
exit
fi

if [ -z "${8}" ]
then
echo "Eighth argument is not set: NFDAYS"
exit
fi

if [ -z "${9}" ]
then
echo "Nineth argument is not set: HUMID"
exit
else
HUMID=${9}
fi

if [ -z "${10}" ]
then
echo "Tenth argument is not set: NUMPERT"
exit
else
export NUMPERT=`echo ${10} | awk '{print $1/1}'` 
fi

if [ -z "${11}" ]
then
echo "Warning NMSST is not set. Using default: sstaoi"
fi
#
#   Set host, machine, NQS Queue, Run time and Extention
#
COMPILE=${1}
CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
HSTMAQ=`hostname`
MACHINE=una
QUEUE=global.q
RUNTM=`date +'%Y%m%d%H:%M'`
EXT=out
export HSTMAQ MACHINE RUNTM EXT
#
#   Set directories and remote machines
#
#   OPERM : is the directory for sources, scripts and printouts 
#           at SX6.
#   SOPERM: is the directory for input and output files at SX6.
#   ROPERM: is the directory for big selected output files at SX6.
#   CTLDIR: is the directory where the outputs will be available
#           after they where send to the machine RMTCPF.
#   DIRPRD: is the directory of the machine RMTCPR where there
#           are the programs to generate special products.
#   RMTUSR: is the remote user at the telecom machine RMTCPR.
#   RMTCPR: is the remote machine to run the special products.
#   RMTCPF: is the remote archieve machine.
#   RMUSCF: is the remote archieve machine user.
#   RMPWCF: is the remote archieve machine password.
#   RMTCPY: is the remote products machine for external users.
#
PATHA=`pwd`
export FILEENV=`find ${PATHA} -name EnvironmentalVariablesMCGA -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${CASE} ${4}${5}
cd ${HOME_suite}/run
#
CTLDIR=
DIRPRD=
RMTUSR=
RMTCPR=
RMTCPF=
RMUSCF=
RMPWCF=
RMTCPY=
#
export OPERM SOPERM ROPERM CTLDIR DIRPRD
export RMTUSR RMTCPR RMTCPF RMUSCF RMPWCF RMTCPY
#
#   Set Horizontal Truncation (TRUNC) and Vertical Layers (LEV)
#
export TRUNC=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' `
export LEV=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `
NIVELP=K15
export TRUNC LEV NIVELP
NUM=`echo ${4} |awk '{ printf("%2.2d\n",$1)  }' `
NIVELP=K15
PT=${5}
export NUM PT
#
#   Set initial and final forecasting labels and UTC Hour
#
LABELI=${6}
LABELF=${7}
NFDAYS=${8}
export LABELI LABELF NFDAYS
#
#############Set number (fct) post-processing #########
NF=24
export NF
set -x
find ${ROPERM}/ -name "GFCT${NUM}${PT}${LABELI}*" -print -exec rm -f {} \;
set +x

####################################################### 
#
#   Set Perturbation
#
#   Set SST file name
#
if [ -z "${11}" ]
then
NMSST=sstaoi
else
NMSST=${11}
fi
export NMSST
#
cd ${OPERM}/run
#
#
#   Run spectral global model forecast   
#
#PK echo "runpermodgprompi.${MACHINE} ${TRC} ${LV} ${LABELI} ${LABELF} 03 ${NUM}${PT} " 
it=1
export FIRST='     '
export SECOND='     '
export it

#${OPERM}/run/runModel 96 24 1 ENSMODEL${NUM}${PT} ${TRC} ${LV} ${LABELI} ${LABELF} ${NUM}${PT} hold
let it=${it}+1
export it
#ittc=0
#while [ ${ittc} -lt 14 ];do
#cci=0
#if [  -s   ${DK_suite2}/model/dataout/${CASE}/${LABELI}/GFCT${NUM}${PT}${LABELI}${LABELF}F.dir.${CASE}.files  ] ;then
#let cci=cci+20
#fi
#let ittc=cci
#sleep 10
#done

#
# ${DK_suite2}/model/dataout/${CASE}/${LABELI}/GFCT${NUM}${PT}${LABELI}${LABELF}F.fct.${CASE}.files
#                                              GFCT 05      R  2009012100 2009012312F.fct.TQ0126L028
echo runperreconoeof.${MACHINE} ${TRC} ${LV} ${NUM} ${LABELI} ${NFDAYS} ${HUMID} ${NUMPERT}

export PBS_SERVER=aux20-eth4
mkdir -p ${ROPERM}/model/exec/setout
SCRIPTSFILE=setperpntg.1.${NUM}${TRUNC}${LEV}.${LABELI}.${MACHINE}
cat <<EOT0 > ${OPERM}/run/${SCRIPTSFILE}
#!/bin/bash
#
#*********************************************************#
#                                                         #
#      Name:     setperpntg${NUM}${TRUNC}${LEV}.${MACHINE}#
#                                                         #
#      Function: This script runs the spectral            #
#                global numerical weather                 #
#                forecasting at ${MACHINE}.                 #
#                                                         #
#*********************************************************#
#
#PBS -o ${HSTMAQ}:${ROPERM}/model/exec/setout/setperpntg.1.${NUM}${TRUNC}${LEV}.${LABELI}.${MACHINE}.${RUNTM}.${EXT}
#PBS -j oe
#PBS -l walltime=0:40:00
#######PBS -l mppwidth=1
#PBS -l mppnppn=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N ENSANL${NUM}
#PBS -q ${AUX_QUEUE}
export PBS_SERVER=aux20-eth4

#
#
#
#   Change directory to run
#
cd ${OPERM}/run
#
#   Set current sst file name for model run
#
SSTNM=${NMSST}
#echo "+++ cp -rpf ${ROPERM}/model/dataout/T${TRC}L${LV}/GFCT${NUM}${PT}${LABELI}*fct* ${ROPERM}/model/datain"
#
#  Run scripts of recomposition         
#
echo "${OPERM}/runperreconoeof.${MACHINE} ${TRC} ${LV} ${NUM} ${LABELI} ${NFDAYS} ${HUMID} ${NUMPERT} "
${OPERM}/run/runperreconoeof.${MACHINE} ${COMPILE} ${TRC} ${LV} ${NUM} ${LABELI} ${NFDAYS} ${HUMID} ${NUMPERT}
#
# Decomposicao das analises perturbadas
#
sleep 70
${OPERM}/run/rundeco.${MACHINE} ${COMPILE} ${TRC} ${LV} ${NUM} ${LABELI} ${NFDAYS} ${HUMID} ${NUMPERT}
#
#  grib files
#
#echo "rungpgrib.sx6 $LABELI $NOUP"
#rungpgrib.sx6 $LABELI $NOUP
#
### F I M ###
#
exit 0
EOT0
#
echo "chmod a+x setperpntg.1.${NUM}${TRUNC}${LEV}.${MACHINE}"
chmod a+x ${OPERM}/run/${SCRIPTSFILE}
#
#   Submit setperpntg${NUM}${TRUNC}${LEV}.${MACHINE} to NQS ${QUEUE}
#
#echo "qsub ${OPERM}/run/setperpntg.1.${NUM}${TRUNC}${LEV}.${MACHINE} ${QUEUE}"
#qsub ${OPERM}/run/setperpntg.1.${NUM}${TRUNC}${LEV}.${MACHINE} 
${OPERM}/run/${SCRIPTSFILE}
