#! /bin/bash -x

#./run_perturbations.sh 126 28 2020031300 7 NMC

#help#
#**********************************************************#
#                                                          #
#     Name:           initpert.scr                         #
#                                                          #
#     Function:       This script plots the initial        #
#                     perturbations for the ensemble.      #
#                     It runs in K shell.                  #
#                                                          #
#     Date:           February   05th, 2002.               #
#     Last change:    August     15th, 2002.               #
#                                                          #
#     Valid Arguments for initpert.scr                     #
#                                                          #
#     First :         : help or                            #
#     First : TRC     : horizontal resolution              #
#    Second : LV      : vertical resolution                #
#     Third : LABELI  : initial forecasting label          #
#    Fourth : NMEMBR  : number of members of the ensemble  #
#     Fifth : PREFX   : preffix for input and output files #
#                                                          #
#             LABELx : yyyymmddhh                          #
#                      yyyy = four digit year              #
#                        mm = two digit month              #
#                        dd = two digit day                #
#                        hh = two digit hour               #
#                                                          #
#**********************************************************#
#help#
#
#       Help:
#
#echo "INICIANDO... \(set \-x\)"
#set -x

#. 

LABELI=${1}
if [ -s $LABELI ]
then
  echo "ERRO: FALTA PARAMETRO.\nrunmodgmpi.sx6 YYYYMMDDHH"
  exit 1
else
  if [ ${#LABELI} -lt 10 ]
  then
    echo "ERRO: PARAMETRO INCORRETO.\nrunmodgmpi.sx6 YYYYMMDDHH"
    exit 2
  else
    YYYY=$(echo ${LABELI} |cut -c 1-4)
    MM=$(echo ${LABELI} |cut -c 5-6)
    DD=$(echo ${LABELI} |cut -c 7-8)
    HH=$(echo ${LABELI} |cut -c 9-10)
   
    LABELF=$(date -d "${NFDAYS} day ${YYYY}${MM}${DD}" +"%Y%m%d${HH}")
    YYYYF=$(echo ${LABELF} |cut -c 1-4)
    MMF=$(echo ${LABELF} |cut -c 5-6)
    DDF=$(echo ${LABELF} |cut -c 7-8)
    HHF=$(echo ${LABELF} |cut -c 9-10)
  fi
fi

PREFX=${2}

if [ -s ${PREFX} ]
then
  echo "ERRO - PARAMETRO PERT\nFORMATO: runrectigge.sx6 yyyymmddhh 01N"
  exit 2
fi

NFCTDY=${FSCT}
NMDAYS=${FSCT}
#NMEMBR=$(echo "${NPERT}*2+1" | bc -l)
NMEMBR=$(echo "${NPERT}*2+1" | bc -l) # +1 para ler enmrmv
OUT=out
NPROC=1
RESOL=T${TRC}

RESOL=TQ0${TRC}L0${LV}
TRC=T${TRC}

#
# Set directories
#

YY=$(echo ${LABELI} |cut -c 1-4)
MM=$(echo ${LABELI} |cut -c 5-6)
DD=$(echo ${LABELI} |cut -c 7-8)
HH=$(echo ${LABELI} |cut -c 9-10)

echo 'LABELI='${LABELI}
                                                                                                 
DIRSCR=${OPERM}/perturbations/scripts
DIRGIF=${ROPERM}/perturbations/gif
#DIRCTL=${BANGU}/GPOS/${YY}/${MM}/${DD}
DIRCTL=${ROPERM}/pos/dataout/${TRC}L${LV}
#DIRENM=${BANGU}/ENSMED/${YY}/${MM}/${DD}
DIRENM=${ROPERM}/ensmed/dataout/${TRC}L${LV}

if [ ! -d ${DIRGIF} ]
then
  mkdir -p ${DIRGIF}
else
  echo "${DIRGIF} has already been created"
fi

#
# Evaluate the number of random perturbations
#

let AUX=NMEMBR-1
let NRNDP=AUX/2

echo 'NRNDP='${NRNDP}

#
# Create files which contains the ctl's and the cluster list
#

cd ${DIRSCR}

NPERT=1

while [ ${NPERT} -le ${NRNDP} ]
do
  if [ ${NPERT} -lt 10 ]
  then
    NPERT='0'${NPERT}
  fi

  rm -f filefct${NPERT}P${LABELI}.${TRC}
  rm -f filefct${NPERT}N${LABELI}.${TRC}

  let NPERT=NPERT+1
done

rm -f filefct${PREFX}${LABELI}.${TRC}
rm -f filefctENM${LABELI}.${TRC}
rm -f fileclt${LABELI}.${TRC}

NCTLS=1
LABELF=${LABELI}
TYPE='P.icn'
NPERT=1

while [ ${NPERT} -le ${NRNDP} ]
do
  if [ ${NPERT} -lt 10 ]
  then
    NPERT='0'${NPERT}
  fi

  if [ -s ${DIRCTL}/GPOS${NPERT}P${LABELI}${LABELF}${TYPE}.${RESOL}.ctl ]
  then

cat << EOT >> filefct${NPERT}P${LABELI}.${TRC}
${DIRCTL}/GPOS${NPERT}P${LABELI}${LABELF}${TYPE}.${RESOL}.ctl
EOT

  else
    echo "${DIRCTL}/GPOS${NPERT}P${LABELI}${LABELF}${TYPE}.${RESOL}.ctl does not exist"
    exit
  fi

  if [ -s ${DIRCTL}/GPOS${NPERT}N${LABELI}${LABELF}${TYPE}.${RESOL}.ctl ]
  then

cat << EOT >> filefct${NPERT}N${LABELI}.${TRC}
${DIRCTL}/GPOS${NPERT}N${LABELI}${LABELF}${TYPE}.${RESOL}.ctl
EOT
  else
    echo "${DIRCTL}/GPOS${NPERT}N${LABELI}${LABELF}${TYPE}.${RESOL}.ctl does not exist"
    exit
  fi

  let NPERT=NPERT+1
done

  if [ -s ${DIRCTL}/GPOS${PREFX}${LABELI}${LABELF}${TYPE}.${RESOL}.ctl ]
  then

cat << EOT >> filefct${PREFX}${LABELI}.${TRC}
${DIRCTL}/GPOS${PREFX}${LABELI}${LABELF}${TYPE}.${RESOL}.ctl
EOT

  else
    echo "${DIRCTL}/GPOS${PREFX}${LABELI}${LABELF}${TYPE}.${RESOL}.ctl does not exist"
    exit
  fi

   if [ -s ${DIRENM}/GPOSENM${LABELI}${LABELF}${TYPE}.${RESOL}.ctl ]
   then

cat << EOT >> filefctENM${LABELI}.${TRC}
${DIRENM}/GPOSENM${LABELI}${LABELF}${TYPE}.${RESOL}.ctl
EOT

  else
    echo "${DIRENM}/GPOSENM${LABELI}${LABELF}${TYPE}.${RESOL}.ctl does not exist"
    exit
  fi

echo "NCTLS="${NCTLS}

#
# Plot the figures
#

echo "grads -lb \"run initpert.gs ${TRC} ${LABELI} ${NMEMBR} ${NCTLS} ${RESOL} ${PREFX} ${EXP}\""
$GRADSB/grads -lb << EOT
run initpert.gs
${TRC} ${LABELI} ${NMEMBR} ${NCTLS} ${RESOL} ${PREFX} ${EXP}
EOT

exit 0
