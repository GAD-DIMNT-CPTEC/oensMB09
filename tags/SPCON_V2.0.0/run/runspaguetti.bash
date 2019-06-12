#!/bin/bash
#help#
#**********************************************************#
#                                                          #
#     Name:           spaguetti.scr                        #
#                                                          #
#     Function:       This script plots the spaguetti      #
#                     diagrams from the ensemble.          #
#                     It runs in K shell.                  #
#                                                          #
#     Date:           February   05th, 2002.               #
#     Last change:    August     15th, 2002.               #
#                                                          #
#     Valid Arguments for spaguetti.scr                    #
#                                                          #
#     First :         : help or                            #
#     First : TRC     : horizontal resolution              #
#    Second : LV      : vertical resolution                #
#     Third : LABELI  : initial forecasting label          #
#    Fourth : NFDAYS  : number of forecast days            #
#     Fifth : NMEMBR  : number of members of the ensemble  #
#     Sixth : PREFX   : preffix for input and output files #
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
CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `

PATHA=`pwd`
export FILEENV=`find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print`

. ${FILEENV} ${CASE} ${PREFX}

let NMEMBR=$NPERT*2+1

LABELS=`echo $LABELI | cut -c 1-8`
HH=`echo $LABELI | cut -c 9-10`
set -x
LABELF=$(date -d "$LABELS ${NFDAYS} days" +"%Y%m%d${HH}")

cd ${HOME_suite}/run

RESOL=TQ${TRC}L${LV}
TRC=TQ${TRC}

#
# Set directories
#

YY=`echo ${LABELI} | cut -c 1-4`
MM=`echo ${LABELI} | cut -c 5-6`
DD=`echo ${LABELI} | cut -c 7-8`
HH=`echo ${LABELI} | cut -c 9-10`
echo 'LABELI='${LABELI}
                                                                                                 
DIRSCR=${OPERM}/spaguetti/scripts
DIRGIF=${ROPERM}/spaguetti/gif
DIRCTL=${ROPERM}/pos/dataout/${TRC}L${LV}
DIRENM=${ROPERM}/ensmed/dataout/${TRC}L${LV}
if [ ! -d ${SC2_suite}/produtos/spaguetti/dataout/${CASE}/${LABELI}/gif ]
then
   mkdir -p ${SC2_suite}/produtos/spaguetti/dataout/${CASE}/${LABELI}/gif
else
   echo "${SC2_suite}/produtos/spaguetti/dataout/${CASE}/${LABELI} has already been created"
fi

#
# Evaluate the number of random perturbations
#

let AUX=$NMEMBR-1
let NRNDP=${NPERT}
echo 'NRNDP='${NPERT}

#
# Create files which contains the ctl's and the cluster list
#

cd ${HOME_suite}/produtos/spaguetti/scripts

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
   rm -f filefctNMC${LABELI}.${TRC}
   rm -f fileclt${LABELI}.${TRC}


let NHOURS=24*NFDAYS
NCTLS=0
TIM=0
 while [ ${TIM} -le ${NHOURS} ]
do

   LABELF=$(date -d "${LABELI:0:8} ${LABELI:8:2}:00 ${TIM} hours" +"%Y%m%d%H")
   echo 'LABELF='${LABELF}

   if [ ${TIM} -eq 0 ]
   then
      TYPE='P.icn'
   else
      TYPE='P.fct'
   fi

   NPERT=1
   while [ ${NPERT} -le ${NRNDP} ]
   do
      if [ ${NPERT} -lt 10 ]
      then
         NPERT='0'${NPERT}
      fi

      if [ -s ${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${NPERT}P${LABELI}${LABELF}${TYPE}.${CASE}.ctl ]
      then
cat <<EOT>> filefct${NPERT}P${LABELI}.${TRC}
${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${NPERT}P${LABELI}${LABELF}${TYPE}.${CASE}.ctl
EOT
      else
         echo "${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${NPERT}P${LABELI}${LABELF}${TYPE}.${CASE}.ctl does not exist"
         exit
      fi

      if [ -s ${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${NPERT}N${LABELI}${LABELF}${TYPE}.${CASE}.ctl ]
      then
cat <<EOT>> filefct${NPERT}N${LABELI}.${TRC}
${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${NPERT}N${LABELI}${LABELF}${TYPE}.${CASE}.ctl
EOT
      else
         echo "${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${NPERT}N${LABELI}${LABELF}${TYPE}.${CASE}.ctl does not exist"
         exit
      fi

      let NPERT=NPERT+1
   done

   if [ -s ${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFX}${LABELI}${LABELF}${TYPE}.${CASE}.ctl ]
   then
cat <<EOT>> filefct${PREFX}${LABELI}.${TRC}
${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFX}${LABELI}${LABELF}${TYPE}.${CASE}.ctl
EOT
   else
      echo "${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFX}${LABELI}${LABELF}${TYPE}.${CASE}.ctl does not exist"
      exit
   fi

   if [ -s ${SC2_suite}/ensmed/dataout/${CASE}/${LABELI}/GPOSENM${LABELI}${LABELF}${TYPE}.${CASE}.ctl ]
   then
cat <<EOT>> filefctENM${LABELI}.${TRC}
${SC2_suite}/ensmed/dataout/${CASE}/${LABELI}/GPOSENM${LABELI}${LABELF}${TYPE}.${CASE}.ctl
EOT
   else
      echo "${SC2_suite}/ensmed/dataout/${CASE}/${LABELI}/GPOSENM${LABELI}${LABELF}${TYPE}.${CASE}.ctl does not exist"
      exit
   fi

   let NCTLS=NCTLS+1
   let TIM=TIM+6
done

echo "NCTLS="${NCTLS}
mkdir -p ${SC2_suite}/produtos/spaguetti/dataout/${CASE}/${LABELI}/gif
#
# Plot the figures
#
${GRADSB}/grads -bp << EOT
run sptas.gs
${TRC} ${LABELI} ${NMEMBR} ${NCTLS} ${CASE} ${PREFX} ${SC2_suite}/produtos/spaguetti/dataout/${CASE}/${LABELI}/gif
EOT
echo ${TRC} ${LABELI} ${NMEMBR} ${NCTLS} ${CASE} ${PREFX} ${SC2_suite}/produtos/spaguetti/dataout/${CASE}/${LABELI}/gif

${GRADSB}/grads -bp << EOT
run sptgl.gs
${TRC} ${LABELI} ${NMEMBR} ${NCTLS} ${CASE} ${PREFX} ${SC2_suite}/produtos/spaguetti/dataout/${CASE}/${LABELI}/gif
EOT
echo ${TRC} ${LABELI} ${NMEMBR} ${NCTLS} ${CASE} ${PREFX} ${SC2_suite}/produtos/spaguetti/dataout/${CASE}/${LABELI}/gif

#
# Remove temporary files
#

NPERT=1
while [ ${NPERT} -le ${NRNDP} ]; do
   if [ ${NPERT} -lt 10 ];    then
      NPERT='0'${NPERT}
   fi
   rm -f filefct${NPERT}P${LABELI}.${TRC}
   rm -f filefct${NPERT}N${LABELI}.${TRC}
   let NPERT=NPERT+1
done
rm -f filefct${PREFX}${LABELI}.${TRC}
rm -f filefctENM${LABELI}.${TRC}
rm -f putgif.${LABELI}.${TRC}

exit 0


