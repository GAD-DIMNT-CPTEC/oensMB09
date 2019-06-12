#!/bin/bash

#help#
#************************************************************************************#
#                                                                                    #
# script to run CPTEC Post-processing on PC Clusters under MPI Scali                 #
# and Sun Grid Engine without OpenMP                                                 #
#                                                                                    #
# assumptions: assume present but NOT at the same directory:                         #
#              $FEXE/PostGrib (Post-processing Executable file)                      #
#              $FSCR/POSTIN-GRIB (Post-processing input Namelist file)               #
#                                                                                    #
# usage: run_post_UNA.sh cpu_mpi  cpu_node name TRC LV LABELI LABELF hold            #
# where:                                                                             #
# cpu_mpi: integer, the desired number of mpi processes                              #
# cpu_node: integer, the desired number of mpi processes per shared memory node      #
#************************************************************************************#
#help#
#
#       Help:
#

if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#help#/,/^#help#/p'
  exit 1
fi

if [ -z "${1}" ]
then
  echo "TRC is not set" 
  exit 2
else
  TRC=`echo ${1} | awk '{print $1/1}'`   
fi

if [ -z "${2}" ]
then
  echo "LV is not set" 
  exit 2
else
  LV=`echo ${2} | awk '{print $1/1}'`    
fi

if [ -z "${3}" ]
then
  echo "LABELI is not set" 
  exit 3
else
  export LABELI=${3}  
fi
if [ -z "${4}" ]
then
  echo "LABELF is not set" 
  exit 3
else
  export LABELF=${4}  
fi
if [ -z "${5}" ]
then
  export PREFX=NMC
else
  export PREFX=$5
fi
#
# SETTING THE APPROPRIATED ENVIRONMENT
#
CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
PATHA=`pwd`
export FILEENV=`find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${CASE}
cd ${HOME_suite}/run
set +x

echo "GPOS${PREFX}${LABELI}2*.[if]c[tn].*grb"

#DIRIN=/stornext/online7/pnt/MCGA/${CASE}/pos/2012050912 

DIRIN=${SC2_suite}/pos/dataout/${CASE}/${LABELI}/
DIROUT=${SC2_suite}/produtos/recortes/controle/${CASE}/${LABELI}/
WGRIB=$GRADSB/wgrib
arqOut=GDHN${PREFX}${LABELI}${LABELF}S.fct.${CASE}.grb
arqOutP=GDHN${PREFX}${LABELI}${LABELF}S.fct.${CASE}

cd $DIROUT
rm -f $arqOut
n=0
for arq in $(ls -ltr ${DIRIN}/GPOS${PREFX}${LABELI}2*.[if]c[tn].*grb | grep -v ^l | awk '{print $9}' | sort); do
  ${WGRIB} $arq | egrep ":kpds5=193:|:kpds5=195:" | wgrib -i $arq -grib -o ${arqOut}.tmp
  cat ${arqOut}.tmp >> $arqOut
  rm ${arqOut}.tmp
  let n=$n+1
done
  let n=$n-1

set -x

nlCtl=$(cat ${DIRIN}/GPOS${PREFX}${LABELI}${LABELF}P.fct.${CASE}.ctl | sed -n '/^dset/,/^vars/p'  | wc -l)
nlCtl=$(echo $nlCtl-3 | bc)

#GPOSNMC20120511002012051812P.fct.TQ0213L042.grb
#GDHNNMC20120511002012051812S.fct.TQ0213L042.ctl
echo "CRIANDO ${DIROUT}/${arqOutP}.ct"

head -$nlCtl ${DIRIN}/GPOS${PREFX}${LABELI}${LABELF}P.fct.${CASE}.ctl > ${arqOutP}.ctl
echo "zdef 1 levels 1013"  >> ${arqOutP}.ctl
echo "vars 2" >> ${arqOutP}.ctl
echo "USST    0  193,    1,    0  ** sfc   SURFACE ZONAL WIND STRESS               (PA              )" >> ${arqOutP}.ctl
echo "VSST    0  195,    1,    0  ** sfc   SURFACE MERIDIONAL WIND STRESS          (PA              )" >> ${arqOutP}.ctl
echo "endvars" >> ${arqOutP}.ctl
dateCtl=$(date -d "${LABELF:0:8} ${LABELF:8:2}:00" +"%HZ%d%b%Y" | tr a-z A-Z)
dateCtli=$(date -d "${LABELI:0:8} ${LABELI:8:2}:00 6 hours" +"%HZ%d%b%Y")

cat ${arqOutP}.ctl | sed "s%GPOS%GDHN%" | sed "s%P.fct.%S.fct.%" | sed "s%1 linear ${dateCtl}%${n} linear ${dateCtli}%"> ${arqOutP}.ctl.tmp
mv ${arqOutP}.ctl.tmp ${arqOutP}.ctl

$GRADSB/gribmap -i  ${arqOutP}.ctl

exit 0
