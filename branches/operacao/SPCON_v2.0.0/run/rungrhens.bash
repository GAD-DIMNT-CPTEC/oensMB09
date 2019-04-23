#!/bin/bash
#help#
#**********************************************************#
#                                                          #
#     Name:           plumes.scr                           #
#                                                          #
#     Function:       This script makes the gifs of        #
#                     probabilistic grid history forecast  #
#                     It runs in K shell.                  #
#                                                          #
#     labeli:           November  2007                     #
#     Last change:    November  2007                       #
#                                                          #
#     Valid Arguments for plumes.scr                       #
#                                                          #
#     First :         : nothing for help or                #
#     First : trcm    : horizontal resolution              #
#    Second : lv      : vertical resolution                #
#     Third : labeli  : initial forecasting label          #
#    Fourth : fctdays : number of forecast days            #
#      Fifth: nmembers: number of members of ensemble      #
#      Sixth:    prefx: preffix for input and output files #
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
# Help:
#
if [ "${1}" = "help" -o -z "${1}" ] 
then
cat < ${0} | sed -n '/^#help#/,/^#help#/p'
exit 0
fi
#
# Test of Valid Arguments
#
if [ -z "${1}" ] 
then
   echo "trcm is not set"
   exit
else
   trc=${1}
fi
if [ -z "${2}" ] 
then
   echo "lv is not set"
   exit
else
   lv=${2}
fi
if [ -z  "${3}" ] 
then
   echo "labeli is not set (yyyymmddhh)"
   exit 1
else
   labeli=${3}
fi
if [ -z "${4}" ] 
then
   echo "fctdays is not set"
   exit
else
   fctdays=${4}
fi
if [ -z "${5}" ] 
then
   echo "nmembers is not set"
   exit
else
   nmembers=${5}
fi
if [ -z "${6}" ] 
then
   echo "prefx is not set"
   exit
else
   prefx=${6}
fi

CASE=`echo ${trc} ${lv} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `

PATHA=`pwd`
export FILEENV=`find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print`

. ${FILEENV} ${CASE} ${prefx}
#
# Set model resolution
#

trunc=`echo ${trc} |awk '{ printf("TQ%4.4d\n",$1)  }' `
lev=`echo ${lv} |awk '{ printf("L%3.3d\n",$1)  }' `
resol=${trunc}${lev}
labelf=$(date -d "${labeli:0:8} ${labeli:8:2} ${fctdays} days" +"%Y%m%d%H")
gname=GFGN
fileloc=LOCMM${labeli}${labelf}.${CASE}
ps=local

#
# Set directories
#

yy=`echo ${labeli} | cut -c 1-4`
mm=`echo ${labeli} | cut -c 5-6`
dd=`echo ${labeli} | cut -c 7-8`
hh=`echo ${labeli} | cut -c 9-10`

#dirbct=/${BANGU}/PLUMES/${yy}/${mm}/${dd}
dirbct=/gfs/dk10/mod_glob/oens/plumes/dataout/T126L28
dirfig=/gfs/dk10/mod_glob/oens/produtos/grh


if [ -z "${ps}" ]
then
ps=psuperf
fi

#
# Set GADDIR
#

#
# Generate the figures of plumes
#
cd ${HOME_suite}/produtos/plumes/scripts/

mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/AC/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/AP/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/DF/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/MA/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/MT/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/PE/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/RJ/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/RR/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/SE/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/WW/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/AL/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/BA/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/ES/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/MG/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/PA/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/PI/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/RN/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/RS/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/SP/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/ZZ/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/AM/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/CE/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/GO/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/MS/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/PB/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/PR/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/RO/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/SC/
mkdir -p ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif/TO/

${GRADSB}/grads -bp << EOT
run plumes.gs
${labeli} ${labelf} ${gname} ${CASE} ${ps} ${nmembers} ${fileloc} ${SC2_suite}/plumes/dataout/${CASE}/${labeli} ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif 1 360
quit
EOT
#
echo ${labeli} ${labelf} ${gname} ${CASE} ${ps} ${nmembers} ${fileloc} ${SC2_suite}/plumes/dataout/${CASE}/${labeli} ${SC2_suite}/produtos/plumes/dataout/${CASE}/${labeli}/gif 1 360
#
exit 0
