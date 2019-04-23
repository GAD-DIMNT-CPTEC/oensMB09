#!/bin/bash -x 
#help#
#*******************************************************************#
#                                                                   #
#     Name:           runpntg1.una                                  #
#     runpntg1.una 42 28 2004032600 7 NMC 7 sstwkl                  #
#                                                                   #
#     Function:       This script generated and submit              #
#                     a script to start the global model            #
#                     at a given date/time to integration           #
#                     from initial date till final date.            #
#                                                                   #
#     Date:           June   02th, 2003.                            #
#     Last change:    June   02th, 2003.                            #
#                                                                   #
#     Valid Arguments for runpntg.sx6                               #
#                                                                   #
#     First :   HELP: help or nothing for getting help              #
#     First :    TRC: three-digit triangular truncation             #
#     Second:     LV: two-digit number of vertical sigma-layers     #
#     Third : LABELI: initial forecasting label                     #
#     Fourth: NFDAYS: number of forecasting days                    #
#     Fifth :   PERT: perturbation                                  #
#     Sixth :NUMPERT: number of random perturbations                #
#   Seventh :  NMSST: SST file name or nothing                      #
#                                                                   #
#     Obs.  : NMSST : default sstaoi                                #
#             PERT  : NMC,01P,01N,02P,02N,...                       #
#             LABELx: yyyymmddhh                                    #
#                     yyyy = four digit year                        #
#                       mm = two digit month                        #
#                       dd = two digit day                          #
#                       hh = two digit UTC hour                     #
#*******************************************************************#
#help#
#
# Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
    cat < ${0} | sed -n '/^#help#/,/^#help#/p'
    exit 0
fi
if [ -z "${2}" ]
then
echo "Second argument is not set: LV"
exit
fi
if [ -z "${3}" ]
then
echo "Third argument is not set (LABELI: yyyymmddhh)"
exit
fi
if [ -z "${4}" ]
then
echo "Fourth argument is not set NFDAYS"
exit
fi
if [ -z "${5}" ]
then
echo "Fifth argument is not set PERT"
exit
else
PERR=${5}
fi
if [ -z "${6}" ]
then
echo "Sixth argument is not set NUMPERT"
exit
else
NUMPERT=${6}
fi
if [ -z "${7}" ]
then
echo "Warning NMSST is not set. Using default: sstaoi"
fi
#
#       Testing Valid Arguments
#
if [ -z "${1}" ]
then
TRC=62
else
export TRC=`echo $1 | awk '{print $1/1}'`
fi
if [ -z "${2}" ]
then
LV=28
else
export LV=`echo $2 | awk '{print $1/1}'` 
fi
#
#   Set host, machine, NQS Queue, Run time and Extention
#
CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
HSTMAQ=`hostname` # example una2
RUNTM=`date +'%Y%m%d%H:%M'`
EXT=out
export HSTMAQ MAQUI RUNTM EXT
#
#   Set Perturbation
#
PERT=`echo ${5} |awk '{ printf("%3.3s\n",$1)  }' `
export PERT
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
. ${FILEENV} ${CASE} ${PERT}
cd ${HOME_suite}/run

#################################################
CTLDIR=
DIRPRD=
RMTUSR=
RMTCPR=
RMTCPF=
RMUSCF=
RMPWCF=
RMTCPY=
#################################################
export OPERM SOPERM ROPERM CTLDIR DIRPRD
export RMTUSR RMTCPR RMTCPF RMUSCF RMPWCF RMTCPY
#
#   Set Horizontal Truncation (TRUNC) and Vertical Layers (LEV)
#
export TRUNC=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' `
export LEV=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `
NIVELP=K15
export TRUNC LEV NIVELP
#
#   Set initial and final forecasting labels and UTC Hour
#
LABELI=${3}
export LABELI
#
NFDAYS=${4}
#
calday ()
{
echo ${LABELI} > labeli${PERR}.out
yi=`awk '{ print substr($1,1,4)/1 }' labeli${PERR}.out`
mi=`awk '{ print substr($1,5,2)/1 }' labeli${PERR}.out`
di=`awk '{ print substr($1,7,2)/1 }' labeli${PERR}.out`
hi=`awk '{ print substr($1,9,2)/1 }' labeli${PERR}.out`
rm -f labeli${PERR}.out
let ybi=${yi}%4
if [ ${ybi} = 0 ]
then
declare -a md=( 31 29 31 30 31 30 31 31 30 31 30 31 )
else
declare -a md=( 31 28 31 30 31 30 31 31 30 31 30 31 )
fi
let df=${di}+${NFDAYS}
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
caldaya ()
{
echo ${LABELI} > labeli${PERR}.out
yi=`awk '{ print substr($1,1,4)/1 }' labeli${PERR}.out`
mi=`awk '{ print substr($1,5,2)/1 }' labeli${PERR}.out`
di=`awk '{ print substr($1,7,2)/1 }' labeli${PERR}.out`
hi=`awk '{ print substr($1,9,2)/1 }' labeli${PERR}.out`
rm -f labeli${PERR}.out
let ybi=${yi}%4
if [ ${ybi} = 0 ]
then
declare -a md=( 31 29 31 30 31 30 31 31 30 31 30 31 )
else
declare -a md=( 31 28 31 30 31 30 31 31 30 31 30 31 )
fi
let df=${di}
let mf=${mi}
let yf=${yi}
let hf=${hi}+12
let n=${mi}-1
if [ ${hf} -gt 23 ]
then
let hf=${hf}-24
let df=${df}+1
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
#############Set number (fct) post-processing (out's each 12 hs) #########
calday

UTC=${hi}
if [ ${UTC} -eq 00  -o  ${UTC} -eq 12 ]
then
let NF=2*${NFDAYS}
echo "NF="${NF}
else
caldaya
NF=2
fi
let NFILEOUT=${NF}+2
export UTC NF

####################################################### 
LABELF=${YF}${MF}${DF}${HF}
export LABELF
#
#   Set SST file name
#
if [ -z "${7}" ]
then
NMSST=sstaoi
else
NMSST=${7}
fi
export NMSST
#
######Set some parameters to run ensemble members#########
dhfct=06
dhdhn=0
nhdhn=0
dhext=6
nhext=120
if [ "${PREFIX}" = "CTR" ]; then
prefx=${PREFIX}
prefy=${PREFIX}
else
prefx=${PERT}
prefy=${PERT}
fi
nproc=1
##########################################################
#
######Set parameters to run probability plumes############
let NMEMB=2*${NUMPERT}+1
NPRPLM=4
##########################################################
#

cd ${OPERM}/run
#
#   Run spectral global model forecast
#
set -x
arqfiles=${ROPERM}/model/dataout/${CASE}/${LABELI}/GFCT${prefx}${LABELI}${LABELF}F.dir.${CASE}.files
it=1
export FIRST='     '
export SECOND='     '
export it
if [  -s $arqfiles ]; then
      echo "Modelo ja Rodado .files Gerado Corretamente..."
else
      echo "\n+++ Rodando Modelo AGCMCPT ${CASE}"
      echo "   runModel 60 24 1 ENSMODEL${PERT} ${TRC} ${LV} ${LABELI}  ${LABELF} ${PERT} hold"
${OPERM}/run/runModel 48 24 1 ENSMODEL${PERT} ${TRC} ${LV} ${LABELI}  ${LABELF} ${PERT} hold
let it=${it}+1
export it
fi

#
#CONTROLE DE ARQUIVOS DE SAIDA DO MODELO
#
#sleep 30
arqctrl=`ls -l ${ROPERM}/model/dataout/${CASE}/${LABELI}/GFCT${prefx}${LABELI}* | awk 'BEGIN { s=0 } { s+=$5 } END { printf ( "%17d",s)}'`
if [ $arqctrl -lt 32714064 ]; then
      echo "ERRO NO CONTROLE DE ARQUIVOS DO MODELO, VERIFICANDO NOVAMENTE..."
      sleep 30
      arqctrl=`ls -l ${ROPERM}/model/dataout/${CASE}/${LABELI}/GFCT${prefx}${LABELI}* | awk 'BEGIN { s=0 } { s+=$5 } END { printf ( "%17d",s)}'`
      if [ $arqctrl -lt 32714064 ]; then
            rm -f ${ROPERM}/model/dataout/${CASE}/${LABELI}/G???${prefx}${LABELI}*
            exit 2
      fi
fi
#arqctrl=`ls -l ${ROPERM}/model/dataout/${CASE}/${LABELI}/GFGH${prefx}${LABELI}* | awk 'BEGIN { s=0 } { s+=$5 } END { printf ( "%17d",s)}'`
#if [ $arqctrl -lt 4266189 ]; then
#      echo "ERRO NO CONTROLE DE ARQUIVOS DO MODELO"
#      sleep 30
#      arqctrl=`ls -l ${ROPERM}/model/dataout/${CASE}/${LABELI}/GFGH${prefx}${LABELI}* | awk 'BEGIN { s=0 } { s+=$5 } END { printf ( "%17d",s)}'`
#      if [ $arqctrl -lt 4266189 ]; then
#            rm -f ${ROPERM}/model/dataout/${CASE}/${LABELI}/G???${prefx}${LABELI}*
#            exit 2
#      fi
#fi

set -x
#
#   Run upper-level post-processing
#
echo "\n+++ Pos-Processando Modelo AGCMCPT T${TRC}L${LV} em Formato Binario"
echo "COMENTADO runposb.${MAQUI} ${LABELI} ${prefx}\nRODANDO APENAS GRIB..."

echo "runPos 4 4 EnsPos ${TRC} ${LV}  ${LABELI} ${LABELF} hold"
${OPERM}/run/runPos 1 1 EnsPos${PERT} ${TRC} ${LV}  ${LABELI} ${LABELF} ${PERT} hold
let it=${it}+1
export it
#

sleep 30
arqctrl=`ls -l ${ROPERM}/pos/dataout/${CASE}/${LABELI}/GPOS${prefx}${LABELI}* | awk 'BEGIN { s=0 } { s+=$5 } END { printf ( "%17d",s)}'`
#if [ $arqctrl -lt 2037870824 ]; then
if [ $arqctrl -lt 2616885 ]; then
      echo "ERRO NO CONTROLE DE ARQUIVOS DO POS"
      exit 2
fi


echo "\n+++ Pos-Processando GRH do Modelo AGCMCPT T${TRC}L${LV} em Formato Binario"
echo "runGrh 4 4 EnsGRH  213 42 ${LABELI} ${LABELF}  GFGNNMC psnm hold"
#PK ${OPERM}/run/runGrh  1 1 EnsGRH ${TRC} ${LV}   ${LABELI} ${LABELF}  GFGN${PERT} psnm ${PERT} hold
exit 0
### F I M ###
