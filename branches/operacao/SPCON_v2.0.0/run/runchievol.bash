#!/bin/ksh 
#help#
#*******************************************************************#
#                                                                   #
#      Name:           chi_evol.scr                                 #
#                                                                   #
#      Function:       This script produces week mean               #
#                      precipitation figures                        #
#                                                                   #
#      Date:           March 15th, 2005.                            #
#      Last change:    March 15th, 2005.                            #
#                                                                   #
#      Valid Arguments for week_mean_prec.scr                       #
#                                                                   #
#      First  : TRC    : three-digit triangular truncation          #
#      Second : LV     : two-digit number of vertical sigma-layers  #
#      Third  : LABELI : initial forecasting label                  #
#      Fourth : NFDAYS : number of forecasting days                 #
#                                                                   #
#              LABELx : yyyymmddhh                                  #
#                       yyyy = two digit year                       #
#                       mm   = two digit month                      #
#                       dd   = two digit day                        #
#                       hh   = two digit hour                       #
#                                                                   #
#*******************************************************************#
echo "INICIANDO... \(set \-x\)"

LABELI=$3
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

LABELS=`echo $LABELI | cut -c 1-8`
TRC=$1
LV=$2
PREFX=$4

CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `

PATHA=`pwd`
export FILEENV=`find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print`

. ${FILEENV} ${CASE} ${PREFX}

LABELF=`date -d "$LABELS $NFDAYS days" +"%Y%m%d${HH}"`

cd ${HOME_suite}/produtos/chievol/scripts

NMEMBR=`echo "${NPERT}*2+1" | bc -l`
OUT=out
NPROC=1
#
# Set TERM
#
TERM=vt100; export TERM

#
#   Set LABELF
#
LABELF=`date -d "${LABELI:0:8} ${LABELI:8:2}:00 ${NFDAYS} days" +"%Y%m%d%H"`

#
#   Set Horizontal Truncation (TRUNC) and Vertical Layers (LEV)
#
RESOL=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }'`
NIVEL=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `
CASE=$RESOL$NIVEL

#
# Set directories
#
#   DIRSCR: is the directory of the scripts which grib model data
#
yydir=`awk 'BEGIN {print substr("'${LABELI}'",1,4)}'`
mmdir=`awk 'BEGIN {print substr("'${LABELI}'",5,2)}'`
dddir=`awk 'BEGIN {print substr("'${LABELI}'",7,2)}'`
#
#   Set prefix of files
#
GPOS=GPOSENM


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Generate the list of ctl's to be opened
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm -f filefct${LABELI}.${RESOL}

echo "${SC2_suite}/ensmed/dataout/${CASE}/${LABELI}/${GPOS}${LABELI}${LABELI}P.icn.${CASE}.ctl" >> filefct${LABELI}.${RESOL}

for nd in 5 10 15
do

LABELPF=`date -d "${LABELI:0:8} ${LABELI:8:2}:00  ${nd} days" +"%Y%m%d%H"`

echo "${SC2_suite}/ensmed/dataout/${CASE}/${LABELI}/${GPOS}${LABELI}${LABELPF}P.fct.${CASE}.ctl" >> filefct${LABELI}.${RESOL}

done

# End of generation the list of ctl's to be opened ++++++


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Produce the graphics
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#NWK -> number of files to be opened
NWK=4
mkdir -p ${SC2_suite}/produtos/chievol/dataout/${CASE}/${LABELI}/
${GRADSB}/grads -bpc << EOT
run plot_chi_evol.gs
${LABELI} ${LABELF} ${NWK} ${CASE} ${RESOL} ${SC2_suite}/produtos/chievol/dataout/${CASE}/${LABELI}/
EOT

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Remove temporary files
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm -f filefct${LABELI}.${RESOL}

exit

