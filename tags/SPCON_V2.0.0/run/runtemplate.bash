#!/bin/bash
TRC=$1
LV=$2

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

PREFX=$4
CASE=$(echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' )

PATHA=$(pwd)
export FILEENV=$(find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print)

. ${FILEENV} ${CASE} ${PREFX}

LABELS=$(echo $LABELI | cut -c 1-8)
LABELF=$(date -d "$LABELS $NFDAYS days" +"%Y%m%d${HH}")

export LANG=EN
for arqctl in $(ls ${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFX}${LABELI}${LABELI}P.icn.${CASE}.grb ${SC2_suite}/ensmed/dataout/${CASE}/${LABELI}/GPOS${PREFX}${LABELI}${LABELI}P.icn.${CASE}.grb); do
   dir=$(dirname $arqctl)
   cd $dir
   arqctl=$(basename $arqctl)
   nomefct=$(echo $arqctl | sed -e s@P.icn.${CASE}@P.fct.${CASE}@g)
   ln -s $arqctl $nomefct
done
cd ${SC2_suite}/pos/dataout/${CASE}/${LABELI}

for arqctl in $(ls ${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFX}${LABELI}${LABELF}P.fct.${CASE}.ctl ${SC2_suite}/ensmed/dataout/${CASE}/${LABELI}/GPOS${PREFX}${LABELI}${LABELF}P.fct.${CASE}.ctl ); do
   DIRTMPL=$(dirname $arqctl})
   CTLORIG=$( basename $arqctl | cut -c 1-43)
   DELNOME=$( basename $arqctl | cut -c 18-43)
   FIMTMPL=$( basename $arqctl | cut -c 28-43)
   NOMETMPL=$(basename $arqctl | cut -c 1-17)
   NFCT=$(ls ${DIRTMPL}/${NOMETMPL}*fct*.grb | wc -l)
  
   DATEORIGCTL=$(echo $DELNOME | cut -c 1-8)
   HHORIGCTL=$(echo $DELNOME | cut -c 9-10)

   DATEORIGCTL=$(date -d "${LABELF:0:8} ${LABELF:8:2}:00" +"%HZ%d%b%Y" | tr a-z A-Z)
   DATENWCTL=$(  date -d "${LABELI:0:8} ${LABELI:8:2}:00" +"%HZ%d%b%Y" | tr a-z A-Z)

   echo $DATEORIGCTL - ${DATENWCTL}

   cat $DIRTMPL/$CTLORIG.ctl   | sed -e s/"${DELNOME}.grb"/"%y4%m2%d2%h2${FIMTMPL}.grb"/g > $DIRTMPL/$NOMETMPL.tmp
   cat $DIRTMPL/$NOMETMPL.tmp  | sed -e s/"${CTLORIG}.idx"/"${NOMETMPL}.gmp"/g            > $DIRTMPL/$NOMETMPL.ctl
   cat $DIRTMPL/$NOMETMPL.ctl  | sed -e s/"${DATEORIGCTL}"/"${DATENWCTL}"/g               > $DIRTMPL/$NOMETMPL.tmp
   cat $DIRTMPL/$NOMETMPL.tmp  | sed -e s/" 1 linear"/" $NFCT linear"/g                   > $DIRTMPL/$NOMETMPL.ctl
   cat $DIRTMPL/$NOMETMPL.ctl  | sed -e s/"options"/"options template"/g                  > $DIRTMPL/$NOMETMPL.tmp
   mv  $DIRTMPL/$NOMETMPL.tmp                                                               $DIRTMPL/$NOMETMPL.ctl

   rm -f $DIRTMPL/$NOMETMPL.gmp
   ${GRADSB}/gribmap -i ${DIRTMPL}/${NOMETMPL}.ctl

done

exit 0

