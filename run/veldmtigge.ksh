#!/bin/ksh
. ../include/config.sx6


LABELI=$1 #YYYYMMDDHH

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
LABELF=`date -d "$LABELS 15 days" +"%Y%m%d${HH}"`

startinglog=`ssh -l ldm ${tiggeldm} "cat /usr/local/ldm/logs/tigge_send.log | grep sbsj_${LABELI}00_glob | grep Starting | wc -l"`
echo "STARTING: $startinglog"
fileslog=`ssh -l ldm ${tiggeldm} "cat /usr/local/ldm/logs/tigge_send.log | grep sbsj_${LABELI}00_glob | grep Files | cut -d: -f3 | tail -1"`
echo "FILES:    $fileslog"

if [ $startinglog -ge 1 -a $fileslog -ge 48542 ]; then
      echo "\nTIGGE/LDM - APARENTEMENTE OK..."
      echo `ssh -l ldm ${tiggeldm} "cat /usr/local/ldm/logs/tigge_send.log | grep sbsj_${LABELI}00_glob | grep Starting"`
      echo `ssh -l ldm ${tiggeldm} "cat /usr/local/ldm/logs/tigge_send.log | grep sbsj_${LABELI}00_glob | grep Files| tail -1"`
      echo " "
else
      echo "+++"
      echo "+++"
      echo "+++"
      echo "+++ ERRO, ARQUIVOS NAO INSERIDOS OU COM PROBLEMAS NO LDM!"
      echo "+++ ESPERAR VeTIGGE RODAR E VERIFICAR STATUS"
      echo "+++"
      echo "+++"
      echo "+++"
#      exit 2
fi

typeset -ZR3 MEMB
set -A m AVN 01N 02N 03N 04N 05N 06N 07N 01P 02P 03P 04P 05P 06P 07P
MEMB=000
while [ $MEMB -le 014 ]; do

      if [ $MEMB -eq 000 ]; then
            comp=3350
      else
            comp=3228
      fi
      if [ `find ${tiggenas}/z_tigge_c_sbsj_${LABELI}0000_glob -name "*prod*_${MEMB}_*" -print | wc -l` -ne $comp ]; then
            echo "+++ MEMBRO ${m[$MEMB]} COM ERROS NO TIGGE, ESPERAR VeTIGGE RODAR E VERIFICAR STATUS"
      fi
let MEMB=${MEMB}+1
done

exit 0
