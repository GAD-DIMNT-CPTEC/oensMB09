#!/bin/ksh

. ../include/config.sx6
. ../include/copyhsmensmed.1.0.ksh

LABELI=$1
if [ -s $LABELI ]; then
      echo "ERRO: FALTA PARAMETRO.\nrunensmedg.sx6 YYYYMMDDHH n/h (n=nas, h=bangu)"
      exit 1
else
      if [ ${#LABELI} -lt 10 ]; then
            echo "ERRO: PARAMETRO INCORRETO.\nrunensmedg.sx6 YYYYMMDDHH"
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

if [ -s $2 ]; then
      echo "ERRO: FALTA PARAMETRO. \nrunensmedg.sx6 YYYYMMDDHH n/h (n=nas, h=bangu)"
      exit 3
else
      if [ "$2" = "n" -o "$2" = "N" ]; then
            BANGU=$NAS
            ntentativas=1
      else
            if [ "$2" = "h" -o "$2" = "H" ]; then
                  BANGU=$BANGU
                  ntentativas=180
            else
                  echo "ERRO: PARAMETRO 2 deve ser n ou h"
                  exit 4
            fi

      fi
fi

NFCTDY=$FSCT
let NMEMBR=${NPERT}*2+1
PREFX=${PERT}

set -x

YYYY=`echo $LABELI | cut -c 1-4`
MM=`echo $LABELI | cut -c 5-6`
DD=`echo $LABELI | cut -c 7-8`

#sleep 60 
echo "\n"

i=0
while [ $i -lt 70 ]; do
f=0

ensmed $LABELI $BANGU &

err=`plumes $LABELI $BANGU | tail -1`
if [ $err -eq 0 ]; then
      echo "+++++ PLUMES COPIADO COM SUCESSO"
      let f=$f+1
fi

err=`gpos $LABELI $BANGU | tail -1`
if [ $err -eq 0 ]; then
      echo "+++++ GPOS COPIADO COM SUCESSO"
      let f=$f+1
fi

ensmed $LABELI $BANGU &

err=`gfgh $LABELI $BANGU | tail -1`
if [ $err -eq 0 ]; then
      echo "+++++ GFGH COPIADO COM SUCESSO"
      let f=$f+1
fi

err=`gprg $LABELI $BANGU | tail -1`
if [ $err -eq 0 ]; then
      echo "+++++ GPRG COPIADO COM SUCESSO"
      let f=$f+1
fi

ensmed $LABELI $BANGU &

err=`gfct $LABELI $BANGU | tail -1`
if [ $err -eq 0 ]; then
      echo "+++++ GFCT COPIADO COM SUCESSO"
      let f=$f+1
fi

ensmed $LABELI $BANGU &

err=`gfgn $LABELI $BANGU | tail -1`
if [ $err -eq 0 ]; then
      echo "+++++ GFGN COPIADO COM SUCESSO"
      let f=$f+1
fi

if [ $f -ge 6 ]; then
      echo "+++++ VERIFICANDO ENSEMBLE MEDIO"
      
      err=`ensmed $LABELI $BANGU | tail -1`
      if [ $err -eq 0 ]; then
            echo "+++++ ENSMED COPIADO COM SUCESSO"
            let f=$f+1
      fi
      if [ $f -ge 7 ]; then
            echo "+++++ TODAS AS COPIAS REALIZADAS COM SUCESSO"
            exit 0
      fi

fi

if [ $i -lt 30 ]; then
      sleep 60
fi
#sleep 8

let i=$i+1
done

exit 15
