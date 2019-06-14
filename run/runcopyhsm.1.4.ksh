#!/bin/ksh -x

. ../include/config.sx6

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
      else
            if [ "$2" = "h" -o "$2" = "H" ]; then
                  BANGU=$BANGU
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

aaa=0
itc=0
test=0
while [ 0 ]; do

f=0

a=`pwd`

# COPIA DOS ARQUIVOS DO MODELO (GFCT e GFGH)

cd $ROPERM/model/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/
mkdir -p ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/
mkdir -p ${BANGU}/GPRG/${YYYY}/${MM}/${DD}/

find . -name "GFCT???${LABELI}*" -print | cpio -pdmv ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/ > .out 2>&1 
if [ `cat .out | tail -1 | cut -c1` -eq 0 ]; then
      echo "GFCT COPIADO"
      let f=$f+1
fi

find . -name "GFGH???${LABELI}*" -print | cpio -pdmv ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/ > .out 2>&1 
if [ `cat .out | tail -1 | cut -c1` -eq 0 ]; then
      echo "GFGH COPIADO"
      let f=$f+1
fi

find . -name "GPRG???${LABELI}*" -print | cpio -pdmv ${BANGU}/GPRG/${YYYY}/${MM}/${DD}/ > .out 2>&1 
if [ `cat .out | tail -1 | cut -c1` -eq 0 ]; then
      echo "GPRG COPIADO"
      let f=$f+1
fi


# COPIA DOS ARQUIVOS DO POS (GPOS)
cd $ROPERM/pos/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/
find . -name "GPOS[0A]?[NP]${LABELI}*" -print | cpio -pdmv ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/ > .out 2>&1 
if [ `cat .out | tail -1 | cut -c1` -eq 0 ]; then
      echo "GPOS COPIADO"
      let f=$f+1
fi


cd $ROPERM/pos/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/GRH/${YYYY}/${MM}/${DD}/
find . -name "GFGN[0A]?[NP]${LABELI}*" -print | cpio -pdmv ${BANGU}/GRH/${YYYY}/${MM}/${DD}/ > .out 2>&1 
if [ `cat .out | tail -1 | cut -c1` -eq 0 ]; then
      echo "GFGN COPIADO"
      let f=$f+1
fi


# COPIA DOS ARQUIVOS DO ENSMED (GPOSENM)

if [ "$2" = "n" -o "$2" = "N" ]; then
      numaaa=2
else
      numaaa=0
fi

#AAF if [ $aaa -ge $numaaa ]; then
cd $ROPERM/ensmed/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/
find . -name "GPOSENM${LABELI}*" -print | cpio -pdmv ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/ > .out 2>&1 
if [ `cat .out | tail -1 | cut -c1` -eq 0 ]; then
echo "ENSMED COPIADO"
      let f=$f+1
fi
#AAF fi

# COPIA DOS ARQUIVOS DO PLUMES (?????)
cd $ROPERM/plumes/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/PLUMES/${YYYY}/${MM}/${DD}/
find . -name "*${LABELI}2*" -print | cpio -pdmv ${BANGU}/PLUMES/${YYYY}/${MM}/${DD}/ > .out 2>&1 
if [ `cat .out | tail -1 | cut -c1` -eq 0 ]; then
      echo "PLUMES COPIADO"
      let f=$f+1
fi


# VERIFICACAO PARA SAIR DO LOOP
set -x
if [ $f -eq 7 ]; then
      let test=$test+1
      if [ $test -eq 3 ]; then
            aaa=121
      fi
else
      test=0
      echo "Copias realizadas: $f\ni=$aaa"
fi

if [ $aaa -gt 120 ]; then
      break
fi

sleep 120
echo "sleep 120"
let aaa=$aaa+1
set +x
done

exit 0
