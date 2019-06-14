#!/bin/ksh

. ../../include/config.sx6

LABELI=$1

if [ ${#LABELI} -eq 10 ]; then
      echo LABELI=$LABELI
else
      echo "LABELI NAO DEFINIDO (YYYYMMDDHH)... SAINDO."
      exit 2
fi

YYYY=`echo $LABELI | cut -c 1-4`
MM=`echo $LABELI | cut -c 5-6`
DD=`echo $LABELI | cut -c 7-8`
HH=`echo $LABELI | cut -c 9-10`

LATS=/usr/local/grads/bin/lats4d

out=${mDK}/produtos/recortes/dataout/t${TRC}l${LV}


# FAZ O RECORTE LENDO OS ARQUIVOS DA BANGU CASO NAO ENCONTRE ARQUIVOS NO DK.
if [ -s ${mDK}/pos/dataout/T${TRC}L${LV}/GPOSAVN${LABELI}${LABELI}P.icn.T${TRC}L${LV}.ctl ]; then
      dirarqin1="${mDK}/pos/dataout/T${TRC}L${LV}"
      dirarqin2="${mDK}/ensmed/dataout/T${TRC}L${LV}"
else
      dirarqin1=${BANGU}/GPOS/${YYYY}/${MM}/${DD}
      dirarqin2=${BANGU}/ENSMED/${YYYY}/${MM}/${DD}
fi

for arq in `ls ${dirarqin1}/GPOS???${LABELI}2*ctl ${dirarqin2}/GPOSENM${LABELI}2*ctl`
do

tmp=`basename $arq`

PREFX=`echo $tmp | cut -c 5-40`
ARQINI=GSKL$PREFX
$LATS -i $arq -o ${out}/${ARQINI} \
-grid gaussian \
-format grads_grib \
-levs "1000 850 500 250" \
-vars "psnm temp umes agpl zgeo uvel vvel" \
-table /gfs/home3/io_dop/tempo/regional/worketa/eta/grb/cptec.table

arqinilower=`awk 'BEGIN {print tolower("'$ARQINI'")}'`

mv $out/${arqinilower}.grb $out/${ARQINI}.grb
cat $out/${arqinilower}.ctl | sed -e s%"${arqinilower}"%"${ARQINI}"%g > $out/${ARQINI}.ctl
rm -f $out/${arqinilower}.ctl $out/${arqinilower}.gmp
$GRIBMAP -i $out/${ARQINI}.ctl

done
mkdir -p ${NAS}/produtos/skill/${LABELI}
cd $out
find . -name "GSKL???${LABELI}*" -print | cpio -pdmv ${NAS}/produtos/skill/${LABELI}/

cd ${mHOME}/SKILL/scripts
./apagaskill.ksh ${LABELI}

exit 0
