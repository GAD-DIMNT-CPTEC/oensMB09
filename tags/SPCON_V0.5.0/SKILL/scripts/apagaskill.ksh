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

LABELR=`date -d "${YYYY}${MM}${DD} ${HH}:00 15 days ago" +"%Y%m%d%H"`
echo $LABELR

for dir in `ls -l ${NAS}/produtos/skill/ | grep ^d | awk '{print $9}'`
do
      if [ $dir -lt $LABELR ]; then
            if [ ${#dir} -eq 10 ]; then
                  echo "Removendo $dir..."
                  rm -rfv ${NAS}/produtos/skill/$dir
            fi
      fi
done

exit 0
