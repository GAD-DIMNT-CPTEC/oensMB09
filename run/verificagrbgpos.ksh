#!/bin/ksh
. ./config.sx6
LABELI=$1
YYYY=`echo $LABELI | cut -c 1-4`
MM=`echo $LABELI | cut -c 5-6`
DD=`echo $LABELI | cut -c 7-8`
HH=`echo $LABELI | cut -c 9-10`

DIR="$BANGU/GPOS"


LABELV=`date -d "$YYYY$MM$DD ${HH}:00 1 days ago" +"%Y%m%d%H"`

while [ ${LABELV} -le ${LABELI} ]; do
      YYYY=`echo $LABELV | cut -c 1-4`
      MM=`echo $LABELV | cut -c 5-6`
      DD=`echo $LABELV | cut -c 7-8`
      HH=`echo $LABELV | cut -c 9-10`
for memb in `echo 01N 01P 02N 02P 03N 03P 04N 04P 05N 05P 06N 06P 07N 07P AVN` ; do
      echo " Verificando: ${DIR}/${YYYY}/${MM}/${DD}/GPOS${memb}${LABELV}.ctl"
      ARQCTL="${DIR}/${YYYY}/${MM}/${DD}/GPOS${memb}${LABELV}.ctl"
      i=1
      f=0
      while [ $i -lt 62 ]; do

            /usr/local/grads-1.9b4/bin/gradsc -blc "open ${ARQCTL}" << EOF > saida.txt
            set t 1
            set lat -30
            set lon -50
            set t $i
            d psnm 
            quit
EOF

            V=`cat saida.txt | grep "Result value = " | cut -d" " -f4 | cut -d. -f1`
            if [ \( $V -gt 850 \) -a \( $V -lt 1100 \) ]; then
                  .
            else
                  echo "TEMPO $V ERRO"
                  echo "TEMPO $V ERRO" >> mail.list
                  f=1
            fi
            let i=$i+1
      done
      if [ $f -eq 1 ]; then
            mail -s "MEMBRO ${memb} $LABELV" alex.fernandes@cptec.inpe.br < mail.list
      fi
      rm -f mail.list saida.txt

done # MEMBROS FOR

      LABELV=`date -d "$YYYY$MM$DD ${HH}:00 12 hours" +"%Y%m%d%H"`
done


exit 0
