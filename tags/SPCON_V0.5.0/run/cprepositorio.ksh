#!/bin/ksh

set +x

. ../include/config.sx6

LABELI=$1
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

            LABELF=`date -d "72 hours ago ${YYYY}${MM}${DD} ${HH}:00" +"%Y%m%d%H"`
            YYYYF=`echo $LABELF |cut -c 1-4`
            MMF=`echo $LABELF |cut -c 5-6`
            DDF=`echo $LABELF |cut -c 7-8`
            HHF=`echo $LABELF |cut -c 9-10`
      fi
fi

set +x

user=`cat ~/.config/io.img0.cptec.inpe.br | head -1`
pass=`cat ~/.config/io.img0.cptec.inpe.br | head -2 | tail -1`
maqi=`cat ~/.config/io.img0.cptec.inpe.br | tail -1`

rm -f ${OPERM}/run/setout/ncftprepositor.${LABELI}.out

while [ ${LABELF} -le ${LABELI} ]; do
      YYYYF=`echo $LABELF |cut -c 1-4`
      MMF=`echo $LABELF |cut -c 5-6`
      DDF=`echo $LABELF |cut -c 7-8`
      HHF=`echo $LABELF |cut -c 9-10`
      echo "COPIANDO: $LABELF"
ncftp -u $user -p $pass $maqi << EOF > ${OPERM}/run/setout/ncftprepositor.${LABELI}.out
cd /
rm -f ll20gfs*
cd /tempo/global/ensemble/campos/${LABELF}
!mkdir -p ${ROPERM}/repositorio/campos/${LABELF}
lcd ${ROPERM}/repositorio/campos/${LABELF}
get *.png
quit
EOF

LABELF=`date -d "12 hours ${YYYYF}${MMF}${DDF} ${HHF}:00" +"%Y%m%d%H"`
done

LABELF=`date -d "15 day ago ${YYYY}${MM}${DD}" +"%Y%m%d${HH}"`
LABELF2=`date -d "84 hours ago ${YYYY}${MM}${DD} ${HH}:00" +"%Y%m%d%H"`
while [ ${LABELF} -le ${LABELF2} ]; do
      YYYYF=`echo $LABELF |cut -c 1-4`
      MMF=`echo $LABELF |cut -c 5-6`
      DDF=`echo $LABELF |cut -c 7-8`
      HHF=`echo $LABELF |cut -c 9-10`
      
      echo "REMOVENDO: ${ROPERM}/repositorio/campos/${LABELF}"
      rm -Rf ${ROPERM}/repositorio/campos/${LABELF}
      
      LABELF=`date -d "12 hours ${YYYYF}${MMF}${DDF} ${HHF}:00" +"%Y%m%d%H"`  
done

cd ${ROPERM}/repositorio/campos/

for dir in `ls`; do

echo "INSERINDO: $dir"
ncftp -u $user -p $pass $maqi << EOF >> ${OPERM}/run/setout/ncftprepositor.${LABELI}.out
mkdir /tempo/global/ensemble/campos/${dir}
cd /tempo/global/ensemble/campos/${dir}
lcd ${ROPERM}/repositorio/campos/${dir}
put *.png
quit
EOF

done

exit 0
