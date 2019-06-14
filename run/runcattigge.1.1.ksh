#!/bin/ksh


echo "INICIANDO... \(set \+x\)"
set +x

. ../include/config.sx6

LABELI=$1
if [ -s $LABELI ]; then
      echo "ERRO: FALTA PARAMETRO.\nruncattigge.1.1.ksh YYYYMMDDHH PREFX"
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

set +x

# ./z_tigge_c_sbsj_${LABELI}0000_glob_prod_cf_pl_0006_000_0850_gh.grib

DIRTIGGEHSM=/rede/nas/modoper/tigge/z_tigge_c_sbsj_${LABELI}0000_glob
#DIRTIGGEHSM=
typeset -RZ4 HHH
typeset -RZ3 imemb

imemb=$2
if [ -s ${imemb} ]; then
      echo "ERRO: FALTA PARAMETRO.\nruncattigge.1.1.ksh YYYYMMDDHH PREFX"
      exit 1
fi

if [ ${imemb} -eq 000 ]; then
      cfpf=cf
else
      cfpf=pf
fi
echo "CONCATENANDO MEMBRO: ${imemb}"

HHH=0
while [ $HHH -le 360 ]; do

# CONCATENA ARQUIVOS EM PL
arqcatout=${ROPERM}/tigge/dataout/concat/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}
arqcatoutext=${ROPERM}/tigge/dataout/concat/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_
mkdir -p ${ROPERM}/tigge/dataout/concat

rm -f ${arqcatout}.grib
rm -f ${arqcatout}.dsct
touch ${arqcatout}.grib
touch ${arqcatout}.dsct

a=0
for arq in `ls -lt ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_????_*.grib | awk '{print $5":"$9}'`
do
if [ `echo $arq | cut -d: -f1` -ge 100643 ]; then
      if [ $a -eq 0 ]; then
            echo ${arqcatout}.dsct
            let a=a+1
      fi
      arqname=`echo $arq | cut -d: -f2`
      arqsize=`echo $arq | cut -d: -f1`

      #CONCATENACAO DOS ARQUIVOS GRIB EM UM UNICO (PL - PRESSURE LEVELS)
#      set -x
      cat ${arqcatout}.grib $arqname > ${arqcatout}.tmp
#      set +x
      mv ${arqcatout}.tmp ${arqcatout}.grib

      # CRIACAO DO ARQUIVO DESCRITOR DOS ARQUIVOS GRIB
      arqlst=`basename ${arqname}`
      echo "${arqlst}:${arqsize}" >> ${arqcatout}.dsct
fi
done

# CONCATENA ARQUIVOS EM SL

arqcatout=${ROPERM}/tigge/dataout/concat/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}

rm -f ${arqcatout}.grib
rm -f ${arqcatout}.dsct
touch ${arqcatout}.grib
touch ${arqcatout}.dsct

#AAF # CAMPOS QUE NAO EXISTEM PARA O PRIMEIRO TEMPO DE PREVISAO:
#AAF # sf
#AAF # st
#AAF # ssr
#AAF # tcc
#AAF # tp

#AAF # CAMPOS NAO PRESENTES NOS MEMBROS PERTURBADOS
#AAF # lsm
#AAF # orog

# CAT PARA OS ARQUIVOS SINGLE LEVEL

a=0
for arq in `ls -ltr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_*.grib | awk '{print $5":"$9}'`
do
if [ `echo $arq | cut -d: -f1` -ge 147643 ]; then
      if [ $a -eq 0 ]; then
            echo ${arqcatout}.dsct
            let a=a+1
      fi
      arqname=`echo $arq | cut -d: -f2`
      arqsize=`echo $arq | cut -d: -f1`

      #CONCATENACAO DOS ARQUIVOS GRIB EM UM UNICO (PL - PRESSURE LEVELS)
      cat ${arqcatout}.grib $arqname > ${arqcatout}.tmp
      mv ${arqcatout}.tmp ${arqcatout}.grib

      # CRIACAO DO ARQUIVO DESCRITOR DOS ARQUIVOS GRIB
      arqlst=`basename ${arqname}`
      echo $arqlst:${arqsize} >> ${arqcatout}.dsct
fi
done

let HHH=$HHH+006
done

teste=`ls -ltr ${ROPERM}/tigge/dataout/concat/z_tigge_c_sbsj_${LABELI}0000_glob_prod*_${imemb}.* | wc -l`
if [ $teste -lt 244 ]; then
      echo " ERRO NA CONTAGEM DE ARQUIVOS DA CONCATENACAO... REFAZER..."
fi
exit 0
