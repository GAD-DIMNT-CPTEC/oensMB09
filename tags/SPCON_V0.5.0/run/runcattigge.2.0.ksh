#!/bin/ksh -x

. ./config.sx6

echo "INICIANDO... \(set \+x\)"

set -x

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
      fi
fi

set +x

# ./z_tigge_c_sbsj_${LABELI}0000_glob_prod_cf_pl_0006_000_0850_gh.grib

DIRTIGGEHSM=${mDK}/tigge/dataout/T${TRC}L${LV}/${imemb}
#DIRTIGGEHSM=/rede/nas/modoper/tigge/sbsj_200906111200_glob_prod/
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


for plsl in `echo pl sl`
do

   echo "CONCATENANDO MEMBRO: ${imemb}"
   HHH=0
   while [ $HHH -le 360 ]; do
      a=0
      arqcatout=${mDK}/tigge/dataout/concat/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_${plsl}_${HHH}_${imemb}

      rm -f ${arqcatout}.grib
      rm -f ${arqcatout}.dsct
      touch ${arqcatout}.grib
      touch ${arqcatout}.dsct

      set `find  ${DIRTIGGEHSM} -name "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_${plsl}_${HHH}_${imemb}_????_*.grib" -exec ls -ltr {} \; | awk '{print $5":"$9}'`
      shift # APAGA O PRIMEIRO PARAMETRO
      shift  # APAGA O SEGUNDO PARAMETRO

      for arq in `echo $*`
      do
            if [ `echo $arq | cut -d: -f1` -ge 100643 ]; then
               if [ $a -eq 0 ]; then
                  echo ${arqcatout}.dsct
                  let a=a+1
               fi
               arqname=`echo $arq | cut -d: -f2`
               arqsize=`echo $arq | cut -d: -f1`
      
               #CONCATENACAO DOS ARQUIVOS GRIB EM UM UNICO (PL - PRESSURE LEVELS)
      #         set -x
               cat ${arqcatout}.grib $arqname > ${arqcatout}.tmp
      #         set +x
               mv ${arqcatout}.tmp ${arqcatout}.grib
      
               # CRIACAO DO ARQUIVO DESCRITOR DOS ARQUIVOS GRIB
               arqlst=`basename ${arqname}`
               echo "${arqlst}:${arqsize}" >> ${arqcatout}.dsct
            fi
      done

   let HHH=$HHH+006
   done
   
done

teste=`ls -ltr ${mDK}/tigge/dataout/concat/z_tigge_c_sbsj_${LABELI}0000_glob_prod*_${imemb}.* | wc -l`
if [ $teste -lt 244 ]; then
      echo " ERRO NA CONTAGEM DE ARQUIVOS DA CONCATENACAO... REFAZER..."
fi

mkdir -p /rede/hsm/tigge/${YYYY}/${MM}/z_tigge_c_sbsj_${LABELI}0000_glob
cp -rpfv ${mDK}/tigge/dataout/concat/z_tigge_c_sbsj_${LABELI}0000_glob_prod*_${imemb}.* /rede/hsm/tigge/${YYYY}/${MM}/z_tigge_c_sbsj_${LABELI}0000_glob/
rm -f ${mDK}/tigge/dataout/concat/z_tigge_c_sbsj_${LABELI}0000_glob_prod*_${imemb}.*

exit 0

#AAF # CAMPOS QUE NAO EXISTEM PARA O PRIMEIRO TEMPO DE PREVISAO:
#AAF # sf
#AAF # st
#AAF # ssr
#AAF # tcc
#AAF # tp

#AAF # CAMPOS NAO PRESENTES NOS MEMBROS PERTURBADOS
#AAF # lsm
#AAF # orog
