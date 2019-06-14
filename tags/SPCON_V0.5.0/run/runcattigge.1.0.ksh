#!/bin/ksh

echo "INICIANDO... \(set \-x\)"
set -x

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

            LABELF=`date -d "${NFDAYS} day ${YYYY}${MM}${DD}" +"%Y%m%d${HH}"`
            YYYYF=`echo $LABELF |cut -c 1-4`
            MMF=`echo $LABELF |cut -c 5-6`
            DDF=`echo $LABELF |cut -c 7-8`
            HHF=`echo $LABELF |cut -c 9-10`
      fi
fi

set +x

# ./z_tigge_c_sbsj_${LABELI}0000_glob_prod_cf_pl_0006_000_0850_gh.grib

DIRTIGGEHSM=/rede/nas/io_dop/tigge/z_tigge_c_sbsj_${LABELI}0000_glob
#DIRTIGGEHSM=/rede/hsm/tigge/z_tigge_c_sbsj_${LABELI}0000_glob
typeset -RZ4 HHH
typeset -RZ3 imemb

imemb=000
while [ ${imemb} -le 014 ]; do

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
touch ${arqcatout}.grib
rm -f ${arqcatout}.dsct
touch ${arqcatout}.dsct

cat \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_gh.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_gh.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_gh.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_gh.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_gh.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_gh.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_gh.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_gh.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0050_gh.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_q.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_q.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_q.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_q.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_q.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_q.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_q.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_q.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_t.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_t.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_t.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_t.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_t.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_t.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_t.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_t.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_u.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_u.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_u.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_u.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_u.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_u.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_u.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_u.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_v.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_v.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_v.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_v.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_v.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_v.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_v.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_v.grib \
${arqcatout}.grib > ${arqcatout}.tmp
mv ${arqcatout}.tmp ${arqcatout}.grib

SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_gh.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_gh.grib:${SZ}" > ${arqcatout}.dsct
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_gh.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_gh.grib:${SZ}" >> ${arqcatout}.dsct
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_gh.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_gh.grib:${SZ}" >> ${arqcatout}.dsct
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_gh.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_gh.grib:${SZ}" >> ${arqcatout}.dsct
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_gh.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_gh.grib:${SZ}" >> ${arqcatout}.dsct
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_gh.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_gh.grib:${SZ}" >> ${arqcatout}.dsct
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_gh.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_gh.grib:${SZ}" >> ${arqcatout}.dsct
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_gh.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_gh.grib:${SZ}" >> ${arqcatout}.dsct
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0050_gh.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0050_gh.grib:${SZ}" >> ${arqcatout}.dsct
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_q.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_q.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_q.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_q.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_q.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_q.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_q.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_q.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_q.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_q.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_q.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_q.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_q.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_q.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_q.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_q.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_t.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_t.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_t.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_t.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_t.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_t.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_t.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_t.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_t.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_t.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_t.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_t.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_t.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_t.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_t.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_t.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_u.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_u.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_u.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_u.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_u.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_u.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_u.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_u.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_u.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_u.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_u.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_u.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_u.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_u.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_u.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_u.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_v.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_1000_v.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_v.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0925_v.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_v.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0850_v.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_v.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0700_v.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_v.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0500_v.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_v.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0300_v.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_v.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0250_v.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_v.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_pl_${HHH}_${imemb}_0200_v.grib:${SZ}" >> ${arqcatout}.dsct 

# CONCATENA ARQUIVOS EM SL

arqcatout=${ROPERM}/tigge/dataout/concat/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}

rm -f ${arqcatout}.grib
touch ${arqcatout}.grib
rm -f ${arqcatout}.dsct
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

cat \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_10u.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_10v.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_lsm.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_msl.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_orog.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_skt.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_sf.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_st.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_2t.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_sp.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_ssr.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_tcc.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_tcw.grib \
${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_tp.grib \
${arqcatout}.grib > ${arqcatout}.tmp

mv ${arqcatout}.tmp ${arqcatout}.grib

SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_10u.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_10u.grib:${SZ}" > ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_10v.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_10v.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_lsm.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_lsm.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_msl.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_msl.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_orog.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_orog.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_skt.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_skt.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_sf.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_sf.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_st.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_st.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_2t.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_2t.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_sp.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_sp.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_ssr.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_ssr.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_tcc.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_tcc.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_tcw.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_tcw.grib:${SZ}" >> ${arqcatout}.dsct 
SZ=`ls -tlr ${DIRTIGGEHSM}/z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_tp.grib | awk '{print $5}'`
echo "z_tigge_c_sbsj_${LABELI}0000_glob_prod_${cfpf}_sl_${HHH}_${imemb}_0000_tp.grib:${SZ}" >> ${arqcatout}.dsct 

let HHH=$HHH+006
done

let imemb=$imemb+001
done

set -x
#rm -f /rede/hsm/tigge/z_tigge_c_sbsj_${LABELI}0000_glob
mkdir -p /rede/hsm/tigge/z_tigge_c_sbsj_${LABELI}0000_glob
cp -rpf ${ROPERM}/tigge/dataout/concat/z_tigge_c_sbsj_${LABELI}0000_glob_prod_* /rede/hsm/tigge/z_tigge_c_sbsj_${LABELI}0000_glob

rm -Rf /rede/nas/io_dop/tigge/z_tigge_c_sbsj_${LABELI}0000_glob

#APAGA ARQUIVOS DO DIRETORIO CONCAT APOS COPIA-LOS PARA A BANGU (HSM)
find ${ROPERM}/tigge/dataout/concat/ -name "z_tigge_c_sbsj_*" -exec rm -fv {} \;

set +x
exit 0
