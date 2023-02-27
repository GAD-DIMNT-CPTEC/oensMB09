#! /bin/bash

# Script bash para calcular a diferença entre duas datas no formato YYYYMMDDHH
# Uso: ./date_diff.sh YYYYiMMiDDiHHi YYYYfMMfDDfHHf
# Exemplo:
# ./date_diff.sh 2013010100 2013011600
# 15 dia(s) e 0 hora(s) ou 360 hora(s)
# @cfbastarz

data_anl=${1}
data_fct=${2}

yyyy_anl=$(echo ${data_anl} | cut -c 1-4)
  mm_anl=$(echo ${data_anl} | cut -c 5-6)
  dd_anl=$(echo ${data_anl} | cut -c 7-8)
  hh_anl=$(echo ${data_anl} | cut -c 9-10)

yyyy_fct=$(echo ${data_fct} | cut -c 1-4)
  mm_fct=$(echo ${data_fct} | cut -c 5-6)
  dd_fct=$(echo ${data_fct} | cut -c 7-8)
  hh_fct=$(echo ${data_fct} | cut -c 9-10)

data1=$(date -d "${yyyy_anl}-${mm_anl}-${dd_anl} ${hh_anl}:00:00 UTC" "+%s")
data2=$(date -d "${yyyy_fct}-${mm_fct}-${dd_fct} ${hh_fct}:00:00 UTC" "+%s")

if [ ${data2} -gt ${data1} ]
then
  if [ ${hh_fct} -eq "0" -a ${hh_fct} -ne ${hh_anl} ]
  then 
    hh_diff=$(( 24-${hh_anl} ))
  else    
    hh_diff=$(( ${hh_fct}-${hh_anl} ))
  fi
else
  if [ ${hh_anl} -eq "0" -a ${hh_fct} -ne ${hh_anl} ]
  then 
    hh_diff=$(( 24-${hh_fct} ))
  else    
    hh_diff=$(( ${hh_anl}-${hh_fct} ))
  fi
fi  
  
dd_diff=$(( (${data2}-${data1}) / 86400 ))

hh_tot=$(( ${dd_diff}*24 + ${hh_diff} ))

echo "${dd_diff} dia(s) e ${hh_diff} hora(s) ou ${hh_tot} hora(s)"

exit 0
