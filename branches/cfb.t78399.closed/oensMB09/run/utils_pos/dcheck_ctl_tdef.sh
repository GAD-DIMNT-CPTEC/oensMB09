#! /bin/bash

data_fmt() {
  yyyy=${1:0:4}
  mm=${1:4:2}
  dd=${1:6:2}
  hh=${1:8:2}

  if [ ${mm} -eq 01 ]; then nmm="JAN"; fi
  if [ ${mm} -eq 02 ]; then nmm="FEB"; fi
  if [ ${mm} -eq 03 ]; then nmm="MAR"; fi
  if [ ${mm} -eq 04 ]; then nmm="APR"; fi
  if [ ${mm} -eq 05 ]; then nmm="MAY"; fi
  if [ ${mm} -eq 06 ]; then nmm="JUN"; fi
  if [ ${mm} -eq 07 ]; then nmm="JUL"; fi
  if [ ${mm} -eq 08 ]; then nmm="AGO"; fi
  if [ ${mm} -eq 09 ]; then nmm="SEP"; fi
  if [ ${mm} -eq 10 ]; then nmm="OCT"; fi
  if [ ${mm} -eq 11 ]; then nmm="NOV"; fi
  if [ ${mm} -eq 12 ]; then nmm="DEV"; fi

  export d1f=${hh}Z${dd}${nmm}${yyyy}

}

#for arq in $(ls *ctl)
for arq in $(find . -maxdepth 2 -type f -name "*.ctl")
do 
  d1=$(basename ${arq} | cut -c 18-27)
  d2=$(cat ${arq} | grep tdef | awk -F " " '{print $4}')

  if [ ${d1} == "2020062000" ]
  then 
    d1="${d1}"
  fi

  if [ ${d2} == "00Z20JUN2020" ]
  then 
    d2="${d2}"
  fi

  data_fmt ${d1}

  if [ ${d1f} != ${d2} ]
  then
    sed -i "s,${d2},${d1f},g" ${arq}
    echo -e "${arq} - ${d1} - ${d2} - ${d1f}"
    echo "fixing ${arq} ..."
  fi

done
