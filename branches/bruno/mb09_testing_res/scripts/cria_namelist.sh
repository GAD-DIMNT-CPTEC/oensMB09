#! /bin/bash

if [ $# -ne 1 ]
then
  echo "Uso: ./cria_namelist.sh <var>"
  echo "Exs: ./cria_namelist.sh uvel"
  echo "     ./cria_namelist.sh vvel"
  echo "     ./cria_namelist.sh umi"
  echo "     ./cria_namelist.sh temp"
  echo "     ./cria_namelist.sh pres"
  exit 1
else
  if [ ${1} == "uvel" ]
  then 
    var=${1}
  elif [ ${1} == "vvel" ] 
  then 
    var=${1}
  elif [ ${1} == "umi" ] 
  then 
    var=${1}
  elif [ ${1} == "temp" ] 
  then 
    var=${1}
  elif [ ${1} == "pres" ] 
  then 
    var=${1}
  else
    echo "Nome incorreto da variável: ${1}"
    exit 1
  fi
fi

VALS=($(cat stdev_${var}.txt))

ncols=4
cont1=0
cont2=1

if [ ${var} == "uvel" ]; then echo "&STZWIN"; fi
if [ ${var} == "vvel" ]; then echo "&STMWIN"; fi
if [ ${var} == "umi" ]; then echo "&STHUMI"; fi
if [ ${var} == "temp" ]; then echo "&STTEMP"; fi
if [ ${var} == "pres" ]; then echo "&STPRES"; fi
for ((i=0;i<=$((ncols*2));i++))
do
  for ((j=0;j<=ncols;j++))
  do
    if [ ${cont2} -lt 10 ]; then cont2=" ${cont2}"; fi

    if [ ${cont2} -ne ${#VALS[@]} ]; then comma=","; else comma=""; fi
 
    if [ ${var} == "uvel" ]; then echo -n " STDU(${cont2})=${VALS[$cont1]}${comma}"; fi
    if [ ${var} == "vvel" ]; then echo -n " STDV(${cont2})=${VALS[$cont1]}${comma}"; fi
    if [ ${var} == "umi" ]; then echo -n " STDQ(${cont2})=${VALS[$cont1]}${comma}"; fi
    if [ ${var} == "temp" ]; then echo -n " STDT(${cont2})=${VALS[$cont1]}${comma}"; fi
    if [ ${var} == "pres" ]; then echo -n " STDP=${VALS[$cont1]}${comma}"; fi

    cont1=$((cont1+1))
    cont2=$((cont2+1))
    
    if [ ${cont1} -eq ${#VALS[@]} ]; then break 2; fi
  done
  echo
done
echo
echo "&END"

exit 0
