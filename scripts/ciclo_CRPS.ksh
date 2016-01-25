#! /bin/ksh

#set -o xtrace

inctime=${HOME}/bin/inctime

datai=2014111712
dataf=2015022812

var=psnm
lev=1000

set -A Lags `echo $(seq 24 24 360)` 

data=${datai}

while [ ${data} -le ${dataf} ]
do
  for lag in ${Lags[@]}
  do
    print "${data} - ${lag}"

    ./CRPS.2.1.bash ${data} ${lag}h ${var} ${lev} > saida_crps_${data}_${lag}h.txt

    tail -4 saida_crps_${data}_${lag}h.txt > crps_${data}_${lag}h.txt

  done
  print ""
  data=`${inctime} ${data} +12hr %y4%m2%d2%h2`
done

exit 0
