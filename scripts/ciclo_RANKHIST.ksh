#! /bin/ksh

#set -o xtrace

# Caminho para o inctime
inctime=${HOME}/bin/inctime

# Datas de inicio e fim do loop
datai=2014111800
dataf=2015022812

# Variavel e nivel
var=temp
lev=850

# Lags e um array com os lags (de 24 a 360, com intervalos de 24)
set -A Lags `echo $(seq 24 24 360)` 

data=${datai}

# Loop sobre as datas
while [ ${data} -le ${dataf} ]
do
  # Loop sobre os lags
  for lag in ${Lags[@]}
  do
    print "${data} - ${lag}"

    ./RANKHIST.2.1.bash ${data} ${lag}h ${var} ${lev} > saida_rankhist_${data}_${lag}h.txt

    tail -4 saida_rankhist_${data}_${lag}h.txt > rankhist_${data}_${lag}h.txt

  done

  print ""

  # Incrementa a data
  data=`${inctime} ${data} +12hr %y4%m2%d2%h2`

done

exit 0
