#! /bin/ksh

#set -o xtrace

# CFB
# Objetivo
# Este script complementa o script CRPS.2.1.bash, que e utilizado para
# disparar o programa do CRPS.
# A funcao deste script e apenas realizar um loop entre datas e os lags.
#
# Dependencia;
# - inctime ou caldate
#
# Historico;
# XX/08/2015 - First crack (cfbastarz)
# 05/04/2015 - Comentarios e melhorias (cfbastarz)
# CFB

# Caminho para o inctime
inctime=${HOME}/bin/inctime

# Datas de inicio e fim do loop
datai=2014111712
dataf=2015022812

# Variavel e nivel
var=psnm
lev=1000

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

    # Execura o CRPS e salva a saida no arquivo saida_crps_${data}_${lag}h.txt
    ./CRPS.2.1.bash ${data} ${lag}h ${var} ${lev} > saida_crps_${data}_${lag}h.txt

    # Pega as ultimas 4 linha do arquivo saida_crps_${data}_${lag}h.txt e salva no arquivo
    # crps_${data}_${lag}h.txt (importante para diagnostico)
    tail -4 saida_crps_${data}_${lag}h.txt > crps_${data}_${lag}h.txt

  done

  print ""

  # Incrementa a data
  data=`${inctime} ${data} +12hr %y4%m2%d2%h2`

done

exit 0
