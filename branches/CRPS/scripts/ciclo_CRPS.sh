#! /bin/bash

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
# 17/09/2020 - Conversão para Bash e adaptações para o XC50 (cfbastarz)
# CFB

# Caminho para o inctime
inctime=${HOME}/bin/inctime

bpath=/lustre_xc50/carlos_bastarz/CRPS/scripts

# Datas de inicio e fim do loop
datai=2020060100
dataf=2020060500

# Variavel, nivel e dominio
var=temp
lev=850
dominio=HN

data=${datai}

while [ ${data} -le ${dataf} ]
do
  echo "${data}"

  ${bpath}/run_crps.sh TQ0126L028 NMC ${data} ${var} ${lev} ${dominio}
  wait 

  data=$(${inctime} ${data} +1dy %y4%m2%d2%h2)

done

exit 0
