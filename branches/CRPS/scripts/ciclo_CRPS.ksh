#! /bin/ksh

#set -o xtrace

# Este script tem por finalidade executar o CRPS para um determinado periodo
# e para todas as previsoes ate 15 dias.
#
# Historico: 20/08/2015 - First crack (cfbastarz)

inctime=${HOME}/bin/inctime

datai=2015030300
dataf=2015033112

data=${datai}

set -A Horarios `echo $(seq 24 24 360)`

while [ ${data} -le ${dataf} ]
do

  for hora in ${Horarios[@]}
  do

    print "${data} ${hora}"

    ${PWD}/CRPS.2.0.bash ${data} ${hora}h

  done

  print ""

  data=`${inctime} ${data} +12hr %y4%m2%d2%h2`

done

exit 0
