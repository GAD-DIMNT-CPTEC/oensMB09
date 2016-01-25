#! /bin/ksh

#set -o xtrace

DATAINDIR=${PWD}/../datain
DATAOUTDIR=${PWD}/../dataout

CURRENTDIR=`pwd`

set -A Lags `echo $(seq 24 24 360)`

# Para nao complicar muito o script, por favor, ajuste firstdate e lastdate com as datas
# iniciais e finais dos meses de interesse (eg., firstdate=2014120100, lastdate=2015022800)

firstdate=2014120112
lastdate=2015022812

targetdate=${firstdate}

hsin=`echo ${firstdate} | cut -c 9-10`

anoi=`echo ${firstdate} | cut -c 1-4`
anof=`echo ${lastdate} | cut -c 1-4`

mesi=`echo ${firstdate} | cut -c 5-6`
mesf=`echo ${lastdate} | cut -c 5-6`

if [ ${mesi} -eq "12" ]; then mesin="dec"; fi
if [ ${mesi} -eq "01" ]; then mesin="jan"; fi
if [ ${mesi} -eq "02" ]; then mesin="fev"; fi

if [ ${mesf} -eq "12" ]; then mesfi="dec"; fi
if [ ${mesf} -eq "01" ]; then mesfi="jan"; fi
if [ ${mesf} -eq "02" ]; then mesfi="fev"; fi

if [ ${mesi} -eq "12" -a ${mesf} -eq "02" ] # periodo completo (dec, jan e fev)
then 
  mesme="jan"
  periodo="${mesin}${mesme}${mesfi}"
elif [ ${mesi} -eq ${mesf} ] # apenas um mes (dec, jan ou fev)
then
  periodo="${mesin}"
elif [ ${mesi} -ne ${mesf} ] # apenas dois meses (dec e jan ou jan e fev)
then
  periodo="${mesin}${mesfi}"
fi

for lag in ${Lags[@]}
do

  arquivo=crpsfilesin.aave-${periodo}${hsin}z-lag${lag}h.txt
  
  rm -f ${DATAOUTDIR}/${arquivo}
  touch ${DATAOUTDIR}/${arquivo}

  while [ ${targetdate} -lt ${lastdate} ]
  do

    set -x
    ls -1 ${DATAOUTDIR}/CRPS4CPTECEPS.ForecastFor${targetdate}.aave.ctl | awk '{print "open "$1}' >> ${DATAOUTDIR}/${arquivo}
    set +x

    targetdate=`${HOME}/scripts/caldate.3.1.2 ${targetdate} + ${lag}h "yyyymmddhh"`

  done

  targetdate=${firstdate}

done
