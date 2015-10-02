#!/bin/bash -x
#
# Descricao:
# Este script executa o script CRPS.2.0.bash para uma determinada data ($TARGETDATE) e varias horas ($hora)
#
# Dependencias:
# - CRPS.2.0.bash
#
# Uso (exemplo):
# $ ./call_CRPS.2.0.bash 2008122000
#
# Historico:
# Firts crack: Christopher et al., XXXX
# 13/08/2015: - Incluida verificacao da quantidade de argumentos na linha de comando (cfbastarz)
#             - Simplificacao do script (loop sobre a variavel hora) (cfbastarz)
#
# Todo:
# 13/08/2015: - Documentacao
#             - Investigar a funcao dos ctls que sao criados no final do script
#             - Acrescentar um loop para os dominios e datas

if [ $# -ne "1" ]
then
  echo "USE: call_CRPS.2.0.bash YYYYMMDDHH"
  exit 1
fi

TARGETDATE=${1}

CURRENTDIR=`pwd`

for hora in `echo $(seq 24 24 360)`
do
  CURRENTDATE=`date '+%Y-%m-%d_%H:%M:%S'`
  ./CRPS.2.0.bash ${TARGETDATE} ${hora}h > ../output/CRPS.2.0.bash.${CURRENTDATE}.${hora}hForecastFor${TARGETDATE}.out 2>&1
done 

DOMAIN="TR"

if [ "${DOMAIN}" == "HN" ]
then
  LA1=74
  LA2=121
  LO1=1
  LO2=240;
elif [ "${DOMAIN}" == "HS" ]
then
  LA1=1
  LA2=48
  LO1=1
  LO2=240;
elif [ "${DOMAIN}" == "TR" ]
then
  LA1=48
  LA2=74
  LO1=1
  LO2=240;
else
  echo "DOMAIN WAS NOT SET PROPERLY"
  exit 1
fi

(( DY = LA2 - LA1 + 1 ))

cd ${CURRENTDIR}

cd ../dataout

cat <<EOFCTL4 > ../dataout/CRPS4CPTECEPS.ForecastFor${TARGETDATE}.ctl
DSET CRPS4CPTECEPS.%f2hForecastFor${TARGETDATE}.grads
UNDEF -9.99e+08
OPTIONS SEQUENTIAL TEMPLATE
XDEF 240  Linear    0.00 1.5 
YDEF $DY  Linear  -90.00 1.5 
ZDEF 1 LINEAR 1000 10
TDEF 16 LINEAR 12Z01DEC2008 24HR
VARS 1
CRPS 1 99 CRPS FOR FORECASTED ABSOLUTE TEMPERATURE AT 850 hPa [ K ] 
ENDVARS
EOFCTL4

cat <<EOFCTL5 > ../dataout/CRPSC4CPTECEPS.ForecastFor${TARGETDATE}.ctl
DSET CRPSC4CPTECEPS.%f2hForecastFor${TARGETDATE}.grads
UNDEF -9.99e+08
OPTIONS SEQUENTIAL TEMPLATE
XDEF 240  Linear    0.00 1.5 
YDEF $DY  Linear  -90.00 1.5 
ZDEF 1 LINEAR 1000 10
TDEF 16 LINEAR 12Z01DEC2008 24HR
VARS 1
CRPSC 1 99 CRPS FOR CLIMATOLOGICAL ABSOLUTE TEMPERATURE AT 850 hPa [ K ] 
ENDVARS
EOFCTL5

cat <<EOFCTL6 > ../dataout/CRPS4CPTECEPS.ForecastFor${TARGETDATE}.aave.ctl
DSET CRPS4CPTECEPS.%f2hForecastFor${TARGETDATE}.aave.grads
UNDEF -9.99e+08
OPTIONS SEQUENTIAL TEMPLATE
XDEF 1 LINEAR 1.0 1.0 
YDEF 1 LINEAR 1.0 1.0 
ZDEF 1 LINEAR 1000 10
TDEF 16 LINEAR 12Z01DEC2008 24HR
VARS 2
CRPSF 1 99 CRPS FOR FORECAST [no dim - the lower the better]
CRPSC 1 99 CRPS FOR CLIMATOLOGICAL DISTRIBUTION [no dim - the lower the better] 
ENDVARS
EOFCTL6
