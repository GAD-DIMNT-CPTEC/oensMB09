#!/bin/bash

case "${#1}" in
10)
set -x
targetdate=${1}
fctlag=${2}
VARCRPS=${3}
VARLEV=${4}

# Prefixo dos arquivos de CRPS a serem criados
OBSTYPE="CPT"
# Diretorio com os dados de previsao do experimento de interesse
DATAINDIR=/stornext/online1/ensemble_g/oens_new/oensMB09

# Diretorio com os dados recortados para a veriavel do experimento de interesse
# Para mais informacoes sobre o recorte dos dados veja o script recorta_dados/recorta_dados.ksh
DATAINDIR2=/stornext/online1/ensemble_g/novos_dados_recortados/t850-oens_MB09

# Define o diretorio onde estara a climatologia (ERA Interim, 1.5 graus)
# Atencao para o nome da variavel de interesse
dirclm="/stornext/online1/ensemble_g/ERAinterim1.5/t850/"

# Manipulacao das datas
yyyytgt=`echo $targetdate | cut -c 1-4`
mmtgt=`echo $targetdate | cut -c 5-6`
ddtgt=`echo $targetdate | cut -c 7-8`
hhtgt=`echo $targetdate | cut -c 9-10`

# Define do nome do mes de acordo com a data inicial
if [ ${mmtgt} -eq "01" ]; then MMM="JAN"; fi
if [ ${mmtgt} -eq "02" ]; then MMM="FEB"; fi
if [ ${mmtgt} -eq "03" ]; then MMM="MAR"; fi
if [ ${mmtgt} -eq "04" ]; then MMM="APR"; fi
if [ ${mmtgt} -eq "05" ]; then MMM="MAY"; fi
if [ ${mmtgt} -eq "06" ]; then MMM="JUN"; fi
if [ ${mmtgt} -eq "07" ]; then MMM="JUL"; fi
if [ ${mmtgt} -eq "08" ]; then MMM="AUG"; fi
if [ ${mmtgt} -eq "09" ]; then MMM="SEP"; fi
if [ ${mmtgt} -eq "10" ]; then MMM="OCT"; fi
if [ ${mmtgt} -eq "11" ]; then MMM="NOV"; fi
if [ ${mmtgt} -eq "12" ]; then MMM="DEC"; fi

#REFFCT="EQPROBBIN"
REFFCT="CLIMRECOR"

# Define o dominio de interesse
DOMAIN="HN"

# Com base no dominio de interesse, define os limites da regiao
if [ "${DOMAIN}" == "HN" ]
then
  LA1=74
  LA2=121
  LO1=1
  LO2=240
elif [ "${DOMAIN}" == "HS" ]
then
  LA1=1
  LA2=48
  LO1=1
  LO2=240
elif [ "${DOMAIN}" == "TR" ]
then
  LA1=48
  LA2=74
  LO1=1
  LO2=240
else
  echo "DOMAIN WAS NOT SET PROPERLY"
  exit 1
fi

# Apaga o namelist do CRPS (vai criar um novo)
rm -f ../datain/CRPS.nml

cat <<EOFNML2 > ../datain/CRPS.nml
&PARAM
ANLDATE="${targetdate}",
Month="${MMM}",
dirclm="${dirclm} ",
FCTLAG="${fctlag}",
ObsType="${OBSTYPE}",
RefFct="${REFFCT}",
LAT1=${LA1},
LAT2=${LA2},
LON1=${LO1},
LON2=${LO2},
/
&FILES
ObsFile="../datain/CPTEC.${VARCRPS}.${targetdate}.grads ",
EPSFctFiles="../datain/CPTECEPS.${fctlag}ForecastFor${targetdate}.15Members.grads ",
ClimateRecordFile="${dirclm}/ERA40.DailyTimeSeries.1979-2001.${mmtgt}${ddtgt}${hhtgt}.grads ",
/
EOFNML2

# Executa o CRPS
cd ../exec; ./CRPS.exe < ../datain/CRPS.nml
;;
*)
set +x
echo "+---------------------------------------------------------------------------------------------------+"
echo "|Invalid Option!!                                                                                  |"
echo "|To execute : "${0}" targetdate fctlag                                                                |" 
echo "|where                                                                                              |"
echo "|targetdate is the year, month, day and hour which the forecast points to                           |"
echo "|fctlag is the forecast lag (24h,48h,72h, etc)                                                      |"
echo "+---------------------------------------------------------------------------------------------------+"
;;
esac
