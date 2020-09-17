#!/bin/bash

#
# Function: to run the process to calculate the CRPS 
#
# This version uses as input data (DATAIN) the forecast made with the EPS configured as following:
# cq_in=0.0
# bias correction based on the last 60 forecasts
# bias correction only for T850 (hence, this is the only variable available)
# period availabel: December 16, 2008 to February 28, 2009
#

# CFB 
# Algoritimo:
# 1) Extracao da variavel T850 (ou PSNM) da condicao inicial do membro controle (script fwr.ExtractICfromEPS.IEEEOutput.Regrid2_1.5x1.5.gs)
# - Sao escritos os seguintes arquivos:
# -- CPTEC.${VARCRPS}.2015030300.ctl
# -- CPTEC.${VARCRPS}.2015030300.grads
# -- epsfilesin.24h.tmp (contem uma lista com os arquivos a serem abertos)
# -- epsfilesin.24h.txt (contem uma lista de comando do GrADS para abrir os arquivos)
#
# 2) Recorte do subdominio (TR, HS ou HN) da variavel da condicao inicial (script fwr.ExtractICfromEPS.IEEEOutput.Regrid2_1.5x1.5.gs)
# - Sao escritos os seguintes arquivos:
# -- SDAnalise.ctl 
# -- SDAnalise.grads (representa a referencia)
# -- wght.grads
# Obs.: Na realidade, este script recupera as "observacoes" que serao usadas no calculo da CDF utilizada pelo CRPS
#
# 3) Calculo do CRPS (e do CRPSS) utilizando da variavel de interesse:
# - Sao escritos os seguintes arquivos:
# -- CRPSC4CPTECEPS.48hForecastFor2015030300.grads
# -- CRPS4CPTECEPS.48hForecastFor2015030300.grads
# -- CRPS4CPTECEPS.48hForecastFor2015030300.aave.grads
# -- CRPS4CPTECEPS.48hForecastFor2015030300.aave.ctl
# -- CRPS4CPTECEPS.48hForecastFor2015030300.txt
# Obs.: No final deste processo, e necessario criar um template para abrir os arquivos
# de CRPS dos lags (estes templates nao sao criados automaticamente)
#
# Observacoes Importantes:
# - CRPSS = Continous Rank Proability Skill Score;
# -- CRPSS = 1 - CRPSS_prev/CRPSS_ref (Cunningham et al., 2014)
# - A principio, os arquivos com os membros a serem abertos foram anteriormente recortados para conter apenas as variaveis de interesse: psnm, z500 e t850.
# - O CPRS e calculado para o ensemble de cada data considerada, e.g.:
# -- para o ensemble da data 2015030300 (considerando uma previsao de 24h), temos o seguinte: {EPS 2015030300: 01N 01P 02N 02P 03N 03P ... 07N 07P NMC}, onde:
# N: indica a perturbacao subtraida, P perturbacao somada e NMC o membro controle;
# -- para este ensemble, e atribuido apenas um valor do CRPS referente a previsao de 24h
# -- a serie que e mostrada no grafico da curva do CRPSS e formada portanto, pelo CRPS das previsoes de 24 a 360h partindo-se da previsao inicial
#
# Todo:
# - Refazer este script;
# - Criar os templates para abrir os arquivos com os CRPS dos lags;
#
# Historico:
# XX/XX/XXXX - First crack (Christopher et al.)
# 12/08/2015 - pequenas adequacoes para que o script funcione com os dados no scratchin e online e documentacao inicial
# 13/08/2015 - modificacoes em algumas partes do script
# 14/08/2015 - modificada a forma como sao atribuidos os nomes dos meses e mais documentacao
# 06/10/2015 - correcoes nas informacoes contidas no cabecalho do script
# 05/04/2016 - mellhorada a documentacao, limpeza e mais melhorias
# 10/09/2020 - mais algumas melhorias e simplificações (uso no XC50)
#
# Dependencias:
# - caldate ou inctime
# CFB

DIRGRADS=/cray_home/carlos_bastarz/bin/tools/opengrads-2.2.1.oga.1/Contents

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
#DATAINDIR=/stornext/online1/ensemble_g/oens_new/oensMB09
DATAINDIR=/lustre_xc50/carlos_bastarz/oensMB09_test_preXC50/pos/dataout/TQ0126L028

# Diretorio com os dados recortados para a veriavel do experimento de interesse
# Para mais informacoes sobre o recorte dos dados veja o script recorta_dados/recorta_dados.ksh
#DATAINDIR2=/stornext/online1/ensemble_g/novos_dados_recortados/psnm-oens_MB09
DATAINDIR2=/lustre_xc50/carlos_bastarz/oensMB09_test_preXC50/pos/dataout/rec/psnm-oens_MB09

# Define o diretorio onde estara a climatologia (ERA Interim, 1.5 graus)
# Atencao para o nome da variavel de interesse
#dirclm="/stornext/online1/ensemble_g/ERAinterim1.5/psml/",
dirclm="/lustre_xc50/carlos_bastarz/ERAinterim1.5/psml/",

# Manipulacao das datas
yyyytgt=$(echo $targetdate | cut -c 1-4)
mmtgt=$(echo $targetdate | cut -c 5-6)
ddtgt=$(echo $targetdate | cut -c 7-8)
hhtgt=$(echo $targetdate | cut -c 9-10)

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

# GET THE ANALYSIS FILE FROM CPTEC CONTROL RUN
if [ "${OBSTYPE}" == "CPT" ]
then
  if [ ! -s ../datain/CPTEC.${VARCRPS}.${targetdate}.grads ]
  then
    if [ ! -s ${DATAINDIR}/${yyyytgt}${mmtgt}${ddtgt}${hhtgt}/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.idx ]
    then
      gribmap -i ${DATAINDIR}/${yyyytgt}${mmtgt}/${ddtgt}${hhtgt}/NMC/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.ctl
    fi
    ${DIRGRADS}/grads -blc "run fwr.ExtractICfromEPS.IEEEOutput.Regrid2_1.5x1.5_variable.gs ${targetdate} ${DATAINDIR}/${yyyytgt}${mmtgt}/${ddtgt}${hhtgt}/NMC/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.ctl ${VARCRPS} ${VARLEV}"
  fi
else
  echo "Analysis file ( ../datain/CPTEC.${VARCRPS}.${targetdate}.grads ) was already withdrawn"
fi

rm -f ../datain/epsfilesin.${fctlag}.tmp; touch ../datain/epsfilesin.${fctlag}.tmp;

# Incremento da data (depende do script caldate)
icdate=$(${HOME}/scripts/caldate.3.1.2 ${targetdate} - ${fctlag} "yyyymmddhh")

# Loop sobre os membros do ensemble (incluindo o controle)
# Aqui sera formada a lista com os nomes dos arquivos a serem abertos pelo GrADS
for memb in 01N 01P 02N 02P 03N 03P 04N 04P 05N 05P 06N 06P 07N 07P NMC
do
  yyyyic=$(echo $icdate | cut -c 1-4)
  mmic=$(echo   $icdate | cut -c 5-6)
  ddic=$(echo   $icdate | cut -c 7-8)
  hhic=$(echo   $icdate | cut -c 9-10)
  ls ${DATAINDIR2}/${yyyyic}${mmic}${ddic}${hhic}/GBRM${memb}${yyyyic}${mmic}${ddic}${hhic}${targetdate}.ctl >> ../datain/epsfilesin.${fctlag}.tmp
done

# Apara o arquivo ../datain/epsfilesin.${fctlag}.txt (vai criar um novo)
rm -f ../datain/epsfilesin.${fctlag}.txt

# Acrescenta o comando open na frente dos nomes dos arquivos na lista criada
awk '{print "open " $1}' ../datain/epsfilesin.${fctlag}.tmp > ../datain/epsfilesin.${fctlag}.txt

# Define a variavel vargrads (vargrads = temp850 ou vargrads = psnm)
if [ ${VARCRPS} = "temp" ]
then
  vargrads=${VARCRPS:0:1}${VARLEV}
else
  vargrads=${VARCRPS}
fi

# Executa o GrADS lendo os arquivos da lida gerada e escreve os arquivos 
# ../datain/CPTECEPS.'fctlag'ForecastFor'datei'.15Members.grads'
# Estes arquivos contem os campos de interesse de todos os membros interpolados na grade da climatologia (verificar)
${DIRGRADS}/grads -blc "run fwr.ExtractVariablefromEPS.BRM.IEEEOutput.Regrid2_1.5x1.5_variable.gs ${targetdate} ${fctlag} ${vargrads}"

# Cria o descritor para o arquivo gerado (atencao ao nome da variavel e nivel)
cat <<EOFCTL2 > ../datain/CPTECEPS.${fctlag}ForecastFor${targetdate}.15Members.ctl
DSET ^CPTECEPS.${fctlag}ForecastFor${targetdate}.15Members.grads
UNDEF -9.99e+08
XDEF 240 LINEAR 0.0 1.5
YDEF 121 LINEAR -90.0 1.5
ZDEF 1 LEVELS 1000
TDEF 1 LINEAR ${hhtgt}Z${ddtgt}${MMM}${yyyytgt} ${fctlag}r
VARS 15
M01N 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
M01P 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
M02N 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
M02P 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
M03N 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
M03P 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
M04N 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
M04P 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
M05N 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
M05P 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
M06N 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
M06P 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
M07N 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
M07P 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
MAVN 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]
ENDVARS
EOFCTL2

REFFCT="EQPROBBIN"
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

#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#
# Writes the NAMELIST for the CPT option
#
if [ "${OBSTYPE}" == "CPT" ]
then
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
fi
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

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
