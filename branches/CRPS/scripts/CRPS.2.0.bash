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

# Algoritimo:
# -----------
# 1) Extracao da variavel T850 da condicao inicial do controle (script fwr.ExtractICfromEPS.IEEEOutput.Regrid2_1.5x1.5.gs)
# - Sao escritos os seguintes arquivos:
# -- CPTEC.TEMP850.2015030300.ctl
# -- CPTEC.TEMP850.2015030300.grads
# -- epsfilesin.24h.tmp
# -- epsfilesin.24h.txt
#
# 2) Recorte do subdominio (TR, HS ou HN) da variavel T850 da condicao inicial (script fwr.ExtractICfromEPS.IEEEOutput.Regrid2_1.5x1.5.gs)
# - Sao escritos os seguintes arquivos:
# -- SDAnalise.ctl
# -- SDAnalise.grads
# -- wght.grads
#
# 3) Calculo do CRPS (e do CRPSS) utilizando o campo de T850 (isto por enquanto esta fixo no script):
# - Sao escritos os seguintes arquivos:
# -- CRPSC4CPTECEPS.48hForecastFor2015030300.grads
# -- CRPS4CPTECEPS.48hForecastFor2015030300.grads
# -- CRPS4CPTECEPS.48hForecastFor2015030300.aave.grads
# -- CRPS4CPTECEPS.48hForecastFor2015030300.aave.ctl
# -- CRPS4CPTECEPS.48hForecastFor2015030300.txt

# Observacoes:
# ------------
# - CRPSS = Continous Rank Proability Skill Score;
# -- CRPSS = 1 - CRPSS_prev/CRPSS_ref (Cunningham et al., 2014)
# - A principio, os arquivos com os membros a serem abertos foram anteriormente recortados para conter apenas as variaveis de interesse: psnm, z500 e t850.
# - O CPRP e calculado para o ensemble de cada data considerada, e.g.:
# -- para o ensemble da data 2015030300 (considerando uma previsao de 24h), temos o seguinte: {EPS 2015030300: 01N 01P 02N 02P 03N 03P ... 07N 07P NMC}, onde:
#    N: indica a perturbacao subtraida, P perturbacao somada e NMC o membro controle
# -- para este ensemble, e atribuido apenas um valor do CRPS referente a previsao de 24h
# -- a serie que e mostrada no grafico da curva do CRPS e formada portanto, pelo CRPS das previsoes de 24 a 360h partindo-se da previsao inicial

# Todo:
# -----
# - Melhorar algumas partes do script
# - Entender o conteudo de cada um dos arquivos gerados
# - Entender a funcao e o uso do diretorio no namelist /Users/christopher/EPS/CRPS/datain/MonthlyClimatology/

# Historico:
# ----------
# XX/XX/XXXX - First crack (Christopher et al.)
# 12/08/2015 - pequenas adequacoes para que o script funcione com os dados no scratchin e online e documentacao inicial
# 13/08/2015 - pequenas melhorias em algumas partes do script
# 14/08/2015 - modificada a forma como sao atribuidos os nomes dos meses e mais documentacao
# 06/10/2015 - correcoes nas informacoes contidas no cabecalho do script

#export GASCRP=/stornext/home/chris.castro/GrADS/lib
export GASCRP=${PWD}/grads_libs

case "${#1}" in
10)
set -x
targetdate=${1}
fctlag=${2}

OBSTYPE="CPT"
#DATAINDIR=/stornext/online1/ensemble_g/TIGGE/semvies
#DATAINDIR=/stornext/online1/ensemble_g/oens_new/oensMB09
DATAINDIR=/stornext/online17/pnt/preoper/tempo/oensMB09
DATAINDIR2=/scratchout/grupos/assim_dados/home/carlos.bastarz/ensemble_g/oens_new/CRPS1.0/dados_recortados
DATAOUTDIR=/scratchin/grupos/assim_dados/home/carlos.bastarz/ensemble_g/oens_new/CRPS1.0/output

yyyytgt=`echo $targetdate | cut -c 1-4`
mmtgt=`echo $targetdate | cut -c 5-6`
ddtgt=`echo $targetdate | cut -c 7-8`
hhtgt=`echo $targetdate | cut -c 9-10`

if [ ${mmtgt} -eq "01" ]; then MMM="JAN"; fi
if [ ${mmtgt} -eq "02" ]; then MMM="FEB"; fi
if [ ${mmtgt} -eq "03" ]; then MMM="MAR"; fi
if [ ${mmtgt} -eq "04" ]; then MMM="APR"; fi
if [ ${mmtgt} -eq "05" ]; then MMM="MAY"; fi
if [ ${mmtgt} -eq "06" ]; then MMM="JUN"; fi
if [ ${mmtgt} -eq "07" ]; then MMM="JUL"; fi
if [ ${mmtgt} -eq "08" ]; then MMM="AGO"; fi
if [ ${mmtgt} -eq "09" ]; then MMM="SEP"; fi
if [ ${mmtgt} -eq "10" ]; then MMM="OCT"; fi
if [ ${mmtgt} -eq "11" ]; then MMM="NOV"; fi
if [ ${mmtgt} -eq "12" ]; then MMM="DEC"; fi

#
# GET THE ANALYSIS FILE FROM CPTEC CONTROL RUN
#
if [ "${OBSTYPE}" == "CPT" ]
then
  if [ ! -s ../datain/CPTEC.TEMP850.${targetdate}.grads ]
  then
#   if [ ! -s ${DATAINDIR}/../comvies/${yyyytgt}${mmtgt}${ddtgt}${hhtgt}/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.idx ]; then
    if [ ! -s ${DATAINDIR}/${yyyytgt}${mmtgt}${ddtgt}${hhtgt}/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.idx ]
#    if [ ! -s ${DATAINDIR}/${yyyytgt}${mmtgt}/${ddtgt}${hhtgt}/NMC/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.idx ]
    then
#      gribmap -i ${DATAINDIR}/../comvies/${yyyytgt}${mmtgt}${ddtgt}${hhtgt}/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.ctl
#      gribmap -i ${DATAINDIR}/${yyyytgt}${mmtgt}${ddtgt}${hhtgt}/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.ctl
#      gribmap -i /scratchout/grupos/assim_dados/home/carlos.bastarz/tmp/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.ctl
      gribmap -i ${DATAINDIR}/${yyyytgt}${mmtgt}/${ddtgt}${hhtgt}/NMC/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.ctl
    fi
#   grads -blc "run fwr.ExtractICfromEPS.IEEEOutput.Regrid2_1.5x1.5.gs ${targetdate} \
#   ${DATAINDIR}/../comvies/${yyyytgt}${mmtgt}${ddtgt}${hhtgt}/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.ctl"
#    grads -blc "run fwr.ExtractICfromEPS.IEEEOutput.Regrid2_1.5x1.5.gs ${targetdate} ${DATAINDIR}/${yyyytgt}${mmtgt}${ddtgt}${hhtgt}/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.ctl"
#    grads -blc "run fwr.ExtractICfromEPS.IEEEOutput.Regrid2_1.5x1.5.gs ${targetdate} /scratchout/grupos/assim_dados/home/carlos.bastarz/tmp/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.ctl"
    grads -blc "run fwr.ExtractICfromEPS.IEEEOutput.Regrid2_1.5x1.5.gs ${targetdate} ${DATAINDIR}/${yyyytgt}${mmtgt}/${ddtgt}${hhtgt}/NMC/GPOSNMC${targetdate}${targetdate}P.icn.TQ0126L028.ctl"
  fi
else
  echo "Analysis file ( ../datain/CPTEC.TEMP850.${targetdate}.grads ) was already withdrawn"
fi

rm -f ../datain/epsfilesin.${fctlag}.tmp; touch ../datain/epsfilesin.${fctlag}.tmp;

icdate=`${HOME}/scripts/caldate.3.1.2 ${targetdate} - ${fctlag} "yyyymmddhh"`

for memb in 01N 01P 02N 02P 03N 03P 04N 04P 05N 05P 06N 06P 07N 07P NMC
do
  yyyyic=`echo $icdate | cut -c 1-4`
  mmic=`echo   $icdate | cut -c 5-6`
  ddic=`echo   $icdate | cut -c 7-8`
  hhic=`echo   $icdate | cut -c 9-10`
#  ls ${DATAINDIR}/${yyyyic}${mmic}${ddic}${hhic}/GBRM${memb}${yyyyic}${mmic}${ddic}${hhic}${targetdate}.ctl >> ../datain/epsfilesin.${fctlag}.tmp
  ls ${DATAINDIR2}/${yyyyic}${mmic}${ddic}${hhic}/GBRM${memb}${yyyyic}${mmic}${ddic}${hhic}${targetdate}.ctl >> ../datain/epsfilesin.${fctlag}.tmp
#  ls ${DATAINDIR}/${yyyyic}${mmic}/${ddic}${hhic}/${memb}/GPOS${memb}${yyyyic}${mmic}${ddic}${hhic}${targetdate}.ctl >> ../datain/epsfilesin.${fctlag}.tmp
done

rm -f ../datain/epsfilesin.${fctlag}.txt

awk '{print "open " $1}' ../datain/epsfilesin.${fctlag}.tmp > ../datain/epsfilesin.${fctlag}.txt

grads -blc "run fwr.ExtractVariablefromEPS.BRM.IEEEOutput.Regrid2_1.5x1.5.gs ${targetdate} ${fctlag}"

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

DOMAIN="HN"

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
dirclm="/Users/christopher/EPS/CRPS/datain/MonthlyClimatology/ ",
FCTLAG="${fctlag}",
ObsType="${OBSTYPE}",
RefFct="${REFFCT}",
LAT1=${LA1},
LAT2=${LA2},
LON1=${LO1},
LON2=${LO2},
/
&FILES
ObsFile="../datain/CPTEC.TEMP850.${targetdate}.grads ",
EPSFctFiles="../datain/CPTECEPS.${fctlag}ForecastFor${targetdate}.15Members.grads ",
ClimateRecordFile="/scratchin/grupos/ensemble_g/home/chris.castro/EPS/CRPS1.0/datain/ERA40/ERA40.DailyTimeSeries.1979-2001.${mmtgt}${ddtgt}${hhtgt}.grads ",
/
EOFNML2
fi
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

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
