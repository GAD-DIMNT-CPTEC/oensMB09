#!/bin/ksh -x

. ${FILEENV} ${CASE}

unlimit
limit
#################################################################
#
# CALCULO DE DATAS
#
YYYY=`echo $LABELI |cut -c 1-4`
MM=`echo $LABELI |cut -c 5-6`
DD=`echo $LABELI |cut -c 7-8`
HH=`echo $LABELI |cut -c 9-10`
DD_1=`date -d "1 day ago ${YYYY}${MM}${DD}" +"${PERCT}Y${PERCT}m${PERCT}d"`
#
YYYYF=`echo $LABELF |cut -c 1-4`
MMF=`echo $LABELF |cut -c 5-6`
DDF=`echo $LABELF |cut -c 7-8`
HHF=`echo $LABELF |cut -c 9-10`
#
TRC=${1}
LV=${2}
LABELI=${3}
LABELF=${4}

LABELS=`echo ${LABELI} | cut -c1-8`
HH=`echo ${LABELI} | cut -c9-10`

DATEF=`echo ${LABELF} | cut -c1-8`
HHF=`echo ${LABELF} | cut -c9-10`

jjj1=`date -d "${LABELS} ${HH}:00" +"%j"`
jjj2=`date -d "${DATEF} ${HHF}:00" +"%j"`
NFDAYS=`echo ${jjj2} ${jjj1} |awk '{{nday=$1-$2}if(nday < 0){nday = $1 + (365-$2)} if(nday >7){nday=7} {print nday}}'`

echo "INICIO:  ${LABELI}\nTERMINO: ${LABELF}"

GAUDFT=/usr/local/grads/udf/udft
GADDIR=/usr/local/grads/dat
GRADSB=/usr/local/grads/bin
GASCRP=/usr/local/grads/lib

cd ${HOME_suite}/mapas/scripts
LABELP=`date -d "$LABELS ${HH}:00 12 hours ago" +"%Y%m%d%H"` 

echo "LABELI=$LABELI\nLABELP=$LABELP"

ARQ=`ls -tr ${DK_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFXO}${LABELI}*.fct.${CASE}.ctl | head -1`
NF=`ls -tr ${DK_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFXO}${LABELI}*.fct.${CASE}.grb | wc -l`

echo "CTLBASE="$ARQ"\n"

set +e
#mkdir -p ${DK_suite}/pos/dataout/${CASE}/${LABELI}/tmp
#mv -f ${DK_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFXO}${LABELI}${LABELI}*.icn.${CASE}.grb ${DK_suite}/pos/dataout/${CASE}/${LABELI}/tmp
#mv -f ${DK_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFXO}${LABELP}${LABELP}*.fct.${CASE}.grb ${DK_suite}/pos/dataout/${CASE}/${LABELI}/tmp
set -e
set -x
#######################################################################
#
# GERA CTL TEMPLATE PARA PREVISOES - RODADA ATUAL
#
cp $ARQ gposfct${LABELI}.ctl

DSET=`cat gposfct${LABELI}.ctl | grep -i dset`
cat gposfct${LABELI}.ctl | sed -e s%"$DSET"%"dset ${DK_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFXO}${LABELI}DATAP.fct.${CASE}.grb"% >> gposfct${LABELI}.ctl.tmp
mv gposfct${LABELI}.ctl.tmp gposfct${LABELI}.ctl
cat gposfct${LABELI}.ctl | sed -e s/DATA/%y4%m2%d2%h2/ >> gposfct${LABELI}.ctl.tmp
mv gposfct${LABELI}.ctl.tmp gposfct${LABELI}.ctl

INDEX=`cat gposfct${LABELI}.ctl | grep -i index`
cat gposfct${LABELI}.ctl | sed -e s%"$INDEX"%"index gposfct${LABELI}.gmp"% >> gposfct${LABELI}.ctl.tmp
mv gposfct${LABELI}.ctl.tmp gposfct${LABELI}.ctl

OPTIONS=`cat gposfct${LABELI}.ctl | grep -i options`
cat gposfct${LABELI}.ctl | sed -e s%"$OPTIONS"%"options yrev template"% >> gposfct${LABELI}.ctl.tmp
mv gposfct${LABELI}.ctl.tmp gposfct${LABELI}.ctl

TITLE=`cat gposfct${LABELI}.ctl | grep -i title`
cat gposfct${LABELI}.ctl | sed -e s%"$TITLE"%"title PRESSURE HISTORY    CPTEC AGCM V2.0 97 ${CASE}:COLD"% >> gposfct${LABELI}.ctl.tmp
mv gposfct${LABELI}.ctl.tmp gposfct${LABELI}.ctl

DATE=`date -d "${LABELS} ${HH}:00" +"%HZ%d%h%Y"`
TDEF=`cat gposfct${LABELI}.ctl | grep -i tdef`
cat gposfct${LABELI}.ctl | sed -e s%"$TDEF"%"tdef $NF linear ${DATE} 06hr"% >> gposfct${LABELI}.ctl.tmp
mv gposfct${LABELI}.ctl.tmp gposfct${LABELI}.ctl

rm -f gposfct${LABELI}.gmp
set +e
/usr/local/grads/bin/gribmap -i gposfct${LABELI}.ctl
set -e

ARQ=`ls -tr ${DK_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFXO}${LABELI}*.icn.${CASE}.ctl | head -1`
NF=`ls -tr ${DK_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFXO}${LABELI}*.icn.${CASE}.grb | wc -l`

echo "CTLBASE="$ARQ"\n"

#######################################################################
#
# GERA CTL TEMPLATE PARA ANALISE - RODADA ATUAL
#
cp $ARQ gposicn${LABELI}.ctl

DSET=`cat gposicn${LABELI}.ctl | grep -i dset`
cat gposicn${LABELI}.ctl | sed -e s%"$DSET"%"dset ${DK_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFXO}${LABELI}DATAP.icn.${CASE}.grb"% >> gposicn${LABELI}.ctl.tmp
mv gposicn${LABELI}.ctl.tmp gposicn${LABELI}.ctl
cat gposicn${LABELI}.ctl | sed -e s/DATA/%y4%m2%d2%h2/ >> gposicn${LABELI}.ctl.tmp
mv gposicn${LABELI}.ctl.tmp gposicn${LABELI}.ctl

INDEX=`cat gposicn${LABELI}.ctl | grep -i index`
cat gposicn${LABELI}.ctl | sed -e s%"$INDEX"%"index gposicn${LABELI}.gmp"% >> gposicn${LABELI}.ctl.tmp
mv gposicn${LABELI}.ctl.tmp gposicn${LABELI}.ctl

OPTIONS=`cat gposicn${LABELI}.ctl | grep -i options`
cat gposicn${LABELI}.ctl | sed -e s%"$OPTIONS"%"options yrev template"% >> gposicn${LABELI}.ctl.tmp
mv gposicn${LABELI}.ctl.tmp gposicn${LABELI}.ctl

TITLE=`cat gposicn${LABELI}.ctl | grep -i title`
cat gposicn${LABELI}.ctl | sed -e s%"$TITLE"%"title PRESSURE HISTORY    CPTEC AGCM V2.0 97 ${CASE}:COLD"% >> gposicn${LABELI}.ctl.tmp
mv gposicn${LABELI}.ctl.tmp gposicn${LABELI}.ctl

DATE=`date -d "${LABELS} ${HH}:00" +"%HZ%d%h%Y"`
TDEF=`cat gposicn${LABELI}.ctl | grep -i tdef`
cat gposicn${LABELI}.ctl | sed -e s%"$TDEF"%"tdef $NF linear ${DATE} 06hr"% >> gposicn${LABELI}.ctl.tmp
mv gposicn${LABELI}.ctl.tmp gposicn${LABELI}.ctl

rm -f gposicn${LABELI}.gmp
set +e
/usr/local/grads/bin/gribmap -i gposicn${LABELI}.ctl
set -e


ARQ=`ls -tr ${DK_suite}/pos/dataout/${CASE}/${LABELP}/GPOS${PREFXO}${LABELP}*.fct.${CASE}.ctl | head -1`
NF=`ls -tr ${DK_suite}/pos/dataout/${CASE}/${LABELP}/GPOS${PREFXO}${LABELP}*.fct.${CASE}.grb | wc -l`

echo "CTLBASE="$ARQ"\n"

#######################################################################
#
# GERA CTL TEMPLATE PARA PREVISAO - RODADA 12 HORAS ANTES
#
set -x
set +e

if [ ! -z $ARQ ]; then
cp $ARQ gposfct${LABELP}.ctl

DSET=`cat gposfct${LABELP}.ctl | grep -i dset`
cat gposfct${LABELP}.ctl | sed -e s%"$DSET"%"dset ${DK_suite}/pos/dataout/${CASE}/${LABELP}/GPOS${PREFXO}${LABELP}DATAP.fct.${CASE}.grb"% >> gposfct${LABELP}.ctl.tmp
mv gposfct${LABELP}.ctl.tmp gposfct${LABELP}.ctl
cat gposfct${LABELP}.ctl | sed -e s/DATA/%y4%m2%d2%h2/ >> gposfct${LABELP}.ctl.tmp
mv gposfct${LABELP}.ctl.tmp gposfct${LABELP}.ctl

INDEX=`cat gposfct${LABELP}.ctl | grep -i index`
cat gposfct${LABELP}.ctl | sed -e s%"$INDEX"%"index gposfct${LABELP}.gmp"% >> gposfct${LABELP}.ctl.tmp
mv gposfct${LABELP}.ctl.tmp gposfct${LABELP}.ctl

OPTIONS=`cat gposfct${LABELP}.ctl | grep -i options`
cat gposfct${LABELP}.ctl | sed -e s%"$OPTIONS"%"options yrev template"% >> gposfct${LABELP}.ctl.tmp
mv gposfct${LABELP}.ctl.tmp gposfct${LABELP}.ctl

TITLE=`cat gposfct${LABELP}.ctl | grep -i title`
cat gposfct${LABELP}.ctl | sed -e s%"$TITLE"%"title PRESSURE HISTORY    CPTEC AGCM V2.0 97 ${CASE}:COLD"% >> gposfct${LABELP}.ctl.tmp
mv gposfct${LABELP}.ctl.tmp gposfct${LABELP}.ctl

HH=`echo $LABELP | cut -c 9-10`
LABELS=`echo $LABELP | cut -c 1-8`
DATE=`date -d "${LABELS} ${HH}:00" +"%HZ%d%h%Y"`
TDEF=`cat gposfct${LABELP}.ctl | grep -i tdef`
cat gposfct${LABELP}.ctl | sed -e s%"$TDEF"%"tdef $NF linear ${DATE} 06hr"% >> gposfct${LABELP}.ctl.tmp
mv gposfct${LABELP}.ctl.tmp gposfct${LABELP}.ctl
rm -f gposfct${LABELP}.gmp
set +e
/usr/local/grads/bin/gribmap -i gposfct${LABELP}.ctl
set -e
#else
#cp gposfct${LABELI}.ctl gposfct${LABELP}.ctl
fi

#######################################################################
#
# INICIA GERACAO DAS FIGURAS
#
rm -f *.png
dir=`pwd`
echo "DIRETORIO => $dir"
#
ulimit -a
# FIGURAS - POLO NORTE
DIRPOS=${DK_suite}/pos/dataout/${CASE}
echo ${LABELI} ${LABELP} 1 ${NFDAYS} ${DIRPOS}/${LABELI}/GPOS${PREFXO}${LABELI}.ctl ${DIRPOS}/${LABELP}/GPOS${PREFXO}${LABELP}.ctl ${DIRPOS}/${LABELI}/GPOS${PREFXO}${LABELI}.ctl 

/usr/local/grads-1.8sl11/bin/gradsnc -bp << EOT
run ${HOME_suite}/mapas/scripts/Mapa1.sx6.gs
${LABELI} ${LABELP} 1 ${NFDAYS} ${DIRPOS}/${LABELI}/GPOS${PREFXO}${LABELI}.ctl ${DIRPOS}/${LABELP}/PGOS${PREFXO}${LABELP}.ctl ${DIRPOS}/${LABELI}/GPOS${PREFXO}${LABELI}.ctl 
quit
EOT
mv *.png ${DK_suite}/mapas/${CASE}/${LABELI}
#
# FIGURAS - POLO SUL
/usr/local/grads/bin/gradsnc -bp << EOT
run ${HOME_suite}/mapas/scripts/Mapa1.sx6.gs
${LABELI} ${LABELP} 2 ${NFDAYS} ${DIRPOS}/${LABELI}/GPOS${PREFXO}${LABELI}.ctl ${DIRPOS}/${LABELP}/GPOS${PREFXO}${LABELP}.ctl ${DIRPOS}/${LABELI}/GPOS${PREFXO}${LABELI}.ctl 
quit
EOT
mv *.png ${DK_suite}/mapas/${CASE}/${LABELI}
#
# FIGURAS - TROPICAL
/usr/local/grads/bin/gradsnc -bl << EOT
run ${HOME_suite}/mapas/scripts/Mapa1.sx6.gs
${LABELI} ${LABELP} 3 ${NFDAYS} ${DIRPOS}/${LABELI}/GPOS${PREFXO}${LABELI}.ctl ${DIRPOS}/${LABELP}/GPOS${PREFXO}${LABELP}.ctl ${DIRPOS}/${LABELI}/GPOS${PREFXO}${LABELI}.ctl 
quit
EOT
mv *.png ${DK_suite}/mapas/${CASE}/${LABELI}
#
#  FIGURAS - AMERICA DO SUL - PARTE I
/usr/local/grads/bin/gradsnc -bp << EOT
run ${HOME_suite}/mapas/scripts/Mapa1.sx6.gs
${LABELI} ${LABELP} 4 ${NFDAYS} ${DIRPOS}/${LABELI}/GPOS${PREFXO}${LABELI}.ctl ${DIRPOS}/${LABELP}/GPOS${PREFXO}${LABELP}.ctl ${DIRPOS}/${LABELI}/GPOS${PREFXO}${LABELI}.ctl 
quit
EOT
mv *.png ${DK_suite}/mapas/${CASE}/${LABELI}
#
#   FIGURAS AMERICA DO SUL -  PARTE II
/usr/local/grads/bin/gradsnc -bp << EOT
run ${HOME_suite}/mapas/scripts/Mapa2.sx6.gs
${LABELI} 4 ${NFDAYS} ${DIRPOS}/${LABELI}/GPOS${PREFXO}${LABELI}.ctl ${DIRPOS}/${LABELI}/GPOS${PREFXO}${LABELI}.ctl
quit
EOT
mv *.png ${DK_suite}/mapas/${CASE}/${LABELI}

HOURF=`date +'%H:%M'`
echo "  HOUR = $HOURF"



exit 0
######################################################################################
#
# VERIFICACAO DOS GIFS.
#
# O NUMERO CORRETO DE GIFS (CORR) QUE SERAO COPIADOS PARA A PAGINA VEM DO CALCULO DO
# NUMERO DE GIFS/DIA DE PREVISAO (43) VEZES O NUMERO DE DIAS DE PREVISAO + 1
#

CORR=`echo "43*(${NFDAYS}+1)" | bc -l`
CONT=`find ${DK_suite}/mapas -name "*${LABELI}2*.png" -ls | awk 'BEGIN {c=0} {if ($7>3000) c+=1} END {printf( "%d", c)}'`

if [ ${CONT} -ge ${CORR} ]; then
      echo "NUMERO e TAMANHO DE ARQUIVOS GERADOS OK: ${CONT} ARQUIVOS" 
else
      echo "PROBLEMA COM A GERACAO DAS FIGURAS DA PAGINA: ${CONT} ARQUIVOS"
#      exit 1
fi

set +e
mv -f ${DK_suite}/pos/dataout/${CASE}/${LABELI}/tmp/GPOS${PREFXO}${LABELI}${LABELI}*.fct.${CASE}.grb ${DK_suite}/pos/dataout/${CASE}/${LABELI}/
mv -f ${DK_suite}/pos/dataout/${CASE}/${LABELI}/tmp/GPOS${PREFXO}${LABELP}${LABELP}*.fct.${CASE}.grb ${DK_suite}/pos/dataout/${CASE}/${LABELI}/
rmdir ${DK_suite}/pos/dataout/${CASE}/${LABELI}/tmp
set -e

exit 0
