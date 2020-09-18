#! /bin/bash -x

#BOP
# Uso: ./run_crps.sh TQ0126L028 NMC 2020051500 temp 850 HN
#      ./run_crps.sh TQ0126L028 NMC 2020051500 zgeo 500 HN
#      ./run_crps.sh TQ0126L028 NMC 2020051500 psnm HN
#EOP

# Descomentar para debugar
#set -o xtrace

# Menu de opções/ajuda
if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#BOP/,/^#EOP/p'
  exit 0
fi

if [ -z ${1} ]
then
  echo "Argument is not set: RESOL"
  exit 1
else
  export resol=${1}
fi

if [ -z ${2} ]
then
  echo "Argument is not set: PREFIX"
  exit 1
else
  export prefix=${2}
fi

if [ -z ${3} ]
then
  echo "Argument is not set: LABELI"
  exit 1
else
  export LABELI=${3}
fi

if [ -z ${4} ]
then
  echo "Argument is not set: VARCRPS"
  exit 1
else
  export VARCRPS=${4}
fi

if [ -z ${5} ]
then
  echo "Argument is not set: VARLEV"
  if [ "${VARCRPS}" != 'psnm' ]
  then
    exit 1
  fi
else
  if [ ${VARCRPS} != 'psnm' ]
  then
    export VARLEV=${5}
  fi
fi

if [ -z ${6} ]
then
  echo "Argument is not set: DOMAIN"
  exit 1
else
  export DOMAIN=${6}
fi

export WORKDIR=/lustre_xc50/carlos_bastarz
export DIRCRPS=${WORKDIR}/CRPS
export DIRGRADS=${HOME}/bin/tools/opengrads-2.2.1.oga.1/Contents
export caldate=${HOME}/bin/tools/caldate.3.1.2

# Define a variavel vargrads (vargrads = t850, z500 ou psnm)
if [ ${VARCRPS} = "temp" ]
then
  vargrads=${VARCRPS:0:1}${VARLEV}
  descvar="ABSOLUTE TEMPERATURE AT 850 hPa [ K ]"
elif [ ${VARCRPS} = "zgeo" ]
then
  vargrads=${VARCRPS:0:1}${VARLEV}
  descvar="GEOPOTENTIAL HEIGHT AT 500 hPA [ m ]"
elif [ ${VARCRPS} = "psnm" ]
then
  vargrads=${VARCRPS}
  descvar="MEAN SEA LEVEL PRESSURE [ hPa ]"
else
  vargrads=${VARCRPS}
  descvar=""
fi

# Prefixo dos arquivos de CRPS a serem criados
export OBSTYPE="CPT"

# Nome do experimento
export EXPNAME=oensMB09_test_preXC50

# Diretorio com os dados de previsao do experimento de interesse
export DATAINDIR=${WORKDIR}/${EXPNAME}/pos/dataout/${resol}

# Diretorio com os dados recortados para a veriavel do experimento de interesse
# Para mais informacoes sobre o recorte dos dados veja o script recorta_dados/recorta_dados.ksh
export DATAINDIR2=${WORKDIR}/${EXPNAME}/pos/dataout/rec/${vargrads}

# Define o diretorio onde estara a climatologia (ERA Interim, 1.5 graus)
# Atencao para o nome da variavel de interesse
export dirclm=${WORKDIR}/ERAinterim1.5/${vargrads}

# Apaga o namelist do CRPS (vai criar um novo)
rm -f ${DIRCRPS}/datain/CRPS.nml

cd ${DIRCRPS}/scripts

export MAQUI=$(uname -s)

export SCRIPTSFILE=setcrps.${prefix}.${resol}.${LABELI}.${MAQUI}

cat <<EOT0 > ${DIRCRPS}/scripts/${SCRIPTSFILE}
#!/bin/bash -x
#PBS -o ${DIRCRPS}/scripts/${SCRIPTSFILE}.out
#PBS -e ${DIRCRPS}/scripts/${SCRIPTSFILE}.err
#PBS -l walltime=0:10:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N CRPS
#PBS -q pesq
#PBS -J 1-15

export GASCRP=${DIRCRPS}/scripts/grads_libs

export targetdate=${LABELI}
export fctlag=\$((\${PBS_ARRAY_INDEX}*24)) # 24, 48, 72, ..., 360h

# Manipulacao das datas
export yyyytgt=\$(echo \${targetdate} | cut -c 1-4)
export mmtgt=\$(echo \${targetdate} | cut -c 5-6)
export ddtgt=\$(echo \${targetdate} | cut -c 7-8)
export hhtgt=\$(echo \${targetdate} | cut -c 9-10)

# Define do nome do mes de acordo com a data inicial
if [ \${mmtgt} -eq "01" ]; then export MMM="JAN"; fi
if [ \${mmtgt} -eq "02" ]; then export MMM="FEB"; fi
if [ \${mmtgt} -eq "03" ]; then export MMM="MAR"; fi
if [ \${mmtgt} -eq "04" ]; then export MMM="APR"; fi
if [ \${mmtgt} -eq "05" ]; then export MMM="MAY"; fi
if [ \${mmtgt} -eq "06" ]; then export MMM="JUN"; fi
if [ \${mmtgt} -eq "07" ]; then export MMM="JUL"; fi
if [ \${mmtgt} -eq "08" ]; then export MMM="AUG"; fi
if [ \${mmtgt} -eq "09" ]; then export MMM="SEP"; fi
if [ \${mmtgt} -eq "10" ]; then export MMM="OCT"; fi
if [ \${mmtgt} -eq "11" ]; then export MMM="NOV"; fi
if [ \${mmtgt} -eq "12" ]; then export MMM="DEC"; fi

cd ${DIRCRPS}/scripts

# GET THE ANALYSIS FILE FROM CPTEC CONTROL RUN
if [ "${OBSTYPE}" == "CPT" ]
then
  if [ ! -s ${DIRCRPS}/datain/CPTEC.${VARCRPS}.\${targetdate}.grads ]
  then
    if [ ! -s ${DATAINDIR}/\${yyyytgt}\${mmtgt}\${ddtgt}\${hhtgt}/${prefix}/GPOS${prefix}\${targetdate}\${targetdate}P.icn.${resol}.idx ]
    then
      ${DIRGRADS}/gribmap -i ${DATAINDIR}/\${yyyytgt}\${mmtgt}\${ddtgt}\${hhtgt}/${prefix}/GPOS${prefix}\${targetdate}\${targetdate}P.icn.${resol}.ctl
    fi
    ${DIRGRADS}/grads -blc "run fwr.ExtractICfromEPS.IEEEOutput.Regrid2_1.5x1.5_variable.gs \${targetdate} ${DATAINDIR}/\${yyyytgt}\${mmtgt}\${ddtgt}\${hhtgt}/${prefix}/GPOS${prefix}\${targetdate}\${targetdate}P.icn.${resol}.ctl ${VARCRPS} ${VARLEV}"
  fi
else
  echo "Analysis file ( ${DIRCRPS}/datain/CPTEC.${VARCRPS}.\${targetdate}.grads ) was already withdrawn"
fi

# Incremento da data (depende do script caldate)
export icdate=\$(${caldate} \${targetdate} - \${fctlag}h "yyyymmddhh")

rm -f ${DIRCRPS}/datain/epsfilesin.\${fctlag}.tmp

# Loop sobre os membros do ensemble (incluindo o controle)
# Aqui sera formada a lista com os nomes dos arquivos a serem abertos pelo GrADS
for memb in 01N 01P 02N 02P 03N 03P 04N 04P 05N 05P 06N 06P 07N 07P NMC
do
  export yyyyic=\$(echo \${icdate} | cut -c 1-4)
  export mmic=\$(echo \${icdate} | cut -c 5-6)
  export ddic=\$(echo \${icdate} | cut -c 7-8)
  export hhic=\$(echo \${icdate} | cut -c 9-10)

  ls ${DATAINDIR2}/\${yyyyic}\${mmic}\${ddic}\${hhic}/GBRM\${memb}\${yyyyic}\${mmic}\${ddic}\${hhic}\${targetdate}.ctl >> ${DIRCRPS}/datain/epsfilesin.\${fctlag}.tmp
done

# Apaga o arquivo ../datain/epsfilesin.${fctlag}.txt (vai criar um novo)
rm -f ${DIRCRPS}/datain/epsfilesin.\${fctlag}.txt

# Acrescenta o comando open na frente dos nomes dos arquivos na lista criada
awk '{print "open " \$1}' ${DIRCRPS}/datain/epsfilesin.\${fctlag}.tmp > ${DIRCRPS}/datain/epsfilesin.\${fctlag}.txt

# Executa o GrADS lendo os arquivos da lista gerada e escreve os arquivos 
# ../datain/CPTECEPS.'fctlag'ForecastFor'datei'.15Members.grads'
# Estes arquivos contém os campos de interesse de todos os membros interpolados na grade da climatologia (verificar)
${DIRGRADS}/grads -blc "run fwr.ExtractVariablefromEPS.BRM.IEEEOutput.Regrid2_1.5x1.5_variable.gs \${targetdate} \${fctlag} ${vargrads}"

# Cria o descritor para o arquivo gerado (atenção ao nome da variável e nível)
cat <<EOFCTL2 > ${DIRCRPS}/datain/CPTECEPS.\${fctlag}ForecastFor\${targetdate}.15Members.ctl
DSET ^CPTECEPS.\${fctlag}ForecastFor\${targetdate}.15Members.grads
UNDEF -9.99e+08
XDEF 240 LINEAR 0.0 1.5
YDEF 121 LINEAR -90.0 1.5
ZDEF 1 LEVELS 1000
TDEF 1 LINEAR \${hhtgt}Z\${ddtgt}\${MMM}\${yyyytgt} \${fctlag}hr
VARS 15
M01N 1 99 ${descvar} 
M01P 1 99 ${descvar} 
M02N 1 99 ${descvar} 
M02P 1 99 ${descvar} 
M03N 1 99 ${descvar} 
M03P 1 99 ${descvar} 
M04N 1 99 ${descvar} 
M04P 1 99 ${descvar} 
M05N 1 99 ${descvar} 
M05P 1 99 ${descvar} 
M06N 1 99 ${descvar} 
M06P 1 99 ${descvar} 
M07N 1 99 ${descvar} 
M07P 1 99 ${descvar} 
MAVN 1 99 ${descvar} 
ENDVARS
EOFCTL2

#export REFFCT="EQPROBBIN"
export REFFCT="CLIMRECOR"

# Com base no dominio de interesse, define os limites da regiao
if [ "${DOMAIN}" == "HN" ]
then
  export LA1=74
  export LA2=121
  export LO1=1
  export LO2=240
elif [ "${DOMAIN}" == "HS" ]
then
  export LA1=1
  export LA2=48
  export LO1=1
  export LO2=240
elif [ "${DOMAIN}" == "TR" ]
then
  export LA1=48
  export LA2=74
  export LO1=1
  export LO2=240
else
  echo "DOMAIN WAS NOT SET PROPERLY"
  exit 1
fi

#
# Writes the NAMELIST for the CPT option
#
if [ "${OBSTYPE}" == "CPT" ]
then
cat <<EOFNML2 > ${DIRCRPS}/datain/CRPS.nml
&PARAM
ANLDATE="\${targetdate}",
Month="\${MMM}",
dirclm="${dirclm} ",
FCTLAG="\${fctlag}",
ObsType="${OBSTYPE}",
RefFct="\${REFFCT}",
LAT1=\${LA1},
LAT2=\${LA2},
LON1=\${LO1},
LON2=\${LO2},
/
&FILES
ObsFile="${DIRCRPS}/datain/CPTEC.${VARCRPS}.\${targetdate}.grads ",
EPSFctFiles="${DIRCRPS}/datain/CPTECEPS.\${fctlag}ForecastFor\${targetdate}.15Members.grads ",
ClimateRecordFile="${dirclm}/ERA40.DailyTimeSeries.1979-2001.\${mmtgt}\${ddtgt}\${hhtgt}.grads ",
/
EOFNML2
fi

cd ${DIRCRPS}/exec/

# Executa o CRPS
aprun -n 1 -N 1 -d 1 ${DIRCRPS}/exec/CRPS.exe < ${DIRCRPS}/datain/CRPS.nml
EOT0

# Submete o script e aguarda o fim da execução
chmod +x ${DIRCRPS}/scripts/${SCRIPTSFILE}

qsub -W block=true ${DIRCRPS}/scripts/${SCRIPTSFILE}

exit 0
