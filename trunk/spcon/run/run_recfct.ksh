# !/bin/ksh
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2017  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para a recomposição dos coeficientes espectrais para ponto de
# grade das previsões do Sistema de Previsão por Conjunto Global 
# (SPCON) do CPTEC.
#
# !INTERFACE:
#      ./run_recfct.ksh <opcao1> <opcao2> <opcao3> 
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> resolucao -> resolução espectral do modelo
#                                
#            <opcao2> membro    -> membro controle ou tamanho do
#                                  conjunto
#
#            <opcao3> data      -> data da análise corrente (a partir
#                                  da qual as previsões foram feitas)
#            
#  Uso/Exemplos: ./run_recfct.ksh TQ0126L028 CTR 2012123118
#                (recompõe os coeficientes espectrais das previsões
#                feitas a partir da análise controle das 2012123118
#                na resolução TQ0126L028)
#                ./run_recfct.ksh TQ0126L028 7 2012123118
#                (recompõe os coeficientes espectrais do conjunto de 7
#                membros das previsões feitas a partir da análise
#                das 2012123118 na resolução TQ0126L028)
# 
# !REVISION HISTORY:
#
# XX Julho de 2017 - C. F. Bastarz - Versão inicial.  
# 16 Agosto de 2017 - C. F. Bastarz - Inclusão comentários.
#
# !REMARKS:
#
# !BUGS:
#
#EOP  
#--------------------------------------------------------------------#
#BOC

# Descomentar para debugar
#set -o xtrace

# Menu de opções/ajuda
if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#BOP/,/^#EOP/p'
  exit 0
fi

# Diretórios principais
export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export PATHENV=$(dirname ${FILEENV})
export PATHBASE=$(cd ${PATHENV}; cd ../; pwd)

. ${FILEENV} ${1} ${2}

cd ${HOME_suite}/../run

# Verificação dos argumentos de entrada
if [ -z "${1}" ]
then
  echo "First argument is not set: TRCLV"
  exit
else
  TRCLV=${1}
fi
if [ -z "${2}" ]
then
  echo "Second argument is not set: PREFIC"
  exit
else
  if ! [[ "${2}" =~ ^[0-9]+$ ]]
  then
    PREFIC=${2}
    TYPES=FCT${PREFIC}
  else
    PREFIC=R
    NMEM=${2}
    TYPES=FCT${PREFIC}PT
  fi
fi
if [ -z "${3}" ]
then
  echo "Third argument is not set (LABELI: yyyymmddhh)"
  exit
else
  LABELI=${3}
fi

TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")

# Variáveis utilizadas no script de submissão
HSTMAQ=$(hostname)
RUNTM=$(date +'%y')$(date +'%m')$(date +'%d')$(date +'%H:%M')
EXT=out

mkdir -p ${DK_suite}/../recfct/output

# Opções específicas para o conjunto de membros ou apenas o controle
if [ ${PREFIC} != CTR ]
then
  export PBSDIRECTIVE="#PBS -J 1-${NMEM}"
  export DEFINEMEM="export MEM=\$(printf %02g \${PBS_ARRAY_INDEX})"
  export MODELDATAOUT="cd ${DK_suite}/model/dataout/${TRCLV}/${LABELI}/\${MEM}${PREFIC}/"
  export ENSTYPE="export TYPES=FCT\${MEM}${PREFIC}"
else
  export MODELDATAOUT="cd ${DK_suite}/model/dataout/${TRCLV}/${LABELI}/${PREFIC}/"
  export ENSTYPE="export TYPES=${TYPES}"
fi

MONITORID=${RANDOM}

export PBS_SERVER=aux20-eth4
RUNTM=$(date +"%s")

# Script de submissão
SCRIPTSFILE=setrecfct${TYPES}.${TRCLV}.${LABELI}${LABELF}.${PBS_SERVER}

cat <<EOT0 > ${HOME_suite}/../run/${SCRIPTSFILE}
#!/bin/bash -x
#PBS -o ${DK_suite}/../recfct/output/${SCRIPTSFILE}.${RUNTM}.out
#PBS -e ${DK_suite}/../recfct/output/${SCRIPTSFILE}.${RUNTM}.err
#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N RECFCT
#PBS -q ${AUX_QUEUE}
${PBSDIRECTIVE}

${DEFINEMEM}

${MODELDATAOUT}

${ENSTYPE}

for LABELF in \$(ls G\${TYPES}${LABELI}* | cut -c 18-27)
do 

  #
  #  Set date (year,month,day) and hour (hour:minute) 
  #
  #  DATE=yyyymmdd
  #  HOUR=hh:mn
  #
  
  export DATE=\$(date +'%Y')\$(date +'%m')\$(date +'%d')
  export HOUR=\$(date +'%H:%M')
  echo "Date: "\$DATE
  echo "Hour: "\$HOUR
  
  #
  #  LABELI = yyyymmddhh
  #  LABELI = input file start label
  #
  
  export LABELI=${LABELI}
  
  #
  #  Prefix names for the FORTRAN files
  #
  #  NAMEL - List file name prefix
  #  NAMES - Input spectral file name prefix
  #  NAMER - Output gridded file name prefix
  #
  #  Suffix names for the FORTRAN files
  #
  #  EXTL - List file name suffix
  #  ERSi - Input spectral file name suffix
  #  ERRi - Output gridded file name suffix
  #
  
  export NAMEL=G\${TYPES}
  export NAMES=G\${TYPES}
  export NAMER=G\${TYPES}
  
  if [ \${TYPES} = ANLAVN ] 
  then
    export EXTL=S.unf
    export ERS1=S.unf
    export ERR1=R.unf
  else
    export EXTL=F.fct
    export ERS1=F.fct
    export ERR1=R.fct
  fi
  
  #
  #  Now, build the necessary NAMELIST input:
  #
  
  GNAMEL=\${NAMEL}\${LABELI}\${LABELF}\${EXTL}.\${TRCLV}
  echo \${GNAMEL}
  echo \${DK_suite}/../recfct/datain/\${GNAMEL}
  
cat <<EOT2 > \${DK_suite}/../recfct/datain/\${GNAMEL}
\${NAMES}\${LABELI}\${LABELF}\${ERS1}.\${TRCLV}
\${NAMER}\${LABELI}\${LABELF}\${ERR1}.\${TRCLV}
EOT2

cat <<EOT3 > \${DK_suite}/../recfct/datain/recfct\${TYPES}.nml
 &DATAIN
  LDIM=1
  DIRL='\${DK_suite}/../recfct/datain/ '
  DIRS='\${DK_suite}/model/dataout/\${TRCLV}/\${LABELI}/\${MEM}${PREFIC}/  '
  DIRR='\${DK_suite}/../recfct/dataout/\${TRCLV}/\${LABELI}/ '
  GNAMEL='\${GNAMEL} '
 &END
EOT3

  mkdir -p \${DK_suite}/../recfct/dataout/\${TRCLV}/\${LABELI}/

  #
  #  Run Decomposition
  #
  
  cd ${HOME_suite}/../recfct/bin/\${TRCLV}
  
  ./recfct.\${TRCLV} < ${DK_suite}/../recfct/datain/recfct\${TYPES}.nml > ${DK_suite}/../recfct/output/recfct\${TYPES}.out.\${LABELI}\${LABELF}.\${HOUR}.\${TRCLV}
  
done

touch ${DK_suite}/../recfct/bin/\${TRCLV}/monitor.${MONITORID}
EOT0

# Submete o script e aguarda o fim da execução
chmod +x ${HOME_suite}/../run/${SCRIPTSFILE}

qsub ${SCRIPTSFILE}

until [ -e "${DK_suite}/../recfct/bin/${TRCLV}/monitor.${MONITORID}" ]; do sleep 1s; done

rm ${DK_suite}/../recfct/bin/${TRCLV}/monitor.${MONITORID}

exit 0
