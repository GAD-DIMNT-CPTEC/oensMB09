#! /bin/bash 
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2017  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para plotar os campos de spaguetti para a temperatura absoluta em 850 e 1000 hPa
# e para a altura geopotencial em 500 hPa, do Sistema de Previsão por Conjunto Global (SPCON) 
# do CPTEC. Os campos são plotados sobre o domínio global e sobre a América do Sul.
#
# !INTERFACE:
#      ./run_spaguetti.sh <opcao1> <opcao2> <opcao3> <opcao4> <opcao5> 
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> resolucao -> resolução espectral do modelo
#
#            <opcao2> data      -> data da análise corrente (a partir
#                                  da qual as previsões foram feitas)
#
#            <opcao3> dias      -> dias de previsões que serão consideradas
#                                  a partir da data da análise
#
#            <opcao4> prefixo   -> prefixo que identifica o tipo de análise
#
#            <opcao5> membro    -> tamanho do conjunto
#            
#  Uso/Exemplos: ./run_spaguetti.sh TQ0126L028 2020031300 15 NMC 7
#                (plota os campos de spaguetti para a análise das
#                2020031300 considerando as 7 perturbações 
#                N e P membros na resolução TQ0126L028) 
#
# !REVISION HISTORY:
#
# 28 Agosto de 2020 - C. F. Bastarz - Versão inicial.  
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

if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#BOP/,/^#EOP/p'
  exit 0
fi

if [ -z ${1} ]
then
  echo "RES if not set"
  exit 1
else
  export RES=${1}
fi

if [ -z ${2} ]
then
  echo "LABELI is not set"
  exit 1
else
  export LABELI=${2}
fi

if [ -z ${3} ]
then
  echo "NFCTDY is not set"
  exit 1
else
  export NFCTDY=${3}
fi

if [ -z ${4} ]
then
  echo "PREFX is not set"
  exit 1
else
  export PREFX=${4}
fi

if [ -z ${5} ]
then
  echo "NRNDP is not set"
  exit 1
else
  export NRNDP=${5}
fi

export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export PATHENV=$(dirname ${FILEENV})
export PATHBASE=$(cd ${PATHENV}; cd ; pwd)

. ${FILEENV} ${RES} ${PREFX}

export OPERM=${DK_suite}
export ROPERM=${DK_suite}/produtos

cd ${HOME_suite}/run

TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")

export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

export NMEMBR=$((2*${NRNDP}+1))

export LABELF=$(${inctime} ${LABELI} +${NFCTDY}dy %y4%m2%d2%h2)

OUT=out
NPROC=1
RESOL=${TRCLV}

#
# Set directories
#

YY=$(echo ${LABELI} | cut -c 1-4)
MM=$(echo ${LABELI} | cut -c 5-6)
DD=$(echo ${LABELI} | cut -c 7-8)
HH=$(echo ${LABELI} | cut -c 9-10)

echo 'LABELI='${LABELI}
                                                                                                 
DIRSCR=${ROPERM}/spaguetti/scripts
DIRGIF=${ROPERM}/spaguetti/gif
DIRCTL=${OPERM}/pos/dataout/${TRCLV}/${LABELI}
DIRENM=${OPERM}/ensmed/dataout/${TRCLV}/${LABELI}

if [ ! -d ${DIRGIF} ]
then
  mkdir -p ${DIRGIF}
else
  echo "${DIRGIF} has already been created"
fi

#
# Evaluate the number of random perturbations
#

let AUX=NMEMBR-1
let NRNDP=AUX/2

echo 'NRNDP='${NRNDP}

#
# Create files which contains the ctl's and the cluster list
#

cd ${DIRSCR}

rm filefct*

TIM=0

LABEL=${LABELI}

while [ ${LABEL} -le ${LABELF} ]
do

  NPERT=1
  
  while [ ${NPERT} -le ${NRNDP} ]
  do
    if [ ${NPERT} -lt 10 ]
    then
      NPERT='0'${NPERT}
    fi
  
    let NPERT=NPERT+1
  done
  
  NCTLS=1
  NPERT=1

  if [ ${LABEL} == ${LABELI} ]
  then
    TYPE='P.icn'
  else
    TYPE='P.fct'
  fi
  
  while [ ${NPERT} -le ${NRNDP} ]
  do

    if [ ${NPERT} -lt 10 ]
    then
      NPERT='0'${NPERT}
    fi
  
    if [ -s ${DIRCTL}/GPOS${NPERT}P${LABELI}${LABEL}${TYPE}.${RESOL}.ctl ]
    then
  
cat << EOT >> filefct${NPERT}P${LABELI}.${TRC}
${DIRCTL}/GPOS${NPERT}P${LABELI}${LABEL}${TYPE}.${RESOL}.ctl
EOT
  
      NCTLS=$((${NCTLS}+1))
 
    else

      echo "${DIRCTL}/GPOS${NPERT}P${LABELI}${LABEL}${TYPE}.${RESOL}.ctl does not exist"
      exit

    fi
  
    if [ -s ${DIRCTL}/GPOS${NPERT}N${LABELI}${LABEL}${TYPE}.${RESOL}.ctl ]
    then
  
cat << EOT >> filefct${NPERT}N${LABELI}.${TRC}
${DIRCTL}/GPOS${NPERT}N${LABELI}${LABEL}${TYPE}.${RESOL}.ctl
EOT

      NCTLS=$((${NCTLS}+1))
 
    else

      echo "${DIRCTL}/GPOS${NPERT}N${LABELI}${LABEL}${TYPE}.${RESOL}.ctl does not exist"
      exit

    fi
  
    let NPERT=NPERT+1

  done
  
  if [ -s ${DIRCTL}/GPOS${PREFX}${LABELI}${LABEL}${TYPE}.${RESOL}.ctl ]
  then
  
cat << EOT >> filefct${PREFX}${LABELI}.${TRC}
${DIRCTL}/GPOS${PREFX}${LABELI}${LABEL}${TYPE}.${RESOL}.ctl
EOT
  
    NCTLS=$((${NCTLS}+1))
 
  else

    echo "${DIRCTL}/GPOS${PREFX}${LABELI}${LABEL}${TYPE}.${RESOL}.ctl does not exist"
    exit

  fi
  
  if [ -s ${DIRENM}/GPOSENM${LABELI}${LABEL}${TYPE}.${RESOL}.ctl ]
  then
  
cat << EOT >> filefctENM${LABELI}.${TRC}
${DIRENM}/GPOSENM${LABELI}${LABEL}${TYPE}.${RESOL}.ctl
EOT
 
    NCTLS=$((${NCTLS}+1))
 
  else

    echo "${DIRENM}/GPOSENM${LABELI}${LABEL}${TYPE}.${RESOL}.ctl does not exist"
    exit

  fi
  
  echo "NCTLS="${NCTLS}
  
  LABEL=$(${inctime} ${LABEL} +1dy %y4%m2%d2%h2)

done

#
# Plot the figures
#
  
# South America
  
${DIRGRADS}/grads -pb << EOT
run sptas.gs
${TRC} ${LABELI} ${NMEMBR} ${NCTLS} ${RESOL} ${PREFX} ${DIRGIF}
EOT

# Global
  
${DIRGRADS}/grads -pb << EOT
run sptgl.gs
${TRC} ${LABELI} ${NMEMBR} ${NCTLS} ${RESOL} ${PREFX} ${DIRGIF}
EOT
  
exit 0
