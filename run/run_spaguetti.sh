#! /bin/bash 
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2021  #
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
# 28 Agosto de 2020    - C. F. Bastarz - Versão inicial.  
# 18 Junho de 2021     - C. F. Bastarz - Revisão geral.
# 06 Fevereiro de 2023 - C. F. Bastarz - Adaptações para a Egeon.
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
  echo "RES esta faltando"
  exit 1
else
  export RES=${1}
fi

if [ -z ${2} ]
then
  echo "LABELI esta faltando"
  exit 1
else
  export LABELI=${2}
fi

if [ -z ${3} ]
then
  echo "NFCTDY esta faltando"
  exit 1
else
  export NFCTDY=${3}
fi

if [ -z ${4} ]
then
  echo "PREFX esta faltando"
  exit 1
else
  export PREFX=${4}
fi

if [ -z ${5} ]
then
  echo "NRNDP esta faltando"
  exit 1
else
  export NRNDP=${5}
fi

export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export PATHENV=$(dirname ${FILEENV})
export PATHBASE=$(cd ${PATHENV}; cd ; pwd)

. ${FILEENV} ${RES} ${PREFX}

#
# Script de submissão
#

export OPERM=${DK_suite}
export ROPERM=${DK_suite}/produtos

export RUNTM=$(date +'%Y%m%d%T')

TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")

export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

mkdir -p ${ROPERM}/spaguetti/output

cd ${HOME_suite}/run

export SCRIPTFILEPATH=${DK_suite}/run/setspaguetti.${RES}.${LABELI}.${MAQUI}

if [ $(echo "$QSUB" | grep qsub) ]
then
  SCRIPTHEADER="
#PBS -o ${ROPERM}/spaguetti/output/spaguetti.${RUNTM}.out
#PBS -e ${ROPERM}/spaguetti/output/spaguetti.${RUNTM}.err
#PBS -l walltime=00:10:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N SPAGUETTI
#PBS -q ${AUX_QUEUE}
"
  SCRIPTRUNJOB="qsub -W block=true "
else
  SCRIPTHEADER="
#SBATCH --output=${ROPERM}/spaguetti/output/spaguetti.${RUNTM}.out
#SBATCH --error=${ROPERM}/spaguetti/output/spaguetti.${RUNTM}.err
#SBATCH --time=00:10:00
#SBATCH --tasks-per-node=1
#SBATCH --nodes=1
#SBATCH --job-name=SPAGUETTI
#SBATCH --partition=${AUX_QUEUE}
"
  SCRIPTRUNJOB="sbatch "
fi

if [ -e ${ROPERM}/spaguetti/output/spaguetti_figs-${LABELI}.ok ]; then rm ${ROPERM}/spaguetti/output/spaguetti_figs-${LABELI}.ok; fi

cat <<EOT > ${SCRIPTFILEPATH}
#! /bin/bash -x
${SCRIPTHEADER}

export NMEMBR=$((2*${NRNDP}+1))

export LABELF=\$(${inctime} ${LABELI} +${NFCTDY}d %y4%m2%d2%h2)

OUT=out
NPROC=1
RESOL=${TRCLV}

YY=$(echo ${LABELI} | cut -c 1-4)
MM=$(echo ${LABELI} | cut -c 5-6)
DD=$(echo ${LABELI} | cut -c 7-8)
HH=$(echo ${LABELI} | cut -c 9-10)

echo 'LABELI='${LABELI}
                                                                                                 
#
# Diretórios
#

DIRSCR=${ROPERM}/spaguetti/scripts
DIRGIF=${ROPERM}/spaguetti/gif/${LABELI}
DIRCTL=${OPERM}/pos/dataout/${TRCLV}/${LABELI}
DIRENM=${OPERM}/ensmed/dataout/${TRCLV}/${LABELI}

if [ ! -d \${DIRGIF} ]
then
  mkdir -p \${DIRGIF}
else
  echo "\${DIRGIF} has already been created"
fi

#
# Número de perturbações randômicas
#

let AUX=NMEMBR-1
let NRNDP=AUX/2

echo 'NRNDP='\${NRNDP}

#
# Lista com os nomes dos arquivos descritores (ctl) 
#

cd \${DIRSCR}

rm filefct*

TIM=0

LABEL=${LABELI}

while [ \${LABEL} -le \${LABELF} ]
do

  NPERT=1
  
  while [ \${NPERT} -le \${NRNDP} ]
  do
    if [ \${NPERT} -lt 10 ]
    then
      NPERT='0'\${NPERT}
    fi
  
    let NPERT=NPERT+1
  done
  
  NCTLS=1
  NPERT=1

  if [ \${LABEL} == \${LABELI} ]
  then
    TYPE='P.icn'
  else
    TYPE='P.fct'
  fi
  
  while [ \${NPERT} -le \${NRNDP} ]
  do

    if [ \${NPERT} -lt 10 ]
    then
      NPERT='0'\${NPERT}
    fi
  
    if [ -s \${DIRCTL}/GPOS\${NPERT}P${LABELI}\${LABEL}\${TYPE}.${RES}.ctl ]
    then
  
cat << EOT1 >> filefct\${NPERT}P${LABELI}.${TRC}
\${DIRCTL}/GPOS\${NPERT}P${LABELI}\${LABEL}\${TYPE}.${RES}.ctl
EOT1
  
      NCTLS=\$((\${NCTLS}+1))
 
    else

      echo "\${DIRCTL}/GPOS\${NPERT}P${LABELI}\${LABEL}\${TYPE}.${RES}.ctl nao existe"
      exit 1

    fi
  
    if [ -s \${DIRCTL}/GPOS\${NPERT}N${LABELI}\${LABEL}\${TYPE}.${RES}.ctl ]
    then
  
cat << EOT2 >> filefct\${NPERT}N${LABELI}.${TRC}
\${DIRCTL}/GPOS\${NPERT}N${LABELI}\${LABEL}\${TYPE}.\${RES}.ctl
EOT2

      NCTLS=\$((\${NCTLS}+1))
 
    else

      echo "\${DIRCTL}/GPOS\${NPERT}N${LABELI}\${LABEL}\${TYPE}.${RES}.ctl nao existe"
      exit 1

    fi
  
    let NPERT=NPERT+1

  done
  
  if [ -s \${DIRCTL}/GPOS${PREFX}${LABELI}\${LABEL}\${TYPE}.${RES}.ctl ]
  then
  
cat << EOT3 >> filefct${PREFX}${LABELI}.${TRC}
\${DIRCTL}/GPOS${PREFX}${LABELI}\${LABEL}\${TYPE}.${RES}.ctl
EOT3
  
    NCTLS=\$((\${NCTLS}+1))
 
  else

    echo "\${DIRCTL}/GPOS${PREFX}${LABELI}\${LABEL}\${TYPE}.${RES}.ctl nao existe"
    exit 1

  fi
  
  if [ -s \${DIRENM}/GPOSENM${LABELI}\${LABEL}\${TYPE}.${RES}.ctl ]
  then
  
cat << EOT4 >> filefctENM${LABELI}.${TRC}
\${DIRENM}/GPOSENM${LABELI}\${LABEL}\${TYPE}.${RES}.ctl
EOT4
 
    NCTLS=\$((\${NCTLS}+1))
 
  else

    echo "\${DIRENM}/GPOSENM${LABELI}\${LABEL}\${TYPE}.${RES}.ctl nao existe"
    exit 1

  fi
  
  echo "NCTLS="\${NCTLS}
  
  LABEL=\$(${inctime} \${LABEL} +1d %y4%m2%d2%h2)

done

#
# Figuras
#
  
# América do Sul
  
${DIRGRADS}/grads -pb << EOT5
run sptas.gs
${TRC} ${LABELI} \${NMEMBR} \${NCTLS} ${RES} ${PREFX} \${DIRGIF} ${convert}
EOT5

# Global
  
${DIRGRADS}/grads -pb << EOT6
run sptgl.gs
${TRC} ${LABELI} \${NMEMBR} \${NCTLS} ${RES} ${PREFX} \${DIRGIF} ${convert}
EOT6

echo "" > \${ROPERM}/spaguetti/output/spaguetti_figs-${LABELI}.ok
EOT

#
# Submissão
#

export PBS_SERVER=${pbs_server2}

chmod +x ${SCRIPTFILEPATH}

${SCRIPTRUNJOB} ${SCRIPTFILEPATH}

until [ -e "${ROPERM}/spaguetti/output/spaguetti_figs-${LABELI}.ok" ]; do sleep 1s; done

if [ ${SEND_TO_FTP} == true ]
then
  cd ${ROPERM}/spaguetti/gif/${LABELI}/
  ls *.png >  list.txt
  rsync -arv * ${FTP_ADDRESS}/spaguetti/${LABELI}/
fi

#exit 0
