#! /bin/bash 
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2021  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para a plotagem dos gráficos do potencial de velocidade em 
# 200 hPa do Sistema de Previsão por Conjunto Global (SPCON) do CPTEC.
#
# !INTERFACE:
#      ./run_chievol.sh <opcao1> <opcao2> <opcao3> <opcao4> <opcao5> 
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
#  Uso/Exemplos: ./run_chievol.sh TQ0126L028 2020031300 15 NMC 7
#                (plota o plotencial de velocidade da análise e das previsões de 
#                5, 10 e 15 dias a partir da data 2020031300  considerando as 7 
#                perturbações N e P membros na resolução TQ0126L028) 
#
# !REVISION HISTORY:
#
# 10 Julho de 2020     - C. F. Bastarz - Versão inicial.  
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

#
# Menu de ajuda
#

if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#BOP/,/^#EOP/p'
  exit 0
fi

#
# Argumentos da linha de comando
#

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
  echo "NPERT esta faltando"
  exit 1
else
  export NPERT=${5}
fi

export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export PATHENV=$(dirname ${FILEENV})
export PATHBASE=$(cd ${PATHENV}; cd ; pwd)

. ${FILEENV} ${RES} ${PREFX}

#
# Script de submissão
#

export TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
export LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")
export RESOL=T${TRC}
export NIVEL=L${LV}
export CASE=${TRCLV}

export RUNTM=$(date +'%Y%m%d%T')

export OPERM=${DK_suite}
export ROPERM=${DK_suite}/produtos

mkdir -p ${ROPERM}/chievol/output/

cd ${OPERM}/run

export SCRIPTFILEPATH=${DK_suite}/run/setchievol.${RES}.${LABELI}.${MAQUI}

if [ $(echo "$QSUB" | grep qsub) ]
then
  SCRIPTHEADER="
#PBS -o ${ROPERM}/chievol/output/chievol.${RUNTM}.out
#PBS -e ${ROPERM}/chievol/output/chievol.${RUNTM}.err
#PBS -l walltime=00:10:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N CHIEVOL
#PBS -q ${AUX_QUEUE}
"
  SCRIPTRUNJOB="qsub -W block=true "
else
  SCRIPTHEADER="
#SBATCH --output=${ROPERM}/chievol/output/chievol.${RUNTM}.out
#SBATCH --error=${ROPERM}/chievol/output/chievol.${RUNTM}.err
#SBATCH --time=00:10:00
#SBATCH --tasks-per-node=1
#SBATCH --nodes=1
#SBATCH --job-name=CHIEVOL
#SBATCH --partition=${AUX_QUEUE}
"
  SCRIPTRUNJOB="sbatch "
fi

if [ -e ${ROPERM}/chievol/output/chievol_figs-${LABELI}.ok ]; then rm ${ROPERM}/chievol/output/chievol_figs-${LABELI}.ok; fi

cat <<EOT > ${SCRIPTFILEPATH}
#! /bin/bash -x
${SCRIPTHEADER}

export NMEMBR=${NPERT}
export OUT=out
export NPROC=1

#
# Cálculo de LABELF
#

LABELF=$(${inctime} ${LABELI} +${NFCTDY}d %y4%m2%d2%h2)

export yydir=$(awk 'BEGIN {print substr("'${LABELI}'",1,4)}')
export mmdir=$(awk 'BEGIN {print substr("'${LABELI}'",5,2)}')
export dddir=$(awk 'BEGIN {print substr("'${LABELI}'",7,2)}')

#
# Diretórios
#

export OPERM=${DK_suite}
export ROPERM=${DK_suite}/produtos

export DIRSCR=${ROPERM}/chievol/scripts
export DIRGIF=${ROPERM}/chievol/gif/${LABELI}
export DIRINP=${OPERM}/ensmed/dataout/${TRCLV}/${LABELI}

#
# Prefixo dos arquivos
#

export GPOS=GPOSENM

#
# Lista dos arquivos descritore (ctl) a serem abertos
#

cd \${DIRSCR}

rm -f filefct${LABELI}.${TRC}

echo "\${DIRINP}/\${GPOS}${LABELI}${LABELI}P.icn.${CASE}.ctl" >> filefct${LABELI}.${RESOL}

for nd in 5 10 15
do
  LABELPF=\$(${inctime} ${LABELI} +\${nd}d %y4%m2%d2%h2)
  echo "\${DIRINP}/\${GPOS}${LABELI}\${LABELPF}P.fct.${CASE}.ctl" >> filefct${LABELI}.${RESOL}
done

#
# Figuras
#

# Número de arquivos a serem abertos
NWK=4 

mkdir -p \${ROPERM}/chievol/gif/${LABELI}

${DIRGRADS}/grads -bpc << EOF
run plot_chi_evol.gs
${LABELI} \${LABELF} \${NWK} ${CASE} ${RESOL} \${DIRGIF} ${convert}
EOF

rm -f filefct${LABELI}.${RESOL}

echo "" > \${ROPERM}/chievol/output/chievol_figs-${LABELI}.ok
EOT

#
# Submissão
#

export PBS_SERVER=${pbs_server2}

chmod +x ${SCRIPTFILEPATH}

${SCRIPTRUNJOB} ${SCRIPTFILEPATH}

until [ -e "${ROPERM}/chievol/output/chievol_figs-${LABELI}.ok" ]; do sleep 1s; done

if [ ${SEND_TO_FTP} == true ]
then
  cd ${ROPERM}/chievol/gif/${LABELI}/
  ls *.png >  list.txt
  rsync -arv * ${FTP_ADDRESS}/chievol/${LABELI}/
fi

#exit 0
