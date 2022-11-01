#! /bin/bash
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2021  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para a decomposição para coeficientes espectrais das análises
# em ponto de grade do Sistema de Previsão por Conjunto Global (SPCON) 
# do CPTEC.
#
# !INTERFACE:
#      ./run_decanl.sh <opcao1> <opcao2> <opcao3> <opcao4> <opcao5> 
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> resolucao -> resolução espectral do modelo
#
#            <opcao2> prefixo   -> prefixo que identifica o tipo de
#                                  análise
#
#            <opcao3> moist_opt -> opção lógica (YES/NO) para
#                                  perturbar ou não a umidade
#
#            <opcao4> data      -> data da análise corrente (a partir
#                                  da qual as previsões foram feitas)
#
#            <opcao5> membro    -> tamanho do conjunto
#            
#  Uso/Exemplos: ./run_decanl.sh TQ0126L028 NMC YES 2012111200 7
#                (decompõe em coeficientes espectrais as análises
#                das 2012111200 do conjunto de 7 membros na resolução
#                TQ0126L028) 
#
# !REVISION HISTORY:
#
# XX Julho de 2017   - C. F. Bastarz - Versão inicial.  
# 16 Agosto de 2017  - C. F. Bastarz - Inclusão comentários.
# 17 Junho de 2021   - C. F. Bastarz - Ajustes no nome do script de submissão.
# 18 Junho de 2021   - C. F. Bastarz - Revisão geral.
# 26 Outubro de 2022 - C. F. Bastarz - Inclusão de diretivas do SLURM.
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
# Menu de opções/ajuda
#

if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#BOP/,/^#EOP/p'
  exit 0
fi

export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export PATHENV=$(dirname ${FILEENV})
export PATHBASE=$(cd ${PATHENV}; cd  ;pwd)

. ${FILEENV} ${1} ${2}

cd ${HOME_suite}/run

TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")

export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

#
# Verificação dos argumentos de entrada
#

if [ -z "${2}" ]
then
  echo "PREFIC esta faltando"
  exit 1
else
  PREFIC=${2}
fi

if [ -z "${3}" ]
then
  echo "HUMID esta faltando"
  exit 1
else
  HUMID=${3}
fi

if [ -z "${4}" ]
then
  echo "LABELI esta faltando"
  exit 1
else
  LABELI=${4}
fi
if [ -z "${5}" ]
then
  echo "NPERT esta faltando"
  exit 1
else
  NPERT=${5}
fi

#
# Variáveis utilizadas no script de submissão
#

HSTMAQ=$(hostname)
RUNTM=$(date +'%Y')$(date +'%m')$(date +'%d')$(date +'%H:%M')
EXT=out

#
# Script de submissão
#

cd ${HOME_suite}/run

SCRIPTSFILE=setdecanl.${RESOL}${NIVEL}.${LABELI}.${MAQUI}

if [ $(echo "$QSUB" | grep qsub) ]
then
  SCRIPTHEADER="
#PBS -o ${DK_suite}/decanl/output/${SCRIPTSFILE}.${RUNTM}.out
#PBS -e ${DK_suite}/decanl/output/${SCRIPTSFILE}.${RUNTM}.err
#PBS -S /bin/bash
#PBS -l walltime=0:10:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N DECANLRDP
#PBS -q ${AUX_QUEUE}
"
  SCRIPTRUNCMD="aprun -n 1 -N 1 -d 1 ${HOME_suite}/decanl/bin/\${TRUNC}\${LEV}/decanl.\${TRUNC}\${LEV} < ${DK_suite}/decanl/datain/decanl.nml > ${DK_suite}/decanl/output/decanl.out.\${LABELI}.${PREFIC}.\${HOUR}.\${RESOL}\${NIVEL}"
  SCRIPTRUNJOB="qsub -W block=true "
else
  SCRIPTHEADER="
#SBATCH --output=${DK_suite}/decanl/output/${SCRIPTSFILE}.${RUNTM}.out
#SBATCH --error=${DK_suite}/decanl/output/${SCRIPTSFILE}.${RUNTM}.err
#SBATCH --time=${AUX_WALLTIME}
#SBATCH --tasks-per-node=1
#SBATCH --nodes=1
#SBATCH --job-name=DECANLRDP
#SBATCH --partition=${AUX_QUEUE}
"
  SCRIPTRUNCMD="module load singularity ; singularity exec -e --bind /mnt/beegfs/carlos.bastarz:/mnt/beegfs/carlos.bastarz /mnt/beegfs/carlos.bastarz/containers/egeon_dev.sif mpirun -np 1 ${HOME_suite}/decanl/bin/\${TRUNC}\${LEV}/decanl.\${TRUNC}\${LEV} < ${DK_suite}/decanl/datain/decanl.nml > ${DK_suite}/decanl/output/decanl.out.\${LABELI}.${PREFIC}.\${HOUR}.\${RESOL}\${NIVEL}"
  SCRIPTRUNJOB="sbatch --dependency=afterok:${job_rdpert_id}"
fi

cat <<EOT0 > ${HOME_suite}/run/${SCRIPTSFILE}
#! /bin/bash -x
${SCRIPTHEADER}

export PBS_SERVER=${pbs_server2}

cd ${HOME_suite}/run
. ${FILEENV} ${1} ${2}

#
# Set date (year,month,day) and hour (hour:minute) 
#
# DATE=yyyymmdd
# HOUR=hh:mn
#

DATE=\$(date +'%Y')\$(date +'%m')\$(date +'%d')
HOUR=\$(date +'%H:%M')

echo "Date: "\${DATE}
echo "Hour: "\${HOUR}
export DATE HOUR

#
# LABELI = yyyymmddhh
# LABELI = input file label
#

export LABELI=${LABELI}

#
# Prefix names for the FORTRAN files
#
# NAMEL - List file name prefix
# GNAME - Initial condition file name prefix
# NAMER - Input gridded file name prefix
# NAMES - Output spectral file name prefix
#

if [ ${PREFIC} == AVN ]
then
  export NAMEL=GANLAVN
  export GNAME=GANLAVN
elif [ ${PREFIC} == SMT ]
then
  export NAMEL=GANLSMT 
  export GNAME=GANLSMT
elif [ ${PREFIC} == EIT ]
then
  export NAMEL=GANLEIT 
  export GNAME=GANLEIT
elif [ ${PREFIC} == EIH ]
then
  export NAMEL=GANLEIH 
  export GNAME=GANLEIH
else 
  export NAMEL=GANLNMC
  export GNAME=GANLNMC
fi

#
# Suffix names for the FORTRAN files
#
# EXTL - List file name suffix
# EXTG - Initial condition file name suffix
# ERRi - Input gridded file name suffix
# ERSi - Output spectral file name suffix
#

export EXTL=P.rpt
export EXTG=S.unf
export EXTR=R.unf
export PT=R

#
# Set directories
#
# OPERMOD  is the directory for sources, scripts and
#          printouts files.
# DK_suite is the directory for input and output data
#          and bin files.
# DK_suite is the directory for big selected output files.
# IOPERMOD is the directory for input file.
#

echo \${HOME_suite}
echo \${DK_suite}
echo \${DK_suite}
echo \${DK_suite}/model/datain

cd ${HOME_suite}/run

#
# Set Horizontal Truncation and Vertical Layers
#

export LEV=${NIVEL}
export TRUNC=${RESOL}

#
# Now, build the necessary NAMELIST input:
#

GNAMEL=\${NAMEL}\${LABELI}\${EXTL}.\${TRUNC}\${LEV}

mkdir -p \${DK_suite}/decanl/datain

cat <<EOT2 > \${DK_suite}/decanl/datain/decanl.nml
 &DATAIN
  LDIM=1
  DIRL='\${DK_suite}/decanl/datain/ '
  DIRI='\${DK_suite}/model/datain/ '
  DIRG='\${DK_suite}/rdpert/dataout/\${TRUNC}\${LEV}/ '
  DIRS='\${DK_suite}/model/datain/ '
  GNAMEL='\${GNAMEL} '
 &END
 &HUMIDI
  HUM='${HUMID}'
 &END
EOT2

i=1

while [ \${i} -le ${NPERT} ]
do

  if [ \${i} -le 9 ]
  then

cat <<EOT3 > \${DK_suite}/decanl/datain/\${GNAMEL}
\${GNAME}\${LABELI}\${EXTG}.\${TRUNC}\${LEV}
GANL0\${i}R\${LABELI}\${EXTR}.\${TRUNC}\${LEV}
GANL0\${i}R\${LABELI}\${EXTG}.\${TRUNC}\${LEV}
EOT3

  else

cat <<EOT3 > \${DK_suite}/decanl/datain/\${GNAMEL}
\${GNAME}\${LABELI}\${EXTG}.\${TRUNC}\${LEV}
GANL\${i}R\${LABELI}\${EXTR}.\${TRUNC}\${LEV}
GANL\${i}R\${LABELI}\${EXTG}.\${TRUNC}\${LEV}
EOT3

  fi

#
# Run Decomposition
#

  cd ${HOME_suite}/decanl/bin/\${TRUNC}\${LEV}

  ${SCRIPTRUNCMD}

  echo \${i}
  i=\$((\${i}+1))

done
EOT0

#
# Submete o script e aguarda o fim da execução
#

export PBS_SERVER=${pbs_server2}

chmod +x ${HOME_suite}/run/${SCRIPTSFILE}

job_decanl=$(${SCRIPTRUNJOB} ${HOME_suite}/run/${SCRIPTSFILE})
export job_decanl_id=$(echo ${job_decanl} | awk -F " " '{print $4}')
echo ${job_decanl_id}

#exit 0
