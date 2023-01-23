#! /bin/bash 
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2021  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para a recomposição dos coeficientes espectrais para ponto de
# grade das análises do Sistema de Previsão por Conjunto Global (SPCON) 
# do CPTEC.
#
# !INTERFACE:
#      ./run_recanl.sh <opcao1> <opcao2> <opcao3> <opcao4>
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> resolucao -> resolução espectral do modelo
#                                
#            <opcao2> pref_anl  -> prefixo que identifica o tipo
#                                   de análise
#
#            <opcao3> pref_nome -> prefixo do arquivo de análise
# 
#            <opcao4> data      -> data da análise corrente
#           
#  Uso/Exemplos: ./run_recanl.sh TQ0126L028 SMT ANLSMT 2012123118
#                (recompõe os coeficientes espectrais da análise
#                das 2012123118 na resolução TQ0126L028)
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

#export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export FILEENV=$(find ${PWD} -name EnvironmentalVariablesMCGA -print)
#export PATHENV=$(dirname ${FILEENV})
#export PATHBASE=$(cd ${PATHENV}; cd ; pwd)

. ${FILEENV} ${1} ${2}

cd ${HOME_suite}/run

TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")

export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

#
# Verificação dos argumentos de entrada
#

if [ -z "${3}" ]
then
  echo "PERR esta faltando"
  exit 1
else
  PERR=${3}
fi
if [ -z "${4}" ]; then
  echo "LABELI esta faltando"
  exit 1
else
  LABELI=${4}
fi

bin=${DK_suite}/recanl/bin/${RESOL}${NIVEL}; mkdir -p ${bin}

#
# Variáveis utilizadas no script de submissão
#

HSTMAQ=$(hostname)

RUNTM=$(date +'%y')$(date +'%m')$(date +'%d')$(date +'%H:%M')
EXT=out

#
# Script de submissão
#

cd ${HOME_suite}/run

mkdir -p ${DK_suite}/recanl/output

SCRIPTSFILE=setrecanl${PERR}.${RESOL}${NIVEL}.${LABELI}.${MAQUI}

if [ $(echo "$QSUB" | grep qsub) ]
then
  SCRIPTHEADER="
#PBS -o ${DK_suite}/recanl/output/${SCRIPTSFILE}.${RUNTM}.out
#PBS -e ${DK_suite}/recanl/output/${SCRIPTSFILE}.${RUNTM}.err
#PBS -l walltime=0:10:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N RECANL
#PBS -q ${AUX_QUEUE}
"
  SCRIPTRUNCMD="aprun -n 1 -N 1 -d 1 " 
  SCRIPTRUNJOB="qsub -W block=true "
else
  SCRIPTHEADER="
#SBATCH --output=${DK_suite}/recanl/output/${SCRIPTSFILE}.${RUNTM}.out
#SBATCH --error=${DK_suite}/recanl/output/${SCRIPTSFILE}.${RUNTM}.err
#SBATCH --time=${AUX_WALLTIME}
#SBATCH --tasks-per-node=1
#SBATCH --nodes=1
#SBATCH --job-name=RECANL
#SBATCH --partition=${AUX_QUEUE}
"
  if [ $USE_SINGULARITY == true ]
  then          
    SCRIPTRUNCMD="module load singularity ; singularity exec -e --bind ${WORKBIND}:${WORKBIND} ${SIFIMAGE} mpirun -np 1 " 
  else
    SCRIPTRUNCMD="mpirun -np 1 " 
  fi  
  #if [ ! -z ${job_pre_id} ]
  #then
  #  SCRIPTRUNJOB="sbatch --dependency=afterok:${job_pre_id}"
  #else
  SCRIPTRUNJOB="sbatch "
  #fi
fi

monitor=${DK_suite}/recanl/output/monitor.t
if [ -e ${monitor} ]; then rm ${monitor}; fi

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

export DATE=\$(date +'%Y')\$(date +'%m')\$(date +'%d')
export HOUR=\$(date +'%H:%M')

echo "Date: "\${DATE}
echo "Hour: "\${HOUR}

#
# LABELI = yyyymmddhh
# LABELI = input file start label
#

export LABELI=${LABELI}

#
# Prefix names for the FORTRAN files
#
# NAMEL - List file name prefix
# NAMES - Input spectral file name prefix
# NAMER - Output gridded file name prefix
#
# Suffix names for the FORTRAN files
#
# EXTL - List file name suffix
# ERSi - Input spectral file name suffix
# ERRi - Output gridded file name suffix
#

export NAMEL=G${PERR}
export NAMES=G${PERR}
export NAMER=G${PERR}

if [ ${PERR} == ANLAVN -o ${PERR} == ANLNMC -o ${PERR} == ANLSMT -o ${PERR} == ANLEIT -o ${PERR} == ANLEIH ]
then
  export EXTL=S.unf
  export ERS1=S.unf
  export ERR1=R.unf
else
  export EXTL=F.ens
  export ERS1=F.ens
  export ERR1=R.ens
fi

#
# Set directories
#
# OPERMOD  is the directory for sources, scripts and
#          printouts files.
# DK_suite is the directory for input and output data
#          and bin files.
# ROPERMOD is the directory for big selected output files.
# IOPERMOD is the directory for input file.
#

cd ${HOME_suite}/run

#
# Now, build the necessary NAMELIST input:
#

export GNAMEL=\${NAMEL}${LABELI}\${EXTL}.${RESOL}${NIVEL}
echo ${DK_suite}/recanl/datain/\${GNAMEL}

cat <<EOT2 > ${DK_suite}/recanl/datain/\${GNAMEL}
\${NAMES}${LABELI}\${ERS1}.${RESOL}${NIVEL}
\${NAMER}${LABELI}\${ERR1}.${RESOL}${NIVEL}
EOT2

mkdir -p ${DK_suite}/recanl/datain

cat <<EOT3 > ${DK_suite}/recanl/datain/recanl${PERR}.nml
 &DATAIN
  LDIM=1
  DIRL='${DK_suite}/recanl/datain/ '
  DIRS='${DK_suite}/model/datain/ '
  DIRR='${DK_suite}/recanl/dataout/${RESOL}${NIVEL}/ '
  GNAMEL='\${GNAMEL} '
 &END
EOT3

mkdir -p ${DK_suite}/recanl/dataout/${RESOL}${NIVEL}/ 
cd ${HOME_suite}/run

#
# Run recomposition
#

echo 'Running recomposition...'

#
# Set directories
#

export recanl_dir=${HOME_suite}/recanl

export source=\${recanl_dir}/source
export bin=\${recanl_dir}/bin/${RESOL}${NIVEL}; mkdir -p \${bin}
export input=\${recanl_dir}/datain; mkdir -p \${input}
export out=\${recanl_dir}/output; mkdir -p \${out}

cd \${bin}

${SCRIPTRUNCMD} ${bin}/recanl.${RESOL}${NIVEL} < \${input}/recanl${PERR}.nml > \${out}/recanl.out.${LABELI}.\${HOUR}.${RESOL}${NIVEL}

touch ${monitor}
EOT0

#
# Submete o script e aguarda o fim da execução
#

export PBS_SERVER=${pbs_server2}

chmod +x ${HOME_suite}/run/${SCRIPTSFILE}

job_recanl=$(${SCRIPTRUNJOB} ${HOME_suite}/run/${SCRIPTSFILE})
export job_recanl_id=$(echo ${job_recanl} | awk -F " " '{print $4}')
echo "recanl ${job_recanl_id}"

until [ -e ${monitor} ]; do sleep 1s; done

#exit 0
