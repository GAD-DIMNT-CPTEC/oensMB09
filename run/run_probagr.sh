#! /bin/bash 
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2021  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para o cálculo das probabilidades de precipitação acima de 10mm
# para 3 semanas, a partir da  média da precipitação em ponto de grade
# do Sistema de Previsão por Conjunto Global (SPCON) do CPTEC.
#
# !INTERFACE:
#      ./run_probagr.sh <opcao1> <opcao2> <opcao3> <opcao4> <opcao5> 
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
#  Uso/Exemplos: ./run_probagr.sh TQ0126L028 2020031300 15 NMC 7
#                (calcula as probabilidades de precipitação acumulada acima de 10 mm
#                para 3 semanas, a partir da data 2020031300 considerando 
#                a média das 7 perturbações N e P membros na resolução TQ0126L028) 
#
# !REVISION HISTORY:
#
# 03 Julho de 2020     - C. F. Bastarz - Versão inicial.  
# 18 Junho de 2020     - C. F. Bastarz - Revisão geral.
# 01 Novembro de 2022  - C. F. Bastarz - Inclusão de diretivas do SLURM.
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

#
# Parâmetros do produto (não alterar)
#

export ndacc=5    # número de dias em que a precipitação deverá ser acumulada (maior ou igual a 1)
export noutpday=3 # número de semanas a serem consideradas (múltiplo de 3)

export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export PATHENV=$(dirname ${FILEENV})
export PATHBASE=$(cd ${PATHENV}; cd ; pwd)

. ${FILEENV} ${RES} ${PREFX}

cd ${HOME_suite}/run

TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")

export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

export NMEMBR=$((2*${NRNDP}+1))

export LABELF=$(${inctime} ${LABELI} +${NFCTDY}d %y4%m2%d2%h2)

case ${TRC} in
  021) MR=22  ; IR=64  ; JR=32  ; NPGH=93   ; DT=1800 ;;
  030) MR=31  ; IR=96  ; JR=48  ; NPGH=140  ; DT=1800 ;;
  042) MR=43  ; IR=128 ; JR=64  ; NPGH=187  ; DT=1800 ;;
  047) MR=48  ; IR=144 ; JR=72  ; NPGH=26   ; DT=1200 ;;
  062) MR=63  ; IR=192 ; JR=96  ; NPGH=315  ; DT=1200 ;;
  079) MR=80  ; IR=240 ; JR=120 ; NPGH=26   ; DT=900  ;;
  085) MR=86  ; IR=256 ; JR=128 ; NPGH=26   ; DT=720  ;;
  094) MR=95  ; IR=288 ; JR=144 ; NPGH=591  ; DT=720  ;;
  106) MR=107 ; IR=320 ; JR=160 ; NPGH=711  ; DT=600  ;;
  126) MR=127 ; IR=384 ; JR=192 ; NPGH=284  ; DT=600  ;;
  159) MR=160 ; IR=480 ; JR=240 ; NPGH=1454 ; DT=450  ;;
  170) MR=171 ; IR=512 ; JR=256 ; NPGH=1633 ; DT=450  ;;
  213) MR=214 ; IR=640 ; JR=320 ; NPGH=2466 ; DT=360  ;;
  254) MR=255 ; IR=768 ; JR=384 ; NPGH=3502 ; DT=300  ;;
  319) MR=320 ; IR=960 ; JR=480 ; NPGH=26   ; DT=240  ;;
  *) echo "Wrong request for horizontal resolution: ${TRC}" ; exit 1;
esac

export RUNTM=$(date +'%Y%m%d%T')

#
# Diretórios
#

export OPERM=${DK_suite}
export ROPERM=${DK_suite}/produtos

#
# Script de submissão
#

cd ${OPERM}/run

export SCRIPTFILEPATH1=${DK_suite}/run/setprobagr.${RESOL}${NIVEL}.${LABELI}.${MAQUI}
export SCRIPTFILEPATH2=${DK_suite}/run/setprobagr_figs.${RESOL}${NIVEL}.${LABELI}.${MAQUI}

if [ $(echo "$QSUB" | grep qsub) ]
then
  SCRIPTHEADER1="
#PBS -o ${ROPERM}/probagr/output/probagr.${RUNTM}.out
#PBS -e ${ROPERM}/probagr/output/probagr.${RUNTM}.err
#PBS -l walltime=00:10:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N PROBAGR
#PBS -q ${AUX_QUEUE}
"
  SCRIPTHEADER2="
#PBS -o ${ROPERM}/probagr/output/probagr_figs.${RUNTM}.out
#PBS -e ${ROPERM}/probagr/output/probagr_figs.${RUNTM}.err
#PBS -l walltime=00:10:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N PROBAGRFIGS
#PBS -q ${AUX_QUEUE}
"
  SCRIPTRUNCMD="aprun -n 1 -N 1 -d 1 \${ROPERMOD}/probagr/bin/probagr.x ${LABELI} > \${ROPERMOD}/probagr/output/probagr.${RUNTM}.log"
  SCRIPTRUNJOB="qsub -W block=true "
else
  SCRIPTHEADER1="
#SBATCH --output=${ROPERM}/probagr/output/probagr.${RUNTM}.out
#SBATCH --error=${ROPERM}/probagr/output/probagr.${RUNTM}.err
#SBATCH --time=00:10:00
#SBATCH --tasks-per-node=1
#SBATCH --nodes=1
#SBATCH --job-name=PROBAGR
#SBATCH --partition=${AUX_QUEUE}
"
  SCRIPTHEADER2="
#SBATCH --output=${ROPERM}/probagr/output/probagr_figs.${RUNTM}.out
#SBATCH --error=${ROPERM}/probagr/output/probagr_figs.${RUNTM}.err
#SBATCH --time=00:10:00
#SBATCH --tasks-per-node=1
#SBATCH --nodes=1
#SBATCH --job-name=PROBAGRFIGS
#SBATCH --partition=${AUX_QUEUE}
"
  if [ $USE_SINGULARITY == true ]
  then
    SCRIPTRUNCMD="module load singularity ; singularity exec -e --bind ${WORKBIND}:${WORKBIND} ${SIFIMAGE} mpirun -np 1 ${SIFOENSMB09BIN}/produtos/probagr/bin/probagr.x ${LABELI} > \${ROPERMOD}/probagr/output/probagr.${RUNTM}.log"
  else
    SCRIPTRUNCMD="mpirun -np 1 \${ROPERMOD}/probagr/bin/probagr.x ${LABELI} > \${ROPERMOD}/probagr/output/probagr.${RUNTM}.log"
  fi
  SCRIPTRUNJOB="sbatch "
fi

if [ -e ${ROPERM}/probagr/bin/probagr-${LABELI}.ok ]; then rm ${ROPERM}/probagr/bin/probagr-${LABELI}.ok; fi

if [ -e ${ROPERM}/probagr/bin/probagr_figs-${LABELI}.ok ]; then rm ${ROPERM}/probagr/bin/probagr_figs-${LABELI}.ok; fi

cat <<EOT0 > ${SCRIPTFILEPATH1}
#! /bin/bash -x
${SCRIPTHEADER1}

export DATE=$(date +'%Y%m%d')
export HOUR=$(date +'%T')

# OPERMOD is the directory for sources, scripts and printouts files.
# SOPERMOD is the directory for input and output data and bin files.
# ROPERMOD is the directory for big selected output files.

export OPERMOD=${OPERM}
export ROPERMOD=${ROPERM}
export LEV=${NIVEL}
export TRUNC=${RESOL}
export MACH=${MAQUI}
export EXTS=S.unf

# Parameter to be read by prcmed.f90 : namelist file
# UNDEF     : ( REAL    ) undef value set up according to original grib files
# IMAX      : ( INTEGER ) number of points in zonal direction
# JMAX      : ( INTEGER ) number of points in merdional direction
# NMEMBERS  : ( INTEGER ) number of members of the ensemble
# NFCTDY    : ( INTEGER ) number of forecast days
# FREQCALC  : ( INTEGER ) interval in hours of output ensemble forecast
# NWEEK     : ( INTEGER ) number of times that will be accumulate the precipitation
# NDACC     : ( INTEGER ) number of days to accumlate the precipitation 
# DATALSTDIR: ( CHAR    ) input directory (ensemble members)
# DATAOUTDIR: ( CHAR    ) output directory (probability outputs)
# PREFX     : ( CHAR    ) preffix for input and output files 
# RESOL     : ( CHAR    ) horizontal and vertical model resolution

mkdir -p \${ROPERMOD}/probagr/dataout/\${TRUNC}\${LEV}/\${LABELI}/

cat <<EOT > \${ROPERMOD}/probagr/bin/probagr.${LABELI}.nml
UNDEF     :   9.999E+20
IMAX      :   ${IR}
JMAX      :   ${JR}
NMEMBERS  :   ${NMEMBR}
NFCTDY    :   ${NFCTDY}
FREQCALC  :   6
NWEEK     :   3
NDACC     :   5
DATAINDIR :   \${OPERMOD}/pos/dataout/\${TRUNC}\${LEV}/\${LABELI}/
DATAOUTDIR:   \${ROPERMOD}/probagr/dataout/\${TRUNC}\${LEV}/\${LABELI}/
PREFX     :   ${PREFX}
RESOL     :   \${TRUNC}\${LEV}
EOT

cd \${ROPERMOD}/probagr/bin

${SCRIPTRUNCMD}

echo "" > \${ROPERMOD}/probagr/bin/probagr-${LABELI}.ok
EOT0

#
# Figuras
#

cat <<EOT1 > ${SCRIPTFILEPATH2}
#! /bin/bash -x
${SCRIPTHEADER2}

export DATE=$(date +'%Y%m%d')
export HOUR=$(date +'%T')

# OPERMOD is the directory for sources, scripts and printouts files.
# SOPERMOD is the directory for input and output data and bin files.
# ROPERMOD is the directory for big selected output files.

export OPERMOD=${OPERM}
export ROPERMOD=${ROPERM}
export LEV=${NIVEL}
export TRUNC=${RESOL}
export MACH=${MAQUI}
export EXTS=S.unf

yy=$(echo ${LABELI} | cut -c 1-4)
mm=$(echo ${LABELI} | cut -c 5-6)
dd=$(echo ${LABELI} | cut -c 7-8)
hh=$(echo ${LABELI} | cut -c 9-10)

dirscr=${ROPERM}/probagr/scripts
dirgif=${ROPERM}/probagr/gif/${LABELI}

dirbct=${ROPERM}/probagr/dataout/${RES}/${LABELI}

if [ ! -d \${dirgif} ]
then
  mkdir -p \${dirgif}
else
  echo "\${dirgif} ja existe"
fi

#
# Lista de arquivos descritores (ctl)
#

labelf=\$(${inctime} ${LABELI} +\${NFCTDY}d %y4%m2%d2%h2)

arqlist=wmaprecprob${LABELI}\${labelf}.${RES}.lst

rm -f \${dirbct}/filefct${LABELI}.${resol}

for arq in \$(cat \${dirbct}/\${arqlist} | grep ctl)
do
  echo \${arq} >> \${dirbct}/filefct${LABELI}.${RES}
done

nblst=\$(cat \${dirbct}/filefct${LABELI}.${RES} | wc -l)
echo "nblst="\${nblst}

cd ${ROPERM}/probagr/scripts

${DIRGRADS}/grads -lb << EOT
run plot_precprob_agric.gs
${RES} ${TRC} ${LABELI} \${nblst} \${ndacc} \${noutpday} \${dirbct} \${dirgif} ${convert}
EOT

echo "" > \${ROPERMOD}/probagr/bin/probagr_figs-${LABELI}.ok
EOT1

#
# Submissão
#

export PBS_SERVER=${pbs_server2}

chmod +x ${SCRIPTFILEPATH1}

${SCRIPTRUNJOB} ${SCRIPTFILEPATH1}

until [ -e "${ROPERM}/probagr/bin/probagr-${LABELI}.ok" ]; do sleep 1s; done
                                                                                                 
chmod +x ${SCRIPTFILEPATH2}

${SCRIPTRUNJOB} ${SCRIPTFILEPATH2}

until [ -e "${ROPERM}/probagr/bin/probagr_figs-${LABELI}.ok" ]; do sleep 1s; done
                                                                                                 
if [ ${SEND_TO_FTP} == true ]
then
  cd ${ROPERM}/probagr/gif/${LABELI}/
  ls *.png >  list.txt
  rsync -arv * ${FTP_ADDRESS}/probagr/${LABELI}/
fi

#exit 0
