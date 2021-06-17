#! /bin/bash 
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2017  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para a plotagem dos meteogramas com as plumas de probabilidade
# do GridHistory do modelo do Sistema de Previsão por Conjunto Global 
# SPCON) do CPTEC.
#
# !INTERFACE:
#      ./run_plumes.sh <opcao1> <opcao2> <opcao3> <opcao4> <opcao5> 
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
#  Uso/Exemplos: ./run_plumes.sh TQ0126L028 2020031300 15 NMC 7
#                (calcula e plota os meteogramas com as plumas de probabilidades 
#                das previsões a partir da data 2020031300 para até 15 dias,
#                considerando as 7 perturbações N e P membros na resolução TQ0126L028) 
#
# !REVISION HISTORY:
#
# 09 Julho de 2020 - C. F. Bastarz - Versão inicial.  
#
# !REMARKS:
#
#  As coordenadas com os pontos a serem plotados são lidos a partir dos
#  arquivos do GridHistory do modelo.
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

export LABELF=$(${inctime} ${LABELI} +${NFCTDY}dy %y4%m2%d2%h2)

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

export OPERM=${DK_suite}
export ROPERM=${DK_suite}/produtos

cd ${OPERM}/run

export SCRIPTFILEPATH=${DK_suite}/run/setplumes${RESOL}${NIVEL}.${MAQUI}

cat <<EOT0 > ${SCRIPTFILEPATH}
#!/bin/bash -x
#PBS -o ${ROPERM}/plumes/output/plumes.${RUNTM}.out
#PBS -e ${ROPERM}/plumes/output/plumes.${RUNTM}.err
#PBS -l walltime=00:10:00
#PBS -l select=1:ncpus=1
#PBS -W umask=026
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N PLUMES
#PBS -q ${AUX_QUEUE}

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

# Parameter to be read by plumes : namelist file
# UNDEF     : ( REAL    ) undef value set up according to original grib files
# IMAX      : ( INTEGER ) number of points in zonal direction
# JMAX      : ( INTEGER ) number of points in merdional direction
# NMEMBERS  : ( INTEGER ) number of members of the ensemble
# NFCTDY    : ( INTEGER ) number of forecast days of model integration
# NFCTHOURS : ( INTEGER ) number of hours of model integration
# FREQCALC  : ( INTEGER ) interval in hours of output ensemble forecast
# DIRINP    : ( CHAR    ) input directory (ensemble members)
# DIROUT    : ( CHAR    ) output directory (probability outputs)
# RESOL     : ( CHAR    ) horizontal and vertical model resolution
# PREFX     : ( CHAR    ) preffix for input and output files 

mkdir -p \${ROPERMOD}/plumes/dataout/\${TRUNC}\${LEV}/\${LABELI}/

cat <<EOT > \${OPERMOD}/plumes/bin/plmsetup.${LABELI}.nml
UNDEF     :   -2.56E33
IMAX      :   ${IR}
JMAX      :   ${JR}
NMEMBERS  :   ${NMEMBR}
NFCTDY    :   ${NFCTDY}
NFCTHOURS :   360
FREQCALC  :   6
DIRINP    :   \${ROPERMOD}/pos/dataout/\${TRUNC}\${LEV}/\${LABELI}/
DIROUT    :   \${ROPERMOD}/plumes/dataout/\${TRUNC}\${LEV}/\${LABELI}/
RESOL     :   \${TRUNC}\${LEV}
PREFX     :   ${PREFX}
EOT

cd \${ROPERMOD}/plumes/bin

aprun -n 1 -N 1 -d 1 \${ROPERMOD}/plumes/bin/plumes.x ${LABELI} 

echo "" > \${ROPERMOD}/plumes/bin/plumes-${LABELI}.ok
EOT0

export PBS_SERVER=${pbs_server2}

chmod +x ${SCRIPTFILEPATH}

qsub -W block=true ${SCRIPTFILEPATH}

until [ -e "${ROPERM}/plumes/bin/plumes-${LABELI}.ok" ]; do sleep 1s; done
                                                                                                 
#
# Set directories
#

gname=GFGN
CASE=${RES}
fileloc=LOCMM${LABELI}${LABELF}.${CASE}

if [ -z "${ps}" ]
then
  ps=psuperf
else
  ps=local
fi

yy=$(echo ${LABELI} | cut -c 1-4)
mm=$(echo ${LABELI} | cut -c 5-6)
dd=$(echo ${LABELI} | cut -c 7-8)
hh=$(echo ${LABELI} | cut -c 9-10)

dirbct=${ROPERM}/plumes/dataout/${RES}

#
# Generate the figures of plumes
#
cd ${ROPERM}/plumes/scripts/

mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/AC/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/AP/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/DF/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/MA/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/MT/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/PE/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/RJ/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/RR/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/SE/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/WW/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/AL/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/BA/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/ES/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/MG/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/PA/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/PI/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/RN/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/RS/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/SP/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/ZZ/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/AM/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/CE/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/GO/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/MS/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/PB/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/PR/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/RO/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/SC/
mkdir -p ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif/TO/

${DIRGRADS}/grads -bp << EOT
run plumes.gs
${LABELI} ${LABELF} ${gname} ${CASE} ${ps} ${NMEMBR} ${fileloc} ${ROPERM}/plumes/dataout/${CASE}/${LABELI} ${ROPERM}/plumes/dataout/${CASE}/${LABELI}/gif 1 360
quit
EOT

exit 0
