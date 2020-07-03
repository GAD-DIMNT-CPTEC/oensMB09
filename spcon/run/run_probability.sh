#! /bin/bash -x

#help#
# Uso:
# ./run_probability.sh TQ0126L028 2020031300 15 NMC 7
#help#

#set -o xtrace

if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#help#/,/^#help#/p'
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

export RUNTM=`date +'%Y%m%d%T'`

export OPERM=${DK_suite}
export ROPERM=${DK_suite}

cd ${OPERM}/run

export PBS_SERVER=aux20-eth4

export SCRIPTFILEPATH=${DK_suite}/run/setprobability${RESOL}${NIVEL}.${MAQUI}

cat <<EOT0 > ${SCRIPTFILEPATH}
#!/bin/bash -x
#PBS -o ${DK_suite}/probability/output/probability.${RUNTM}.out
#PBS -e ${DK_suite}/probability/output/probability.${RUNTM}.err
#PBS -l walltime=00:10:00
#PBS -l select=1:ncpus=1
#PBS -W umask=026
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N PROBS
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

# Parameter to be read by prcmed.f90 : namelist file
# UNDEF     : ( REAL    ) undef value set up according to original grib files
# IMAX      : ( INTEGER ) number of points in zonal direction
# JMAX      : ( INTEGER ) number of points in merdional direction
# NMEMBERS  : ( INTEGER ) number of members of the ensemble
# NFCTDY    : ( INTEGER ) number of forecast days
# FREQCALC  : ( INTEGER ) interval in hours for computing the probability precipitation
# DIRINP    : ( CHAR    ) input directory (ensemble members)
# DIROUT    : ( CHAR    ) output directory (probability outputs)
# RESOL     : ( CHAR    ) horizontal and vertical model resolution
# PREFX     : ( CHAR    ) preffix for input and output files 

mkdir -p \${ROPERMOD}/probability/dataout/\${TRUNC}\${LEV}/\${LABELI}/
mkdir -p \${ROPERMOD}/probability/rmsclim

cat <<EOT > \${OPERMOD}/probability/bin/probsetup.${LABELI}.nml
UNDEF     :   9.999E+20
IMAX      :   ${IR}
JMAX      :   ${JR}
NMEMBERS  :   ${NMEMBR}
NFCTDY    :   ${NFCTDY}
FREQCALC  :   6
DIRINP    :   \${ROPERMOD}/pos/dataout/\${TRUNC}\${LEV}/\${LABELI}/
DIROUT    :   \${ROPERMOD}/probability/dataout/\${TRUNC}\${LEV}/\${LABELI}/
RESOL     :   \${TRUNC}\${LEV}
PREFX     :   ${PREFX}
EOT

cd \${OPERMOD}/probability/bin

time \${OPERMOD}/probability/bin/probability.x ${LABELI} 

echo "" > \${OPERMOD}/probability/bin/probability-${LABELI}.ok
EOT0

chmod +x ${SCRIPTFILEPATH}

#qsub -W block=true ${SCRIPTFILEPATH}
#
#until [ -e "${OPERM}/probability/bin/probability-${LABELI}.ok" ]; do sleep 1s; done
                                                                                                 
#
#  Set directories
#

yy=$(echo ${LABELI} | cut -c 1-4)
mm=$(echo ${LABELI} | cut -c 5-6)
dd=$(echo ${LABELI} | cut -c 7-8)
hh=$(echo ${LABELI} | cut -c 9-10)

dirscr=${OPERM}/probability/scripts
dirgif=${ROPERM}/probability/gif

dirbct=${ROPERM}/probability/dataout/${RES}/${LABELI}
if [ ! -d ${dirgif} ]
then
  mkdir -p ${dirgif}
else
  echo "${dirgif} has already been created"
fi

#
# Create the list of probability ctls  
#

labelf=$(${caldate} ${LABELI} + ${NFCTDY}d 'yyyymmddhh')

arqlist=prob${LABELI}${labelf}.${RES}.lst
ls -l ${ROPERM}/probability/dataout/${RES}/${LABELI}/prob${LABELI}* | awk '{print $9}' > ${ROPERM}/probability/dataout/${RES}/${LABELI}/prob${LABELI}${labelf}.${RES}.lst

rm -f ${dirscr}/filefct${LABELI}.${TRC}
for arq in $(cat ${dirbct}/${arqlist} | grep ctl)
do
  echo ${arq} >> ${dirscr}/filefct${LABELI}.${TRC}
done

#
# Number of ctl files on the list 
#

nblst=$(cat ${dirscr}/filefct${LABELI}.${TRC} | wc -l)
echo "nblst="${nblst}

#
# Generate the figures
#

cd ${ROPERM}/probability/scripts

echo "${DIRGRADS}/grads -lb run plot_precprob.gs ${RES} ${TRC} ${LABELI} ${nblst} ${dirbct} ${dirgif}"
${DIRGRADS}/grads -lb << EOT
run plot_precprob.gs
${RES} ${TRC} ${LABELI} ${nblst} ${dirbct} ${dirgif}
EOT

exit 0