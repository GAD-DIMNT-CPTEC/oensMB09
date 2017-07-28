#! /bin/ksh

set -o xtrace

#./run_recanl.ksh TQ0126L028 NMC ANLNMC 2012111200

#help#
#*******************************************************************#
#                                                                   #
#                                                                   #
#*******************************************************************#
#help#

#
#  Help
#

if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#help#/,/^#help#/p'
  exit 0
fi

#
#  Set directories
#
#  HOME_suite - HOME DA SUITE
#  DK_suite - /scratchin
#  DK_suite - /scratchout
#

export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export PATHENV=$(dirname ${FILEENV})

export PATHBASE=$(cd ${PATHENV}; cd ../; pwd)

. ${FILEENV} ${1} ${2}

cd ${HOME_suite}/run

TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")

export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

if [ -z "${3}" ]
then
  echo "Fourth argument is not set: TYPES"
  exit
else
  PERR=${3}
fi
if [ -z "${4}" ]; then
  echo "Fifth argument is not set (LABELI: yyyymmddhh)"
  exit
else
  LABELI=${4}
fi

bin=${DK_suite}/recanl/bin/${RESOL}${NIVEL}; mkdir -p ${bin}

#
# Set machine, Run time and Extention
#

HSTMAQ=$(hostname)

RUNTM=$(date +'%y')$(date +'%m')$(date +'%d')$(date +'%H:%M')
EXT=out

echo ${MAQUI}
echo ${AUX_QUEUE}
echo ${RUNTM}
echo ${EXT}

export PBS_SERVER=aux20-eth4

cd ${HOME_suite}/run

mkdir -p ${DK_suite}/recanl/output

SCRIPTSFILE=setrecanl.${PERR}${RESOL}${NIVEL}.${LABELI}.${MAQUI}

cat <<EOT0 > ${SCRIPTSFILE}
#!/bin/bash -x
#PBS -o ${DK_suite}/recanl/output/${SCRIPTSFILE}.${RUNTM}.out
#PBS -e ${DK_suite}/recanl/output/${SCRIPTSFILE}.${RUNTM}.err
#PBS -l walltime=0:05:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N RECANL
#PBS -q ${AUX_QUEUE}

export PBS_SERVER=aux20-eth4

cd ${HOME_suite}/run
. ${FILEENV} ${1} ${2}

#
#  Set date (year,month,day) and hour (hour:minute) 
#
#  DATE=yyyymmdd
#  HOUR=hh:mn
#

export DATE=\$(date +'%Y')\$(date +'%m')\$(date +'%d')
export HOUR=\$(date +'%H:%M')

echo "Date: "\${DATE}
echo "Hour: "\${HOUR}

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

export NAMEL=G${PERR}
export NAMES=G${PERR}
export NAMER=G${PERR}

if [ ${PERR} == ANLAVN -o ${PERR} == ANLNMC -o ${PERR} == ANLSMT ]
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
#  Set directories
#
#  OPERMOD  is the directory for sources, scripts and
#           printouts files.
#  DK_suite is the directory for input and output data
#           and bin files.
#  ROPERMOD is the directory for big selected output files.
#  IOPERMOD is the directory for input file.
#

cd ${HOME_suite}/run

#
#  Now, build the necessary NAMELIST input:
#

export GNAMEL=\${NAMEL}${LABELI}\${EXTL}.${RESOL}${NIVEL}
echo ${DK_suite}/recanl/datain/\${GNAMEL}

cat <<EOT2 > ${DK_suite}/recanl/datain/\${GNAMEL}
\${NAMES}${LABELI}\${ERS1}.${RESOL}${NIVEL}
\${NAMER}${LABELI}\${ERR1}.${RESOL}${NIVEL}
EOT2

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
#  Run Decomposition
#

echo 'Running recomposition...'

#
#  Set directories
#

export recanl_dir=${HOME_suite}/recanl

export source=\${recanl_dir}/source
export bin=\${recanl_dir}/bin/${RESOL}${NIVEL}; mkdir -p \${bin}
export input=\${recanl_dir}/datain; mkdir -p \${input}
export out=\${recanl_dir}/output; mkdir -p \${out}

cd \${bin}

echo "./recanl.${RESOL}${NIVEL} < \${input}/recanl${PERR}.nml > \${out}/recanl.out.${LABELI}.\${HOUR}.${RESOL}${NIVEL}"
./recanl.${RESOL}${NIVEL} < \${input}/recanl${PERR}.nml > \${out}/recanl.out.${LABELI}.\${HOUR}.${RESOL}${NIVEL}

touch \${bin}/monitor.t
EOT0

#
#  Change mode to be executable
#

chmod +x setrecanl.${PERR}${RESOL}${NIVEL}.${LABELI}.${MAQUI}

qsub setrecanl.${PERR}${RESOL}${NIVEL}.${LABELI}.${MAQUI}

until [ -e ${bin}/monitor.t ]; do sleep 1s; done

rm ${bin}/monitor.t

exit 0
