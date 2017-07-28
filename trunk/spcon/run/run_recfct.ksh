# !/bin/ksh

#./run_recfct.ksh TQ0126L028 CTR 2012123118
#./run_recfct.ksh TQ0126L028 7 2012123118

set -o xtrace

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

export MR=126
export IR=384
export JR=192
export KR=09
export LR=11
export KR=18
export LR=11
export KR=28
export LR=11
export KR=42
export LR=11

#
#  Set machine, Run time and Extention
#

HSTMAQ=$(hostname)
RUNTM=$(date +'%y')$(date +'%m')$(date +'%d')$(date +'%H:%M')
EXT=out

export PBS_SERVER=aux20-eth4
mkdir -p ${DK_suite}/recfct/output

##################################################################################

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

SCRIPTSFILE=setrecfct${TYPES}.${TRCLV}.${LABELI}${LABELF}.${PBS_SERVER}

cat <<EOT0 > ${HOME_suite}/run/${SCRIPTSFILE}
#!/bin/bash -x
#PBS -o ${DK_suite}/recfct/output/${SCRIPTSFILE}.${RUNTM}.out
#PBS -e ${DK_suite}/recfct/output/${SCRIPTSFILE}.${RUNTM}.err
#PBS -l walltime=00:05:00
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
  #  Now, build the necessary INCLUDE for the choosen truncation and 
  #       vertical resolution.. 
  #
  cd \${INC}
cat <<EOT1 > recfct.n
      INTEGER IMAX,JMAX,MEND,KMAX,LMAX
      PARAMETER (IMAX=${IR},JMAX=${JR},MEND=${MR},KMAX=${KR},LMAX=${LR})
EOT1

  if (diff recfct.n recfct.h > /dev/null)
  then
    echo "recfct.n and recfct.h are the same"
    rm -f recfct.n
  else
    echo "recfct.n and recfct.h are different"
    mv recfct.n recfct.h
  fi
  
  #
  #  End of includes
  #
  
  #
  #  Now, build the necessary NAMELIST input:
  #
  
  GNAMEL=\${NAMEL}\${LABELI}\${LABELF}\${EXTL}.\${TRCLV}
  echo \${GNAMEL}
  echo \${DK_suite}/recfct/datain/\${GNAMEL}
  
cat <<EOT2 > \${DK_suite}/recfct/datain/\${GNAMEL}
\${NAMES}\${LABELI}\${LABELF}\${ERS1}.\${TRCLV}
\${NAMER}\${LABELI}\${LABELF}\${ERR1}.\${TRCLV}
EOT2

cat <<EOT3 > \${DK_suite}/recfct/datain/recfct\${TYPES}.nml
 &DATAIN
  LDIM=1
  DIRL='\${DK_suite}/recfct/datain/ '
  DIRS='\${DK_suite}/model/dataout/\${TRCLV}/\${LABELI}/\${MEM}${PREFIC}/  '
  DIRR='\${DK_suite}/recfct/dataout/\${TRCLV}/\${LABELI}/ '
  GNAMEL='\${GNAMEL} '
 &END
EOT3

  mkdir -p \${DK_suite}/recfct/dataout/\${TRCLV}/\${LABELI}/

  #
  #  Run Decomposition
  #
  
  cd ${HOME_suite}/recfct/bin/\${TRCLV}
  
  ./recfct.\${TRCLV} < ${DK_suite}/recfct/datain/recfct\${TYPES}.nml > ${DK_suite}/recfct/output/recfct\${TYPES}.out.\${LABELI}\${LABELF}.\${HOUR}.\${TRCLV}
  
done

touch ${DK_suite}/recfct/bin/\${TRCLV}/monitor.${MONITORID}
EOT0

#
#  Change mode to be executable
#

chmod +x ${HOME_suite}/run/${SCRIPTSFILE}

qsub ${SCRIPTSFILE}

until [ -e "${DK_suite}/recfct/bin/${TRCLV}/monitor.${MONITORID}" ]; do sleep 1s; done

rm ${DK_suite}/recfct/bin/${TRCLV}/monitor.${MONITORID}

exit 0
