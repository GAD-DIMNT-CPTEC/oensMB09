#! /bin/ksh 

set -o xtrace

# Membro controle:
#./run_pos.ksh 48 24 1 TQ0126L028 2012123118 CTR 2
#
# Demais membros:
# - pos-processamento das previsoes geradas a partir das analises perturbadas randomicamente:
#./run_pos.ksh 48 24 1 TQ0126L028 2012123118 7 R
# - pos-processamento das previsoes geradas a partir das analises perturbadas por EOF (subtraidas):
#./run_pos.ksh 48 24 1 TQ0126L028 2012123118 7 N
# - pos-processamento das previsoes geradas a partir das analises perturbadas por EOF (somadas):
#./run_pos.ksh 48 24 1 TQ0126L028 2012123118 7 P

#
# Help
#

#help#
#*********************************************************************************#
#                                                                                 #
#                                                                                 #
#*********************************************************************************#
#help#

cria_namelist() {

sed -e "s;#TRUNC#;${1};g" \
    -e "s;#LEV#;${2};g" \
    -e "s;#LABELI#;${3};g" \
    -e "s;#LABELF#;${4};g" \
    -e "s;#PREFIX#;${5};g" \
    -e "s;#DATAIN#;${6};g" \
    -e "s;#DATAOUT#;${7};g" \
    -e "s;#DATALIB#;${8};g" \
    -e "s;#Binary#;${9};g" \
    -e "s;#REQTB#;${10};g" \
    -e "s;#REGINT#;${11};g" \
    -e "s;#RES#;${12};g" \
    ${13}/POSTIN-GRIB.template > ${14}/POSTIN-GRIB

echo "Namelist criado em: ${14}/POSTIN-GRIB"

}

if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#help#/,/^#help#/p'
  exit 1
fi
if [ -z "${1}" ]
then
  echo "MPPWIDTH is not set" 
  exit 3
else
  export MPPWIDTH=${1}  
fi
if [ -z "${2}" ]
then
  echo "MPPNPPN is not set" 
  exit 3
else
  export MPPNPPN=${2}  
fi
if [ -z "${3}" ]
then
  echo "MPPDEPTH is not set" 
  exit 3
else
  export MPPDEPTH=${3}  
fi
if [ -z "${4}" ]
then
  echo "RESOL is not set" 
  exit 3
else
  export RES=${4}  
fi
if [ -z "${5}" ]
then
  echo "LABELI is not set" 
  exit 3
else
  export LABELI=${5} 
fi
#if [ -z "${6}" ]
#then
#  echo "NFDAYS is not set" 
#  exit 3
#else
#  export NFDAYS=${6}  
#fi
if [ -z "${6}" ]
then
  echo "ANLTYPE is not set" 
else
  if [ "${6}" != "CTR" ]
  then 
    export ANLTYPE=${6}  
    if [ -z "${7}" ]
    then
      echo "ANLPERT is not set" 
      exit 1
    else
      export ANLPERT=${7}  
    fi
  else
    export ANLTYPE=${6}  
  fi
fi

export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export PATHENV=$(dirname ${FILEENV})
export PATHBASE=$(cd ${PATHENV}; cd ../; pwd)

. ${FILEENV} ${RES} ${PREFIC}

cd ${HOME_suite}/run

TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")

export RESOL=$(echo ${TRCLV} | cut -c 1-6)
export NIVEL=$(echo ${TRCLV} | cut -c 7-11)

# Se for a previsao controle, integra o modelo por NFDAYS;
# Se nao for a previsao controle, integra o modelo por 48 horas
# (revisar)
#if [ $(echo ${ANLTYPE} | grep R | wc -l) -eq 0 ]
#then
NFDAYS=2 # controle, kpds13=10
LABELF=$(date -d "${LABELI:0:8} ${LABELI:8:2}:00 ${NFDAYS} days" +"%Y%m%d%H")
#else
#LABELF=$(date -d "${LABELI:0:8} ${LABELI:8:2}:00 48 hours" +"%Y%m%d%H")   
#fi

export INCTIME=${HOME}/bin/inctime

export DIRRESOL=$(echo ${TRC} ${LV} | awk '{printf("TQ%4.4dL%3.3d\n",$1,$2)}')
export MAQUI=$(hostname -s)

export SCRIPTFILEPATH=${HOME_suite}/run/set$(echo "${ANLTYPE}" | awk '{print tolower($0)}')${ANLPERT}posg.${DIRRESOL}.${LABELI}.${MAQUI}
export NAMELISTFILEPATH=${HOME_suite}/run

export BINARY=".FALSE."
export REQTB="p"
export REGINT=".FALSE."
export RESPOS="-0.50000"

if [ ${ANLTYPE} == CTR ]
then

  EXECFILEPATH=${DK_suite}/pos/exec_SMT${LABELI}.${ANLTYPE}

  mkdir -p ${EXECFILEPATH}/setout
   
  ln -sf ${DK_suite}/pos/exec/PostGrib ${EXECFILEPATH}

  export DATALIB=${DK_suite}/pos/datain/

  export DATAIN=${DK_suite}/model/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}
  export DATAOUT=${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}

  mkdir -p ${DATAOUT}

  export PREFIX=${ANLTYPE}

  cria_namelist ${RESOL} ${NIVEL} ${LABELI} ${LABELF} ${PREFIX} ${DATAIN} ${DATAOUT} ${DATALIB} ${BINARY} ${REQTB} ${REGINT} ${RESPOS} ${NAMELISTFILEPATH} ${EXECFILEPATH} 
 
else

  for MEM in $(seq -f %02g 1 ${ANLTYPE})
  do

    EXECFILEPATH=${DK_suite}/pos/exec_SMT${LABELI}.${ANLPERT}PT/${MEM}${ANLPERT}
    EXECFILEPATHMEM=${DK_suite}/pos/exec_SMT${LABELI}.${ANLPERT}PT/${MEM}${ANLPERT}

    mkdir -p ${EXECFILEPATH}/setout ${EXECFILEPATHMEM}
   
    ln -sf ${DK_suite}/pos/exec/PostGrib ${EXECFILEPATHMEM}

    export DATALIB=${DK_suite}/pos/datain/

    export DATAIN=${DK_suite}/model/dataout/${DIRRESOL}/${LABELI}/${MEM}${ANLPERT}
    export DATAOUT=${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${MEM}${ANLPERT}

    mkdir -p ${DATAOUT}

    export PREFIX=${MEM}${ANLPERT}

    cria_namelist ${RESOL} ${NIVEL} ${LABELI} ${LABELF} ${PREFIX} ${DATAIN} ${DATAOUT} ${DATALIB} ${BINARY} ${REQTB} ${REGINT} ${RESPOS} ${NAMELISTFILEPATH} ${EXECFILEPATH} 

  done

fi

if [ ${ANLTYPE} != CTR ]
then
  export PBSOUTFILE="#PBS -o ${DK_suite}/pos/exec_SMT${LABELI}.${ANLPERT}PT/setout/Out.pos.${LABELI}.MPI${MPPWIDTH}.out"
  export PBSERRFILE="#PBS -e ${DK_suite}/pos/exec_SMT${LABELI}.${ANLPERT}PT/setout/Out.pos.${LABELI}.MPI${MPPWIDTH}.err"
  export PBSDIRECTIVENAME="#PBS -N POSENS${ANLPERT}PT"
  export PBSDIRECTIVEARRAY="#PBS -J 1-${ANLTYPE}"
  export PBSMEM="export MEM=\$(printf %02g \${PBS_ARRAY_INDEX})"
  export PBSEXECFILEPATH="export EXECFILEPATH=${DK_suite}/pos/exec_SMT${LABELI}.${ANLPERT}PT/\${MEM}${ANLPERT}"
  export MONITORFILE="${DK_suite}/pos/exec_SMT${LABELI}.${ANLPERT}PT/pos.${ANLTYPE}"
else
  export PBSOUTFILE="#PBS -o ${DK_suite}/pos/exec_SMT${LABELI}.${ANLTYPE}/setout/Out.pos.${LABELI}.MPI${MPPWIDTH}.out"
  export PBSERRFILE="#PBS -e ${DK_suite}/pos/exec_SMT${LABELI}.${ANLTYPE}/setout/Out.pos.${LABELI}.MPI${MPPWIDTH}.err"
  export PBSDIRECTIVENAME="#PBS -N POSENS${ANLTYPE}"
  export PBSDIRECTIVEARRAY=""
  export PBSMEM=""
  export PBSEXECFILEPATH="export EXECFILEPATH=${DK_suite}/pos/exec_SMT${LABELI}.${ANLTYPE}"
  export MONITORFILE="${DK_suite}/pos/exec_SMT${LABELI}.${ANLTYPE}/pos.${ANLTYPE}"
fi

PBSServer='eslogin'

cat <<EOF0 > ${SCRIPTFILEPATH}
#! /bin/bash -x
${PBSOUTFILE}
${PBSERRFILE}
#PBS -l walltime=0:30:00
#PBS -l mppwidth=${MPPWIDTH}
#PBS -l mppnppn=${MPPNPPN}
#PBS -l mppdepth=${MPPDEPTH}
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
${PBSDIRECTIVENAME}
${PBSDIRECTIVEARRAY}
#PBS -q ${QUEUE}

ulimit -s unlimited
ulimit -c unlimited

export PBS_SERVER=${PBSServar}
export KMP_STACKSIZE=128m

${PBSMEM}
${PBSEXECFILEPATH}

cd \${EXECFILEPATH}

date

aprun -m500h -n ${MPPWIDTH} -N ${MPPNPPN} -d ${MPPDEPTH} \${EXECFILEPATH}/PostGrib < \${EXECFILEPATH}/POSTIN-GRIB > \${EXECFILEPATH}/Print.pos.${LABELI}.MPI${MPPWIDTH}.log 

date

touch ${MONITORFILE}
EOF0

chmod +x ${SCRIPTFILEPATH}

qsub ${SCRIPTFILEPATH}

until [ -e ${MONITORFILE} ]; do sleep 1s; done
rm ${MONITORFILE}

exit 0
