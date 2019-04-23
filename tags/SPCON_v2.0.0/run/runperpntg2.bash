#!/bin/bash -x
#help#
#********************************************************************#
#                                                                    #
#     Name:           runperpntg.sx6                                 #
#                                                                    #
#     Function:       This script generated and submit               #
#                     a script to start the global model             #
#                     at a given date/time to integration            #
#                     from initial date till final date.             #
#                                                                    #
#     Date:           May    28th, 2003.                             #
#     Last change:    May    28th, 2003.                             #
#                                                                    #
#     Valid Arguments for runperpntg.sx6                             #
#                                                                    #
#     First    :   HELP: help or nothing for getting help            #
#     First    : COMPILE: help, make, clean or run                   #
#     Second   :    TRC: three-digit triangular truncation           #
#     Third    :     LV: two-digit number of vertical sigma-layers   #
#     Fourth   :    NUM: pertubation number                          #
#     Fifth    :     PT: pertubation suffix                          #        
#     Sixth    : LABELI: initial forecasting label                   #
#     Seventh  : LABELF: final perturbation forecasting label        #
#     Eighth   : NFDAYS: number of forecasting days                  #
#     Nineth   :  HUMID: YES or NO (humidity will be perturbed)      #
#     Tenth    :NUMPERT: number of random perturbations              #
#     Eleventh :  NMSST: SST file name or nothing                    #
#                                                                    #
#     Obs.   : NMSST : default sstaoi                                #
#              LABELx: yyyymmddhh                                    #
#                      yyyy = four digit year                        #
#                        mm = two digit month                        #
#                        dd = two digit day                          #
#                        hh = two digit UTC hour                     #
#********************************************************************#
#help#
#
# Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
    cat < ${0} | sed -n '/^#help#/,/^#help#/p'
    exit 0
fi

export FILEENV=`find ./ -name EnvironmentalVariablesOENS -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${1} ${2}
cd ${HOME_suite}/run

TRC=`echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0"`
LV=`echo ${TRCLV} | cut -c 7-11 | tr -d "L0"`
export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

if [ -z "${2}" ]; then
   echo "PERT: NMC, AVN CTR 01N  "
   exit
else
   PREFIC=${2}
fi

if [ -z "${3}" ]; then
   echo "argument is not set (H)"
   exit
else
   HUMID=${3}
fi

if [ -z "${4}" ]; then
   echo "Fifth argument is not set (LABELI: yyyymmddhh)"
   exit
else
   LABELI=${4}
fi

export NUM=${PREFIC:0:2}
export PT=${PREFIC:2:1}

export NUMPERT=${NPERT}

if [ -z "${5}" ]; then
   echo "Warning NMSST is not set. Using default: sstaoi"
   export NMSST=sstaio
else
   export NMSST=${5}
fi

export NUM PT
LABELF=$(date -d "${LABELI:0:8} ${LABELI:8:2}:00 ${NFDAYS} days" +"%Y%m%d%H")
export LABELI LABELF NFDAYS
NIVELP=K15

find ${SC2_suite}/model/dataout/${TRCLV}/${LABELI} -name "GFCT${NUM}${PT}${LABELI}*" -print -exec rm -f {} \;

#
cd ${HOME_suite}/run

export PREFXO=${NUM}${PT}
export PREFIC=${NUM}${PT}

./runctrmodgpro.bash 48 24 1 ${TRCLV} ${NUM}${PT} ${LABELI} ${NUM}${PT}

export PBS_SERVER=aux20-eth4
mkdir -p ${SC2_suite}/model/exec/setout

SCRIPTSFILE=setperpntg.${NUM}${TRUNC}${LEV}.${LABELI}

RUNTM=$(date +"%s")

cat <<EOT0 > ${HOME_suite}/run/${SCRIPTSFILE}
#!/bin/bash
#
#*********************************************************#
#                                                         #
#      Name:     setperpntg${NUM}${TRUNC}${LEV}.${MACHINE}#
#                                                         #
#      Function: This script runs the spectral            #
#                global numerical weather                 #
#                forecasting at ${MACHINE}.               #
#                                                         #
#*********************************************************#
#
#PBS -o ${SC2_suite}/model/exec/setout/${SCRIPTSFILE}.${NUM}${TRUNC}${LEV}.${LABELI}.${RUNTM}.out
#PBS -e ${SC2_suite}/model/exec/setout/${SCRIPTSFILE}.${NUM}${TRUNC}${LEV}.${LABELI}.${RUNTM}.err
#PBS -l walltime=0:40:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N ENSANL${NUM}
#PBS -q ${QUEUE1}
#
#   Change directory to run
#
cd ${HOME_suite}/run
#
#   Set current sst file name for model run
#
SSTNM=${NMSST}
#echo "+++ cp -rpf ${ROPERM}/model/dataout/T${TRC}L${LV}/GFCT${NUM}${PT}${LABELI}*fct* ${ROPERM}/model/datain"
#
#  Run scripts of recomposition         
#
./runrecfct.bash   ${TRCLV} ${NUM}${PT} FCT${NUM}${PT} ${LABELI}
./runeofs_hn.bash  ${TRCLV} ${NUM}${PT} YES ${LABELI}
./runeofs_hs.bash  ${TRCLV} ${NUM}${PT} YES ${LABELI}
./runeofs_san.bash ${TRCLV} ${NUM}${PT} YES ${LABELI}
./runeofs_sas.bash ${TRCLV} ${NUM}${PT} YES ${LABELI}
./runeofs_tr.bash  ${TRCLV} ${NUM}${PT} YES ${LABELI}
./rundeco.bash ${TRCLV} ${NUM}${PT} YES ${LABELI} sstwkl

#
### F I M ###
#
EOT0
#
set +x

chmod a+x ${HOME_suite}/run/${SCRIPTSFILE}

echo "qsub ${HOME_suite}/run/${SCRIPTSFILE}"
jobnumber=$(qsub ${HOME_suite}/run/${SCRIPTSFILE} | cut -d. -f1)
it=2
i=1
while [ ${it} -gt 0 ];do
   echo "$i >>> Verificando job $jobnumber na Fila"
   it=`qstat | grep $jobnumber | wc -l`
   let i=$i+1
   sleep 3
done

exit 0
