#!/bin/bash -x
#help#
#*************************************************************************************#
#                                                                                     #
# script to run CPTEC Global Model on PC Clusters under MPI Scali                     #
# and Sun Grid Engine without OpenMP                                                  #
#                                                                                     #
# assumptions: assume present at the same directory:                                  #
#              ParModel_MPI (Global Model Executable file)                            #
#              MODELIN (Global Model input Namelist file)                             #
#                                                                                     #
# usage: run_multi_UNA cpu_mpi cpu_node  task_omp name TRC LV LABELI LABELF NMC hold  #
# where:                                                                              #
# cpu_mpi: integer, the desired number of mpi processes                               #
# cpu_node: integer, the desired number of mpi processes per shared memory node       #
# name: character, the job name (for SGE)                                             #
# hold: any, present or not;                                                          #
#            if absent, script finishes after queueing job;                           #
#            if present, script holds till job completion                             #
#*************************************************************************************#
#help#
#
#       Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#help#/,/^#help#/p'
  exit 1
else
  TRC=`echo ${5} | awk '{print $1/1}'`   
fi
if [ -z "${6}" ]
then
  echo "LV is not set" 
  exit 2
else
  LV=`echo ${6} | awk '{print $1/1}'`    
fi

if [ -z "${7}" ]
then
  echo "LABELI is not set" 
  exit 3
else
  export LABELI=${7}  
fi
if [ -z "${8}" ]
then
  echo "LABELF is not set" 
  exit 3
else
  export LABELF=${8}  
fi
if [ -z "${9}" ]
then
  echo "PREFIC is not set" 
  exit 3
else
  export PREFIC=${9}  
fi

if [ "$#" == 10 ]
then hold=""
else hold=
fi


if [ ${TRC} = 21 ]; then
 export timestep=3600
fi 
if [ ${TRC} = 31 ]; then
 export timestep=1800
fi 
if [ ${TRC} = 42 ]; then
 export timestep=1800
fi 
if [ ${TRC} = 62 ]; then
 export timestep=1200
fi
if [ ${TRC} = 106 ]; then
 export timestep=900
fi
if [ ${TRC} = 126 ]; then
 export timestep=600
fi
if [ ${TRC} = 133 ]; then
 export timestep=600
fi
if [ ${TRC} = 159 ]; then
 export timestep=600
fi
if [ ${TRC} = 170 ]; then
 export timestep=450
fi
if [ ${TRC} = 213 ]; then
 export timestep=450
fi
if [ ${TRC} = 213 ]; then
 export timestep=360
fi
if [ ${TRC} = 254 ]; then
 export timestep=300
fi
if [ ${TRC} = 299 ]; then
 export timestep=300
fi
if [ ${TRC} = 319 ]; then
 export timestep=225
fi
if [ ${TRC} = 341 ]; then
 export timestep=200
fi
if [ ${TRC} = 382 ]; then
 export timestep=180
fi
if [ ${TRC} = 511 ]; then
 export timestep=150
fi
if [ ${TRC} = 533 ]; then
 export timestep=150
fi
if [ ${TRC} = 666 ]; then
 export timestep=150
fi
if [ ${TRC} = 863 ]; then
 export timestep=150
fi
if [ ${TRC} = 1279 ]; then
 export timestep=20
fi

#
# SETTING THE APPROPRIATED ENVIRONMENT
#
CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
PATHA=`pwd`
export FILEENV=`find ${PATHA} -name EnvironmentalVariablesMCGA -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${CASE} ${PREFIC}
cd ${HOME_suite}/run
#
#   Set nproc, resol, host, machine, NQS Queue and Run time
#
DIRRESOL=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
HSTMAQ=`hostname`
MAQUI=`hostname -s`
QUEUE=${QUEUE}
RUNTM=`date +'%Y%m%d%T'`
yi=`awk 'BEGIN {print substr("'${LABELI}'",1,4)}'` ; export yi
mi=`awk 'BEGIN {print substr("'${LABELI}'",5,2)}'` ; export mi
di=`awk 'BEGIN {print substr("'${LABELI}'",7,2)}'` ; export di
hi=`awk 'BEGIN {print substr("'${LABELI}'",9,2)}'` ; export hi
yf=`awk 'BEGIN {print substr("'${LABELF}'",1,4)}'` ; export yf
mf=`awk 'BEGIN {print substr("'${LABELF}'",5,2)}'` ; export mf
df=`awk 'BEGIN {print substr("'${LABELF}'",7,2)}'` ; export df
hf=`awk 'BEGIN {print substr("'${LABELF}'",9,2)}'` ; export hf

echo $yi $mi $di $hi  $yf $mf $df $hf
mkdir -p ${DK_HSM}/GFCT/${yi}${mi}${di}${hi} 
mkdir -p ${DK_HSM}/GFGH/${yi}${mi}${di}${hi} 
mkdir -p ${HOME_suite}/run/setout
#
#########################################################
#
#      SCRIPT FOR GLOBAL MODEL PRODUCTION RUNS 
#
#########################################################

#
# Step 1: Set Directories and files:
#
#   DIRBASE is the root directory path; 
#           all files belong to subdirectories of root;
#   EXECFILEPATH is the executable filename (with path)
#   SCRIPTFILEPATH is the script file that submits executable (with path)
#   NAMELISTFILEPATH contains the namelist file read by the executable (with path)
#   OUTPUTFILEPATH is the executable output file (with path)
#
EXECFILEPATH=${DK_suite}/model/exec
EXECFILEPATH2=${DK_suite}/model/exec_${PREFIC}
SCRIPTFILEPATH=${EXECFILEPATH2}/modg${DIRRESOL}.${MAQUI}
NAMELISTFILEPATH=${HOME_suite}/run
OUTPUTFILEPATH=${HOME_suite}/run/setout/modg${DIRRESOL}.${MAQUI}.${RUNTM}.out
FSCR=${HOME_suite}/run
mkdir -p ${EXECFILEPATH2}
export cpu_mpi=$1
export cpu_node=$2
export ThreadsperMPITASK=$3
export RES=$4
num=$(($cpu_mpi+$cpu_node-1))
fra=$(($num/$cpu_node))
cpu_tot=$(($fra*$cpu_node))
echo fila=mpi-npn${cpu_node} total cpus=${cpu_tot}
#
# Step 3: Build script to submit the script above in the UNA
#
cat <<EOF0>${SCRIPTFILEPATH}
#!/bin/bash
#PBS -o ${HSTMAQ}:${EXECFILEPATH2}/setout/Out.model.${LABELI}.${tmstp}.%s.MPI${cpu_mpi}.out
#PBS -j oe
#PBS -l walltime=4:00:00
#PBS -l mppwidth=${cpu_mpi}
#PBS -l mppnppn=${cpu_node}
#PBS -l mppdepth=${ThreadsperMPITASK}
#PBS -V
#PBS -S /bin/bash
#PBS -N $RES
#PBS -q ${QUEUE}
#PBS -h u

#

EOF0
chmod +x ${SCRIPTFILEPATH}
cd ${EXECFILEPATH2}
if [[ ${it} -eq 1 ]];then
FIRST=`qsub ${SCRIPTFILEPATH}`
export FIRST
echo $FIRST
else
SECOND=`qsub -W depend=afterok:$FIRST ${SCRIPTFILEPATH}`
echo $SECOND
fi
#qsub ${hold} ${SCRIPTFILEPATH}
echo  ${hold} ${SCRIPTFILEPATH}

#it=2
#while [ ${it} -gt 0 ];do
#it=`qstat | grep $RES | wc -l`
#done

