#!/bin/bash -x
# 48 24 1 para o controle
#./runctrmodgpro.bash 48 24 1 TQ0126L028 NMC 2012111200 CTR
#help#
#*********************************************************************************#
#                                                                                 #
# script to run CPTEC Global Model on PC Clusters under MPI Scali                 #
# and Sun Grid Engine without OpenMP                                              #
#                                                                                 #
# assumptions: assume present at the same directory:                              #
#              ParModel_MPI (Global Model Executable file)                        #
#              MODELIN (Global Model input Namelist file)                         #
#                                                                                 #
# usage:                                                                          #
#   runctrmodgpro.una cpu_mpi cpu_node name TRC LV LABELI LABELF NMC anltype hold #
# where:                                                                          #
# cpu_mpi: integer, the desired number of mpi processes                           #
# cpu_node: integer, the desired number of mpi processes per shared memory node   #
# name: character, the job name (for SGE)                                         #
# anltype: NMC or AVN                                                             #
# hold: any, present or not;                                                      #
#            if absent, script finishes after queueing job;                       #
#            if present, script holds till job completion                         #
#*********************************************************************************#
#help#
#
#       Help:
#
if [ "${1}" = "help" -o -z "${1}" ]; then
  cat < ${0} | sed -n '/^#help#/,/^#help#/p'
  exit 1
fi

# NAO ESQUECER DE TRATAR ESTES DADOS DE ENTRADA
export MPPWIDTH=${1}
export MPPNPPN=${2}
export MPPDEPTH=${3}

if [ -z "${5}" ]
then
  echo "PREFIC is not set" 
  exit 3
else
  export PREFIC=${5}  
fi

export FILEENV=`find ./ -name EnvironmentalVariablesOENS -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`

# Ex. das variaveis:     . ${FILEENV} TQ0126L028 NMC
. ${FILEENV} ${4} ${5}

cd ${HOME_suite}/run

TRC=`echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0"`
LV=`echo ${TRCLV} | cut -c 7-11 | tr -d "L0"`
export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

if [ -z "${6}" ]
then
  echo "LABELI is not set" 
  exit 3
else
  export LABELI=${6}  
fi
if [ -z "${7}" ]
then
  echo "ANLTYPE is not set" 
  exit 3
else
  export ANLTYPE=${7}  
fi

if [ $(echo $ANLTYPE | grep R | wc -l) -eq 0 ]; then
   LABELF=$(date -d "${LABELI:0:8} ${LABELI:8:2}:00 $NFDAYS days" +"%Y%m%d%H")
else
   LABELF=$(date -d "${LABELI:0:8} ${LABELI:8:2}:00 48 hours" +"%Y%m%d%H")   
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

cd ${HOME_suite}/run
#
#   Set nproc, resol, host, machine, NQS Queue and Run time
#
DIRRESOL=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
HSTMAQ=`hostname`
MAQUI=`hostname -s`
RUNTM=`date +'%Y%m%d%T'`
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
SCRIPTFILEPATH=${HOME_suite}/run/setctrmodg.${DIRRESOL}.${LABELI}.${MAQUI}
NAMELISTFILEPATH=${HOME_suite}/run
OUTPUTFILEPATH=${HOME_suite}/run/setout/modgctr.${DIRRESOL}.${MAQUI}.${RUNTM}
FSCR=${HOME_suite}/run

export cpu_mpi=$1
export cpu_node=$2
export ThreadsperMPITASK=$3

export RES=ENSCTR
num=$(($cpu_mpi+$cpu_node-1))
fra=$(($num/$cpu_node))
cpu_tot=$(($fra*$cpu_node))
echo fila=mpi-npn${cpu_node} total cpus=${cpu_tot}
#
# build Nanmelist to model MCGA
#
if [ -z "${PREFXO}" ] ;then
  export PREFX=${ANLTYPE} 
else
  export PREFX=${PREFXO}
fi 
if [[ "${PREFIC}" == "CTR" ]] ;then
  export PREFY=${ANLTYPE}
else
  export PREFY=${PREFIC}
fi

EXECFILEPATH=${SC1_suite}/model/exec_${PREFY};mkdir -p ${EXECFILEPATH}/setout

ln -sf ${SC1_suite}/model/exec/ParModel_MPI ${EXECFILEPATH}

#if [ `echo $PREFIC | grep "R" | wc -l` -ge 1 ]; then
#   export DHFCT=6
#else
   export DHFCT=3
#fi

export NMSST="sstwkl"
export eigeninit=".FALSE."
export mgiven=".TRUE."      #,   ! mgiven       --> .FALSE.
export gaussgiven=".TRUE."      #,   ! gaussgiven       --> .FALSE.
export aspa="'"
export path_in=${SC1_suite}/model/datain; mkdir -p ${path_in}
export dirfNameOutput=${SC2_suite}/model/dataout/${DIRRESOL}/${LABELI}; mkdir -p ${dirfNameOutput}

yi=`awk 'BEGIN {print substr("'${LABELI}'",1,4)}'` ; export yi
mi=`awk 'BEGIN {print substr("'${LABELI}'",5,2)}'` ; export mi
di=`awk 'BEGIN {print substr("'${LABELI}'",7,2)}'` ; export di
hi=`awk 'BEGIN {print substr("'${LABELI}'",9,2)}'` ; export hi
yf=`awk 'BEGIN {print substr("'${LABELF}'",1,4)}'` ; export yf
mf=`awk 'BEGIN {print substr("'${LABELF}'",5,2)}'` ; export mf
df=`awk 'BEGIN {print substr("'${LABELF}'",7,2)}'` ; export df
hf=`awk 'BEGIN {print substr("'${LABELF}'",9,2)}'` ; export hf

cat ${NAMELISTFILEPATH}/MODELIN | awk '{  
 if (substr($1,1,5) == "trunc")
  {
   "echo ${TRC}" | getline TRC	   
    printf(" trunc    =%4.4d,		      !TRC   : three-digit triangular truncation\n",TRC)
  }
 else if (substr($1,1,4) == "vert")
  {
   "echo ${LV}" | getline LV	 
    printf(" vert     =%3.3d,		      !LV    : two-digit number of vertical sigma-layers\n",LV)
  }
 else if (substr($1,1,2) == "dt")
  {
   "echo $timestep" | getline timestep       
    printf(" dt       =%.1f,	     !      : delta t\n",timestep)
  }
 else if (substr($1,1,6) == "IDATEI")
  {
   "echo $yi" | getline yi	 
   "echo $mi" | getline mi	 
   "echo $di" | getline di	 
   "echo $hi" | getline hi	 
    printf(" IDATEI   = %2.2d,%2.2d,%2.2d,%4.4d, !LABELI: initial forecasting label\n",hi,di,mi,yi)
  }
 else if (substr($1,1,6) == "IDATEW")
  {
   "echo $yf" | getline yf	 
   "echo $mf" | getline mf	 
   "echo $df" | getline df	 
   "echo $hf" | getline hf	 
    printf(" IDATEW   = %2.2d,%2.2d,%2.2d,%4.4d, !LABELC: final forecasting label for cold\n",hf,df,mf,yf)
  }
 else if (substr($1,1,6) == "IDATEF")
  {
   "echo $yf" | getline yf	 
   "echo $mf" | getline mf	 
   "echo $df" | getline df	 
   "echo $hf" | getline hf	 
    printf(" IDATEF   = %2.2d,%2.2d,%2.2d,%4.4d, !LABELF: final forecasting label for warm\n",hf,df,mf,yf)
  }
 else if (substr($1,1,5) == "NMSST")
  { 
   "echo $aspa" | getline aspa
   "echo $NMSST" | getline NMSST
    printf(" NMSST     =%s%s%s,       !NMSST : sst file name\n",aspa,NMSST,aspa)
  }
 else if (substr($1,1,5) == "DHFCT")
  { 
   "echo $DHFCT" | getline DHFCT
    printf(" DHFCT    =%2.2d,             !DHFCT : > 0 interval in hours to output diagnostics\n",DHFCT)
  }
 else if (substr($1,1,9) == "eigeninit")
  { 
   "echo $eigeninit" | getline eigeninit
    printf(" eigeninit     =%s,  ! eigenInit  --> .FALSE.\n",eigeninit)
  }
 else if (substr($1,1,6) == "mgiven")
  { 
   "echo $mgiven" | getline mgiven
    printf(" mgiven	   =%s,  ! mgiven  --> .FALSE.\n",mgiven)
  }
 else if (substr($1,1,10) == "gaussgiven")
  { 
   "echo $gaussgiven" | getline gaussgiven
    printf(" gaussgiven    =%s,  ! gaussgiven  --> .FALSE.\n",gaussgiven)
  }
 else if (substr($1,1,5) == "PREFX")
  { 
   "echo $aspa" | getline aspa
   "echo $PREFX" | getline PREFX
    printf(" PREFX    =%s%s%s , 	 !PREFX : preffix for name of output files\n",aspa,PREFX,aspa)
  }
 else if (substr($1,1,5) == "PREFY")
  { 
   "echo $aspa" | getline aspa
   "echo $PREFY" | getline PREFY
    printf(" PREFY    =%s%s%s , 	 !PREFY : preffix for name of input files\n",aspa,PREFY,aspa)
  }
 else if (substr($1,1,7) == "path_in")
  { 
   "echo $aspa" | getline aspa
   "echo $path_in" | getline path_in
    printf(" path_in=%s%s%s , \n",aspa,path_in,aspa)
  }
 else if (substr($1,1,14) == "dirfNameOutput")
  { 
   "echo $aspa" | getline aspa
   "echo $dirfNameOutput" | getline dirfNameOutput
    printf(" dirfNameOutput=%s%s%s , \n",aspa,dirfNameOutput,aspa)
  }
  else
  {
    print $0
  }
 }'    > ${EXECFILEPATH}/MODELIN
 
export PBS_SERVER=eslogin13

echo -e "Namelista criado em:\n ${EXECFILEPATH}/MODELIN"
#
# Step 2: Build script that runs the AGCM executable getting information
#         from namelist 
#

cat <<EOF1>${EXECFILEPATH}/mpisepCTRL.${LABELI}.bash
#!/bin/bash
export F_UFMTENDIAN=18,19,20,22,23,24,25,26,27,31,32,33,36,37,38,39,42,43,44,45,49,50,51,52,53,55,61,66,71,77,80,81,82,83,88,91,92,93,99
export GFORTRAN_CONVERT_UNIT=big_endian:18,19,20,22,23,24,25,26,27,31,32,33,36,37,38,39,42,43,44,45,49,50,51,52,53,55,61,66,71,77,80,81,82,83,88,91,92,93,99
export KMP_STACKSIZE=128m
ulimit -s unlimited
export MPID_RSH
cd ${EXECFILEPATH}
${EXECFILEPATH}/ParModel_MPI < ${EXECFILEPATH}/MODELIN > ${EXECFILEPATH}/setout/Print.model.${LABELI}.${tmstp}.%s.MPI${cpu_mpi}.out
EOF1
chmod +x ${EXECFILEPATH}/mpisepCTRL.${LABELI}.bash
#
# Step 3: Build script to submit the script above in the UNA
#
mkdir -p ${EXECFILEPATH}/setout
export PBS_SERVER=eslogin13

tmstp=$(date +"%s")

cat <<EOF0>${SCRIPTFILEPATH}
#!/bin/bash
#PBS -o ${EXECFILEPATH}/setout/Out.model_CTRL.${LABELI}.${tmstp}.%s.MPI${cpu_mpi}.out
#PBS -e ${EXECFILEPATH}/setout/Out.model_CTRL.${LABELI}.${tmstp}.%s.MPI${cpu_mpi}.err
#PBS -l walltime=0:30:00
#PBS -l mppwidth=${MPPWIDTH}
#PBS -l mppnppn=${MPPNPPN}
#PBS -l mppdepth=${MPPDEPTH}
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N $RES
#PBS -q ${QUEUE2}
export PBS_SERVER=eslogin13

#
cd ${EXECFILEPATH}
date
export OMP_NUM_THREADS=${ThreadsperMPITASK}
optserver=`printf "$PBS_SERVER \n" | cut -c1-3`
if [[ (\${optserver} = "aux") ]]; then
   ${EXECFILEPATH}/mpisepCTRL.${LABELI}.bash
else
   aprun -n ${MPPWIDTH} -N  ${MPPNPPN} -d ${MPPDEPTH} ${EXECFILEPATH}/mpisepCTRL.${LABELI}.bash
fi
date

EOF0
chmod +x ${SCRIPTFILEPATH}

qsub -W block=true,sandbox=PRIVATE ${SCRIPTFILEPATH}

exit 0
