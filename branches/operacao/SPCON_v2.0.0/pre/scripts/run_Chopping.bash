#!/bin/bash 
# 
# script to run CPTEC Global Model on PC Clusters under MPI Scali
# and Sun Grid Engine without OpenMP
#
# assumptions: assume present at the same directory:
#              ParModel_MPI (Global Model Executable file)
#              MODELIN (Global Model input Namelist file)
#
# usage: run_multi_UNA cpu_mpi cpu_node name hold
# where:
# cpu_mpi: integer, the desired number of mpi processes
# cpu_node: integer, the desired number of mpi processes per shared memory node
# name: character, the job name (for SGE)
# hold: any, present or not;
#            if absent, script finishes after queueing job;
#            if present, script holds till job completion
if [ "$#" == 4 ]
then hold=" "
else hold=
fi
export FEXE=`pwd`
export cpu_mpi=$1
export cpu_node=$2
export RES=$3
num=$(($cpu_mpi+$cpu_node-1))
fra=$(($num/$cpu_node))
cpu_tot=$(($fra*$cpu_node))
echo fila=mpi-npn${cpu_node} total cpus=${cpu_tot}

######################################################
#dirhome=/home/pkubota/mcgacptec/pre
#dirdata=/mpp/pkubota/mcgacptec
#dirgrads=/usr/local/grads
#
# Set  Res for Chopping
#RESIN=382
#KMIN=64
#RESOUT=042
#KMOUT=28
#
#SetLinear=F
#
#RESO=042
#IM=128
#JM=64
#prefix=${JM}
##set run date
#export DATA=2004032600
#
## Machine options: SX6; Linux
#MAQUI=Linux
#####################################################

varname=Chopping_parallel
ieeefiles='10,15,20,25,30,35,40,45,50,55,60,65,70,75'
direxe=${dirdata}/pre/exec
dirsrc=${dirhome}/sources/${varname}
dirout=${dirhome}/scripts/output
dirrun=${dirhome}/scripts
echo " "
host=`hostname`
echo " ${host}"
RUNTM=`date +'%d_%H:%M'`
hh=`echo ${DATA} | cut -c9-10`
#
echo "NAMELIST: ${direxe}/${varname}.nml"
cat <<EOT0 > ${direxe}/${varname}.nml
 &ChopNML
  MendOut=${RESOUT},        ! Spectral Horizontal Resolution of Output Data
  KmaxOut=${KMOUT},         ! Number of Layers of Output Data
  MendMin=127               ! Minimum Spectral Resolution For Doing Topography Smoothing
  MendCut=${RESOUT},        ! Spectral Resolution Cut Off for Topography Smoothing
  Iter=10,            ! Number of Iteractions in Topography Smoothing
  SmthPerCut=0.12,    ! Percentage for Topography Smoothing
  GetOzone=T,         ! Flag to Produce Ozone Files
  GetTracers=F,       ! Flag to Produce Tracers Files
  GrADS=T,            ! Flag to Produce GrADS Files
  GrADSOnly=F,        ! Flag to Only Produce GrADS Files (Do Not Produce Inputs for Model)
  GDASOnly=F,         ! Flag to Only Produce Input CPTEC Analysis File
  SmoothTopo=T,       ! Flag to Performe Topography Smoothing
  RmGANL=T,           ! Flag to Remove GANL File if Desired
  LinearGrid=${SetLinear},           ! Flag to Set Linear or Quadratic Gaussian Grid
  DateLabel='${DATA}', ! Date Label: yyyymmddhh or DateLabel='          '
                          !       If Year (yyyy), Month (mm) and Day (dd) Are Unknown
  UTC='${hh}',               ! UTC Hour: hh, Must Be Given if Label='          ', else UTC=' '
  NCEPName='${AnlPref} ',                       ! NCEP Analysis Preffix for Input File Name
  DirMain='${dirdata}/ '  ! Main User Data Directory
  DirHome='${dirhome}/ ' ! Home User Source Directory
 /
EOT0

cat <<EOT1 > ${direxe}/set${varname}.bash
#!/bin/bash
#PBS -o ${direxe}/${varname}_${DATA}.out
#PBS -e ${direxe}/${varname}_${DATA}.err
#PBS -l walltime=0:05:00
#PBS -l mppwidth=${cpu_mpi}
#PBS -l mppnppn=${cpu_node}
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N $RES
#PBS -q ${QUEUE2}

#
if [[ (${MAQUI} = "Linux") || (${MAQUI} = "linux") ]]; then
  export F_UFMTENDIAN=${ieeefiles}
  export GFORTRAN_CONVERT_UNIT=big_endian:${ieeefiles}
  echo "F_UFMTENDIAN= " ${F_UFMTENDIAN}
  echo "GFORTRAN_CONVERT_UNIT= " ${GFORTRAN_CONVERT_UNIT}
fi
#
export KMP_STACKSIZE=128m
ulimit -s unlimited

cd ${direxe}
ulimit -s
date
optserver=`printf "$PBS_SERVER \n" | cut -c1-3`
if [[ (\${optserver} = "aux") ]]; then
${direxe}/${varname} -i ${direxe}/${varname}.nml
else
time aprun -n ${cpu_mpi} -N ${cpu_node} ${direxe}/${varname} -i ${direxe}/${varname}.nml
fi
date
EOT1
#
#   Change mode to be executable
#
set -x

chmod +x ${direxe}/set${varname}.bash
jobnumber=$(qsub ${hold} ${direxe}/set${varname}.bash | cut -d. -f1)
it=2
while [ ${it} -gt 0 ];do
it=`qstat | grep $jobnumber | wc -l`
sleep 30
done

