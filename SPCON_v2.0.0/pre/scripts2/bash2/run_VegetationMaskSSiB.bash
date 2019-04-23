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

#################################################
#export dirhome=/scratch/p01211/mcga2/pre
#export dirdata=/scratch/p01211/mcga2
#export dirgrads=/usr/local/grads
#
## Machine options: SX6; Linux
#export MAQUI=Linux
#
## Set  Res for Chopping
#export RESIN=382
#export KMIN=64
#export RESOUT=213
#export KMOUT=42
#export SetLinear=FALSE
#export RESO=213
#export IM=640
#export JM=320
#export prefix=${JM}
##set run date
#export DATA=2004032600
####################################################

varname=VegetationMaskSSiB
ieeefiles='20,30'
#
direxe=${dirdata}/pre/exec
dirsrc=${dirhome}/sources/${varname}
dirout=${dirhome}/scripts/output
dirrun=${dirhome}/scripts
echo " "
host=`hostname`
echo " ${host}"
RUNTM=`date +'%d_%H:%M'`
#
cat <<EOT0 > ${dirrun}/aux/${varname}.nml
 &InputDim
  Idim=360,     ! Number of Longitudes in SSiB Vegetation Mask Data
  Jdim=180,     ! Number of Latitudes in SSiB Vegetation Mask Data
  GrADS=.TRUE., ! Flag for GrADS Outputs
  DirMain='${dirdata}/ ' ! Main Datain/Dataout Directory
 /

EOT0
cat <<EOT1 > ${dirrun}/aux/set${varname}.bash
#!/bin/bash
#PBS -o crow:${FEXE}/Out.MPI${cpu_mpi}
#PBS -j oe
#PBS -l walltime=4:00:00
#PBS -l mppwidth=${cpu_mpi}
#PBS -l mppnppn=${cpu_node}
#PBS -V
#PBS -S /bin/bash
#PBS -N $RES
#
if [[ (${MAQUI} = "Linux") || (${MAQUI} = "linux") ]]; then
  export F_UFMTENDIAN=${ieeefiles}
  export GFORTRAN_CONVERT_UNIT=big_endian:${ieeefiles}
  echo "F_UFMTENDIAN= " ${F_UFMTENDIAN}
  echo "GFORTRAN_CONVERT_UNIT= " ${GFORTRAN_CONVERT_UNIT}
fi
export KMP_STACKSIZE=128m
ulimit -s unlimited
#
cd ${dirrun}/aux
date
time aprun -n ${cpu_mpi} -N ${cpu_node} ${direxe}/${varname} -i ${dirrun}/aux/${varname}.nml
date
EOT1
#
#   Change mode to be executable
#
chmod +x ${dirrun}/aux/set${varname}.bash
qsub $hold ${dirrun}/aux/set${varname}.bash
it=2
while [ ${it} -gt 0 ];do
it=`qstat | grep $RES | wc -l`
done

