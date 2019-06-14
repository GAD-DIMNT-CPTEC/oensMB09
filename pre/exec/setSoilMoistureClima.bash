#!/bin/bash
#PBS -o eslogin11:/scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/pre/exec/Out.MPI1
#PBS -j oe
#PBS -l walltime=4:00:00
#PBS -l mppwidth=1
#PBS -l mppnppn=1
#PBS -V
#PBS -S /bin/bash
#PBS -N SoilMoisClm
#PBS -q pesq.q

export PBS_SERVER=aux20-eth4

#
if [[ (Linux = "Linux") || (Linux = "linux") ]]; then
  export F_UFMTENDIAN=20
  export GFORTRAN_CONVERT_UNIT=big_endian:20
  echo "F_UFMTENDIAN= " 
  echo "GFORTRAN_CONVERT_UNIT= " 
fi
export KMP_STACKSIZE=128m
ulimit -s unlimited

#
cd /scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/pre/exec
date
optserver=aux
if [[ (${optserver} = "aux") ]]; then
/scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/pre/exec/SoilMoistureClima -i /scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/pre/exec/SoilMoistureClima.nml
else
time aprun -n 1 -N 1 /scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/pre/exec/SoilMoistureClima -i /scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/pre/exec/SoilMoistureClima.nml
fi
date
