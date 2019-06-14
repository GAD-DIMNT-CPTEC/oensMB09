#!/bin/bash
#PBS -o crow:/scratch/p01211/oens-1.0.0/run/Out.MPI1
#PBS -j oe
#PBS -l walltime=4:00:00
#PBS -l mppwidth=1
#PBS -l mppnppn=1
#PBS -V
#PBS -S /bin/bash
#PBS -N DeepSoilTemClim
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
cd /scratch/p01211/oens-1.0.0/pre/scripts/aux
date
time aprun -n 1 -N 1 /scratch/p01211/oens-1.0.0/pre/exec/DeepSoilTemperatureClima -i /scratch/p01211/oens-1.0.0/pre/scripts/aux/DeepSoilTemperatureClima.nml
date
