#!/bin/bash
##################$ -pe mpi-npn1 1
#PBS -o eslogin11:/scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/pre/exec/Out.MPI1
#PBS -j oe
#PBS -l walltime=4:00:00 
#PBS -l mppwidth=1
#PBS -l mppnppn=1
#PBS -V
#PBS -S /bin/bash
#PBS -N SeaMask
#PBS -q pesq.q

export PBS_SERVER=aux20-eth4

#
export PBS_ENVIRONMENT
if [[ (Linux = "Linux") || (Linux = "linux") ]]; then
  export F_UFMTENDIAN=10,20,30
  export GFORTRAN_CONVERT_UNIT=big_endian:10,20,30
  echo "F_UFMTENDIAN= " 
  echo "GFORTRAN_CONVERT_UNIT= " 
fi
export KMP_STACKSIZE=128m
ulimit -s unlimited

#
cd /scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/pre/exec
ulimit -s
date 
optserver=aux
if [[ (${optserver} = "aux") ]]; then
/scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/pre/exec/LandSeaMask -i /scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/pre/exec/LandSeaMask.nml
else
time aprun -n 1 -N 1 /scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/pre/exec/LandSeaMask -i /scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/pre/exec/LandSeaMask.nml
fi
date 
#time /scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/pre/exec/LandSeaMask 
