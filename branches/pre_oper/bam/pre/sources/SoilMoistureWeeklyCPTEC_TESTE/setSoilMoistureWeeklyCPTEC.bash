#!/bin/bash
#PBS -o  /lustre_xc50/ioper/models/BAM/pre/sources/SoilMoistureWeeklyCPTEC_TESTE/SoilMoistureWeeklyCPTEC_Out.MPI1
#PBS -j oe
#PBS -l walltime=00:30:00
#PBS -l nodes=1:ppn=40
#PBS -V
#PBS -S /bin/bash
#PBS -q workq

#
ulimit -s unlimited

#
cd /lustre_xc50/ioper/models/BAM/pre/sources/SoilMoistureWeeklyCPTEC_TESTE
date
time aprun -n 1 -N 1 /lustre_xc50/ioper/models/BAM/pre/sources/SoilMoistureWeeklyCPTEC_TESTE/SoilMoistureWeeklyCPTEC -i SoilMoistureWeeklyCPTEC.nml
date

