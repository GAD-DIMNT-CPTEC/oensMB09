#!/bin/bash
#$ -q global.q
#$ -pe mpi-npn4 4
#$ -o una1:/home/io_pad/tempo/global/MCGATQ0213L042/run/setout/Out.grh.2010041500.1271332659.%s.MPI4.out
#$ -j y
#$ -V
#$ -S /bin/bash
#$ -N grh
if [[ Linux == "Linux" || Linux == "linux" ]]; then
export F_UFMTENDIAN=10,11
fi
export KMP_STACKSIZE=128m
ulimit -s unlimited
cd /mpp/io_pad/tempo/global/MCGATQ0213L042/pos/exec

/home/io_pad/tempo/global/MCGATQ0213L042/grh/exec/PostGridHistory < /home/io_pad/tempo/global/MCGATQ0213L042/grh/exec/PostGridHistory.nml

exit 0
