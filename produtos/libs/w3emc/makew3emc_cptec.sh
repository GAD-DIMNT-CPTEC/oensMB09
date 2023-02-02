#! /bin/bash -x

COMP=${1}

#if [ ${COMP} == "linux_intel_egeon" ]
if [ ${COMP} == "comp=intel" ]
then
  FC=/opt/intel/oneapi/mpi/2021.4.0/bin/mpif90
  CC=/opt/intel/oneapi/mpi/2021.4.0/bin/mpiicc
#elif [ ${COMP} == "linux_gnu_egeon" ]
elif [ ${COMP} == "comp=gnu" ]
then
  FC=/opt/ohpc/pub/mpi/openmpi4-gnu9/4.1.1/bin/mpif90
  CC=/opt/ohpc/pub/mpi/openmpi4-gnu9/4.1.1/bin/mpicc
else
  FC=mpif90
  CC=mpicc
fi

mkdir -p NCEPLIBS-w3emc-2.9.3/build
cd NCEPLIBS-w3emc-2.9.3/build
CC=${CC} FC=${FC} cmake -DCMAKE_INSTALL_PREFIX=../../NCEPLIBS-w3emc-2.9.3/rls -DCMAKE_PREFIX_PATH="../../bacio/NCEPLIBS-bacio-2.5.0/rls" ..
make -j2
make install
