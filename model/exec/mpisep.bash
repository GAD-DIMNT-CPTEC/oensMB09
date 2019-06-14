#!/bin/bash
export F_UFMTENDIAN=18,19,20,22,23,24,25,26,27,31,32,33,36,37,38,39,42,43,44,45,49,50,51,52,53,55,61,66,71,77,80,81,82,83,88,91,92,93,99
export GFORTRAN_CONVERT_UNIT=big_endian:18,19,20,22,23,24,25,26,27,31,32,33,36,37,38,39,42,43,44,45,49,50,51,52,53,55,61,66,71,77,80,81,82,83,88,91,92,93,99
export KMP_STACKSIZE=128m
ulimit -s unlimited
export MPID_RSH
cd /scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/model/exec
/scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/model/exec/ParModel_MPI < /scratch1/grupos/pad/home/paulo.kubota/oens-1.0.0/model/exec/MODELIN
