#!/usr/bin/ksh
#PBS -l cpunum_prc=1
#PBS -l tasknum_prc=1
#PBS -l memsz_job=8gb
#PBS -l cputim_job=86400
#PBS -o tupay:/gfs/home3/io_dop/tempo/global/T299L64/pos/scripts/run.out
#PBS -e tupay:/gfs/home3/io_dop/tempo/global/T299L64/pos/scripts/run.out
#PBS -j o
#
F_RSVTASK=1

OMP_NUM_THREADS=1
F_ERRCNT=1
F_SETBUF=20480
export F_ERRCNT F_SETBUF F_RSVTASK OMP_NUM_THREADS

cd /gfs/home3/io_dop/tempo/global/T299L64/pos/exec
Post < /gfs/home3/io_dop/tempo/global/T299L64/pos/scripts/POSTIN_T299L64 > /gfs/home3/io_dop/tempo/global/T299L64/pos/scripts/xmit_pos.20060811_20060811.POS

#
